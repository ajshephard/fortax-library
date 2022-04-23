
! This file is part of the FORTAX library;

! FORTAX is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! FORTAX is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with FORTAX.  If not, see <http://www.gnu.org/licenses/>.




! fortax_prices
! -----------------------------------------------------------------------
! module provides date and price uprating capabilities for FORTAX, AS

! updated 22/06/13. removed global variables for storing price index
! introduced rpi_t defined in fortax_type


module fortax_prices

    use fortax_realtype, only : dp
    private :: dp

    private

    public :: loadindex, setindex, getindex, upratefactor, upratesys
    public :: checkdate, loadsysindex, getsysindex, rpi_saveF90
    public :: operator(*), operator(/)

    interface operator(*)
        module procedure sys_times_factor
        module procedure sys_times_factor_integer
        module procedure factor_times_sys
        module procedure factor_times_sys_integer
    end interface

    interface operator(/)
        module procedure sys_div_factor
        module procedure sys_div_factor_integer
    end interface

contains

    ! setindex
    ! -------------------------------------------------------------
    ! sets the price index data in rpi using the specified date and
    ! index information

    subroutine setindex(rpi, mydate, myindex, mysize)

        use fortax_util, only : fortaxwarn
        use fortax_type, only : rpi_t, maxRPI

        implicit none

        integer, intent(in) :: mysize
        real(dp), intent(in) :: myindex(mysize)
        integer, intent(in) :: mydate(mysize)
        type(rpi_t), intent(out) :: rpi

        if (mysize<=maxRPI) then
            rpi%date(1:mysize)  = mydate
            rpi%index(1:mysize) = myindex
            rpi%ndate = mysize
        else
            call fortaxwarn('out of range in call to setindex')
            rpi%date  = mydate(1:maxRPI)
            rpi%index = mydate(1:maxRPI)
            rpi%ndate = maxRPI
        end if

    end subroutine setindex


    ! loadindex
    ! -----------------------------------------------------------------------
    ! loads a price index file saved as a comma separated values (CSV) file.
    ! If fname is not specified it defaults to 'prices/rpi.csv'

    subroutine loadindex(rpi, fname)

        use fortax_util, only : fortaxerror, fortaxwarn, inttostr
        use fortax_type, only : rpi_t, maxRPI

        implicit none

        type(rpi_t), intent(out) :: rpi
        character(len = *), intent(in), optional :: fname

        integer :: funit
        integer :: istat
        integer :: nrec

        logical :: isfile
        integer :: tempdate, ndate
        real(dp) :: tempindex

        if (present(fname)) then
            inquire(file = fname, exist = isfile)
            if (isfile) then
                open(newunit = funit, file = fname, status = 'old')
            else
                call fortaxerror('price index file does not exist (' // trim(adjustl(fname)) // ')')
            end if
        else
            inquire(file = 'prices/rpi.csv', exist = isfile)
            if (isfile) then
                open(newunit = funit, file = 'prices/rpi.csv', status = 'old')
            else
                call fortaxerror('default price index file does not exist')
            end if
        end if

        read (funit, *, iostat = istat) ndate

        if (ndate > maxRPI) then
            call fortaxwarn('declared ndate exceeds maxRPI')
        end if

        if (istat .ne. 0) then
            call fortaxerror('error reading number of records on line 1')
        end if

        nrec = 0

        do

            read(funit, *, iostat = istat) tempdate, tempindex

            if (istat == -1) then !eof
                exit
            else if (istat > 0) then !error
                call fortaxerror('error reading record after '//inttostr(nrec))
            else
                nrec = nrec + 1
                if (nrec > maxRPI) then
                    exit
                else
                    rpi%date(nrec)  = tempdate
                    rpi%index(nrec) = tempindex
                end if
            end if

        end do

        close(funit)

        if (nrec .ne. ndate) then
            call fortaxerror('number of rpi records does not equal number declared on line 1')
        else
            rpi%ndate = ndate
        end if

    end subroutine loadindex


    ! getindex
    ! -----------------------------------------------------------------------
    ! returns the price index associated with the supplied YYYYMMDD date

    real(dp) elemental function getindex(rpi, date)

        use fortax_type, only : rpi_t

        implicit none

        type(rpi_t), intent(in) :: rpi
        integer, intent(in) :: date
        integer :: year,  month
        integer :: year1, month1

        !exploits structure of data
        if (date < rpi%date(1)) then
            getindex = 0.0_dp
        elseif (date > rpi%date(rpi%ndate)) then
            getindex = 0.0_dp
        else
            year = date / 10000
            month = (date - year * 10000) / 100
            year1 = rpi%date(1) / 10000
            month1 = (rpi%date(1) - year1 * 10000) / 100
            getindex = rpi%index((year - year1) * 12 + month)
        end if

    end function getindex


    ! upratefactor
    ! -----------------------------------------------------------------------
    ! returns uprating factor from date0 to date1 prices (both in YYYYMMDD
    ! format). it calls the function getindex.

    real(dp) elemental function upratefactor(rpi, date0, date1)

        use fortax_type, only : rpi_t

        implicit none

        type(rpi_t), intent(in) :: rpi
        integer, intent(in) :: date0, date1

        upratefactor = getindex(rpi, date1) / getindex(rpi, date0)

    end function upratefactor


    ! upratesys
    ! -----------------------------------------------------------------------
    ! uprates the tax system sys using the specified uprating factor. If
    ! newdate is present, it will set the prices attribute to this value.

    subroutine upratesys(sys, factor, newdate)

        use fortax_type, only : sys_t
        use fortax_util, only : fortaxwarn

        implicit none

        type(sys_t), intent(inout) :: sys
        real(dp), intent(in) :: factor
        integer, intent(in), optional :: newdate

        if (present(newdate)) sys%extra%prices = newdate

sys%inctax%pa = sys%inctax%pa * factor
            sys%inctax%paTaperThresh = sys%inctax%paTaperThresh * factor
            sys%inctax%mma = sys%inctax%mma * factor
            sys%inctax%ctc = sys%inctax%ctc * factor
            sys%inctax%ctcyng = sys%inctax%ctcyng * factor
            sys%inctax%bands = sys%inctax%bands * factor
sys%natins%c2floor = sys%natins%c2floor * factor
            sys%natins%c2rate = sys%natins%c2rate * factor
            sys%natins%ceiling = sys%natins%ceiling * factor
            sys%natins%bands = sys%natins%bands * factor
            sys%natins%c4bands = sys%natins%c4bands * factor
sys%chben%basic = sys%chben%basic * factor
            sys%chben%kid1xtr = sys%chben%kid1xtr * factor
            sys%chben%opf = sys%chben%opf * factor
            sys%chben%MatGrantVal = sys%chben%MatGrantVal * factor
            sys%chben%taperStart = sys%chben%taperStart * factor
sys%fc%adult = sys%fc%adult * factor
            sys%fc%ftprem = sys%fc%ftprem * factor
            sys%fc%thres = sys%fc%thres * factor
            sys%fc%MaintDisreg = sys%fc%MaintDisreg * factor
            sys%fc%MaxCC1 = sys%fc%MaxCC1 * factor
            sys%fc%MaxCC2 = sys%fc%MaxCC2 * factor
            sys%fc%WFTCMaxCC1 = sys%fc%WFTCMaxCC1 * factor
            sys%fc%WFTCMaxCC2 = sys%fc%WFTCMaxCC2 * factor
            sys%fc%kidcred = sys%fc%kidcred * factor
sys%ctc%fam = sys%ctc%fam * factor
            sys%ctc%baby = sys%ctc%baby * factor
            sys%ctc%kid = sys%ctc%kid * factor
sys%wtc%Basic = sys%wtc%Basic * factor
            sys%wtc%CouLP = sys%wtc%CouLP * factor
            sys%wtc%FT = sys%wtc%FT * factor
            sys%wtc%MaxCC1 = sys%wtc%MaxCC1 * factor
            sys%wtc%MaxCC2 = sys%wtc%MaxCC2 * factor
sys%ntc%thr1lo = sys%ntc%thr1lo * factor
            sys%ntc%thr1hi = sys%ntc%thr1hi * factor
            sys%ntc%thr2 = sys%ntc%thr2 * factor
sys%incsup%MainCou = sys%incsup%MainCou * factor
            sys%incsup%YngCou = sys%incsup%YngCou * factor
            sys%incsup%MainLP = sys%incsup%MainLP * factor
            sys%incsup%YngLP = sys%incsup%YngLP * factor
            sys%incsup%MainSin = sys%incsup%MainSin * factor
            sys%incsup%YngSin = sys%incsup%YngSin * factor
            sys%incsup%ValFSM = sys%incsup%ValFSM * factor
            sys%incsup%DisregLP = sys%incsup%DisregLP * factor
            sys%incsup%DisregSin = sys%incsup%DisregSin * factor
            sys%incsup%DisregCou = sys%incsup%DisregCou * factor
            sys%incsup%PremFam = sys%incsup%PremFam * factor
            sys%incsup%PremLP = sys%incsup%PremLP * factor
            sys%incsup%MaintDisreg = sys%incsup%MaintDisreg * factor
            sys%incsup%AddKid = sys%incsup%AddKid * factor
sys%ctax%bandD = sys%ctax%bandD * factor
sys%rebatesys%MainCou = sys%rebatesys%MainCou * factor
            sys%rebatesys%YngCou = sys%rebatesys%YngCou * factor
            sys%rebatesys%MainLP = sys%rebatesys%MainLP * factor
            sys%rebatesys%YngLP = sys%rebatesys%YngLP * factor
            sys%rebatesys%MainSin = sys%rebatesys%MainSin * factor
            sys%rebatesys%YngSin = sys%rebatesys%YngSin * factor
            sys%rebatesys%DisregSin = sys%rebatesys%DisregSin * factor
            sys%rebatesys%DisregLP = sys%rebatesys%DisregLP * factor
            sys%rebatesys%DisregCou = sys%rebatesys%DisregCou * factor
            sys%rebatesys%PremFam = sys%rebatesys%PremFam * factor
            sys%rebatesys%PremLP = sys%rebatesys%PremLP * factor
            sys%rebatesys%MaintDisreg = sys%rebatesys%MaintDisreg * factor
            sys%rebatesys%MaxCC1 = sys%rebatesys%MaxCC1 * factor
            sys%rebatesys%MaxCC2 = sys%rebatesys%MaxCC2 * factor
            sys%rebatesys%AddKid = sys%rebatesys%AddKid * factor



sys%uc%MainCou = sys%uc%MainCou * factor
            sys%uc%YngCou = sys%uc%YngCou * factor
            sys%uc%MainSin = sys%uc%MainSin * factor
            sys%uc%YngSin = sys%uc%YngSin * factor
            sys%uc%FirstKid = sys%uc%FirstKid * factor
            sys%uc%OtherKid = sys%uc%OtherKid * factor
            sys%uc%MaxCC1 = sys%uc%MaxCC1 * factor
            sys%uc%MaxCC2 = sys%uc%MaxCC2 * factor
            sys%uc%DisregSinNoKidsHi = sys%uc%DisregSinNoKidsHi * factor
            sys%uc%DisregSinNoKidsLo = sys%uc%DisregSinNoKidsLo * factor
            sys%uc%DisregSinKidsHi = sys%uc%DisregSinKidsHi * factor
            sys%uc%DisregSinKidsLo = sys%uc%DisregSinKidsLo * factor
            sys%uc%DisregCouNoKidsHi = sys%uc%DisregCouNoKidsHi * factor
            sys%uc%DisregCouNoKidsLo = sys%uc%DisregCouNoKidsLo * factor
            sys%uc%DisregCouKidsHi = sys%uc%DisregCouKidsHi * factor
            sys%uc%DisregCouKidsLo = sys%uc%DisregCouKidsLo * factor

sys%bencap%sinNoKids = sys%bencap%sinNoKids * factor
            sys%bencap%sinKids = sys%bencap%sinKids * factor
            sys%bencap%couNoKids = sys%bencap%couNoKids * factor
            sys%bencap%couKids = sys%bencap%couKids * factor
            sys%bencap%UCEarnThr = sys%bencap%UCEarnThr * factor


    end subroutine upratesys

    function sys_times_factor(sys, factor) result(sys2)
        use fortax_type, only : sys_t
        implicit none
        type(sys_t), intent(in) :: sys
        real(dp), intent(in) :: factor
        type(sys_t) :: sys2
sys2%inctax%numbands = sys%inctax%numbands
        sys2%inctax%pa = sys%inctax%pa * factor
        sys2%inctax%doPATaper = sys%inctax%doPATaper
        sys2%inctax%disablePATaperRounding = sys%inctax%disablePATaperRounding
        sys2%inctax%paTaperThresh = sys%inctax%paTaperThresh * factor
        sys2%inctax%paTaperRate = sys%inctax%paTaperRate
        sys2%inctax%mma = sys%inctax%mma * factor
        sys2%inctax%ctc = sys%inctax%ctc * factor
        sys2%inctax%ctcyng = sys%inctax%ctcyng * factor
        sys2%inctax%mmarate = sys%inctax%mmarate
        sys2%inctax%ctctaper = sys%inctax%ctctaper
        sys2%inctax%c4rebate = sys%inctax%c4rebate
        sys2%inctax%bands = sys%inctax%bands * factor
        sys2%inctax%rates = sys%inctax%rates
sys2%natins%numrates = sys%natins%numrates
        sys2%natins%c4nrates = sys%natins%c4nrates
        sys2%natins%c2floor = sys%natins%c2floor * factor
        sys2%natins%c2rate = sys%natins%c2rate * factor
        sys2%natins%ceiling = sys%natins%ceiling * factor
        sys2%natins%rates = sys%natins%rates
        sys2%natins%bands = sys%natins%bands * factor
        sys2%natins%c4rates = sys%natins%c4rates
        sys2%natins%c4bands = sys%natins%c4bands * factor
sys2%chben%doChBen = sys%chben%doChBen
        sys2%chben%basic = sys%chben%basic * factor
        sys2%chben%kid1xtr = sys%chben%kid1xtr * factor
        sys2%chben%opf = sys%chben%opf * factor
        sys2%chben%MatGrantVal = sys%chben%MatGrantVal * factor
        sys2%chben%MatGrantOnlyFirstKid = sys%chben%MatGrantOnlyFirstKid
        sys2%chben%doTaper = sys%chben%doTaper
        sys2%chben%disableTaperRounding = sys%chben%disableTaperRounding
        sys2%chben%taperStart = sys%chben%taperStart * factor
        sys2%chben%taperRate = sys%chben%taperRate
        sys2%chben%taperIsIncTax = sys%chben%taperIsIncTax
sys2%fc%dofamcred = sys%fc%dofamcred
        sys2%fc%NumAgeRng = sys%fc%NumAgeRng
        sys2%fc%MaxAgeCC = sys%fc%MaxAgeCC
        sys2%fc%WFTCMaxAgeCC = sys%fc%WFTCMaxAgeCC
        sys2%fc%adult = sys%fc%adult * factor
        sys2%fc%ftprem = sys%fc%ftprem * factor
        sys2%fc%hours1 = sys%fc%hours1
        sys2%fc%hours2 = sys%fc%hours2
        sys2%fc%thres = sys%fc%thres * factor
        sys2%fc%taper = sys%fc%taper
        sys2%fc%MaintDisreg = sys%fc%MaintDisreg * factor
        sys2%fc%MaxCC1 = sys%fc%MaxCC1 * factor
        sys2%fc%MaxCC2 = sys%fc%MaxCC2 * factor
        sys2%fc%WFTCMaxCC1 = sys%fc%WFTCMaxCC1 * factor
        sys2%fc%WFTCMaxCC2 = sys%fc%WFTCMaxCC2 * factor
        sys2%fc%WFTCPropCC = sys%fc%WFTCPropCC
        sys2%fc%MinAmt = sys%fc%MinAmt
        sys2%fc%kidagel = sys%fc%kidagel
        sys2%fc%kidageu = sys%fc%kidageu
        sys2%fc%kidcred = sys%fc%kidcred * factor
sys2%ctc%fam = sys%ctc%fam * factor
        sys2%ctc%baby = sys%ctc%baby * factor
        sys2%ctc%kid = sys%ctc%kid * factor
sys2%wtc%Basic = sys%wtc%Basic * factor
        sys2%wtc%CouLP = sys%wtc%CouLP * factor
        sys2%wtc%FT = sys%wtc%FT * factor
        sys2%wtc%MinHrsKids = sys%wtc%MinHrsKids
        sys2%wtc%MinHrsCouKids = sys%wtc%MinHrsCouKids
        sys2%wtc%MinHrsNoKids = sys%wtc%MinHrsNoKids
        sys2%wtc%FTHrs = sys%wtc%FTHrs
        sys2%wtc%MinAgeKids = sys%wtc%MinAgeKids
        sys2%wtc%MinAgeNoKids = sys%wtc%MinAgeNoKids
        sys2%wtc%MaxCC1 = sys%wtc%MaxCC1 * factor
        sys2%wtc%MaxCC2 = sys%wtc%MaxCC2 * factor
        sys2%wtc%PropCC = sys%wtc%PropCC
        sys2%wtc%MaxAgeCC = sys%wtc%MaxAgeCC
        sys2%wtc%NewDisreg = sys%wtc%NewDisreg
        sys2%wtc%NewDisregCon = sys%wtc%NewDisregCon
sys2%ntc%donewtaxcred = sys%ntc%donewtaxcred
        sys2%ntc%thr1lo = sys%ntc%thr1lo * factor
        sys2%ntc%thr1hi = sys%ntc%thr1hi * factor
        sys2%ntc%thr2 = sys%ntc%thr2 * factor
        sys2%ntc%taper1 = sys%ntc%taper1
        sys2%ntc%taper2 = sys%ntc%taper2
        sys2%ntc%taperCTCInOneGo = sys%ntc%taperCTCInOneGo
        sys2%ntc%MinAmt = sys%ntc%MinAmt
sys2%incsup%doIncSup = sys%incsup%doIncSup
        sys2%incsup%IncChben = sys%incsup%IncChben
        sys2%incsup%NumAgeRng = sys%incsup%NumAgeRng
        sys2%incsup%MainCou = sys%incsup%MainCou * factor
        sys2%incsup%YngCou = sys%incsup%YngCou * factor
        sys2%incsup%MainLP = sys%incsup%MainLP * factor
        sys2%incsup%YngLP = sys%incsup%YngLP * factor
        sys2%incsup%MainSin = sys%incsup%MainSin * factor
        sys2%incsup%YngSin = sys%incsup%YngSin * factor
        sys2%incsup%ValFSM = sys%incsup%ValFSM * factor
        sys2%incsup%DisregLP = sys%incsup%DisregLP * factor
        sys2%incsup%DisregSin = sys%incsup%DisregSin * factor
        sys2%incsup%DisregCou = sys%incsup%DisregCou * factor
        sys2%incsup%DisregShared = sys%incsup%DisregShared
        sys2%incsup%PremFam = sys%incsup%PremFam * factor
        sys2%incsup%PremLP = sys%incsup%PremLP * factor
        sys2%incsup%hours = sys%incsup%hours
        sys2%incsup%MaintDisreg = sys%incsup%MaintDisreg * factor
        sys2%incsup%AgeRngl = sys%incsup%AgeRngl
        sys2%incsup%AgeRngu = sys%incsup%AgeRngu
        sys2%incsup%AddKid = sys%incsup%AddKid * factor
sys2%ctax%docounciltax = sys%ctax%docounciltax
        sys2%ctax%bandD = sys%ctax%bandD * factor
        sys2%ctax%SinDis = sys%ctax%SinDis
        sys2%ctax%RatioA = sys%ctax%RatioA
        sys2%ctax%RatioB = sys%ctax%RatioB
        sys2%ctax%RatioC = sys%ctax%RatioC
        sys2%ctax%RatioE = sys%ctax%RatioE
        sys2%ctax%RatioF = sys%ctax%RatioF
        sys2%ctax%RatioG = sys%ctax%RatioG
        sys2%ctax%RatioH = sys%ctax%RatioH
sys2%rebatesys%RulesUnderFC = sys%rebatesys%RulesUnderFC
        sys2%rebatesys%RulesUnderWFTC = sys%rebatesys%RulesUnderWFTC
        sys2%rebatesys%RulesUnderNTC = sys%rebatesys%RulesUnderNTC
        sys2%rebatesys%RulesUnderUC = sys%rebatesys%RulesUnderUC
        sys2%rebatesys%NumAgeRng = sys%rebatesys%NumAgeRng
        sys2%rebatesys%Restrict = sys%rebatesys%Restrict
        sys2%rebatesys%docap = sys%rebatesys%docap
        sys2%rebatesys%MainCou = sys%rebatesys%MainCou * factor
        sys2%rebatesys%YngCou = sys%rebatesys%YngCou * factor
        sys2%rebatesys%MainLP = sys%rebatesys%MainLP * factor
        sys2%rebatesys%YngLP = sys%rebatesys%YngLP * factor
        sys2%rebatesys%MainSin = sys%rebatesys%MainSin * factor
        sys2%rebatesys%YngSin = sys%rebatesys%YngSin * factor
        sys2%rebatesys%DisregSin = sys%rebatesys%DisregSin * factor
        sys2%rebatesys%DisregLP = sys%rebatesys%DisregLP * factor
        sys2%rebatesys%DisregCou = sys%rebatesys%DisregCou * factor
        sys2%rebatesys%CredInDisregCC = sys%rebatesys%CredInDisregCC
        sys2%rebatesys%ChbenIsIncome = sys%rebatesys%ChbenIsIncome
        sys2%rebatesys%PremFam = sys%rebatesys%PremFam * factor
        sys2%rebatesys%PremLP = sys%rebatesys%PremLP * factor
        sys2%rebatesys%MaintDisreg = sys%rebatesys%MaintDisreg * factor
        sys2%rebatesys%MaxCC1 = sys%rebatesys%MaxCC1 * factor
        sys2%rebatesys%MaxCC2 = sys%rebatesys%MaxCC2 * factor
        sys2%rebatesys%MaxAgeCC = sys%rebatesys%MaxAgeCC
        sys2%rebatesys%AgeRngl = sys%rebatesys%AgeRngl
        sys2%rebatesys%AgeRngu = sys%rebatesys%AgeRngu
        sys2%rebatesys%AddKid = sys%rebatesys%AddKid * factor
sys2%hben%doHBen = sys%hben%doHBen
        sys2%hben%taper = sys%hben%taper
        sys2%hben%MinAmt = sys%hben%MinAmt
sys2%ctaxben%docounciltaxben = sys%ctaxben%docounciltaxben
        sys2%ctaxben%taper = sys%ctaxben%taper
        sys2%ctaxben%doEntitlementCut = sys%ctaxben%doEntitlementCut
        sys2%ctaxben%entitlementShare = sys%ctaxben%entitlementShare
sys2%ccben%dopolltax = sys%ccben%dopolltax
        sys2%ccben%taper = sys%ccben%taper
        sys2%ccben%PropElig = sys%ccben%PropElig
        sys2%ccben%MinAmt = sys%ccben%MinAmt
        sys2%ccben%CCrate = sys%ccben%CCrate
sys2%uc%doUnivCred = sys%uc%doUnivCred
        sys2%uc%MainCou = sys%uc%MainCou * factor
        sys2%uc%YngCou = sys%uc%YngCou * factor
        sys2%uc%MainSin = sys%uc%MainSin * factor
        sys2%uc%YngSin = sys%uc%YngSin * factor
        sys2%uc%MinAgeMain = sys%uc%MinAgeMain
        sys2%uc%FirstKid = sys%uc%FirstKid * factor
        sys2%uc%OtherKid = sys%uc%OtherKid * factor
        sys2%uc%MaxCC1 = sys%uc%MaxCC1 * factor
        sys2%uc%MaxCC2 = sys%uc%MaxCC2 * factor
        sys2%uc%PropCC = sys%uc%PropCC
        sys2%uc%MaxAgeCC = sys%uc%MaxAgeCC
        sys2%uc%doRentCap = sys%uc%doRentCap
        sys2%uc%DisregSinNoKidsHi = sys%uc%DisregSinNoKidsHi * factor
        sys2%uc%DisregSinNoKidsLo = sys%uc%DisregSinNoKidsLo * factor
        sys2%uc%DisregSinKidsHi = sys%uc%DisregSinKidsHi * factor
        sys2%uc%DisregSinKidsLo = sys%uc%DisregSinKidsLo * factor
        sys2%uc%DisregCouNoKidsHi = sys%uc%DisregCouNoKidsHi * factor
        sys2%uc%DisregCouNoKidsLo = sys%uc%DisregCouNoKidsLo * factor
        sys2%uc%DisregCouKidsHi = sys%uc%DisregCouKidsHi * factor
        sys2%uc%DisregCouKidsLo = sys%uc%DisregCouKidsLo * factor
        sys2%uc%taper = sys%uc%taper
        sys2%uc%MinAmt = sys%uc%MinAmt
sys2%statepen%doStatePen = sys%statepen%doStatePen
        sys2%statepen%PenAgeMan = sys%statepen%PenAgeMan
        sys2%statepen%PenAgeWoman = sys%statepen%PenAgeWoman
sys2%bencap%doCap = sys%bencap%doCap
        sys2%bencap%doThruUC = sys%bencap%doThruUC
        sys2%bencap%sinNoKids = sys%bencap%sinNoKids * factor
        sys2%bencap%sinKids = sys%bencap%sinKids * factor
        sys2%bencap%couNoKids = sys%bencap%couNoKids * factor
        sys2%bencap%couKids = sys%bencap%couKids * factor
        sys2%bencap%UCEarnThr = sys%bencap%UCEarnThr * factor
sys2%extra%fsminappamt = sys%extra%fsminappamt
        sys2%extra%matgrant = sys%extra%matgrant
        sys2%extra%prices = sys%extra%prices
    end function sys_times_factor

    function sys_times_factor_integer(sys, factor) result(sys2)
        use fortax_type, only : sys_t
        implicit none
        type(sys_t), intent(in) :: sys
        integer, intent(in) :: factor
        type(sys_t) :: sys2
        sys2 = sys_times_factor(sys, real(factor, dp))
    end function sys_times_factor_integer

    function sys_div_factor(sys, factor) result(sys2)
        use fortax_type, only : sys_t
        implicit none
        type(sys_t), intent(in) :: sys
        real(dp), intent(in) :: factor
        type(sys_t) :: sys2
        sys2 = sys_times_factor(sys, (1.0_dp / factor))
    end function sys_div_factor

    function sys_div_factor_integer(sys, factor) result(sys2)
        use fortax_type, only : sys_t
        implicit none
        type(sys_t), intent(in) :: sys
        integer, intent(in) :: factor
        type(sys_t) :: sys2
        sys2 = sys_times_factor(sys, (1.0_dp / real(factor, dp)))
    end function sys_div_factor_integer



    function factor_times_sys(factor, sys) result(sys2)
        use fortax_type, only : sys_t
        implicit none
        real(dp), intent(in) :: factor
        type(sys_t), intent(in) :: sys
        type(sys_t) :: sys2
sys2%inctax%numbands = sys%inctax%numbands
        sys2%inctax%pa = sys%inctax%pa * factor
        sys2%inctax%doPATaper = sys%inctax%doPATaper
        sys2%inctax%disablePATaperRounding = sys%inctax%disablePATaperRounding
        sys2%inctax%paTaperThresh = sys%inctax%paTaperThresh * factor
        sys2%inctax%paTaperRate = sys%inctax%paTaperRate
        sys2%inctax%mma = sys%inctax%mma * factor
        sys2%inctax%ctc = sys%inctax%ctc * factor
        sys2%inctax%ctcyng = sys%inctax%ctcyng * factor
        sys2%inctax%mmarate = sys%inctax%mmarate
        sys2%inctax%ctctaper = sys%inctax%ctctaper
        sys2%inctax%c4rebate = sys%inctax%c4rebate
        sys2%inctax%bands = sys%inctax%bands * factor
        sys2%inctax%rates = sys%inctax%rates
sys2%natins%numrates = sys%natins%numrates
        sys2%natins%c4nrates = sys%natins%c4nrates
        sys2%natins%c2floor = sys%natins%c2floor * factor
        sys2%natins%c2rate = sys%natins%c2rate * factor
        sys2%natins%ceiling = sys%natins%ceiling * factor
        sys2%natins%rates = sys%natins%rates
        sys2%natins%bands = sys%natins%bands * factor
        sys2%natins%c4rates = sys%natins%c4rates
        sys2%natins%c4bands = sys%natins%c4bands * factor
sys2%chben%doChBen = sys%chben%doChBen
        sys2%chben%basic = sys%chben%basic * factor
        sys2%chben%kid1xtr = sys%chben%kid1xtr * factor
        sys2%chben%opf = sys%chben%opf * factor
        sys2%chben%MatGrantVal = sys%chben%MatGrantVal * factor
        sys2%chben%MatGrantOnlyFirstKid = sys%chben%MatGrantOnlyFirstKid
        sys2%chben%doTaper = sys%chben%doTaper
        sys2%chben%disableTaperRounding = sys%chben%disableTaperRounding
        sys2%chben%taperStart = sys%chben%taperStart * factor
        sys2%chben%taperRate = sys%chben%taperRate
        sys2%chben%taperIsIncTax = sys%chben%taperIsIncTax
sys2%fc%dofamcred = sys%fc%dofamcred
        sys2%fc%NumAgeRng = sys%fc%NumAgeRng
        sys2%fc%MaxAgeCC = sys%fc%MaxAgeCC
        sys2%fc%WFTCMaxAgeCC = sys%fc%WFTCMaxAgeCC
        sys2%fc%adult = sys%fc%adult * factor
        sys2%fc%ftprem = sys%fc%ftprem * factor
        sys2%fc%hours1 = sys%fc%hours1
        sys2%fc%hours2 = sys%fc%hours2
        sys2%fc%thres = sys%fc%thres * factor
        sys2%fc%taper = sys%fc%taper
        sys2%fc%MaintDisreg = sys%fc%MaintDisreg * factor
        sys2%fc%MaxCC1 = sys%fc%MaxCC1 * factor
        sys2%fc%MaxCC2 = sys%fc%MaxCC2 * factor
        sys2%fc%WFTCMaxCC1 = sys%fc%WFTCMaxCC1 * factor
        sys2%fc%WFTCMaxCC2 = sys%fc%WFTCMaxCC2 * factor
        sys2%fc%WFTCPropCC = sys%fc%WFTCPropCC
        sys2%fc%MinAmt = sys%fc%MinAmt
        sys2%fc%kidagel = sys%fc%kidagel
        sys2%fc%kidageu = sys%fc%kidageu
        sys2%fc%kidcred = sys%fc%kidcred * factor
sys2%ctc%fam = sys%ctc%fam * factor
        sys2%ctc%baby = sys%ctc%baby * factor
        sys2%ctc%kid = sys%ctc%kid * factor
sys2%wtc%Basic = sys%wtc%Basic * factor
        sys2%wtc%CouLP = sys%wtc%CouLP * factor
        sys2%wtc%FT = sys%wtc%FT * factor
        sys2%wtc%MinHrsKids = sys%wtc%MinHrsKids
        sys2%wtc%MinHrsCouKids = sys%wtc%MinHrsCouKids
        sys2%wtc%MinHrsNoKids = sys%wtc%MinHrsNoKids
        sys2%wtc%FTHrs = sys%wtc%FTHrs
        sys2%wtc%MinAgeKids = sys%wtc%MinAgeKids
        sys2%wtc%MinAgeNoKids = sys%wtc%MinAgeNoKids
        sys2%wtc%MaxCC1 = sys%wtc%MaxCC1 * factor
        sys2%wtc%MaxCC2 = sys%wtc%MaxCC2 * factor
        sys2%wtc%PropCC = sys%wtc%PropCC
        sys2%wtc%MaxAgeCC = sys%wtc%MaxAgeCC
        sys2%wtc%NewDisreg = sys%wtc%NewDisreg
        sys2%wtc%NewDisregCon = sys%wtc%NewDisregCon
sys2%ntc%donewtaxcred = sys%ntc%donewtaxcred
        sys2%ntc%thr1lo = sys%ntc%thr1lo * factor
        sys2%ntc%thr1hi = sys%ntc%thr1hi * factor
        sys2%ntc%thr2 = sys%ntc%thr2 * factor
        sys2%ntc%taper1 = sys%ntc%taper1
        sys2%ntc%taper2 = sys%ntc%taper2
        sys2%ntc%taperCTCInOneGo = sys%ntc%taperCTCInOneGo
        sys2%ntc%MinAmt = sys%ntc%MinAmt
sys2%incsup%doIncSup = sys%incsup%doIncSup
        sys2%incsup%IncChben = sys%incsup%IncChben
        sys2%incsup%NumAgeRng = sys%incsup%NumAgeRng
        sys2%incsup%MainCou = sys%incsup%MainCou * factor
        sys2%incsup%YngCou = sys%incsup%YngCou * factor
        sys2%incsup%MainLP = sys%incsup%MainLP * factor
        sys2%incsup%YngLP = sys%incsup%YngLP * factor
        sys2%incsup%MainSin = sys%incsup%MainSin * factor
        sys2%incsup%YngSin = sys%incsup%YngSin * factor
        sys2%incsup%ValFSM = sys%incsup%ValFSM * factor
        sys2%incsup%DisregLP = sys%incsup%DisregLP * factor
        sys2%incsup%DisregSin = sys%incsup%DisregSin * factor
        sys2%incsup%DisregCou = sys%incsup%DisregCou * factor
        sys2%incsup%DisregShared = sys%incsup%DisregShared
        sys2%incsup%PremFam = sys%incsup%PremFam * factor
        sys2%incsup%PremLP = sys%incsup%PremLP * factor
        sys2%incsup%hours = sys%incsup%hours
        sys2%incsup%MaintDisreg = sys%incsup%MaintDisreg * factor
        sys2%incsup%AgeRngl = sys%incsup%AgeRngl
        sys2%incsup%AgeRngu = sys%incsup%AgeRngu
        sys2%incsup%AddKid = sys%incsup%AddKid * factor
sys2%ctax%docounciltax = sys%ctax%docounciltax
        sys2%ctax%bandD = sys%ctax%bandD * factor
        sys2%ctax%SinDis = sys%ctax%SinDis
        sys2%ctax%RatioA = sys%ctax%RatioA
        sys2%ctax%RatioB = sys%ctax%RatioB
        sys2%ctax%RatioC = sys%ctax%RatioC
        sys2%ctax%RatioE = sys%ctax%RatioE
        sys2%ctax%RatioF = sys%ctax%RatioF
        sys2%ctax%RatioG = sys%ctax%RatioG
        sys2%ctax%RatioH = sys%ctax%RatioH
sys2%rebatesys%RulesUnderFC = sys%rebatesys%RulesUnderFC
        sys2%rebatesys%RulesUnderWFTC = sys%rebatesys%RulesUnderWFTC
        sys2%rebatesys%RulesUnderNTC = sys%rebatesys%RulesUnderNTC
        sys2%rebatesys%RulesUnderUC = sys%rebatesys%RulesUnderUC
        sys2%rebatesys%NumAgeRng = sys%rebatesys%NumAgeRng
        sys2%rebatesys%Restrict = sys%rebatesys%Restrict
        sys2%rebatesys%docap = sys%rebatesys%docap
        sys2%rebatesys%MainCou = sys%rebatesys%MainCou * factor
        sys2%rebatesys%YngCou = sys%rebatesys%YngCou * factor
        sys2%rebatesys%MainLP = sys%rebatesys%MainLP * factor
        sys2%rebatesys%YngLP = sys%rebatesys%YngLP * factor
        sys2%rebatesys%MainSin = sys%rebatesys%MainSin * factor
        sys2%rebatesys%YngSin = sys%rebatesys%YngSin * factor
        sys2%rebatesys%DisregSin = sys%rebatesys%DisregSin * factor
        sys2%rebatesys%DisregLP = sys%rebatesys%DisregLP * factor
        sys2%rebatesys%DisregCou = sys%rebatesys%DisregCou * factor
        sys2%rebatesys%CredInDisregCC = sys%rebatesys%CredInDisregCC
        sys2%rebatesys%ChbenIsIncome = sys%rebatesys%ChbenIsIncome
        sys2%rebatesys%PremFam = sys%rebatesys%PremFam * factor
        sys2%rebatesys%PremLP = sys%rebatesys%PremLP * factor
        sys2%rebatesys%MaintDisreg = sys%rebatesys%MaintDisreg * factor
        sys2%rebatesys%MaxCC1 = sys%rebatesys%MaxCC1 * factor
        sys2%rebatesys%MaxCC2 = sys%rebatesys%MaxCC2 * factor
        sys2%rebatesys%MaxAgeCC = sys%rebatesys%MaxAgeCC
        sys2%rebatesys%AgeRngl = sys%rebatesys%AgeRngl
        sys2%rebatesys%AgeRngu = sys%rebatesys%AgeRngu
        sys2%rebatesys%AddKid = sys%rebatesys%AddKid * factor
sys2%hben%doHBen = sys%hben%doHBen
        sys2%hben%taper = sys%hben%taper
        sys2%hben%MinAmt = sys%hben%MinAmt
sys2%ctaxben%docounciltaxben = sys%ctaxben%docounciltaxben
        sys2%ctaxben%taper = sys%ctaxben%taper
        sys2%ctaxben%doEntitlementCut = sys%ctaxben%doEntitlementCut
        sys2%ctaxben%entitlementShare = sys%ctaxben%entitlementShare
sys2%ccben%dopolltax = sys%ccben%dopolltax
        sys2%ccben%taper = sys%ccben%taper
        sys2%ccben%PropElig = sys%ccben%PropElig
        sys2%ccben%MinAmt = sys%ccben%MinAmt
        sys2%ccben%CCrate = sys%ccben%CCrate
sys2%uc%doUnivCred = sys%uc%doUnivCred
        sys2%uc%MainCou = sys%uc%MainCou * factor
        sys2%uc%YngCou = sys%uc%YngCou * factor
        sys2%uc%MainSin = sys%uc%MainSin * factor
        sys2%uc%YngSin = sys%uc%YngSin * factor
        sys2%uc%MinAgeMain = sys%uc%MinAgeMain
        sys2%uc%FirstKid = sys%uc%FirstKid * factor
        sys2%uc%OtherKid = sys%uc%OtherKid * factor
        sys2%uc%MaxCC1 = sys%uc%MaxCC1 * factor
        sys2%uc%MaxCC2 = sys%uc%MaxCC2 * factor
        sys2%uc%PropCC = sys%uc%PropCC
        sys2%uc%MaxAgeCC = sys%uc%MaxAgeCC
        sys2%uc%doRentCap = sys%uc%doRentCap
        sys2%uc%DisregSinNoKidsHi = sys%uc%DisregSinNoKidsHi * factor
        sys2%uc%DisregSinNoKidsLo = sys%uc%DisregSinNoKidsLo * factor
        sys2%uc%DisregSinKidsHi = sys%uc%DisregSinKidsHi * factor
        sys2%uc%DisregSinKidsLo = sys%uc%DisregSinKidsLo * factor
        sys2%uc%DisregCouNoKidsHi = sys%uc%DisregCouNoKidsHi * factor
        sys2%uc%DisregCouNoKidsLo = sys%uc%DisregCouNoKidsLo * factor
        sys2%uc%DisregCouKidsHi = sys%uc%DisregCouKidsHi * factor
        sys2%uc%DisregCouKidsLo = sys%uc%DisregCouKidsLo * factor
        sys2%uc%taper = sys%uc%taper
        sys2%uc%MinAmt = sys%uc%MinAmt
sys2%statepen%doStatePen = sys%statepen%doStatePen
        sys2%statepen%PenAgeMan = sys%statepen%PenAgeMan
        sys2%statepen%PenAgeWoman = sys%statepen%PenAgeWoman
sys2%bencap%doCap = sys%bencap%doCap
        sys2%bencap%doThruUC = sys%bencap%doThruUC
        sys2%bencap%sinNoKids = sys%bencap%sinNoKids * factor
        sys2%bencap%sinKids = sys%bencap%sinKids * factor
        sys2%bencap%couNoKids = sys%bencap%couNoKids * factor
        sys2%bencap%couKids = sys%bencap%couKids * factor
        sys2%bencap%UCEarnThr = sys%bencap%UCEarnThr * factor
sys2%extra%fsminappamt = sys%extra%fsminappamt
        sys2%extra%matgrant = sys%extra%matgrant
        sys2%extra%prices = sys%extra%prices
    end function factor_times_sys

    function factor_times_sys_integer(factor, sys) result(sys2)
        use fortax_type, only : sys_t
        implicit none
        integer, intent(in) :: factor
        type(sys_t), intent(in) :: sys
        type(sys_t) :: sys2
        sys2 = factor_times_sys(real(factor, dp), sys)
    end function factor_times_sys_integer

    ! checkDate
    ! -----------------------------------------------------------------------
    ! returns true or false depending on whether date YYYYMMDD is valid

    logical pure function checkDate(date)

        implicit none

        integer, intent(in) :: date
        integer :: year, month, day, maxday

        year  = date / 10000
        month = (date - year * 10000) / 100
        day   = date - (date / 100) * 100

        select case (month)
            case (1,3,5,7,8,10,12)
                maxday = 31
            case (4,6,9,11)
                maxday = 30
            case (2)
                if (((modulo(year, 4) == 0) .and. (modulo(year, 100) .ne. 0)) .or. (modulo(year, 400) == 0)) then
                    maxday = 29
                else
                    maxday = 28
                end if
            case default
                maxday = 0
        end select

        if ((year >= 0) .and. (month >= 1 .and. month <= 12) .and. (day >= 1 .and. day <= maxday)) then
            checkDate = .true.
        else
            checkDate = .false.
        end if

    end function checkDate


    ! loadsysindex
    ! -----------------------------------------------------------------------
    ! provides quick access to the actual system that individuals faced
    ! requires an external system index file (sysindexfile)

    subroutine loadsysindex(sysindex, sysindexfile)

        use fortax_util, only : fortaxerror, inttostr
        use fortax_type, only : sysindex_t, len_sysindex, maxSysIndex

        implicit none

        type(sysindex_t), intent(out) :: sysindex
        character(len = *), intent(in), optional :: sysindexfile

        integer :: funit
        integer :: istat, nrec

        logical :: isfile
        integer :: tempdate0, tempdate1, ndate
        character(len = len_sysindex) :: tempfname

        if (present(sysindexfile)) then
            inquire(file = sysindexfile, exist = isfile)
            if (isfile) then
                open(newunit = funit, file = sysindexfile, status = 'old')
            else
                call fortaxerror('system index file does not exist (' // sysindexfile // ')')
            end if
        else
            inquire(file = 'systems/sysindex.csv', exist = isfile)
            if (isfile) then
                open(newunit = funit, file = 'systems/sysindex.csv', status = 'old')
            else
                call fortaxerror('default system index file does not exist')
            end if
        end if

        read (funit, *, iostat = istat) ndate

        call freesysindex(sysindex)

        if (istat .ne. 0) then
            call fortaxerror('error reading number of records on line 1')
        else
            sysindex%nsys = ndate
        end if

        nrec = 0

        do

            read(funit, *, iostat = istat) tempdate0, tempdate1, tempfname

            if (istat == -1) then !eof
                exit
            elseif (istat > 0) then !error
                call fortaxerror('error reading record after ' // inttostr(nrec))
            else
                nrec = nrec + 1
                if (nrec > maxSysIndex) then
                    call fortaxerror('nrec > maxSysIndex')
                end if
                sysindex%date0(nrec) = tempdate0
                sysindex%date1(nrec) = tempdate1
                sysindex%fname(:, nrec) = transfer(tempfname, sysindex%fname(:, nrec))
            end if

        end do


        close(funit)

        if (nrec .ne. ndate) then
            call fortaxerror('number of records does not equal number declared on line 1')
        end if

    end subroutine loadsysindex


    ! getsysindex
    ! -----------------------------------------------------------------------
    ! returns information which allows the user to easily identify which
    ! tax system operated at any given YYYYMMDD date as specified in sysindex

    subroutine getsysindex(sysindex, date, sysfilepath, sysnum)

        use fortax_util, only : lower, fortaxerror
        use fortax_type, only : sysindex_t, len_sysindex

        implicit none

        type(sysindex_t), intent(in)  :: sysindex
        integer, intent(in) :: date
        character(len = 256), intent(out) :: sysfilepath
        integer, intent(out) :: sysnum

        integer :: i
        character(len = len_sysindex):: sysname

        if (sysindex%nsys == 0) then
            call fortaxerror('system index file is not in memory')
        end if

        if (checkdate(date)) then
            sysnum = 0
            do i = 1, sysindex%nsys
                if (date >= sysindex%date0(i) .and. date <= sysindex%date1(i)) then
                    sysname = transfer(sysindex%fname(:, i), sysname)
                    sysfilepath = 'systems/fortax/' // trim(adjustl(sysname)) // '.json'
                    sysnum = i
                    exit
                end if
            end do
            if (sysnum == 0) then
                call fortaxerror('getsysindex date not contained in sysindex')
            end if
        else
            call fortaxerror('invalid date in getsysindex')
        end if

    end subroutine getsysindex


    ! freesysindex
    ! -----------------------------------------------------------------------
    ! deallocates the data structures which store price information and set
    ! initialization variables to .false.

    subroutine freesysindex(sysindex)

        use fortax_type, only : sysindex_t

        implicit none

        type(sysindex_t), intent(inout) :: sysindex

        sysindex%nsys = 0
        sysindex%date0 = 0
        sysindex%date1 = 0
        sysindex%fname = ''

    end subroutine freesysindex

    subroutine rpi_saveF90(rpi,fname)

        use fortax_util, only : fortaxError, intToStr, dblToStr
        use fortax_type, only : rpi_t

        use, intrinsic :: iso_fortran_env

        implicit none

        type(rpi_t), intent(in) :: rpi
        character(len = *), intent(in), optional :: fname

        integer :: funit, ios, i

        if ( (rpi%ndate <= 0) ) then
            call fortaxError('no price data for writing')
        end if

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit, '(a)') '! .f90 FORTAX Price index; generated using rpi_saveF90'
        write(funit, *)

        write(funit, '(a)') 'integer, parameter :: nindex_f90 =' // intToStr(rpi%ndate)
        write(funit, *)
        write(funit, '(a)') 'integer, parameter :: rpidate_f90(nindex_f90) = (/ &'
        do i = 1, rpi%ndate - 1
            write(funit, '(a)') '    ' // intToStr(rpi%date(i)) // ', &'
        end do
        write(funit, '(a)') '    ' // intToStr(rpi%date(rpi%ndate)) // '/)'
        write(funit, *)
        write(funit, '(a)') 'real(dp), parameter :: rpiindex_f90(nindex_f90) = (/ &'
        do i = 1, rpi%ndate - 1
            write(funit, '(a)') '    ' // dblToStr(rpi%index(i)) // '_dp, &'
        end do
        write(funit, '(a)') '    ' // dblToStr(rpi%index(rpi%ndate)) // '_dp/)'
        write(funit, *)
        write(funit, '(a)') '! .f90 FORTAX Price index; END-OF-FILE'
        write(funit, *)

        if (present(fname)) close(funit)

    end subroutine rpi_saveF90

end module fortax_prices
