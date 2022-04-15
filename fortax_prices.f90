
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

        use fortax_util, only : getunit, fortaxerror, fortaxwarn, inttostr
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

        call getunit(funit)

        if (present(fname)) then
            inquire(file = fname, exist = isfile)
            if (isfile) then
                open(funit, file = fname, status = 'old')
            else
                call fortaxerror('price index file does not exist (' // trim(adjustl(fname)) // ')')
            end if
        else
            inquire(file = 'prices/rpi.csv', exist = isfile)
            if (isfile) then
                open (funit, file = 'prices/rpi.csv', status = 'old')
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

        use fortax_util, only : getunit, fortaxerror, inttostr
        use fortax_type, only : sysindex_t

        implicit none

        type(sysindex_t), intent(out) :: sysindex
        character(len = *), intent(in), optional :: sysindexfile

        integer :: funit
        integer :: istat, nrec

        logical :: isfile
        integer :: tempdate0, tempdate1, ndate
        character(len = 256) :: tempfname

        call getunit(funit)

        if (present(sysindexfile)) then
            inquire(file = sysindexfile, exist = isfile)
            if (isfile) then
                open (funit, file = sysindexfile, status = 'old')
            else
                call fortaxerror('system index file does not exist (' // sysindexfile // ')')
            end if
        else
            inquire(file = 'systems/sysindex.csv', exist = isfile)
            if (isfile) then
                open (funit, file = 'systems/sysindex.csv', status = 'old')
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
                sysindex%date0(nrec) = tempdate0
                sysindex%date1(nrec) = tempdate1
                sysindex%fname(nrec) = tempfname
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

    subroutine getsysindex(sysindex, date, systemformat, sysfilepath, sysnum)

        use fortax_util, only : lower, fortaxerror
        use fortax_type, only : sysindex_t

        implicit none

        type(sysindex_t), intent(in)  :: sysindex
        integer, intent(in) :: date
        character(len = *), intent(in)  :: systemformat
        character(len = 256), intent(out) :: sysfilepath
        integer, intent(out) :: sysnum

        integer :: i
        character(len = 4) :: fext !extension
        character(len = 7) :: fsub !subdirectory

        if (sysindex%nsys == 0) then
            call fortaxerror('system index file is not in memory')
        end if

        select case(lower(systemformat))
            case('taxben')
                fsub = 'taxben/'
                fext = '.bp3'
            case('fortax')
                fsub = 'fortax/'
                fext = '.xml'
            case default
                call fortaxerror('Unknown system format specified (' // systemformat // ') in getsysindex')
        end select

        if (checkdate(date)) then
            sysnum = 0
            do i = 1, sysindex%nsys
                if (date >= sysindex%date0(i) .and. date <= sysindex%date1(i)) then
                    sysfilepath = adjustl('systems/' // trim(fsub) // trim(sysindex%fname(i)) // trim(fext))
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

        use fortax_util, only : getUnit, fortaxError, intToStr, dblToStr
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
            call getUnit(funit)
            open(funit, file = fname, action = 'write', status = 'replace', iostat = ios)
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
