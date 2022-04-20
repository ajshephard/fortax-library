
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




! fortax_read
! -----------------------------------------------------------------------
! module provides the main system file reading functionality, AS


module fortax_read

    use fortax_realtype, only : dp
    private :: dp

    private
    public  :: readFortaxParams

contains

    ! readFortaxParams
    ! -----------------------------------------------------------------------
    ! reads tax parameters from systemfile into a sys_t derived type. It
    ! supports files in the TAXBEN file format, as well as the native format
    ! used by FORTAX.

    subroutine readFortaxParams(sys, systemFile, prices)

        use fortax_util, only : strToDouble, strToInt, strToLogical, lower, fortaxError, fortaxWarn
        use fortax_type
        use json_module

        implicit none

        type(sys_t), intent(out) :: sys
        character(len = *), intent(in) :: systemFile
        integer, optional, intent(in) :: prices

        type(json_file) :: json

        integer, allocatable :: integer_array(:)
        real(dp), allocatable :: double_array(:)
        character(len = :), allocatable :: string
        character(len = len_sysname) :: sysname
        character(len = len_sysdesc) :: sysdesc
        logical :: found
        integer :: sz
        logical :: status_ok
        character(len = :), allocatable :: error_msg

        ! initialise the system
        ! (and value not in json file will retain default)
        call sys_init(sys)

        ! initialize the class
        call json%initialize()

        ! read the file
        call json%load(filename = systemFile)
        if (json%failed()) then
            call json%check_for_errors(status_ok, error_msg)
            call json%clear_exceptions()
            call fortaxError(trim(adjustl(error_msg)))
        end if

        call json%get("sysname", string, found)
        if (len(string) > len_sysname) then
            call fortaxError("json file: length sysname > len_sysname")
        else
            sysname = string
            sys%sysname = transfer(sysname, sys%sysname)
        end if

        call json%get("sysdesc", string, found)
        if (len(string) > len_sysdesc) then
            call fortaxError("json file: length sysdesc > len_sysdesc")
        else
            sysdesc = string
            sys%sysdesc = transfer(sysdesc, sys%sysdesc)
        end if

        call json%get("inctax.numbands", sys%inctax%numbands, found)
        call json%get("inctax.pa", sys%inctax%pa, found)
        call json%get("inctax.doPATaper", sys%inctax%doPATaper, found)
        call json%get("inctax.disablePATaperRounding", sys%inctax%disablePATaperRounding, found)
        call json%get("inctax.paTaperThresh", sys%inctax%paTaperThresh, found)
        call json%get("inctax.paTaperRate", sys%inctax%paTaperRate, found)
        call json%get("inctax.mma", sys%inctax%mma, found)
        call json%get("inctax.ctc", sys%inctax%ctc, found)
        call json%get("inctax.ctcyng", sys%inctax%ctcyng, found)
        call json%get("inctax.mmarate", sys%inctax%mmarate, found)
        call json%get("inctax.ctctaper", sys%inctax%ctctaper, found)
        call json%get("inctax.c4rebate", sys%inctax%c4rebate, found)
        call json%get("inctax.bands", double_array, found)
        if (found) then
            sz = size(double_array)
            if (sz > MaxIncTaxBands) then
                call fortaxError("json file: length inctax.bands > MaxIncTaxBands")
            else
                sys%inctax%bands(1:sz) = double_array
            endif
        end if
        call json%get("inctax.rates", double_array, found)
        if (found) then
            sz = size(double_array)
            if (sz > MaxIncTaxBands) then
                call fortaxError("json file: length inctax.rates > MaxIncTaxBands")
            else
                sys%inctax%rates(1:sz) = double_array
            endif
        end if
        call json%get("natins.numrates", sys%natins%numrates, found)
        call json%get("natins.c4nrates", sys%natins%c4nrates, found)
        call json%get("natins.c2floor", sys%natins%c2floor, found)
        call json%get("natins.c2rate", sys%natins%c2rate, found)
        call json%get("natins.ceiling", sys%natins%ceiling, found)
        call json%get("natins.rates", double_array, found)
        if (found) then
            sz = size(double_array)
            if (sz > MaxNatInsBands) then
                call fortaxError("json file: length natins.rates > MaxNatInsBands")
            else
                sys%natins%rates(1:sz) = double_array
            endif
        end if
        call json%get("natins.bands", double_array, found)
        if (found) then
            sz = size(double_array)
            if (sz > MaxNatInsBands) then
                call fortaxError("json file: length natins.bands > MaxNatInsBands")
            else
                sys%natins%bands(1:sz) = double_array
            endif
        end if
        call json%get("natins.c4rates", double_array, found)
        if (found) then
            sz = size(double_array)
            if (sz > MaxNatInsC4Bands) then
                call fortaxError("json file: length natins.c4rates > MaxNatInsC4Bands")
            else
                sys%natins%c4rates(1:sz) = double_array
            endif
        end if
        call json%get("natins.c4bands", double_array, found)
        if (found) then
            sz = size(double_array)
            if (sz > MaxNatInsC4Bands) then
                call fortaxError("json file: length natins.c4bands > MaxNatInsC4Bands")
            else
                sys%natins%c4bands(1:sz) = double_array
            endif
        end if
        call json%get("chben.doChBen", sys%chben%doChBen, found)
        call json%get("chben.basic", sys%chben%basic, found)
        call json%get("chben.kid1xtr", sys%chben%kid1xtr, found)
        call json%get("chben.opf", sys%chben%opf, found)
        call json%get("chben.MatGrantVal", sys%chben%MatGrantVal, found)
        call json%get("chben.MatGrantOnlyFirstKid", sys%chben%MatGrantOnlyFirstKid, found)
        call json%get("chben.doTaper", sys%chben%doTaper, found)
        call json%get("chben.disableTaperRounding", sys%chben%disableTaperRounding, found)
        call json%get("chben.taperStart", sys%chben%taperStart, found)
        call json%get("chben.taperRate", sys%chben%taperRate, found)
        call json%get("chben.taperIsIncTax", sys%chben%taperIsIncTax, found)
        call json%get("fc.dofamcred", sys%fc%dofamcred, found)
        call json%get("fc.NumAgeRng", sys%fc%NumAgeRng, found)
        call json%get("fc.MaxAgeCC", sys%fc%MaxAgeCC, found)
        call json%get("fc.WFTCMaxAgeCC", sys%fc%WFTCMaxAgeCC, found)
        call json%get("fc.adult", sys%fc%adult, found)
        call json%get("fc.ftprem", sys%fc%ftprem, found)
        call json%get("fc.hours1", sys%fc%hours1, found)
        call json%get("fc.hours2", sys%fc%hours2, found)
        call json%get("fc.thres", sys%fc%thres, found)
        call json%get("fc.taper", sys%fc%taper, found)
        call json%get("fc.MaintDisreg", sys%fc%MaintDisreg, found)
        call json%get("fc.MaxCC1", sys%fc%MaxCC1, found)
        call json%get("fc.MaxCC2", sys%fc%MaxCC2, found)
        call json%get("fc.WFTCMaxCC1", sys%fc%WFTCMaxCC1, found)
        call json%get("fc.WFTCMaxCC2", sys%fc%WFTCMaxCC2, found)
        call json%get("fc.WFTCPropCC", sys%fc%WFTCPropCC, found)
        call json%get("fc.MinAmt", sys%fc%MinAmt, found)
        call json%get("fc.kidagel", integer_array, found)
        if (found) then
            sz = size(integer_array)
            if (sz > MaxNumAgeRng) then
                call fortaxError("json file: length fc.kidagel > MaxNumAgeRng")
            else
                sys%fc%kidagel(1:sz) = integer_array
            endif
        end if
        call json%get("fc.kidageu", integer_array, found)
        if (found) then
            sz = size(integer_array)
            if (sz > MaxNumAgeRng) then
                call fortaxError("json file: length fc.kidageu > MaxNumAgeRng")
            else
                sys%fc%kidageu(1:sz) = integer_array
            endif
        end if
        call json%get("fc.kidcred", double_array, found)
        if (found) then
            sz = size(double_array)
            if (sz > MaxNumAgeRng) then
                call fortaxError("json file: length fc.kidcred > MaxNumAgeRng")
            else
                sys%fc%kidcred(1:sz) = double_array
            endif
        end if
        call json%get("ctc.fam", sys%ctc%fam, found)
        call json%get("ctc.baby", sys%ctc%baby, found)
        call json%get("ctc.kid", sys%ctc%kid, found)
        call json%get("wtc.Basic", sys%wtc%Basic, found)
        call json%get("wtc.CouLP", sys%wtc%CouLP, found)
        call json%get("wtc.FT", sys%wtc%FT, found)
        call json%get("wtc.MinHrsKids", sys%wtc%MinHrsKids, found)
        call json%get("wtc.MinHrsCouKids", sys%wtc%MinHrsCouKids, found)
        call json%get("wtc.MinHrsNoKids", sys%wtc%MinHrsNoKids, found)
        call json%get("wtc.FTHrs", sys%wtc%FTHrs, found)
        call json%get("wtc.MinAgeKids", sys%wtc%MinAgeKids, found)
        call json%get("wtc.MinAgeNoKids", sys%wtc%MinAgeNoKids, found)
        call json%get("wtc.MaxCC1", sys%wtc%MaxCC1, found)
        call json%get("wtc.MaxCC2", sys%wtc%MaxCC2, found)
        call json%get("wtc.PropCC", sys%wtc%PropCC, found)
        call json%get("wtc.MaxAgeCC", sys%wtc%MaxAgeCC, found)
        call json%get("wtc.NewDisreg", sys%wtc%NewDisreg, found)
        call json%get("wtc.NewDisregCon", sys%wtc%NewDisregCon, found)
        call json%get("ntc.donewtaxcred", sys%ntc%donewtaxcred, found)
        call json%get("ntc.thr1lo", sys%ntc%thr1lo, found)
        call json%get("ntc.thr1hi", sys%ntc%thr1hi, found)
        call json%get("ntc.thr2", sys%ntc%thr2, found)
        call json%get("ntc.taper1", sys%ntc%taper1, found)
        call json%get("ntc.taper2", sys%ntc%taper2, found)
        call json%get("ntc.taperCTCInOneGo", sys%ntc%taperCTCInOneGo, found)
        call json%get("ntc.MinAmt", sys%ntc%MinAmt, found)
        call json%get("incsup.doIncSup", sys%incsup%doIncSup, found)
        call json%get("incsup.IncChben", sys%incsup%IncChben, found)
        call json%get("incsup.NumAgeRng", sys%incsup%NumAgeRng, found)
        call json%get("incsup.MainCou", sys%incsup%MainCou, found)
        call json%get("incsup.YngCou", sys%incsup%YngCou, found)
        call json%get("incsup.MainLP", sys%incsup%MainLP, found)
        call json%get("incsup.YngLP", sys%incsup%YngLP, found)
        call json%get("incsup.MainSin", sys%incsup%MainSin, found)
        call json%get("incsup.YngSin", sys%incsup%YngSin, found)
        call json%get("incsup.ValFSM", sys%incsup%ValFSM, found)
        call json%get("incsup.DisregLP", sys%incsup%DisregLP, found)
        call json%get("incsup.DisregSin", sys%incsup%DisregSin, found)
        call json%get("incsup.DisregCou", sys%incsup%DisregCou, found)
        call json%get("incsup.DisregShared", sys%incsup%DisregShared, found)
        call json%get("incsup.PremFam", sys%incsup%PremFam, found)
        call json%get("incsup.PremLP", sys%incsup%PremLP, found)
        call json%get("incsup.hours", sys%incsup%hours, found)
        call json%get("incsup.MaintDisreg", sys%incsup%MaintDisreg, found)
        call json%get("incsup.AgeRngl", integer_array, found)
        if (found) then
            sz = size(integer_array)
            if (sz > MaxNumAgeRng) then
                call fortaxError("json file: length incsup.AgeRngl > MaxNumAgeRng")
            else
                sys%incsup%AgeRngl(1:sz) = integer_array
            endif
        end if
        call json%get("incsup.AgeRngu", integer_array, found)
        if (found) then
            sz = size(integer_array)
            if (sz > MaxNumAgeRng) then
                call fortaxError("json file: length incsup.AgeRngu > MaxNumAgeRng")
            else
                sys%incsup%AgeRngu(1:sz) = integer_array
            endif
        end if
        call json%get("incsup.AddKid", double_array, found)
        if (found) then
            sz = size(double_array)
            if (sz > MaxNumAgeRng) then
                call fortaxError("json file: length incsup.AddKid > MaxNumAgeRng")
            else
                sys%incsup%AddKid(1:sz) = double_array
            endif
        end if
        call json%get("ctax.docounciltax", sys%ctax%docounciltax, found)
        call json%get("ctax.bandD", sys%ctax%bandD, found)
        call json%get("ctax.SinDis", sys%ctax%SinDis, found)
        call json%get("ctax.RatioA", sys%ctax%RatioA, found)
        call json%get("ctax.RatioB", sys%ctax%RatioB, found)
        call json%get("ctax.RatioC", sys%ctax%RatioC, found)
        call json%get("ctax.RatioE", sys%ctax%RatioE, found)
        call json%get("ctax.RatioF", sys%ctax%RatioF, found)
        call json%get("ctax.RatioG", sys%ctax%RatioG, found)
        call json%get("ctax.RatioH", sys%ctax%RatioH, found)
        call json%get("rebatesys.RulesUnderFC", sys%rebatesys%RulesUnderFC, found)
        call json%get("rebatesys.RulesUnderWFTC", sys%rebatesys%RulesUnderWFTC, found)
        call json%get("rebatesys.RulesUnderNTC", sys%rebatesys%RulesUnderNTC, found)
        call json%get("rebatesys.RulesUnderUC", sys%rebatesys%RulesUnderUC, found)
        call json%get("rebatesys.NumAgeRng", sys%rebatesys%NumAgeRng, found)
        call json%get("rebatesys.Restrict", sys%rebatesys%Restrict, found)
        call json%get("rebatesys.docap", sys%rebatesys%docap, found)
        call json%get("rebatesys.MainCou", sys%rebatesys%MainCou, found)
        call json%get("rebatesys.YngCou", sys%rebatesys%YngCou, found)
        call json%get("rebatesys.MainLP", sys%rebatesys%MainLP, found)
        call json%get("rebatesys.YngLP", sys%rebatesys%YngLP, found)
        call json%get("rebatesys.MainSin", sys%rebatesys%MainSin, found)
        call json%get("rebatesys.YngSin", sys%rebatesys%YngSin, found)
        call json%get("rebatesys.DisregSin", sys%rebatesys%DisregSin, found)
        call json%get("rebatesys.DisregLP", sys%rebatesys%DisregLP, found)
        call json%get("rebatesys.DisregCou", sys%rebatesys%DisregCou, found)
        call json%get("rebatesys.CredInDisregCC", sys%rebatesys%CredInDisregCC, found)
        call json%get("rebatesys.ChbenIsIncome", sys%rebatesys%ChbenIsIncome, found)
        call json%get("rebatesys.PremFam", sys%rebatesys%PremFam, found)
        call json%get("rebatesys.PremLP", sys%rebatesys%PremLP, found)
        call json%get("rebatesys.MaintDisreg", sys%rebatesys%MaintDisreg, found)
        call json%get("rebatesys.MaxCC1", sys%rebatesys%MaxCC1, found)
        call json%get("rebatesys.MaxCC2", sys%rebatesys%MaxCC2, found)
        call json%get("rebatesys.MaxAgeCC", sys%rebatesys%MaxAgeCC, found)
        call json%get("rebatesys.AgeRngl", integer_array, found)
        if (found) then
            sz = size(integer_array)
            if (sz > MaxNumAgeRng) then
                call fortaxError("json file: length rebatesys.AgeRngl > MaxNumAgeRng")
            else
                sys%rebatesys%AgeRngl(1:sz) = integer_array
            endif
        end if
        call json%get("rebatesys.AgeRngu", integer_array, found)
        if (found) then
            sz = size(integer_array)
            if (sz > MaxNumAgeRng) then
                call fortaxError("json file: length rebatesys.AgeRngu > MaxNumAgeRng")
            else
                sys%rebatesys%AgeRngu(1:sz) = integer_array
            endif
        end if
        call json%get("rebatesys.AddKid", double_array, found)
        if (found) then
            sz = size(double_array)
            if (sz > MaxNumAgeRng) then
                call fortaxError("json file: length rebatesys.AddKid > MaxNumAgeRng")
            else
                sys%rebatesys%AddKid(1:sz) = double_array
            endif
        end if
        call json%get("hben.doHBen", sys%hben%doHBen, found)
        call json%get("hben.taper", sys%hben%taper, found)
        call json%get("hben.MinAmt", sys%hben%MinAmt, found)
        call json%get("ctaxben.docounciltaxben", sys%ctaxben%docounciltaxben, found)
        call json%get("ctaxben.taper", sys%ctaxben%taper, found)
        call json%get("ctaxben.doEntitlementCut", sys%ctaxben%doEntitlementCut, found)
        call json%get("ctaxben.entitlementShare", sys%ctaxben%entitlementShare, found)
        call json%get("ccben.dopolltax", sys%ccben%dopolltax, found)
        call json%get("ccben.taper", sys%ccben%taper, found)
        call json%get("ccben.PropElig", sys%ccben%PropElig, found)
        call json%get("ccben.MinAmt", sys%ccben%MinAmt, found)
        call json%get("ccben.CCrate", sys%ccben%CCrate, found)
        call json%get("uc.doUnivCred", sys%uc%doUnivCred, found)
        call json%get("uc.MainCou", sys%uc%MainCou, found)
        call json%get("uc.YngCou", sys%uc%YngCou, found)
        call json%get("uc.MainSin", sys%uc%MainSin, found)
        call json%get("uc.YngSin", sys%uc%YngSin, found)
        call json%get("uc.MinAgeMain", sys%uc%MinAgeMain, found)
        call json%get("uc.FirstKid", sys%uc%FirstKid, found)
        call json%get("uc.OtherKid", sys%uc%OtherKid, found)
        call json%get("uc.MaxCC1", sys%uc%MaxCC1, found)
        call json%get("uc.MaxCC2", sys%uc%MaxCC2, found)
        call json%get("uc.PropCC", sys%uc%PropCC, found)
        call json%get("uc.MaxAgeCC", sys%uc%MaxAgeCC, found)
        call json%get("uc.doRentCap", sys%uc%doRentCap, found)
        call json%get("uc.DisregSinNoKidsHi", sys%uc%DisregSinNoKidsHi, found)
        call json%get("uc.DisregSinNoKidsLo", sys%uc%DisregSinNoKidsLo, found)
        call json%get("uc.DisregSinKidsHi", sys%uc%DisregSinKidsHi, found)
        call json%get("uc.DisregSinKidsLo", sys%uc%DisregSinKidsLo, found)
        call json%get("uc.DisregCouNoKidsHi", sys%uc%DisregCouNoKidsHi, found)
        call json%get("uc.DisregCouNoKidsLo", sys%uc%DisregCouNoKidsLo, found)
        call json%get("uc.DisregCouKidsHi", sys%uc%DisregCouKidsHi, found)
        call json%get("uc.DisregCouKidsLo", sys%uc%DisregCouKidsLo, found)
        call json%get("uc.taper", sys%uc%taper, found)
        call json%get("uc.MinAmt", sys%uc%MinAmt, found)
        call json%get("statepen.doStatePen", sys%statepen%doStatePen, found)
        call json%get("statepen.PenAgeMan", sys%statepen%PenAgeMan, found)
        call json%get("statepen.PenAgeWoman", sys%statepen%PenAgeWoman, found)
        call json%get("bencap.doCap", sys%bencap%doCap, found)
        call json%get("bencap.doThruUC", sys%bencap%doThruUC, found)
        call json%get("bencap.sinNoKids", sys%bencap%sinNoKids, found)
        call json%get("bencap.sinKids", sys%bencap%sinKids, found)
        call json%get("bencap.couNoKids", sys%bencap%couNoKids, found)
        call json%get("bencap.couKids", sys%bencap%couKids, found)
        call json%get("bencap.UCEarnThr", sys%bencap%UCEarnThr, found)
        call json%get("extra.fsminappamt", sys%extra%fsminappamt, found)
        call json%get("extra.matgrant", sys%extra%matgrant, found)
        call json%get("extra.prices", sys%extra%prices, found)

        ! clean up
        call json%destroy()
        if (json%failed()) then
            call fortaxError("Error clearing JSON.")
        end if

        if (present(prices)) sys%extra%prices = prices

    end subroutine readFortaxParams

end module fortax_read
