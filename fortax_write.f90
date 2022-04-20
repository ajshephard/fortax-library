
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




! fortax_write
! -----------------------------------------------------------------------
! module for saving (in json format) and printing of system files
! code will reflect any changes to the structure of the system, AS


module fortax_write

    use fortax_realtype, only : dp
    private :: dp

    private
    public  :: fortaxPrint, fortaxWrite

contains

    ! fortaxPrint
    ! -----------------------------------------------------------------------
    ! outputs a summary of the tax system to the default output unit if fname
    ! is not specified. Otherwise, this output summary will be written to
    ! disk with file name fname. This printing code is self-maintaining

    subroutine fortaxPrint(sys, fname)

        use fortax_type, only : sys_t
        use fortax_util, only : upper, fortaxError
        use, intrinsic :: iso_fortran_env

        implicit none

        type(sys_t),      intent(in)           :: sys
        character(len=*), intent(in), optional :: fname
        character(len = 64) :: sysname
        character(len = 512) :: sysdesc
        integer :: funit, ios

        sysname = transfer(sys%sysname, sysname)
        sysdesc = transfer(sys%sysdesc, sysdesc)

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        if ( sysname .ne. "" ) then
            write(funit,*)
            write(funit,'(1X,(A))') 'SYSNAME:'
            write(funit,'(1X,(A))') trim(sysname)
        end if

        if ( sysdesc .ne. "" ) then
            write(funit,*)
            write(funit,'(1X,(A))') 'SYSDESC:'
            write(funit,'(1X,(A))') trim(sysdesc)
        end if

        if (present(fname)) close(funit)

        return

    end subroutine fortaxPrint


    ! writeFortaxParams
    ! -----------------------------------------------------------------------
    ! writes the system file sys to disk with file name fname in the native
    ! FORTAX file format. This writing code is self-maintaining.

    subroutine writeFortaxParams(sys, fname)

        use fortax_type
        use fortax_util, only : fortaxError
        use json_module

        implicit none

        type(sys_t), intent(in) :: sys
        character(len = *), intent(in) :: fname

        type(json_core) :: json
        type(json_value), pointer :: p, inp

        character(len = len_sysname) :: sysname
        character(len = len_sysdesc) :: sysdesc
        logical :: status_ok
        character(len = :), allocatable :: error_msg

        sysname = transfer(sys%sysname, sysname)
        sysdesc = transfer(sys%sysdesc, sysdesc)

        ! initialize the class
        call json%initialize()

        ! initialize the structure:
        call json%create_object(p, '')

        call json%add(p, 'sysname', trim(adjustl(sysname)))
        call json%add(p, 'sysdesc', trim(adjustl(sysdesc)))

        call json%create_object(inp, "inctax")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "numbands", sys%inctax%numbands)
        call json%add(inp, "pa", sys%inctax%pa)
        call json%add(inp, "doPATaper", sys%inctax%doPATaper)
        call json%add(inp, "disablePATaperRounding", sys%inctax%disablePATaperRounding)
        call json%add(inp, "paTaperThresh", sys%inctax%paTaperThresh)
        call json%add(inp, "paTaperRate", sys%inctax%paTaperRate)
        call json%add(inp, "mma", sys%inctax%mma)
        call json%add(inp, "ctc", sys%inctax%ctc)
        call json%add(inp, "ctcyng", sys%inctax%ctcyng)
        call json%add(inp, "mmarate", sys%inctax%mmarate)
        call json%add(inp, "ctctaper", sys%inctax%ctctaper)
        call json%add(inp, "c4rebate", sys%inctax%c4rebate)
        call json%add(inp, "bands", sys%inctax%bands(1:sys%inctax%numbands))
        call json%add(inp, "rates", sys%inctax%rates(1:sys%inctax%numbands))
        nullify(inp)

        call json%create_object(inp, "natins")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "numrates", sys%natins%numrates)
        call json%add(inp, "c4nrates", sys%natins%c4nrates)
        call json%add(inp, "c2floor", sys%natins%c2floor)
        call json%add(inp, "c2rate", sys%natins%c2rate)
        call json%add(inp, "ceiling", sys%natins%ceiling)
        call json%add(inp, "rates", sys%natins%rates(1:sys%natins%numrates))
        call json%add(inp, "bands", sys%natins%bands(1:sys%natins%numrates))
        call json%add(inp, "c4rates", sys%natins%c4rates(1:sys%natins%c4nrates))
        call json%add(inp, "c4bands", sys%natins%c4bands(1:sys%natins%c4nrates))
        nullify(inp)

        call json%create_object(inp, "chben")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "doChBen", sys%chben%doChBen)
        call json%add(inp, "basic", sys%chben%basic)
        call json%add(inp, "kid1xtr", sys%chben%kid1xtr)
        call json%add(inp, "opf", sys%chben%opf)
        call json%add(inp, "MatGrantVal", sys%chben%MatGrantVal)
        call json%add(inp, "MatGrantOnlyFirstKid", sys%chben%MatGrantOnlyFirstKid)
        call json%add(inp, "doTaper", sys%chben%doTaper)
        call json%add(inp, "disableTaperRounding", sys%chben%disableTaperRounding)
        call json%add(inp, "taperStart", sys%chben%taperStart)
        call json%add(inp, "taperRate", sys%chben%taperRate)
        call json%add(inp, "taperIsIncTax", sys%chben%taperIsIncTax)
        nullify(inp)

        call json%create_object(inp, "fc")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "dofamcred", sys%fc%dofamcred)
        call json%add(inp, "NumAgeRng", sys%fc%NumAgeRng)
        call json%add(inp, "MaxAgeCC", sys%fc%MaxAgeCC)
        call json%add(inp, "WFTCMaxAgeCC", sys%fc%WFTCMaxAgeCC)
        call json%add(inp, "adult", sys%fc%adult)
        call json%add(inp, "ftprem", sys%fc%ftprem)
        call json%add(inp, "hours1", sys%fc%hours1)
        call json%add(inp, "hours2", sys%fc%hours2)
        call json%add(inp, "thres", sys%fc%thres)
        call json%add(inp, "taper", sys%fc%taper)
        call json%add(inp, "MaintDisreg", sys%fc%MaintDisreg)
        call json%add(inp, "MaxCC1", sys%fc%MaxCC1)
        call json%add(inp, "MaxCC2", sys%fc%MaxCC2)
        call json%add(inp, "WFTCMaxCC1", sys%fc%WFTCMaxCC1)
        call json%add(inp, "WFTCMaxCC2", sys%fc%WFTCMaxCC2)
        call json%add(inp, "WFTCPropCC", sys%fc%WFTCPropCC)
        call json%add(inp, "MinAmt", sys%fc%MinAmt)
        call json%add(inp, "kidagel", sys%fc%kidagel(1:sys%fc%NumAgeRng))
        call json%add(inp, "kidageu", sys%fc%kidageu(1:sys%fc%NumAgeRng))
        call json%add(inp, "kidcred", sys%fc%kidcred(1:sys%fc%NumAgeRng))
        nullify(inp)

        call json%create_object(inp, "ctc")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "fam", sys%ctc%fam)
        call json%add(inp, "baby", sys%ctc%baby)
        call json%add(inp, "kid", sys%ctc%kid)
        nullify(inp)

        call json%create_object(inp, "wtc")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "Basic", sys%wtc%Basic)
        call json%add(inp, "CouLP", sys%wtc%CouLP)
        call json%add(inp, "FT", sys%wtc%FT)
        call json%add(inp, "MinHrsKids", sys%wtc%MinHrsKids)
        call json%add(inp, "MinHrsCouKids", sys%wtc%MinHrsCouKids)
        call json%add(inp, "MinHrsNoKids", sys%wtc%MinHrsNoKids)
        call json%add(inp, "FTHrs", sys%wtc%FTHrs)
        call json%add(inp, "MinAgeKids", sys%wtc%MinAgeKids)
        call json%add(inp, "MinAgeNoKids", sys%wtc%MinAgeNoKids)
        call json%add(inp, "MaxCC1", sys%wtc%MaxCC1)
        call json%add(inp, "MaxCC2", sys%wtc%MaxCC2)
        call json%add(inp, "PropCC", sys%wtc%PropCC)
        call json%add(inp, "MaxAgeCC", sys%wtc%MaxAgeCC)
        call json%add(inp, "NewDisreg", sys%wtc%NewDisreg)
        call json%add(inp, "NewDisregCon", sys%wtc%NewDisregCon)
        nullify(inp)

        call json%create_object(inp, "ntc")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "donewtaxcred", sys%ntc%donewtaxcred)
        call json%add(inp, "thr1lo", sys%ntc%thr1lo)
        call json%add(inp, "thr1hi", sys%ntc%thr1hi)
        call json%add(inp, "thr2", sys%ntc%thr2)
        call json%add(inp, "taper1", sys%ntc%taper1)
        call json%add(inp, "taper2", sys%ntc%taper2)
        call json%add(inp, "taperCTCInOneGo", sys%ntc%taperCTCInOneGo)
        call json%add(inp, "MinAmt", sys%ntc%MinAmt)
        nullify(inp)

        call json%create_object(inp, "incsup")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "doIncSup", sys%incsup%doIncSup)
        call json%add(inp, "IncChben", sys%incsup%IncChben)
        call json%add(inp, "NumAgeRng", sys%incsup%NumAgeRng)
        call json%add(inp, "MainCou", sys%incsup%MainCou)
        call json%add(inp, "YngCou", sys%incsup%YngCou)
        call json%add(inp, "MainLP", sys%incsup%MainLP)
        call json%add(inp, "YngLP", sys%incsup%YngLP)
        call json%add(inp, "MainSin", sys%incsup%MainSin)
        call json%add(inp, "YngSin", sys%incsup%YngSin)
        call json%add(inp, "ValFSM", sys%incsup%ValFSM)
        call json%add(inp, "DisregLP", sys%incsup%DisregLP)
        call json%add(inp, "DisregSin", sys%incsup%DisregSin)
        call json%add(inp, "DisregCou", sys%incsup%DisregCou)
        call json%add(inp, "DisregShared", sys%incsup%DisregShared)
        call json%add(inp, "PremFam", sys%incsup%PremFam)
        call json%add(inp, "PremLP", sys%incsup%PremLP)
        call json%add(inp, "hours", sys%incsup%hours)
        call json%add(inp, "MaintDisreg", sys%incsup%MaintDisreg)
        call json%add(inp, "AgeRngl", sys%incsup%AgeRngl(1:sys%incsup%NumAgeRng))
        call json%add(inp, "AgeRngu", sys%incsup%AgeRngu(1:sys%incsup%NumAgeRng))
        call json%add(inp, "AddKid", sys%incsup%AddKid(1:sys%incsup%NumAgeRng))
        nullify(inp)

        call json%create_object(inp, "ctax")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "docounciltax", sys%ctax%docounciltax)
        call json%add(inp, "bandD", sys%ctax%bandD)
        call json%add(inp, "SinDis", sys%ctax%SinDis)
        call json%add(inp, "RatioA", sys%ctax%RatioA)
        call json%add(inp, "RatioB", sys%ctax%RatioB)
        call json%add(inp, "RatioC", sys%ctax%RatioC)
        call json%add(inp, "RatioE", sys%ctax%RatioE)
        call json%add(inp, "RatioF", sys%ctax%RatioF)
        call json%add(inp, "RatioG", sys%ctax%RatioG)
        call json%add(inp, "RatioH", sys%ctax%RatioH)
        nullify(inp)

        call json%create_object(inp, "rebatesys")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "RulesUnderFC", sys%rebatesys%RulesUnderFC)
        call json%add(inp, "RulesUnderWFTC", sys%rebatesys%RulesUnderWFTC)
        call json%add(inp, "RulesUnderNTC", sys%rebatesys%RulesUnderNTC)
        call json%add(inp, "RulesUnderUC", sys%rebatesys%RulesUnderUC)
        call json%add(inp, "NumAgeRng", sys%rebatesys%NumAgeRng)
        call json%add(inp, "Restrict", sys%rebatesys%Restrict)
        call json%add(inp, "docap", sys%rebatesys%docap)
        call json%add(inp, "MainCou", sys%rebatesys%MainCou)
        call json%add(inp, "YngCou", sys%rebatesys%YngCou)
        call json%add(inp, "MainLP", sys%rebatesys%MainLP)
        call json%add(inp, "YngLP", sys%rebatesys%YngLP)
        call json%add(inp, "MainSin", sys%rebatesys%MainSin)
        call json%add(inp, "YngSin", sys%rebatesys%YngSin)
        call json%add(inp, "DisregSin", sys%rebatesys%DisregSin)
        call json%add(inp, "DisregLP", sys%rebatesys%DisregLP)
        call json%add(inp, "DisregCou", sys%rebatesys%DisregCou)
        call json%add(inp, "CredInDisregCC", sys%rebatesys%CredInDisregCC)
        call json%add(inp, "ChbenIsIncome", sys%rebatesys%ChbenIsIncome)
        call json%add(inp, "PremFam", sys%rebatesys%PremFam)
        call json%add(inp, "PremLP", sys%rebatesys%PremLP)
        call json%add(inp, "MaintDisreg", sys%rebatesys%MaintDisreg)
        call json%add(inp, "MaxCC1", sys%rebatesys%MaxCC1)
        call json%add(inp, "MaxCC2", sys%rebatesys%MaxCC2)
        call json%add(inp, "MaxAgeCC", sys%rebatesys%MaxAgeCC)
        call json%add(inp, "AgeRngl", sys%rebatesys%AgeRngl(1:sys%rebatesys%NumAgeRng))
        call json%add(inp, "AgeRngu", sys%rebatesys%AgeRngu(1:sys%rebatesys%NumAgeRng))
        call json%add(inp, "AddKid", sys%rebatesys%AddKid(1:sys%rebatesys%NumAgeRng))
        nullify(inp)

        call json%create_object(inp, "hben")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "doHBen", sys%hben%doHBen)
        call json%add(inp, "taper", sys%hben%taper)
        call json%add(inp, "MinAmt", sys%hben%MinAmt)
        nullify(inp)

        call json%create_object(inp, "ctaxben")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "docounciltaxben", sys%ctaxben%docounciltaxben)
        call json%add(inp, "taper", sys%ctaxben%taper)
        call json%add(inp, "doEntitlementCut", sys%ctaxben%doEntitlementCut)
        call json%add(inp, "entitlementShare", sys%ctaxben%entitlementShare)
        nullify(inp)

        call json%create_object(inp, "ccben")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "dopolltax", sys%ccben%dopolltax)
        call json%add(inp, "taper", sys%ccben%taper)
        call json%add(inp, "PropElig", sys%ccben%PropElig)
        call json%add(inp, "MinAmt", sys%ccben%MinAmt)
        call json%add(inp, "CCrate", sys%ccben%CCrate)
        nullify(inp)

        call json%create_object(inp, "uc")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "doUnivCred", sys%uc%doUnivCred)
        call json%add(inp, "MainCou", sys%uc%MainCou)
        call json%add(inp, "YngCou", sys%uc%YngCou)
        call json%add(inp, "MainSin", sys%uc%MainSin)
        call json%add(inp, "YngSin", sys%uc%YngSin)
        call json%add(inp, "MinAgeMain", sys%uc%MinAgeMain)
        call json%add(inp, "FirstKid", sys%uc%FirstKid)
        call json%add(inp, "OtherKid", sys%uc%OtherKid)
        call json%add(inp, "MaxCC1", sys%uc%MaxCC1)
        call json%add(inp, "MaxCC2", sys%uc%MaxCC2)
        call json%add(inp, "PropCC", sys%uc%PropCC)
        call json%add(inp, "MaxAgeCC", sys%uc%MaxAgeCC)
        call json%add(inp, "doRentCap", sys%uc%doRentCap)
        call json%add(inp, "DisregSinNoKidsHi", sys%uc%DisregSinNoKidsHi)
        call json%add(inp, "DisregSinNoKidsLo", sys%uc%DisregSinNoKidsLo)
        call json%add(inp, "DisregSinKidsHi", sys%uc%DisregSinKidsHi)
        call json%add(inp, "DisregSinKidsLo", sys%uc%DisregSinKidsLo)
        call json%add(inp, "DisregCouNoKidsHi", sys%uc%DisregCouNoKidsHi)
        call json%add(inp, "DisregCouNoKidsLo", sys%uc%DisregCouNoKidsLo)
        call json%add(inp, "DisregCouKidsHi", sys%uc%DisregCouKidsHi)
        call json%add(inp, "DisregCouKidsLo", sys%uc%DisregCouKidsLo)
        call json%add(inp, "taper", sys%uc%taper)
        call json%add(inp, "MinAmt", sys%uc%MinAmt)
        nullify(inp)

        call json%create_object(inp, "statepen")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "doStatePen", sys%statepen%doStatePen)
        call json%add(inp, "PenAgeMan", sys%statepen%PenAgeMan)
        call json%add(inp, "PenAgeWoman", sys%statepen%PenAgeWoman)
        nullify(inp)

        call json%create_object(inp, "bencap")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "doCap", sys%bencap%doCap)
        call json%add(inp, "doThruUC", sys%bencap%doThruUC)
        call json%add(inp, "sinNoKids", sys%bencap%sinNoKids)
        call json%add(inp, "sinKids", sys%bencap%sinKids)
        call json%add(inp, "couNoKids", sys%bencap%couNoKids)
        call json%add(inp, "couKids", sys%bencap%couKids)
        call json%add(inp, "UCEarnThr", sys%bencap%UCEarnThr)
        nullify(inp)

        call json%create_object(inp, "extra")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "fsminappamt", sys%extra%fsminappamt)
        call json%add(inp, "matgrant", sys%extra%matgrant)
        call json%add(inp, "prices", sys%extra%prices)
        nullify(inp)


        ! write the file:
        call json%print(p, fname)

        if (json%failed()) then
            call json%check_for_errors(status_ok, error_msg)
            call json%clear_exceptions()
            call json%destroy(p)
            call fortaxError(trim(adjustl(error_msg)))
        end if

        !cleanup:
        call json%destroy(p)
        if (json%failed()) stop 1

        return

    end subroutine writeFortaxParams

end module fortax_write
