
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
    public  :: sys_desc, writeFortaxParams

contains

    ! printFortaxParams
    ! -----------------------------------------------------------------------
    ! outputs a summary of the tax system to the default output unit if fname
    ! is not specified. Otherwise, this output summary will be written to
    ! disk with file name fname. This printing code is self-maintaining

    subroutine sys_desc(sys, fname)

        use fortax_type!, only : sys_t
        use fortax_util, only : upper, fortaxError, strCentre, intToStr
        use, intrinsic :: iso_fortran_env

        implicit none

        type(sys_t),      intent(in)           :: sys
        character(len=*), intent(in), optional :: fname
        character(len = 64) :: sysname
        character(len = 512) :: sysdesc
        integer :: funit, ios, i

        sysname = transfer(sys%sysname, sysname)
        sysdesc = transfer(sys%sysdesc, sysdesc)

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit, *)
        write(funit, '(A)') repeat("=", 62)
        write(funit, '(A)') strCentre('sys_desc (' // trim(adjustl(sysname)) // '):', 62)
        if ( sysdesc .ne. "" ) then
            write(funit, '(A)') strCentre(trim(adjustl(sysdesc)), 62)
        end if
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "INCTAX:"
write(funit, '(A)')
        if (sys%inctax%numbands >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Number of income tax bands (numbands)", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "Number of income tax bands (numbands)", sys%inctax%numbands
        end if
        if (sys%inctax%pa >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Personal allowance (PA) (pa)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Personal allowance (PA) (pa)", sys%inctax%pa
        end if
        if (sys%inctax%doPATaper >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Taper PA (doPATaper)", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "Taper PA (doPATaper)", &
                  trim(adjustl(label_bool(sys%inctax%doPATaper))) 
        end if
        if (sys%inctax%disablePATaperRounding >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Disable rounding of PA tapered amount (disablePATaperRounding)", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "Disable rounding of PA tapered amount (disablePATaperRounding)", &
                  trim(adjustl(label_bool(sys%inctax%disablePATaperRounding))) 
        end if
        if (sys%inctax%paTaperThresh >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Threshold for PA taper (paTaperThresh)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Threshold for PA taper (paTaperThresh)", sys%inctax%paTaperThresh
        end if
        if (sys%inctax%paTaperRate >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "PA taper rate (paTaperRate)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "PA taper rate (paTaperRate)", sys%inctax%paTaperRate
        end if
        if (sys%inctax%doTPA >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Transferable marriage allowance (doTPA)", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "Transferable marriage allowance (doTPA)", &
                  trim(adjustl(label_bool(sys%inctax%doTPA))) 
        end if
        if (sys%inctax%maxTPA >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Max. transferable marriage allowance (maxTPA)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Max. transferable marriage allowance (maxTPA)", sys%inctax%maxTPA
        end if
        if (sys%inctax%mma >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Married couples allowance (mma)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Married couples allowance (mma)", sys%inctax%mma
        end if
        if (sys%inctax%ctc >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Children's tax credit (ctc)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Children's tax credit (ctc)", sys%inctax%ctc
        end if
        if (sys%inctax%ctcyng >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Children's tax credit young kid (ctcyng)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Children's tax credit young kid (ctcyng)", sys%inctax%ctcyng
        end if
        if (sys%inctax%mmarate >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Married couples allowance rate (mmarate)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Married couples allowance rate (mmarate)", sys%inctax%mmarate
        end if
        if (sys%inctax%ctctaper >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Children's tax credit taper (ctctaper)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Children's tax credit taper (ctctaper)", sys%inctax%ctctaper
        end if
        if (sys%inctax%c4rebate >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Rebate for Class 4 NI (c4rebate)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Rebate for Class 4 NI (c4rebate)", sys%inctax%c4rebate
        end if
        if (sys%inctax%bands(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Income tax bands (bands)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Income tax bands (bands)", sys%inctax%bands(1)
        end if
        do i = 2, sys%inctax%numbands
            if (sys%inctax%bands(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, F20.5)') "", sys%inctax%bands(i)
            end if
        end do
        if (sys%inctax%rates(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Income tax rates (rates)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Income tax rates (rates)", sys%inctax%rates(1)
        end if
        do i = 2, sys%inctax%numbands
            if (sys%inctax%rates(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, F20.5)') "", sys%inctax%rates(i)
            end if
        end do
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "NATINS:"
write(funit, '(A)')
        if (sys%natins%numrates >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "numrates", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "numrates", sys%natins%numrates
        end if
        if (sys%natins%c4nrates >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "c4nrates", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "c4nrates", sys%natins%c4nrates
        end if
        if (sys%natins%c2floor >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "c2floor", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "c2floor", sys%natins%c2floor
        end if
        if (sys%natins%c2rate >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "c2rate", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "c2rate", sys%natins%c2rate
        end if
        if (sys%natins%ceiling >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "ceiling", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "ceiling", sys%natins%ceiling
        end if
        if (sys%natins%rates(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "rates", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "rates", sys%natins%rates(1)
        end if
        do i = 2, sys%natins%numrates
            if (sys%natins%rates(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, F20.5)') "", sys%natins%rates(i)
            end if
        end do
        if (sys%natins%bands(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "bands", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "bands", sys%natins%bands(1)
        end if
        do i = 2, sys%natins%numrates
            if (sys%natins%bands(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, F20.5)') "", sys%natins%bands(i)
            end if
        end do
        if (sys%natins%c4rates(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "c4rates", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "c4rates", sys%natins%c4rates(1)
        end if
        do i = 2, sys%natins%c4nrates
            if (sys%natins%c4rates(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, F20.5)') "", sys%natins%c4rates(i)
            end if
        end do
        if (sys%natins%c4bands(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "c4bands", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "c4bands", sys%natins%c4bands(1)
        end if
        do i = 2, sys%natins%c4nrates
            if (sys%natins%c4bands(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, F20.5)') "", sys%natins%c4bands(i)
            end if
        end do
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "CHBEN:"
write(funit, '(A)')
        if (sys%chben%doChBen >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "doChBen", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "doChBen", &
                  trim(adjustl(label_bool(sys%chben%doChBen))) 
        end if
        if (sys%chben%basic >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "basic", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "basic", sys%chben%basic
        end if
        if (sys%chben%kid1xtr >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "kid1xtr", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "kid1xtr", sys%chben%kid1xtr
        end if
        if (sys%chben%opf >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "opf", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "opf", sys%chben%opf
        end if
        if (sys%chben%MatGrantVal >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MatGrantVal", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MatGrantVal", sys%chben%MatGrantVal
        end if
        if (sys%chben%MatGrantOnlyFirstKid >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MatGrantOnlyFirstKid", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "MatGrantOnlyFirstKid", &
                  trim(adjustl(label_bool(sys%chben%MatGrantOnlyFirstKid))) 
        end if
        if (sys%chben%doTaper >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "doTaper", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "doTaper", &
                  trim(adjustl(label_bool(sys%chben%doTaper))) 
        end if
        if (sys%chben%disableTaperRounding >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "disableTaperRounding", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "disableTaperRounding", &
                  trim(adjustl(label_bool(sys%chben%disableTaperRounding))) 
        end if
        if (sys%chben%taperStart >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "taperStart", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "taperStart", sys%chben%taperStart
        end if
        if (sys%chben%taperRate >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "taperRate", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "taperRate", sys%chben%taperRate
        end if
        if (sys%chben%taperIsIncTax >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "taperIsIncTax", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "taperIsIncTax", &
                  trim(adjustl(label_bool(sys%chben%taperIsIncTax))) 
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "FC:"
write(funit, '(A)')
        if (sys%fc%dofamcred >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "dofamcred", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "dofamcred", &
                  trim(adjustl(label_bool(sys%fc%dofamcred))) 
        end if
        if (sys%fc%NumAgeRng >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "NumAgeRng", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "NumAgeRng", sys%fc%NumAgeRng
        end if
        if (sys%fc%MaxAgeCC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxAgeCC", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MaxAgeCC", sys%fc%MaxAgeCC
        end if
        if (sys%fc%WFTCMaxAgeCC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "WFTCMaxAgeCC", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "WFTCMaxAgeCC", sys%fc%WFTCMaxAgeCC
        end if
        if (sys%fc%adult >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "adult", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "adult", sys%fc%adult
        end if
        if (sys%fc%ftprem >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "ftprem", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "ftprem", sys%fc%ftprem
        end if
        if (sys%fc%hours1 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "hours1", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "hours1", sys%fc%hours1
        end if
        if (sys%fc%hours2 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "hours2", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "hours2", sys%fc%hours2
        end if
        if (sys%fc%thres >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "thres", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "thres", sys%fc%thres
        end if
        if (sys%fc%taper >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "taper", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "taper", sys%fc%taper
        end if
        if (sys%fc%MaintDisreg >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaintDisreg", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaintDisreg", sys%fc%MaintDisreg
        end if
        if (sys%fc%MaxCC1 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxCC1", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaxCC1", sys%fc%MaxCC1
        end if
        if (sys%fc%MaxCC2 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxCC2", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaxCC2", sys%fc%MaxCC2
        end if
        if (sys%fc%WFTCMaxCC1 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "WFTCMaxCC1", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "WFTCMaxCC1", sys%fc%WFTCMaxCC1
        end if
        if (sys%fc%WFTCMaxCC2 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "WFTCMaxCC2", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "WFTCMaxCC2", sys%fc%WFTCMaxCC2
        end if
        if (sys%fc%WFTCPropCC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "WFTCPropCC", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "WFTCPropCC", sys%fc%WFTCPropCC
        end if
        if (sys%fc%MinAmt >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinAmt", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MinAmt", sys%fc%MinAmt
        end if
        if (sys%fc%kidagel(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "kidagel", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "kidagel", sys%fc%kidagel(1)
        end if
        do i = 2, sys%fc%NumAgeRng
            if (sys%fc%kidagel(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, I20)') "", sys%fc%kidagel(i)
            end if
        end do
        if (sys%fc%kidageu(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "kidageu", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "kidageu", sys%fc%kidageu(1)
        end if
        do i = 2, sys%fc%NumAgeRng
            if (sys%fc%kidageu(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, I20)') "", sys%fc%kidageu(i)
            end if
        end do
        if (sys%fc%kidcred(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "kidcred", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "kidcred", sys%fc%kidcred(1)
        end if
        do i = 2, sys%fc%NumAgeRng
            if (sys%fc%kidcred(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, F20.5)') "", sys%fc%kidcred(i)
            end if
        end do
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "CTC:"
write(funit, '(A)')
        if (sys%ctc%fam >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "fam", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "fam", sys%ctc%fam
        end if
        if (sys%ctc%baby >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "baby", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "baby", sys%ctc%baby
        end if
        if (sys%ctc%kid >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "kid", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "kid", sys%ctc%kid
        end if
        if (sys%ctc%maxKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "maxKids", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "maxKids", sys%ctc%maxKids
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "WTC:"
write(funit, '(A)')
        if (sys%wtc%Basic >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Basic", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Basic", sys%wtc%Basic
        end if
        if (sys%wtc%CouLP >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "CouLP", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "CouLP", sys%wtc%CouLP
        end if
        if (sys%wtc%FT >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "FT", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "FT", sys%wtc%FT
        end if
        if (sys%wtc%MinHrsKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinHrsKids", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MinHrsKids", sys%wtc%MinHrsKids
        end if
        if (sys%wtc%MinHrsCouKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinHrsCouKids", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MinHrsCouKids", sys%wtc%MinHrsCouKids
        end if
        if (sys%wtc%MinHrsNoKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinHrsNoKids", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MinHrsNoKids", sys%wtc%MinHrsNoKids
        end if
        if (sys%wtc%FTHrs >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "FTHrs", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "FTHrs", sys%wtc%FTHrs
        end if
        if (sys%wtc%MinAgeKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinAgeKids", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MinAgeKids", sys%wtc%MinAgeKids
        end if
        if (sys%wtc%MinAgeNoKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinAgeNoKids", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MinAgeNoKids", sys%wtc%MinAgeNoKids
        end if
        if (sys%wtc%MaxCC1 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxCC1", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaxCC1", sys%wtc%MaxCC1
        end if
        if (sys%wtc%MaxCC2 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxCC2", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaxCC2", sys%wtc%MaxCC2
        end if
        if (sys%wtc%PropCC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "PropCC", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "PropCC", sys%wtc%PropCC
        end if
        if (sys%wtc%MaxAgeCC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxAgeCC", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MaxAgeCC", sys%wtc%MaxAgeCC
        end if
        if (sys%wtc%NewDisreg >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "NewDisreg", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "NewDisreg", sys%wtc%NewDisreg
        end if
        if (sys%wtc%NewDisregCon >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "NewDisregCon", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "NewDisregCon", &
                  trim(adjustl(label_bool(sys%wtc%NewDisregCon))) 
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "NTC:"
write(funit, '(A)')
        if (sys%ntc%donewtaxcred >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "donewtaxcred", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "donewtaxcred", &
                  trim(adjustl(label_bool(sys%ntc%donewtaxcred))) 
        end if
        if (sys%ntc%thr1lo >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "thr1lo", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "thr1lo", sys%ntc%thr1lo
        end if
        if (sys%ntc%thr1hi >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "thr1hi", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "thr1hi", sys%ntc%thr1hi
        end if
        if (sys%ntc%thr2 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "thr2", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "thr2", sys%ntc%thr2
        end if
        if (sys%ntc%taper1 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "taper1", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "taper1", sys%ntc%taper1
        end if
        if (sys%ntc%taper2 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "taper2", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "taper2", sys%ntc%taper2
        end if
        if (sys%ntc%taperCTCInOneGo >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "taperCTCInOneGo", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "taperCTCInOneGo", &
                  trim(adjustl(label_bool(sys%ntc%taperCTCInOneGo))) 
        end if
        if (sys%ntc%MinAmt >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinAmt", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MinAmt", sys%ntc%MinAmt
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "CCTAXREFUND:"
write(funit, '(A)')
        if (sys%cctaxrefund%doCCTaxRefund >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "doCCTaxRefund", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "doCCTaxRefund", &
                  trim(adjustl(label_bool(sys%cctaxrefund%doCCTaxRefund))) 
        end if
        if (sys%cctaxrefund%MaxPerChild >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxPerChild", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaxPerChild", sys%cctaxrefund%MaxPerChild
        end if
        if (sys%cctaxrefund%MaxAge >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxAge", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MaxAge", sys%cctaxrefund%MaxAge
        end if
        if (sys%cctaxrefund%ReceiptProp >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "ReceiptProp", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "ReceiptProp", sys%cctaxrefund%ReceiptProp
        end if
        if (sys%cctaxrefund%MinEarn >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinEarn", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MinEarn", sys%cctaxrefund%MinEarn
        end if
        if (sys%cctaxrefund%MaxInc >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxInc", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaxInc", sys%cctaxrefund%MaxInc
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "INCSUP:"
write(funit, '(A)')
        if (sys%incsup%doIncSup >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "doIncSup", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "doIncSup", &
                  trim(adjustl(label_bool(sys%incsup%doIncSup))) 
        end if
        if (sys%incsup%IncChben >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "IncChben", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "IncChben", &
                  trim(adjustl(label_bool(sys%incsup%IncChben))) 
        end if
        if (sys%incsup%NumAgeRng >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "NumAgeRng", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "NumAgeRng", sys%incsup%NumAgeRng
        end if
        if (sys%incsup%MinAgeMain >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinAgeMain", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MinAgeMain", sys%incsup%MinAgeMain
        end if
        if (sys%incsup%MinAgeMainSin >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinAgeMainSin", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MinAgeMainSin", sys%incsup%MinAgeMainSin
        end if
        if (sys%incsup%MainCou >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MainCou", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MainCou", sys%incsup%MainCou
        end if
        if (sys%incsup%YngCou >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "YngCou", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "YngCou", sys%incsup%YngCou
        end if
        if (sys%incsup%MainLP >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MainLP", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MainLP", sys%incsup%MainLP
        end if
        if (sys%incsup%YngLP >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "YngLP", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "YngLP", sys%incsup%YngLP
        end if
        if (sys%incsup%MainSin >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MainSin", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MainSin", sys%incsup%MainSin
        end if
        if (sys%incsup%YngSin >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "YngSin", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "YngSin", sys%incsup%YngSin
        end if
        if (sys%incsup%MinAgeFSM >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinAgeFSM", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MinAgeFSM", sys%incsup%MinAgeFSM
        end if
        if (sys%incsup%MaxAgeUniversalFSM >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxAgeUniversalFSM", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MaxAgeUniversalFSM", sys%incsup%MaxAgeUniversalFSM
        end if
        if (sys%incsup%ValFSM >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "ValFSM", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "ValFSM", sys%incsup%ValFSM
        end if
        if (sys%incsup%DisregLP >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregLP", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregLP", sys%incsup%DisregLP
        end if
        if (sys%incsup%DisregSin >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregSin", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregSin", sys%incsup%DisregSin
        end if
        if (sys%incsup%DisregCou >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregCou", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregCou", sys%incsup%DisregCou
        end if
        if (sys%incsup%DisregShared >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregShared", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "DisregShared", &
                  trim(adjustl(label_bool(sys%incsup%DisregShared))) 
        end if
        if (sys%incsup%PremFam >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "PremFam", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "PremFam", sys%incsup%PremFam
        end if
        if (sys%incsup%PremLP >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "PremLP", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "PremLP", sys%incsup%PremLP
        end if
        if (sys%incsup%hours >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "hours", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "hours", sys%incsup%hours
        end if
        if (sys%incsup%MaintDisreg >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaintDisreg", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaintDisreg", sys%incsup%MaintDisreg
        end if
        if (sys%incsup%AgeRngl(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "AgeRngl", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "AgeRngl", sys%incsup%AgeRngl(1)
        end if
        do i = 2, sys%incsup%NumAgeRng
            if (sys%incsup%AgeRngl(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, I20)') "", sys%incsup%AgeRngl(i)
            end if
        end do
        if (sys%incsup%AgeRngu(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "AgeRngu", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "AgeRngu", sys%incsup%AgeRngu(1)
        end if
        do i = 2, sys%incsup%NumAgeRng
            if (sys%incsup%AgeRngu(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, I20)') "", sys%incsup%AgeRngu(i)
            end if
        end do
        if (sys%incsup%AddKid(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "AddKid", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "AddKid", sys%incsup%AddKid(1)
        end if
        do i = 2, sys%incsup%NumAgeRng
            if (sys%incsup%AddKid(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, F20.5)') "", sys%incsup%AddKid(i)
            end if
        end do
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "CTAX:"
write(funit, '(A)')
        if (sys%ctax%docounciltax >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Calculate council tax (docounciltax)", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "Calculate council tax (docounciltax)", &
                  trim(adjustl(label_bool(sys%ctax%docounciltax))) 
        end if
        if (sys%ctax%bandD >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Average Band D amount (bandD)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Average Band D amount (bandD)", sys%ctax%bandD
        end if
        if (sys%ctax%SinDis >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Single discount (SinDis)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Single discount (SinDis)", sys%ctax%SinDis
        end if
        if (sys%ctax%EnglandRatioA >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "England Band A ratio (EnglandRatioA)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "England Band A ratio (EnglandRatioA)", sys%ctax%EnglandRatioA
        end if
        if (sys%ctax%EnglandRatioB >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "England Band B ratio (EnglandRatioB)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "England Band B ratio (EnglandRatioB)", sys%ctax%EnglandRatioB
        end if
        if (sys%ctax%EnglandRatioC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "England Band C ratio (EnglandRatioC)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "England Band C ratio (EnglandRatioC)", sys%ctax%EnglandRatioC
        end if
        if (sys%ctax%EnglandRatioE >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "England Band E ratio (EnglandRatioE)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "England Band E ratio (EnglandRatioE)", sys%ctax%EnglandRatioE
        end if
        if (sys%ctax%EnglandRatioF >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "England Band F ratio (EnglandRatioF)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "England Band F ratio (EnglandRatioF)", sys%ctax%EnglandRatioF
        end if
        if (sys%ctax%EnglandRatioG >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "England Band G ratio (EnglandRatioG)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "England Band G ratio (EnglandRatioG)", sys%ctax%EnglandRatioG
        end if
        if (sys%ctax%EnglandRatioH >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "England Band H ratio (EnglandRatioH)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "England Band H ratio (EnglandRatioH)", sys%ctax%EnglandRatioH
        end if
        if (sys%ctax%EnglandRatioI >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "England Band I ratio (EnglandRatioI)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "England Band I ratio (EnglandRatioI)", sys%ctax%EnglandRatioI
        end if
        if (sys%ctax%ScotlandRatioA >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Scotland Band A ratio (ScotlandRatioA)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Scotland Band A ratio (ScotlandRatioA)", sys%ctax%ScotlandRatioA
        end if
        if (sys%ctax%ScotlandRatioB >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Scotland Band B ratio (ScotlandRatioB)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Scotland Band B ratio (ScotlandRatioB)", sys%ctax%ScotlandRatioB
        end if
        if (sys%ctax%ScotlandRatioC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Scotland Band C ratio (ScotlandRatioC)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Scotland Band C ratio (ScotlandRatioC)", sys%ctax%ScotlandRatioC
        end if
        if (sys%ctax%ScotlandRatioE >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Scotland Band E ratio (ScotlandRatioE)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Scotland Band E ratio (ScotlandRatioE)", sys%ctax%ScotlandRatioE
        end if
        if (sys%ctax%ScotlandRatioF >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Scotland Band F ratio (ScotlandRatioF)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Scotland Band F ratio (ScotlandRatioF)", sys%ctax%ScotlandRatioF
        end if
        if (sys%ctax%ScotlandRatioG >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Scotland Band G ratio (ScotlandRatioG)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Scotland Band G ratio (ScotlandRatioG)", sys%ctax%ScotlandRatioG
        end if
        if (sys%ctax%ScotlandRatioH >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Scotland Band H ratio (ScotlandRatioH)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Scotland Band H ratio (ScotlandRatioH)", sys%ctax%ScotlandRatioH
        end if
        if (sys%ctax%ScotlandRatioI >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Scotland Band I ratio (ScotlandRatioI)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Scotland Band I ratio (ScotlandRatioI)", sys%ctax%ScotlandRatioI
        end if
        if (sys%ctax%WalesRatioA >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Wales Band A ratio (WalesRatioA)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Wales Band A ratio (WalesRatioA)", sys%ctax%WalesRatioA
        end if
        if (sys%ctax%WalesRatioB >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Wales Band B ratio (WalesRatioB)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Wales Band B ratio (WalesRatioB)", sys%ctax%WalesRatioB
        end if
        if (sys%ctax%WalesRatioC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Wales Band C ratio (WalesRatioC)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Wales Band C ratio (WalesRatioC)", sys%ctax%WalesRatioC
        end if
        if (sys%ctax%WalesRatioE >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Wales Band E ratio (WalesRatioE)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Wales Band E ratio (WalesRatioE)", sys%ctax%WalesRatioE
        end if
        if (sys%ctax%WalesRatioF >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Wales Band F ratio (WalesRatioF)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Wales Band F ratio (WalesRatioF)", sys%ctax%WalesRatioF
        end if
        if (sys%ctax%WalesRatioG >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Wales Band G ratio (WalesRatioG)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Wales Band G ratio (WalesRatioG)", sys%ctax%WalesRatioG
        end if
        if (sys%ctax%WalesRatioH >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Wales Band H ratio (WalesRatioH)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Wales Band H ratio (WalesRatioH)", sys%ctax%WalesRatioH
        end if
        if (sys%ctax%WalesRatioI >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Wales Band I ratio (WalesRatioI)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Wales Band I ratio (WalesRatioI)", sys%ctax%WalesRatioI
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "REBATESYS:"
write(funit, '(A)')
        if (sys%rebatesys%RulesUnderFC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "RulesUnderFC", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "RulesUnderFC", &
                  trim(adjustl(label_bool(sys%rebatesys%RulesUnderFC))) 
        end if
        if (sys%rebatesys%RulesUnderWFTC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "RulesUnderWFTC", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "RulesUnderWFTC", &
                  trim(adjustl(label_bool(sys%rebatesys%RulesUnderWFTC))) 
        end if
        if (sys%rebatesys%RulesUnderNTC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "RulesUnderNTC", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "RulesUnderNTC", &
                  trim(adjustl(label_bool(sys%rebatesys%RulesUnderNTC))) 
        end if
        if (sys%rebatesys%RulesUnderUC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "RulesUnderUC", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "RulesUnderUC", &
                  trim(adjustl(label_bool(sys%rebatesys%RulesUnderUC))) 
        end if
        if (sys%rebatesys%NumAgeRng >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "NumAgeRng", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "NumAgeRng", sys%rebatesys%NumAgeRng
        end if
        if (sys%rebatesys%Restrict >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Restrict", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "Restrict", &
                  trim(adjustl(label_bool(sys%rebatesys%Restrict))) 
        end if
        if (sys%rebatesys%docap >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "docap", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "docap", &
                  trim(adjustl(label_bool(sys%rebatesys%docap))) 
        end if
        if (sys%rebatesys%MinAgeMain >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinAgeMain", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MinAgeMain", sys%rebatesys%MinAgeMain
        end if
        if (sys%rebatesys%MinAgeMainSin >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinAgeMainSin", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MinAgeMainSin", sys%rebatesys%MinAgeMainSin
        end if
        if (sys%rebatesys%MainCou >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MainCou", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MainCou", sys%rebatesys%MainCou
        end if
        if (sys%rebatesys%YngCou >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "YngCou", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "YngCou", sys%rebatesys%YngCou
        end if
        if (sys%rebatesys%MainLP >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MainLP", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MainLP", sys%rebatesys%MainLP
        end if
        if (sys%rebatesys%YngLP >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "YngLP", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "YngLP", sys%rebatesys%YngLP
        end if
        if (sys%rebatesys%MainSin >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MainSin", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MainSin", sys%rebatesys%MainSin
        end if
        if (sys%rebatesys%YngSin >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "YngSin", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "YngSin", sys%rebatesys%YngSin
        end if
        if (sys%rebatesys%DisregSin >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregSin", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregSin", sys%rebatesys%DisregSin
        end if
        if (sys%rebatesys%DisregLP >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregLP", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregLP", sys%rebatesys%DisregLP
        end if
        if (sys%rebatesys%DisregCou >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregCou", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregCou", sys%rebatesys%DisregCou
        end if
        if (sys%rebatesys%CredInDisregCC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "CredInDisregCC", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "CredInDisregCC", &
                  trim(adjustl(label_bool(sys%rebatesys%CredInDisregCC))) 
        end if
        if (sys%rebatesys%ChbenIsIncome >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "ChbenIsIncome", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "ChbenIsIncome", &
                  trim(adjustl(label_bool(sys%rebatesys%ChbenIsIncome))) 
        end if
        if (sys%rebatesys%PremFam >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "PremFam", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "PremFam", sys%rebatesys%PremFam
        end if
        if (sys%rebatesys%PremLP >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "PremLP", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "PremLP", sys%rebatesys%PremLP
        end if
        if (sys%rebatesys%MaintDisreg >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaintDisreg", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaintDisreg", sys%rebatesys%MaintDisreg
        end if
        if (sys%rebatesys%MaxCC1 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxCC1", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaxCC1", sys%rebatesys%MaxCC1
        end if
        if (sys%rebatesys%MaxCC2 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxCC2", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaxCC2", sys%rebatesys%MaxCC2
        end if
        if (sys%rebatesys%MaxAgeCC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxAgeCC", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MaxAgeCC", sys%rebatesys%MaxAgeCC
        end if
        if (sys%rebatesys%AgeRngl(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "AgeRngl", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "AgeRngl", sys%rebatesys%AgeRngl(1)
        end if
        do i = 2, sys%rebatesys%NumAgeRng
            if (sys%rebatesys%AgeRngl(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, I20)') "", sys%rebatesys%AgeRngl(i)
            end if
        end do
        if (sys%rebatesys%AgeRngu(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "AgeRngu", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "AgeRngu", sys%rebatesys%AgeRngu(1)
        end if
        do i = 2, sys%rebatesys%NumAgeRng
            if (sys%rebatesys%AgeRngu(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, I20)') "", sys%rebatesys%AgeRngu(i)
            end if
        end do
        if (sys%rebatesys%AddKid(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "AddKid", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "AddKid", sys%rebatesys%AddKid(1)
        end if
        do i = 2, sys%rebatesys%NumAgeRng
            if (sys%rebatesys%AddKid(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, F20.5)') "", sys%rebatesys%AddKid(i)
            end if
        end do
        if (sys%rebatesys%MaxKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxKids", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MaxKids", sys%rebatesys%MaxKids
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "HBEN:"
write(funit, '(A)')
        if (sys%hben%doHBen >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Calculate housing benefit (doHBen)", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "Calculate housing benefit (doHBen)", &
                  trim(adjustl(label_bool(sys%hben%doHBen))) 
        end if
        if (sys%hben%taper >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Taper rate (taper)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Taper rate (taper)", sys%hben%taper
        end if
        if (sys%hben%MinAmt >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Minimum amount (MinAmt)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Minimum amount (MinAmt)", sys%hben%MinAmt
        end if
        if (sys%hben%doUnderOccCharge >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Under occupancy charge (doUnderOccCharge)", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "Under occupancy charge (doUnderOccCharge)", &
                  trim(adjustl(label_bool(sys%hben%doUnderOccCharge))) 
        end if
        if (sys%hben%doUnderOccChargeScotland >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Under occupancy charge Scotland (doUnderOccChargeScotland)", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "Under occupancy charge Scotland (doUnderOccChargeScotland)", &
                  trim(adjustl(label_bool(sys%hben%doUnderOccChargeScotland))) 
        end if
        if (sys%hben%doUnderOccChargeNI >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Under occupancy charge NI (doUnderOccChargeNI)", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "Under occupancy charge NI (doUnderOccChargeNI)", &
                  trim(adjustl(label_bool(sys%hben%doUnderOccChargeNI))) 
        end if
        if (sys%hben%numUnderOccBands >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Under occupancy bands (numUnderOccBands)", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "Under occupancy bands (numUnderOccBands)", sys%hben%numUnderOccBands
        end if
        if (sys%hben%underOccRates(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Under occupancy rates (underOccRates)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "Under occupancy rates (underOccRates)", sys%hben%underOccRates(1)
        end if
        do i = 2, sys%hben%numUnderOccBands
            if (sys%hben%underOccRates(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, F20.5)') "", sys%hben%underOccRates(i)
            end if
        end do
        if (sys%hben%doLHA >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Local housing allowance (doLHA)", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "Local housing allowance (doLHA)", sys%hben%doLHA
        end if
        if (sys%hben%LHASharedAccAge >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Shared accommodation age (LHASharedAccAge)", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "Shared accommodation age (LHASharedAccAge)", sys%hben%LHASharedAccAge
        end if
        if (sys%hben%numLHABands >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "Number LHA bands (numLHABands)", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "Number LHA bands (numLHABands)", sys%hben%numLHABands
        end if
        if (sys%hben%LHARates(1) >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "LHA Rates (LHARates)", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "LHA Rates (LHARates)", sys%hben%LHARates(1)
        end if
        do i = 2, sys%hben%numLHABands
            if (sys%hben%LHARates(i) >= sysHuge) then
                write(funit, '(A40, 2X, A20)') "", "unbounded"
            else
                write(funit, '(A40, 2X, F20.5)') "", sys%hben%LHARates(i)
            end if
        end do
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "CTAXBEN:"
write(funit, '(A)')
        if (sys%ctaxben%docounciltaxben >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "docounciltaxben", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "docounciltaxben", &
                  trim(adjustl(label_bool(sys%ctaxben%docounciltaxben))) 
        end if
        if (sys%ctaxben%taper >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "taper", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "taper", sys%ctaxben%taper
        end if
        if (sys%ctaxben%doEntitlementCut >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "doEntitlementCut", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "doEntitlementCut", &
                  trim(adjustl(label_bool(sys%ctaxben%doEntitlementCut))) 
        end if
        if (sys%ctaxben%entitlementShare >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "entitlementShare", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "entitlementShare", sys%ctaxben%entitlementShare
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "CCBEN:"
write(funit, '(A)')
        if (sys%ccben%dopolltax >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "dopolltax", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "dopolltax", &
                  trim(adjustl(label_bool(sys%ccben%dopolltax))) 
        end if
        if (sys%ccben%taper >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "taper", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "taper", sys%ccben%taper
        end if
        if (sys%ccben%PropElig >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "PropElig", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "PropElig", sys%ccben%PropElig
        end if
        if (sys%ccben%MinAmt >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinAmt", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MinAmt", sys%ccben%MinAmt
        end if
        if (sys%ccben%CCrate >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "CCrate", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "CCrate", sys%ccben%CCrate
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "UC:"
write(funit, '(A)')
        if (sys%uc%doUnivCred >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "doUnivCred", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "doUnivCred", &
                  trim(adjustl(label_bool(sys%uc%doUnivCred))) 
        end if
        if (sys%uc%MainCou >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MainCou", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MainCou", sys%uc%MainCou
        end if
        if (sys%uc%YngCou >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "YngCou", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "YngCou", sys%uc%YngCou
        end if
        if (sys%uc%MainSin >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MainSin", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MainSin", sys%uc%MainSin
        end if
        if (sys%uc%YngSin >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "YngSin", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "YngSin", sys%uc%YngSin
        end if
        if (sys%uc%MinAgeMain >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinAgeMain", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MinAgeMain", sys%uc%MinAgeMain
        end if
        if (sys%uc%FirstKid >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "FirstKid", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "FirstKid", sys%uc%FirstKid
        end if
        if (sys%uc%OtherKid >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "OtherKid", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "OtherKid", sys%uc%OtherKid
        end if
        if (sys%uc%MaxKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxKids", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MaxKids", sys%uc%MaxKids
        end if
        if (sys%uc%MaxCC1 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxCC1", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaxCC1", sys%uc%MaxCC1
        end if
        if (sys%uc%MaxCC2 >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxCC2", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MaxCC2", sys%uc%MaxCC2
        end if
        if (sys%uc%PropCC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "PropCC", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "PropCC", sys%uc%PropCC
        end if
        if (sys%uc%MaxAgeCC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MaxAgeCC", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "MaxAgeCC", sys%uc%MaxAgeCC
        end if
        if (sys%uc%doRentCap >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "doRentCap", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "doRentCap", &
                  trim(adjustl(label_bool(sys%uc%doRentCap))) 
        end if
        if (sys%uc%DisregSinNoKidsHi >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregSinNoKidsHi", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregSinNoKidsHi", sys%uc%DisregSinNoKidsHi
        end if
        if (sys%uc%DisregSinNoKidsLo >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregSinNoKidsLo", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregSinNoKidsLo", sys%uc%DisregSinNoKidsLo
        end if
        if (sys%uc%DisregSinKidsHi >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregSinKidsHi", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregSinKidsHi", sys%uc%DisregSinKidsHi
        end if
        if (sys%uc%DisregSinKidsLo >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregSinKidsLo", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregSinKidsLo", sys%uc%DisregSinKidsLo
        end if
        if (sys%uc%DisregCouNoKidsHi >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregCouNoKidsHi", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregCouNoKidsHi", sys%uc%DisregCouNoKidsHi
        end if
        if (sys%uc%DisregCouNoKidsLo >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregCouNoKidsLo", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregCouNoKidsLo", sys%uc%DisregCouNoKidsLo
        end if
        if (sys%uc%DisregCouKidsHi >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregCouKidsHi", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregCouKidsHi", sys%uc%DisregCouKidsHi
        end if
        if (sys%uc%DisregCouKidsLo >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "DisregCouKidsLo", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "DisregCouKidsLo", sys%uc%DisregCouKidsLo
        end if
        if (sys%uc%taper >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "taper", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "taper", sys%uc%taper
        end if
        if (sys%uc%MinAmt >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "MinAmt", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "MinAmt", sys%uc%MinAmt
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "STATEPEN:"
write(funit, '(A)')
        if (sys%statepen%doStatePen >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "doStatePen", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "doStatePen", &
                  trim(adjustl(label_bool(sys%statepen%doStatePen))) 
        end if
        if (sys%statepen%PenAgeMan >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "PenAgeMan", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "PenAgeMan", sys%statepen%PenAgeMan
        end if
        if (sys%statepen%PenAgeWoman >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "PenAgeWoman", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "PenAgeWoman", sys%statepen%PenAgeWoman
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "BENCAP:"
write(funit, '(A)')
        if (sys%bencap%doCap >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "doCap", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "doCap", &
                  trim(adjustl(label_bool(sys%bencap%doCap))) 
        end if
        if (sys%bencap%doNI >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "doNI", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "doNI", &
                  trim(adjustl(label_bool(sys%bencap%doNI))) 
        end if
        if (sys%bencap%doThruUC >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "doThruUC", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "doThruUC", &
                  trim(adjustl(label_bool(sys%bencap%doThruUC))) 
        end if
        if (sys%bencap%sinNoKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "sinNoKids", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "sinNoKids", sys%bencap%sinNoKids
        end if
        if (sys%bencap%sinKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "sinKids", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "sinKids", sys%bencap%sinKids
        end if
        if (sys%bencap%couNoKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "couNoKids", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "couNoKids", sys%bencap%couNoKids
        end if
        if (sys%bencap%couKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "couKids", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "couKids", sys%bencap%couKids
        end if
        if (sys%bencap%LondonCapAmt >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "LondonCapAmt", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "LondonCapAmt", &
                  trim(adjustl(label_bool(sys%bencap%LondonCapAmt))) 
        end if
        if (sys%bencap%LondonSinNoKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "LondonSinNoKids", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "LondonSinNoKids", sys%bencap%LondonSinNoKids
        end if
        if (sys%bencap%LondonSinKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "LondonSinKids", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "LondonSinKids", sys%bencap%LondonSinKids
        end if
        if (sys%bencap%LondonCouNoKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "LondonCouNoKids", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "LondonCouNoKids", sys%bencap%LondonCouNoKids
        end if
        if (sys%bencap%LondonCouKids >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "LondonCouKids", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "LondonCouKids", sys%bencap%LondonCouKids
        end if
        if (sys%bencap%UCEarnThr >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "UCEarnThr", "unbounded"
        else
            write(funit, '(A40, 2X, F20.5)') "UCEarnThr", sys%bencap%UCEarnThr
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)

write(funit, '(A)')
write(funit, '(A)') "EXTRA:"
write(funit, '(A)')
        if (sys%extra%fsminappamt >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "fsminappamt", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "fsminappamt", &
                  trim(adjustl(label_bool(sys%extra%fsminappamt))) 
        end if
        if (sys%extra%matgrant >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "matgrant", "unbounded"
        else
            write(funit, '(A40, 2X, A20)') "matgrant", &
                  trim(adjustl(label_bool(sys%extra%matgrant))) 
        end if
        if (sys%extra%prices >= sysHuge) then
            write(funit, '(A40, 2X, A20)') "prices", "unbounded"
        else
            write(funit, '(A40, 2X, I20)') "prices", sys%extra%prices
        end if
write(funit, '(A)')
        write(funit, '(A)') repeat("=", 62)


        if (present(fname)) close(funit)

        return

    end subroutine sys_desc


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
        call json%add(inp, "doTPA", sys%inctax%doTPA)
        call json%add(inp, "maxTPA", sys%inctax%maxTPA)
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
        call json%add(inp, "maxKids", sys%ctc%maxKids)
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

        call json%create_object(inp, "cctaxrefund")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "doCCTaxRefund", sys%cctaxrefund%doCCTaxRefund)
        call json%add(inp, "MaxPerChild", sys%cctaxrefund%MaxPerChild)
        call json%add(inp, "MaxAge", sys%cctaxrefund%MaxAge)
        call json%add(inp, "ReceiptProp", sys%cctaxrefund%ReceiptProp)
        call json%add(inp, "MinEarn", sys%cctaxrefund%MinEarn)
        call json%add(inp, "MaxInc", sys%cctaxrefund%MaxInc)
        nullify(inp)

        call json%create_object(inp, "incsup")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "doIncSup", sys%incsup%doIncSup)
        call json%add(inp, "IncChben", sys%incsup%IncChben)
        call json%add(inp, "NumAgeRng", sys%incsup%NumAgeRng)
        call json%add(inp, "MinAgeMain", sys%incsup%MinAgeMain)
        call json%add(inp, "MinAgeMainSin", sys%incsup%MinAgeMainSin)
        call json%add(inp, "MainCou", sys%incsup%MainCou)
        call json%add(inp, "YngCou", sys%incsup%YngCou)
        call json%add(inp, "MainLP", sys%incsup%MainLP)
        call json%add(inp, "YngLP", sys%incsup%YngLP)
        call json%add(inp, "MainSin", sys%incsup%MainSin)
        call json%add(inp, "YngSin", sys%incsup%YngSin)
        call json%add(inp, "MinAgeFSM", sys%incsup%MinAgeFSM)
        call json%add(inp, "MaxAgeUniversalFSM", sys%incsup%MaxAgeUniversalFSM)
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
        call json%add(inp, "EnglandRatioA", sys%ctax%EnglandRatioA)
        call json%add(inp, "EnglandRatioB", sys%ctax%EnglandRatioB)
        call json%add(inp, "EnglandRatioC", sys%ctax%EnglandRatioC)
        call json%add(inp, "EnglandRatioE", sys%ctax%EnglandRatioE)
        call json%add(inp, "EnglandRatioF", sys%ctax%EnglandRatioF)
        call json%add(inp, "EnglandRatioG", sys%ctax%EnglandRatioG)
        call json%add(inp, "EnglandRatioH", sys%ctax%EnglandRatioH)
        call json%add(inp, "EnglandRatioI", sys%ctax%EnglandRatioI)
        call json%add(inp, "ScotlandRatioA", sys%ctax%ScotlandRatioA)
        call json%add(inp, "ScotlandRatioB", sys%ctax%ScotlandRatioB)
        call json%add(inp, "ScotlandRatioC", sys%ctax%ScotlandRatioC)
        call json%add(inp, "ScotlandRatioE", sys%ctax%ScotlandRatioE)
        call json%add(inp, "ScotlandRatioF", sys%ctax%ScotlandRatioF)
        call json%add(inp, "ScotlandRatioG", sys%ctax%ScotlandRatioG)
        call json%add(inp, "ScotlandRatioH", sys%ctax%ScotlandRatioH)
        call json%add(inp, "ScotlandRatioI", sys%ctax%ScotlandRatioI)
        call json%add(inp, "WalesRatioA", sys%ctax%WalesRatioA)
        call json%add(inp, "WalesRatioB", sys%ctax%WalesRatioB)
        call json%add(inp, "WalesRatioC", sys%ctax%WalesRatioC)
        call json%add(inp, "WalesRatioE", sys%ctax%WalesRatioE)
        call json%add(inp, "WalesRatioF", sys%ctax%WalesRatioF)
        call json%add(inp, "WalesRatioG", sys%ctax%WalesRatioG)
        call json%add(inp, "WalesRatioH", sys%ctax%WalesRatioH)
        call json%add(inp, "WalesRatioI", sys%ctax%WalesRatioI)
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
        call json%add(inp, "MinAgeMain", sys%rebatesys%MinAgeMain)
        call json%add(inp, "MinAgeMainSin", sys%rebatesys%MinAgeMainSin)
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
        call json%add(inp, "MaxKids", sys%rebatesys%MaxKids)
        nullify(inp)

        call json%create_object(inp, "hben")
        call json%add(p, inp) !add it to the root
        call json%add(inp, "doHBen", sys%hben%doHBen)
        call json%add(inp, "taper", sys%hben%taper)
        call json%add(inp, "MinAmt", sys%hben%MinAmt)
        call json%add(inp, "doUnderOccCharge", sys%hben%doUnderOccCharge)
        call json%add(inp, "doUnderOccChargeScotland", sys%hben%doUnderOccChargeScotland)
        call json%add(inp, "doUnderOccChargeNI", sys%hben%doUnderOccChargeNI)
        call json%add(inp, "numUnderOccBands", sys%hben%numUnderOccBands)
        call json%add(inp, "underOccRates", sys%hben%underOccRates(1:sys%hben%numUnderOccBands))
        call json%add(inp, "doLHA", sys%hben%doLHA)
        call json%add(inp, "LHASharedAccAge", sys%hben%LHASharedAccAge)
        call json%add(inp, "numLHABands", sys%hben%numLHABands)
        call json%add(inp, "LHARates", sys%hben%LHARates(1:sys%hben%numLHABands))
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
        call json%add(inp, "MaxKids", sys%uc%MaxKids)
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
        call json%add(inp, "doNI", sys%bencap%doNI)
        call json%add(inp, "doThruUC", sys%bencap%doThruUC)
        call json%add(inp, "sinNoKids", sys%bencap%sinNoKids)
        call json%add(inp, "sinKids", sys%bencap%sinKids)
        call json%add(inp, "couNoKids", sys%bencap%couNoKids)
        call json%add(inp, "couKids", sys%bencap%couKids)
        call json%add(inp, "LondonCapAmt", sys%bencap%LondonCapAmt)
        call json%add(inp, "LondonSinNoKids", sys%bencap%LondonSinNoKids)
        call json%add(inp, "LondonSinKids", sys%bencap%LondonSinKids)
        call json%add(inp, "LondonCouNoKids", sys%bencap%LondonCouNoKids)
        call json%add(inp, "LondonCouKids", sys%bencap%LondonCouKids)
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
