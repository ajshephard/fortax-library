
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




! fortax_library_c
! -----------------------------------------------------------------------
! module provides c bindings for the main user routines from fortax_library

module fortax_library_c

    use iso_c_binding
    use fortax_library

    implicit none

    private :: copy_a2s, copy_s2a

contains

    pure function copy_a2s(a) result(s)    ! copy char array to string
        character(kind=c_char), intent(in) :: a(:)
        character(size(a)) :: s
        integer :: i
        do i = 1, size(a)
            s(i:i) = a(i)
        end do
    end function copy_a2s

    pure function copy_s2a(s) result (a)   ! copy s(1:clen(s)) to char array
        character(len = *), intent(in) :: s
        character(kind=c_char) :: a(len(s))
        integer :: i
        do i = 1, len(s)
            a(i) = s(i:i)
        end do
    end function copy_s2a

    ! use fortax_calc,        only :  Y FORTAX_calcNetInc => calcNetInc

    subroutine c_CalcNetInc(sys, fam, net)  bind(C, name = "C_FORTAX_calcNetInc")
        implicit none
        type(sys_t), intent(in)  :: sys
        type(fam_t), intent(in)  :: fam
        type(net_t), intent(out) :: net
        call FORTAX_CalcNetInc(sys, fam, net)
    end subroutine c_CalcNetInc

    ! use fortax_compare,     only :  FORTAX_writeFamCompareDatabase => writeFamCompareDatabase,  &
    !                                 FORTAX_readFamCompareDatabase => readFamCompareDatabase,    &
    !                                 FORTAX_compareFamDatabase => compareFamDatabase,            &
    !                                 FORTAX_compareNet => compareNet

    ! use fortax_extra,       only :  FORTAX_setMinAmount => setMinAmount,                        &
    !                                 FORTAX_abolishNIFee => abolishNIFee,                        &
    !                                 FORTAX_disableTaperRounding => disableTaperRounding,        &
    !                                 FORTAX_fsMinAppAmt => fsMinAppAmt,                          &
    !                                 FORTAX_taperMatGrant => taperMatGrant,                      &
    !                                 FORTAX_imposeUC => imposeUC,                                &
    !                                 FORTAX_netoutDesc => netoutDesc,                            &
    !                                 FORTAX_netoutDescNoName => netoutDescNoName

    ! use fortax_kinks,       only :  bcout_t,                                                    &
    !                                 FORTAX_evalKinksHours => evalKinksHours,                    &
    !                                 FORTAX_evalKinksEarn => evalKinksEarn,                      &
    !                                 FORTAX_kinkshours => kinkshours,                            &
    !                                 FORTAX_kinksearn => kinksearn,                              &
    !                                 FORTAX_kinksccexp => kinksccexp,                            &
    !                                 FORTAX_maxkinks => maxkinks

    ! use fortax_prices,      only :  sysIndex_t, &
    !                         YY       FORTAX_loadIndex => loadIndex,                              &
    !                         Y       FORTAX_uprateSys => uprateSys,                              &
    !                         Y       FORTAX_uprateFactor => uprateFactor,                        &
    !                         Y       FORTAX_loadSysIndex => loadSysIndex,                        &
    !                         Y       FORTAX_getSysIndex => getSysIndex

    subroutine c_loadIndex(rpi, fname, len_fname) bind(C, name = "C_FORTAX_loadIndex")
        implicit none
        type(rpi_t), intent(out) :: rpi
        character(kind=c_char), dimension(*), intent(in), optional :: fname
        integer(kind=c_int), intent(in), optional :: len_fname
        if (present(fname) .and. present(len_fname)) then
            call FORTAX_loadIndex(rpi, copy_a2s(fname(1:len_fname)))
        else
            call FORTAX_loadIndex(rpi)
        end if
    end subroutine c_loadIndex

    function c_upratefactor(rpi, date0, date1) bind(C, name = "C_FORTAX_uprateFactor")
        implicit none
        type(rpi_t), intent(in) :: rpi
        integer(kind=c_int), intent(in) :: date0, date1
        real(kind=c_double) :: c_upratefactor
        c_upratefactor = FORTAX_uprateFactor(rpi, date0, date1)
    end function c_upratefactor

    subroutine c_upratesys(sys, factor, newdate) bind(C, name = "C_FORTAX_uprateSys")
        implicit none
        type(sys_t), intent(inout) :: sys
        real(kind=c_double), intent(in) :: factor
        integer(kind=c_int), intent(in), optional :: newdate
        call FORTAX_upratesys(sys, factor, newdate)
    end subroutine c_upratesys

    subroutine c_loadsysindex(sysindex, fname, len_fname) bind(C, name = "C_FORTAX_loadSysIndex")
        implicit none
        type(sysindex_t), intent(out) :: sysindex
        character(kind=c_char), dimension(*), intent(in), optional :: fname
        integer(kind=c_int), intent(in), optional :: len_fname
        if (present(fname) .and. present(len_fname)) then
            call FORTAX_loadsysindex(sysindex, copy_a2s(fname(1:len_fname)))
        else
            call FORTAX_loadsysindex(sysindex)
        end if
    end subroutine c_loadsysindex

    subroutine c_getsysindex(sysindex, date, systemformat, len_systemformat, c_sysfilepath, len_sysfilepath, sysnum) bind(C, name = "C_FORTAX_getSysIndex")
        implicit none
        type(sysindex_t), intent(in) :: sysindex
        integer(kind=c_int), intent(in) :: date
        character(kind=c_char), dimension(*), intent(in) :: systemformat
        integer(kind=c_int), intent(in) :: len_systemformat
        character(kind=c_char), dimension(255), intent(out) :: c_sysfilepath
        integer(kind=c_int), intent(out) :: len_sysfilepath
        integer(kind=c_int), intent(out) :: sysnum
        character(len=255) :: sysfilepath
        call FORTAX_getsysindex(sysindex, date, copy_a2s(systemformat(1:len_systemformat)), sysfilepath, sysnum)
        len_sysfilepath = len_trim(sysfilepath)
        c_sysfilepath = transfer(sysfilepath, c_sysfilepath)
        ! c_sysfilepath = copy_s2a(sysfilepath)
    end subroutine c_getsysindex

    ! use fortax_read,        only : Y FORTAX_readFortaxParams => readFortaxParams

    subroutine c_readFortaxParams(sys, systemFile, len_systemFile, prices) bind(C, name = "C_FORTAX_readFortaxParams")
        implicit none
        type(sys_t), intent(out) :: sys
        character(kind=c_char), dimension(*), intent(in) :: systemFile
        integer(kind=c_int), intent(in) :: len_systemFile
        integer(kind=c_int), optional, intent(in) :: prices
        call FORTAX_readFortaxParams(sys, copy_a2s(systemFile(1:len_systemFile)), prices)
    end subroutine c_readFortaxParams

    ! use fortax_taxbenread,  only :  FORTAX_readTaxbenParams => readTaxbenParams,                &
    !                                 FORTAX_batchConvertTaxben => batchConvertTaxben


    ! use fortax_type,        only :  fam_t, net_t, sys_t, rpi_t,                                 &
    !                               YY FORTAX_fam_init => fam_init,                                &
    !                               YY FORTAX_net_init => net_init,                                &
    !                               Y FORTAX_sys_init => sys_init,                                &
    !                               YY FORTAX_fam_saveF90 => fam_saveF90,                          &
    !                               Y FORTAX_sys_saveF90 => sys_saveF90,                          &
    !                               X FORTAX_lab => lab,                                          &
    !                               X FORTAX_maxkids => maxkids,                                  &
    !                               X FORTAX_fam_gen => fam_gen,                                  &
    !                               YY FORTAX_fam_desc => fam_desc,                                &
    !                               YY FORTAX_net_desc => net_desc,                                &
    !                               X operator(+), operator(*), operator(/)

    subroutine c_fam_init(fam) bind(C, name = "C_FORTAX_fam_init")
        implicit none
        type(fam_t), intent(inout) :: fam
        call FORTAX_fam_init(fam)
    end subroutine c_fam_init

    subroutine c_net_init(net) bind(C, name = "C_FORTAX_net_init")
        implicit none
        type(net_t), intent(inout) :: net
        call FORTAX_net_init(net)
    end subroutine c_net_init

    subroutine c_sys_init(sys) bind(C, name = "C_FORTAX_sys_init")
        implicit none
        type(sys_t), intent(inout) :: sys
        call FORTAX_sys_init(sys)
    end subroutine c_sys_init

    subroutine c_fam_saveF90(fam, fname, len_fname) bind(C, name = "C_FORTAX_fam_saveF90")
        implicit none
        type(fam_t), intent(in) :: fam
        character(kind=c_char), dimension(*), intent(in), optional :: fname
        integer(kind=c_int), intent(in), optional :: len_fname
        if (present(fname) .and. present(len_fname)) then
            call FORTAX_fam_saveF90(fam, copy_a2s(fname(1:len_fname)))
        else
            call FORTAX_fam_saveF90(fam)
        end if
    end subroutine c_fam_saveF90

    subroutine c_sys_saveF90(sys, fname, len_fname) bind(C, name = "C_FORTAX_sys_saveF90")
        implicit none
        type(sys_t), intent(in) :: sys
        character(kind=c_char), dimension(*), intent(in), optional :: fname
        integer(kind=c_int), intent(in), optional :: len_fname
        if (present(fname) .and. present(len_fname)) then
            call FORTAX_sys_saveF90(sys, copy_a2s(fname(1:len_fname)))
        else
            call FORTAX_sys_saveF90(sys)
        end if
    end subroutine c_sys_saveF90


    subroutine c_fam_desc(fam, fname, len_fname) bind(C, name = "C_FORTAX_fam_desc")
        implicit none
        type(fam_t), intent(in) :: fam
        character(kind=c_char), dimension(*), optional, intent(in) :: fname
        integer(kind=c_int), optional, intent(in) :: len_fname
        if (present(fname) .and. present(len_fname)) then
            call FORTAX_fam_desc(fam, copy_a2s(fname(1:len_fname)))
        else
            call FORTAX_fam_desc(fam)
        end if
    end subroutine c_fam_desc

    subroutine c_net_desc(net, fname, len_fname) bind(C, name = "C_FORTAX_net_desc")
        implicit none
        type(net_t), intent(in) :: net
        character(kind=c_char), dimension(*), optional, intent(in) :: fname
        integer(kind=c_int), optional, intent(in) :: len_fname
        if (present(fname) .and. present(len_fname)) then
            call FORTAX_net_desc(net, copy_a2s(fname(1:len_fname)))
        else
            call FORTAX_net_desc(net)
        end if
    end subroutine c_net_desc

    ! use fortax_write,       only :  FORTAX_fortaxPrint => fortaxPrint,                          &
    !                                 FORTAX_fortaxWrite => fortaxWrite

    ! use fortax_realtype,    only :  FORTAX_dp => dp,                                            &
    !                                 FORTAX_sp => sp,                                            &
    !                                 FORTAX_ep => ep,                                            &
    !                                 FORTAX_qp => qp

end module fortax_library_c
