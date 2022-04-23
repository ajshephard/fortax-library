
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

    pure function copy_a2s(a) result(s)
        character(kind = c_char), intent(in) :: a(:)
        character(size(a)) :: s
        integer :: i
        s = ''
        do i = 1, size(a)
            if (a(i) == c_null_char) exit
            s(i:i) = a(i)
        end do
    end function copy_a2s

    pure function copy_s2a(s) result(a)
        character(len = *), intent(in) :: s
        character(kind = c_char) :: a(len(s))
        integer :: i
        do i = 1, len(s)
            a(i) = s(i:i)
        end do
    end function copy_s2a

    ! fortax_calc

    function c_CalcNetInc(sys, fam) result(net) bind(C, name = "C_FORTAX_calcNetInc")
        implicit none
        type(sys_t), intent(in)  :: sys
        type(fam_t), intent(in)  :: fam
        type(net_t) :: net
        call FORTAX_CalcNetInc(sys, fam, net)
    end function c_CalcNetInc

    ! fortax_extra

    subroutine c_setMinAmount(sys, minamt) bind(C, name = "C_FORTAX_setMinAmount")
        implicit none
        type(sys_t), intent(inout) :: sys
        real(kind = c_double), intent(in) :: minamt
        call FORTAX_setMinAmount(sys, minamt)
    end subroutine c_setMinAmount

    subroutine c_abolishNIFee(sys) bind(C, name = "C_FORTAX_abolishNIFee")
        implicit none
        type(sys_t), intent(inout) :: sys
        call FORTAX_abolishNIFee(sys)
    end subroutine c_abolishNIFee

    subroutine c_disableTaperRounding(sys) bind(C, name = "C_FORTAX_disableTaperRounding")
        implicit none
        type(sys_t), intent(inout) :: sys
        call FORTAX_disableTaperRounding(sys)
    end subroutine c_disableTaperRounding

    subroutine c_fsMinAppAmt(sys, inappamt) bind(C, name = "C_FORTAX_fsMinAppAmt")
        implicit none
        type(sys_t), intent(inout) :: sys
        integer(kind = c_int), intent(in) :: inappamt
        call FORTAX_fsMinAppAmt(sys, inappamt)
    end subroutine c_fsMinAppAmt

    subroutine c_taperMatGrant(sys, taper) bind(C, name = "C_FORTAX_taperMatGrant")
        implicit none
        type(sys_t), intent(inout) :: sys
        integer(kind = c_int), intent(in) :: taper
        call FORTAX_taperMatGrant(sys, taper)
    end subroutine c_taperMatGrant

    subroutine c_imposeUC(sys) bind(C, name = "C_FORTAX_imposeUC")
        implicit none
        type(sys_t), intent(inout) :: sys
        call FORTAX_imposeUC(sys)
    end subroutine c_imposeUC

    ! fortax_kinks

    subroutine c_evalKinksHours(bcout, hours, earn, net, mtr, iin, iout) bind(C, name = "C_FORTAX_evalKinksHours")
        implicit none
        type(bcout_t), intent(in)  :: bcout
        real(kind = c_double), intent(in)  :: hours
        real(kind = c_double), intent(out) :: earn, net, mtr
        integer(kind = c_int), intent(in), optional :: iin
        integer(kind = c_int), intent(out), optional :: iout
        call FORTAX_evalKinksHours(bcout, hours, earn, net, mtr, iin, iout)
    end subroutine c_evalKinksHours

    subroutine c_evalKinksEarn(bcout, earn, hours, net, mtr, iin, iout) bind(C, name = "C_FORTAX_evalKinksEarn")
        implicit none
        type(bcout_t), intent(in)  :: bcout
        real(kind = c_double), intent(in)  :: earn
        real(kind = c_double), intent(out) :: hours, net, mtr
        integer(kind = c_int), intent(in), optional :: iin
        integer(kind = c_int), intent(out), optional :: iout
        call FORTAX_evalKinksEarn(bcout, earn, hours, net, mtr, iin, iout)
    end subroutine c_evalKinksEarn

    function c_kinksHoursDefault(sys, fam, ad, wage, hours1, hours2) result(bcout) &
        bind(C, name = "C_FORTAX_kinksHoursDefault")
        use fortax_type, only : len_label
        implicit none
        type(sys_t), intent(in)  :: sys
        type(fam_t), intent(in)  :: fam
        integer(kind = c_int), intent(in) :: ad
        real(kind = c_double), intent(in) :: wage
        real(kind = c_double), intent(in) :: hours1, hours2
        type(bcout_t) :: bcout
        call FORTAX_kinksHours(sys, fam, ad, wage, hours1, hours2, bcout)
    end function c_kinksHoursDefault

    function c_kinksHours(sys, fam, ad, wage, hours1, hours2, taxlevel, len_taxlevel, taxoutc, len_taxout) &
        result(bcout) bind(C, name = "C_FORTAX_kinksHours")
        use fortax_type, only : len_label
        implicit none
        type(sys_t), intent(in)  :: sys
        type(fam_t), intent(in)  :: fam
        integer(kind = c_int), intent(in) :: ad
        real(kind = c_double), intent(in) :: wage
        real(kind = c_double), intent(in) :: hours1, hours2
        integer(kind = c_int), intent(in) :: len_taxlevel
        integer(kind = c_int), intent(in) :: len_taxout
        character(kind = c_char), dimension(*), intent(in) :: taxlevel
        character(kind = c_char), dimension(len_label, len_taxout), intent(in) :: taxoutc
        character(len = len_label) :: taxout(len_taxout)
        type(bcout_t) :: bcout
        integer :: i
        do i = 1, len_taxout
            taxout(i) = copy_a2s(taxoutc(:, i))
        end do
        call FORTAX_kinksHours(sys, fam, ad, wage, hours1, hours2, bcout, copy_a2s(taxlevel(1:len_taxlevel)), taxout)
    end function c_kinksHours

    function c_kinksEarnDefault(sys, fam, ad, hours, earn1, earn2) result(bcout) &
        bind(C, name = "C_FORTAX_kinksEarnDefault")
        use fortax_type, only : len_label
        implicit none
        type(sys_t), intent(in)  :: sys
        type(fam_t), intent(in)  :: fam
        integer(kind = c_int), intent(in) :: ad
        real(kind = c_double), intent(in) :: hours
        real(kind = c_double), intent(in) :: earn1, earn2
        type(bcout_t) :: bcout
        call FORTAX_kinksEarn(sys, fam, ad, hours, earn1, earn2, bcout)
    end function c_kinksEarnDefault

    function c_kinksEarn(sys, fam, ad, hours, earn1, earn2, taxlevel, len_taxlevel, taxoutc, len_taxout) &
        result(bcout) bind(C, name = "C_FORTAX_kinksEarn")
        use fortax_type, only : len_label
        implicit none
        type(sys_t), intent(in)  :: sys
        type(fam_t), intent(in)  :: fam
        integer(kind = c_int), intent(in) :: ad
        real(kind = c_double), intent(in) :: hours
        real(kind = c_double), intent(in) :: earn1, earn2
        integer(kind = c_int), intent(in) :: len_taxlevel
        integer(kind = c_int), intent(in) :: len_taxout
        character(kind = c_char), dimension(*), intent(in) :: taxlevel
        character(kind = c_char), dimension(len_label, len_taxout), intent(in) :: taxoutc
        character(len = len_label) :: taxout(len_taxout)
        type(bcout_t) :: bcout
        integer :: i
        do i = 1, len_taxout
            taxout(i) = copy_a2s(taxoutc(:, i))
        end do
        call FORTAX_kinksEarn(sys, fam, ad, hours, earn1, earn2, bcout, copy_a2s(taxlevel(1:len_taxlevel)), taxout)
    end function c_kinksEarn

    function c_kinksCcexpDefault(sys, fam, ad, hours, earn, ccexp1, ccexp2) &
        result(bcout) bind(C, name = "C_FORTAX_kinksCcexpDefault")
        use fortax_type, only : len_label
        implicit none
        type(sys_t), intent(in)  :: sys
        type(fam_t), intent(in)  :: fam
        integer(kind = c_int), intent(in) :: ad
        real(kind = c_double), intent(in) :: hours
        real(kind = c_double), intent(in) :: earn
        real(kind = c_double), intent(in) :: ccexp1, ccexp2
        type(bcout_t) :: bcout
        call FORTAX_kinksCcexp(sys, fam, ad, hours, earn, ccexp1, ccexp2, bcout)
    end function c_kinksCcexpDefault

    function c_kinksCcexp(sys, fam, ad, hours, earn, ccexp1, ccexp2,  taxlevel, len_taxlevel, taxoutc, len_taxout) &
        result(bcout) bind(C, name = "C_FORTAX_kinksCcexp")
        use fortax_type, only : len_label
        implicit none
        type(sys_t), intent(in)  :: sys
        type(fam_t), intent(in)  :: fam
        integer(kind = c_int), intent(in) :: ad
        real(kind = c_double), intent(in) :: hours
        real(kind = c_double), intent(in) :: earn
        real(kind = c_double), intent(in) :: ccexp1, ccexp2
        integer(kind = c_int), intent(in) :: len_taxlevel
        integer(kind = c_int), intent(in) :: len_taxout
        character(kind = c_char), dimension(*), intent(in) :: taxlevel
        character(kind = c_char), dimension(len_label, len_taxout), intent(in) :: taxoutc
        character(len = len_label) :: taxout(len_taxout)
        type(bcout_t) :: bcout
        integer :: i
        do i = 1, len_taxout
            taxout(i) = copy_a2s(taxoutc(:, i))
        end do
        call FORTAX_kinksCcexp(sys, fam, ad, hours, earn, ccexp1, ccexp2, bcout, copy_a2s(taxlevel(1:len_taxlevel)), taxout)
    end function c_kinksCcexp

    subroutine c_kinks_desc(bcout) bind(C, name = "C_FORTAX_kinks_desc")
        implicit none
        type(bcout_t), intent(in) :: bcout
        call FORTAX_kinks_desc(bcout)
    end subroutine c_kinks_desc

    ! fortax_prices

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

    subroutine c_getsysindex(sysindex, date, c_sysfilepath, len_sysfilepath, sysnum) &
        bind(C, name = "C_FORTAX_getSysIndex")
        implicit none
        type(sysindex_t), intent(in) :: sysindex
        integer(kind=c_int), intent(in) :: date
        character(kind=c_char), dimension(256), intent(out) :: c_sysfilepath
        integer(kind=c_int), intent(out) :: len_sysfilepath
        integer(kind=c_int), intent(out) :: sysnum
        character(len = 256) :: sysfilepath
        call FORTAX_getsysindex(sysindex, date, sysfilepath, sysnum)
        len_sysfilepath = len_trim(sysfilepath)
        c_sysfilepath = copy_s2a(sysfilepath)
    end subroutine c_getsysindex

    ! fortax_read

    subroutine c_readFortaxParams(sys, systemFile, len_systemFile, prices) bind(C, name = "C_FORTAX_readFortaxParams")
        implicit none
        type(sys_t), intent(out) :: sys
        character(kind=c_char), dimension(*), intent(in) :: systemFile
        integer(kind=c_int), intent(in) :: len_systemFile
        integer(kind=c_int), optional, intent(in) :: prices
        call FORTAX_readFortaxParams(sys, copy_a2s(systemFile(1:len_systemFile)), prices)
    end subroutine c_readFortaxParams

    ! fortax type

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
        character(kind = c_char), dimension(*), optional, intent(in) :: fname
        integer(kind = c_int), optional, intent(in) :: len_fname
        if (present(fname) .and. present(len_fname)) then
            call FORTAX_net_desc(net, copy_a2s(fname(1:len_fname)))
        else
            call FORTAX_net_desc(net)
        end if
    end subroutine c_net_desc

    ! fortax_write

    subroutine c_sys_desc(sys, fname, len_fname) bind(C, name = "C_FORTAX_sys_desc")
        implicit none
        type(sys_t), intent(in) :: sys
        character(kind = c_char), dimension(*), optional, intent(in) :: fname
        integer(kind = c_int), optional, intent(in) :: len_fname
        if (present(fname) .and. present(len_fname)) then
            call FORTAX_sys_desc(sys, copy_a2s(fname(1:len_fname)))
        else
            call FORTAX_sys_desc(sys)
        end if
    end subroutine c_sys_desc

    subroutine c_writeFortaxParams(sys, fname, len_fname) bind(C, name = "C_FORTAX_writeFortaxParams")
        implicit none
        type(sys_t), intent(in) :: sys
        character(kind = c_char), dimension(*), intent(in) :: fname
        integer(kind = c_int), intent(in) :: len_fname
        call FORTAX_writeFortaxParams(sys, copy_a2s(fname(1:len_fname)))
    end subroutine c_writeFortaxParams

end module fortax_library_c
