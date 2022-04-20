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


! fortax_type
! -----------------------------------------------------------------------
! module defines the main derived types that describe families, the tax
! system, and the information returned by the calculation routines, AS

#:include "fortax.fypp"

module fortax_type

    use fortax_realtype, only : dp
    use iso_c_binding

    implicit none

    ! TODO improve label handling
    private :: dp
    private
    public :: fam_init, net_init, sys_init
    public :: fam_saveF90, sys_saveF90
    public :: fam_t, net_t, sys_t, rpi_t, sysindex_t, bcout_t
    public :: lab
    public :: fam_gen, fam_desc
    public :: operator(+), operator(*), operator(/)
    public :: net_desc

    ! constants for array bounds and internal values
    @:fortax_const(public = True)

    ! lab_t
    ! -----------------------------------------------------------------------
    ! defines various labels as specified in lablist and the associated
    ! include files. at the moment I am not using the string values, but they
    ! are still defined for possible future use

    #:for LAB in LABLIST
    type :: lab_${LAB}$_t
        @:fortax_type_def(lab_${LAB}$)
    end type lab_${LAB}$_t

    #:endfor

    type :: lab_t
        #:for LAB in LABLIST
        type(lab_${LAB}$_t) :: ${LAB}$
        #:endfor
    end type lab_t

    @:fortax_label_param()

    ! rpi_t
    ! -----------------------------------------------------------------------
    ! defines the prices indexing for uprating

    type, bind(c) :: rpi_t
        integer  :: ndate
        integer  :: date(maxRPI)
        real(dp) :: index(maxRPI)
    end type


    ! sysindex_t
    ! -----------------------------------------------------------------------
    ! defines sysindex

    type, bind(c) :: sysindex_t
        integer :: nsys
        integer :: date0(maxSysIndex), date1(maxSysIndex)
        character(kind = c_char) :: fname(len_sysindex, maxSysIndex)
    end type


    ! famad_t
    ! -----------------------------------------------------------------------
    ! defines the adult level family type structure (see fam_t below)

    type, bind(c) :: famad_t
        @:fortax_type_def(famad)
    end type famad_t

    ! fam_t
    ! -----------------------------------------------------------------------
    ! defines the family type structure, containing information on
    ! demographic characteristics, earnings, hours of work, and other
    ! information. Anything that can affect the taxes and transfer payments
    ! of a family is defined in here.

    type, bind(c) :: fam_t
        @:fortax_type_def(fam)
        type(famad_t) :: ad(2)
    end type fam_t

    ! netad_t
    ! -----------------------------------------------------------------------
    ! defines the adult level information returned following calls to the
    ! main calculation routines (see net_t below).

    type, bind(c) :: netad_t
        @:fortax_type_def(netad)
    end type netad_t

    ! nettu_t
    ! -----------------------------------------------------------------------
    ! defines the tax unit level information returned following calls to the
    ! main calculation routines (see net_t below).

    type, bind(c) :: nettu_t
        @:fortax_type_def(nettu)
    end type nettu_t

    ! net_t
    ! -----------------------------------------------------------------------
    ! defines the information returned following calls to the main
    ! calculation routines within fortax_calc. It contains measures of net
    ! income, together with various tax amounts and other components of income

    type, bind(c) :: net_t
        type(netad_t) :: ad(2)
        type(nettu_t) :: tu
    end type net_t


    ! Associate "net_plus_net" routine with "+" operator
    interface operator(+)
      module procedure net_plus_net
    end interface

    ! Associate "net_minus_net" routine with "-" operator
    interface operator(-)
      module procedure net_minus_net
    end interface

    ! Associate "net_times_scalar" and "scalar_times_net" routines with "*" operator
    interface operator(*)
      module procedure net_times_scalar
      module procedure net_times_scalar_integer
      module procedure scalar_times_net
      module procedure scalar_times_net_integer
    end interface

    ! Associate "net_div_scalar" routine with "/" operator
    interface operator(/)
      module procedure net_div_scalar
      module procedure net_div_scalar_integer
    end interface


    interface write_f90
        module procedure write_f90integer
        module procedure write_f90integerarray
        module procedure write_f90integerarray2
        module procedure write_f90double
        module procedure write_f90doublearray
        module procedure write_f90doublearray2
    end interface write_f90

    interface desc_f90
        module procedure desc_f90integer
        module procedure desc_f90integer_label
        module procedure desc_f90integerarray
        module procedure desc_f90integerarray_label
        module procedure desc_f90integerarray2
        module procedure desc_f90integerarray2_label
        module procedure desc_f90double
        module procedure desc_f90doublearray
        module procedure desc_f90doublearray2
    end interface desc_f90


    ! sys_t
    ! -----------------------------------------------------------------------
    ! defines the tax system structure which families of type fam_t face.
    ! It describes all the parameters which are interpreted within the
    ! module fortax_calc

    #:for SYS in SYSLIST
    type, bind(c) :: ${SYS}$_t
        @:fortax_type_def(${SYS}$)
    end type ${SYS}$_t

    #:endfor

    type, bind(c) :: sys_t
        character(kind = c_char) :: sysname(len_sysname)
        character(kind = c_char) :: sysdesc(len_sysdesc)
        #:for SYS in SYSLIST
        type(${SYS}$_t) :: ${SYS}$
        #:endfor
    end type sys_t

    type, bind(c) :: bcout_t
        integer :: kinks_num
        real(dp), dimension(maxkinks) :: kinks_hrs
        real(dp), dimension(maxkinks) :: kinks_earn
        real(dp), dimension(maxkinks) :: kinks_net
        real(dp), dimension(maxkinks) :: kinks_mtr
    end type bcout_t

contains


    ! fam_init
    ! -----------------------------------------------------------------------
    ! intializes family structure. unless defaults are coded here, integers
    ! are set to 0, doubles to 0.0_dp and logicals to .false. (and similarly
    ! for arrays). Could possibly add defaults in fam_t, but given it is
    ! only referenced here, there probably isn't much advantage to that

    elemental subroutine fam_init(fam)

        implicit none

        type(fam_t), intent(out) :: fam

        @:fortax_type_init(fam, fam)

        @:fortax_type_init(famad, fam%ad)

    end subroutine fam_init


    ! fam_desc
    ! -----------------------------------------------------------------------
    ! will display the information contained in the family variable fam and
    ! write this to a file if fname is specified

    subroutine fam_desc(fam, fname)

        use fortax_util, only : intToStr, strCentre, fortaxError

        use, intrinsic :: iso_fortran_env

        type(fam_t), intent(in) :: fam
        character(len = *), optional :: fname

        integer :: funit, i, ios

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit, *)
        write(funit, '(A)') repeat("=", 62)
        write(funit, '(A)') strCentre('fam_desc (FAMILY):', 62)
        write(funit, '(A)') repeat("=", 62)
        @:fortax_type_desc(fam, fam)
        write(funit, '(A)') repeat("=", 62)

        ! write(funit, *)
        ! write(funit, '(A)') repeat("=", 62)
        write(funit, '(A)') strCentre('fam_desc (ADULT 1):', 62)
        write(funit, '(A)') repeat("=", 62)
        @:fortax_type_desc(famad, fam%ad(1))
        write(funit, '(A)') repeat("=", 62)

        if (fam%couple == 1) then
        ! write(funit, *)
        ! write(funit, '(A)') repeat("=", 62)
        write(funit, '(A)') strCentre('fam_desc (ADULT 2):', 62)
        write(funit, '(A)') repeat("=", 62)
        @:fortax_type_desc(famad, fam%ad(2))
        write(funit, '(A)') repeat("=", 62)
        end if
        write(funit, *)

        close(funit)

    end subroutine fam_desc

    ! obtain string labels for the variable value labels
    @:fortax_get_label()

    ! fam_gen
    ! -----------------------------------------------------------------------
    ! will return fam setting any characteristics to the values that are
    ! specified. Adult information should be passed by adding a suffix 1 or 2
    ! for the respective adult number.

    @:fortax_fam_gen()


    ! net_plus_net
    ! ------------------------------------
    ! Addition of two net_t type variables

    function net_plus_net(net1, net2) result(net)
        implicit none
        type(net_t), intent(in) :: net1, net2
        type(net_t) :: net

        #:for ad in [1, 2]
        @:fortax_net_operator(net, netad, net1, net2, ad(${ad}$), +)
        #:endfor

        @:fortax_net_operator(net, nettu, net1, net2, tu, +)

    end function net_plus_net



    ! net_minus_net
    ! --------------------------------------
    ! Difference of two net_t type variables

    function net_minus_net(net1, net2) result(net)
        implicit none
        type(net_t), intent(in) :: net1, net2
        type(net_t) :: net

        #:for ad in [1, 2]
        @:fortax_net_operator(net, netad, net1, net2, ad(${ad}$), -)
        #:endfor

        @:fortax_net_operator(net, nettu, net1, net2, tu, -)

    end function net_minus_net


    ! net_times_scalar
    ! ------------------------------------
    ! Multiply net_t type variable by scalar

    function net_times_scalar(net1, scalar) result(net)
        implicit none
        type(net_t), intent(in) :: net1
        real(dp), intent(in) :: scalar
        type(net_t) :: net

        #:for ad in [1, 2]
        @:fortax_net_operator(net, netad, net1, net2, ad(${ad}$), *, scalar2 = scalar)
        #:endfor

        @:fortax_net_operator(net, nettu, net1, net2, tu, *, scalar2 = scalar)

    end function net_times_scalar


    ! net_times_scalar_integer
    ! ------------------------------------
    ! Multiply net_t type variable by an integer scalar

    function net_times_scalar_integer(net1, int_scalar) result(net)
        implicit none
        type(net_t), intent(in) :: net1
        integer, intent(in) :: int_scalar
        type(net_t) :: net
        net = net_times_scalar(net1, real(int_scalar, dp))
    end function net_times_scalar_integer


    ! scalar_times_net
    ! ------------------------------------
    ! Multiply net_t type variable by scalar

    function scalar_times_net(scalar, net2) result(net)
        implicit none
        real(dp), intent(in) :: scalar
        type(net_t), intent(in) :: net2
        type(net_t) :: net

        #:for ad in [1, 2]
        @:fortax_net_operator(net, netad, net1, net2, ad(${ad}$), *, scalar1 = scalar)
        #:endfor

        @:fortax_net_operator(net, nettu, net1, net2, tu, *, scalar1 = scalar)

    end function scalar_times_net


    ! scalar_times_net_integer
    ! ------------------------------------
    ! Multiply net_t type variable by scalar

    function scalar_times_net_integer(int_scalar, net2) result(net)
        implicit none
        integer, intent(in) :: int_scalar
        type(net_t), intent(in) :: net2
        type(net_t) :: net
        net = scalar_times_net(real(int_scalar, dp), net2)
    end function scalar_times_net_integer


    ! net_div_scalar
    ! ------------------------------------
    ! Divide net_t type variable by scalar

    function net_div_scalar(net1, scalar) result(net)
        implicit none
        type(net_t), intent(in) :: net1
        real(dp), intent(in) :: scalar
        type(net_t) :: net

        #:for ad in [1, 2]
        @:fortax_net_operator(net, netad, net1, net2, ad(${ad}$), /, scalar2 = scalar)
        #:endfor

        @:fortax_net_operator(net, nettu, net1, net2, tu, /, scalar2 = scalar)

    end function net_div_scalar


    ! net_div_scalar_integer
    ! ------------------------------------
    ! Divide net_t type variable by scalar

    function net_div_scalar_integer(net1, int_scalar) result(net)
        implicit none
        type(net_t), intent(in) :: net1
        integer, intent(in) :: int_scalar
        type(net_t) :: net
        net = net_div_scalar(net1, real(int_scalar, dp))
    end function net_div_scalar_integer


    ! net_init
    ! -----------------------------------------------------------------------
    ! intializes net_t type. unless defaults are coded here, integers are
    ! set to 0, doubles to 0.0_dp and logicals to .false. (and similarly
    ! for arrays)

    elemental subroutine net_init(net)

        implicit none

        type(net_t), intent(out) :: net
        integer :: ad

        @:fortax_type_init(nettu, net%tu)
        @:fortax_type_init(netad, net%ad(1))
        @:fortax_type_init(netad, net%ad(2))

    end subroutine net_init


    ! net_desc
    ! -----------------------------------------------------------------------
    ! will display the information contained in the net income structure and
    ! write this to a file if fname is specified

    subroutine net_desc(net, fname)

        use fortax_util, only : intToStr, strCentre, fortaxError

        use, intrinsic :: iso_fortran_env

        type(net_t), intent(in) :: net
        character(len = *), optional   :: fname

        integer :: funit, i, ios

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit, *)
        write(funit, '(A)') repeat("=", 62) 
        write(funit, '(A)') strCentre('net_desc (TAX UNIT):', 62)
        write(funit, '(A)') repeat("=", 62) 
        @:fortax_type_desc(nettu, net%tu)
        write(funit, '(A)') repeat("=", 62) 

        ! write(funit, *)
        ! write(funit, '(A)') repeat("=", 62) 
        write(funit, '(A)') strCentre('net_desc (ADULT 1):', 62)
        write(funit, '(A)') repeat("=", 62) 
        @:fortax_type_desc(netad, net%ad(1))
        write(funit, '(A)') repeat("=", 62) 

        ! write(funit, *)
        ! write(funit, '(A)') repeat("=", 62) 
        write(funit, '(A)') strCentre('net_desc (ADULT 2):', 62)
        write(funit, '(A)') repeat("=", 62) 
        @:fortax_type_desc(netad, net%ad(2))
        write(funit, '(A)') repeat("=", 62) 
        close(funit)

    end subroutine net_desc


    ! sys_init
    ! -----------------------------------------------------------------------
    ! intializes sys_t type. unless defaults are coded here, integers are
    ! set to 0, doubles to 0.0_dp and logicals to .false. (and similarly
    ! for arrays)

    elemental subroutine sys_init(sys)

        implicit none

        type(sys_t), intent(out) :: sys

        sys%sysname = ''
        sys%sysdesc = ''
        #:for SYS in SYSLIST

        @:fortax_type_init(${SYS}$, sys%${SYS}$)
        #:endfor

    end subroutine sys_init


    ! sys_saveF90
    ! -----------------------------------------------------------------------
    ! write as a file the contents of the system file which can then be
    ! directly included in the source code of the calling program

    subroutine sys_saveF90(sys, fname)

        use fortax_util, only : fortaxError
        use, intrinsic :: iso_fortran_env

        implicit none

        type(sys_t), intent(in) :: sys
        character(len = *), intent(in), optional :: fname

        integer :: funit, ios

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit, '(A)') '! .f90 FORTAX system; generated using sys_saveF90'
        write(funit, *)

        write(funit, '(A)') 'call sys_init(sys)'

        #:for SYS in SYSLIST
        write(funit, *)
        write(funit, '(A)') "! ${SYS}$"
        @:fortax_type_save(${SYS}$, sys%${SYS}$)

        #:endfor

        write(funit, *)
        write(funit, '(A)') '! .f90 FORTAX system; END-OF-FILE'
        write(funit, *)

        if (present(fname)) close(funit)

    end subroutine sys_saveF90


    ! fam_saveF90
    ! -----------------------------------------------------------------------
    ! write as a file the contents of the family type which can then be
    ! directly included in the source code of the calling program

    subroutine fam_saveF90(fam, fname)

        use fortax_util, only : fortaxError
        use, intrinsic :: iso_fortran_env

        implicit none

        type(fam_t), intent(in) :: fam
        character(len = *), intent(in), optional :: fname

        integer :: funit, ios

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit, '(A)') '! .f90 FORTAX family; generated using fam_saveF90'
        write(funit, *)

        write(funit, '(A)') 'call fam_init(fam)'

        @:fortax_type_save(fam, fam)

        @:fortax_type_save(famad, fam%ad(1))

        @:fortax_type_save(famad, fam%ad(2))

        write(funit, *)
        write(funit, '(A)') '! .f90 FORTAX family; END-OF-FILE'
        write(funit, *)

        if (present(fname)) close(funit)

    end subroutine fam_saveF90

    subroutine write_f90integer(funit, str, val)
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: str
        integer, intent(in) :: val
        write(funit, '(A, " = ", I0)') str, val
    end subroutine write_f90integer

    subroutine write_f90integerarray(funit, str, val)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: str
        integer, intent(in) :: val(:)
        integer :: ix
        do ix = 1, size(val)
            write(funit, '(A, " = ", I0)') str // "(" // intToStr(iX) // ")", val(ix)
        end do
    end subroutine write_f90integerarray

    subroutine write_f90integerarray2(funit, str, val, nval)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: str
        integer, intent(in) :: val(:)
        integer, intent(in) :: nval
        integer :: ix
        do ix = 1, nval
            write(funit, '(A, " = ", I0)') str // "(" // intToStr(iX) // ")", val(ix)
        end do
    end subroutine write_f90integerarray2

    subroutine write_f90double(funit, str, val)
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: str
        real(dp), intent(in) :: val
        write(funit, '(A, " = ", ES24.17)') str, val
    end subroutine write_f90double

    subroutine write_f90doublearray(funit, str, val)
        use fortax_util, only : intToStr        
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: str
        real(dp), intent(in) :: val(:)
        integer :: ix
        do ix = 1, size(val)
            write(funit, '(A, " = ", ES24.17)') str // "(" // intToStr(iX) // ")", val(ix)
        end do
    end subroutine write_f90doublearray

    subroutine write_f90doublearray2(funit, str, val, nval)
        use fortax_util, only : intToStr        
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: str
        real(dp), intent(in) :: val(:)
        integer, intent(in) :: nval
        integer :: ix
        do ix = 1, nval
            write(funit, '(A, " = ", ES24.17)') str // "(" // intToStr(iX) // ")", val(ix)
        end do
    end subroutine write_f90doublearray2

    subroutine desc_f90integer(funit, longstr, shortstr, val)
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        integer, intent(in) :: val
        if (longstr .ne. "") then
            write(funit, '(A40, 2X, I20)') longstr // ' (' // shortstr // ')', val
        else
            write(funit, '(A40, 2X, I20)') shortstr, val
        end if
    end subroutine desc_f90integer

    subroutine desc_f90integer_label(funit, longstr, shortstr, val, label)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        integer, intent(in) :: val
        character(len = len_label), intent(in) :: label
        if (longstr .ne. "") then
            write(funit, '(A40, 2X, A20)') longstr // ' (' // shortstr // ')', &
                trim(adjustl(label)) // ' (' // intToStr(val) // ')'
        else
            write(funit, '(A40, 2X, A20)') shortstr, label
        end if
    end subroutine desc_f90integer_label

    subroutine desc_f90integerarray(funit, longstr, shortstr, val)
        use fortax_util, only : intToStr        
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        integer, intent(in) :: val(:)
        integer :: ix
        if (longstr .ne. "") then
            do ix = 1, size(val)
                write(funit, '(A40, 2X, I20)') longstr // ' (' // shortstr // '[' // intToStr(ix) // '])', val(ix)
            end do
        else
            do ix = 1, size(val)
                write(funit, '(A40, 2X, I20)') shortstr // '[' // intToStr(ix) // ']', val(ix)
            end do
        end if
    end subroutine desc_f90integerarray

    subroutine desc_f90integerarray_label(funit, longstr, shortstr, val, label)
        use fortax_util, only : intToStr        
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        integer, intent(in) :: val(:)
        character(len = len_label), intent(in) :: label(:)
        integer :: ix
        if (longstr .ne. "") then
            do ix = 1, size(val)
                write(funit, '(A40, 2X, I20)') longstr // ' (' // shortstr // '[' // intToStr(ix) // '])', label(ix)
            end do
        else
            do ix = 1, size(val)
                write(funit, '(A40, 2X, I20)') shortstr // '[' // intToStr(ix) // ']', label(ix)
            end do
        end if
    end subroutine desc_f90integerarray_label

    subroutine desc_f90integerarray2(funit, longstr, shortstr, val, nval)
        use fortax_util, only : intToStr        
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        integer, intent(in) :: val(:)
        integer, intent(in) :: nval
        integer :: ix
        if (longstr .ne. "") then
            do ix = 1, nval
                write(funit, '(A40, 2X, I20)') longstr // ' (' // shortstr // '[' // intToStr(ix) // '])', val(ix)
            end do
        else
            do ix = 1, nval
                write(funit, '(A40, 2X, I20)') shortstr // '[' // intToStr(ix) // ']', val(ix)
            end do
        end if
    end subroutine desc_f90integerarray2

    subroutine desc_f90integerarray2_label(funit, longstr, shortstr, val, nval, label)
        use fortax_util, only : intToStr        
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        integer, intent(in) :: val(:)
        integer, intent(in) :: nval
        character(len = len_label), intent(in) :: label(:)
        integer :: ix
        if (longstr .ne. "") then
            do ix = 1, nval
                write(funit, '(A40, 2X, I20)') longstr // ' (' // shortstr // '[' // intToStr(ix) // '])', label(ix)
            end do
        else
            do ix = 1, nval
                write(funit, '(A40, 2X, I20)') shortstr // '[' // intToStr(ix) // ']', label(ix)
            end do
        end if
    end subroutine desc_f90integerarray2_label

    subroutine desc_f90double(funit, longstr, shortstr, val)
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        real(dp), intent(in) :: val
        if (longstr .ne. "") then
            write(funit, '(A40, 2X, F20.4)') longstr // ' (' // shortstr // ')', val
        else
            write(funit, '(A40, 2X, F20.4)') shortstr, val
        end if
    end subroutine desc_f90double

    subroutine desc_f90doublearray(funit, longstr, shortstr, val)
        use fortax_util, only : intToStr        
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        real(dp), intent(in) :: val(:)
        integer :: ix
        if (longstr .ne. "") then
            do ix = 1, size(val)
                write(funit, '(A40, 2X, F20.4)') longstr // ' (' // shortstr // '[' // intToStr(ix) // '])', val(ix)
            end do
        else
            do ix = 1, size(val)
                write(funit, '(A40, 2X, F20.4)') shortstr // '[' // intToStr(ix) // ']', val(ix)
            end do
        end if
    end subroutine desc_f90doublearray

    subroutine desc_f90doublearray2(funit, longstr, shortstr, val, nval)
        use fortax_util, only : intToStr        
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        real(dp), intent(in) :: val(:)
        integer, intent(in) :: nval
        integer :: ix
        if (longstr .ne. "") then
            do ix = 1, nval
                write(funit, '(A40, 2X, F20.4)') longstr // ' (' // shortstr // '[' // intToStr(ix) // '])', val(ix)
            end do
        else
            do ix = 1, nval
                write(funit, '(A40, 2X, F20.4)') shortstr // '[' // intToStr(ix) // ']', val(ix)
            end do
        end if
    end subroutine desc_f90doublearray2

end module fortax_type
