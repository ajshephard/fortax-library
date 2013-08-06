
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

module fortax_type
              
    use fortax_realtype, only : dp
    
    implicit none
            
    private
    public :: fam_init, net_init, sys_init
    public :: fam_saveF90, sys_saveF90
    public :: lab_t, fam_t, net_t, sys_t
    public :: lab, maxkids
    public :: fam_gen, fam_desc
    public :: net_desc
    
    public :: ctax_banda, ctax_bandb, ctax_bandc, ctax_bandd, &
        & ctax_bande, ctax_bandf, ctax_bandg, ctax_bandh

#   ifndef _maxkids_
    integer, parameter :: maxKids = 10
#   else
    integer, parameter :: maxKids = _maxkids_
#   endif
#   undef _maxkids_

    type :: lab_t
        character(32), dimension(7)  :: tenure
        character(32), dimension(12) :: region
    end type lab_t

    type(lab_t), parameter :: lab = lab_t((/character(len=32) :: 'Own outright','Mortgage', &
        & 'Part own, part rent','Social renter','Private renter','Rent free','Other'/), &
        & (/character(len=32) :: 'North East','North West and Merseyside','Yorks and Humberside',&
        & 'East Midlands','West Midlands','Eastern','London','South East','South West','Wales',&
        & 'Scotland','Northern Ireland'/))

    integer, parameter :: ctax_banda = 1
    integer, parameter :: ctax_bandb = 2
    integer, parameter :: ctax_bandc = 3
    integer, parameter :: ctax_bandd = 4
    integer, parameter :: ctax_bande = 5
    integer, parameter :: ctax_bandf = 6
    integer, parameter :: ctax_bandg = 7
    integer, parameter :: ctax_bandh = 8


    ! famad_t
    ! -----------------------------------------------------------------------
    ! defines the adult level family type structure (see fam_t below)

    type :: famad_t

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,y) integer  :: x
#       define _$double(x,y)  real(dp) :: x
#       define _$logical(x,y) logical  :: x
#       define _$integerarray(x,y,z) integer  :: x(z)
#       define _$doublearray(x,y,z)  real(dp) :: x(z)
#       define _$logicalarray(x,y,z) logical  :: x(z)

#       include 'includes/famad_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray
    end type famad_t


    ! fam_t
    ! -----------------------------------------------------------------------
    ! defines the family type structure, containing information on 
    ! demographic characteristics, earnings, hours of work, and other 
    ! information. Anything that can affect the taxes and transfer payments 
    ! of a family is defined in here.

    type :: fam_t

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,y) integer  :: x
#       define _$double(x,y)  real(dp) :: x
#       define _$logical(x,y) logical  :: x
#       define _$integerarray(x,y,z) integer  :: x(z)
#       define _$doublearray(x,y,z)  real(dp) :: x(z)
#       define _$logicalarray(x,y,z) logical  :: x(z)

#       include 'includes/fam_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

        type(famad_t) :: ad(2)

    end type fam_t


    ! netad_t
    ! -----------------------------------------------------------------------
    ! defines the adult level information returned following calls to the 
    ! main calculation routines (see net_t below).
        
    type :: netad_t

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,lab,y) integer  :: x
#       define _$double(x,lab,y)  real(dp) :: x
#       define _$logical(x,lab,y) logical  :: x
#       define _$integerarray(x,lab,y,z) integer  :: x(z)
#       define _$doublearray(x,lab,y,z)  real(dp) :: x(z)
#       define _$logicalarray(x,lab,y,z) logical  :: x(z)

#       include 'includes/netad_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

    end type netad_t


    ! nettu_t
    ! -----------------------------------------------------------------------
    ! defines the tax unit level information returned following calls to the 
    ! main calculation routines (see net_t below).

    type :: nettu_t

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,lab,y) integer  :: x
#       define _$double(x,lab,y)  real(dp) :: x
#       define _$logical(x,lab,y) logical  :: x
#       define _$integerarray(x,lab,y,z) integer  :: x(z)
#       define _$doublearray(x,lab,y,z)  real(dp) :: x(z)
#       define _$logicalarray(x,lab,y,z) logical  :: x(z)

#       include 'includes/nettu_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

    end type nettu_t


    ! net_t
    ! -----------------------------------------------------------------------
    ! defines the information returned following calls to the main 
    ! calculation routines within fortax_calc. It contains measures of net 
    ! income, together with various tax amounts and other components of income

    type :: net_t
        type(netad_t) :: ad(2)
        type(nettu_t) :: tu
    end type net_t


    ! sys_t
    ! -----------------------------------------------------------------------
    ! defines the tax system structure which families of type fam_t face. 
    ! It describes all the parameters which are interpreted within the
    ! module fortax_calc

    ! I use a lot of preprocessor stuff for handling the sys_t type
    ! we just "include" it here to keep the code tidy, AS
#   include 'includes/sys_t.inc'
       
contains
    

    ! fam_init
    ! -----------------------------------------------------------------------
    ! intializes family structure. unless defaults are coded here, integers
    ! are set to 0, doubles to 0.0_dp and logicals to .false. (and similarly
    ! for arrays). Could possibly add defaults in fam_t, but given it is 
    ! only referenced here, there probably isn't much advantage to that

    elemental subroutine fam_init(fam)

        implicit none

        type(fam_t), intent(inout) :: fam

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,y) fam%x = 0
#       define _$double(x,y)  fam%x = 0.0_dp
#       define _$logical(x,y) fam%x = .false.
#       define _$integerarray(x,y,z) fam%x = 0
#       define _$doublearray(x,y,z)  fam%x = 0.0_dp
#       define _$logicalarray(x,y,z) fam%x = .false.

#       include 'includes/fam_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,y) fam%ad%x = 0
#       define _$double(x,y)  fam%ad%x = 0.0_dp
#       define _$logical(x,y) fam%ad%x = .false.
#       define _$integerarray(x,y,z) fam%ad(1)%x = 0; fam%ad(2)%x = 0
#       define _$doublearray(x,y,z)  fam%ad(1)%x = 0.0_dp; fam%ad(2)%x = 0.0_dp
#       define _$logicalarray(x,y,z) fam%ad(1)%x = .false.; fam%ad(2)%x = .false.

#       include 'includes/famad_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

        fam%ad(1)%age  = 25
        fam%ad(2)%age  = 0
        fam%tenure     = 1 !owner
        fam%region     = 1 !north east
        fam%ctBand     = 4 !band D
        fam%bandDRatio = 1.0_dp
        fam%intDate    = 19900101

    end subroutine fam_init


    ! fam_desc
    ! -----------------------------------------------------------------------
    ! will display the information contained in the family variable fam and
    ! write this to a file if fname is specified

    subroutine fam_desc(fam,fname)

        use fortax_util, only :  getUnit, intToStr, fortaxError

        use, intrinsic :: iso_fortran_env

        type(fam_t),      intent(in) :: fam
        character(len=*), optional   :: fname

        integer :: funit, i, ios

        if (present(fname)) then
            call getUnit(funit)
            open(funit,file=fname,action='write',status='replace',iostat=ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header write(funit,*); write(funit,'(A16)') 'FAMILY'; write(funit,*)
#       define _$footer
#       define _$integer(x,y) write(funit,'(A16,2X,I16)') #x, fam%x
#       define _$double(x,y)  write(funit,'(A16,2X,F16.4)') #x, fam%x
#       define _$logical(x,y) write(funit,'(A16,2X,L16)') #x, fam%x
#       define _$integerarray(x,y,z) do i =1, merge(min(fam%nkids,z),size(fam%x),#x=='kidage'); write(funit,'(A16,2X,I16)') #x//'('//inttostr(i)//')', fam%x(i); end do
#       define _$doublearray(x,y,z)  do i =1, size(fam%x); write(funit,'(A16,2X,F16.4)') #x//'('//inttostr(i)//')', fam%x(i); end do
#       define _$logicalarray(x,y,z) do i =1, size(fam%x); write(funit,'(A16,2X,L16)') #x//'('//inttostr(i)//')', fam%x(i); end do

#       include 'includes/fam_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header write(funit,*); write(funit,'(A16)') 'ADULT1'; write(funit,*)
#       define _$footer
#       define _$integer(x,y) write(funit,'(A16,2X,I16)') #x, fam%ad(1)%x
#       define _$double(x,y)  write(funit,'(A16,2X,F16.4)') #x, fam%ad(1)%x
#       define _$logical(x,y) write(funit,'(A16,2X,L16)') #x, fam%ad(1)%x
#       define _$integerarray(x,y,z) do i =1, size(fam%ad(1)%x); write(funit,'(A16,2X,I16)') #x//'('//inttostr(i)//')', fam%ad(1)%x(i); end do
#       define _$doublearray(x,y,z)  do i =1, size(fam%ad(1)%x); write(funit,'(A16,2X,F16.4)') #x//'('//inttostr(i)//')', fam%ad(1)%x(i); end do
#       define _$logicalarray(x,y,z) do i =1, size(fam%ad(1)%x); write(funit,'(A16,2X,L16)') #x//'('//inttostr(i)//')', fam%ad(1)%x(i); end do

#       include 'includes/famad_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

        if (fam%couple) then
#           define _$header write(funit,*); write(funit,'(A16)') 'ADULT2'; write(funit,*)
#           define _$footer
#           define _$integer(x,y) write(funit,'(A16,2X,I16)') #x, fam%ad(2)%x
#           define _$double(x,y)  write(funit,'(A16,2X,F16.4)') #x, fam%ad(2)%x
#           define _$logical(x,y) write(funit,'(A16,2X,L16)') #x, fam%ad(2)%x
#           define _$integerarray(x,y,z) do i =1, size(fam%ad(2)%x); write(funit,'(A16,2X,I16)') #x//'('//inttostr(i)//')', fam%ad(2)%x(i); end do
#           define _$doublearray(x,y,z)  do i =1, size(fam%ad(2)%x); write(funit,'(A16,2X,F16.4)') #x//'('//inttostr(i)//')', fam%ad(2)%x(i); end do
#           define _$logicalarray(x,y,z) do i =1, size(fam%ad(2)%x); write(funit,'(A16,2X,L16)') #x//'('//inttostr(i)//')', fam%ad(2)%x(i); end do

#           include 'includes/famad_t.inc'

#           undef  _$header
#           undef  _$footer
#           undef  _$integer
#           undef  _$double
#           undef  _$logical
#           undef  _$doublearray
#           undef  _$integerarray
#           undef  _$logicalarray
        end if

    end subroutine fam_desc


    ! fam_gen
    ! -----------------------------------------------------------------------
    ! will return fam setting any characteristics to the values that are 
    ! specified. Adult information should be passed by adding a suffix 1 or 2
    ! for the respective adult number.

    pure function fam_gen( &
#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,y) x, &
#       define _$double(x,y)  x, &
#       define _$logical(x,y) x, &
#       define _$integerarray(x,y,z) x, &
#       define _$doublearray(x,y,z)  x, &
#       define _$logicalarray(x,y,z) x, &
#       include 'includes/fam_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer correct)
#       define _$integer(x,y) x##1, x##2, &
#       define _$double(x,y)  x##1, x##2, &
#       define _$logical(x,y) x##1, x##2, &
#       define _$integerarray(x,y,z) x##1, x##2, &
#       define _$doublearray(x,y,z)  x##1, x##2, &
#       define _$logicalarray(x,y,z) x##1, x##2, &
#       include 'includes/famad_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,y) integer,  optional, intent(in) :: x
#       define _$double(x,y)  real(dp), optional, intent(in) :: x
#       define _$logical(x,y) logical,  optional, intent(in) :: x
#       define _$integerarray(x,y,z) integer,  optional, intent(in) :: x(:)
#       define _$doublearray(x,y,z)  real(dp), optional, intent(in) :: x(:)
#       define _$logicalarray(x,y,z) logical,  optional, intent(in) :: x(:)
#       include 'includes/fam_t.inc'
#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,y) integer,  optional, intent(in) :: x##1;\
                              integer,  optional, intent(in) :: x##2
#       define _$double(x,y)  real(dp), optional, intent(in) :: x##1;\
                              real(dp), optional, intent(in) :: x##2
#       define _$logical(x,y) logical,  optional, intent(in) :: x##1;\
                              logical,  optional, intent(in) :: x##2
#       define _$integerarray(x,y,z) integer,  optional, intent(in) :: x##1(:);\
                                     integer,  optional, intent(in) :: x##2(:)
#       define _$doublearray(x,y,z)  real(dp), optional, intent(in) :: x##1(:);\
                                     real(dp), optional, intent(in) :: x##2(:)
#       define _$logicalarray(x,y,z) logical,  optional, intent(in) :: x##1(:);\
                                     logical,  optional, intent(in) :: x##2(:)
#       include 'includes/famad_t.inc'
#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

        logical, intent(in), optional :: correct

        logical     :: correct2
        type(fam_t) :: fam_gen
        integer     :: kidsize

        call fam_init(fam_gen)

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,y) if (present(x)) fam_gen%x = x
#       define _$double(x,y)  if (present(x)) fam_gen%x = x
#       define _$logical(x,y) if (present(x)) fam_gen%x = x
#       define _$integerarray(x,y,z) if (present(x)) fam_gen%x(1:size(x)) = x
#       define _$doublearray(x,y,z)  if (present(x)) fam_gen%x(1:size(x)) = x
#       define _$logicalarray(x,y,z) if (present(x)) fam_gen%x(1:size(x)) = x
#       include 'includes/fam_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,y) if (present(x##1)) fam_gen%ad(1)%x = x##1; if (present(x##2)) fam_gen%ad(2)%x = x##2
#       define _$double(x,y)  if (present(x##1)) fam_gen%ad(1)%x = x##1; if (present(x##2)) fam_gen%ad(2)%x = x##2
#       define _$logical(x,y) if (present(x##1)) fam_gen%ad(1)%x = x##1; if (present(x##2)) fam_gen%ad(2)%x = x##2
#       define _$integerarray(x,y,z) if (present(x##1)) fam_gen%ad(1)%x(1:size(x##1)) = x##1;\
                                     if (present(x##2)) fam_gen%ad(2)%x(1:size(x##2)) = x##2
#       define _$doublearray(x,y,z)  if (present(x##1)) fam_gen%ad(1)%x(1:size(x##1)) = x##1;\
                                     if (present(x##2)) fam_gen%ad(2)%x(1:size(x##2)) = x##2
#       define _$logicalarray(x,y,z) if (present(x##1)) fam_gen%ad(1)%x(1:size(x##1)) = x##1;\
                                     if (present(x##2)) fam_gen%ad(2)%x(1:size(x##2)) = x##2
#       include 'includes/famad_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

        if (present(correct)) then
            correct2 = correct
        else
            correct2 = .true.
        end if

        !rules: 
        !if kidage is specified, then set nkids equal to size unless nkids is present
        !in this case, set nkids = max(nkids,size of kidage). kidage for additional kids
        !is then set to zero (or to yngkid if present and .le. age of those specified)
        !if kidage is not specified, any kids (equal to nkids if present) have age yngkid if present (otherwise zero)
        !if neither kidage, not nkids is present, but yngkid is present. assign a single child with this age

        if (correct2) then

            if ((.not. present(kidAge)) .and. (.not. (present(nkids))) .and. (present(yngKid))) then
                fam_gen%nkids = 1
                fam_gen%kidAge(1) = yngKid
            end if

            !how to handle kids
            if (present(kidAge)) then
                kidSize = size(kidAge)
                if (present(nkids)) then
                    if (nkids>size(kidAge)) then
                        !fam_gen%nkids = nkids
                        if (present(yngKid)) then
                            if (yngKid .le. minval(kidAge)) then
                                fam_gen%kidAge(kidSize+1:nkids) = yngKid
                            else
                                fam_gen%kidAge(kidSize+1:nkids) = 0
                                fam_gen%yngkid = 0
                            end if
                            fam_gen%kidAge(kidSize+1:nkids) = 0
                            fam_gen%yngKid = 0                    
                        end if
                    else    
                        fam_gen%nkids  = kidSize
                        fam_gen%yngKid = minval(kidAge)
                    end if
                else
                    fam_gen%nkids = kidSize
                    fam_gen%yngKid = minval(kidAge)
                end if
            else
                if (present(nkids)) then
                    if (present(yngKid)) then
                        fam_gen%kidAge(1:nkids) = yngKid
                    else
                        fam_gen%kidAge(1:nkids) = 0
                    end if
                end if
            end if
        
            !if married true, then couple is true regardless of whether specified
            if (fam_gen%married) fam_gen%couple = .true.
                    
            !automatically set couple=.true. if any second adult information is passed
            if (.not. (fam_gen%couple)) then
            
#               define _$header
#               define _$footer
#               define _$integer(x,y) if (present(x##2)) fam_gen%couple=.true.
#               define _$double(x,y)  if (present(x##2)) fam_gen%couple=.true.
#               define _$logical(x,y) if (present(x##2)) fam_gen%couple=.true.
#               define _$integerarray(x,y,z) if (present(x##2)) fam_gen%couple=.true.
#               define _$doublearray(x,y,z)  if (present(x##2)) fam_gen%couple=.true.
#               define _$logicalarray(x,y,z) if (present(x##2)) fam_gen%couple=.true.
#               include 'includes/famad_t.inc'

#               undef  _$header
#               undef  _$footer
#               undef  _$integer
#               undef  _$double
#               undef  _$logical
#               undef  _$doublearray
#               undef  _$integerarray
#               undef  _$logicalarray
            
            end if
            
            if (.not. fam_gen%couple) fam_gen%ad(2)%age = 0

        end if

    end function fam_gen

    
    ! net_init
    ! -----------------------------------------------------------------------
    ! intializes net_t type. unless defaults are coded here, integers are
    ! set to 0, doubles to 0.0_dp and logicals to .false. (and similarly
    ! for arrays)

    elemental subroutine net_init(net)

        implicit none

        type(net_t), intent(inout) :: net
        integer                    :: ad

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,lab,y) net%_$level(ad)%x = 0
#       define _$double(x,lab,y)  net%_$level(ad)%x = 0.0_dp
#       define _$logical(x,lab,y) net%_$level(ad)%x = .false.
#       define _$integerarray(x,lab,y,z) net%_$level(ad)%x = 0
#       define _$doublearray(x,lab,y,z)  net%_$level(ad)%x = 0.0_dp
#       define _$logicalarray(x,lab,y,z) net%_$level(ad)%x = .false.

#       define _$level ad
        do ad = 1, 2
#           include 'includes/netad_t.inc'
        end do
#       undef _$level

#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$integer(x,lab,y) net%_$level%x = 0
#       define _$double(x,lab,y)  net%_$level%x = 0.0_dp
#       define _$logical(x,lab,y) net%_$level%x = .false.
#       define _$integerarray(x,lab,y,z) net%_$level%x = 0
#       define _$doublearray(x,lab,y,z)  net%_$level%x = 0.0_dp
#       define _$logicalarray(x,lab,y,z) net%_$level%x = .false.

#       define _$level tu
#       include 'includes/nettu_t.inc'
#       undef _$level

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

    end subroutine net_init


    ! net_desc
    ! -----------------------------------------------------------------------
    ! will display the information contained in the net income structure and
    ! write this to a file if fname is specified

    subroutine net_desc(net,fname)

        use fortax_util, only :  getUnit, intToStr, fortaxError

        use, intrinsic :: iso_fortran_env

        type(net_t),      intent(in) :: net
        character(len=*), optional   :: fname

        integer :: funit, i, ios

        if (present(fname)) then
            call getUnit(funit)
            open(funit,file=fname,action='write',status='replace',iostat=ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header write(funit,*); write(funit,'(A40)') 'TAX UNIT'; write(funit,*)
#       define _$footer

#       define _$integer(x,y,z) write(funit,'(A40,2X,I16)') y//' ('//#x//')', net%tu%x
#       define _$double(x,y,z)  write(funit,'(A40,2X,F16.4)') y//' ('//#x//')', net%tu%x
#       define _$logical(x,y,z) write(funit,'(A40,2X,L16)') y//' ('//#x//')', net%tu%x

#       include 'includes/nettu_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header write(funit,*); write(funit,'(A40)') 'ADULT 1'; write(funit,*)
#       define _$footer

#       define _$integer(x,y,z) write(funit,'(A40,2X,I16)') y//' ('//#x//')', net%ad(1)%x
#       define _$double(x,y,z)  write(funit,'(A40,2X,F16.4)') y//' ('//#x//')', net%ad(1)%x
#       define _$logical(x,y,z) write(funit,'(A40,2X,L16)') y//' ('//#x//')', net%ad(1)%x

#       include 'includes/netad_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header write(funit,*); write(funit,'(A40)') 'ADULT 2'; write(funit,*)
#       define _$footer

#       define _$integer(x,y,z) write(funit,'(A40,2X,I16)') y//' ('//#x//')', net%ad(2)%x
#       define _$double(x,y,z)  write(funit,'(A40,2X,F16.4)') y//' ('//#x//')', net%ad(2)%x
#       define _$logical(x,y,z) write(funit,'(A40,2X,L16)') y//' ('//#x//')', net%ad(2)%x

#       include 'includes/netad_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray      
          
    end subroutine net_desc
    
    !again, i include the file here as it contains a lot of
    !preprocessor directives, AS    
#   include 'includes/sys_init.inc'
    

    ! sys_init
    ! -----------------------------------------------------------------------
    ! intializes sys_t type. unless defaults are coded here, integers are
    ! set to 0, doubles to 0.0_dp and logicals to .false. (and similarly
    ! for arrays)

    subroutine sys_init(sys)

        implicit none

        type(sys_t), intent(inout) :: sys
        
#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header call _$append(_$typelist)_init(sys%_$typelist)
#       define _$footer
#       define _$integer(x,y)
#       define _$double(x,y)
#       define _$logical(x,y)
#       define _$integerarray(x,y,z)
#       define _$doublearray(x,y,z)
#       define _$logicalarray(x,y,z)

#       include 'includes/system/syslist.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

    end subroutine sys_init


    ! sys_saveF90
    ! -----------------------------------------------------------------------
    ! write as a file the contents of the system file which can then be 
    ! directly included in the source code of the calling program

#   include 'includes/sys_save.inc'
    
    subroutine sys_saveF90(sys,fname)

        !use fortax_type, only : sys_t
        use fortax_util, only : getUnit, fortaxError

        use, intrinsic :: iso_fortran_env

        implicit none

        type(sys_t), intent(in) :: sys
        character(*), intent(in), optional :: fname

        integer :: funit, ios

        if (present(fname)) then
            call getUnit(funit)
            open(funit,file=fname,action='write',status='replace',iostat=ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit,'(a)') '! .f90 FORTAX system; generated using sys_saveF90'
        write(funit,*)

        write(funit,'(a)') 'call sys_init(sys) !deallocates arrays and sets values to default'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header call _$append(_$typelist)_save(sys%_$typelist,funit)
#       define _$footer
#       define _$integer(x,y)
#       define _$double(x,y)
#       define _$logical(x,y)
#       define _$integerarray(x,y,z)
#       define _$doublearray(x,y,z)
#       define _$logicalarray(x,y,z)

#       include 'includes/system/syslist.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

        write(funit,*)
        write(funit,'(a)') '!.f90 FORTAX system; END-OF-FILE'
        write(funit,*)

        if (present(fname)) close(funit)

    end subroutine sys_saveF90


    ! fam_saveF90
    ! -----------------------------------------------------------------------
    ! write as a file the contents of the family type which can then be 
    ! directly included in the source code of the calling program

    subroutine fam_saveF90(fam,fname)

        !use fortax_type, only : sys_t
        use fortax_util, only : getUnit, dblToStr, intToStr, fortaxError

        use, intrinsic :: iso_fortran_env

        implicit none

        type(fam_t), intent(in) :: fam
        character(*), intent(in), optional :: fname

        integer :: funit, ios, i

        if (present(fname)) then
            call getUnit(funit)
            open(funit,file=fname,action='write',status='replace',iostat=ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit,'(a)') '! .f90 FORTAX family; generated using fam_saveF90'
        write(funit,*)

        write(funit,'(a)') 'call fam_init(fam) !deallocates arrays and sets values to default'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header write(funit,*); write(funit,'(a)') '!family'
#       define _$footer
#       define _$integer(x,y) write(funit,'(a)',advance="no") "fam%"//#x//"="; write(funit,'(a)') intToStr(fam% x)
#       define _$double(x,y)  write(funit,'(a)',advance="no") "fam%"//#x//"="; write(funit,'(a)') dblToStr(fam% x)//'_dp'
#       define _$logical(x,y) write(funit,'(a)',advance="no") "fam%"//#x//"="; write(funit,'(a)') merge('.true. ','.false.',fam% x)

#       define _$doublearray(x,y,z) if (#z==':') write(funit,'(a)') "allocate(fam%"//#x//"("//intToStr(size(fam%x))//"))"; \
 do i=1,size(fam%x); write(funit,'(a)',advance="no") \
 "fam%"//#x//"("//intToStr(i)//")="; write(funit,'(a)') dblToStr(fam% x(i))//'_dp'; end do

#       define _$logicalarray(x,y,z) if (#z==':') write(funit,'(a)') "allocate(fam%"//#x//"("//intToStr(size(fam%x))//"))"; \
 do i=1,size(fam%x); write(funit,'(a)',advance="no") \
 "fam%"//#x//"("//intToStr(i)//")="; write(funit,'(a)') merge('.true. ','.false.',fam% x(i)); end do

#       define _$integerarray(x,y,z) if (#z==':') write(funit,'(a)') "allocate(fam%"//#x//"("//intToStr(size(fam%x))//"))"; \
 do i=1,size(fam%x); write(funit,'(a)',advance="no") \
 "fam%"//#x//"("//intToStr(i)//")="; write(funit,'(a)') intToStr(fam% x(i)); end do

#       include 'includes/fam_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header write(funit,*); write(funit,'(a)') '!adult1'
#       define _$footer
#       define _$integer(x,y) write(funit,'(a)',advance="no") "fam%ad(1)%"//#x//"="; write(funit,'(a)') intToStr(fam%ad(1)% x)
#       define _$double(x,y)  write(funit,'(a)',advance="no") "fam%ad(1)%"//#x//"="; write(funit,'(a)') dblToStr(fam%ad(1)% x)//'_dp'
#       define _$logical(x,y) write(funit,'(a)',advance="no") "fam%ad(1)%"//#x//"="; write(funit,'(a)') merge('.true. ','.false.',fam%ad(1)% x)

#       define _$doublearray(x,y,z) if (#z==':') write(funit,'(a)') "allocate(fam%ad(1)%"//#x//"("//intToStr(size(fam%ad(1)%x))//"))"; \
 do i=1,size(fam%ad(1)%x); write(funit,'(a)',advance="no") \
 "fam%ad(1)%"//#x//"("//intToStr(i)//")="; write(funit,'(a)') dblToStr(fam%ad(1)% x(i))//'_dp'; end do

#       define _$logicalarray(x,y,z) if (#z==':') write(funit,'(a)') "allocate(fam%ad(1)%"//#x//"("//intToStr(size(fam%ad(1)%x))//"))"; \
 do i=1,size(fam%ad(1)%x); write(funit,'(a)',advance="no") \
 "fam%ad(1)%"//#x//"("//intToStr(i)//")="; write(funit,'(a)') merge('.true. ','.false.',fam%ad(1)% x(i)); end do

#       define _$integerarray(x,y,z) if (#z==':') write(funit,'(a)') "allocate(fam%ad(1)%"//#x//"("//intToStr(size(fam%ad(1)%x))//"))"; \
 do i=1,size(fam%ad(1)%x); write(funit,'(a)',advance="no") \
 "fam%ad(1)%"//#x//"("//intToStr(i)//")="; write(funit,'(a)') intToStr(fam%ad(1)% x(i)); end do
#       include 'includes/famad_t.inc'

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

        if (fam%couple) then
#           define _$header write(funit,*); write(funit,'(a)') '!adult2'
#           define _$footer
#           define _$integer(x,y) write(funit,'(a)',advance="no") "fam%ad(2)%"//#x//"="; write(funit,'(a)') intToStr(fam%ad(2)% x)
#           define _$double(x,y)  write(funit,'(a)',advance="no") "fam%ad(2)%"//#x//"="; write(funit,'(a)') dblToStr(fam%ad(2)% x)//'_dp'
#           define _$logical(x,y) write(funit,'(a)',advance="no") "fam%ad(2)%"//#x//"="; write(funit,'(a)') merge('.true. ','.false.',fam%ad(2)% x)

#           define _$doublearray(x,y,z) if (#z==':') write(funit,'(a)') "allocate(fam%ad(2)%"//#x//"("//intToStr(size(fam%ad(2)%x))//"))"; \
 do i=1,size(fam%ad(2)%x); write(funit,'(a)',advance="no") \
 "fam%ad(2)%"//#x//"("//intToStr(i)//")="; write(funit,'(a)') dblToStr(fam%ad(2)% x(i))//'_dp'; end do

#           define _$logicalarray(x,y,z) if (#z==':') write(funit,'(a)') "allocate(fam%ad(2)%"//#x//"("//intToStr(size(fam%ad(2)%x))//"))"; \
 do i=1,size(fam%ad(2)%x); write(funit,'(a)',advance="no") \
 "fam%ad(2)%"//#x//"("//intToStr(i)//")="; write(funit,'(a)') merge('.true. ','.false.',fam%ad(2)% x(i)); end do

#           define _$integerarray(x,y,z) if (#z==':') write(funit,'(a)') "allocate(fam%ad(2)%"//#x//"("//intToStr(size(fam%ad(2)%x))//"))"; \
 do i=1,size(fam%ad(2)%x); write(funit,'(a)',advance="no") \
 "fam%ad(2)%"//#x//"("//intToStr(i)//")="; write(funit,'(a)') intToStr(fam%ad(2)% x(i)); end do

#           include 'includes/famad_t.inc'

#           undef  _$header
#           undef  _$footer
#           undef  _$integer
#           undef  _$double
#           undef  _$logical
#           undef  _$doublearray
#           undef  _$integerarray
#           undef  _$logicalarray
        end if

        write(funit,*)
        write(funit,'(a)') '!.f90 FORTAX family; END-OF-FILE'
        write(funit,*)

        if (present(fname)) close(funit)

    end subroutine fam_saveF90

end module fortax_type
