
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

    private
    public  :: readFortaxParams, fortax_read_assign

    interface fortax_read_assign
        module procedure assign_integer
        module procedure assign_logical
        module procedure assign_double
        module procedure assign_integer_array
        module procedure assign_logical_array
        module procedure assign_double_array 
        module procedure assign_integer_array_fixed
        module procedure assign_logical_array_fixed
        module procedure assign_double_array_fixed  
    end interface fortax_read_assign
    

contains
    
    ! readFortaxParams
    ! -----------------------------------------------------------------------
    ! reads tax parameters from systemfile into a sys_t derived type. It
    ! supports files in the TAXBEN file format, as well as the native format
    ! used by FORTAX.

    subroutine readFortaxParams(sys,systemFile,prices)
    
        use xml_data_xmlfortax_t, only : read_xml_file_xmlfortax_t, system, sysname, sysdesc
        use fortax_util,          only : getunit, strToDouble, strToInt, strToLogical, lower, fortaxError, fortaxWarn
        use fortax_type,          only : sys_t, sys_init
                
        implicit none
        
        type(sys_t),       intent(out) :: sys
        character(len=*),  intent(in)  :: systemFile
        integer, optional, intent(in)  :: prices
        integer                        :: i, j
        logical                        :: isFile
        
        inquire(file=systemFile, exist=isFile)
        
        if (.not. isFile) then
            call fortaxError('system file does not exist ('//trim(adjustl(systemFile))//')')
        end if              
        
        call read_xml_file_xmlfortax_t(systemFile)
        call sys_init(sys)
        !use fpp so that the reading will fully reflect the data structure                    
#       include 'includes/fortax_typeread.inc'

        ! internal system name
        sys%sysname = sysname
        sys%sysdesc = sysdesc

        !free up memory
        do i = 1, size(system)
        
            if (associated(system(i)%finteger))  deallocate(system(i)%finteger)
            if (associated(system(i)%flogical))  deallocate(system(i)%flogical)
            if (associated(system(i)%fdouble))   deallocate(system(i)%fdouble)

            if (associated(system(i)%fintegerarray)) then
                do j = 1, size(system(i)%fintegerarray)
                    if (associated(system(i)%fintegerarray(j)%value)) &
                        & deallocate(system(i)%fintegerarray(j)%value)
                end do
                deallocate(system(i)%fintegerarray)
            end if
            
            if (associated(system(i)%flogicalarray)) then
                do j = 1, size(system(i)%flogicalarray)
                    if (associated(system(i)%flogicalarray(j)%value)) &
                        & deallocate(system(i)%flogicalarray(j)%value)
                end do
                deallocate(system(i)%flogicalarray)
            end if
                                
            if (associated(system(i)%fdoublearray)) then
                do j = 1, size(system(i)%fdoublearray)
                    if (associated(system(i)%fdoublearray(j)%value)) &
                        & deallocate(system(i)%fdoublearray(j)%value)
                end do
            end if
            deallocate(system(i)%fdoublearray)
        end do
        
        if (associated(system)) deallocate(system)
        
        if (present(prices)) sys%extra%prices = prices

    end subroutine readFortaxParams

        
    ! assign_double_array
    ! -----------------------------------------------------------------------
    ! copies pointer data to double array (used when readTaxParams is reading
    ! a fortax system file)

    subroutine assign_double_array(x,y)
        
        implicit none
    
        real(dp), intent(inout), allocatable :: x(:)
        real(dp), pointer                    :: y(:)
        
        if (allocated(x)) deallocate(x)
        allocate(x(size(y)))
        
        x = y
    
    end subroutine assign_double_array

    ! assign_double_array_fixed
    ! -----------------------------------------------------------------------
    ! copies pointer data to double array (used when readTaxParams is reading
    ! a fortax system file)

    subroutine assign_double_array_fixed(x,y,n)
        
        implicit none
    
        integer,  intent(in)  :: n   
        real(dp), intent(out) :: x(n)
        real(dp), pointer     :: y(:)
        integer :: ny

        ny = size(y)

        if (ny<n) then
            x(1:ny)   = y
            x(ny+1:n) = 0.0_dp
        else if (ny==n) then
            x = y
        else if (ny>n) then
            x = y(1:n)
        end if
    
    end subroutine assign_double_array_fixed
   
    
    ! assign_integer_array
    ! -----------------------------------------------------------------------
    ! copies pointer data to integer array (used when readTaxParams is reading
    ! a fortax system file)

    subroutine assign_integer_array(x,y)
        
        implicit none
    
        integer, intent(inout), allocatable :: x(:)
        integer, pointer                    :: y(:)
        
        if (allocated(x)) deallocate(x)
        allocate(x(size(y)))
        
        x = y
    
    end subroutine assign_integer_array

    ! assign_integer_array_fixed
    ! -----------------------------------------------------------------------
    ! copies pointer data to integer array (used when readTaxParams is reading
    ! a fortax system file)

    subroutine assign_integer_array_fixed(x,y,n)
        
        implicit none
    
        integer, intent(in)    :: n
        integer, intent(out)   :: x(n)
        integer, pointer       :: y(:)
        integer :: ny

        ny = size(y)

        if (ny<n) then
            x(1:ny)   = y
            x(ny+1:n) = 0
        else if (ny==n) then
            x = y
        else if (ny>n) then
            x = y(1:n)
        end if
    
    end subroutine assign_integer_array_fixed

    
    ! assign_logical_array
    ! -----------------------------------------------------------------------
    ! copies pointer data to logical array (used when readTaxParams is reading
    ! a fortax system file)

    subroutine assign_logical_array(x,y)
        
        implicit none
    
        logical, intent(inout), allocatable :: x(:)
        logical, pointer                    :: y(:)
        
        if (allocated(x)) deallocate(x)
        allocate(x(size(y)))
        
        x = y
    
    end subroutine assign_logical_array

    ! assign_logical_array_fixed
    ! -----------------------------------------------------------------------
    ! copies pointer data to logical array (used when readTaxParams is reading
    ! a fortax system file)

    subroutine assign_logical_array_fixed(x,y,n)
        
        implicit none
    
        integer, intent(in)  :: n
        logical, intent(out) :: x(n)
        logical, pointer     :: y(:)
        integer :: ny

        ny = size(y)

        if (ny<n) then
            x(1:ny)    = y
            x(ny+1:ny) = .false.
        else if (ny==n) then
            x = y
        else if (ny>n) then
            x = y(1:n)
        end if

    end subroutine assign_logical_array_fixed

    ! assign_double
    ! -----------------------------------------------------------------------
    ! copies pointer data to double (used when readTaxParams is reading a
    ! fortax system file)

    subroutine assign_double(x,y)
    
        implicit none
    
        real(dp), intent(out) :: x
        real(dp), intent(in)  :: y
        
        x = y
    
    end subroutine assign_double


    ! assign_integer
    ! -----------------------------------------------------------------------
    ! copies pointer data to integer (used when readTaxParams is reading a
    ! fortax system file)

    subroutine assign_integer(x,y)
        
        implicit none
    
        integer, intent(out) :: x
        integer, intent(in)  :: y

        x = y
    
    end subroutine assign_integer


    ! assign_logical
    ! -----------------------------------------------------------------------
    ! copies pointer data to logical (used when readTaxParams is reading a
    ! fortax system file)

    subroutine assign_logical(x,y)
        
        implicit none
    
        logical, intent(out) :: x
        logical, intent(in)  :: y
        
        x = y
    
    end subroutine assign_logical
            
end module fortax_read
