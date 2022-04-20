
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

#:include "fortax.fypp"

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

        #:for SYS in SYSLIST
        call json%create_object(inp, "${SYS}$")
        call json%add(p, inp) !add it to the root
        @:fortax_sys_write(inp, ${SYS}$, sys%${SYS}$)
        nullify(inp)

        #:endfor

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
