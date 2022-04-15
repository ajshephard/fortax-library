
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

        use fortax_util,          only : getunit, strToDouble, strToInt, strToLogical, lower, fortaxError, fortaxWarn
        use fortax_type,          only : sys_t, sys_init

        implicit none

        type(sys_t), intent(out) :: sys
        character(len = *), intent(in) :: systemFile
        integer, optional, intent(in) :: prices

        if (present(prices)) sys%extra%prices = prices

    end subroutine readFortaxParams

end module fortax_read
