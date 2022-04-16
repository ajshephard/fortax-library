
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




! fortax_realtype
! -----------------------------------------------------------------------
! defines real precision types data types, AS

module fortax_realtype

    use iso_fortran_env

    implicit none

    public

    integer, parameter :: sp = real32
    integer, parameter :: dp = real64
    integer, parameter :: qp = real128

end module fortax_realtype
