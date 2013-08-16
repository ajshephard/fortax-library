
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

    implicit none

    public

    integer, parameter :: sp = selected_real_kind(6,37)
    integer, parameter :: dp = selected_real_kind(15,307)
    integer, parameter :: ep = selected_real_kind(18,4931)
    integer, parameter :: qp = selected_real_kind(33,4931)

end module fortax_realtype
