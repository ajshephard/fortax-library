
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




! fortax_library
! -----------------------------------------------------------------------
! module provides easy access to all the other modules that are part of
! the fortax_library

module fortax_library

    use fortax_calc
    use fortax_compare
    use fortax_extra
    use fortax_kinks
    use fortax_prices
    use fortax_read
    use fortax_realtype
    use fortax_taxbenread
    use fortax_type
    use fortax_util
    use fortax_write

end module fortax_library
