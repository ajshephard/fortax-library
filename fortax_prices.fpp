
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




! fortax_prices
! -----------------------------------------------------------------------
! module provides date and price uprating capabilities for FORTAX, AS

! updated 22/06/13. removed global variables for storing price index
! introduced rpi_t defined in fortax_type

#:include "fortax.fypp"

module fortax_prices

    use fortax_realtype, only : dp
    private :: dp

    private

    public :: loadindex, loadindex2, setindex, getindex, upratefactor, upratesys, upratefam
    public :: checkdate, loadsysindex, loadsysindex2, getsysindex, getsysindex2, rpi_saveF90
    public :: operator(*), operator(/)

    interface operator(*)
        module procedure sys_times_factor
        module procedure sys_times_factor_integer
        module procedure factor_times_sys
        module procedure factor_times_sys_integer
        module procedure fam_times_factor
        module procedure fam_times_factor_integer
        module procedure factor_times_fam
        module procedure factor_times_fam_integer
    end interface

    interface operator(/)
        module procedure sys_div_factor
        module procedure sys_div_factor_integer
        module procedure fam_div_factor
        module procedure fam_div_factor_integer
    end interface

contains

    ! setindex
    ! -------------------------------------------------------------
    ! sets the price index data in rpi using the specified date and
    ! index information

    subroutine setindex(rpi, mydate, myindex, mysize)

        use fortax_util, only : fortaxwarn
        use fortax_type, only : rpi_t, maxRPI

        implicit none

        integer, intent(in) :: mysize
        real(dp), intent(in) :: myindex(mysize)
        integer, intent(in) :: mydate(mysize)
        type(rpi_t), intent(out) :: rpi

        if (mysize<=maxRPI) then
            rpi%date(1:mysize)  = mydate
            rpi%index(1:mysize) = myindex
            rpi%ndate = mysize
        else
            call fortaxwarn('out of range in call to setindex')
            rpi%date  = mydate(1:maxRPI)
            rpi%index = mydate(1:maxRPI)
            rpi%ndate = maxRPI
        end if

    end subroutine setindex


    ! loadindex
    ! -----------------------------------------------------------------------
    ! loads a price index file saved as a comma separated values (CSV) file.
    ! If fname is not specified it defaults to 'prices/rpi.csv'

    subroutine loadindex(rpi, fname)

        use fortax_type, only : rpi_t

        implicit none

        type(rpi_t), intent(out) :: rpi
        character(len = *), intent(in), optional :: fname

        call loadindex2(rpi%ndate, rpi%date, rpi%index, fname)

    end subroutine loadindex


    ! loadindex
    ! -----------------------------------------------------------------------
    ! loads a price index file saved as a comma separated values (CSV) file.
    ! If fname is not specified it defaults to 'prices/rpi.csv'

    subroutine loadindex2(ndate, date, index, fname)

        use fortax_util, only : fortaxerror, fortaxwarn, inttostr
        use fortax_type, only : maxRPI

        implicit none

        integer, intent(out) :: ndate
        integer, intent(out) :: date(maxRPI)
        real(dp), intent(out) :: index(maxRPI)
        character(len = *), intent(in), optional :: fname

        integer :: funit
        integer :: istat
        integer :: nrec

        logical :: isfile
        integer :: tempdate
        real(dp) :: tempindex

        if (present(fname)) then
            inquire(file = fname, exist = isfile)
            if (isfile) then
                open(newunit = funit, file = fname, status = 'old')
            else
                call fortaxerror('price index file does not exist (' // trim(adjustl(fname)) // ')')
            end if
        else
            inquire(file = 'prices/rpi.csv', exist = isfile)
            if (isfile) then
                open(newunit = funit, file = 'prices/rpi.csv', status = 'old')
            else
                call fortaxerror('default price index file does not exist')
            end if
        end if

        read (funit, *, iostat = istat) ndate

        if (ndate > maxRPI) then
            call fortaxwarn('declared ndate exceeds maxRPI')
        end if

        if (istat .ne. 0) then
            call fortaxerror('error reading number of records on line 1')
        end if

        nrec = 0

        do

            read(funit, *, iostat = istat) tempdate, tempindex

            if (istat == -1) then !eof
                exit
            else if (istat > 0) then !error
                call fortaxerror('error reading record after '//inttostr(nrec))
            else
                nrec = nrec + 1
                if (nrec > maxRPI) then
                    exit
                else
                    date(nrec)  = tempdate
                    index(nrec) = tempindex
                end if
            end if

        end do

        close(funit)

        if (nrec .ne. ndate) then
            call fortaxerror('number of rpi records does not equal number declared on line 1')
        end if

    end subroutine loadindex2

    ! getindex
    ! -----------------------------------------------------------------------
    ! returns the price index associated with the supplied YYYYMMDD date

    real(dp) elemental function getindex(rpi, date)

        use fortax_type, only : rpi_t

        implicit none

        type(rpi_t), intent(in) :: rpi
        integer, intent(in) :: date
        integer :: year,  month
        integer :: year1, month1

        !exploits structure of data
        if (date < rpi%date(1)) then
            getindex = 0.0_dp
        elseif (date > rpi%date(rpi%ndate)) then
            getindex = 0.0_dp
        else
            year = date / 10000
            month = (date - year * 10000) / 100
            year1 = rpi%date(1) / 10000
            month1 = (rpi%date(1) - year1 * 10000) / 100
            getindex = rpi%index((year - year1) * 12 + month)
        end if

    end function getindex


    ! upratefactor
    ! -----------------------------------------------------------------------
    ! returns uprating factor from date0 to date1 prices (both in YYYYMMDD
    ! format). it calls the function getindex.

    real(dp) elemental function upratefactor(rpi, date0, date1)

        use fortax_type, only : rpi_t

        implicit none

        type(rpi_t), intent(in) :: rpi
        integer, intent(in) :: date0, date1

        upratefactor = getindex(rpi, date1) / getindex(rpi, date0)

    end function upratefactor


    ! upratesys
    ! -----------------------------------------------------------------------
    ! uprates the tax system sys using the specified uprating factor. If
    ! newdate is present, it will set the prices attribute to this value.

    subroutine upratesys(sys, factor, newdate)

        use fortax_type, only : sys_t
        use fortax_util, only : fortaxwarn

        implicit none

        type(sys_t), intent(inout) :: sys
        real(dp), intent(in) :: factor
        integer, intent(in), optional :: newdate

        if (present(newdate)) sys%extra%prices = newdate

        #:for SYS in SYSLIST
        @:fortax_uprate(${SYS}$, sys%${SYS}$, factor, amount = True, minamount = True)
        #:endfor

    end subroutine upratesys

    subroutine upratefam(fam, factor)

        use fortax_type, only : fam_t
        use fortax_util, only : fortaxwarn

        implicit none

        type(fam_t), intent(inout) :: fam
        real(dp), intent(in) :: factor

        @:fortax_uprate(fam, fam, factor, amount = True, minamount = True)
        @:fortax_uprate(famad, fam%ad(1), factor, amount = True, minamount = True)
        @:fortax_uprate(famad, fam%ad(2), factor, amount = True, minamount = True)

    end subroutine upratefam

    function sys_times_factor(sys, factor) result(sys2)
        use fortax_type, only : sys_t
        implicit none
        type(sys_t), intent(in) :: sys
        real(dp), intent(in) :: factor
        type(sys_t) :: sys2
        #:for SYS in SYSLIST
        @:fortax_uprate_op(${SYS}$, sys2%${SYS}$, sys%${SYS}$, factor, amount = True, minamount = True)
        #:endfor
    end function sys_times_factor

    function sys_times_factor_integer(sys, factor) result(sys2)
        use fortax_type, only : sys_t
        implicit none
        type(sys_t), intent(in) :: sys
        integer, intent(in) :: factor
        type(sys_t) :: sys2
        sys2 = sys_times_factor(sys, real(factor, dp))
    end function sys_times_factor_integer

    function sys_div_factor(sys, factor) result(sys2)
        use fortax_type, only : sys_t
        implicit none
        type(sys_t), intent(in) :: sys
        real(dp), intent(in) :: factor
        type(sys_t) :: sys2
        sys2 = sys_times_factor(sys, (1.0_dp / factor))
    end function sys_div_factor

    function sys_div_factor_integer(sys, factor) result(sys2)
        use fortax_type, only : sys_t
        implicit none
        type(sys_t), intent(in) :: sys
        integer, intent(in) :: factor
        type(sys_t) :: sys2
        sys2 = sys_times_factor(sys, (1.0_dp / real(factor, dp)))
    end function sys_div_factor_integer



    function factor_times_sys(factor, sys) result(sys2)
        use fortax_type, only : sys_t
        implicit none
        real(dp), intent(in) :: factor
        type(sys_t), intent(in) :: sys
        type(sys_t) :: sys2
        #:for SYS in SYSLIST
        @:fortax_uprate_op(${SYS}$, sys2%${SYS}$, sys%${SYS}$, factor, amount = True, minamount = True)
        #:endfor
    end function factor_times_sys

    function factor_times_sys_integer(factor, sys) result(sys2)
        use fortax_type, only : sys_t
        implicit none
        integer, intent(in) :: factor
        type(sys_t), intent(in) :: sys
        type(sys_t) :: sys2
        sys2 = factor_times_sys(real(factor, dp), sys)
    end function factor_times_sys_integer


    function fam_times_factor(fam, factor) result(fam2)
        use fortax_type, only : fam_t
        implicit none
        type(fam_t), intent(in) :: fam
        real(dp), intent(in) :: factor
        type(fam_t) :: fam2
        @:fortax_uprate_op(fam, fam2, fam, factor, amount = True, minamount = True)
        @:fortax_uprate_op(famad, fam2%ad(1), fam%ad(1), factor, amount = True, minamount = True)
        @:fortax_uprate_op(famad, fam2%ad(2), fam%ad(2), factor, amount = True, minamount = True)
    end function fam_times_factor

    function fam_times_factor_integer(fam, factor) result(fam2)
        use fortax_type, only : fam_t
        implicit none
        type(fam_t), intent(in) :: fam
        integer, intent(in) :: factor
        type(fam_t) :: fam2
        fam2 = fam_times_factor(fam, real(factor, dp))
    end function fam_times_factor_integer

    function fam_div_factor(fam, factor) result(fam2)
        use fortax_type, only : fam_t
        implicit none
        type(fam_t), intent(in) :: fam
        real(dp), intent(in) :: factor
        type(fam_t) :: fam2
        fam2 = fam_times_factor(fam, (1.0_dp / factor))
    end function fam_div_factor

    function fam_div_factor_integer(fam, factor) result(fam2)
        use fortax_type, only : fam_t
        implicit none
        type(fam_t), intent(in) :: fam
        integer, intent(in) :: factor
        type(fam_t) :: fam2
        fam2 = fam_times_factor(fam, (1.0_dp / real(factor, dp)))
    end function fam_div_factor_integer

    function factor_times_fam(factor, fam) result(fam2)
        use fortax_type, only : fam_t
        implicit none
        real(dp), intent(in) :: factor
        type(fam_t), intent(in) :: fam
        type(fam_t) :: fam2
        @:fortax_uprate_op(fam, fam2, fam, factor, amount = True, minamount = True)
        @:fortax_uprate_op(famad, fam2%ad(1), fam%ad(1), factor, amount = True, minamount = True)
        @:fortax_uprate_op(famad, fam2%ad(2), fam%ad(2), factor, amount = True, minamount = True)
    end function factor_times_fam

    function factor_times_fam_integer(factor, fam) result(fam2)
        use fortax_type, only : fam_t
        implicit none
        integer, intent(in) :: factor
        type(fam_t), intent(in) :: fam
        type(fam_t) :: fam2
        fam2 = factor_times_fam(real(factor, dp), fam)
    end function factor_times_fam_integer

    ! loadsysindex
    ! -----------------------------------------------------------------------
    ! provides quick access to the actual system that individuals faced
    ! requires an external system index file (sysindexfile)

    subroutine loadsysindex(sysindex, sysindexfile)

        use fortax_util, only : fortaxerror, inttostr
        use fortax_type, only : sysindex_t, len_sysindex, maxSysIndex

        implicit none

        type(sysindex_t), intent(out) :: sysindex
        character(len = *), intent(in), optional :: sysindexfile

        call loadsysindex2(sysindex%nsys, sysindex%date0, sysindex%date1, sysindex%fname, sysindexfile)

    end subroutine loadsysindex


    ! loadsysindex2
    ! -----------------------------------------------------------------------
    ! provides quick access to the actual system that individuals faced
    ! requires an external system index file (sysindexfile)

    subroutine loadsysindex2(nsys, date0, date1, fname, sysindexfile)

        use fortax_util, only : fortaxerror, inttostr
        use fortax_type, only : len_sysindex, maxSysIndex
        use, intrinsic :: iso_c_binding

        implicit none

        integer, intent(out) :: nsys
        integer, intent(out) :: date0(maxSysIndex), date1(maxSysIndex)
        character(kind = c_char) :: fname(len_sysindex, maxSysIndex)
        character(len = *), intent(in), optional :: sysindexfile

        integer :: funit
        integer :: istat, nrec

        logical :: isfile
        integer :: tempdate0, tempdate1, ndate
        character(len = len_sysindex) :: tempfname

        if (present(sysindexfile)) then
            inquire(file = sysindexfile, exist = isfile)
            if (isfile) then
                open(newunit = funit, file = sysindexfile, status = 'old')
            else
                call fortaxerror('system index file does not exist (' // sysindexfile // ')')
            end if
        else
            inquire(file = 'systems/sysindex.csv', exist = isfile)
            if (isfile) then
                open(newunit = funit, file = 'systems/sysindex.csv', status = 'old')
            else
                call fortaxerror('default system index file does not exist')
            end if
        end if

        read (funit, *, iostat = istat) ndate

        !call freesysindex(sysindex)

        if (istat .ne. 0) then
            call fortaxerror('error reading number of records on line 1')
        else
            nsys = ndate
        end if

        nrec = 0

        do

            read(funit, *, iostat = istat) tempdate0, tempdate1, tempfname

            if (istat == -1) then !eof
                exit
            elseif (istat > 0) then !error
                call fortaxerror('error reading record after ' // inttostr(nrec))
            else
                nrec = nrec + 1
                if (nrec > maxSysIndex) then
                    call fortaxerror('nrec > maxSysIndex')
                end if
                date0(nrec) = tempdate0
                date1(nrec) = tempdate1
                fname(:, nrec) = transfer(tempfname, fname(:, nrec))
            end if

        end do

        close(funit)

        if (nrec .ne. ndate) then
            call fortaxerror('number of records does not equal number declared on line 1')
        end if

    end subroutine loadsysindex2


    ! getsysindex
    ! -----------------------------------------------------------------------
    ! returns information which allows the user to easily identify which
    ! tax system operated at any given YYYYMMDD date as specified in sysindex

    subroutine getsysindex(sysindex, date, sysfilepath, sysnum)

        use fortax_type, only : sysindex_t, len_sysindex

        implicit none

        type(sysindex_t), intent(in)  :: sysindex
        integer, intent(in) :: date
        character(len = len_sysindex), intent(out) :: sysfilepath
        integer, intent(out) :: sysnum

        call getsysindex2(sysindex%nsys, sysindex%date0, sysindex%date1, sysindex%fname, date, sysfilepath, sysnum)

    end subroutine getsysindex


    ! getsysindex2
    ! -----------------------------------------------------------------------
    ! returns information which allows the user to easily identify which
    ! tax system operated at any given YYYYMMDD date as specified in sysindex

    subroutine getsysindex2(nsys, date0, date1, fname, date, sysfilepath, sysnum)

        use fortax_util, only : lower, fortaxerror, checkdate
        use fortax_type, only : len_sysindex, maxSysIndex
        use iso_c_binding

        implicit none

        integer, intent(in) :: nsys
        integer, intent(in) :: date0(maxSysIndex), date1(maxSysIndex)
        character(kind = c_char), intent(in) :: fname(len_sysindex, maxSysIndex)
        integer, intent(in) :: date
        character(len = len_sysindex), intent(out) :: sysfilepath
        integer, intent(out) :: sysnum

        integer :: i
        character(len = len_sysindex):: sysname

        if (nsys == 0) then
            call fortaxerror('system index file is not in memory')
        end if

        if (checkdate(date)) then
            sysnum = 0
            do i = 1, nsys
                if (date >= date0(i) .and. date <= date1(i)) then
                    sysname = transfer(fname(:, i), sysname)
                    sysfilepath = 'systems/fortax/' // trim(adjustl(sysname)) // '.json'
                    sysnum = i
                    exit
                end if
            end do
            if (sysnum == 0) then
                call fortaxerror('getsysindex date not contained in sysindex')
            end if
        else
            call fortaxerror('invalid date in getsysindex')
        end if

    end subroutine getsysindex2

    ! freesysindex
    ! -----------------------------------------------------------------------
    ! deallocates the data structures which store price information and set
    ! initialization variables to .false.

    subroutine freesysindex(sysindex)

        use fortax_type, only : sysindex_t

        implicit none

        type(sysindex_t), intent(inout) :: sysindex

        sysindex%nsys = 0
        sysindex%date0 = 0
        sysindex%date1 = 0
        sysindex%fname = ''

    end subroutine freesysindex

    subroutine rpi_saveF90(rpi,fname)

        use fortax_util, only : fortaxError, intToStr, dblToStr
        use fortax_type, only : rpi_t

        use, intrinsic :: iso_fortran_env

        implicit none

        type(rpi_t), intent(in) :: rpi
        character(len = *), intent(in), optional :: fname

        integer :: funit, ios, i

        if ( (rpi%ndate <= 0) ) then
            call fortaxError('no price data for writing')
        end if

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit, '(a)') '! .f90 FORTAX Price index; generated using rpi_saveF90'
        write(funit, *)

        write(funit, '(a)') 'integer, parameter :: nindex_f90 =' // intToStr(rpi%ndate)
        write(funit, *)
        write(funit, '(a)') 'integer, parameter :: rpidate_f90(nindex_f90) = (/ &'
        do i = 1, rpi%ndate - 1
            write(funit, '(a)') '    ' // intToStr(rpi%date(i)) // ', &'
        end do
        write(funit, '(a)') '    ' // intToStr(rpi%date(rpi%ndate)) // '/)'
        write(funit, *)
        write(funit, '(a)') 'real(dp), parameter :: rpiindex_f90(nindex_f90) = (/ &'
        do i = 1, rpi%ndate - 1
            write(funit, '(a)') '    ' // dblToStr(rpi%index(i)) // '_dp, &'
        end do
        write(funit, '(a)') '    ' // dblToStr(rpi%index(rpi%ndate)) // '_dp/)'
        write(funit, *)
        write(funit, '(a)') '! .f90 FORTAX Price index; END-OF-FILE'
        write(funit, *)

        if (present(fname)) close(funit)

    end subroutine rpi_saveF90

end module fortax_prices
