
! This file is part of the FORTAX library;
! (c) 2009 Andrew Shephard; andrubuntu@gmail.com

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

module fortax_prices

    use fortax_realtype, only : dp
    
    private
    
    integer,  allocatable :: rpidate(:)
    real(dp), allocatable :: rpiindex(:)
    
    type sysindex_t
        logical                     :: indexinit = .false.
        integer,        allocatable :: date0(:), date1(:)
        character(255), allocatable :: fname(:)
    end type
    
    public :: loadindex, setindex, getindex, upratefactor, upratesys
    public :: sysindex_t, checkdate, loadsysindex, getsysindex
    
contains

    ! setindex
    ! -----------------------------------------------------------------------
    ! sets the price index data in sysindex using the specified date and
    ! index information

    subroutine setindex(mydate,myindex,mysize)

        implicit none

        real(dp), intent(in) :: myindex(mysize)
        integer,  intent(in) :: mydate(mysize)
        integer,  intent(in) :: mysize

        if (allocated(rpidate)) deallocate(rpidate)
        if (allocated(rpiindex)) deallocate(rpiindex)
        allocate(rpidate(mysize),rpiindex(mysize))

        rpidate  = mydate
        rpiindex = myindex

    end subroutine setindex


    ! loadindex
    ! -----------------------------------------------------------------------
    ! loads a price index file saved as a comma separated values (CSV) file. 
    ! If fname is not specified it defaults to 'prices/rpi.csv'

    subroutine loadindex(fname)
    
        use fortax_util, only : getunit, fortaxerror, inttostr
        
        implicit none
       
        character(*), intent(in), optional :: fname
        
        integer                            :: funit
        integer                            :: istat, nrec
        
        logical                            :: isfile
        integer                            :: tempdate, ndate
        real(dp)                           :: tempindex

        call getunit(funit)

        if (present(fname)) then
            inquire(file=fname, exist=isfile)
            if (isfile) then
                open(funit, file=fname, status='old')
            else
                call fortaxerror('price index file does not exist ('//trim(adjustl(fname))//')')
            end if
        else
            inquire(file='prices/rpi.csv', exist=isfile)
            if (isfile) then
                open (funit, file='prices/rpi.csv', status='old')
            else
                call fortaxerror('default price index file does not exist')
            end if
        end if
        
        read (funit, *, iostat = istat) ndate

        if (allocated(rpidate))  deallocate(rpidate)
        if (allocated(rpiindex)) deallocate(rpiindex)
        
        if (istat .ne. 0) then
            call fortaxerror('error reading number of records on line 1')
        else
            allocate(rpidate(ndate))
            allocate(rpiindex(ndate))
        end if
            
        nrec = 0
                
        do
            
            read(funit, *, iostat = istat) tempdate,tempindex
            
            if (istat==-1) then !eof
                exit
            elseif (istat>0) then !error
                call fortaxerror('error reading record after '//inttostr(nrec))
            else
                nrec           = nrec + 1
                rpidate(nrec)  = tempdate
                rpiindex(nrec) = tempindex                
            end if
            
        end do
        
        close(funit)
        
        if (nrec.ne.ndate) then
            call fortaxerror('number of records does not equal number declared on line 1')
        end if
        
    end subroutine loadindex

    
    ! getindex
    ! -----------------------------------------------------------------------
    ! returns the price index associated with the supplied YYYYMMDD date

    real(dp) elemental function getindex(date)
            
        implicit none
        
        integer, intent(in) :: date
        integer             :: year,  month
        integer             :: year1, month1
           
        !exploits structure of data
        if (date<rpidate(1)) then
            getindex = 0.0_dp
        else
            year     = date/10000
            month    = (date - year*10000)/100
            year1    = rpidate(1)/10000
            month1   = (rpidate(1) - year1*10000)/100
            getindex = rpiindex((year-year1)*12 + month)
        end if
        
    end function getindex
    

    ! upratefactor
    ! -----------------------------------------------------------------------
    ! returns uprating factor from date0 to date1 prices (both in YYYYMMDD 
    ! format). it calls the function getindex.

    real(dp) elemental function upratefactor(date0,date1)
        
        implicit none
        
        integer, intent(in) :: date0, date1
        
        upratefactor = getindex(date1)/getindex(date0)
            
    end function upratefactor
    

    ! upratesys
    ! -----------------------------------------------------------------------
    ! uprates the tax system sys using the specified uprating factor. If 
    ! newdate is present, it will set the prices attribute to this value.

    subroutine upratesys(sys,factor,newdate)
    
        use fortax_type, only : sys_t
        use fortax_util, only : fortaxwarn

        implicit none
    
        type(sys_t), intent(inout)        :: sys
        real(dp),    intent(in)           :: factor
        integer,     intent(in), optional :: newdate    
        
        logical, parameter :: null      = .false.          
        logical, parameter :: range     = .false.
        logical, parameter :: scale     = .false.          
        logical, parameter :: rate      = .false.          
        logical, parameter :: amount    = .true.
        logical, parameter :: minamount = .true.
        
        if (present(newdate)) sys%extra%prices = newdate

        !use preprocessor commands here. we define the "type" in the
        !original include files so it is very easy to apply uprating
        !and can easily be extended to have rpi/rossi uprating, etc.
        !by using a suitably defined parameter. AS 30/10/08
#       include 'includes/fortax_uprate.inc'

    end subroutine upratesys


    ! checkDate
    ! -----------------------------------------------------------------------
    ! returns true or false depending on whether date YYYYMMDD is valid

    logical pure function checkDate(date)

        implicit none

        integer, intent(in) :: date
        integer             :: year, month, day, maxday
        
        year  = date/10000
        month = (date - year*10000)/100
        day   = date - (date/100)*100

        select case (month)
            case (1,3,5,7,8,10,12)
                maxday = 31
            case (4,6,9,11)
                maxday = 30
            case (2)
                if (((modulo(year,4) == 0) .and. (modulo(year,100) .ne. 0)) .or. (modulo(year,400) == 0)) then
                    maxday = 29
                else
                    maxday = 28
                end if
            case default
                maxday = 0
        end select

        if ((year >= 0) .and. (month >= 1 .and. month <= 12) .and. (day >= 1 .and. day <= maxday)) then
            checkDate = .true.
        else
            checkDate = .false.
        end if

    end function checkDate


    ! loadsysindex
    ! -----------------------------------------------------------------------
    ! provides quick access to the actual system that individuals faced
    ! requires an external system index file (sysindexfile)

    subroutine loadsysindex(sysindex,sysindexfile)
            
        use fortax_util, only : getunit, fortaxerror, inttostr

        implicit none
    
        type(sysindex_t), intent(out)          :: sysindex
        character(*),     intent(in), optional :: sysindexfile
        
        integer                                :: funit
        integer                                :: istat, nrec
        
        logical                                :: isfile
        integer                                :: tempdate0, tempdate1, ndate
        character(255)                         :: tempfname
                    
        call getunit(funit)

        if (present(sysindexfile)) then
            inquire(file=sysindexfile, exist=isfile)
            if (isfile) then
                open (funit, file=sysindexfile, status='old')
            else
                call fortaxerror('system index file does not exist ('//sysindexfile//')')
            end if
        else
            inquire(file='systems/sysindex.csv', exist=isfile)
            if (isfile) then
                open (funit,file='systems/sysindex.csv', status='old')
            else
                call fortaxerror('default system index file does not exist')
            end if
        end if
        
        read (funit, *, iostat = istat) ndate

        call freesysindex(sysindex)
        
        if (istat .ne. 0) then
            call fortaxerror('error reading number of records on line 1')
        else
            allocate(sysindex%date0(ndate))
            allocate(sysindex%date1(ndate))
            allocate(sysindex%fname(ndate))
        end if
            
        nrec = 0
                
        do
            
            read(funit, *, iostat = istat) tempdate0,tempdate1,tempfname
            
            if (istat==-1) then !eof
                exit
            elseif (istat>0) then !error
                call fortaxerror('error reading record after '//inttostr(nrec))
            else
                nrec                 = nrec + 1
                sysindex%date0(nrec) = tempdate0
                sysindex%date1(nrec) = tempdate1
                sysindex%fname(nrec) = tempfname
            end if
            
        end do
        
        close(funit)
        
        if (nrec.ne.ndate) then
            call fortaxerror('number of records does not equal number declared on line 1')
        end if   
        
        sysindex%indexinit = .true.
        
    end subroutine loadsysindex
        

    ! getsysindex
    ! -----------------------------------------------------------------------
    ! returns information which allows the user to easily identify which 
    ! tax system operated at any given YYYYMMDD date as specified in sysindex

    subroutine getsysindex(sysindex,date,systemformat,sysfilepath,sysnum)
    
        use fortax_util, only : lower, fortaxerror
        
        implicit none
                
        type(sysindex_t), intent(in)  :: sysindex
        integer,          intent(in)  :: date
        character(*),     intent(in)  :: systemformat
        character(255),   intent(out) :: sysfilepath
        integer,          intent(out) :: sysnum        
        
        integer      :: i
        character(4) :: fext !extension
        character(7) :: fsub !subdirectory
        
        if (.not. sysindex%indexinit) then
            call fortaxerror('system index file is not in memory')
        end if
        
        select case(lower(systemformat))
            case('taxben')
                fsub = 'taxben/'
                fext = '.bp3'
            case('fortax')
                fsub = 'fortax/'
                fext = '.xml'
            case default
                call fortaxerror('Unknown system format specified ('//systemformat//') in getsysindex')
        end select
        
        if (checkdate(date)) then
            sysnum = 0
            do i = 1, size(sysindex%date0,1)
                if (date>=sysindex%date0(i) .and. date<=sysindex%date1(i)) then
                    sysfilepath = adjustl('systems/'//trim(fsub)//trim(sysindex%fname(i))//trim(fext))
                    sysnum      = i
                    exit
                end if
            end do
            if (sysnum==0) then
                call fortaxerror('getsysindex date not contained in sysindex')
            end if
        else
            call fortaxerror('invalid date in getsysindex')
        end if

    end subroutine getsysindex

       
    ! freesysindex
    ! -----------------------------------------------------------------------
    ! deallocates the data structures which store price information and set
    ! initialization variables to .false.

    subroutine freesysindex(sysindex)
        
        implicit none
        
        type(sysindex_t), intent(inout) :: sysindex
        
        sysindex%indexinit = .false.

        if (allocated(sysindex%date0)) deallocate(sysindex%date0)
        if (allocated(sysindex%date1)) deallocate(sysindex%date1)
        if (allocated(sysindex%fname)) deallocate(sysindex%fname)
        
    end subroutine freesysindex
                        
end module fortax_prices
