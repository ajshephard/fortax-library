
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




! fortax_util
! -----------------------------------------------------------------------
! module provides a number of support utilities are available as well as 
! generic error handling routines, AS

module fortax_util

    use fortax_realtype, only : dp

    public 
    private :: intToStrLen
    
contains
    
    ! intToStr
    ! -----------------------------------------------------------------------
    ! converts integer to variable length string by calling intToStrLen to
    ! determine return length

    pure function intToStr(N)
        
        implicit none
                 
        integer, intent(in)       :: N
        character(intToStrLen(N)) :: intToStr
        character(20)             :: tempStr20
        
        write (tempStr20,'(I20)') N
        intToStr = adjustl(tempStr20)

    end function intToStr


    ! intToStrLen
    ! -----------------------------------------------------------------------
    ! returns length of integer N as a string

    pure function intToStrLen(N)
        
        implicit none

        integer, intent(in) :: N
        integer             :: intToStrLen
        character(20)       :: tempStr20
        
        write (tempStr20,'(I20)') N
        intToStrLen = len_trim(adjustl(tempStr20))
        
    end function intToStrLen

    ! logicalToStr
    ! -----------------------------------------------------------------------
    ! converts logical to string (.true. = T, .false. = F)

    pure function logicalToStr(N)
        
        implicit none
                 
        logical, intent(in) :: N
        character(1)        :: logicalToStr
        
        logicalToStr = merge('T','F',N)

    end function logicalToStr


    ! dblToStr
    ! -----------------------------------------------------------------------
    ! converts double to variable length string by calling intToStrLen to
    ! determine return length

    pure function dblToStr(N)
        
        implicit none
                 
        real(dp), intent(in)      :: N
        character(dblToStrLen(N)) :: dblToStr
        character(40)             :: tempStr40
        
        write (tempStr40,*) N
        dblToStr = adjustl(tempStr40)

    end function dblToStr


    ! dblToStrLen
    ! -----------------------------------------------------------------------
    ! returns length of double N as a string

    pure function dblToStrLen(N)
        
        implicit none

        real(dp), intent(in) :: N
        integer              :: dblToStrLen
        character(40)        :: tempStr40
        
        write (tempStr40,*) N
        dblToStrLen = len_trim(adjustl(tempStr40))
        
    end function dblToStrLen


    ! strToDouble
    ! -----------------------------------------------------------------------
    ! converts a string to a double. No error checking is performed.

    pure function strToDouble(string)
            
        implicit none
        
        character(*), intent(in) :: string
        real(dp)                 :: strToDouble
        
        read (string,*) strToDouble
                
    end function strToDouble


    ! strToInt
    ! -----------------------------------------------------------------------
    ! converts a string to an integer. No error checking is performed.

    pure function strToInt(string)

        implicit none
        
        character(*), intent(in) :: string
        integer                  :: strToInt
        
        read (string,*) strToInt
    
    end function strToInt


    ! strToLogical
    ! -----------------------------------------------------------------------
    ! converts a string to a logical. No error checking is performed.
    ! zero is false, everything else is true.

    pure function strToLogical(str)

        implicit none
        
        character(*), intent(in) :: str
        logical                  :: strToLogical
        
        if (adjustl(str) .eq. "0") then
            strToLogical = .false.
        else
            strToLogical = .true.
        end if
       
    end function strToLogical


    ! lower
    ! -----------------------------------------------------------------------
    ! converts a string to a lowercase. Useful for string comparisons.
    
    pure function lower(str)

        implicit none
        
        character(*), intent(in) :: str
        character(len(str))      :: lower
        
        character                :: ch
        integer, parameter       :: offset = ichar('A') - ichar('a')
        integer                  :: i

        do i = 1, len(str)
            ch = str(i:i)
            if (ch >= 'A' .and. ch <= 'Z') ch = char(ichar(ch)-offset)
            lower(i:i) = ch
        end do
        
    end function lower


    ! upper
    ! -----------------------------------------------------------------------
    ! converts a string to a uppercase. Useful for string comparisons.    

    pure function upper(str)

        implicit none
        
        character(*), intent(in) :: str
        character(len(str))      :: upper
        
        character                :: ch
        integer, parameter       :: offset = ichar('a') - ichar('A')
        integer                  :: i

        do i = 1, len(str)
            ch = str(i:i)
            if (ch >= 'a' .and. ch <= 'z') ch = char(ichar(ch)-offset)
            upper(i:i) = ch
        end do
        
    end function upper


    ! compact
    ! -----------------------------------------------------------------------
    ! converts multiple spaces and tabs into single spaces. Also removes any
    ! initial whitespace, and deletes control characters
    
    pure subroutine compact(str)

        implicit none

        character(*), intent(inout) :: str
        character(1)                :: ch
        character(len_trim(str))    :: outstr

        integer                     :: i, k, isp, ich
        integer                     :: lenstr

        str    = adjustl(str)
        lenstr = len_trim(str)
        outstr = ' '
        isp    = 0
        k      = 0

        do i = 1,lenstr
            ch=str(i:i)
            ich=iachar(ch)
          
            select case(ich)
            case(9,32)     ! space or tab character
                if(isp==0) then
                    k=k+1
                    outstr(k:k)=' '
                end if
                isp=1
            case(33:)      ! not a space, quote, or control character
                k=k+1
                outstr(k:k)=ch
                isp=0
            end select
                
        end do

        str=adjustl(outstr)

    end subroutine compact

    
    ! compact
    ! -----------------------------------------------------------------------
    ! trims zeros from numeric string. no error checking is performed.

    pure subroutine trimZero(str)

        implicit none

        character(*), intent(inout) :: str
        integer, parameter          :: eeLen = 32
        character(eeLen)            :: ee, eestr
        character                   :: ch
        integer                     :: i, lstr, ipos, eex

        str = adjustl(trim(str))
        ipos=scan(str,'eE')

        if (ipos>0) then
            ee=str(ipos:)
!            read (ee(3:),*) eex
            read (ee(3:eeLen),*) eex
            if (eex==0) then
            ee=''
            else
            write (eestr,'(I20)') eex
            ee(3:eeLen) = adjustl(eestr)
!            ee(3:) = adjustl(eestr)
            end if
            str=str(1:ipos-1)
        end if

        lstr=len_trim(str)

        do i=lstr,1,-1
            ch=str(i:i)
            if (ch=='0') cycle
            if (ch=='.') then
            str=str(1:i)//'0'
            if (ipos>0) str=adjustl(trim(str)//trim(ee))
            return
            end if
            str=str(1:i)
            exit
        end do

        if (ipos>0) str=adjustl(trim(str)//trim(ee))

    end subroutine trimZero


    ! strPad
    ! -----------------------------------------------------------------------
    ! pads a string with a character n

    pure function strPad(str,n)

        implicit none
    
        character(*), intent(in) :: str
        integer,      intent(in) :: n
        character(n)             :: strPad

        strPad = str
    
    end function strPad


    ! getUnit
    ! -----------------------------------------------------------------------
    ! returns a free file unit

    subroutine getUnit(funit)

        use, intrinsic :: iso_fortran_env
        
        implicit none

        integer, intent(out) :: funit
        integer              :: i
        logical              :: opend

        integer, parameter   :: stdout = output_unit
        integer, parameter   :: maxunit = 99

        do i = stdout+1, maxunit
            inquire(unit=i, opened=opend)
            if (.not. opend) then
                funit = i
                exit
            endif
        end do

        return

    end subroutine getUnit


    ! strToIntArray
    ! -----------------------------------------------------------------------
    ! convert a string to integer array (need to get rid of error for pure)

    subroutine strToIntArray(x,str)

        implicit none
        
        character(*), intent(in)    :: str
        integer,      intent(inout) :: x(:)
        
        !character(len(string))   :: tmpstring

        integer      :: n, i, i0, i1
        integer      :: strLen, thisLen
        character(8) :: strFor

        n = size(x)
        strLen = len(str)

        i0 = 1
        
        do i = 1, n

            i1 = index(str(i0:),',') + i0-1
            !print *,i,i0,i1,string(i0:)
            if (i0==i1) then
                i0=i1+1
                cycle
            else if (i1<i0) then
                if (str(i0:)==' ') exit
                thisLen = len(str(i0:))
                strFor  = '(I'//intToStr(thisLen)//')'
                read (str(i0:),strFor,err=999) x(i)
                exit
            else
                if (str(i0:i1-1)==' ') then
                    i0 = i1 +1
                    cycle
                else
                    thisLen = len(str(i0:i1-1))
                    strFor  = '(I'//intToStr(thisLen)//')'
                    read (str(i0:i1-1),strFor,err=999) x(i)
                    i0 = i1+1
                    if (i0>strLen) exit
                end if
            end if

        end do

        return

999     print *, 'error: not a valid integer'
        stop

    end subroutine strToIntArray


    ! strToDoubleArray
    ! -----------------------------------------------------------------------
    ! convert a string to double array (need to get rid of error for pure)

    subroutine strToDoubleArray(x,str)

        implicit none
        
        character(*), intent(in)    :: str
        real(dp),     intent(inout) :: x(:)
        
        !character(len(string))      :: tmpstring

        integer      :: n, i, i0, i1
        integer      :: strLen, thisLen
        character(8) :: strFor

        n      = size(x)
        strlen = len(str)

        i0 = 1
        
        do i = 1, n

            i1 = index(str(i0:),',') + i0-1
            !print *,i,i0,i1,string(i0:)
            if (i0==i1) then
                i0=i1+1
                cycle
            else if (i1<i0) then
                if (str(i0:)==' ') exit
                thisLen = len(str(i0:))
                strFor = '(G'//intToStr(thisLen)//'.0)'
                read (str(i0:),strFor,err=999) x(i)
                exit
            else
                if (str(i0:i1-1)==' ') then
                    i0 = i1 +1
                    cycle
                else
                    thisLen = len(str(i0:i1-1))
                    strFor = '(G'//intToStr(thisLen)//'.0)'
                    read (str(i0:i1-1),strFor,err=999) x(i)
                    i0 = i1+1
                    if (i0>strLen) exit
                end if
            end if

        end do

        return

999     print *, 'error: not a valid real type'
        stop

    end subroutine strToDoubleArray


    ! fortaxError
    ! -----------------------------------------------------------------------
    ! halts program on an error. Should allow for soft failures.

    subroutine fortaxError(errMsg,funit)

        implicit none

        character(*),      intent(in) :: errMsg
        integer, optional, intent(in) :: funit
        
        if (present(funit)) then
            write (funit,*) 'Fortax error: ',errMsg
            stop 'program terminated by fortaxError'
        else
            write (*,*) 'Fortax error: ',errMsg
            stop 'program terminated by fortaxError'
        end if

        return

    end subroutine fortaxError


    ! fortaxWarn
    ! -----------------------------------------------------------------------
    ! displays a warning message.

    subroutine fortaxWarn(warnMsg,funit)
    
        implicit none

        character(*),      intent(in) :: warnMsg
        integer, optional, intent(in) :: funit
        
        if (present(funit)) then
            write (funit,*) 'Fortax warning: ', warnMsg
        else
            write (*,*) 'Fortax warning: ', warnMsg
        end if

        return

    end subroutine fortaxWarn

end module fortax_util
