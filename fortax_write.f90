
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
! module for saving (to native XML format) and printing of system files
! code will reflect any changes to the structure of the system, AS

module fortax_write

    use fortax_realtype, only : dp

    private
    public  :: fortaxPrint, fortaxWrite

    interface ftxmlwrite
        module procedure xml_write_fchar
        module procedure xml_write_finteger
        module procedure xml_write_fdouble
        module procedure xml_write_flogical
        module procedure xml_write_fintegerarray
        module procedure xml_write_fdoublearray
        module procedure xml_write_flogicalarray
    end interface

    interface ftprint
        module procedure ftprint_finteger
        module procedure ftprint_fdouble
        module procedure ftprint_flogical
        module procedure ftprint_fintegerarray
        module procedure ftprint_fdoublearray
        module procedure ftprint_flogicalarray
    end interface

contains
    
    ! fortaxPrint
    ! -----------------------------------------------------------------------
    ! outputs a summary of the tax system to the default output unit if fname
    ! is not specified. Otherwise, this output summary will be written to 
    ! disk with file name fname. This printing code is self-maintaining

    subroutine fortaxPrint(sys,fname)

        use fortax_type, only : sys_t
        use fortax_util, only : upper, getUnit, fortaxError
        use, intrinsic :: iso_fortran_env

        implicit none

        type(sys_t),  intent(in)           :: sys
        character(*), intent(in), optional :: fname

        integer            :: funit, ios
        
        if (present(fname)) then
            call getUnit(funit)
            open(funit,file=fname,action='write',status='replace',iostat=ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

#       include "includes/fortax_print.inc"

        if (present(fname)) close(funit)

        return

    end subroutine fortaxPrint


    ! fortaxWrite
    ! -----------------------------------------------------------------------
    ! writes the system file sys to disk with file name fname in the native 
    ! FORTAX file format. This writing code is self-maintaining.

    subroutine fortaxWrite(sys,fname)
    
        use fortax_type, only : sys_t
        use xmlparse,    only : xml_parse, xml_open, xml_close
            
        implicit none
        
        type(sys_t),  intent(in) :: sys
        character(*), intent(in) :: fname
                
        character(255)           :: attribs(2,1)
        logical                  :: mustRead
        type(xml_parse)          :: info

        mustRead = .false.

        attribs(1,1) = 'basename'
        
        call xml_open(info,fname,mustRead)
        call xml_ftag(info,'fortax','open')

#       include "includes/fortax_write.inc"

        call xml_ftag(info,'fortax','close')
        call xml_close(info)

        return

    end subroutine fortaxWrite
            


    ! xml_write_fchar
    ! -----------------------------------------------------------------------
    ! xml writing of string when saving system file (called by fortaxWrite)

    subroutine xml_write_fchar(info,field,fieldname)
    
        use xmlparse, only : xml_parse, xml_put
        
        implicit none
        
        type(xml_parse), intent(inout) :: info
        character(*),    intent(in)    :: field
        character(*),    intent(in)    :: fieldName
        
        character(255) :: myData(1)
        character(255) :: attribs(2,2)
        
        attribs(1,1) = 'name'
        attribs(2,1) = fieldName
        attribs(1,2) = 'value'
        write(attribs(2,2),*) field
               
        call xml_put(info, 'fchar',adjustl(attribs), 2, &
            myData, 0, 'elem')
            
        return

    end subroutine xml_write_fchar

    ! xml_write_finteger
    ! -----------------------------------------------------------------------
    ! xml writing of integer when saving system file (called by fortaxWrite)

    subroutine xml_write_finteger(info,field,fieldname)
    
        use xmlparse, only : xml_parse, xml_put
        
        implicit none
        
        type(xml_parse), intent(inout) :: info
        integer,         intent(in)    :: field
        character(*),    intent(in)    :: fieldName
        
        character(255) :: myData(1)
        character(255) :: attribs(2,2)
        
        attribs(1,1) = 'name'
        attribs(2,1) = fieldName
        attribs(1,2) = 'value'
        write(attribs(2,2),*) field
               
        call xml_put(info, 'finteger',adjustl(attribs), 2, &
            myData, 0, 'elem')
            
        return

    end subroutine xml_write_finteger


    ! xml_write_fdouble
    ! -----------------------------------------------------------------------
    ! xml writing of double when saving system file (called by fortaxWrite)
            
    subroutine xml_write_fdouble(info,field,fieldname)
    
        use xmlparse,    only : xml_parse, xml_put
        use fortax_util, only : compact
        
        implicit none
        
        type(xml_parse), intent(inout) :: info
        real(dp),        intent(in)    :: field
        character(*),    intent(in)    :: fieldName
        
        character(255)                 :: myData(1)
        character(255)                 :: attribs(2,2)
                                
        attribs(1,1) = 'name'
        attribs(2,1) = fieldName
        attribs(1,2) = 'value'
        
        write(attribs(2,2),*) field
                
        call xml_put(info, 'fdouble',adjustl(attribs), 2, &
            & mydata, 0, 'elem')

        return

    end subroutine xml_write_fdouble


    ! xml_write_flogical
    ! -----------------------------------------------------------------------
    ! xml writing of logical when saving system file (called by fortaxWrite)
        
    subroutine xml_write_flogical(info,field,fieldname)
    
        use xmlparse, only : xml_parse, xml_put
        
        implicit none
        
        type(xml_parse), intent(inout) :: info
        logical,         intent(in)    :: field
        character(*),    intent(in)    :: fieldName
        character(255)                 :: myData(1)
        character(255)                 :: attribs(2,2)
        
        attribs(1,1) = 'name'
        attribs(2,1) = fieldName
        attribs(1,2) = 'value'
        write(attribs(2,2),*) merge('T', 'F', field)
        
        call xml_put(info, 'flogical',adjustl(attribs), 2, &
            & mydata, 0, 'elem')

        return

    end subroutine xml_write_flogical


    ! xml_write_fintegerarray
    ! -----------------------------------------------------------------------
    ! xml writing of integer array when saving system file (called by 
    ! fortaxWrite)

    subroutine xml_write_fintegerarray(info,field,fieldname)
    
        use xmlparse,    only : xml_parse, xml_put
        use fortax_util, only : compact
        
        implicit none
        
        type(xml_parse), intent(inout) :: info
        integer,         intent(in)    :: field(:)
        character(*),    intent(in)    :: fieldName
        
        character(1)                   :: myData(1)
        character(255)                 :: attribs(2,2)
        character(64)                  :: tempStr(size(field))
        character(64*size(field)+1)    :: tempStr2
        
        integer                        :: i

        attribs(1,1) = 'name'
        attribs(2,1) = fieldName
        attribs(1,2) = 'value'
                
        do i = 1, size(field)
            write (tempStr(i),*) field(i)
        end do
            
        write(tempStr2,*) (tempStr(i), i=1,size(field))
        call compact(tempStr2)
        attribs(2,2) = adjustl(trim(tempStr2))

        if (len_trim(adjustl(trim(tempStr2)))>len(attribs)) then
            print *, 'warning: field truncated when writing integer array'
        end if
                
        call xml_put(info, 'fintegerarray',adjustl(attribs), 2, &
            & mydata, 0, 'openclose')       

        return

    end subroutine xml_write_fintegerarray
    

    ! xml_write_fdoublearray
    ! -----------------------------------------------------------------------
    ! xml writing of double array when saving system file (called by 
    ! fortaxWrite)

    subroutine xml_write_fdoublearray(info,field,fieldName)
    
        use xmlparse,    only : xml_parse, xml_put
        use fortax_util, only : compact
        
        implicit none
        
        type(xml_parse), intent(inout) :: info
        real(dp),        intent(in)    :: field(:)
        character(*),    intent(in)    :: fieldName
        
        character(1)                   :: mydata(1)
        character(255)                 :: attribs(2,2)
        character(64)                  :: tempStr(size(field))
        character(64*size(field)+1)    :: tempStr2
        
        integer                        :: i

        attribs(1,1) = 'name'
        attribs(2,1) = fieldname
        attribs(1,2) = 'value'
                
        do i = 1, size(field)
            write (tempstr(i),*) field(i)
        end do 
            
        write(tempStr2,*) (tempStr(i), i=1,size(field))
        call compact(tempStr2)
        attribs(2,2) = adjustl(trim(tempStr2))

        if (len_trim(adjustl(trim(tempStr2)))>len(attribs)) then
            print *, 'warning: field truncated when writing logical array'
        end if
                
        call xml_put(info, 'fdoublearray',adjustl(attribs), 2, &
            & mydata, 0, 'openclose')       

        return

    end subroutine xml_write_fdoublearray


    ! xml_write_fdoublearray
    ! -----------------------------------------------------------------------
    ! xml writing of double array when saving system file (called by 
    ! fortaxWrite)
    
    subroutine xml_write_flogicalarray(info,field,fieldName)
    
        use xmlparse,    only : xml_parse, xml_put
        use fortax_util, only : compact
        
        implicit none
        
        type(xml_parse), intent(inout) :: info
        logical,         intent(in)    :: field(:)
        character(*),    intent(in)    :: fieldName
        
        character(1)                   :: myData(1)
        character(255)                 :: attribs(2,2)
        character(1)                   :: tempStr(size(field))
        character(1*size(field)+1)     :: tempStr2
        
        integer                        :: i

        attribs(1,1) = 'name'
        attribs(2,1) = fieldname
        attribs(1,2) = 'value'
                
        do i = 1, size(field)
            write (tempStr(i),*) merge('T', 'F', field(i))
        end do 
                    
        write(tempStr2,*) (tempStr(i), i=1,size(field))
        call compact(tempStr2)
        attribs(2,2) = adjustl(trim(tempStr2))

        if (len_trim(adjustl(trim(tempStr2)))>len(attribs)) then
            print *, 'warning: field truncated when writing logical array'
        end if
                
        call xml_put(info, 'flogicalarray',adjustl(attribs), 2, &
            & mydata, 0, 'openclose')

        return

    end subroutine xml_write_flogicalarray


    ! xml_write_fintegerarray
    ! -----------------------------------------------------------------------
    ! xml writing of tags when saving system file (called by fortaxWrite)
    
    subroutine xml_ftag(info,tag,openClose,attribs)
    
        use xmlparse, only : xml_parse, xml_put
        
        implicit none
        
        type(xml_parse),        intent(inout) :: info
        character(*),           intent(in)    :: tag
        character(*),           intent(in)    :: openClose
        character(*), optional, intent(in)    :: attribs(:,:)
        
        character(255)                        :: myData(1)

        if (present(attribs)) then
            call xml_put(info, tag, attribs, size(attribs,2), myData, 0, openClose)
        else
            call xml_put(info, tag, reshape((/'',''/),(/2,1/)), 0, myData, 0, openClose)
        end if

        return
        
    end subroutine xml_ftag


    ! ftprint_finteger
    ! -----------------------------------------------------------------------
    ! fortax printing of integer when printing system file (called by 
    ! fortaxPrint)
    
    subroutine ftprint_finteger(funit,finteger,fname)

        use fortax_util, only: intToStr, strPad

        implicit none

        integer,      intent(in) :: funit
        integer,      intent(in) :: finteger
        character(*), intent(in) :: fname

        write(funit,*) strPad(fname,16)//intToStr(finteger)

        return

    end subroutine ftprint_finteger


    ! ftprint_fdouble
    ! -----------------------------------------------------------------------
    ! fortax printing of double when printing system file (called by 
    ! fortaxPrint)

    subroutine ftprint_fdouble(funit,fdouble,fname)

        use fortax_util, only : trimZero, strPad

        implicit none

        integer,      intent(in) :: funit
        real(dp),     intent(in) :: fdouble
        character(*), intent(in) :: fname
        
        character(32) :: str
            
        write(str,*) fdouble
        call trimZero(str)

        write(funit,*) strPad(fname,16)//str

        return

    end subroutine ftprint_fdouble


    ! ftprint_flogical
    ! -----------------------------------------------------------------------
    ! fortax printing of logical when printing system file (called by 
    ! fortaxPrint)

    subroutine ftprint_flogical(funit,flogical,fname)

        use fortax_util, only : strPad

        implicit none

        integer,      intent(in) :: funit
        logical,      intent(in) :: flogical
        character(*), intent(in) :: fname

        write(funit,*) strpad(fname,16)//merge('yes','no ',flogical)

        return

    end subroutine ftprint_flogical


    ! ftprint_fintegerarray
    ! -----------------------------------------------------------------------
    ! fortax printing of integer array when printing system file (called by 
    ! fortaxPrint)

    subroutine ftprint_fintegerarray(funit,fintegerarray,fname)

        use fortax_util, only : intToStr, strPad

        implicit none

        integer,      intent(in) :: funit
        integer,      intent(in) :: fintegerarray(:)
        character(*), intent(in) :: fname

        integer :: i
        
        do i = 1, size(fintegerarray)
            write(funit,*) strPad(fname//'('//IntToStr(i)//')',16)//inttostr(fintegerarray(i))
        end do

        return

    end subroutine ftprint_fintegerarray


    ! ftprint_fdoublearray
    ! -----------------------------------------------------------------------
    ! fortax printing of double array when printing system file (called by 
    ! fortaxPrint)

    subroutine ftprint_fdoublearray(funit,fdoublearray,fname)

        use fortax_util, only : intToStr, strPad, trimZero

        implicit none

        integer,      intent(in) :: funit
        real(dp),     intent(in) :: fdoublearray(:)
        character(*), intent(in) :: fname

        integer       :: i
        character(32) :: str
            
        do i = 1, size(fdoublearray)
            write(str,*) fdoublearray(i)
            call trimZero(str)
            write(funit,*) strpad(fname//'('//IntToStr(i)//')',16)//str
        end do

        return

    end subroutine ftprint_fdoublearray


    ! ftprint_flogicalarray
    ! -----------------------------------------------------------------------
    ! fortax printing of logical array when printing system file (called by 
    ! fortaxPrint)

    subroutine ftprint_flogicalarray(funit,flogicalarray,fname)

        use fortax_util, only : intToStr, strPad

        implicit none

        integer,      intent(in) :: funit
        logical,      intent(in) :: flogicalarray(:)
        character(*), intent(in) :: fname

        integer :: i

        do i = 1, size(flogicalarray)
            write(funit,*) strPad(fname//'('//intToStr(i)//')',16)//merge('yes','no ',flogicalarray(i))
        end do
        
        return

    end subroutine ftprint_flogicalarray


end module fortax_write
