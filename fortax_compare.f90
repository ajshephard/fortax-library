
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




! fortax_compare
! -----------------------------------------------------------------------
! module provides the ability to create a database of family types and
! incomes which can then easily be compated against. AS 04/07/13


module fortax_compare

    use fortax_realtype

contains

    pure subroutine getRnd_Logical(rnd,grid,y)        
        implicit none
        real(dp), intent(in)  :: rnd(:) 
        logical,  intent(in)  :: grid(:)
        logical,  intent(out) :: y(size(rnd))
        integer :: ix, n
        integer :: rndInt(size(rnd))
        n = size(grid)
        rndInt = ceiling(rnd*n)
        do ix = 1, size(rnd)
            y(ix) = grid(rndInt(ix))
        end do
    end subroutine getRnd_Logical

    pure subroutine getRnd_Int(rnd,grid,y)        
        implicit none
        real(dp), intent(in)  :: rnd(:) 
        integer,  intent(in)  :: grid(:)
        integer,  intent(out) :: y(size(rnd))
        integer :: ix, n
        integer :: rndInt(size(rnd))
        n = size(grid)
        rndInt = ceiling(rnd*n)
        do ix = 1, size(rnd)
            y(ix) = grid(rndInt(ix))
        end do
    end subroutine getRnd_Int

    pure subroutine getRnd_real(rnd,grid,y)
        implicit none
        real(dp), intent(in)  :: rnd(:) 
        real(dp),  intent(in)  :: grid(:)
        real(dp),  intent(out) :: y(size(rnd))
        integer :: ix, n
        integer :: rndInt(size(rnd))
        n = size(grid)
        rndInt = ceiling(rnd*n)
        do ix = 1, size(rnd)
            y(ix) = grid(rndInt(ix))
        end do
    end subroutine getRnd_real

    pure subroutine getRnd_Char(rnd,grid,y)        
        implicit none
        real(dp), intent(in)  :: rnd(:) 
        character(len=*), intent(in)  :: grid(:)
        character(len=40), intent(out) :: y(size(rnd))
        integer :: ix, n
        integer :: rndInt(size(rnd))
        n = size(grid)
        rndInt = ceiling(rnd*n)
        do ix = 1, size(rnd)
            y(ix) = grid(rndInt(ix))
        end do
    end subroutine getRnd_Char

    subroutine writeFamCompareDatabase(nfam,fam,sysname,sysdir,fname)
    
        use fortax_type, only : fam_t, fam_init, net_t, net_init, sys_t
        use fortax_write, only : xml_ftag,ftxmlwrite
        use fortax_util, only : intToStr
        use xmlparse,    only : xml_parse, xml_open, xml_close
        use fortax_read, only : readFortaxParams
        use fortax_calc, only : calcNetInc

        implicit none
        
        integer, intent(in) :: nfam
        type(fam_t), intent(in)  :: fam(nfam)
        character(len=40), intent(in) :: sysname(nfam)
        character(len=*), intent(in) :: sysdir
        character(len=*), intent(in) :: fname
        character(len=40) :: syslist(nfam)
        integer :: sysnum(nfam)
        type(net_t) :: net(nfam)
        type(sys_t), allocatable :: sys(:)

        character(255)           :: attribs(2,1)
        logical                  :: mustRead
        type(xml_parse)          :: info

        integer :: i, n2

        call uniqueList(sysname,syslist,nfam,sysnum,n2)

        allocate(sys(n2))

        do i = 1, n2
            call readFortaxParams(sys(i),sysdir//'/'//trim(syslist(i))//'.xml')
        end do

        call net_init(net)

        do i = 1, nfam
            call calcNetInc(sys(sysnum(i)),fam(i),net(i))
        end do

        mustRead = .false.

        attribs(1,1) = 'system'
        call xml_open(info,fname,mustRead)
        call xml_ftag(info,'fortax','open')

        do i = 1, size(fam)
            call xml_ftag(info,'family','open')
#           include "includes/famcompare_write.inc"
            call xml_ftag(info,'family','close')
        end do

        call xml_ftag(info,'fortax','close')
        call xml_close(info)

        return

    end subroutine writeFamCompareDatabase
            

    subroutine readFamCompareDatabase(familyFile,fam,net,sysname,nfam)
    
        use xml_data_xmlfamcompare_t, only : read_xml_file_xmlfamcompare_t, family
        use fortax_util,          only : getunit, strToDouble, strToInt, strToLogical, lower, fortaxError, fortaxWarn
        use fortax_type,          only : sys_t, sys_init, fam_t, fam_init, fam_desc, net_t, net_init
        use fortax_read,          only : fortax_read_assign

        implicit none
        
        character(len=*),  intent(in)               :: familyFile
        type(fam_t),       intent(out), allocatable :: fam(:)
        type(net_t),       intent(out), allocatable :: net(:)
        character(len=40), intent(out), allocatable :: sysname(:)
        integer,           intent(out)              :: nfam

        integer :: i, j, xmlUnit
        logical :: isFile

        inquire(file=familyFile, exist=isFile)
        
        if (.not. isFile) then
            call fortaxError('family file does not exist ('//trim(adjustl(familyFile))//')')
        end if              

        call getunit(xmlUnit)
        call read_xml_file_xmlfamcompare_t(familyFile,xmlUnit)

        nfam = size(family)
        allocate(fam(nfam))
        allocate(net(nfam))
        allocate(sysname(nfam))

        call fam_init(fam)
        call net_init(net)
        
        !use fpp so that the reading will fully reflect the data structure
        do i = 1, size(family)
#           include 'includes/famcompare_read.inc'
        end do

        if (associated(family)) deallocate(family)
        
        close(xmlunit)

    end subroutine readFamCompareDatabase


    subroutine compareFamDatabase(n,fam,sysname,net0,maxdiff,tolin,funitin)
        use fortax_type, only : fam_t, sys_t, net_t
        use fortax_read, only : readFortaxParams
        use fortax_calc, only : calcNetInc
        use fortax_util, only : intToStr
        use, intrinsic :: iso_fortran_env
        implicit none
        integer, intent(in) :: n
        type(fam_t), intent(in) :: fam(n)
        type(net_t), intent(in) :: net0(n)
        integer, intent(in) :: maxdiff
        character(len=40), intent(in) :: sysname(n)
        real(dp), intent(in), optional :: tolin
        integer, intent(in), optional :: funitin

        type(sys_t), allocatable :: sys(:)
        character(len=40) :: syslist(n)
        integer :: sysnum(n)
        integer :: n2

        real(dp) :: tol
        integer  :: funit

        integer :: diff, diffid, difftot
        logical :: verbose

        integer :: i

        type(net_t) :: net1(n)

        if (.not.present(tolin)) then
            tol = 1.0e-10_dp !tiny(0.0_dp)
        else
            tol = tolin
        end if

        if (.not.present(funitin)) then
            funit = output_unit
        else
            funit = funitin
        end if

        call uniqueList(sysname,syslist,n,sysnum,n2)

        allocate(sys(n2))

        do i = 1, n2
            call readFortaxParams(sys(i),'../systems/'//trim(syslist(i))//'.xml')
        end do

        verbose = .true.
        diffid  = 0
        difftot = 0

        if (maxdiff==0) then
            verbose = .false.
        end if

        write(funit,'(A)') 'Comparing incomes for '//intToStr(n)//' families...'

        do i = 1, n

            call calcNetInc(sys(sysnum(i)),fam(i),net1(i))
            call compareNet(net0(i),net1(i),diff,i,funit,verbose,tol)

            diffid = diffid + merge(1,0,diff>0)
            difftot = difftot + diff

            if (diffid==maxdiff) then
                verbose = .false.
                write(funit,*)
                write(funit,'(A)') 'Printing of further detailed differences is supressed.'
            end if

        end do

        write(funit,*)

        if (diffid>0) then
            write(funit,'(A)') 'Differences were identified for '//intToStr(diffid)//' families.'
            write(funit,'(A)') 'The total number of differences in net is '//intToStr(difftot)//'.'
        else
            write(funit,'(A)') 'No differences were found.'
        end if
        
        write(funit,*)
        
    end subroutine compareFamDatabase

    pure subroutine uniqueList(list,list2,nlist,listid,nlist2)
        implicit none
        integer, intent(in) :: nlist
        character(len=40), intent(in) :: list(nlist)
        character(len=40), intent(out) :: list2(nlist)
        integer, intent(out) :: listid(nlist)
        integer, intent(out) :: nlist2

        integer :: i, j
        logical :: inlist

        nlist2 = 1
        list2(1) = list(1)
        listid(1) = 1

        do i = 2, nlist
            inlist = .false.
            do j = 1, nlist2
                if (list(i)==list2(j)) then
                    inlist = .true.
                    listid(i) = j
                    exit
                end if
            end do
            if (.not. inlist) then
                nlist2 = nlist2+1
                list2(nlist2) = list(i)
                listid(i) = nlist2
            end if
        end do

    end subroutine uniqueList

    subroutine compareNet(net0,net1,diff,id,funit,verbose,tol)
        use fortax_type, only : net_t
        implicit none
        type(net_t), intent(in) :: net0, net1
        integer, intent(in) :: id
        integer, intent(in) :: funit
        integer, intent(out) :: diff
        logical, intent(in)  :: verbose
        real(dp), intent(in) :: tol

        diff = 0

#       undef _$header
#       undef _$footer
#       undef _$integer
#       undef _$double
#       undef _$logical

#       define _$header
#       define _$footer
#       define _$integer(x,y,z) call compareInteger(net0%tu%x,net1%tu%x,#x,funit,id,diff,verbose)
#       define _$double(x,y,z)  call compareDouble(net0%tu%x,net1%tu%x,#x,funit,id,diff,verbose,tol)
#       define _$logical(x,y,z) call compareLogical(net0%tu%x,net1%tu%x,#x,funit,id,diff,verbose)
        
#       include "includes/nettu_t.inc"

#       undef _$header
#       undef _$footer
#       undef _$integer
#       undef _$double
#       undef _$logical

#       define _$header
#       define _$footer
#       define _$integer(x,y,z) call compareInteger(net0%ad(1)%x,net1%ad(1)%x,#x//' (ad1)',funit,id,diff,verbose)
#       define _$double(x,y,z)  call compareDouble(net0%ad(1)%x,net1%ad(1)%x,#x//' (ad1)',funit,id,diff,verbose,tol)
#       define _$logical(x,y,z) call compareLogical(net0%ad(1)%x,net1%ad(1)%x,#x//' (ad1)',funit,id,diff,verbose)

#       include "includes/netad_t.inc"

#       undef _$header
#       undef _$footer
#       undef _$integer
#       undef _$double
#       undef _$logical

#       define _$header
#       define _$footer
#       define _$integer(x,y,z) call compareInteger(net0%ad(2)%x,net1%ad(2)%x,#x//' (ad2)',funit,id,diff,verbose)
#       define _$double(x,y,z)  call compareDouble(net0%ad(2)%x,net1%ad(2)%x,#x//' (ad2)',funit,id,diff,verbose,tol)
#       define _$logical(x,y,z) call compareLogical(net0%ad(2)%x,net1%ad(2)%x,#x//' (ad2)',funit,id,diff,verbose)

#       include "includes/netad_t.inc"

#       undef _$integer
#       undef _$double
#       undef _$logical

    end subroutine compareNet

    subroutine compareInteger(int0,int1,name,funit,id,diff,verbose)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: int0, int1
        character(len=*), intent(in) :: name
        integer, intent(in) :: funit, id
        integer, intent(inout) :: diff
        logical, intent(in) :: verbose

        if (int0.ne.int1) then
            diff = diff+1
            if (verbose) then
                write(funit,'(A)') 'id = '//intToStr(id)//', '//name//', file = '//intToStr(int0)// &
                    ', calc = '//intToStr(int1)//', diff = '//intToStr(int0-int1)
            end if
        end if

    end subroutine compareInteger

    subroutine compareLogical(bool0,bool1,name,funit,id,diff,verbose)
        use fortax_util, only : intToStr, logicalToStr
        implicit none
        logical, intent(in) :: bool0, bool1
        character(len=*), intent(in) :: name
        integer, intent(in) :: funit, id
        integer, intent(inout) :: diff
        logical, intent(in) :: verbose

        if (bool0.neqv.bool1) then
            diff = diff+1
            if (verbose) then
                write(funit,'(A)') 'id = '//intToStr(id)//', '//name//', file = '//logicalToStr(bool0)// &
                    ', calc = '//logicalToStr(bool1)//', diff = .'
            end if
        end if

    end subroutine compareLogical

    subroutine compareDouble(real0,real1,name,funit,id,diff,verbose,tol)
        use fortax_util, only : intToStr, dblToStr
        implicit none
        real(dp), intent(in) :: real0, real1
        character(len=*), intent(in) :: name
        integer, intent(in) :: funit, id
        integer, intent(inout) :: diff
        logical, intent(in) :: verbose
        real(dp), intent(in) :: tol

        if (abs(real0-real1)>tol) then
            diff = diff+1
            if (verbose) then
                write(funit,'(A)') 'id = '//intToStr(id)//', '//name//', file = '//dblToStr(real0)// &
                    ', calc = '//dblToStr(real1)//', diff = '//dblToStr(real0-real1)
            end if
        end if

    end subroutine compareDouble

end module fortax_compare