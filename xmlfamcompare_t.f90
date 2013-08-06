module xml_data_xmlfamcompare_t
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_

type finteger_t
   character(len=40)                                :: name
   integer                                         :: value
end type finteger_t

type flogical_t
   character(len=40)                                :: name
   logical                                         :: value
end type flogical_t

type fdouble_t
   character(len=40)                                :: name
   real(kind=kind(1.0d0))                          :: value
end type fdouble_t

type fintegerarray_t
   character(len=40)                                :: name
   integer, dimension(:), pointer                  :: value => null()
end type fintegerarray_t

type flogicalarray_t
   character(len=40)                                :: name
   logical, dimension(:), pointer                  :: value => null()
end type flogicalarray_t

type fdoublearray_t
   character(len=40)                                :: name
   real(kind=kind(1.0d0)), dimension(:), pointer   :: value => null()
end type fdoublearray_t

type ad_t
   type(finteger_t), dimension(:), pointer         :: finteger => null()
   type(flogical_t), dimension(:), pointer         :: flogical => null()
   type(fdouble_t), dimension(:), pointer          :: fdouble => null()
   type(fintegerarray_t), dimension(:), pointer    :: fintegerarray => null()
   type(flogicalarray_t), dimension(:), pointer    :: flogicalarray => null()
   type(fdoublearray_t), dimension(:), pointer     :: fdoublearray => null()
end type ad_t

type nettu_t
   type(finteger_t), dimension(:), pointer         :: finteger => null()
   type(flogical_t), dimension(:), pointer         :: flogical => null()
   type(fdouble_t), dimension(:), pointer          :: fdouble => null()
   type(fintegerarray_t), dimension(:), pointer    :: fintegerarray => null()
   type(flogicalarray_t), dimension(:), pointer    :: flogicalarray => null()
   type(fdoublearray_t), dimension(:), pointer     :: fdoublearray => null()
end type nettu_t

type netad_t
   type(finteger_t), dimension(:), pointer         :: finteger => null()
   type(flogical_t), dimension(:), pointer         :: flogical => null()
   type(fdouble_t), dimension(:), pointer          :: fdouble => null()
   type(fintegerarray_t), dimension(:), pointer    :: fintegerarray => null()
   type(flogicalarray_t), dimension(:), pointer    :: flogicalarray => null()
   type(fdoublearray_t), dimension(:), pointer     :: fdoublearray => null()
end type netad_t

type net_t
   character(len=40)                                :: system
   type(nettu_t)                                   :: nettu
   type(netad_t)                                   :: netad1
   type(netad_t)                                   :: netad2
end type net_t

type fam_t
   type(finteger_t), dimension(:), pointer         :: finteger => null()
   type(flogical_t), dimension(:), pointer         :: flogical => null()
   type(fdouble_t), dimension(:), pointer          :: fdouble => null()
   type(fintegerarray_t), dimension(:), pointer    :: fintegerarray => null()
   type(flogicalarray_t), dimension(:), pointer    :: flogicalarray => null()
   type(fdoublearray_t), dimension(:), pointer     :: fdoublearray => null()
   type(ad_t)                                      :: ad1
   type(ad_t)                                      :: ad2
end type fam_t

type family_t
   type(fam_t)                                     :: fam
   type(net_t)                                     :: net
end type family_t
   type(family_t), dimension(:), pointer           :: family => null()
contains
subroutine read_xml_type_finteger_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(finteger_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(finteger_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_finteger_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_finteger_t_array

subroutine read_xml_type_finteger_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(finteger_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_name
   logical                                         :: has_value
   has_name                             = .false.
   has_value                            = .false.
   call init_xml_type_finteger_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%name, has_name )
      case('value')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%value, has_value )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on name')
   endif
   if ( .not. has_value ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on value')
   endif
end subroutine read_xml_type_finteger_t
subroutine init_xml_type_finteger_t_array( dvar )
   type(finteger_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_finteger_t_array
subroutine init_xml_type_finteger_t(dvar)
   type(finteger_t) :: dvar
end subroutine init_xml_type_finteger_t
subroutine write_xml_type_finteger_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(finteger_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_finteger_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_finteger_t_array

subroutine write_xml_type_finteger_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(finteger_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_word( info, 'name', indent+3, dvar%name)
   call write_to_xml_integer( info, 'value', indent+3, dvar%value)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_finteger_t

subroutine read_xml_type_flogical_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(flogical_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(flogical_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_flogical_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_flogical_t_array

subroutine read_xml_type_flogical_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(flogical_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_name
   logical                                         :: has_value
   has_name                             = .false.
   has_value                            = .false.
   call init_xml_type_flogical_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%name, has_name )
      case('value')
         call read_xml_logical( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%value, has_value )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on name')
   endif
   if ( .not. has_value ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on value')
   endif
end subroutine read_xml_type_flogical_t
subroutine init_xml_type_flogical_t_array( dvar )
   type(flogical_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_flogical_t_array
subroutine init_xml_type_flogical_t(dvar)
   type(flogical_t) :: dvar
end subroutine init_xml_type_flogical_t
subroutine write_xml_type_flogical_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(flogical_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_flogical_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_flogical_t_array

subroutine write_xml_type_flogical_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(flogical_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_word( info, 'name', indent+3, dvar%name)
   call write_to_xml_logical( info, 'value', indent+3, dvar%value)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_flogical_t

subroutine read_xml_type_fdouble_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(fdouble_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(fdouble_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_fdouble_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_fdouble_t_array

subroutine read_xml_type_fdouble_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(fdouble_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_name
   logical                                         :: has_value
   has_name                             = .false.
   has_value                            = .false.
   call init_xml_type_fdouble_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%name, has_name )
      case('value')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%value, has_value )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on name')
   endif
   if ( .not. has_value ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on value')
   endif
end subroutine read_xml_type_fdouble_t
subroutine init_xml_type_fdouble_t_array( dvar )
   type(fdouble_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_fdouble_t_array
subroutine init_xml_type_fdouble_t(dvar)
   type(fdouble_t) :: dvar
end subroutine init_xml_type_fdouble_t
subroutine write_xml_type_fdouble_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(fdouble_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_fdouble_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_fdouble_t_array

subroutine write_xml_type_fdouble_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(fdouble_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_word( info, 'name', indent+3, dvar%name)
   call write_to_xml_double( info, 'value', indent+3, dvar%value)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_fdouble_t

subroutine read_xml_type_fintegerarray_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(fintegerarray_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(fintegerarray_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_fintegerarray_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_fintegerarray_t_array

subroutine read_xml_type_fintegerarray_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(fintegerarray_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_name
   logical                                         :: has_value
   has_name                             = .false.
   has_value                            = .false.
   call init_xml_type_fintegerarray_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%name, has_name )
      case('value')
         call read_xml_integer_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%value, has_value )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on name')
   endif
   if ( .not. has_value ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on value')
   endif
end subroutine read_xml_type_fintegerarray_t
subroutine init_xml_type_fintegerarray_t_array( dvar )
   type(fintegerarray_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_fintegerarray_t_array
subroutine init_xml_type_fintegerarray_t(dvar)
   type(fintegerarray_t) :: dvar
end subroutine init_xml_type_fintegerarray_t
subroutine write_xml_type_fintegerarray_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(fintegerarray_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_fintegerarray_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_fintegerarray_t_array

subroutine write_xml_type_fintegerarray_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(fintegerarray_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_word( info, 'name', indent+3, dvar%name)
   call write_to_xml_integer_array( info, 'value', indent+3, dvar%value)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_fintegerarray_t

subroutine read_xml_type_flogicalarray_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(flogicalarray_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(flogicalarray_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_flogicalarray_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_flogicalarray_t_array

subroutine read_xml_type_flogicalarray_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(flogicalarray_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_name
   logical                                         :: has_value
   has_name                             = .false.
   has_value                            = .false.
   call init_xml_type_flogicalarray_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%name, has_name )
      case('value')
         call read_xml_logical_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%value, has_value )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on name')
   endif
   if ( .not. has_value ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on value')
   endif
end subroutine read_xml_type_flogicalarray_t
subroutine init_xml_type_flogicalarray_t_array( dvar )
   type(flogicalarray_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_flogicalarray_t_array
subroutine init_xml_type_flogicalarray_t(dvar)
   type(flogicalarray_t) :: dvar
end subroutine init_xml_type_flogicalarray_t
subroutine write_xml_type_flogicalarray_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(flogicalarray_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_flogicalarray_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_flogicalarray_t_array

subroutine write_xml_type_flogicalarray_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(flogicalarray_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_word( info, 'name', indent+3, dvar%name)
   call write_to_xml_logical_array( info, 'value', indent+3, dvar%value)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_flogicalarray_t

subroutine read_xml_type_fdoublearray_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(fdoublearray_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(fdoublearray_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_fdoublearray_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_fdoublearray_t_array

subroutine read_xml_type_fdoublearray_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(fdoublearray_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_name
   logical                                         :: has_value
   has_name                             = .false.
   has_value                            = .false.
   call init_xml_type_fdoublearray_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%name, has_name )
      case('value')
         call read_xml_double_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%value, has_value )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on name')
   endif
   if ( .not. has_value ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on value')
   endif
end subroutine read_xml_type_fdoublearray_t
subroutine init_xml_type_fdoublearray_t_array( dvar )
   type(fdoublearray_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_fdoublearray_t_array
subroutine init_xml_type_fdoublearray_t(dvar)
   type(fdoublearray_t) :: dvar
end subroutine init_xml_type_fdoublearray_t
subroutine write_xml_type_fdoublearray_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(fdoublearray_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_fdoublearray_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_fdoublearray_t_array

subroutine write_xml_type_fdoublearray_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(fdoublearray_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_word( info, 'name', indent+3, dvar%name)
   call write_to_xml_double_array( info, 'value', indent+3, dvar%value)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_fdoublearray_t

subroutine read_xml_type_ad_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(ad_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(ad_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_ad_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_ad_t_array

subroutine read_xml_type_ad_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(ad_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_finteger
   logical                                         :: has_flogical
   logical                                         :: has_fdouble
   logical                                         :: has_fintegerarray
   logical                                         :: has_flogicalarray
   logical                                         :: has_fdoublearray
   has_finteger                         = .false.
   allocate(dvar%finteger(0))
   has_flogical                         = .false.
   allocate(dvar%flogical(0))
   has_fdouble                          = .false.
   allocate(dvar%fdouble(0))
   has_fintegerarray                    = .false.
   allocate(dvar%fintegerarray(0))
   has_flogicalarray                    = .false.
   allocate(dvar%flogicalarray(0))
   has_fdoublearray                     = .false.
   allocate(dvar%fdoublearray(0))
   call init_xml_type_ad_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('finteger')
         call read_xml_type_finteger_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%finteger, has_finteger )
      case('flogical')
         call read_xml_type_flogical_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flogical, has_flogical )
      case('fdouble')
         call read_xml_type_fdouble_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fdouble, has_fdouble )
      case('fintegerarray')
         call read_xml_type_fintegerarray_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fintegerarray, has_fintegerarray )
      case('flogicalarray')
         call read_xml_type_flogicalarray_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flogicalarray, has_flogicalarray )
      case('fdoublearray')
         call read_xml_type_fdoublearray_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fdoublearray, has_fdoublearray )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_finteger ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on finteger')
   endif
   if ( .not. has_flogical ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flogical')
   endif
   if ( .not. has_fdouble ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fdouble')
   endif
   if ( .not. has_fintegerarray ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fintegerarray')
   endif
   if ( .not. has_flogicalarray ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flogicalarray')
   endif
   if ( .not. has_fdoublearray ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fdoublearray')
   endif
end subroutine read_xml_type_ad_t
subroutine init_xml_type_ad_t_array( dvar )
   type(ad_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_ad_t_array
subroutine init_xml_type_ad_t(dvar)
   type(ad_t) :: dvar
end subroutine init_xml_type_ad_t
subroutine write_xml_type_ad_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(ad_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_ad_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_ad_t_array

subroutine write_xml_type_ad_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(ad_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_xml_type_finteger_t_array( info, 'finteger', indent+3, dvar%finteger)
   call write_xml_type_flogical_t_array( info, 'flogical', indent+3, dvar%flogical)
   call write_xml_type_fdouble_t_array( info, 'fdouble', indent+3, dvar%fdouble)
   call write_xml_type_fintegerarray_t_array( info, 'fintegerarray', indent+3, dvar%fintegerarray)
   call write_xml_type_flogicalarray_t_array( info, 'flogicalarray', indent+3, dvar%flogicalarray)
   call write_xml_type_fdoublearray_t_array( info, 'fdoublearray', indent+3, dvar%fdoublearray)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_ad_t

subroutine read_xml_type_nettu_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(nettu_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(nettu_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_nettu_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_nettu_t_array

subroutine read_xml_type_nettu_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(nettu_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_finteger
   logical                                         :: has_flogical
   logical                                         :: has_fdouble
   logical                                         :: has_fintegerarray
   logical                                         :: has_flogicalarray
   logical                                         :: has_fdoublearray
   has_finteger                         = .false.
   allocate(dvar%finteger(0))
   has_flogical                         = .false.
   allocate(dvar%flogical(0))
   has_fdouble                          = .false.
   allocate(dvar%fdouble(0))
   has_fintegerarray                    = .false.
   allocate(dvar%fintegerarray(0))
   has_flogicalarray                    = .false.
   allocate(dvar%flogicalarray(0))
   has_fdoublearray                     = .false.
   allocate(dvar%fdoublearray(0))
   call init_xml_type_nettu_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('finteger')
         call read_xml_type_finteger_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%finteger, has_finteger )
      case('flogical')
         call read_xml_type_flogical_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flogical, has_flogical )
      case('fdouble')
         call read_xml_type_fdouble_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fdouble, has_fdouble )
      case('fintegerarray')
         call read_xml_type_fintegerarray_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fintegerarray, has_fintegerarray )
      case('flogicalarray')
         call read_xml_type_flogicalarray_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flogicalarray, has_flogicalarray )
      case('fdoublearray')
         call read_xml_type_fdoublearray_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fdoublearray, has_fdoublearray )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_finteger ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on finteger')
   endif
   if ( .not. has_flogical ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flogical')
   endif
   if ( .not. has_fdouble ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fdouble')
   endif
   if ( .not. has_fintegerarray ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fintegerarray')
   endif
   if ( .not. has_flogicalarray ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flogicalarray')
   endif
   if ( .not. has_fdoublearray ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fdoublearray')
   endif
end subroutine read_xml_type_nettu_t
subroutine init_xml_type_nettu_t_array( dvar )
   type(nettu_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_nettu_t_array
subroutine init_xml_type_nettu_t(dvar)
   type(nettu_t) :: dvar
end subroutine init_xml_type_nettu_t
subroutine write_xml_type_nettu_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(nettu_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_nettu_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_nettu_t_array

subroutine write_xml_type_nettu_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(nettu_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_xml_type_finteger_t_array( info, 'finteger', indent+3, dvar%finteger)
   call write_xml_type_flogical_t_array( info, 'flogical', indent+3, dvar%flogical)
   call write_xml_type_fdouble_t_array( info, 'fdouble', indent+3, dvar%fdouble)
   call write_xml_type_fintegerarray_t_array( info, 'fintegerarray', indent+3, dvar%fintegerarray)
   call write_xml_type_flogicalarray_t_array( info, 'flogicalarray', indent+3, dvar%flogicalarray)
   call write_xml_type_fdoublearray_t_array( info, 'fdoublearray', indent+3, dvar%fdoublearray)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_nettu_t

subroutine read_xml_type_netad_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(netad_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(netad_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_netad_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_netad_t_array

subroutine read_xml_type_netad_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(netad_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_finteger
   logical                                         :: has_flogical
   logical                                         :: has_fdouble
   logical                                         :: has_fintegerarray
   logical                                         :: has_flogicalarray
   logical                                         :: has_fdoublearray
   has_finteger                         = .false.
   allocate(dvar%finteger(0))
   has_flogical                         = .false.
   allocate(dvar%flogical(0))
   has_fdouble                          = .false.
   allocate(dvar%fdouble(0))
   has_fintegerarray                    = .false.
   allocate(dvar%fintegerarray(0))
   has_flogicalarray                    = .false.
   allocate(dvar%flogicalarray(0))
   has_fdoublearray                     = .false.
   allocate(dvar%fdoublearray(0))
   call init_xml_type_netad_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('finteger')
         call read_xml_type_finteger_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%finteger, has_finteger )
      case('flogical')
         call read_xml_type_flogical_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flogical, has_flogical )
      case('fdouble')
         call read_xml_type_fdouble_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fdouble, has_fdouble )
      case('fintegerarray')
         call read_xml_type_fintegerarray_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fintegerarray, has_fintegerarray )
      case('flogicalarray')
         call read_xml_type_flogicalarray_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flogicalarray, has_flogicalarray )
      case('fdoublearray')
         call read_xml_type_fdoublearray_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fdoublearray, has_fdoublearray )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_finteger ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on finteger')
   endif
   if ( .not. has_flogical ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flogical')
   endif
   if ( .not. has_fdouble ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fdouble')
   endif
   if ( .not. has_fintegerarray ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fintegerarray')
   endif
   if ( .not. has_flogicalarray ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flogicalarray')
   endif
   if ( .not. has_fdoublearray ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fdoublearray')
   endif
end subroutine read_xml_type_netad_t
subroutine init_xml_type_netad_t_array( dvar )
   type(netad_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_netad_t_array
subroutine init_xml_type_netad_t(dvar)
   type(netad_t) :: dvar
end subroutine init_xml_type_netad_t
subroutine write_xml_type_netad_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(netad_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_netad_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_netad_t_array

subroutine write_xml_type_netad_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(netad_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_xml_type_finteger_t_array( info, 'finteger', indent+3, dvar%finteger)
   call write_xml_type_flogical_t_array( info, 'flogical', indent+3, dvar%flogical)
   call write_xml_type_fdouble_t_array( info, 'fdouble', indent+3, dvar%fdouble)
   call write_xml_type_fintegerarray_t_array( info, 'fintegerarray', indent+3, dvar%fintegerarray)
   call write_xml_type_flogicalarray_t_array( info, 'flogicalarray', indent+3, dvar%flogicalarray)
   call write_xml_type_fdoublearray_t_array( info, 'fdoublearray', indent+3, dvar%fdoublearray)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_netad_t

subroutine read_xml_type_net_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(net_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(net_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_net_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_net_t_array

subroutine read_xml_type_net_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(net_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_system
   logical                                         :: has_nettu
   logical                                         :: has_netad1
   logical                                         :: has_netad2
   has_system                           = .false.
   has_nettu                            = .false.
   has_netad1                           = .false.
   has_netad2                           = .false.
   call init_xml_type_net_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('system')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%system, has_system )
      case('nettu')
         call read_xml_type_nettu_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%nettu, has_nettu )
      case('netad1')
         call read_xml_type_netad_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%netad1, has_netad1 )
      case('netad2')
         call read_xml_type_netad_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%netad2, has_netad2 )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_system ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on system')
   endif
   if ( .not. has_nettu ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on nettu')
   endif
   if ( .not. has_netad1 ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on netad1')
   endif
   if ( .not. has_netad2 ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on netad2')
   endif
end subroutine read_xml_type_net_t
subroutine init_xml_type_net_t_array( dvar )
   type(net_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_net_t_array
subroutine init_xml_type_net_t(dvar)
   type(net_t) :: dvar
end subroutine init_xml_type_net_t
subroutine write_xml_type_net_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(net_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_net_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_net_t_array

subroutine write_xml_type_net_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(net_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_word( info, 'system', indent+3, dvar%system)
   call write_xml_type_nettu_t( info, 'nettu', indent+3, dvar%nettu)
   call write_xml_type_netad_t( info, 'netad1', indent+3, dvar%netad1)
   call write_xml_type_netad_t( info, 'netad2', indent+3, dvar%netad2)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_net_t

subroutine read_xml_type_fam_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(fam_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(fam_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_fam_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_fam_t_array

subroutine read_xml_type_fam_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(fam_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_finteger
   logical                                         :: has_flogical
   logical                                         :: has_fdouble
   logical                                         :: has_fintegerarray
   logical                                         :: has_flogicalarray
   logical                                         :: has_fdoublearray
   logical                                         :: has_ad1
   logical                                         :: has_ad2
   has_finteger                         = .false.
   allocate(dvar%finteger(0))
   has_flogical                         = .false.
   allocate(dvar%flogical(0))
   has_fdouble                          = .false.
   allocate(dvar%fdouble(0))
   has_fintegerarray                    = .false.
   allocate(dvar%fintegerarray(0))
   has_flogicalarray                    = .false.
   allocate(dvar%flogicalarray(0))
   has_fdoublearray                     = .false.
   allocate(dvar%fdoublearray(0))
   has_ad1                              = .false.
   has_ad2                              = .false.
   call init_xml_type_fam_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('finteger')
         call read_xml_type_finteger_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%finteger, has_finteger )
      case('flogical')
         call read_xml_type_flogical_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flogical, has_flogical )
      case('fdouble')
         call read_xml_type_fdouble_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fdouble, has_fdouble )
      case('fintegerarray')
         call read_xml_type_fintegerarray_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fintegerarray, has_fintegerarray )
      case('flogicalarray')
         call read_xml_type_flogicalarray_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%flogicalarray, has_flogicalarray )
      case('fdoublearray')
         call read_xml_type_fdoublearray_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fdoublearray, has_fdoublearray )
      case('ad1')
         call read_xml_type_ad_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%ad1, has_ad1 )
      case('ad2')
         call read_xml_type_ad_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%ad2, has_ad2 )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_finteger ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on finteger')
   endif
   if ( .not. has_flogical ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flogical')
   endif
   if ( .not. has_fdouble ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fdouble')
   endif
   if ( .not. has_fintegerarray ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fintegerarray')
   endif
   if ( .not. has_flogicalarray ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on flogicalarray')
   endif
   if ( .not. has_fdoublearray ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fdoublearray')
   endif
   if ( .not. has_ad1 ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on ad1')
   endif
   if ( .not. has_ad2 ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on ad2')
   endif
end subroutine read_xml_type_fam_t
subroutine init_xml_type_fam_t_array( dvar )
   type(fam_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_fam_t_array
subroutine init_xml_type_fam_t(dvar)
   type(fam_t) :: dvar
end subroutine init_xml_type_fam_t
subroutine write_xml_type_fam_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(fam_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_fam_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_fam_t_array

subroutine write_xml_type_fam_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(fam_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_xml_type_finteger_t_array( info, 'finteger', indent+3, dvar%finteger)
   call write_xml_type_flogical_t_array( info, 'flogical', indent+3, dvar%flogical)
   call write_xml_type_fdouble_t_array( info, 'fdouble', indent+3, dvar%fdouble)
   call write_xml_type_fintegerarray_t_array( info, 'fintegerarray', indent+3, dvar%fintegerarray)
   call write_xml_type_flogicalarray_t_array( info, 'flogicalarray', indent+3, dvar%flogicalarray)
   call write_xml_type_fdoublearray_t_array( info, 'fdoublearray', indent+3, dvar%fdoublearray)
   call write_xml_type_ad_t( info, 'ad1', indent+3, dvar%ad1)
   call write_xml_type_ad_t( info, 'ad2', indent+3, dvar%ad2)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_fam_t

subroutine read_xml_type_family_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(family_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(family_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_family_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_family_t_array

subroutine read_xml_type_family_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(family_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_fam
   logical                                         :: has_net
   has_fam                              = .false.
   has_net                              = .false.
   call init_xml_type_family_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('fam')
         call read_xml_type_fam_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%fam, has_fam )
      case('net')
         call read_xml_type_net_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%net, has_net )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_fam ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on fam')
   endif
   if ( .not. has_net ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on net')
   endif
end subroutine read_xml_type_family_t
subroutine init_xml_type_family_t_array( dvar )
   type(family_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_family_t_array
subroutine init_xml_type_family_t(dvar)
   type(family_t) :: dvar
end subroutine init_xml_type_family_t
subroutine write_xml_type_family_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(family_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_family_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_family_t_array

subroutine write_xml_type_family_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(family_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_xml_type_fam_t( info, 'fam', indent+3, dvar%fam)
   call write_xml_type_net_t( info, 'net', indent+3, dvar%net)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_family_t

subroutine read_xml_file_xmlfamcompare_t(fname, lurep, errout)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep
   logical, intent(out), optional         :: errout

   type(XML_PARSE)                        :: info
   logical                                :: error
   character(len=80)                      :: tag
   character(len=80)                      :: starttag
   logical                                :: endtag
   character(len=80), dimension(1:2,1:20) :: attribs
   integer                                :: noattribs
   character(len=200), dimension(1:100)   :: data
   integer                                :: nodata
   logical                                         :: has_family
   has_family                           = .false.
   allocate(family(0))

   call init_xml_file_xmlfamcompare_t
   call xml_open( info, fname, .true. )
   call xml_options( info, report_errors=.true., ignore_whitespace=.true.)
   lurep_ = 0
   if ( present(lurep) ) then
      lurep_ = lurep
      call xml_options( info, report_lun=lurep )
   endif
   do
      call xml_get( info, starttag, endtag, attribs, noattribs, &
         data, nodata)
      if ( starttag .ne. '!--' ) exit
   enddo
   if ( starttag .ne. "fortax" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "fortax"')
      error = .true.
      call xml_close(info)
      return
   endif
   strict_ = .false.
   error = .false.
   do
      call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
      if ( xml_error(info) ) then
         write(lurep_,*) 'Error reading input file!'
         error = .true.
         return
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('family')
         call read_xml_type_family_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            family, has_family )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_family ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on family')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_xmlfamcompare_t(fname, lurep)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep

   type(XML_PARSE)                        :: info
   integer                                :: indent = 0

   call xml_open( info, fname, .false. )
   call xml_options( info, report_errors=.true.)
   if ( present(lurep) ) then
       call xml_options( info, report_errors=.true.)
   endif
   write(info%lun,'(a)') &
      '<fortax>'
   call write_xml_type_family_t_array( info, 'family', indent+3, family)
   write(info%lun,'(a)') '</fortax>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_xmlfamcompare_t

end subroutine

end module
