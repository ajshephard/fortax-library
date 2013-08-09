module xml_data_xmltaxben_t
   use READ_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_

type field_t
   character(len=80)                                :: Name
   character(len=80)                                :: Type
   character(len=80)                                :: Value
end type field_t

type fieldheader_t
   character(len=80)                                :: Type
   character(len=80)                                :: Value
end type fieldheader_t

type namedfields_t
   character(len=80)                                :: BaseName
   type(field_t), dimension(:), pointer            :: Field => null()
end type namedfields_t

type object_t
   integer                                         :: ClassNum
   character(len=80)                                :: Name
   type(fieldheader_t), dimension(:), pointer      :: Field => null()
   type(namedfields_t), dimension(:), pointer      :: NamedFields => null()
end type object_t
   type(object_t)                                  :: Object
contains
subroutine read_xml_type_field_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(field_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(field_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_field_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_field_t_array

subroutine read_xml_type_field_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(field_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_Name
   logical                                         :: has_Type
   logical                                         :: has_Value
   has_Name                             = .false.
   has_Type                             = .false.
   has_Value                            = .false.
   call init_xml_type_field_t(dvar)
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
      case('Name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Name, has_Name )
      case('Type')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Type, has_Type )
      case('Value')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Value, has_Value )
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
   if ( .not. has_Name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on Name')
   endif
   if ( .not. has_Type ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on Type')
   endif
   if ( .not. has_Value ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on Value')
   endif
end subroutine read_xml_type_field_t
subroutine init_xml_type_field_t_array( dvar )
   type(field_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_field_t_array
subroutine init_xml_type_field_t(dvar)
   type(field_t) :: dvar
end subroutine init_xml_type_field_t
subroutine read_xml_type_fieldheader_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(fieldheader_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(fieldheader_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_fieldheader_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_fieldheader_t_array

subroutine read_xml_type_fieldheader_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(fieldheader_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_Type
   logical                                         :: has_Value
   has_Type                             = .false.
   has_Value                            = .false.
   call init_xml_type_fieldheader_t(dvar)
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
      case('Type')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Type, has_Type )
      case('Value')
         call read_xml_line( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Value, has_Value )
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
   if ( .not. has_Type ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on Type')
   endif
   if ( .not. has_Value ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on Value')
   endif
end subroutine read_xml_type_fieldheader_t
subroutine init_xml_type_fieldheader_t_array( dvar )
   type(fieldheader_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_fieldheader_t_array
subroutine init_xml_type_fieldheader_t(dvar)
   type(fieldheader_t) :: dvar
end subroutine init_xml_type_fieldheader_t
subroutine read_xml_type_namedfields_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(namedfields_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(namedfields_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_namedfields_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_namedfields_t_array

subroutine read_xml_type_namedfields_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(namedfields_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_BaseName
   logical                                         :: has_Field
   has_BaseName                         = .false.
   has_Field                            = .false.
   allocate(dvar%Field(0))
   call init_xml_type_namedfields_t(dvar)
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
      case('BaseName')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%BaseName, has_BaseName )
      case('Field')
         call read_xml_type_field_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Field, has_Field )
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
   if ( .not. has_BaseName ) then
      dvar%BaseName = ""
   endif
end subroutine read_xml_type_namedfields_t
subroutine init_xml_type_namedfields_t_array( dvar )
   type(namedfields_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_namedfields_t_array
subroutine init_xml_type_namedfields_t(dvar)
   type(namedfields_t) :: dvar
end subroutine init_xml_type_namedfields_t
subroutine read_xml_type_object_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(object_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(object_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_object_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_object_t_array

subroutine read_xml_type_object_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(object_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_ClassNum
   logical                                         :: has_Name
   logical                                         :: has_Field
   logical                                         :: has_NamedFields
   has_ClassNum                         = .false.
   has_Name                             = .false.
   has_Field                            = .false.
   allocate(dvar%Field(0))
   has_NamedFields                      = .false.
   allocate(dvar%NamedFields(0))
   call init_xml_type_object_t(dvar)
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
      case('ClassNum')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%ClassNum, has_ClassNum )
      case('Name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Name, has_Name )
      case('Field')
         call read_xml_type_fieldheader_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Field, has_Field )
      case('NamedFields')
         call read_xml_type_namedfields_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%NamedFields, has_NamedFields )
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
   if ( .not. has_ClassNum ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on ClassNum')
   endif
   if ( .not. has_Name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on Name')
   endif
   if ( .not. has_Field ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on Field')
   endif
   if ( .not. has_NamedFields ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on NamedFields')
   endif
end subroutine read_xml_type_object_t
subroutine init_xml_type_object_t_array( dvar )
   type(object_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_object_t_array
subroutine init_xml_type_object_t(dvar)
   type(object_t) :: dvar
end subroutine init_xml_type_object_t
subroutine read_xml_file_xmltaxben_t(fname, lurep, errout,funit)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep
   logical, intent(out), optional         :: errout
   integer, intent(out), optional         :: funit

   type(XML_PARSE)                        :: info
   logical                                :: error
   character(len=80)                      :: tag
   character(len=80)                      :: starttag
   logical                                :: endtag
   character(len=255), dimension(1:2,1:20) :: attribs
   integer                                :: noattribs
   character(len=200), dimension(1:100)   :: data
   integer                                :: nodata
   logical                                         :: has_Object
   has_Object                           = .false.

   call init_xml_file_xmltaxben_t
   call xml_open( info, fname, .true. )
   if (present(funit)) funit = info%lun
   call xml_options( info, report_errors=.true., ignore_whitespace=.true., no_data_truncation =.true.)
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
   if ( starttag .ne. "DataStream_Id3726" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "DataStream_Id3726"')
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
      case('Object')
         call read_xml_type_object_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            Object, has_Object )
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
   if ( .not. has_Object ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on Object')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_xmltaxben_t(fname, lurep)
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
      '<DataStream_Id3726>'
   write(info%lun,'(a)') '</DataStream_Id3726>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_xmltaxben_t

end subroutine

end module
