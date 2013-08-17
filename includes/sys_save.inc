#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray

#define __$append(x) x
#define _$append(x) __$append(x)

#undef __$makestr
#undef _$makestr
#define __$makestr(x) #x
#define _$makestr(x) __$makestr(x)

#define _$integer(x,y) write(funit,'(a)',advance="no") \
 "sys%"//_$makestr(_$typelist)//"%"//#x//"="; write(funit,'(a)') intToStr(sys% x)
#define _$double(x,y)  write(funit,'(a)',advance="no") \
 "sys%"//_$makestr(_$typelist)//"%"//#x//"="; write(funit,'(a)') dblToStr(sys% x)//'_dp'
#define _$logical(x,y) write(funit,'(a)',advance="no") \
 "sys%"//_$makestr(_$typelist)//"%"//#x//"="; write(funit,'(a)') merge('.true. ','.false.',sys% x)

#define _$doublearray(x,y,z)  \
 write(funit,'(a)') "allocate(sys%"//_$makestr(_$typelist)//"%"//#x//"("//intToStr(size(sys%x))//"))"; \
 do i=1,size(sys%x); write(funit,'(a)',advance="no") \
 "sys%"//_$makestr(_$typelist)//"%"//#x//"("//intToStr(i)//")="; write(funit,'(a)') dblToStr(sys% x(i))//'_dp'; end do

#define _$integerarray(x,y,z) \
 write(funit,'(a)') "allocate(sys%"//_$makestr(_$typelist)//"%"//#x//"("//intToStr(size(sys%x))//"))"; \
 do i=1,size(sys%x); write(funit,'(a)',advance="no") \
 "sys%"//_$makestr(_$typelist)//"%"//#x//"("//intToStr(i)//")="; write(funit,'(a)') intToStr(sys% x(i)); end do

#define _$logicalarray(x,y,z) \
 write(funit,'(a)') "allocate(sys%"//_$makestr(_$typelist)//"%"//#x//"("//intToStr(size(sys%x))//"))"; \
 do i=1,size(sys%x); write(funit,'(a)',advance="no") \
 "sys%"//_$makestr(_$typelist)//"%"//#x//"("//intToStr(i)//")="; write(funit,'(a)') merge('.true. ','.false.',sys% x(i)); end do

#define _$header subroutine _$append(_$typelist)_save(sys,funit); \
use fortax_util, only : intToStr, dblToStr; \
implicit none; \
type(_$append(_$typelist)_t), intent(in) :: sys; \
integer, intent(in) :: funit; integer :: i; \
write(funit,*); write(funit,'(a)') "!"//_$makestr(_$typelist)
#define _$footer end subroutine _$append(_$typelist)_save

#include "system/syslist.inc"

#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray
