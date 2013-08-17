#undef _$name
#undef __$quote
#undef _$quote
#undef _$integer
#undef _$logical
#undef _$double
#undef _$integerarray
#undef _$logicalarray
#undef _$doublearray

#define _$name includes/_$typelist.inc
#define __$quote(x) #x
#define _$quote(x) __$quote(x)

#define _$integer(x,y)        call ftprint(funit,sys% _$typelist % x,#x)
#define _$logical(x,y)        call ftprint(funit,sys% _$typelist % x,#x)
#define _$double(x,y)         call ftprint(funit,sys% _$typelist % x,#x)
#define _$integerarray(x,y,z) call ftprint(funit,sys% _$typelist % x,#x)
#define _$logicalarray(x,y,z) call ftprint(funit,sys% _$typelist % x,#x)
#define _$doublearray(x,y,z)  call ftprint(funit,sys% _$typelist % x,#x)

#undef _$header
#undef _$footer

#define _$header write(funit,*); \
write(funit,'(/1X,(A))') upper(_$quote(_$typelist)//':')
#define _$footer

#include "system/syslist.inc"

#undef _$header
#undef _$footer 
#undef _$typelist
#undef _$name
#undef __$quote
#undef _$quote
#undef _$integer
#undef _$logical
#undef _$double
#undef _$integerarray
#undef _$logicalarray
#undef _$doublearray
