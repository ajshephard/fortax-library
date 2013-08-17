#undef _$header
#undef _$footer
#undef __$quote
#undef _$quote
#undef _$integer
#undef _$logical
#undef _$double
#undef _$integerarray
#undef _$logicalarray
#undef _$doublearray

#define __$quote(x) #x
#define _$quote(x) __$quote(x)

#define _$integer(x,y)        call ftxmlwrite(info,sys% _$typelist % x,#x)
#define _$logical(x,y)        call ftxmlwrite(info,sys% _$typelist % x,#x)
#define _$double(x,y)         call ftxmlwrite(info,sys% _$typelist % x,#x)
#define _$integerarray(x,y,z) call ftxmlwrite(info,sys% _$typelist % x,#x)
#define _$logicalarray(x,y,z) call ftxmlwrite(info,sys% _$typelist % x,#x)
#define _$doublearray(x,y,z)  call ftxmlwrite(info,sys% _$typelist % x,#x)

#define _$header attribs(2,1) = _$quote(_$typelist); \
call xml_ftag(info,'system','open',attribs)
#define _$footer call xml_ftag(info,'system','close')

#include "includes/system/syslist.inc"

#undef _$header
#undef _$footer
#undef __$quote
#undef _$quote
#undef _$integer
#undef _$logical
#undef _$double
#undef _$integerarray
#undef _$logicalarray
#undef _$doublearray
