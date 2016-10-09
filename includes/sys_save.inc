#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray

#undef __$makestr
#undef _$makestr
#define __$makestr(x) #x
#define _$makestr(x) __$makestr(x)

#define _$integer(x,y) call sys_saveInteger(_$makestr(_$typelist),#x,funit,sys%x)
#define _$double(x,y)  call sys_saveDouble(_$makestr(_$typelist),#x,funit,sys%x)
#define _$logical(x,y) call sys_saveLogical(_$makestr(_$typelist),#x,funit,sys%x)

#define _$doublearray(x,y,z) call sys_saveDoubleArray(_$makestr(_$typelist),#x,funit,sys%x)
#define _$integerarray(x,y,z) call sys_saveIntegerArray(_$makestr(_$typelist),#x,funit,sys%x)
#define _$logicalarray(x,y,z) call sys_saveLogicalArray(_$makestr(_$typelist),#x,funit,sys%x)

#define _$header
#define _$headerinc sys_saveheader.inc
#define _$footer end subroutine concat(_$typelist,_save)

#include "system/syslist.inc"

#undef  _$headerinc
#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray
