#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray
        
#define _$logical(x,y)      if (y) print *, 'warning: logical can''t be minamt: '//#x        
#define _$integer(x,y)      if (y) sys% _$typelist %x = minamt
#define _$double(x,y)       if (y) sys% _$typelist %x = minamt

#define _$logicalarray(x,y,z) if (y) print *, 'warning: logicalarray can''t be minamt: '//#x        
#define _$integerarray(x,y,z) if (y) sys% _$typelist %x = minamt 
#define _$doublearray(x,y,z)  if (y) sys% _$typelist %x = minamt

#define _$header
#define _$footer

#include "system/syslist.inc"

#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray
