#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray
        
#define _$logical(x,y)      if (y) call fortaxwarn('can''t uprate logical '//#x)
#define _$integer(x,y)      if (y) sys% _$typelist %x = factor*sys % _$typelist %x 
#define _$double(x,y)       if (y) sys% _$typelist %x = factor*sys % _$typelist %x 

#define _$logicalarray(x,y,z) if (y) call fortaxwarn('can''t uprate logicalarray '//#x)
#define _$integerarray(x,y,z) if (y) sys% _$typelist %x = factor*sys% _$typelist %x 
#define _$doublearray(x,y,z)  if (y) sys% _$typelist %x = factor*sys% _$typelist %x 

#define _$header
#define _$footer

#include "includes/system/syslist.inc"

#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray
