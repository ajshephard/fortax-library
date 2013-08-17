#undef  __$quote
#undef  _$quote
#define __$quote(x) x
#define _$quote(x) __$quote(x)

#undef _$lab
#undef _$labn
#undef _$footer
#undef _$header

#define concat(x, y) concat_(x, y)
#define concat_(x, y) x ## y

#define concat3(x, y, z) concat3_(x, y, z)
#define concat3_(x, y, z) x ## y ## z

!define label type for all listed in lablist

#define _$lab(a,b,c) integer :: b
#define _$labn(a,b,c) integer :: b
#define _$header type :: concat3(lab_, _$typelist, _t)
#define _$footer end type concat3(lab_, _$typelist, _t)

#include "label/lablist.inc"

#undef _$lab
#undef _$labn
#undef _$footer
#undef _$header

#define _$lab(a,b,c)
#define _$labn(a,b,c)
#define _$header type(concat3(lab_,_$typelist,_t)) :: _$typelist
#define _$footer

!define lab_t which contains all label parts
type lab_t
#	include "label/lablist.inc"
    integer :: reserved
end type lab_t

#undef _$lab
#undef _$labn
#undef _$footer
#undef _$header

#define _$lab(a,b,c) a, &
#define _$labn(a,b,c) a &
#define _$header concat3(lab_,_$typelist,_t) ( &
#define _$footer ), _$quote(&)

! create parameterised label
type(lab_t), parameter :: lab = lab_t( &
#	include "label/lablist.inc"
    0)

#undef _$lab
#undef _$labn
#undef _$footer
#undef _$header
