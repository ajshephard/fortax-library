#undef  __$quote(x)
#undef  _$quote(x)
#define __$quote(x) x
#define _$quote(x) __$quote(x)

#undef _$lab
#undef _$labn
#undef _$footer
#undef _$header

!define label type for all listed in lablist

#define _$lab(a,b,c) integer :: b
#define _$labn(a,b,c) integer :: b
#define _$header type :: _$quote(lab_)_$quote(_$typelist)_t
#define _$footer end type _$quote(lab_)_$quote(_$typelist)_t
#include "includes/label/lablist.inc"

#undef _$lab
#undef _$labn
#undef _$footer
#undef _$header

#define _$lab(a,b,c)
#define _$labn(a,b,c)
#define _$header type(_$quote(lab_)_$quote(_$typelist)_t) :: _$typelist
#define _$footer

!define lab_t which contains all label parts
type lab_t
#   include "includes/label/lablist.inc"
    integer :: reserved
end type lab_t

#undef _$lab
#undef _$labn
#undef _$footer
#undef _$header

#define _$lab(a,b,c) a, &
#define _$labn(a,b,c) a &
#define _$header _$quote(lab_)_$quote(_$typelist)_t( &
#define _$footer ), _$quote(&)

! create parameterised label
type(lab_t), parameter :: lab = lab_t( &
#   include "includes/label/lablist.inc"
    0)

#undef _$lab
#undef _$labn
#undef _$footer
#undef _$header