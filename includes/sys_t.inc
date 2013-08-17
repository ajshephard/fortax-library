#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray

#define _$integer(x,y) integer  :: x
#define _$double(x,y)  real(dp) :: x
#define _$logical(x,y) logical  :: x

#define _$doublearray(x,y,z)  real(dp), allocatable, dimension(z) :: x
#define _$integerarray(x,y,z) integer,  allocatable, dimension(z) :: x
#define _$logicalarray(x,y,z) logical,  allocatable, dimension(z) :: x

#define concat(x, y) concat_(x, y)
#define concat_(x, y) x ## y

#define _$header type :: concat(_$typelist,_t)
#define _$footer end type concat(_$typelist,_t)

#include "system/syslist.inc"

#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray

#define _$header type(concat(_$typelist,_t)) :: _$typelist
#define _$footer
#define _$integer(x,y)
#define _$double(x,y)
#define _$logical(x,y)
#define _$integerarray(x,y,z)
#define _$doublearray(x,y,z)
#define _$logicalarray(x,y,z)

type :: sys_t
character(len=40) :: sysname
character(len=80) :: sysdesc
#include "system/syslist.inc"
end type

#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray