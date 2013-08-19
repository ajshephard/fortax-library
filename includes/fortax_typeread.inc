#undef _$header
#undef _$footer
#undef __$quote
#undef _$quote
#undef _$integer
#undef _$double
#undef _$logical
#undef _$integerarray
#undef _$doublearray
#undef _$logicalarray

#define __$quote(x) #x
#define _$quote(x) __$quote(x)

#define _$integer(x,y)
#define _$double(x,y)
#define _$logical(x,y)
#define _$integerarray(x,y,z)
#define _$doublearray(x,y,z)
#define _$logicalarray(x,y,z)

#define _$footer end do

#undef  _$integer
#define _$integer(x,y) if (system(i)%finteger(j)%name==#x) call fr(sys%_$typelist%x,system(i)%finteger(j)%value)
#undef  _$header
#define _$header case(_$quote(_$typelist)); do j = 1, size(system(i)%finteger)
#include "fortax_read.inc"
#undef _$integer
#define _$integer(x,y)

#undef  _$double
#define _$double(x,y) if (system(i)%fdouble(j)%name==#x) call fr(sys%_$typelist%x,system(i)%fdouble(j)%value)
#undef  _$header
#define _$header case(_$quote(_$typelist)); do j = 1, size(system(i)%fdouble)
#include "fortax_read.inc"
#undef _$double
#define _$double(x,y)

#undef  _$logical
#define _$logical(x,y) if (system(i)%flogical(j)%name==#x) call fr(sys%_$typelist%x,system(i)%flogical(j)%value)
#undef  _$header
#define _$header case(_$quote(_$typelist)); do j = 1, size(system(i)%flogical)
#include "fortax_read.inc"
#undef _$logical
#define _$logical(x,y)

#undef  _$integerarray
#define _$integerarray(x,y,z) if (system(i)%fintegerarray(j)%name==#x) call fr(sys%_$typelist%x,system(i)%fintegerarray(j)%value)
#undef  _$header
#define _$header case(_$quote(_$typelist)); do j = 1, size(system(i)%fintegerarray)
#include "fortax_read.inc"
#undef _$integerarray
#define _$integerarray(x,y,z)

#undef  _$doublearray
#define _$doublearray(x,y,z) if (system(i)%fdoublearray(j)%name==#x) call fr(sys%_$typelist%x,system(i)%fdoublearray(j)%value)
#undef  _$header
#define _$header case(_$quote(_$typelist)); do j = 1, size(system(i)%fdoublearray)
#include "fortax_read.inc"
#undef _$doublearray
#define _$doublearray(x,y,z)

#undef  _$logicalarray
#define _$logicalarray(x,y,z) if (system(i)%flogicalarray(j)%name==#x) call fr(sys%_$typelist%x,system(i)%flogicalarray(j)%value)
#undef  _$header
#define _$header case(_$quote(_$typelist)); do j = 1, size(system(i)%flogicalarray)
#include "fortax_read.inc"
#undef _$logicalarray
#define _$logicalarray(x,y,z)

#undef _$header
#undef _$footer
#undef _$integer
#undef _$double
#undef _$logical
#undef _$integerarray
#undef _$doublearray
#undef _$logicalarray
