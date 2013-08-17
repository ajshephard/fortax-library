#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray

#define _$integer(x,y) syst% x = 0
#define _$double(x,y)  syst% x = 0.0_dp
#define _$logical(x,y) syst% x = .false.

#define _$doublearray(x,y,z)  if (allocated(syst% x )) deallocate(syst% x )
#define _$integerarray(x,y,z) if (allocated(syst% x )) deallocate(syst% x )
#define _$logicalarray(x,y,z) if (allocated(syst% x )) deallocate(syst% x )

#define concat(x, y) concat_(x, y)
#define concat_(x, y) x ## y

#define concat3(x, y, z) concat3_(x, y, z)
#define concat3_(x, y, z) x ## y ## z

#define _$header subroutine concat(_$typelist,_init(syst)); \
implicit none; \
type(concat(_$typelist,_t)), intent(inout) :: syst
#define _$footer end subroutine concat(_$typelist,_init)

#include "system/syslist.inc"

#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray
