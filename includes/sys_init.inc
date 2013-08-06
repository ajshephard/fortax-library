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
     
#define __$append(x) x
#define _$append(x) __$append(x)

#define _$header subroutine _$append(_$typelist)_init(syst); \
implicit none; \
type(_$append(_$typelist)_t), intent(inout) :: syst
#define _$footer end subroutine _$append(_$typelist)_init

#include 'includes/system/syslist.inc'

#undef  _$header
#undef  _$footer
#undef  _$integer
#undef  _$double
#undef  _$logical
#undef  _$doublearray
#undef  _$integerarray
#undef  _$logicalarray
