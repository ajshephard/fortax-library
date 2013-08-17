#undef _$header
#undef _$footer
#undef _$integer
#undef _$double
#undef _$logical
#undef _$integerarray
#undef _$doublearray
#undef _$logicalarray

#define _$integer(x,y)
#define _$double(x,y)
#define _$logical(x,y)
#define _$integerarray(x,y,z)
#define _$doublearray(x,y,z)
#define _$logicalarray(x,y,z)

#define _$footer end do

#undef  _$integer
#define _$integer(x,y) if (family(i)%fam%finteger(j)%name==#x) call fortax_read_assign(fam(i)%x,family(i)%fam%finteger(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%finteger)
#include "fam_t.inc"
#undef _$integer
#define _$integer(x,y)

#undef  _$double
#define _$double(x,y) if (family(i)%fam%fdouble(j)%name==#x) call fortax_read_assign(fam(i)%x,family(i)%fam%fdouble(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%fdouble)
#include "fam_t.inc"
#undef _$double
#define _$double(x,y)

#undef  _$logical
#define _$logical(x,y) if (family(i)%fam%flogical(j)%name==#x) call fortax_read_assign(fam(i)%x,family(i)%fam%flogical(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%flogical)
#include "fam_t.inc"
#undef _$logical
#define _$logical(x,y)

#undef  _$integerarray
#define _$integerarray(x,y,z) if (family(i)%fam%fintegerarray(j)%name==#x) call fortax_read_assign(fam(i)%x,family(i)%fam%fintegerarray(j)%value,size(fam(i)%x))
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%fintegerarray)
#include "fam_t.inc"
#undef _$integerarray
#define _$integerarray(x,y,z)

#undef  _$doublearray
#define _$doublearray(x,y,z) if (family(i)%fam%fdoublearray(j)%name==#x) call fortax_read_assign(fam(i)%x,family(i)%fam%fdoublearray(j)%value,size(fam(i)%x))
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%fdoublearray)
#include "fam_t.inc"
#undef _$doublearray
#define _$doublearray(x,y,z)

#undef  _$logicalarray
#define _$logicalarray(x,y,z) if (family(i)%fam%flogicalarray(j)%name==#x) call fortax_read_assign(fam(i)%x,family(i)%fam%flogicalarray(j)%value,size(fam(i)%x))
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%flogicalarray)
#include "fam_t.inc"
#undef _$logicalarray
#define _$logicalarray(x,y,z)



#undef  _$integer
#define _$integer(x,y) if (family(i)%fam%ad1%finteger(j)%name==#x) call fortax_read_assign(fam(i)%ad(1)%x,family(i)%fam%ad1%finteger(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%ad1%finteger)
#include "famad_t.inc"
#undef _$integer
#define _$integer(x,y)

#undef  _$double
#define _$double(x,y) if (family(i)%fam%ad1%fdouble(j)%name==#x) call fortax_read_assign(fam(i)%ad(1)%x,family(i)%fam%ad1%fdouble(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%ad1%fdouble)
#include "famad_t.inc"
#undef _$double
#define _$double(x,y)

#undef  _$logical
#define _$logical(x,y) if (family(i)%fam%ad1%flogical(j)%name==#x) call fortax_read_assign(fam(i)%ad(1)%x,family(i)%fam%ad1%flogical(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%ad1%flogical)
#include "famad_t.inc"
#undef _$logical
#define _$logical(x,y)

#undef  _$integerarray
#define _$integerarray(x,y,z) if (family(i)%fam%ad1%fintegerarray(j)%name==#x) call fortax_read_assign(fam(i)%ad(1)%x,family(i)%fam%ad1%fintegerarray(j)%value,size(fam(i)%ad(1)%x))
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%ad1%fintegerarray)
#include "famad_t.inc"
#undef _$integerarray
#define _$integerarray(x,y,z)

#undef  _$doublearray
#define _$doublearray(x,y,z) if (family(i)%fam%ad1%fdoublearray(j)%name==#x) call fortax_read_assign(fam(i)%ad(1)%x,family(i)%fam%ad1%fdoublearray(j)%value,size(fam(i)%ad(1)%x))
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%ad1%fdoublearray)
#include "famad_t.inc"
#undef _$doublearray
#define _$doublearray(x,y,z)

#undef  _$logicalarray
#define _$logicalarray(x,y,z) if (family(i)%fam%ad1%flogicalarray(j)%name==#x) call fortax_read_assign(fam(i)%ad(1)%x,family(i)%fam%ad1%flogicalarray(j)%value,size(fam(i)%ad(1)%x))
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%ad1%flogicalarray)
#include "famad_t.inc"
#undef _$logicalarray
#define _$logicalarray(x,y,z)



#undef  _$integer
#define _$integer(x,y) if (family(i)%fam%ad2%finteger(j)%name==#x) call fortax_read_assign(fam(i)%ad(2)%x,family(i)%fam%ad2%finteger(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%ad2%finteger)
#include "famad_t.inc"
#undef _$integer
#define _$integer(x,y)

#undef  _$double
#define _$double(x,y) if (family(i)%fam%ad2%fdouble(j)%name==#x) call fortax_read_assign(fam(i)%ad(2)%x,family(i)%fam%ad2%fdouble(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%ad2%fdouble)
#include "famad_t.inc"
#undef _$double
#define _$double(x,y)

#undef  _$logical
#define _$logical(x,y) if (family(i)%fam%ad2%flogical(j)%name==#x) call fortax_read_assign(fam(i)%ad(2)%x,family(i)%fam%ad2%flogical(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%ad2%flogical)
#include "famad_t.inc"
#undef _$logical
#define _$logical(x,y)

#undef  _$integerarray
#define _$integerarray(x,y,z) if (family(i)%fam%ad2%fintegerarray(j)%name==#x) call fortax_read_assign(fam(i)%ad(2)%x,family(i)%fam%ad2%fintegerarray(j)%value,size(fam(i)%ad(2)%x))
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%ad2%fintegerarray)
#include "famad_t.inc"
#undef _$integerarray
#define _$integerarray(x,y,z)

#undef  _$doublearray
#define _$doublearray(x,y,z) if (family(i)%fam%ad2%fdoublearray(j)%name==#x) call fortax_read_assign(fam(i)%ad(2)%x,family(i)%fam%ad2%fdoublearray(j)%value,size(fam(i)%ad(2)%x))
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%ad2%fdoublearray)
#include "famad_t.inc"
#undef _$doublearray
#define _$doublearray(x,y,z)

#undef  _$logicalarray
#define _$logicalarray(x,y,z) if (family(i)%fam%ad2%flogicalarray(j)%name==#x) call fortax_read_assign(fam(i)%ad(2)%x,family(i)%fam%ad2%flogicalarray(j)%value,size(fam(i)%ad(2)%x))
#undef  _$header
#define _$header do j = 1, size(family(i)%fam%ad2%flogicalarray)
#include "famad_t.inc"
#undef _$logicalarray
#define _$logicalarray(x,y,z)

sysname(i) = family(i)%net%system

#undef _$integer
#undef _$double
#undef _$logical

#define _$integer(x,y,z)
#define _$double(x,y,z)
#define _$logical(x,y,z)

#undef  _$integer
#define _$integer(x,y,z) if (family(i)%net%nettu%finteger(j)%name==#x) call fortax_read_assign(net(i)%tu%x,family(i)%net%nettu%finteger(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%net%nettu%finteger)
#include "nettu_t.inc"
#undef _$integer
#define _$integer(x,y,z)

#undef  _$double
#define _$double(x,y,z) if (family(i)%net%nettu%fdouble(j)%name==#x) call fortax_read_assign(net(i)%tu%x,family(i)%net%nettu%fdouble(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%net%nettu%fdouble)
#include "nettu_t.inc"
#undef _$double
#define _$double(x,y,z)

#undef  _$logical
#define _$logical(x,y,z) if (family(i)%net%nettu%flogical(j)%name==#x) call fortax_read_assign(net(i)%tu%x,family(i)%net%nettu%flogical(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%net%nettu%flogical)
#include "nettu_t.inc"
#undef _$logical
#define _$logical(x,y,z)

#undef  _$integer
#define _$integer(x,y,z) if (family(i)%net%netad1%finteger(j)%name==#x) call fortax_read_assign(net(i)%ad(1)%x,family(i)%net%netad1%finteger(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%net%netad1%finteger)
#include "netad_t.inc"
#undef _$integer
#define _$integer(x,y,z)

#undef  _$double
#define _$double(x,y,z) if (family(i)%net%netad1%fdouble(j)%name==#x) call fortax_read_assign(net(i)%ad(1)%x,family(i)%net%netad1%fdouble(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%net%netad1%fdouble)
#include "netad_t.inc"
#undef _$double
#define _$double(x,y,z)

#undef  _$logical
#define _$logical(x,y,z) if (family(i)%net%netad1%flogical(j)%name==#x) call fortax_read_assign(net(i)%ad(1)%x,family(i)%net%netad1%flogical(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%net%netad1%flogical)
#include "netad_t.inc"
#undef _$logical
#define _$logical(x,y,z)

#undef  _$integer
#define _$integer(x,y,z) if (family(i)%net%netad2%finteger(j)%name==#x) call fortax_read_assign(net(i)%ad(2)%x,family(i)%net%netad2%finteger(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%net%netad2%finteger)
#include "netad_t.inc"
#undef _$integer
#define _$integer(x,y,z)

#undef  _$double
#define _$double(x,y,z) if (family(i)%net%netad2%fdouble(j)%name==#x) call fortax_read_assign(net(i)%ad(2)%x,family(i)%net%netad2%fdouble(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%net%netad2%fdouble)
#include "netad_t.inc"
#undef _$double
#define _$double(x,y,z)

#undef  _$logical
#define _$logical(x,y,z) if (family(i)%net%netad2%flogical(j)%name==#x) call fortax_read_assign(net(i)%ad(2)%x,family(i)%net%netad2%flogical(j)%value)
#undef  _$header
#define _$header do j = 1, size(family(i)%net%netad2%flogical)
#include "netad_t.inc"
#undef _$logical
#define _$logical(x,y,z)

#undef _$header
#undef _$footer
#undef _$integer
#undef _$double
#undef _$logical
#undef _$integerarray
#undef _$doublearray
#undef _$logicalarray