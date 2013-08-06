!income tax
#undef   _$typelist
#define  _$typelist inctax
#include "/home/andrew/Dropbox/code/fortax-dev-test/includes/system/inctax.inc"

!national insurance
#undef   _$typelist
#define  _$typelist natins
#include "natins.inc"

!child benefit
#undef   _$typelist
#define  _$typelist chben
#include "includes/system/chben.inc"

!family credit/wftc
#undef   _$typelist
#define  _$typelist fc
#include "includes/system/fc.inc"

!child tax credit
#undef   _$typelist
#define  _$typelist ctc
#include "includes/system/ctc.inc"

!working tax credit
#undef   _$typelist
#define  _$typelist wtc
#include "includes/system/wtc.inc"

!new tax credits
#undef   _$typelist
#define  _$typelist ntc
#include "includes/system/ntc.inc"

!income support
#undef   _$typelist
#define  _$typelist incsup
#include "includes/system/incsup.inc"

!council tax
#undef   _$typelist
#define  _$typelist ctax
#include "includes/system/ctax.inc"

!rebate system (contains elements common to housing benefit and council tax)
#undef   _$typelist
#define  _$typelist rebatesys
#include "includes/system/rebatesys.inc"

!council tax benefit
#undef   _$typelist
#define  _$typelist ctaxben
#include "includes/system/ctaxben.inc"

!community charge benefit
#undef   _$typelist
#define  _$typelist ccben
#include "includes/system/ccben.inc"

!additional information
#undef   _$typelist
#define  _$typelist extra
#include "includes/system/extra.inc"

#undef   _$typelist