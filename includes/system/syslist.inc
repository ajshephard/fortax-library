!income tax
#undef   _$typelist
#define  _$typelist inctax
#include "inctax.inc"

!national insurance
#undef   _$typelist
#define  _$typelist natins
#include "natins.inc"

!child benefit
#undef   _$typelist
#define  _$typelist chben
#include "chben.inc"

!family credit/wftc
#undef   _$typelist
#define  _$typelist fc
#include "fc.inc"

!child tax credit
#undef   _$typelist
#define  _$typelist ctc
#include "ctc.inc"

!working tax credit
#undef   _$typelist
#define  _$typelist wtc
#include "wtc.inc"

!new tax credits
#undef   _$typelist
#define  _$typelist ntc
#include "ntc.inc"

!tax refund for childcare spending
#undef   _$typelist
#define  _$typelist cctaxrefund
#include "cctaxrefund.inc"

!income support
#undef   _$typelist
#define  _$typelist incsup
#include "incsup.inc"

!council tax
#undef   _$typelist
#define  _$typelist ctax
#include "ctax.inc"

!rebate system (contains elements common to housing benefit and council tax)
#undef   _$typelist
#define  _$typelist rebatesys
#include "rebatesys.inc"

!housing benefit
#undef   _$typelist
#define  _$typelist hben
#include "hben.inc"

!council tax benefit
#undef   _$typelist
#define  _$typelist ctaxben
#include "ctaxben.inc"

!community charge benefit
#undef   _$typelist
#define  _$typelist ccben
#include "ccben.inc"

!universal credit
#undef   _$typelist
#define  _$typelist uc
#include "uc.inc"

!state pension
#undef   _$typelist
#define  _$typelist statepen
#include "statepen.inc"

!benefit cap
#undef   _$typelist
#define  _$typelist bencap
#include "bencap.inc"

!additional information
#undef   _$typelist
#define  _$typelist extra
#include "extra.inc"

#undef   _$typelist
