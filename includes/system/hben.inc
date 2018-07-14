_$header
#include "header.inc"

_$logical(doHBen,null)
_$double(taper,rate)
_$double(MinAmt,minamount)
_$logical(doUnderOccCharge,null)
_$logical(doUnderOccChargeInScotland,null)
_$logical(doUnderOccChargeInNI,null)
_$integer(numUnderOccBands,range)
_$doublearray(underOccRates,rate,:)
_$logical(doLHA,null)
_$integer(LHASharedAccAge,range)
_$integer(numLHABands,range)
_$doublearray(LHARates,rate,:)

_$footer
