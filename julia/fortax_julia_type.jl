
const maxKids = 16
const maxKidAge = 19
const maxRpi = 1024
const maxSysIndex = 128
const maxNumAgeRng = 32
const maxIncTaxBands = 32
const maxNatInsBands = 32
const maxNatInsC4Bands = 32
const maxKinks = 256
const maxUnderOccBands = 8
const maxLHABands = 8
const sysHuge = 1.0e100
const len_sysname = 64
const len_sysdesc = 512
const len_sysindex = 256
const len_label = 16
const len_labstring = 64
const len_bcdesc = 256
const lab_bool = (no = 0, yes = 1)
const lab_ctax = (
    banda = 1,
    bandb = 2,
    bandc = 3,
    bandd = 4,
    bande = 5,
    bandf = 6,
    bandg = 7,
    bandh = 8,
    bandi = 9,
)
const lab_tenure = (
    own_outright = 1,
    mortgage = 2,
    part_own = 3,
    social_renter = 4,
    private_renter = 5,
    rent_free = 6,
    other = 7,
)
const lab_region = (
    north_east = 1,
    north_west = 2,
    yorks = 3,
    east_midlands = 4,
    west_midlands = 5,
    eastern = 6,
    london = 7,
    south_east = 8,
    south_west = 9,
    wales = 10,
    scotland = 11,
    northern_ireland = 12,
)
const lab_sex = (male = 0, female = 1)
const lab_famtype = (single_nokids = 1, single_kids = 2, couple_nokids = 3, couple_kids = 4)

struct famad_t
    age::Cint
    selfemp::Cint
    hrs::Cdouble
    earn::Cdouble
end

struct fam_t
    couple::Cint
    married::Cint
    ccexp::Cdouble
    maint::Cdouble
    nkids::Cint
    kidage::SVector{maxKids,Cint}
    kidsex::SVector{maxKids,Cint}
    nothads::Cint
    tenure::Cint
    rent::Cdouble
    rentcap::Cdouble
    bedrooms::Cint
    region::Cint
    ctband::Cint
    banddratio::Cdouble
    intdate::Cint
    famtype::Cint
    yngkid::Cint
    kidagedist::SVector{21,Cint}
    kidagedist0::SVector{21,Cint}
    kidagedist1::SVector{21,Cint}
    ad::SVector{2,famad_t}
end

struct netad_t
    taxable::Cdouble
    inctax::Cdouble
    natins::Cdouble
    natinsc1::Cdouble
    natinsc2::Cdouble
    natinsc4::Cdouble
    pretaxearn::Cdouble
    posttaxearn::Cdouble
end

struct nettu_t
    pretaxearn::Cdouble
    posttaxearn::Cdouble
    chben::Cdouble
    matgrant::Cdouble
    fc::Cdouble
    wtc::Cdouble
    ctc::Cdouble
    ccexp::Cdouble
    cctaxrefund::Cdouble
    incsup::Cdouble
    hben::Cdouble
    polltax::Cdouble
    polltaxben::Cdouble
    ctax::Cdouble
    ctaxben::Cdouble
    maxuc::Cdouble
    uc::Cdouble
    dispinc::Cdouble
    pretax::Cdouble
    nettax::Cdouble
    chcaresub::Cdouble
    fsm::Cdouble
    totben::Cdouble
end

struct net_t
    ad::SVector{2,netad_t}
    tu::nettu_t
end

struct rpi_t
    ndate::Cint
    date::SVector{maxRpi,Cint}
    index::SVector{maxRpi,Cdouble}
end

struct sysindex_t
    nsys::Cint
    date0::SVector{maxSysIndex,Cint}
    date1::SVector{maxSysIndex,Cint}
    index::SMatrix{len_sysindex,maxSysIndex,Cchar}
end

struct inctax_t
    numbands::Cint
    pa::Cdouble
    doPATaper::Cint
    disablePATaperRounding::Cint
    paTaperThresh::Cdouble
    paTaperRate::Cdouble
    doTPA::Cint
    maxTPA::Cdouble
    mma::Cdouble
    ctc::Cdouble
    ctcyng::Cdouble
    mmarate::Cdouble
    ctctaper::Cdouble
    c4rebate::Cdouble
    bands::SVector{maxIncTaxBands,Cdouble}
    rates::SVector{maxIncTaxBands,Cdouble}
end

struct natins_t
    numrates::Cint
    c4nrates::Cint
    c2floor::Cdouble
    c2rate::Cdouble
    ceiling::Cdouble
    rates::SVector{maxNatInsBands,Cdouble}
    bands::SVector{maxNatInsBands,Cdouble}
    c4rates::SVector{maxNatInsC4Bands,Cdouble}
    c4bands::SVector{maxNatInsC4Bands,Cdouble}
end

struct chben_t
    doChBen::Cint
    basic::Cdouble
    kid1xtr::Cdouble
    opf::Cdouble
    MatGrantVal::Cdouble
    MatGrantOnlyFirstKid::Cint
    doTaper::Cint
    disableTaperRounding::Cint
    taperStart::Cdouble
    taperRate::Cdouble
    taperIsIncTax::Cint
end

struct fc_t
    dofamcred::Cint
    NumAgeRng::Cint
    MaxAgeCC::Cint
    WFTCMaxAgeCC::Cint
    adult::Cdouble
    ftprem::Cdouble
    hours1::Cdouble
    hours2::Cdouble
    thres::Cdouble
    taper::Cdouble
    MaintDisreg::Cdouble
    MaxCC1::Cdouble
    MaxCC2::Cdouble
    WFTCMaxCC1::Cdouble
    WFTCMaxCC2::Cdouble
    WFTCPropCC::Cdouble
    MinAmt::Cdouble
    kidagel::SVector{maxNumAgeRng,Cint}
    kidageu::SVector{maxNumAgeRng,Cint}
    kidcred::SVector{maxNumAgeRng,Cdouble}
end

struct ctc_t
    fam::Cdouble
    baby::Cdouble
    kid::Cdouble
    maxKids::Cint
end

struct wtc_t
    Basic::Cdouble
    CouLP::Cdouble
    FT::Cdouble
    MinHrsKids::Cdouble
    MinHrsCouKids::Cdouble
    MinHrsNoKids::Cdouble
    FTHrs::Cdouble
    MinAgeKids::Cint
    MinAgeNoKids::Cint
    MaxCC1::Cdouble
    MaxCC2::Cdouble
    PropCC::Cdouble
    MaxAgeCC::Cint
    NewDisreg::Cdouble
    NewDisregCon::Cint
end

struct ntc_t
    donewtaxcred::Cint
    thr1lo::Cdouble
    thr1hi::Cdouble
    thr2::Cdouble
    taper1::Cdouble
    taper2::Cdouble
    taperCTCInOneGo::Cint
    MinAmt::Cdouble
end

struct cctaxrefund_t
    doCCTaxRefund::Cint
    MaxPerChild::Cdouble
    MaxAge::Cint
    ReceiptProp::Cdouble
    MinEarn::Cdouble
    MaxInc::Cdouble
end

struct incsup_t
    doIncSup::Cint
    IncChben::Cint
    NumAgeRng::Cint
    MinAgeMain::Cint
    MinAgeMainSin::Cint
    MainCou::Cdouble
    YngCou::Cdouble
    MainLP::Cdouble
    YngLP::Cdouble
    MainSin::Cdouble
    YngSin::Cdouble
    MinAgeFSM::Cint
    MaxAgeUniversalFSM::Cint
    ValFSM::Cdouble
    DisregLP::Cdouble
    DisregSin::Cdouble
    DisregCou::Cdouble
    DisregShared::Cint
    PremFam::Cdouble
    PremLP::Cdouble
    hours::Cdouble
    MaintDisreg::Cdouble
    AgeRngl::SVector{maxNumAgeRng,Cint}
    AgeRngu::SVector{maxNumAgeRng,Cint}
    AddKid::SVector{maxNumAgeRng,Cdouble}
end

struct ctax_t
    docounciltax::Cint
    bandD::Cdouble
    SinDis::Cdouble
    EnglandRatioA::Cdouble
    EnglandRatioB::Cdouble
    EnglandRatioC::Cdouble
    EnglandRatioE::Cdouble
    EnglandRatioF::Cdouble
    EnglandRatioG::Cdouble
    EnglandRatioH::Cdouble
    EnglandRatioI::Cdouble
    ScotlandRatioA::Cdouble
    ScotlandRatioB::Cdouble
    ScotlandRatioC::Cdouble
    ScotlandRatioE::Cdouble
    ScotlandRatioF::Cdouble
    ScotlandRatioG::Cdouble
    ScotlandRatioH::Cdouble
    ScotlandRatioI::Cdouble
    WalesRatioA::Cdouble
    WalesRatioB::Cdouble
    WalesRatioC::Cdouble
    WalesRatioE::Cdouble
    WalesRatioF::Cdouble
    WalesRatioG::Cdouble
    WalesRatioH::Cdouble
    WalesRatioI::Cdouble
end

struct rebatesys_t
    RulesUnderFC::Cint
    RulesUnderWFTC::Cint
    RulesUnderNTC::Cint
    RulesUnderUC::Cint
    NumAgeRng::Cint
    Restrict::Cint
    docap::Cint
    MinAgeMain::Cint
    MinAgeMainSin::Cint
    MainCou::Cdouble
    YngCou::Cdouble
    MainLP::Cdouble
    YngLP::Cdouble
    MainSin::Cdouble
    YngSin::Cdouble
    DisregSin::Cdouble
    DisregLP::Cdouble
    DisregCou::Cdouble
    CredInDisregCC::Cint
    ChbenIsIncome::Cint
    PremFam::Cdouble
    PremLP::Cdouble
    MaintDisreg::Cdouble
    MaxCC1::Cdouble
    MaxCC2::Cdouble
    MaxAgeCC::Cint
    AgeRngl::SVector{maxNumAgeRng,Cint}
    AgeRngu::SVector{maxNumAgeRng,Cint}
    AddKid::SVector{maxNumAgeRng,Cdouble}
    MaxKids::Cint
end

struct hben_t
    doHBen::Cint
    taper::Cdouble
    MinAmt::Cdouble
    doUnderOccCharge::Cint
    doUnderOccChargeScotland::Cint
    doUnderOccChargeNI::Cint
    numUnderOccBands::Cint
    underOccRates::SVector{maxUnderOccBands,Cdouble}
    doLHA::Cint
    LHASharedAccAge::Cint
    numLHABands::Cint
    LHARates::SVector{maxLHABands,Cdouble}
end

struct ctaxben_t
    docounciltaxben::Cint
    taper::Cdouble
    doEntitlementCut::Cint
    entitlementShare::Cdouble
end

struct ccben_t
    dopolltax::Cint
    taper::Cdouble
    PropElig::Cdouble
    MinAmt::Cdouble
    CCrate::Cdouble
end

struct uc_t
    doUnivCred::Cint
    MainCou::Cdouble
    YngCou::Cdouble
    MainSin::Cdouble
    YngSin::Cdouble
    MinAgeMain::Cint
    FirstKid::Cdouble
    OtherKid::Cdouble
    MaxKids::Cint
    MaxCC1::Cdouble
    MaxCC2::Cdouble
    PropCC::Cdouble
    MaxAgeCC::Cint
    doRentCap::Cint
    DisregSinNoKidsHi::Cdouble
    DisregSinNoKidsLo::Cdouble
    DisregSinKidsHi::Cdouble
    DisregSinKidsLo::Cdouble
    DisregCouNoKidsHi::Cdouble
    DisregCouNoKidsLo::Cdouble
    DisregCouKidsHi::Cdouble
    DisregCouKidsLo::Cdouble
    taper::Cdouble
    MinAmt::Cdouble
end

struct statepen_t
    doStatePen::Cint
    PenAgeMan::Cint
    PenAgeWoman::Cint
end

struct bencap_t
    doCap::Cint
    doNI::Cint
    doThruUC::Cint
    sinNoKids::Cdouble
    sinKids::Cdouble
    couNoKids::Cdouble
    couKids::Cdouble
    LondonCapAmt::Cint
    LondonSinNoKids::Cdouble
    LondonSinKids::Cdouble
    LondonCouNoKids::Cdouble
    LondonCouKids::Cdouble
    UCEarnThr::Cdouble
end

struct extra_t
    fsminappamt::Cint
    matgrant::Cint
    prices::Cint
end

struct sys_t
    sysname::SVector{len_sysname,Cchar}
    sysdesc::SVector{len_sysdesc,Cchar}
    inctax::inctax_t
    natins::natins_t
    chben::chben_t
    fc::fc_t
    ctc::ctc_t
    wtc::wtc_t
    ntc::ntc_t
    cctaxrefund::cctaxrefund_t
    incsup::incsup_t
    ctax::ctax_t
    rebatesys::rebatesys_t
    hben::hben_t
    ctaxben::ctaxben_t
    ccben::ccben_t
    uc::uc_t
    statepen::statepen_t
    bencap::bencap_t
    extra::extra_t
end

struct bcout_t
    kinks_num::Cint
    kinks_hrs::SVector{maxKinks,Cdouble}
    kinks_earn::SVector{maxKinks,Cdouble}
    kinks_net::SVector{maxKinks,Cdouble}
    kinks_mtr::SVector{maxKinks,Cdouble}
    bc_desc::SVector{len_bcdesc,Cchar}
end

function fam_gen(;
    couple = 0,
    married = 0,
    ccexp = 0.0,
    maint = 0.0,
    nkids = 0,
    kidage = [0],
    kidsex = [0],
    nothads = 0,
    tenure = lab_tenure.own_outright,
    rent = 0.0,
    rentcap = 0.0,
    bedrooms = 0,
    region = lab_region.north_east,
    ctband = lab_ctax.bandd,
    banddratio = 1.0,
    intdate = 19900101,
    age1 = 25,
    selfemp1 = 0,
    hrs1 = 0.0,
    earn1 = 0.0,
    age2 = nothing,
    selfemp2 = nothing,
    hrs2 = nothing,
    earn2 = nothing,
)
    isnothing(age2) && isnothing(selfemp2) && isnothing(hrs2) && isnothing(earn2) ?
    (ad2 = false) : (ad2 = true)

    isnothing(age2) && (age2 = 25)
    isnothing(selfemp2) && (selfemp2 = 0)
    isnothing(hrs2) && (hrs2 = 0.0)
    isnothing(earn2) && (earn2 = 0.0)

    famad1 = famad_t(age1, selfemp1, hrs1, earn1)
    famad2 = famad_t(age2, selfemp2, hrs2, earn2)

    thislen = length(kidage)
    if thislen > maxKids
        fortaxError("kidage exceeds bounds in fam_gam")
    elseif thislen < maxKids
        resize!(kidage, maxKids)
        kidage[thislen+1:maxKids] .= 0
        nkids = max(nkids, thislen)
    end
    thislen = length(kidsex)
    if thislen > maxKids
        fortaxError("kidsex exceeds bounds in fam_gam")
    elseif thislen < maxKids
        resize!(kidsex, maxKids)
        kidsex[thislen+1:maxKids] .= 0
        nkids = max(nkids, thislen)
    end

    ad2 && (couple = 1)

    # equivalent to behaviour fam_refresh

    nkids = min(max(nkids, 0), maxKids)
    kidage[1:nkids] = min.(max.(kidage[1:nkids], 0), 18)
    age1 = min(max(age1, 16), 200)
    age2 = min(max(age2, 16), 200)

    married == 1 && (couple = 1)

    kidagedist0 = zeros(Int32, 21)
    kidagedist1 = zeros(Int32, 21)
    if (nkids > 0)
        for i = 1:nkids
            if (kidsex[i] == 0)
                kidagedist0[2+kidage[i]] = kidagedist0[2+kidage[i]] + 1
            else
                kidagedist1[2+kidage[i]] = kidagedist1[2+kidage[i]] + 1
            end
        end
        kidagedist0 = cumsum(kidagedist0)
        kidagedist1 = cumsum(kidagedist1)
        kidagedist = kidagedist0 + kidagedist1
        yngkid = minimum(kidage[1:nkids])
    else
        kidagedist = zeros(Int32, 21)
        yngkid = -1
    end

    # familty type
    if (couple == 0)
        if (nkids == 0)
            famtype = lab_famtype.single_nokids
        else
            famtype = lab_famtype.single_kids
        end
    else
        if (nkids == 0)
            famtype = lab_famtype.couple_nokids
        else
            famtype = lab_famtype.couple_kids
        end
    end

    fam = fam_t(
        couple,
        married,
        ccexp,
        maint,
        nkids,
        kidage,
        kidsex,
        nothads,
        tenure,
        rent,
        rentcap,
        bedrooms,
        region,
        ctband,
        banddratio,
        intdate,
        famtype,
        yngkid,
        kidagedist,
        kidagedist0,
        kidagedist1,
        [famad1, famad2],
    )
    return fam
end
