! This file is part of the FORTAX library;

! FORTAX is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! FORTAX is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with FORTAX.  If not, see <http://www.gnu.org/licenses/>.


! fortax_type
! -----------------------------------------------------------------------
! module defines the main derived types that describe families, the tax
! system, and the information returned by the calculation routines, AS


module fortax_type

    use fortax_realtype, only : dp
    use iso_c_binding

    implicit none

    ! TODO improve label handling
    private :: dp
    private
    public :: fam_init, net_init, sys_init
    public :: fam_saveF90, sys_saveF90
    public :: fam_t, net_t, sys_t, rpi_t, sysindex_t, bcout_t
    public :: lab, label_bool, label_ctax, label_region, label_tenure
    public :: fam_gen, fam_desc, net_desc
    public :: operator(+), operator(*), operator(/), operator(-)

    ! constants for array bounds and internal values
    integer, parameter, public :: maxKids = 16
    integer, parameter, public :: maxRpi = 1024
    integer, parameter, public :: maxSysIndex = 128
    integer, parameter, public :: maxNumAgeRng = 32
    integer, parameter, public :: maxIncTaxBands = 32
    integer, parameter, public :: maxNatInsBands = 32
    integer, parameter, public :: maxNatInsC4Bands = 32
    integer, parameter, public :: maxKinks = 256
    integer, parameter, public :: maxUnderOccBands = 8
    integer, parameter, public :: maxLHABands = 8
    real(dp), parameter, public :: sysHuge = 1.0e100_dp
    integer, parameter, public :: len_sysname = 64
    integer, parameter, public :: len_sysdesc = 512
    integer, parameter, public :: len_sysindex = 256
    integer, parameter, public :: len_label = 16
    integer, parameter, public :: len_labstring = 64
    integer, parameter, public :: len_bcdesc = 256

    ! lab_t
    ! -----------------------------------------------------------------------
    ! defines various labels as specified in lablist and the associated
    ! include files. at the moment I am not using the string values, but they
    ! are still defined for possible future use

    type :: lab_bool_t
integer :: no
        integer :: yes
    end type lab_bool_t

    type :: lab_ctax_t
integer :: banda
        integer :: bandb
        integer :: bandc
        integer :: bandd
        integer :: bande
        integer :: bandf
        integer :: bandg
        integer :: bandh
        integer :: bandi
    end type lab_ctax_t

    type :: lab_tenure_t
integer :: own_outright
        integer :: mortgage
        integer :: part_own
        integer :: social_renter
        integer :: private_renter
        integer :: rent_free
        integer :: other
    end type lab_tenure_t

    type :: lab_region_t
integer :: north_east
        integer :: north_west
        integer :: yorks
        integer :: east_midlands
        integer :: west_midlands
        integer :: eastern
        integer :: london
        integer :: south_east
        integer :: south_west
        integer :: wales
        integer :: scotland
        integer :: northern_ireland
    end type lab_region_t

    type :: lab_sex_t
integer :: male
        integer :: female
    end type lab_sex_t

    type :: lab_famtype_t
integer :: single_nokids
        integer :: single_kids
        integer :: couple_nokids
        integer :: couple_kids
    end type lab_famtype_t


    type :: lab_t
        type(lab_bool_t) :: bool
        type(lab_ctax_t) :: ctax
        type(lab_tenure_t) :: tenure
        type(lab_region_t) :: region
        type(lab_sex_t) :: sex
        type(lab_famtype_t) :: famtype
    end type lab_t

    type(lab_t), parameter :: lab = lab_t( &
        lab_bool_t(0, 1), &
        lab_ctax_t(1, 2, 3, 4, 5, 6, 7, 8, 9), &
        lab_tenure_t(1, 2, 3, 4, 5, 6, 7), &
        lab_region_t(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), &
        lab_sex_t(0, 1), &
        lab_famtype_t(1, 2, 3, 4))


    ! rpi_t
    ! -----------------------------------------------------------------------
    ! defines the prices indexing for uprating

    type, bind(c) :: rpi_t
        integer  :: ndate
        integer  :: date(maxRPI)
        real(dp) :: index(maxRPI)
    end type


    ! sysindex_t
    ! -----------------------------------------------------------------------
    ! defines sysindex

    type, bind(c) :: sysindex_t
        integer :: nsys
        integer :: date0(maxSysIndex), date1(maxSysIndex)
        character(kind = c_char) :: fname(len_sysindex, maxSysIndex)
    end type


    ! famad_t
    ! -----------------------------------------------------------------------
    ! defines the adult level family type structure (see fam_t below)

    type, bind(c) :: famad_t
integer :: age
        integer :: selfemp
        real(dp) :: hrs
        real(dp) :: earn
    end type famad_t

    ! fam_t
    ! -----------------------------------------------------------------------
    ! defines the family type structure, containing information on
    ! demographic characteristics, earnings, hours of work, and other
    ! information. Anything that can affect the taxes and transfer payments
    ! of a family is defined in here.

    type, bind(c) :: fam_t
integer :: couple
        integer :: married
        real(dp) :: ccexp
        real(dp) :: maint
        integer :: nkids
        integer :: kidage(maxKids)
        integer :: kidsex(maxKids)
        integer :: nothads
        integer :: tenure
        real(dp) :: rent
        real(dp) :: rentcap
        integer :: bedrooms
        integer :: region
        integer :: ctband
        real(dp) :: banddratio
        integer :: intdate
        integer :: famtype
        type(famad_t) :: ad(2)
    end type fam_t

    ! netad_t
    ! -----------------------------------------------------------------------
    ! defines the adult level information returned following calls to the
    ! main calculation routines (see net_t below).

    type, bind(c) :: netad_t
real(dp) :: taxable
        real(dp) :: inctax
        real(dp) :: natins
        real(dp) :: natinsc1
        real(dp) :: natinsc2
        real(dp) :: natinsc4
        real(dp) :: pretaxearn
        real(dp) :: posttaxearn
    end type netad_t

    ! nettu_t
    ! -----------------------------------------------------------------------
    ! defines the tax unit level information returned following calls to the
    ! main calculation routines (see net_t below).

    type, bind(c) :: nettu_t
real(dp) :: pretaxearn
        real(dp) :: posttaxearn
        real(dp) :: chben
        real(dp) :: matgrant
        real(dp) :: fc
        real(dp) :: wtc
        real(dp) :: ctc
        real(dp) :: ccexp
        real(dp) :: cctaxrefund
        real(dp) :: incsup
        real(dp) :: hben
        real(dp) :: polltax
        real(dp) :: polltaxben
        real(dp) :: ctax
        real(dp) :: ctaxben
        real(dp) :: maxuc
        real(dp) :: uc
        real(dp) :: dispinc
        real(dp) :: pretax
        real(dp) :: nettax
        real(dp) :: chcaresub
        real(dp) :: fsm
        real(dp) :: totben
    end type nettu_t

    ! net_t
    ! -----------------------------------------------------------------------
    ! defines the information returned following calls to the main
    ! calculation routines within fortax_calc. It contains measures of net
    ! income, together with various tax amounts and other components of income

    type, bind(c) :: net_t
        type(netad_t) :: ad(2)
        type(nettu_t) :: tu
    end type net_t


    ! Associate "net_plus_net" routine with "+" operator
    interface operator(+)
      module procedure net_plus_net
    end interface

    ! Associate "net_minus_net" routine with "-" operator
    interface operator(-)
      module procedure net_minus_net
    end interface

    ! Associate "net_times_scalar" and "scalar_times_net" routines with "*" operator
    interface operator(*)
      module procedure net_times_scalar
      module procedure net_times_scalar_integer
      module procedure scalar_times_net
      module procedure scalar_times_net_integer
    end interface

    ! Associate "net_div_scalar" routine with "/" operator
    interface operator(/)
      module procedure net_div_scalar
      module procedure net_div_scalar_integer
    end interface


    interface write_f90
        module procedure write_f90integer
        module procedure write_f90integerarray
        module procedure write_f90integerarray2
        module procedure write_f90double
        module procedure write_f90doublearray
        module procedure write_f90doublearray2
    end interface write_f90

    interface desc_f90
        module procedure desc_f90integer
        module procedure desc_f90integer_label
        module procedure desc_f90integerarray
        module procedure desc_f90integerarray_label
        module procedure desc_f90integerarray2
        module procedure desc_f90integerarray2_label
        module procedure desc_f90double
        module procedure desc_f90doublearray
        module procedure desc_f90doublearray2
    end interface desc_f90


    ! sys_t
    ! -----------------------------------------------------------------------
    ! defines the tax system structure which families of type fam_t face.
    ! It describes all the parameters which are interpreted within the
    ! module fortax_calc

    type, bind(c) :: inctax_t
integer :: numbands
        real(dp) :: pa
        integer :: doPATaper
        integer :: disablePATaperRounding
        real(dp) :: paTaperThresh
        real(dp) :: paTaperRate
        integer :: doTPA
        real(dp) :: maxTPA
        real(dp) :: mma
        real(dp) :: ctc
        real(dp) :: ctcyng
        real(dp) :: mmarate
        real(dp) :: ctctaper
        real(dp) :: c4rebate
        real(dp) :: bands(maxIncTaxBands)
        real(dp) :: rates(maxIncTaxBands)
    end type inctax_t

    type, bind(c) :: natins_t
integer :: numrates
        integer :: c4nrates
        real(dp) :: c2floor
        real(dp) :: c2rate
        real(dp) :: ceiling
        real(dp) :: rates(maxNatInsBands)
        real(dp) :: bands(maxNatInsBands)
        real(dp) :: c4rates(maxNatInsC4Bands)
        real(dp) :: c4bands(maxNatInsC4Bands)
    end type natins_t

    type, bind(c) :: chben_t
integer :: doChBen
        real(dp) :: basic
        real(dp) :: kid1xtr
        real(dp) :: opf
        real(dp) :: MatGrantVal
        integer :: MatGrantOnlyFirstKid
        integer :: doTaper
        integer :: disableTaperRounding
        real(dp) :: taperStart
        real(dp) :: taperRate
        integer :: taperIsIncTax
    end type chben_t

    type, bind(c) :: fc_t
integer :: dofamcred
        integer :: NumAgeRng
        integer :: MaxAgeCC
        integer :: WFTCMaxAgeCC
        real(dp) :: adult
        real(dp) :: ftprem
        real(dp) :: hours1
        real(dp) :: hours2
        real(dp) :: thres
        real(dp) :: taper
        real(dp) :: MaintDisreg
        real(dp) :: MaxCC1
        real(dp) :: MaxCC2
        real(dp) :: WFTCMaxCC1
        real(dp) :: WFTCMaxCC2
        real(dp) :: WFTCPropCC
        real(dp) :: MinAmt
        integer :: kidagel(maxNumAgeRng)
        integer :: kidageu(maxNumAgeRng)
        real(dp) :: kidcred(maxNumAgeRng)
    end type fc_t

    type, bind(c) :: ctc_t
real(dp) :: fam
        real(dp) :: baby
        real(dp) :: kid
        integer :: maxKids
    end type ctc_t

    type, bind(c) :: wtc_t
real(dp) :: Basic
        real(dp) :: CouLP
        real(dp) :: FT
        real(dp) :: MinHrsKids
        real(dp) :: MinHrsCouKids
        real(dp) :: MinHrsNoKids
        real(dp) :: FTHrs
        integer :: MinAgeKids
        integer :: MinAgeNoKids
        real(dp) :: MaxCC1
        real(dp) :: MaxCC2
        real(dp) :: PropCC
        integer :: MaxAgeCC
        real(dp) :: NewDisreg
        integer :: NewDisregCon
    end type wtc_t

    type, bind(c) :: ntc_t
integer :: donewtaxcred
        real(dp) :: thr1lo
        real(dp) :: thr1hi
        real(dp) :: thr2
        real(dp) :: taper1
        real(dp) :: taper2
        integer :: taperCTCInOneGo
        real(dp) :: MinAmt
    end type ntc_t

    type, bind(c) :: cctaxrefund_t
integer :: doCCTaxRefund
        real(dp) :: MaxPerChild
        integer :: MaxAge
        real(dp) :: ReceiptProp
        real(dp) :: MinEarn
        real(dp) :: MaxInc
    end type cctaxrefund_t

    type, bind(c) :: incsup_t
integer :: doIncSup
        integer :: IncChben
        integer :: NumAgeRng
        integer :: MinAgeMain
        integer :: MinAgeMainSin
        real(dp) :: MainCou
        real(dp) :: YngCou
        real(dp) :: MainLP
        real(dp) :: YngLP
        real(dp) :: MainSin
        real(dp) :: YngSin
        integer :: MinAgeFSM
        integer :: MaxAgeUniversalFSM
        real(dp) :: ValFSM
        real(dp) :: DisregLP
        real(dp) :: DisregSin
        real(dp) :: DisregCou
        integer :: DisregShared
        real(dp) :: PremFam
        real(dp) :: PremLP
        real(dp) :: hours
        real(dp) :: MaintDisreg
        integer :: AgeRngl(maxNumAgeRng)
        integer :: AgeRngu(maxNumAgeRng)
        real(dp) :: AddKid(maxNumAgeRng)
    end type incsup_t

    type, bind(c) :: ctax_t
integer :: docounciltax
        real(dp) :: bandD
        real(dp) :: SinDis
        real(dp) :: EnglandRatioA
        real(dp) :: EnglandRatioB
        real(dp) :: EnglandRatioC
        real(dp) :: EnglandRatioE
        real(dp) :: EnglandRatioF
        real(dp) :: EnglandRatioG
        real(dp) :: EnglandRatioH
        real(dp) :: EnglandRatioI
        real(dp) :: ScotlandRatioA
        real(dp) :: ScotlandRatioB
        real(dp) :: ScotlandRatioC
        real(dp) :: ScotlandRatioE
        real(dp) :: ScotlandRatioF
        real(dp) :: ScotlandRatioG
        real(dp) :: ScotlandRatioH
        real(dp) :: ScotlandRatioI
        real(dp) :: WalesRatioA
        real(dp) :: WalesRatioB
        real(dp) :: WalesRatioC
        real(dp) :: WalesRatioE
        real(dp) :: WalesRatioF
        real(dp) :: WalesRatioG
        real(dp) :: WalesRatioH
        real(dp) :: WalesRatioI
    end type ctax_t

    type, bind(c) :: rebatesys_t
integer :: RulesUnderFC
        integer :: RulesUnderWFTC
        integer :: RulesUnderNTC
        integer :: RulesUnderUC
        integer :: NumAgeRng
        integer :: Restrict
        integer :: docap
        integer :: MinAgeMain
        integer :: MinAgeMainSin
        real(dp) :: MainCou
        real(dp) :: YngCou
        real(dp) :: MainLP
        real(dp) :: YngLP
        real(dp) :: MainSin
        real(dp) :: YngSin
        real(dp) :: DisregSin
        real(dp) :: DisregLP
        real(dp) :: DisregCou
        integer :: CredInDisregCC
        integer :: ChbenIsIncome
        real(dp) :: PremFam
        real(dp) :: PremLP
        real(dp) :: MaintDisreg
        real(dp) :: MaxCC1
        real(dp) :: MaxCC2
        integer :: MaxAgeCC
        integer :: AgeRngl(maxNumAgeRng)
        integer :: AgeRngu(maxNumAgeRng)
        real(dp) :: AddKid(maxNumAgeRng)
        integer :: MaxKids
    end type rebatesys_t

    type, bind(c) :: hben_t
integer :: doHBen
        real(dp) :: taper
        real(dp) :: MinAmt
        integer :: doUnderOccCharge
        integer :: doUnderOccChargeScotland
        integer :: doUnderOccChargeNI
        integer :: numUnderOccBands
        real(dp) :: underOccRates(maxUnderOccBands)
        integer :: doLHA
        integer :: LHASharedAccAge
        integer :: numLHABands
        real(dp) :: LHARates(maxLHABands)
    end type hben_t

    type, bind(c) :: ctaxben_t
integer :: docounciltaxben
        real(dp) :: taper
        integer :: doEntitlementCut
        real(dp) :: entitlementShare
    end type ctaxben_t

    type, bind(c) :: ccben_t
integer :: dopolltax
        real(dp) :: taper
        real(dp) :: PropElig
        real(dp) :: MinAmt
        real(dp) :: CCrate
    end type ccben_t

    type, bind(c) :: uc_t
integer :: doUnivCred
        real(dp) :: MainCou
        real(dp) :: YngCou
        real(dp) :: MainSin
        real(dp) :: YngSin
        integer :: MinAgeMain
        real(dp) :: FirstKid
        real(dp) :: OtherKid
        integer :: MaxKids
        real(dp) :: MaxCC1
        real(dp) :: MaxCC2
        real(dp) :: PropCC
        integer :: MaxAgeCC
        integer :: doRentCap
        real(dp) :: DisregSinNoKidsHi
        real(dp) :: DisregSinNoKidsLo
        real(dp) :: DisregSinKidsHi
        real(dp) :: DisregSinKidsLo
        real(dp) :: DisregCouNoKidsHi
        real(dp) :: DisregCouNoKidsLo
        real(dp) :: DisregCouKidsHi
        real(dp) :: DisregCouKidsLo
        real(dp) :: taper
        real(dp) :: MinAmt
    end type uc_t

    type, bind(c) :: statepen_t
integer :: doStatePen
        integer :: PenAgeMan
        integer :: PenAgeWoman
    end type statepen_t

    type, bind(c) :: bencap_t
integer :: doCap
        integer :: doNI
        integer :: doThruUC
        real(dp) :: sinNoKids
        real(dp) :: sinKids
        real(dp) :: couNoKids
        real(dp) :: couKids
        integer :: LondonCapAmt
        real(dp) :: LondonSinNoKids
        real(dp) :: LondonSinKids
        real(dp) :: LondonCouNoKids
        real(dp) :: LondonCouKids
        real(dp) :: UCEarnThr
    end type bencap_t

    type, bind(c) :: extra_t
integer :: fsminappamt
        integer :: matgrant
        integer :: prices
    end type extra_t


    type, bind(c) :: sys_t
        character(kind = c_char) :: sysname(len_sysname)
        character(kind = c_char) :: sysdesc(len_sysdesc)
        type(inctax_t) :: inctax
        type(natins_t) :: natins
        type(chben_t) :: chben
        type(fc_t) :: fc
        type(ctc_t) :: ctc
        type(wtc_t) :: wtc
        type(ntc_t) :: ntc
        type(cctaxrefund_t) :: cctaxrefund
        type(incsup_t) :: incsup
        type(ctax_t) :: ctax
        type(rebatesys_t) :: rebatesys
        type(hben_t) :: hben
        type(ctaxben_t) :: ctaxben
        type(ccben_t) :: ccben
        type(uc_t) :: uc
        type(statepen_t) :: statepen
        type(bencap_t) :: bencap
        type(extra_t) :: extra
    end type sys_t

    type, bind(c) :: bcout_t
        integer :: kinks_num
        real(dp), dimension(maxkinks) :: kinks_hrs
        real(dp), dimension(maxkinks) :: kinks_earn
        real(dp), dimension(maxkinks) :: kinks_net
        real(dp), dimension(maxkinks) :: kinks_mtr
        character(kind = c_char) :: bc_desc(len_bcdesc)
    end type bcout_t

contains


    ! fam_init
    ! -----------------------------------------------------------------------
    ! intializes family structure. unless defaults are coded here, integers
    ! are set to 0, doubles to 0.0_dp and logicals to .false. (and similarly
    ! for arrays). Could possibly add defaults in fam_t, but given it is
    ! only referenced here, there probably isn't much advantage to that

    elemental subroutine fam_init(fam)

        implicit none

        type(fam_t), intent(out) :: fam

        fam%couple = 0
        fam%married = 0
        fam%ccexp = 0.0_dp
        fam%maint = 0.0_dp
        fam%nkids = 0
        fam%kidage = 0
        fam%kidsex = 0
        fam%nothads = 0
        fam%tenure = lab%tenure%own_outright
        fam%rent = 0.0_dp
        fam%rentcap = 0.0_dp
        fam%bedrooms = 0
        fam%region = lab%region%north_east
        fam%ctband = lab%ctax%bandd
        fam%banddratio = 1.0
        fam%intdate = 19900101
        fam%famtype = lab%famtype%single_nokids

        fam%ad%age = 25
        fam%ad%selfemp = 0
        fam%ad%hrs = 0.0_dp
        fam%ad%earn = 0.0_dp

    end subroutine fam_init


    ! fam_desc
    ! -----------------------------------------------------------------------
    ! will display the information contained in the family variable fam and
    ! write this to a file if fname is specified

    subroutine fam_desc(fam, fname)

        use fortax_util, only : intToStr, strCentre, fortaxError

        use, intrinsic :: iso_fortran_env

        type(fam_t), intent(in) :: fam
        character(len = *), optional :: fname

        integer :: funit, i, ios

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit, *)
        write(funit, '(A)') repeat("=", 62)
        write(funit, '(A)') strCentre('fam_desc (FAMILY):', 62)
        write(funit, '(A)') repeat("=", 62)
        call desc_f90(funit, "Married/cohabiting", "couple", fam%couple, label_bool(fam%couple))
        call desc_f90(funit, "Married", "married", fam%married, label_bool(fam%married))
        call desc_f90(funit, "Childcare expenditure", "ccexp", fam%ccexp)
        call desc_f90(funit, "Maintenance income", "maint", fam%maint)
        call desc_f90(funit, "Number of children", "nkids", fam%nkids)
        call desc_f90(funit, "Age of children", "kidage", fam%kidage, fam%nkids)
        call desc_f90(funit, "Sex of children", "kidsex", fam%kidsex, fam%nkids, label_sex(fam%kidsex))
        call desc_f90(funit, "Number of other adults", "nothads", fam%nothads)
        call desc_f90(funit, "Housing tenure", "tenure", fam%tenure, label_tenure(fam%tenure))
        call desc_f90(funit, "Housing rent", "rent", fam%rent)
        call desc_f90(funit, "Housing rent cap", "rentcap", fam%rentcap)
        call desc_f90(funit, "Number of bedrooms", "bedrooms", fam%bedrooms)
        call desc_f90(funit, "Region", "region", fam%region, label_region(fam%region))
        call desc_f90(funit, "Council tax band", "ctband", fam%ctband, label_ctax(fam%ctband))
        call desc_f90(funit, "Council tax band-D ratio", "banddratio", fam%banddratio)
        call desc_f90(funit, "Interview date", "intdate", fam%intdate)
        call desc_f90(funit, "Family type", "famtype", fam%famtype, label_famtype(fam%famtype))
        write(funit, '(A)') repeat("=", 62)

        ! write(funit, *)
        ! write(funit, '(A)') repeat("=", 62)
        write(funit, '(A)') strCentre('fam_desc (ADULT 1):', 62)
        write(funit, '(A)') repeat("=", 62)
        call desc_f90(funit, "Age", "age", fam%ad(1)%age)
        call desc_f90(funit, "Self-employed", "selfemp", fam%ad(1)%selfemp, label_bool(fam%ad(1)%selfemp))
        call desc_f90(funit, "Hours-of-work", "hrs", fam%ad(1)%hrs)
        call desc_f90(funit, "Earnings", "earn", fam%ad(1)%earn)
        write(funit, '(A)') repeat("=", 62)

        if (fam%couple == 1) then
        ! write(funit, *)
        ! write(funit, '(A)') repeat("=", 62)
        write(funit, '(A)') strCentre('fam_desc (ADULT 2):', 62)
        write(funit, '(A)') repeat("=", 62)
        call desc_f90(funit, "Age", "age", fam%ad(2)%age)
        call desc_f90(funit, "Self-employed", "selfemp", fam%ad(2)%selfemp, label_bool(fam%ad(2)%selfemp))
        call desc_f90(funit, "Hours-of-work", "hrs", fam%ad(2)%hrs)
        call desc_f90(funit, "Earnings", "earn", fam%ad(2)%earn)
        write(funit, '(A)') repeat("=", 62)
        end if
        write(funit, *)

        close(funit)

    end subroutine fam_desc

    ! obtain string labels for the variable value labels


















elemental function label_bool(val) result(str)
    implicit none
    integer, intent(in) :: val
    character(len = len_label) :: str
    select case(val)
        case(lab%bool%no)
            str = "no"
        case(lab%bool%yes)
            str = "yes"
        case default
            str = "INVALID VALUE"
    end select
end function label_bool

elemental function labstring_bool(val) result(str)
    implicit none
    integer, intent(in) :: val
    character(len = len_labstring) :: str
    select case(val)
        case(lab%bool%no)
            str = "No"
        case(lab%bool%yes)
            str = "Yes"
        case default
            str = "INVALID VALUE"
    end select
end function labstring_bool

elemental function label_ctax(val) result(str)
    implicit none
    integer, intent(in) :: val
    character(len = len_label) :: str
    select case(val)
        case(lab%ctax%banda)
            str = "banda"
        case(lab%ctax%bandb)
            str = "bandb"
        case(lab%ctax%bandc)
            str = "bandc"
        case(lab%ctax%bandd)
            str = "bandd"
        case(lab%ctax%bande)
            str = "bande"
        case(lab%ctax%bandf)
            str = "bandf"
        case(lab%ctax%bandg)
            str = "bandg"
        case(lab%ctax%bandh)
            str = "bandh"
        case(lab%ctax%bandi)
            str = "bandi"
        case default
            str = "INVALID VALUE"
    end select
end function label_ctax

elemental function labstring_ctax(val) result(str)
    implicit none
    integer, intent(in) :: val
    character(len = len_labstring) :: str
    select case(val)
        case(lab%ctax%banda)
            str = "Council Tax Band A"
        case(lab%ctax%bandb)
            str = "Council Tax Band B"
        case(lab%ctax%bandc)
            str = "Council Tax Band C"
        case(lab%ctax%bandd)
            str = "Council Tax Band D"
        case(lab%ctax%bande)
            str = "Council Tax Band E"
        case(lab%ctax%bandf)
            str = "Council Tax Band F"
        case(lab%ctax%bandg)
            str = "Council Tax Band G"
        case(lab%ctax%bandh)
            str = "Council Tax Band H"
        case(lab%ctax%bandi)
            str = "Council Tax Band I"
        case default
            str = "INVALID VALUE"
    end select
end function labstring_ctax

elemental function label_tenure(val) result(str)
    implicit none
    integer, intent(in) :: val
    character(len = len_label) :: str
    select case(val)
        case(lab%tenure%own_outright)
            str = "own_outright"
        case(lab%tenure%mortgage)
            str = "mortgage"
        case(lab%tenure%part_own)
            str = "part_own"
        case(lab%tenure%social_renter)
            str = "social_renter"
        case(lab%tenure%private_renter)
            str = "private_renter"
        case(lab%tenure%rent_free)
            str = "rent_free"
        case(lab%tenure%other)
            str = "other"
        case default
            str = "INVALID VALUE"
    end select
end function label_tenure

elemental function labstring_tenure(val) result(str)
    implicit none
    integer, intent(in) :: val
    character(len = len_labstring) :: str
    select case(val)
        case(lab%tenure%own_outright)
            str = "Own outright"
        case(lab%tenure%mortgage)
            str = "Mortgage"
        case(lab%tenure%part_own)
            str = "Part own,  part rent"
        case(lab%tenure%social_renter)
            str = "Social renter"
        case(lab%tenure%private_renter)
            str = "Private renter"
        case(lab%tenure%rent_free)
            str = "Rent free"
        case(lab%tenure%other)
            str = "Other"
        case default
            str = "INVALID VALUE"
    end select
end function labstring_tenure

elemental function label_region(val) result(str)
    implicit none
    integer, intent(in) :: val
    character(len = len_label) :: str
    select case(val)
        case(lab%region%north_east)
            str = "north_east"
        case(lab%region%north_west)
            str = "north_west"
        case(lab%region%yorks)
            str = "yorks"
        case(lab%region%east_midlands)
            str = "east_midlands"
        case(lab%region%west_midlands)
            str = "west_midlands"
        case(lab%region%eastern)
            str = "eastern"
        case(lab%region%london)
            str = "london"
        case(lab%region%south_east)
            str = "south_east"
        case(lab%region%south_west)
            str = "south_west"
        case(lab%region%wales)
            str = "wales"
        case(lab%region%scotland)
            str = "scotland"
        case(lab%region%northern_ireland)
            str = "northern_ireland"
        case default
            str = "INVALID VALUE"
    end select
end function label_region

elemental function labstring_region(val) result(str)
    implicit none
    integer, intent(in) :: val
    character(len = len_labstring) :: str
    select case(val)
        case(lab%region%north_east)
            str = "North East"
        case(lab%region%north_west)
            str = "North West and Merseyside"
        case(lab%region%yorks)
            str = "Yorks and Humberside"
        case(lab%region%east_midlands)
            str = "East Midlands"
        case(lab%region%west_midlands)
            str = "West Midlands"
        case(lab%region%eastern)
            str = "Eastern"
        case(lab%region%london)
            str = "London"
        case(lab%region%south_east)
            str = "South East"
        case(lab%region%south_west)
            str = "South West"
        case(lab%region%wales)
            str = "Wales"
        case(lab%region%scotland)
            str = "Scotland"
        case(lab%region%northern_ireland)
            str = "Northern Ireland"
        case default
            str = "INVALID VALUE"
    end select
end function labstring_region

elemental function label_sex(val) result(str)
    implicit none
    integer, intent(in) :: val
    character(len = len_label) :: str
    select case(val)
        case(lab%sex%male)
            str = "male"
        case(lab%sex%female)
            str = "female"
        case default
            str = "INVALID VALUE"
    end select
end function label_sex

elemental function labstring_sex(val) result(str)
    implicit none
    integer, intent(in) :: val
    character(len = len_labstring) :: str
    select case(val)
        case(lab%sex%male)
            str = "Male"
        case(lab%sex%female)
            str = "Female"
        case default
            str = "INVALID VALUE"
    end select
end function labstring_sex

elemental function label_famtype(val) result(str)
    implicit none
    integer, intent(in) :: val
    character(len = len_label) :: str
    select case(val)
        case(lab%famtype%single_nokids)
            str = "single_nokids"
        case(lab%famtype%single_kids)
            str = "single_kids"
        case(lab%famtype%couple_nokids)
            str = "couple_nokids"
        case(lab%famtype%couple_kids)
            str = "couple_kids"
        case default
            str = "INVALID VALUE"
    end select
end function label_famtype

elemental function labstring_famtype(val) result(str)
    implicit none
    integer, intent(in) :: val
    character(len = len_labstring) :: str
    select case(val)
        case(lab%famtype%single_nokids)
            str = "Single, no children"
        case(lab%famtype%single_kids)
            str = "Lone parent"
        case(lab%famtype%couple_nokids)
            str = "Couple, no children"
        case(lab%famtype%couple_kids)
            str = "Couple, children"
        case default
            str = "INVALID VALUE"
    end select
end function labstring_famtype


    ! fam_gen
    ! -----------------------------------------------------------------------
    ! will return fam setting any characteristics to the values that are
    ! specified. Adult information should be passed by adding a suffix 1 or 2
    ! for the respective adult number.

    subroutine fam_gen(fam, couple, married, ccexp, maint, nkids, kidage, kidsex, nothads, tenure, rent, rentcap, bedrooms,&
        & region, ctband, banddratio, intdate, age1, selfemp1, hrs1, earn1, age2, selfemp2, hrs2, earn2, correct)

        use fortax_util, only : fortaxError

        implicit none

        type(fam_t), intent(out) :: fam
integer, intent(in), optional :: couple
        integer, intent(in), optional :: married
        real(dp), intent(in), optional :: ccexp
        real(dp), intent(in), optional :: maint
        integer, intent(in), optional :: nkids
        integer, intent(in), optional :: kidage(:)
        integer, intent(in), optional :: kidsex(:)
        integer, intent(in), optional :: nothads
        integer, intent(in), optional :: tenure
        real(dp), intent(in), optional :: rent
        real(dp), intent(in), optional :: rentcap
        integer, intent(in), optional :: bedrooms
        integer, intent(in), optional :: region
        integer, intent(in), optional :: ctband
        real(dp), intent(in), optional :: banddratio
        integer, intent(in), optional :: intdate
integer, intent(in), optional :: age1
        integer, intent(in), optional :: selfemp1
        real(dp), intent(in), optional :: hrs1
        real(dp), intent(in), optional :: earn1
integer, intent(in), optional :: age2
        integer, intent(in), optional :: selfemp2
        real(dp), intent(in), optional :: hrs2
        real(dp), intent(in), optional :: earn2
        logical, intent(in), optional :: correct
        logical :: correct2
        integer :: kidSize
        logical :: ad2

        call fam_init(fam)

        if (present(couple)) fam%couple = couple
        if (present(married)) fam%married = married
        if (present(ccexp)) fam%ccexp = ccexp
        if (present(maint)) fam%maint = maint
        if (present(nkids)) fam%nkids = nkids
        if (present(kidage)) then
            if (size(kidage) > maxKids) then
                call fortaxError("kidage exceeds bounds in fam_gen")
            else
                fam%kidage(1:size(kidage)) = kidage
                if (.not. present(nkids)) then
                    fam%nkids = size(kidage)
                end if
            endif
        end if
        if (present(kidsex)) then
            if (size(kidsex) > maxKids) then
                call fortaxError("kidsex exceeds bounds in fam_gen")
            else
                fam%kidsex(1:size(kidsex)) = kidsex
                if (.not. present(nkids)) then
                    fam%nkids = size(kidsex)
                end if
            endif
        end if
        if (present(nothads)) fam%nothads = nothads
        if (present(tenure)) fam%tenure = tenure
        if (present(rent)) fam%rent = rent
        if (present(rentcap)) fam%rentcap = rentcap
        if (present(bedrooms)) fam%bedrooms = bedrooms
        if (present(region)) fam%region = region
        if (present(ctband)) fam%ctband = ctband
        if (present(banddratio)) fam%banddratio = banddratio
        if (present(intdate)) fam%intdate = intdate

        if (present(age1)) fam%ad(1)%age = age1
        if (present(selfemp1)) fam%ad(1)%selfemp = selfemp1
        if (present(hrs1)) fam%ad(1)%hrs = hrs1
        if (present(earn1)) fam%ad(1)%earn = earn1
        if (present(age2)) fam%ad(2)%age = age2
        if (present(selfemp2)) fam%ad(2)%selfemp = selfemp2
        if (present(hrs2)) fam%ad(2)%hrs = hrs2
        if (present(earn2)) fam%ad(2)%earn = earn2

        if (present(correct)) then
            correct2 = correct
        else
            correct2 = .true.
        end if

        if (correct2) then

            !if married true, then couple is true regardless of whether specified
            if (fam%married == 1) fam%couple = 1

            !automatically set couple=.true. if any second adult information is passed
            if (fam%couple == 0) then
                ad2 = .false.
                if (present(age2)) ad2 = .true.
                if (present(selfemp2)) ad2 = .true.
                if (present(hrs2)) ad2 = .true.
                if (present(earn2)) ad2 = .true.
                if (ad2) fam%couple = 1
            end if

        end if

        call fam_refresh(fam)

    end subroutine fam_gen


    ! net_plus_net
    ! ------------------------------------
    ! Addition of two net_t type variables

    function net_plus_net(net1, net2) result(net)
        implicit none
        type(net_t), intent(in) :: net1, net2
        type(net_t) :: net

        net%ad(1)%taxable = net1%ad(1)%taxable + net2%ad(1)%taxable
        net%ad(1)%inctax = net1%ad(1)%inctax + net2%ad(1)%inctax
        net%ad(1)%natins = net1%ad(1)%natins + net2%ad(1)%natins
        net%ad(1)%natinsc1 = net1%ad(1)%natinsc1 + net2%ad(1)%natinsc1
        net%ad(1)%natinsc2 = net1%ad(1)%natinsc2 + net2%ad(1)%natinsc2
        net%ad(1)%natinsc4 = net1%ad(1)%natinsc4 + net2%ad(1)%natinsc4
        net%ad(1)%pretaxearn = net1%ad(1)%pretaxearn + net2%ad(1)%pretaxearn
        net%ad(1)%posttaxearn = net1%ad(1)%posttaxearn + net2%ad(1)%posttaxearn
        net%ad(2)%taxable = net1%ad(2)%taxable + net2%ad(2)%taxable
        net%ad(2)%inctax = net1%ad(2)%inctax + net2%ad(2)%inctax
        net%ad(2)%natins = net1%ad(2)%natins + net2%ad(2)%natins
        net%ad(2)%natinsc1 = net1%ad(2)%natinsc1 + net2%ad(2)%natinsc1
        net%ad(2)%natinsc2 = net1%ad(2)%natinsc2 + net2%ad(2)%natinsc2
        net%ad(2)%natinsc4 = net1%ad(2)%natinsc4 + net2%ad(2)%natinsc4
        net%ad(2)%pretaxearn = net1%ad(2)%pretaxearn + net2%ad(2)%pretaxearn
        net%ad(2)%posttaxearn = net1%ad(2)%posttaxearn + net2%ad(2)%posttaxearn

        net%tu%pretaxearn = net1%tu%pretaxearn + net2%tu%pretaxearn
        net%tu%posttaxearn = net1%tu%posttaxearn + net2%tu%posttaxearn
        net%tu%chben = net1%tu%chben + net2%tu%chben
        net%tu%matgrant = net1%tu%matgrant + net2%tu%matgrant
        net%tu%fc = net1%tu%fc + net2%tu%fc
        net%tu%wtc = net1%tu%wtc + net2%tu%wtc
        net%tu%ctc = net1%tu%ctc + net2%tu%ctc
        net%tu%ccexp = net1%tu%ccexp + net2%tu%ccexp
        net%tu%cctaxrefund = net1%tu%cctaxrefund + net2%tu%cctaxrefund
        net%tu%incsup = net1%tu%incsup + net2%tu%incsup
        net%tu%hben = net1%tu%hben + net2%tu%hben
        net%tu%polltax = net1%tu%polltax + net2%tu%polltax
        net%tu%polltaxben = net1%tu%polltaxben + net2%tu%polltaxben
        net%tu%ctax = net1%tu%ctax + net2%tu%ctax
        net%tu%ctaxben = net1%tu%ctaxben + net2%tu%ctaxben
        net%tu%maxuc = net1%tu%maxuc + net2%tu%maxuc
        net%tu%uc = net1%tu%uc + net2%tu%uc
        net%tu%dispinc = net1%tu%dispinc + net2%tu%dispinc
        net%tu%pretax = net1%tu%pretax + net2%tu%pretax
        net%tu%nettax = net1%tu%nettax + net2%tu%nettax
        net%tu%chcaresub = net1%tu%chcaresub + net2%tu%chcaresub
        net%tu%fsm = net1%tu%fsm + net2%tu%fsm
        net%tu%totben = net1%tu%totben + net2%tu%totben

    end function net_plus_net



    ! net_minus_net
    ! --------------------------------------
    ! Difference of two net_t type variables

    function net_minus_net(net1, net2) result(net)
        implicit none
        type(net_t), intent(in) :: net1, net2
        type(net_t) :: net

        net%ad(1)%taxable = net1%ad(1)%taxable - net2%ad(1)%taxable
        net%ad(1)%inctax = net1%ad(1)%inctax - net2%ad(1)%inctax
        net%ad(1)%natins = net1%ad(1)%natins - net2%ad(1)%natins
        net%ad(1)%natinsc1 = net1%ad(1)%natinsc1 - net2%ad(1)%natinsc1
        net%ad(1)%natinsc2 = net1%ad(1)%natinsc2 - net2%ad(1)%natinsc2
        net%ad(1)%natinsc4 = net1%ad(1)%natinsc4 - net2%ad(1)%natinsc4
        net%ad(1)%pretaxearn = net1%ad(1)%pretaxearn - net2%ad(1)%pretaxearn
        net%ad(1)%posttaxearn = net1%ad(1)%posttaxearn - net2%ad(1)%posttaxearn
        net%ad(2)%taxable = net1%ad(2)%taxable - net2%ad(2)%taxable
        net%ad(2)%inctax = net1%ad(2)%inctax - net2%ad(2)%inctax
        net%ad(2)%natins = net1%ad(2)%natins - net2%ad(2)%natins
        net%ad(2)%natinsc1 = net1%ad(2)%natinsc1 - net2%ad(2)%natinsc1
        net%ad(2)%natinsc2 = net1%ad(2)%natinsc2 - net2%ad(2)%natinsc2
        net%ad(2)%natinsc4 = net1%ad(2)%natinsc4 - net2%ad(2)%natinsc4
        net%ad(2)%pretaxearn = net1%ad(2)%pretaxearn - net2%ad(2)%pretaxearn
        net%ad(2)%posttaxearn = net1%ad(2)%posttaxearn - net2%ad(2)%posttaxearn

        net%tu%pretaxearn = net1%tu%pretaxearn - net2%tu%pretaxearn
        net%tu%posttaxearn = net1%tu%posttaxearn - net2%tu%posttaxearn
        net%tu%chben = net1%tu%chben - net2%tu%chben
        net%tu%matgrant = net1%tu%matgrant - net2%tu%matgrant
        net%tu%fc = net1%tu%fc - net2%tu%fc
        net%tu%wtc = net1%tu%wtc - net2%tu%wtc
        net%tu%ctc = net1%tu%ctc - net2%tu%ctc
        net%tu%ccexp = net1%tu%ccexp - net2%tu%ccexp
        net%tu%cctaxrefund = net1%tu%cctaxrefund - net2%tu%cctaxrefund
        net%tu%incsup = net1%tu%incsup - net2%tu%incsup
        net%tu%hben = net1%tu%hben - net2%tu%hben
        net%tu%polltax = net1%tu%polltax - net2%tu%polltax
        net%tu%polltaxben = net1%tu%polltaxben - net2%tu%polltaxben
        net%tu%ctax = net1%tu%ctax - net2%tu%ctax
        net%tu%ctaxben = net1%tu%ctaxben - net2%tu%ctaxben
        net%tu%maxuc = net1%tu%maxuc - net2%tu%maxuc
        net%tu%uc = net1%tu%uc - net2%tu%uc
        net%tu%dispinc = net1%tu%dispinc - net2%tu%dispinc
        net%tu%pretax = net1%tu%pretax - net2%tu%pretax
        net%tu%nettax = net1%tu%nettax - net2%tu%nettax
        net%tu%chcaresub = net1%tu%chcaresub - net2%tu%chcaresub
        net%tu%fsm = net1%tu%fsm - net2%tu%fsm
        net%tu%totben = net1%tu%totben - net2%tu%totben

    end function net_minus_net


    ! net_times_scalar
    ! ------------------------------------
    ! Multiply net_t type variable by scalar

    function net_times_scalar(net1, scalar) result(net)
        implicit none
        type(net_t), intent(in) :: net1
        real(dp), intent(in) :: scalar
        type(net_t) :: net

        net%ad(1)%taxable = net1%ad(1)%taxable * scalar
        net%ad(1)%inctax = net1%ad(1)%inctax * scalar
        net%ad(1)%natins = net1%ad(1)%natins * scalar
        net%ad(1)%natinsc1 = net1%ad(1)%natinsc1 * scalar
        net%ad(1)%natinsc2 = net1%ad(1)%natinsc2 * scalar
        net%ad(1)%natinsc4 = net1%ad(1)%natinsc4 * scalar
        net%ad(1)%pretaxearn = net1%ad(1)%pretaxearn * scalar
        net%ad(1)%posttaxearn = net1%ad(1)%posttaxearn * scalar
        net%ad(2)%taxable = net1%ad(2)%taxable * scalar
        net%ad(2)%inctax = net1%ad(2)%inctax * scalar
        net%ad(2)%natins = net1%ad(2)%natins * scalar
        net%ad(2)%natinsc1 = net1%ad(2)%natinsc1 * scalar
        net%ad(2)%natinsc2 = net1%ad(2)%natinsc2 * scalar
        net%ad(2)%natinsc4 = net1%ad(2)%natinsc4 * scalar
        net%ad(2)%pretaxearn = net1%ad(2)%pretaxearn * scalar
        net%ad(2)%posttaxearn = net1%ad(2)%posttaxearn * scalar

        net%tu%pretaxearn = net1%tu%pretaxearn * scalar
        net%tu%posttaxearn = net1%tu%posttaxearn * scalar
        net%tu%chben = net1%tu%chben * scalar
        net%tu%matgrant = net1%tu%matgrant * scalar
        net%tu%fc = net1%tu%fc * scalar
        net%tu%wtc = net1%tu%wtc * scalar
        net%tu%ctc = net1%tu%ctc * scalar
        net%tu%ccexp = net1%tu%ccexp * scalar
        net%tu%cctaxrefund = net1%tu%cctaxrefund * scalar
        net%tu%incsup = net1%tu%incsup * scalar
        net%tu%hben = net1%tu%hben * scalar
        net%tu%polltax = net1%tu%polltax * scalar
        net%tu%polltaxben = net1%tu%polltaxben * scalar
        net%tu%ctax = net1%tu%ctax * scalar
        net%tu%ctaxben = net1%tu%ctaxben * scalar
        net%tu%maxuc = net1%tu%maxuc * scalar
        net%tu%uc = net1%tu%uc * scalar
        net%tu%dispinc = net1%tu%dispinc * scalar
        net%tu%pretax = net1%tu%pretax * scalar
        net%tu%nettax = net1%tu%nettax * scalar
        net%tu%chcaresub = net1%tu%chcaresub * scalar
        net%tu%fsm = net1%tu%fsm * scalar
        net%tu%totben = net1%tu%totben * scalar

    end function net_times_scalar


    ! net_times_scalar_integer
    ! ------------------------------------
    ! Multiply net_t type variable by an integer scalar

    function net_times_scalar_integer(net1, int_scalar) result(net)
        implicit none
        type(net_t), intent(in) :: net1
        integer, intent(in) :: int_scalar
        type(net_t) :: net
        net = net_times_scalar(net1, real(int_scalar, dp))
    end function net_times_scalar_integer


    ! scalar_times_net
    ! ------------------------------------
    ! Multiply net_t type variable by scalar

    function scalar_times_net(scalar, net2) result(net)
        implicit none
        real(dp), intent(in) :: scalar
        type(net_t), intent(in) :: net2
        type(net_t) :: net

        net%ad(1)%taxable = scalar * net2%ad(1)%taxable
        net%ad(1)%inctax = scalar * net2%ad(1)%inctax
        net%ad(1)%natins = scalar * net2%ad(1)%natins
        net%ad(1)%natinsc1 = scalar * net2%ad(1)%natinsc1
        net%ad(1)%natinsc2 = scalar * net2%ad(1)%natinsc2
        net%ad(1)%natinsc4 = scalar * net2%ad(1)%natinsc4
        net%ad(1)%pretaxearn = scalar * net2%ad(1)%pretaxearn
        net%ad(1)%posttaxearn = scalar * net2%ad(1)%posttaxearn
        net%ad(2)%taxable = scalar * net2%ad(2)%taxable
        net%ad(2)%inctax = scalar * net2%ad(2)%inctax
        net%ad(2)%natins = scalar * net2%ad(2)%natins
        net%ad(2)%natinsc1 = scalar * net2%ad(2)%natinsc1
        net%ad(2)%natinsc2 = scalar * net2%ad(2)%natinsc2
        net%ad(2)%natinsc4 = scalar * net2%ad(2)%natinsc4
        net%ad(2)%pretaxearn = scalar * net2%ad(2)%pretaxearn
        net%ad(2)%posttaxearn = scalar * net2%ad(2)%posttaxearn

        net%tu%pretaxearn = scalar * net2%tu%pretaxearn
        net%tu%posttaxearn = scalar * net2%tu%posttaxearn
        net%tu%chben = scalar * net2%tu%chben
        net%tu%matgrant = scalar * net2%tu%matgrant
        net%tu%fc = scalar * net2%tu%fc
        net%tu%wtc = scalar * net2%tu%wtc
        net%tu%ctc = scalar * net2%tu%ctc
        net%tu%ccexp = scalar * net2%tu%ccexp
        net%tu%cctaxrefund = scalar * net2%tu%cctaxrefund
        net%tu%incsup = scalar * net2%tu%incsup
        net%tu%hben = scalar * net2%tu%hben
        net%tu%polltax = scalar * net2%tu%polltax
        net%tu%polltaxben = scalar * net2%tu%polltaxben
        net%tu%ctax = scalar * net2%tu%ctax
        net%tu%ctaxben = scalar * net2%tu%ctaxben
        net%tu%maxuc = scalar * net2%tu%maxuc
        net%tu%uc = scalar * net2%tu%uc
        net%tu%dispinc = scalar * net2%tu%dispinc
        net%tu%pretax = scalar * net2%tu%pretax
        net%tu%nettax = scalar * net2%tu%nettax
        net%tu%chcaresub = scalar * net2%tu%chcaresub
        net%tu%fsm = scalar * net2%tu%fsm
        net%tu%totben = scalar * net2%tu%totben

    end function scalar_times_net


    ! scalar_times_net_integer
    ! ------------------------------------
    ! Multiply net_t type variable by scalar

    function scalar_times_net_integer(int_scalar, net2) result(net)
        implicit none
        integer, intent(in) :: int_scalar
        type(net_t), intent(in) :: net2
        type(net_t) :: net
        net = scalar_times_net(real(int_scalar, dp), net2)
    end function scalar_times_net_integer


    ! net_div_scalar
    ! ------------------------------------
    ! Divide net_t type variable by scalar

    function net_div_scalar(net1, scalar) result(net)
        implicit none
        type(net_t), intent(in) :: net1
        real(dp), intent(in) :: scalar
        type(net_t) :: net

        net%ad(1)%taxable = net1%ad(1)%taxable / scalar
        net%ad(1)%inctax = net1%ad(1)%inctax / scalar
        net%ad(1)%natins = net1%ad(1)%natins / scalar
        net%ad(1)%natinsc1 = net1%ad(1)%natinsc1 / scalar
        net%ad(1)%natinsc2 = net1%ad(1)%natinsc2 / scalar
        net%ad(1)%natinsc4 = net1%ad(1)%natinsc4 / scalar
        net%ad(1)%pretaxearn = net1%ad(1)%pretaxearn / scalar
        net%ad(1)%posttaxearn = net1%ad(1)%posttaxearn / scalar
        net%ad(2)%taxable = net1%ad(2)%taxable / scalar
        net%ad(2)%inctax = net1%ad(2)%inctax / scalar
        net%ad(2)%natins = net1%ad(2)%natins / scalar
        net%ad(2)%natinsc1 = net1%ad(2)%natinsc1 / scalar
        net%ad(2)%natinsc2 = net1%ad(2)%natinsc2 / scalar
        net%ad(2)%natinsc4 = net1%ad(2)%natinsc4 / scalar
        net%ad(2)%pretaxearn = net1%ad(2)%pretaxearn / scalar
        net%ad(2)%posttaxearn = net1%ad(2)%posttaxearn / scalar

        net%tu%pretaxearn = net1%tu%pretaxearn / scalar
        net%tu%posttaxearn = net1%tu%posttaxearn / scalar
        net%tu%chben = net1%tu%chben / scalar
        net%tu%matgrant = net1%tu%matgrant / scalar
        net%tu%fc = net1%tu%fc / scalar
        net%tu%wtc = net1%tu%wtc / scalar
        net%tu%ctc = net1%tu%ctc / scalar
        net%tu%ccexp = net1%tu%ccexp / scalar
        net%tu%cctaxrefund = net1%tu%cctaxrefund / scalar
        net%tu%incsup = net1%tu%incsup / scalar
        net%tu%hben = net1%tu%hben / scalar
        net%tu%polltax = net1%tu%polltax / scalar
        net%tu%polltaxben = net1%tu%polltaxben / scalar
        net%tu%ctax = net1%tu%ctax / scalar
        net%tu%ctaxben = net1%tu%ctaxben / scalar
        net%tu%maxuc = net1%tu%maxuc / scalar
        net%tu%uc = net1%tu%uc / scalar
        net%tu%dispinc = net1%tu%dispinc / scalar
        net%tu%pretax = net1%tu%pretax / scalar
        net%tu%nettax = net1%tu%nettax / scalar
        net%tu%chcaresub = net1%tu%chcaresub / scalar
        net%tu%fsm = net1%tu%fsm / scalar
        net%tu%totben = net1%tu%totben / scalar

    end function net_div_scalar


    ! net_div_scalar_integer
    ! ------------------------------------
    ! Divide net_t type variable by scalar

    function net_div_scalar_integer(net1, int_scalar) result(net)
        implicit none
        type(net_t), intent(in) :: net1
        integer, intent(in) :: int_scalar
        type(net_t) :: net
        net = net_div_scalar(net1, real(int_scalar, dp))
    end function net_div_scalar_integer


    ! net_init
    ! -----------------------------------------------------------------------
    ! intializes net_t type. unless defaults are coded here, integers are
    ! set to 0, doubles to 0.0_dp and logicals to .false. (and similarly
    ! for arrays)

    !DEC$ ATTRIBUTES FORCEINLINE :: net_init
    elemental subroutine net_init(net)

        implicit none

        type(net_t), intent(out) :: net
        integer :: ad

        net%tu%pretaxearn = 0.0_dp
        net%tu%posttaxearn = 0.0_dp
        net%tu%chben = 0.0_dp
        net%tu%matgrant = 0.0_dp
        net%tu%fc = 0.0_dp
        net%tu%wtc = 0.0_dp
        net%tu%ctc = 0.0_dp
        net%tu%ccexp = 0.0_dp
        net%tu%cctaxrefund = 0.0_dp
        net%tu%incsup = 0.0_dp
        net%tu%hben = 0.0_dp
        net%tu%polltax = 0.0_dp
        net%tu%polltaxben = 0.0_dp
        net%tu%ctax = 0.0_dp
        net%tu%ctaxben = 0.0_dp
        net%tu%maxuc = 0.0_dp
        net%tu%uc = 0.0_dp
        net%tu%dispinc = 0.0_dp
        net%tu%pretax = 0.0_dp
        net%tu%nettax = 0.0_dp
        net%tu%chcaresub = 0.0_dp
        net%tu%fsm = 0.0_dp
        net%tu%totben = 0.0_dp
        net%ad(1)%taxable = 0.0_dp
        net%ad(1)%inctax = 0.0_dp
        net%ad(1)%natins = 0.0_dp
        net%ad(1)%natinsc1 = 0.0_dp
        net%ad(1)%natinsc2 = 0.0_dp
        net%ad(1)%natinsc4 = 0.0_dp
        net%ad(1)%pretaxearn = 0.0_dp
        net%ad(1)%posttaxearn = 0.0_dp
        net%ad(2)%taxable = 0.0_dp
        net%ad(2)%inctax = 0.0_dp
        net%ad(2)%natins = 0.0_dp
        net%ad(2)%natinsc1 = 0.0_dp
        net%ad(2)%natinsc2 = 0.0_dp
        net%ad(2)%natinsc4 = 0.0_dp
        net%ad(2)%pretaxearn = 0.0_dp
        net%ad(2)%posttaxearn = 0.0_dp

    end subroutine net_init


    ! net_desc
    ! -----------------------------------------------------------------------
    ! will display the information contained in the net income structure and
    ! write this to a file if fname is specified

    subroutine net_desc(net, fname)

        use fortax_util, only : intToStr, strCentre, fortaxError

        use, intrinsic :: iso_fortran_env

        type(net_t), intent(in) :: net
        character(len = *), optional   :: fname

        integer :: funit, i, ios

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit, *)
        write(funit, '(A)') repeat("=", 62)
        write(funit, '(A)') strCentre('net_desc (TAX UNIT):', 62)
        write(funit, '(A)') repeat("=", 62)
        call desc_f90(funit, "Pre-tax earnings", "pretaxearn", net%tu%pretaxearn)
        call desc_f90(funit, "Post-tax earnings", "posttaxearn", net%tu%posttaxearn)
        call desc_f90(funit, "Child benefit", "chben", net%tu%chben)
        call desc_f90(funit, "Maternity grant", "matgrant", net%tu%matgrant)
        call desc_f90(funit, "Family Credit/WFTC", "fc", net%tu%fc)
        call desc_f90(funit, "Working Tax Credit", "wtc", net%tu%wtc)
        call desc_f90(funit, "Child Tax Credit", "ctc", net%tu%ctc)
        call desc_f90(funit, "Childcare expenditure", "ccexp", net%tu%ccexp)
        call desc_f90(funit, "Childcare tax refund", "cctaxrefund", net%tu%cctaxrefund)
        call desc_f90(funit, "Income Support", "incsup", net%tu%incsup)
        call desc_f90(funit, "Housing Benefit", "hben", net%tu%hben)
        call desc_f90(funit, "Community Charge", "polltax", net%tu%polltax)
        call desc_f90(funit, "Community Charge Benefit", "polltaxben", net%tu%polltaxben)
        call desc_f90(funit, "Council Tax", "ctax", net%tu%ctax)
        call desc_f90(funit, "Council Tax Benefit", "ctaxben", net%tu%ctaxben)
        call desc_f90(funit, "Universal Credit maximum award", "maxuc", net%tu%maxuc)
        call desc_f90(funit, "Universal Credit", "uc", net%tu%uc)
        call desc_f90(funit, "Disposable income", "dispinc", net%tu%dispinc)
        call desc_f90(funit, "Pre-tax income", "pretax", net%tu%pretax)
        call desc_f90(funit, "Total net tax", "nettax", net%tu%nettax)
        call desc_f90(funit, "Childcare subsidy", "chcaresub", net%tu%chcaresub)
        call desc_f90(funit, "Free school meals value", "fsm", net%tu%fsm)
        call desc_f90(funit, "Total benefits and Tax Credits", "totben", net%tu%totben)
        write(funit, '(A)') repeat("=", 62)

        ! write(funit, *)
        ! write(funit, '(A)') repeat("=", 62)
        write(funit, '(A)') strCentre('net_desc (ADULT 1):', 62)
        write(funit, '(A)') repeat("=", 62)
        call desc_f90(funit, "Taxable income", "taxable", net%ad(1)%taxable)
        call desc_f90(funit, "Income tax", "inctax", net%ad(1)%inctax)
        call desc_f90(funit, "National Insurance", "natins", net%ad(1)%natins)
        call desc_f90(funit, "National Insurance, class 1", "natinsc1", net%ad(1)%natinsc1)
        call desc_f90(funit, "National Insurance, class 2", "natinsc2", net%ad(1)%natinsc2)
        call desc_f90(funit, "National Insurance, class 4", "natinsc4", net%ad(1)%natinsc4)
        call desc_f90(funit, "Pre-tax earnings", "pretaxearn", net%ad(1)%pretaxearn)
        call desc_f90(funit, "Post-tax earnings", "posttaxearn", net%ad(1)%posttaxearn)
        write(funit, '(A)') repeat("=", 62)

        ! write(funit, *)
        ! write(funit, '(A)') repeat("=", 62)
        write(funit, '(A)') strCentre('net_desc (ADULT 2):', 62)
        write(funit, '(A)') repeat("=", 62)
        call desc_f90(funit, "Taxable income", "taxable", net%ad(2)%taxable)
        call desc_f90(funit, "Income tax", "inctax", net%ad(2)%inctax)
        call desc_f90(funit, "National Insurance", "natins", net%ad(2)%natins)
        call desc_f90(funit, "National Insurance, class 1", "natinsc1", net%ad(2)%natinsc1)
        call desc_f90(funit, "National Insurance, class 2", "natinsc2", net%ad(2)%natinsc2)
        call desc_f90(funit, "National Insurance, class 4", "natinsc4", net%ad(2)%natinsc4)
        call desc_f90(funit, "Pre-tax earnings", "pretaxearn", net%ad(2)%pretaxearn)
        call desc_f90(funit, "Post-tax earnings", "posttaxearn", net%ad(2)%posttaxearn)
        write(funit, '(A)') repeat("=", 62)
        close(funit)

    end subroutine net_desc


    ! sys_init
    ! -----------------------------------------------------------------------
    ! intializes sys_t type. unless defaults are coded here, integers are
    ! set to 0, doubles to 0.0_dp and logicals to .false. (and similarly
    ! for arrays)

    elemental subroutine sys_init(sys)

        implicit none

        type(sys_t), intent(out) :: sys

        sys%sysname = ''
        sys%sysdesc = ''

        sys%inctax%numbands = 0
        sys%inctax%pa = 0.0_dp
        sys%inctax%doPATaper = 0
        sys%inctax%disablePATaperRounding = 0
        sys%inctax%paTaperThresh = 0.0_dp
        sys%inctax%paTaperRate = 0.0_dp
        sys%inctax%doTPA = 0
        sys%inctax%maxTPA = 0.0_dp
        sys%inctax%mma = 0.0_dp
        sys%inctax%ctc = 0.0_dp
        sys%inctax%ctcyng = 0.0_dp
        sys%inctax%mmarate = 0.0_dp
        sys%inctax%ctctaper = 0.0_dp
        sys%inctax%c4rebate = 0.0_dp
        sys%inctax%bands = 0.0_dp
        sys%inctax%rates = 0.0_dp

        sys%natins%numrates = 0
        sys%natins%c4nrates = 0
        sys%natins%c2floor = 0.0_dp
        sys%natins%c2rate = 0.0_dp
        sys%natins%ceiling = 0.0_dp
        sys%natins%rates = 0.0_dp
        sys%natins%bands = 0.0_dp
        sys%natins%c4rates = 0.0_dp
        sys%natins%c4bands = 0.0_dp

        sys%chben%doChBen = 0
        sys%chben%basic = 0.0_dp
        sys%chben%kid1xtr = 0.0_dp
        sys%chben%opf = 0.0_dp
        sys%chben%MatGrantVal = 0.0_dp
        sys%chben%MatGrantOnlyFirstKid = 0
        sys%chben%doTaper = 0
        sys%chben%disableTaperRounding = 0
        sys%chben%taperStart = 0.0_dp
        sys%chben%taperRate = 0.0_dp
        sys%chben%taperIsIncTax = 0

        sys%fc%dofamcred = 0
        sys%fc%NumAgeRng = 0
        sys%fc%MaxAgeCC = 0
        sys%fc%WFTCMaxAgeCC = 0
        sys%fc%adult = 0.0_dp
        sys%fc%ftprem = 0.0_dp
        sys%fc%hours1 = 0.0_dp
        sys%fc%hours2 = 0.0_dp
        sys%fc%thres = 0.0_dp
        sys%fc%taper = 0.0_dp
        sys%fc%MaintDisreg = 0.0_dp
        sys%fc%MaxCC1 = 0.0_dp
        sys%fc%MaxCC2 = 0.0_dp
        sys%fc%WFTCMaxCC1 = 0.0_dp
        sys%fc%WFTCMaxCC2 = 0.0_dp
        sys%fc%WFTCPropCC = 0.0_dp
        sys%fc%MinAmt = 0.0_dp
        sys%fc%kidagel = 0
        sys%fc%kidageu = 0
        sys%fc%kidcred = 0.0_dp

        sys%ctc%fam = 0.0_dp
        sys%ctc%baby = 0.0_dp
        sys%ctc%kid = 0.0_dp
        sys%ctc%maxKids = 0

        sys%wtc%Basic = 0.0_dp
        sys%wtc%CouLP = 0.0_dp
        sys%wtc%FT = 0.0_dp
        sys%wtc%MinHrsKids = 0.0_dp
        sys%wtc%MinHrsCouKids = 0.0_dp
        sys%wtc%MinHrsNoKids = 0.0_dp
        sys%wtc%FTHrs = 0.0_dp
        sys%wtc%MinAgeKids = 0
        sys%wtc%MinAgeNoKids = 0
        sys%wtc%MaxCC1 = 0.0_dp
        sys%wtc%MaxCC2 = 0.0_dp
        sys%wtc%PropCC = 0.0_dp
        sys%wtc%MaxAgeCC = 0
        sys%wtc%NewDisreg = 0.0_dp
        sys%wtc%NewDisregCon = 0

        sys%ntc%donewtaxcred = 0
        sys%ntc%thr1lo = 0.0_dp
        sys%ntc%thr1hi = 0.0_dp
        sys%ntc%thr2 = 0.0_dp
        sys%ntc%taper1 = 0.0_dp
        sys%ntc%taper2 = 0.0_dp
        sys%ntc%taperCTCInOneGo = 0
        sys%ntc%MinAmt = 0.0_dp

        sys%cctaxrefund%doCCTaxRefund = 0
        sys%cctaxrefund%MaxPerChild = 0.0_dp
        sys%cctaxrefund%MaxAge = 0
        sys%cctaxrefund%ReceiptProp = 0.0_dp
        sys%cctaxrefund%MinEarn = 0.0_dp
        sys%cctaxrefund%MaxInc = 0.0_dp

        sys%incsup%doIncSup = 0
        sys%incsup%IncChben = 0
        sys%incsup%NumAgeRng = 0
        sys%incsup%MinAgeMain = 0
        sys%incsup%MinAgeMainSin = 0
        sys%incsup%MainCou = 0.0_dp
        sys%incsup%YngCou = 0.0_dp
        sys%incsup%MainLP = 0.0_dp
        sys%incsup%YngLP = 0.0_dp
        sys%incsup%MainSin = 0.0_dp
        sys%incsup%YngSin = 0.0_dp
        sys%incsup%MinAgeFSM = 0
        sys%incsup%MaxAgeUniversalFSM = 0
        sys%incsup%ValFSM = 0.0_dp
        sys%incsup%DisregLP = 0.0_dp
        sys%incsup%DisregSin = 0.0_dp
        sys%incsup%DisregCou = 0.0_dp
        sys%incsup%DisregShared = 0
        sys%incsup%PremFam = 0.0_dp
        sys%incsup%PremLP = 0.0_dp
        sys%incsup%hours = 0.0_dp
        sys%incsup%MaintDisreg = 0.0_dp
        sys%incsup%AgeRngl = 0
        sys%incsup%AgeRngu = 0
        sys%incsup%AddKid = 0.0_dp

        sys%ctax%docounciltax = 0
        sys%ctax%bandD = 0.0_dp
        sys%ctax%SinDis = 0.0_dp
        sys%ctax%EnglandRatioA = 0.0_dp
        sys%ctax%EnglandRatioB = 0.0_dp
        sys%ctax%EnglandRatioC = 0.0_dp
        sys%ctax%EnglandRatioE = 0.0_dp
        sys%ctax%EnglandRatioF = 0.0_dp
        sys%ctax%EnglandRatioG = 0.0_dp
        sys%ctax%EnglandRatioH = 0.0_dp
        sys%ctax%EnglandRatioI = 0.0_dp
        sys%ctax%ScotlandRatioA = 0.0_dp
        sys%ctax%ScotlandRatioB = 0.0_dp
        sys%ctax%ScotlandRatioC = 0.0_dp
        sys%ctax%ScotlandRatioE = 0.0_dp
        sys%ctax%ScotlandRatioF = 0.0_dp
        sys%ctax%ScotlandRatioG = 0.0_dp
        sys%ctax%ScotlandRatioH = 0.0_dp
        sys%ctax%ScotlandRatioI = 0.0_dp
        sys%ctax%WalesRatioA = 0.0_dp
        sys%ctax%WalesRatioB = 0.0_dp
        sys%ctax%WalesRatioC = 0.0_dp
        sys%ctax%WalesRatioE = 0.0_dp
        sys%ctax%WalesRatioF = 0.0_dp
        sys%ctax%WalesRatioG = 0.0_dp
        sys%ctax%WalesRatioH = 0.0_dp
        sys%ctax%WalesRatioI = 0.0_dp

        sys%rebatesys%RulesUnderFC = 0
        sys%rebatesys%RulesUnderWFTC = 0
        sys%rebatesys%RulesUnderNTC = 0
        sys%rebatesys%RulesUnderUC = 0
        sys%rebatesys%NumAgeRng = 0
        sys%rebatesys%Restrict = 0
        sys%rebatesys%docap = 0
        sys%rebatesys%MinAgeMain = 0
        sys%rebatesys%MinAgeMainSin = 0
        sys%rebatesys%MainCou = 0.0_dp
        sys%rebatesys%YngCou = 0.0_dp
        sys%rebatesys%MainLP = 0.0_dp
        sys%rebatesys%YngLP = 0.0_dp
        sys%rebatesys%MainSin = 0.0_dp
        sys%rebatesys%YngSin = 0.0_dp
        sys%rebatesys%DisregSin = 0.0_dp
        sys%rebatesys%DisregLP = 0.0_dp
        sys%rebatesys%DisregCou = 0.0_dp
        sys%rebatesys%CredInDisregCC = 0
        sys%rebatesys%ChbenIsIncome = 0
        sys%rebatesys%PremFam = 0.0_dp
        sys%rebatesys%PremLP = 0.0_dp
        sys%rebatesys%MaintDisreg = 0.0_dp
        sys%rebatesys%MaxCC1 = 0.0_dp
        sys%rebatesys%MaxCC2 = 0.0_dp
        sys%rebatesys%MaxAgeCC = 0
        sys%rebatesys%AgeRngl = 0
        sys%rebatesys%AgeRngu = 0
        sys%rebatesys%AddKid = 0.0_dp
        sys%rebatesys%MaxKids = 0

        sys%hben%doHBen = 0
        sys%hben%taper = 0.0_dp
        sys%hben%MinAmt = 0.0_dp
        sys%hben%doUnderOccCharge = 0
        sys%hben%doUnderOccChargeScotland = 0
        sys%hben%doUnderOccChargeNI = 0
        sys%hben%numUnderOccBands = 0
        sys%hben%underOccRates = 0.0_dp
        sys%hben%doLHA = 0
        sys%hben%LHASharedAccAge = 0
        sys%hben%numLHABands = 0
        sys%hben%LHARates = 0.0_dp

        sys%ctaxben%docounciltaxben = 0
        sys%ctaxben%taper = 0.0_dp
        sys%ctaxben%doEntitlementCut = 0
        sys%ctaxben%entitlementShare = 0.0_dp

        sys%ccben%dopolltax = 0
        sys%ccben%taper = 0.0_dp
        sys%ccben%PropElig = 0.0_dp
        sys%ccben%MinAmt = 0.0_dp
        sys%ccben%CCrate = 0.0_dp

        sys%uc%doUnivCred = 0
        sys%uc%MainCou = 0.0_dp
        sys%uc%YngCou = 0.0_dp
        sys%uc%MainSin = 0.0_dp
        sys%uc%YngSin = 0.0_dp
        sys%uc%MinAgeMain = 0
        sys%uc%FirstKid = 0.0_dp
        sys%uc%OtherKid = 0.0_dp
        sys%uc%MaxKids = 0
        sys%uc%MaxCC1 = 0.0_dp
        sys%uc%MaxCC2 = 0.0_dp
        sys%uc%PropCC = 0.0_dp
        sys%uc%MaxAgeCC = 0
        sys%uc%doRentCap = 0
        sys%uc%DisregSinNoKidsHi = 0.0_dp
        sys%uc%DisregSinNoKidsLo = 0.0_dp
        sys%uc%DisregSinKidsHi = 0.0_dp
        sys%uc%DisregSinKidsLo = 0.0_dp
        sys%uc%DisregCouNoKidsHi = 0.0_dp
        sys%uc%DisregCouNoKidsLo = 0.0_dp
        sys%uc%DisregCouKidsHi = 0.0_dp
        sys%uc%DisregCouKidsLo = 0.0_dp
        sys%uc%taper = 0.0_dp
        sys%uc%MinAmt = 0.0_dp

        sys%statepen%doStatePen = 0
        sys%statepen%PenAgeMan = 0
        sys%statepen%PenAgeWoman = 0

        sys%bencap%doCap = 0
        sys%bencap%doNI = 0
        sys%bencap%doThruUC = 0
        sys%bencap%sinNoKids = 0.0_dp
        sys%bencap%sinKids = 0.0_dp
        sys%bencap%couNoKids = 0.0_dp
        sys%bencap%couKids = 0.0_dp
        sys%bencap%LondonCapAmt = 0
        sys%bencap%LondonSinNoKids = 0.0_dp
        sys%bencap%LondonSinKids = 0.0_dp
        sys%bencap%LondonCouNoKids = 0.0_dp
        sys%bencap%LondonCouKids = 0.0_dp
        sys%bencap%UCEarnThr = 0.0_dp

        sys%extra%fsminappamt = 0
        sys%extra%matgrant = 0
        sys%extra%prices = 0

    end subroutine sys_init


    ! sys_saveF90
    ! -----------------------------------------------------------------------
    ! write as a file the contents of the system file which can then be
    ! directly included in the source code of the calling program

    subroutine sys_saveF90(sys, fname)

        use fortax_util, only : fortaxError
        use, intrinsic :: iso_fortran_env

        implicit none

        type(sys_t), intent(in) :: sys
        character(len = *), intent(in), optional :: fname

        integer :: funit, ios

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit, '(A)') '! .f90 FORTAX system; generated using sys_saveF90'
        write(funit, *)

        write(funit, '(A)') 'call sys_init(sys)'

        write(funit, *)
        write(funit, '(A)') "! inctax"
        call write_f90(funit, "sys%inctax%numbands", sys%inctax%numbands)
        call write_f90(funit, "sys%inctax%pa", sys%inctax%pa)
        call write_f90(funit, "sys%inctax%doPATaper", sys%inctax%doPATaper)
        call write_f90(funit, "sys%inctax%disablePATaperRounding", sys%inctax%disablePATaperRounding)
        call write_f90(funit, "sys%inctax%paTaperThresh", sys%inctax%paTaperThresh)
        call write_f90(funit, "sys%inctax%paTaperRate", sys%inctax%paTaperRate)
        call write_f90(funit, "sys%inctax%doTPA", sys%inctax%doTPA)
        call write_f90(funit, "sys%inctax%maxTPA", sys%inctax%maxTPA)
        call write_f90(funit, "sys%inctax%mma", sys%inctax%mma)
        call write_f90(funit, "sys%inctax%ctc", sys%inctax%ctc)
        call write_f90(funit, "sys%inctax%ctcyng", sys%inctax%ctcyng)
        call write_f90(funit, "sys%inctax%mmarate", sys%inctax%mmarate)
        call write_f90(funit, "sys%inctax%ctctaper", sys%inctax%ctctaper)
        call write_f90(funit, "sys%inctax%c4rebate", sys%inctax%c4rebate)
        call write_f90(funit, "sys%inctax%bands", sys%inctax%bands, sys%inctax%numbands)
        call write_f90(funit, "sys%inctax%rates", sys%inctax%rates, sys%inctax%numbands)

        write(funit, *)
        write(funit, '(A)') "! natins"
        call write_f90(funit, "sys%natins%numrates", sys%natins%numrates)
        call write_f90(funit, "sys%natins%c4nrates", sys%natins%c4nrates)
        call write_f90(funit, "sys%natins%c2floor", sys%natins%c2floor)
        call write_f90(funit, "sys%natins%c2rate", sys%natins%c2rate)
        call write_f90(funit, "sys%natins%ceiling", sys%natins%ceiling)
        call write_f90(funit, "sys%natins%rates", sys%natins%rates, sys%natins%numrates)
        call write_f90(funit, "sys%natins%bands", sys%natins%bands, sys%natins%numrates)
        call write_f90(funit, "sys%natins%c4rates", sys%natins%c4rates, sys%natins%c4nrates)
        call write_f90(funit, "sys%natins%c4bands", sys%natins%c4bands, sys%natins%c4nrates)

        write(funit, *)
        write(funit, '(A)') "! chben"
        call write_f90(funit, "sys%chben%doChBen", sys%chben%doChBen)
        call write_f90(funit, "sys%chben%basic", sys%chben%basic)
        call write_f90(funit, "sys%chben%kid1xtr", sys%chben%kid1xtr)
        call write_f90(funit, "sys%chben%opf", sys%chben%opf)
        call write_f90(funit, "sys%chben%MatGrantVal", sys%chben%MatGrantVal)
        call write_f90(funit, "sys%chben%MatGrantOnlyFirstKid", sys%chben%MatGrantOnlyFirstKid)
        call write_f90(funit, "sys%chben%doTaper", sys%chben%doTaper)
        call write_f90(funit, "sys%chben%disableTaperRounding", sys%chben%disableTaperRounding)
        call write_f90(funit, "sys%chben%taperStart", sys%chben%taperStart)
        call write_f90(funit, "sys%chben%taperRate", sys%chben%taperRate)
        call write_f90(funit, "sys%chben%taperIsIncTax", sys%chben%taperIsIncTax)

        write(funit, *)
        write(funit, '(A)') "! fc"
        call write_f90(funit, "sys%fc%dofamcred", sys%fc%dofamcred)
        call write_f90(funit, "sys%fc%NumAgeRng", sys%fc%NumAgeRng)
        call write_f90(funit, "sys%fc%MaxAgeCC", sys%fc%MaxAgeCC)
        call write_f90(funit, "sys%fc%WFTCMaxAgeCC", sys%fc%WFTCMaxAgeCC)
        call write_f90(funit, "sys%fc%adult", sys%fc%adult)
        call write_f90(funit, "sys%fc%ftprem", sys%fc%ftprem)
        call write_f90(funit, "sys%fc%hours1", sys%fc%hours1)
        call write_f90(funit, "sys%fc%hours2", sys%fc%hours2)
        call write_f90(funit, "sys%fc%thres", sys%fc%thres)
        call write_f90(funit, "sys%fc%taper", sys%fc%taper)
        call write_f90(funit, "sys%fc%MaintDisreg", sys%fc%MaintDisreg)
        call write_f90(funit, "sys%fc%MaxCC1", sys%fc%MaxCC1)
        call write_f90(funit, "sys%fc%MaxCC2", sys%fc%MaxCC2)
        call write_f90(funit, "sys%fc%WFTCMaxCC1", sys%fc%WFTCMaxCC1)
        call write_f90(funit, "sys%fc%WFTCMaxCC2", sys%fc%WFTCMaxCC2)
        call write_f90(funit, "sys%fc%WFTCPropCC", sys%fc%WFTCPropCC)
        call write_f90(funit, "sys%fc%MinAmt", sys%fc%MinAmt)
        call write_f90(funit, "sys%fc%kidagel", sys%fc%kidagel, sys%fc%NumAgeRng)
        call write_f90(funit, "sys%fc%kidageu", sys%fc%kidageu, sys%fc%NumAgeRng)
        call write_f90(funit, "sys%fc%kidcred", sys%fc%kidcred, sys%fc%NumAgeRng)

        write(funit, *)
        write(funit, '(A)') "! ctc"
        call write_f90(funit, "sys%ctc%fam", sys%ctc%fam)
        call write_f90(funit, "sys%ctc%baby", sys%ctc%baby)
        call write_f90(funit, "sys%ctc%kid", sys%ctc%kid)
        call write_f90(funit, "sys%ctc%maxKids", sys%ctc%maxKids)

        write(funit, *)
        write(funit, '(A)') "! wtc"
        call write_f90(funit, "sys%wtc%Basic", sys%wtc%Basic)
        call write_f90(funit, "sys%wtc%CouLP", sys%wtc%CouLP)
        call write_f90(funit, "sys%wtc%FT", sys%wtc%FT)
        call write_f90(funit, "sys%wtc%MinHrsKids", sys%wtc%MinHrsKids)
        call write_f90(funit, "sys%wtc%MinHrsCouKids", sys%wtc%MinHrsCouKids)
        call write_f90(funit, "sys%wtc%MinHrsNoKids", sys%wtc%MinHrsNoKids)
        call write_f90(funit, "sys%wtc%FTHrs", sys%wtc%FTHrs)
        call write_f90(funit, "sys%wtc%MinAgeKids", sys%wtc%MinAgeKids)
        call write_f90(funit, "sys%wtc%MinAgeNoKids", sys%wtc%MinAgeNoKids)
        call write_f90(funit, "sys%wtc%MaxCC1", sys%wtc%MaxCC1)
        call write_f90(funit, "sys%wtc%MaxCC2", sys%wtc%MaxCC2)
        call write_f90(funit, "sys%wtc%PropCC", sys%wtc%PropCC)
        call write_f90(funit, "sys%wtc%MaxAgeCC", sys%wtc%MaxAgeCC)
        call write_f90(funit, "sys%wtc%NewDisreg", sys%wtc%NewDisreg)
        call write_f90(funit, "sys%wtc%NewDisregCon", sys%wtc%NewDisregCon)

        write(funit, *)
        write(funit, '(A)') "! ntc"
        call write_f90(funit, "sys%ntc%donewtaxcred", sys%ntc%donewtaxcred)
        call write_f90(funit, "sys%ntc%thr1lo", sys%ntc%thr1lo)
        call write_f90(funit, "sys%ntc%thr1hi", sys%ntc%thr1hi)
        call write_f90(funit, "sys%ntc%thr2", sys%ntc%thr2)
        call write_f90(funit, "sys%ntc%taper1", sys%ntc%taper1)
        call write_f90(funit, "sys%ntc%taper2", sys%ntc%taper2)
        call write_f90(funit, "sys%ntc%taperCTCInOneGo", sys%ntc%taperCTCInOneGo)
        call write_f90(funit, "sys%ntc%MinAmt", sys%ntc%MinAmt)

        write(funit, *)
        write(funit, '(A)') "! cctaxrefund"
        call write_f90(funit, "sys%cctaxrefund%doCCTaxRefund", sys%cctaxrefund%doCCTaxRefund)
        call write_f90(funit, "sys%cctaxrefund%MaxPerChild", sys%cctaxrefund%MaxPerChild)
        call write_f90(funit, "sys%cctaxrefund%MaxAge", sys%cctaxrefund%MaxAge)
        call write_f90(funit, "sys%cctaxrefund%ReceiptProp", sys%cctaxrefund%ReceiptProp)
        call write_f90(funit, "sys%cctaxrefund%MinEarn", sys%cctaxrefund%MinEarn)
        call write_f90(funit, "sys%cctaxrefund%MaxInc", sys%cctaxrefund%MaxInc)

        write(funit, *)
        write(funit, '(A)') "! incsup"
        call write_f90(funit, "sys%incsup%doIncSup", sys%incsup%doIncSup)
        call write_f90(funit, "sys%incsup%IncChben", sys%incsup%IncChben)
        call write_f90(funit, "sys%incsup%NumAgeRng", sys%incsup%NumAgeRng)
        call write_f90(funit, "sys%incsup%MinAgeMain", sys%incsup%MinAgeMain)
        call write_f90(funit, "sys%incsup%MinAgeMainSin", sys%incsup%MinAgeMainSin)
        call write_f90(funit, "sys%incsup%MainCou", sys%incsup%MainCou)
        call write_f90(funit, "sys%incsup%YngCou", sys%incsup%YngCou)
        call write_f90(funit, "sys%incsup%MainLP", sys%incsup%MainLP)
        call write_f90(funit, "sys%incsup%YngLP", sys%incsup%YngLP)
        call write_f90(funit, "sys%incsup%MainSin", sys%incsup%MainSin)
        call write_f90(funit, "sys%incsup%YngSin", sys%incsup%YngSin)
        call write_f90(funit, "sys%incsup%MinAgeFSM", sys%incsup%MinAgeFSM)
        call write_f90(funit, "sys%incsup%MaxAgeUniversalFSM", sys%incsup%MaxAgeUniversalFSM)
        call write_f90(funit, "sys%incsup%ValFSM", sys%incsup%ValFSM)
        call write_f90(funit, "sys%incsup%DisregLP", sys%incsup%DisregLP)
        call write_f90(funit, "sys%incsup%DisregSin", sys%incsup%DisregSin)
        call write_f90(funit, "sys%incsup%DisregCou", sys%incsup%DisregCou)
        call write_f90(funit, "sys%incsup%DisregShared", sys%incsup%DisregShared)
        call write_f90(funit, "sys%incsup%PremFam", sys%incsup%PremFam)
        call write_f90(funit, "sys%incsup%PremLP", sys%incsup%PremLP)
        call write_f90(funit, "sys%incsup%hours", sys%incsup%hours)
        call write_f90(funit, "sys%incsup%MaintDisreg", sys%incsup%MaintDisreg)
        call write_f90(funit, "sys%incsup%AgeRngl", sys%incsup%AgeRngl, sys%incsup%NumAgeRng)
        call write_f90(funit, "sys%incsup%AgeRngu", sys%incsup%AgeRngu, sys%incsup%NumAgeRng)
        call write_f90(funit, "sys%incsup%AddKid", sys%incsup%AddKid, sys%incsup%NumAgeRng)

        write(funit, *)
        write(funit, '(A)') "! ctax"
        call write_f90(funit, "sys%ctax%docounciltax", sys%ctax%docounciltax)
        call write_f90(funit, "sys%ctax%bandD", sys%ctax%bandD)
        call write_f90(funit, "sys%ctax%SinDis", sys%ctax%SinDis)
        call write_f90(funit, "sys%ctax%EnglandRatioA", sys%ctax%EnglandRatioA)
        call write_f90(funit, "sys%ctax%EnglandRatioB", sys%ctax%EnglandRatioB)
        call write_f90(funit, "sys%ctax%EnglandRatioC", sys%ctax%EnglandRatioC)
        call write_f90(funit, "sys%ctax%EnglandRatioE", sys%ctax%EnglandRatioE)
        call write_f90(funit, "sys%ctax%EnglandRatioF", sys%ctax%EnglandRatioF)
        call write_f90(funit, "sys%ctax%EnglandRatioG", sys%ctax%EnglandRatioG)
        call write_f90(funit, "sys%ctax%EnglandRatioH", sys%ctax%EnglandRatioH)
        call write_f90(funit, "sys%ctax%EnglandRatioI", sys%ctax%EnglandRatioI)
        call write_f90(funit, "sys%ctax%ScotlandRatioA", sys%ctax%ScotlandRatioA)
        call write_f90(funit, "sys%ctax%ScotlandRatioB", sys%ctax%ScotlandRatioB)
        call write_f90(funit, "sys%ctax%ScotlandRatioC", sys%ctax%ScotlandRatioC)
        call write_f90(funit, "sys%ctax%ScotlandRatioE", sys%ctax%ScotlandRatioE)
        call write_f90(funit, "sys%ctax%ScotlandRatioF", sys%ctax%ScotlandRatioF)
        call write_f90(funit, "sys%ctax%ScotlandRatioG", sys%ctax%ScotlandRatioG)
        call write_f90(funit, "sys%ctax%ScotlandRatioH", sys%ctax%ScotlandRatioH)
        call write_f90(funit, "sys%ctax%ScotlandRatioI", sys%ctax%ScotlandRatioI)
        call write_f90(funit, "sys%ctax%WalesRatioA", sys%ctax%WalesRatioA)
        call write_f90(funit, "sys%ctax%WalesRatioB", sys%ctax%WalesRatioB)
        call write_f90(funit, "sys%ctax%WalesRatioC", sys%ctax%WalesRatioC)
        call write_f90(funit, "sys%ctax%WalesRatioE", sys%ctax%WalesRatioE)
        call write_f90(funit, "sys%ctax%WalesRatioF", sys%ctax%WalesRatioF)
        call write_f90(funit, "sys%ctax%WalesRatioG", sys%ctax%WalesRatioG)
        call write_f90(funit, "sys%ctax%WalesRatioH", sys%ctax%WalesRatioH)
        call write_f90(funit, "sys%ctax%WalesRatioI", sys%ctax%WalesRatioI)

        write(funit, *)
        write(funit, '(A)') "! rebatesys"
        call write_f90(funit, "sys%rebatesys%RulesUnderFC", sys%rebatesys%RulesUnderFC)
        call write_f90(funit, "sys%rebatesys%RulesUnderWFTC", sys%rebatesys%RulesUnderWFTC)
        call write_f90(funit, "sys%rebatesys%RulesUnderNTC", sys%rebatesys%RulesUnderNTC)
        call write_f90(funit, "sys%rebatesys%RulesUnderUC", sys%rebatesys%RulesUnderUC)
        call write_f90(funit, "sys%rebatesys%NumAgeRng", sys%rebatesys%NumAgeRng)
        call write_f90(funit, "sys%rebatesys%Restrict", sys%rebatesys%Restrict)
        call write_f90(funit, "sys%rebatesys%docap", sys%rebatesys%docap)
        call write_f90(funit, "sys%rebatesys%MinAgeMain", sys%rebatesys%MinAgeMain)
        call write_f90(funit, "sys%rebatesys%MinAgeMainSin", sys%rebatesys%MinAgeMainSin)
        call write_f90(funit, "sys%rebatesys%MainCou", sys%rebatesys%MainCou)
        call write_f90(funit, "sys%rebatesys%YngCou", sys%rebatesys%YngCou)
        call write_f90(funit, "sys%rebatesys%MainLP", sys%rebatesys%MainLP)
        call write_f90(funit, "sys%rebatesys%YngLP", sys%rebatesys%YngLP)
        call write_f90(funit, "sys%rebatesys%MainSin", sys%rebatesys%MainSin)
        call write_f90(funit, "sys%rebatesys%YngSin", sys%rebatesys%YngSin)
        call write_f90(funit, "sys%rebatesys%DisregSin", sys%rebatesys%DisregSin)
        call write_f90(funit, "sys%rebatesys%DisregLP", sys%rebatesys%DisregLP)
        call write_f90(funit, "sys%rebatesys%DisregCou", sys%rebatesys%DisregCou)
        call write_f90(funit, "sys%rebatesys%CredInDisregCC", sys%rebatesys%CredInDisregCC)
        call write_f90(funit, "sys%rebatesys%ChbenIsIncome", sys%rebatesys%ChbenIsIncome)
        call write_f90(funit, "sys%rebatesys%PremFam", sys%rebatesys%PremFam)
        call write_f90(funit, "sys%rebatesys%PremLP", sys%rebatesys%PremLP)
        call write_f90(funit, "sys%rebatesys%MaintDisreg", sys%rebatesys%MaintDisreg)
        call write_f90(funit, "sys%rebatesys%MaxCC1", sys%rebatesys%MaxCC1)
        call write_f90(funit, "sys%rebatesys%MaxCC2", sys%rebatesys%MaxCC2)
        call write_f90(funit, "sys%rebatesys%MaxAgeCC", sys%rebatesys%MaxAgeCC)
        call write_f90(funit, "sys%rebatesys%AgeRngl", sys%rebatesys%AgeRngl, sys%rebatesys%NumAgeRng)
        call write_f90(funit, "sys%rebatesys%AgeRngu", sys%rebatesys%AgeRngu, sys%rebatesys%NumAgeRng)
        call write_f90(funit, "sys%rebatesys%AddKid", sys%rebatesys%AddKid, sys%rebatesys%NumAgeRng)
        call write_f90(funit, "sys%rebatesys%MaxKids", sys%rebatesys%MaxKids)

        write(funit, *)
        write(funit, '(A)') "! hben"
        call write_f90(funit, "sys%hben%doHBen", sys%hben%doHBen)
        call write_f90(funit, "sys%hben%taper", sys%hben%taper)
        call write_f90(funit, "sys%hben%MinAmt", sys%hben%MinAmt)
        call write_f90(funit, "sys%hben%doUnderOccCharge", sys%hben%doUnderOccCharge)
        call write_f90(funit, "sys%hben%doUnderOccChargeScotland", sys%hben%doUnderOccChargeScotland)
        call write_f90(funit, "sys%hben%doUnderOccChargeNI", sys%hben%doUnderOccChargeNI)
        call write_f90(funit, "sys%hben%numUnderOccBands", sys%hben%numUnderOccBands)
        call write_f90(funit, "sys%hben%underOccRates", sys%hben%underOccRates, sys%hben%numUnderOccBands)
        call write_f90(funit, "sys%hben%doLHA", sys%hben%doLHA)
        call write_f90(funit, "sys%hben%LHASharedAccAge", sys%hben%LHASharedAccAge)
        call write_f90(funit, "sys%hben%numLHABands", sys%hben%numLHABands)
        call write_f90(funit, "sys%hben%LHARates", sys%hben%LHARates, sys%hben%numLHABands)

        write(funit, *)
        write(funit, '(A)') "! ctaxben"
        call write_f90(funit, "sys%ctaxben%docounciltaxben", sys%ctaxben%docounciltaxben)
        call write_f90(funit, "sys%ctaxben%taper", sys%ctaxben%taper)
        call write_f90(funit, "sys%ctaxben%doEntitlementCut", sys%ctaxben%doEntitlementCut)
        call write_f90(funit, "sys%ctaxben%entitlementShare", sys%ctaxben%entitlementShare)

        write(funit, *)
        write(funit, '(A)') "! ccben"
        call write_f90(funit, "sys%ccben%dopolltax", sys%ccben%dopolltax)
        call write_f90(funit, "sys%ccben%taper", sys%ccben%taper)
        call write_f90(funit, "sys%ccben%PropElig", sys%ccben%PropElig)
        call write_f90(funit, "sys%ccben%MinAmt", sys%ccben%MinAmt)
        call write_f90(funit, "sys%ccben%CCrate", sys%ccben%CCrate)

        write(funit, *)
        write(funit, '(A)') "! uc"
        call write_f90(funit, "sys%uc%doUnivCred", sys%uc%doUnivCred)
        call write_f90(funit, "sys%uc%MainCou", sys%uc%MainCou)
        call write_f90(funit, "sys%uc%YngCou", sys%uc%YngCou)
        call write_f90(funit, "sys%uc%MainSin", sys%uc%MainSin)
        call write_f90(funit, "sys%uc%YngSin", sys%uc%YngSin)
        call write_f90(funit, "sys%uc%MinAgeMain", sys%uc%MinAgeMain)
        call write_f90(funit, "sys%uc%FirstKid", sys%uc%FirstKid)
        call write_f90(funit, "sys%uc%OtherKid", sys%uc%OtherKid)
        call write_f90(funit, "sys%uc%MaxKids", sys%uc%MaxKids)
        call write_f90(funit, "sys%uc%MaxCC1", sys%uc%MaxCC1)
        call write_f90(funit, "sys%uc%MaxCC2", sys%uc%MaxCC2)
        call write_f90(funit, "sys%uc%PropCC", sys%uc%PropCC)
        call write_f90(funit, "sys%uc%MaxAgeCC", sys%uc%MaxAgeCC)
        call write_f90(funit, "sys%uc%doRentCap", sys%uc%doRentCap)
        call write_f90(funit, "sys%uc%DisregSinNoKidsHi", sys%uc%DisregSinNoKidsHi)
        call write_f90(funit, "sys%uc%DisregSinNoKidsLo", sys%uc%DisregSinNoKidsLo)
        call write_f90(funit, "sys%uc%DisregSinKidsHi", sys%uc%DisregSinKidsHi)
        call write_f90(funit, "sys%uc%DisregSinKidsLo", sys%uc%DisregSinKidsLo)
        call write_f90(funit, "sys%uc%DisregCouNoKidsHi", sys%uc%DisregCouNoKidsHi)
        call write_f90(funit, "sys%uc%DisregCouNoKidsLo", sys%uc%DisregCouNoKidsLo)
        call write_f90(funit, "sys%uc%DisregCouKidsHi", sys%uc%DisregCouKidsHi)
        call write_f90(funit, "sys%uc%DisregCouKidsLo", sys%uc%DisregCouKidsLo)
        call write_f90(funit, "sys%uc%taper", sys%uc%taper)
        call write_f90(funit, "sys%uc%MinAmt", sys%uc%MinAmt)

        write(funit, *)
        write(funit, '(A)') "! statepen"
        call write_f90(funit, "sys%statepen%doStatePen", sys%statepen%doStatePen)
        call write_f90(funit, "sys%statepen%PenAgeMan", sys%statepen%PenAgeMan)
        call write_f90(funit, "sys%statepen%PenAgeWoman", sys%statepen%PenAgeWoman)

        write(funit, *)
        write(funit, '(A)') "! bencap"
        call write_f90(funit, "sys%bencap%doCap", sys%bencap%doCap)
        call write_f90(funit, "sys%bencap%doNI", sys%bencap%doNI)
        call write_f90(funit, "sys%bencap%doThruUC", sys%bencap%doThruUC)
        call write_f90(funit, "sys%bencap%sinNoKids", sys%bencap%sinNoKids)
        call write_f90(funit, "sys%bencap%sinKids", sys%bencap%sinKids)
        call write_f90(funit, "sys%bencap%couNoKids", sys%bencap%couNoKids)
        call write_f90(funit, "sys%bencap%couKids", sys%bencap%couKids)
        call write_f90(funit, "sys%bencap%LondonCapAmt", sys%bencap%LondonCapAmt)
        call write_f90(funit, "sys%bencap%LondonSinNoKids", sys%bencap%LondonSinNoKids)
        call write_f90(funit, "sys%bencap%LondonSinKids", sys%bencap%LondonSinKids)
        call write_f90(funit, "sys%bencap%LondonCouNoKids", sys%bencap%LondonCouNoKids)
        call write_f90(funit, "sys%bencap%LondonCouKids", sys%bencap%LondonCouKids)
        call write_f90(funit, "sys%bencap%UCEarnThr", sys%bencap%UCEarnThr)

        write(funit, *)
        write(funit, '(A)') "! extra"
        call write_f90(funit, "sys%extra%fsminappamt", sys%extra%fsminappamt)
        call write_f90(funit, "sys%extra%matgrant", sys%extra%matgrant)
        call write_f90(funit, "sys%extra%prices", sys%extra%prices)


        write(funit, *)
        write(funit, '(A)') '! .f90 FORTAX system; END-OF-FILE'
        write(funit, *)

        if (present(fname)) close(funit)

    end subroutine sys_saveF90


    ! fam_saveF90
    ! -----------------------------------------------------------------------
    ! write as a file the contents of the family type which can then be
    ! directly included in the source code of the calling program

    subroutine fam_saveF90(fam, fname)

        use fortax_util, only : fortaxError
        use, intrinsic :: iso_fortran_env

        implicit none

        type(fam_t), intent(in) :: fam
        character(len = *), intent(in), optional :: fname

        integer :: funit, ios

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing')
        else
            funit = output_unit
        end if

        write(funit, '(A)') '! .f90 FORTAX family; generated using fam_saveF90'
        write(funit, *)

        write(funit, '(A)') 'call fam_init(fam)'

        call write_f90(funit, "fam%couple", fam%couple)
        call write_f90(funit, "fam%married", fam%married)
        call write_f90(funit, "fam%ccexp", fam%ccexp)
        call write_f90(funit, "fam%maint", fam%maint)
        call write_f90(funit, "fam%nkids", fam%nkids)
        call write_f90(funit, "fam%kidage", fam%kidage, fam%nkids)
        call write_f90(funit, "fam%kidsex", fam%kidsex, fam%nkids)
        call write_f90(funit, "fam%nothads", fam%nothads)
        call write_f90(funit, "fam%tenure", fam%tenure)
        call write_f90(funit, "fam%rent", fam%rent)
        call write_f90(funit, "fam%rentcap", fam%rentcap)
        call write_f90(funit, "fam%bedrooms", fam%bedrooms)
        call write_f90(funit, "fam%region", fam%region)
        call write_f90(funit, "fam%ctband", fam%ctband)
        call write_f90(funit, "fam%banddratio", fam%banddratio)
        call write_f90(funit, "fam%intdate", fam%intdate)
        call write_f90(funit, "fam%famtype", fam%famtype)

        call write_f90(funit, "fam%ad(1)%age", fam%ad(1)%age)
        call write_f90(funit, "fam%ad(1)%selfemp", fam%ad(1)%selfemp)
        call write_f90(funit, "fam%ad(1)%hrs", fam%ad(1)%hrs)
        call write_f90(funit, "fam%ad(1)%earn", fam%ad(1)%earn)

        call write_f90(funit, "fam%ad(2)%age", fam%ad(2)%age)
        call write_f90(funit, "fam%ad(2)%selfemp", fam%ad(2)%selfemp)
        call write_f90(funit, "fam%ad(2)%hrs", fam%ad(2)%hrs)
        call write_f90(funit, "fam%ad(2)%earn", fam%ad(2)%earn)

        write(funit, *)
        write(funit, '(A)') '! .f90 FORTAX family; END-OF-FILE'
        write(funit, *)

        if (present(fname)) close(funit)

    end subroutine fam_saveF90

    subroutine write_f90integer(funit, str, val)
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: str
        integer, intent(in) :: val
        write(funit, '(A, " = ", I0)') str, val
    end subroutine write_f90integer

    subroutine write_f90integerarray(funit, str, val)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: str
        integer, intent(in) :: val(:)
        integer :: ix
        do ix = 1, size(val)
            write(funit, '(A, " = ", I0)') str // "(" // intToStr(iX) // ")", val(ix)
        end do
    end subroutine write_f90integerarray

    subroutine write_f90integerarray2(funit, str, val, nval)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: str
        integer, intent(in) :: val(:)
        integer, intent(in) :: nval
        integer :: ix
        do ix = 1, nval
            write(funit, '(A, " = ", I0)') str // "(" // intToStr(iX) // ")", val(ix)
        end do
    end subroutine write_f90integerarray2

    subroutine write_f90double(funit, str, val)
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: str
        real(dp), intent(in) :: val
        write(funit, '(A, " = ", ES24.17, "_dp")') str, val
    end subroutine write_f90double

    subroutine write_f90doublearray(funit, str, val)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: str
        real(dp), intent(in) :: val(:)
        integer :: ix
        do ix = 1, size(val)
            write(funit, '(A, " = ", ES24.17, "_dp")') str // "(" // intToStr(iX) // ")", val(ix)
        end do
    end subroutine write_f90doublearray

    subroutine write_f90doublearray2(funit, str, val, nval)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: str
        real(dp), intent(in) :: val(:)
        integer, intent(in) :: nval
        integer :: ix
        do ix = 1, nval
            write(funit, '(A, " = ", ES24.17, "_dp")') str // "(" // intToStr(iX) // ")", val(ix)
        end do
    end subroutine write_f90doublearray2

    subroutine desc_f90integer(funit, longstr, shortstr, val)
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        integer, intent(in) :: val
        if (longstr .ne. "") then
            write(funit, '(A40, 2X, I20)') longstr // ' (' // shortstr // ')', val
        else
            write(funit, '(A40, 2X, I20)') shortstr, val
        end if
    end subroutine desc_f90integer

    subroutine desc_f90integer_label(funit, longstr, shortstr, val, label)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        integer, intent(in) :: val
        character(len = len_label), intent(in) :: label
        if (longstr .ne. "") then
            write(funit, '(A40, 2X, A20)') longstr // ' (' // shortstr // ')', &
                trim(adjustl(label)) // ' (' // intToStr(val) // ')'
        else
            write(funit, '(A40, 2X, A20)') shortstr, label
        end if
    end subroutine desc_f90integer_label

    subroutine desc_f90integerarray(funit, longstr, shortstr, val)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        integer, intent(in) :: val(:)
        integer :: ix
        if (longstr .ne. "") then
            do ix = 1, size(val)
                write(funit, '(A40, 2X, I20)') longstr // ' (' // shortstr // '[' // intToStr(ix) // '])', val(ix)
            end do
        else
            do ix = 1, size(val)
                write(funit, '(A40, 2X, I20)') shortstr // '[' // intToStr(ix) // ']', val(ix)
            end do
        end if
    end subroutine desc_f90integerarray

    subroutine desc_f90integerarray_label(funit, longstr, shortstr, val, label)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        integer, intent(in) :: val(:)
        character(len = len_label), intent(in) :: label(:)
        integer :: ix
        if (longstr .ne. "") then
            do ix = 1, size(val)
                write(funit, '(A40, 2X, A20)') longstr // ' (' // shortstr // '[' // intToStr(ix) // '])', &
                      trim(adjustl(label(ix))) // ' (' // intToStr(val(ix)) // ')'
            end do
        else
            do ix = 1, size(val)
                write(funit, '(A40, 2X, A20)') shortstr // '[' // intToStr(ix) // ']', &
                      trim(adjustl(label(ix))) // ' (' // intToStr(val(ix)) // ')'
            end do
        end if
    end subroutine desc_f90integerarray_label

    subroutine desc_f90integerarray2(funit, longstr, shortstr, val, nval)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        integer, intent(in) :: val(:)
        integer, intent(in) :: nval
        integer :: ix
        if (longstr .ne. "") then
            do ix = 1, nval
                write(funit, '(A40, 2X, I20)') longstr // ' (' // shortstr // '[' // intToStr(ix) // '])', val(ix)
            end do
        else
            do ix = 1, nval
                write(funit, '(A40, 2X, I20)') shortstr // '[' // intToStr(ix) // ']', val(ix)
            end do
        end if
    end subroutine desc_f90integerarray2

    subroutine desc_f90integerarray2_label(funit, longstr, shortstr, val, nval, label)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        integer, intent(in) :: val(:)
        integer, intent(in) :: nval
        character(len = len_label), intent(in) :: label(:)
        integer :: ix
        if (longstr .ne. "") then
            do ix = 1, nval
                write(funit, '(A40, 2X, A20)') longstr // ' (' // shortstr // '[' // intToStr(ix) // '])', &
                      trim(adjustl(label(ix))) // ' (' // intToStr(val(ix)) // ')'
            end do
        else
            do ix = 1, nval
                write(funit, '(A40, 2X, A20)') shortstr // '[' // intToStr(ix) // ']', &
                      trim(adjustl(label(ix))) // ' (' // intToStr(val(ix)) // ')'
            end do
        end if
    end subroutine desc_f90integerarray2_label

    subroutine desc_f90double(funit, longstr, shortstr, val)
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        real(dp), intent(in) :: val
        if (longstr .ne. "") then
            write(funit, '(A40, 2X, F20.4)') longstr // ' (' // shortstr // ')', val
        else
            write(funit, '(A40, 2X, F20.4)') shortstr, val
        end if
    end subroutine desc_f90double

    subroutine desc_f90doublearray(funit, longstr, shortstr, val)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        real(dp), intent(in) :: val(:)
        integer :: ix
        if (longstr .ne. "") then
            do ix = 1, size(val)
                write(funit, '(A40, 2X, F20.4)') longstr // ' (' // shortstr // '[' // intToStr(ix) // '])', val(ix)
            end do
        else
            do ix = 1, size(val)
                write(funit, '(A40, 2X, F20.4)') shortstr // '[' // intToStr(ix) // ']', val(ix)
            end do
        end if
    end subroutine desc_f90doublearray

    subroutine desc_f90doublearray2(funit, longstr, shortstr, val, nval)
        use fortax_util, only : intToStr
        implicit none
        integer, intent(in) :: funit
        character(len = *), intent(in) :: longstr
        character(len = *), intent(in) :: shortstr
        real(dp), intent(in) :: val(:)
        integer, intent(in) :: nval
        integer :: ix
        if (longstr .ne. "") then
            do ix = 1, nval
                write(funit, '(A40, 2X, F20.4)') longstr // ' (' // shortstr // '[' // intToStr(ix) // '])', val(ix)
            end do
        else
            do ix = 1, nval
                write(funit, '(A40, 2X, F20.4)') shortstr // '[' // intToStr(ix) // ']', val(ix)
            end do
        end if
    end subroutine desc_f90doublearray2

    subroutine fam_refresh(fam)
        implicit none
        type(fam_t), intent(inout) :: fam

        integer :: i, j
        integer :: tmp1, tmp2

        ! if married, must be a couple

        if (fam%married == 1) fam%couple = 1

        fam%nkids = max(fam%nkids, 0)

        ! Sort kidage and kidsex arrays
        if (fam%nkids > 0) then

            ! insertion sort
            do i = 2, fam%nkids
                tmp1 = fam%kidage(i)
                tmp2 = fam%kidsex(i)
                j = i - 1
                do while (j >= 1)
                    if (fam%kidage(j) >= tmp1) exit
                    fam%kidage(j + 1) = fam%kidage(j)
                    fam%kidsex(j + 1) = fam%kidsex(j)
                    j = j - 1
                end do
                fam%kidage(j + 1) = tmp1
                fam%kidsex(j + 1) = tmp2
            end do

        end if

        ! familty type
        if (fam%couple == 0) then
            if (fam%nkids == 0) then
                fam%famtype = lab%famtype%single_nokids
            else
                fam%famtype = lab%famtype%single_kids
            end if
        else
            if (fam%nkids == 0) then
                fam%famtype = lab%famtype%couple_nokids
            else
                fam%famtype = lab%famtype%couple_kids
            end if
        end if

    end subroutine fam_refresh

end module fortax_type
