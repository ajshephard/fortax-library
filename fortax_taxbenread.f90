
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




! fortax_taxbenread
! -----------------------------------------------------------------------
! module provides read support for TAXBEN .bp3 files. TAXBEN is the
! microsimulation model maintained and developed by the IFS.

module fortax_taxbenread

    use fortax_realtype, only : dp
    private :: dp

contains

    ! readTaxbenParams
    ! -----------------------------------------------------------------------
    ! reads tax parameters from systemfile into a sys_t derived type.

    subroutine readTaxbenParams(sys,systemFile,prices,sysFix,sysDate,sysname,sysdesc)

        use xml_data_xmltaxben_t, only : read_xml_file_xmltaxben_t, object, namedFields_t, field_t
        use fortax_util,          only : getunit, strToDouble, strToInt, strToLogical, lower, fortaxError, fortaxWarn
        use fortax_type,          only : sys_t, sys_init

        implicit none

        type(sys_t),       intent(out) :: sys
        character(len=*),  intent(in)  :: systemFile
        integer, optional, intent(in)  :: prices
        logical, optional, intent(in)  :: sysFix !default = .true.
        integer, optional, intent(in)  :: sysDate
        character(len=*), optional, intent(in)  :: sysname
        character(len=*), optional, intent(in)  :: sysdesc
        type(namedfields_t), pointer   :: cat
        type(field_t),       pointer   :: cat2
        integer                        :: i, j, nField, nnamedFields
        logical                        :: isFile

        inquire(file=systemFile, exist=isFile)

        if (.not. isFile) then
            call fortaxError('system file does not exist ('//trim(adjustl(systemFile))//')')
        end if

        call read_xml_file_xmltaxben_t(systemFile)
        call sys_init(sys)

        nnamedFields = size(object%namedFields)
        sys%sysname = object%name

        if (size(object%field)==3) then
            sys%sysdesc = object%field(3)%value
        end if

        do i = 1, nnamedFields
            cat => Object%namedFields(i)
            select case(cat%baseName)

                !Income tax (personal allowance)
                case('PrmIncTax')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('SA')
                                sys%incTax%pa = strToDouble(cat2%value)
                            case('DoSATaper')
                                sys%incTax%doPATaper = strToLogical(cat2%value)
                            case('SATaperRate')
                                sys%incTax%paTaperRate = strToDouble(cat2%value)
                            case('SATaperThreshold')
                                sys%incTax%paTaperThresh = strToDouble(cat2%value)
                            case('MMA')
                                sys%incTax%mma = strToDouble(cat2%value)
                            case('CTC')
                                sys%incTax%ctc = strToDouble(cat2%value)
                            case('CTCYoungKidSupplement')
                                sys%incTax%ctcYng = strToDouble(cat2%value)
                            case('Class4Rebate')
                                sys%incTax%c4Rebate = strToDouble(cat2%value)
                            case('NumRateBands')
                                sys%incTax%numBands = strToInt(cat2%value)
                        end select
                    end do


                !Income tax (MMA rate)
                case('PrmIncTax.Options')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('MCARestrictedTo')
                                sys%incTax%mmaRate = strToDouble(cat2%value)
                            case('DoTransferablePersonalAllowance')
                                sys%incTax%doTPA = strToLogical(cat2%value)
                            case('MaxTrans')
                                sys%incTax%maxTPA = strToDouble(cat2%value)
                        end select
                    end do


                !Income tax (bands)
                case('PrmIncTax.Bands')

                    if (sys%incTax%numBands>0) then
                        allocate(sys%incTax%bands(sys%incTax%numBands))
                        sys%incTax%bands = 0.0_dp
                    else
                        call fortaxError('sys%incTax%numBands not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
!                        sys%incTax%bands(j) = strToDouble(cat2%value)
                        sys%incTax%bands(strToInt(cat2%name)) = strToDouble(cat2%value)
                    end do


                !Income tax (rates)
                case('PrmIncTax.Rates')

                    if (sys%incTax%numBands>0) then
                        allocate(sys%incTax%rates(sys%incTax%numBands))
                        sys%incTax%rates = 0.0_dp
                    else
                        call fortaxError('sys%incTax%numBands not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
!                        sys%incTax%rates(j) = strToDouble(cat2%value)
                        sys%incTax%rates(strToInt(cat2%name)) = strToDouble(cat2%value)
                    end do


                !NI
                case('PrmNI')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('NumNIRateBands')
                                sys%natIns%numRates = strToInt(cat2%value)
                            case('NumberofClass4Bands')
                                sys%natIns%c4nRates = strToInt(cat2%value)
                            case('Class1Ceiling')
                                sys%natIns%Ceiling = strToDouble(cat2%value)
                            case('Class2Rate')
                                sys%natIns%c2Rate = strToDouble(cat2%value)
                            case('Class2Floor')
                                sys%natIns%c2Floor = strToDouble(cat2%value)
                        end select
                    end do


                !NI (bands)
                case('PrmNI.Ceilings')

                    if (sys%natIns%numRates>0) then
                        allocate(sys%natIns%bands(sys%natIns%numRates))
                        sys%natIns%bands = 0.0_dp
                    else
                        call fortaxError('sys%natIns%numRates not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
!                        sys%natIns%bands(j) = strToDouble(cat2%value)
                        sys%natIns%bands(strToInt(cat2%name)) = strToDouble(cat2%value)
                    end do


                case('PrmNI.Class4Bands')

                    if (sys%natIns%c4nRates>0) then
                        allocate(sys%natIns%c4Bands(sys%natIns%c4nRates))
                        sys%natIns%c4Bands = 0.0_dp
                    else
                        call fortaxError('sys%natIns%c4nRates not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        sys%natIns%c4Bands(strToInt(cat2%name)) = strToDouble(cat2%value)
                    end do


                !NI (rates)
                case('PrmNI.InRates')

                    if (sys%natIns%numRates>0) then
                        !JS can't work out why "+1" in next line
!                        allocate(sys%natIns%rates(sys%natIns%numRates+1))
                        allocate(sys%natIns%rates(sys%natIns%numRates))
                        sys%natIns%rates = 0.0_dp
                    else
                        call fortaxError('sys%natIns%numRates not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        sys%natIns%rates(strToInt(cat2%name)) = strToDouble(cat2%value)
                    end do

                case('PrmNI.FeeRates')

                    if (sys%natIns%numRates<=0) then
                        call fortaxError('sys%natIns%numRates not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        sys%natIns%rates(strToInt(cat2%name)) = strToDouble(cat2%value)
                    end do


                case('PrmNI.Class4Rates')

                    if (sys%natIns%c4nRates>0) then
                        allocate(sys%natIns%c4Rates(sys%natIns%c4nRates))
                        sys%natIns%c4Rates = 0.0_dp
                    else
                        call fortaxError('sys%natIns%c4nRates not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        sys%natIns%c4Rates(strToInt(cat2%name)) = strToDouble(cat2%value)
                    end do


                !Child benefit
                case('PrmCBen')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('CB1')
                                sys%chBen%basic = strToDouble(cat2%value)
                                if (cat2%value .ne. '0') sys%chBen%doChBen = .true.
                            case('CB2')
                                sys%chBen%kid1xtr = strToDouble(cat2%value)
                            case('OPB')
                                sys%chBen%opf = strToDouble(cat2%value)
                            case('WithdrawCB')
                                sys%chBen%doTaper = StrToLogical(cat2%value)
                            case('CBTaperStart')
                                sys%chBen%taperStart = strToDouble(cat2%value)
                            case('CBTaperEnd')
                                ! This assumes that sys%chBen%taperStart has already been read in
                                sys%chBen%taperRate = 100.0_dp / (52.0_dp * (strToDouble(cat2%value) - sys%chBen%taperStart))
                        end select
                    end do


                !Sure Start maternity grant
                case('PrmFowler.SSMG')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('SSMatGrant')
                                sys%chBen%matGrantVal = strToDouble(cat2%value)
                            case('OnlyFirstKid')
                                sys%chBen%MatGrantOnlyFirstKid = StrToLogical(cat2%value)
                        end select
                    end do


                !FC/WFTC
                case('PrmFowler.FC')

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('MinHours')
                                sys%fc%hours1 = strToDouble(cat2%value)
                            case('FTHours')
                                sys%fc%hours2 = strToDouble(cat2%value)
                            case('NumAgeRanges')
                                sys%fc%numAgeRng = strToInt(cat2%value)
                            case('AdultCredit')
                                sys%fc%adult = strToDouble(cat2%value)
                            case('FTPremium')
                                sys%fc%ftPrem = strToDouble(cat2%value)
                            case('Threshold')
                                sys%fc%thres = strToDouble(cat2%value)
                            case('Taper')
                                sys%fc%taper = strToDouble(cat2%value)
                            case('MainDis')
                                sys%fc%maintDisreg = strToDouble(cat2%value)
                        end select
                    end do


                !FC/WFTC (Age limits)
                case('PrmFowler.FC.AgeLim')

                    if (sys%fc%numAgeRng>0) then
                        allocate(sys%fc%kidagel(sys%fc%numAgeRng))
                        allocate(sys%fc%kidageu(sys%fc%numAgeRng))
                        sys%fc%kidAgeL = 0
                        sys%fc%kidAgeU = 0
                    else
                        call fortaxError('sys%fc%numAgeRng not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
!                        sys%fc%kidage(j,2) = strToInt(cat2%value)
                        sys%fc%kidageu(strToInt(cat2%name)) = strToInt(cat2%value)
                    end do


                !FC/WFTC (Child credit)
                case('PrmFowler.FC.ChildCredit')

                    if (sys%fc%numAgeRng>0) then
                        allocate(sys%fc%kidCred(sys%fc%numAgeRng))
                        sys%fc%kidCred = 0.0_dp
                    else
                        call fortaxError('sys%fc%numAgeRng not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
!                        sys%fc%kidCred(j) = strToDouble(cat2%value)
                        sys%fc%kidCred(strToInt(cat2%name)) = strToDouble(cat2%value)
                    end do


                !FC childcare disregard
                case('PrmFowler.FC.CCDisregardInfo')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('MaxAge')
                                sys%fc%maxAgeCC = strToInt(cat2%value)
                        end select
                    end do

                case('PrmFowler.FC.CCDisregardInfo.Values')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('1')
                                sys%fc%maxCC1 = strToDouble(cat2%value)
                            case('2')
                                sys%fc%maxCC2 = strToDouble(cat2%value)
                        end select
                    end do


                !WFTC childcare element
                case('PrmFowler.FC.CCCreditInfo')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('MaxAge')
                                sys%fc%WFTCMaxAgeCC = strToInt(cat2%value)
                            case('ReceiptProp')
                                sys%fc%WFTCPropCC = strToDouble(cat2%value)
                        end select
                    end do

                case('PrmFowler.FC.CCCreditInfo.Values')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('1')
                                sys%fc%WFTCMaxCC1 = strToDouble(cat2%value)
                            case('2')
                                sys%fc%WFTCMaxCC2 = strToDouble(cat2%value)
                        end select
                    end do

                !New tax cred
                case('PrmFowler.NewTaxCreds')

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('Enabled')
                                if (cat2%value=='0') then
                                    sys%fc%doFamCred = .true.
                                else
                                    sys%ntc%doNewTaxCred = .true.
                                end if
                        end select
                    end do

                !CTC and CTC/WTC taper
                case('PrmFowler.NewTaxCreds.PrmChildTaxCred')

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('FamilyPremium')
                                sys%ctc%fam = strToDouble(cat2%value)
                            case('YoungKidPremium')
                                sys%ctc%baby = strToDouble(cat2%value)
                            case('Threshold')
                                sys%ntc%thr1Lo = strToDouble(cat2%value)
                            case('ChildTaxCredOnlyThreshold')
                                sys%ntc%thr1Hi = strToDouble(cat2%value)
                            case('HighIncomeThreshold')
                                sys%ntc%thr2 = strToDouble(cat2%value)
                            case('Taper')
                                sys%ntc%taper1 = strToDouble(cat2%value)
                            case('HighIncomeTaper')
                                sys%ntc%taper2 = strToDouble(cat2%value)
                            case('TaperElementsTogether')
                                sys%ntc%taperCTCInOneGo = strToLogical(cat2%value)
                        end select
                    end do

                case('PrmFowler.NewTaxCreds.PrmChildTaxCred.ChildCredit')

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('1')
                                sys%ctc%kid = strToDouble(cat2%value)
                        end select
                    end do


                !WTC
                case('PrmFowler.NewTaxCreds.PrmWorkingTaxCred')

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('AdultCreditNoKidsSingle')
                                sys%wtc%basic = strToDouble(cat2%value)
                            case('AdultCreditNoKidsCouple')
                                sys%wtc%couLP = strToDouble(cat2%value)
                            case('FTPremiumNoKids')
                                sys%wtc%FT = strToDouble(cat2%value)
                            case('FTHours')
                                sys%wtc%FTHrs = strToDouble(cat2%value)
                            case('MinHoursNoKids')
                                sys%wtc%minHrsNoKids = strToDouble(cat2%value)
                            case('MinHoursLP')
                                sys%wtc%MinHrsKids = strToDouble(cat2%value)
                            case('MinHoursCouKids')
                                sys%wtc%MinHrsCouKids = strToDouble(cat2%value)
                            case('MinimumAgeNoKids')
                                sys%wtc%minAgeNoKids = strToInt(cat2%value)
                            case('MinimumAgeKids')
                                sys%wtc%minAgeKids = strToInt(cat2%value)
                            case('FTPremiumHBDisregardForAll')
                                sys%wtc%newDisregCon = StrToLogical(cat2%value)
                            case('FTPremiumHBdisregardValue')
                                sys%wtc%newDisreg = strToDouble(cat2%value)

                        end select
                    end do


                !Childcare element of WTC
                case('PrmFowler.NewTaxCreds.PrmChildTaxCred.CCCreditInfo.Values')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('1')
                                sys%wtc%maxCC1 = strToDouble(cat2%value)
                            case('2')
                                sys%wtc%maxCC2 = strToDouble(cat2%value)
                        end select
                    end do

                case('PrmFowler.NewTaxCreds.PrmChildTaxCred.CCCreditInfo')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('MaxAge')
                                sys%wtc%maxAgeCC = strToInt(cat2%value)
                            case('ReceiptProp')
                                sys%wtc%propCC = strToDouble(cat2%value)
                        end select
                    end do


                !Income Support
                case('PrmFowler.ISys')

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('AgeYoungBenChange')
                                sys%incSup%MinAgeMainSin = strToInt(cat2%value)
                            case('CouOver18')
                                sys%incSup%mainCou = strToDouble(cat2%value)
                                if (cat2%value .ne. '0') sys%incsup%doIncSup = .true.
                            case('CouUnder18')
                                sys%incSup%yngCou = strToDouble(cat2%value)
                            case('OPFOver18')
                                sys%incSup%mainLP = strToDouble(cat2%value)
                            case('OPFUnder18')
                                sys%incSup%yngLP = strToDouble(cat2%value)
                            case('SinOld')
                                sys%incSup%mainSin = strToDouble(cat2%value)
                            case('SinYoung')
                                sys%incSup%yngSin = strToDouble(cat2%value)
                            case('ValFreeSchoolMeals')
                                sys%incSup%valFSM = strToDouble(cat2%value)
                            case('UniversalFSMAge')
                                sys%incSup%MaxAgeUniversalFSM = strToInt(cat2%value)
                            case('Disregard1')
!                                sys%incSup%Disreg1 = strToDouble(cat2%value)
                                sys%incSup%disregLP = strToDouble(cat2%value)
                            case('Disregard2')
!                                sys%incSup%Disreg2 = strToDouble(cat2%value)
                                sys%incSup%disregSin = strToDouble(cat2%value)
                                sys%incSup%disregCou = sys%incSup%DisregSin*2.0_dp
                            case('NumAgeRanges')
                                sys%incSup%numAgeRng = strToInt(cat2%value)
                            case('MainDis')
                                sys%incSup%maintDisreg = strToDouble(cat2%value)
                        end select
                    end do


                !Income Support: premiums
                case('PrmFowler.ISys.Prem')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('Fam')
                                sys%incSup%premFam = strToDouble(cat2%value)
                            case('OPF')
                                sys%incSup%premLP = strToDouble(cat2%value)
                        end select
                    end do


                !Income Support: age limits
                case('PrmFowler.ISys.AgeLim')

                    if (sys%incSup%numAgeRng>0) then
                        allocate(sys%incSup%ageRngL(sys%incSup%numAgeRng))
                        allocate(sys%incSup%ageRngU(sys%incSup%numAgeRng))
                        sys%incSup%ageRngL = 0
                        sys%incSup%ageRngU = 0
                    else
                        call fortaxError('sys%incSup%numAgeRng not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
!                        sys%incSup%AgeRng(j,2) = strToInt(cat2%value)
                        sys%incSup%ageRngU(strToInt(cat2%name)) = strToInt(cat2%value)
                    end do


                !Income Support: child additions
                case('PrmFowler.ISys.ChildAdd')

                    if (sys%incSup%numAgeRng>0) then
                        allocate(sys%incSup%addKid(sys%incSup%numAgeRng))
                        sys%incSup%addKid = 0.0_dp
                    else
                        call fortaxError('sys%incSup%numAgeRng not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
!                        sys%incSup%AddKid(j) = strToDouble(cat2%value)
                        sys%incSup%addKid(strToInt(cat2%name)) = strToDouble(cat2%value)
                    end do


                !Council Tax
                case('PrmLocTax')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('BandDRate')
                                sys%ctax%bandD = strToDouble(cat2%value)
                            case('SinglePersonDiscount')
                                sys%ctax%sinDis = strToDouble(cat2%value)
                            case('BandA')
                                sys%ctax%ratioA = strToDouble(cat2%value)
                            case('BandB')
                                sys%ctax%ratioB = strToDouble(cat2%value)
                            case('BandC')
                                sys%ctax%ratioC = strToDouble(cat2%value)
                            case('BandE')
                                sys%ctax%ratioE = strToDouble(cat2%value)
                            case('BandF')
                                sys%ctax%ratioF = strToDouble(cat2%value)
                            case('BandG')
                                sys%ctax%ratioG = strToDouble(cat2%value)
                            case('BandH')
                                sys%ctax%ratioH = strToDouble(cat2%value)
                        end select
                    end do


                !HB, CCB and CTB: allowances and disregards; benefit cap values
                case('PrmFowler.Reb')

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('AgeYoungBenChange')
                                sys%rebateSys%minAgeMainSin = strToInt(cat2%value)
                            case('CouOver18')
                                sys%rebateSys%mainCou = strToDouble(cat2%value)
                            case('CouUnder18')
                                sys%rebateSys%yngCou = strToDouble(cat2%value)
                            case('OPFOver18')
                                sys%rebateSys%mainLP = strToDouble(cat2%value)
                            case('OPFUnder18')
                                sys%rebateSys%yngLP = strToDouble(cat2%value)
                            case('SinOld')
                                sys%rebateSys%mainSin = strToDouble(cat2%value)
                            case('SinYoung')
                                sys%rebateSys%yngSin = strToDouble(cat2%value)
                            case('Disregard2')
                                sys%rebateSys%disregSin = strToDouble(cat2%value)
                                sys%rebateSys%disregCou = sys%rebateSys%DisregSin*2.0_dp
                            case('Disregard3')
                                sys%rebateSys%disregLP = strToDouble(cat2%value)
                            case('MainDis')
                                sys%rebateSys%maintDisreg = strToDouble(cat2%value)
                            case('ChildcareDisUnearnedInc')
                                sys%rebateSys%CredInDisregCC = StrToLogical(cat2%value)
                            case('NumAgeRanges')
                                sys%rebateSys%numAgeRng = strToInt(cat2%value)
                            case('RestrictCTB')
                                sys%rebateSys%restrict = StrToLogical(cat2%value)
                            case('DoBenefitCap')
                                sys%bencap%doCap = StrToLogical(cat2%value)
                            case('DoBenefitCapinNI')
                                sys%bencap%doNI = StrToLogical(cat2%value)
                            case('BenefitCapSinglenoKids')
                                sys%bencap%sinNoKids = strToDouble(cat2%value)
                            case('BenefitCapLoneParent')
                                sys%bencap%sinKids = strToDouble(cat2%value)
                            case('BenefitCapCoupleNoKids')
                                sys%bencap%couNoKids = strToDouble(cat2%value)
                            case('BenefitCapCoupleKids')
                                sys%bencap%couKids = strToDouble(cat2%value)
                        end select
                    end do


                !HB, CCB and CTB: premiums
                case('PrmFowler.Reb.Prem')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('Fam')
                                sys%rebateSys%premFam = strToDouble(cat2%value)
                            case('OPF')
                                sys%rebateSys%premLP = strToDouble(cat2%value)
                        end select
                    end do


                !HB, CCB and CTB: whether child benefit counts as income in the taper calculation
                  ! Note: the ChildBenefit component in the PrmFowler.Reb.OthInc structure only exists when child benefit counts as
                  ! income so this code relies on sys%rebateSys%ChbenIsIncome being initialised to .false. by sys_init
                case('PrmFowler.Reb.OthInc')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('ChildBenefit')
                                sys%rebateSys%ChbenIsIncome = merge(.true., .false., (strToInt(cat2%value)==1))
                        end select
                    end do


                !Housing Benefit: taper
                case('PrmFowler.Reb.OneRebate[Reb_Rent]')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('Taper')
                                sys%hben%taper = strToDouble(cat2%value)
                                if (cat2%value .ne. '0') sys%hben%doHBen = .true.
                            case('MinPayment')
                                sys%hben%minAmt = strToDouble(cat2%value)
                        end select
                    end do


                !Council Tax Benefit: taper
                case('PrmFowler.Reb.OneRebate[Reb_CouncilTax]')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('Taper')
                                sys%ctaxben%taper = strToDouble(cat2%value)
                        end select
                    end do


                !Community Charge Benefit: taper
                case('PrmFowler.Reb.OneRebate[Reb_Poll_Tax]')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('Taper')
                                sys%ccBen%taper = strToDouble(cat2%value)
                            case('MinPayment')
                                sys%ccBen%minAmt = strToDouble(cat2%value)
                            case('PropElig')
                                sys%ccBen%propElig = strToDouble(cat2%value)
                        end select
                    end do


                !Community Charge: rate
                case('PrmLocTax.RegionalPollTaxRate')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('Northern') !same for all regions
                                sys%ccBen%CCrate = strToDouble(cat2%value)
                        end select
                    end do

                !here we inspect the file to see what local tax system we have
                !extend this for when we are dealing with Northern Ireland rates
                case('PrmLocTax.RegionalLocalTaxSys')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('Northern') !same for all regions
                                select case(cat2%value)
                                    case('CouncilTax')
                                        sys%ctax%doCouncilTax = .true.
                                        sys%ctaxben%doCouncilTaxBen = .true.
                                    case('PollTax')
                                        sys%ccBen%doPollTax = .true.
                                    case default
                                        call fortaxError('unrecognised local tax system')
                                end select
                        end select
                    end do

                !HB, CCB and CTB: age limits
                case('PrmFowler.Reb.AgeLim')

                    if (sys%rebateSys%numAgeRng>0) then
                        allocate(sys%rebateSys%ageRngL(sys%rebateSys%numAgeRng))
                        allocate(sys%rebateSys%ageRngU(sys%rebateSys%numAgeRng))
                        sys%rebateSys%ageRngL = 0
                        sys%rebateSys%ageRngU = 0
                    else
                        call fortaxError('sys%rebateSys%numAgeRng not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
!                        sys%rebateSys%AgeRng(j,2) = strToInt(cat2%value)
                        sys%rebateSys%ageRngU(strToInt(cat2%name)) = strToInt(cat2%value)
                    end do


                !HB, CCB and CTB: child addition
                case('PrmFowler.Reb.ChildAdd')

                    if (sys%rebateSys%numAgeRng>0) then
                        allocate(sys%rebateSys%AddKid(sys%rebateSys%numAgeRng))
                        sys%rebateSys%addKid = 0
                    else
                        call fortaxError('sys%rebateSys%numAgeRng not set')
                    end if

                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
!                        sys%rebateSys%AddKid(j) = strToDouble(cat2%value)
                        sys%rebateSys%addKid(strToInt(cat2%name)) = strToDouble(cat2%value)
                    end do


                !Childcare disregard for HB/CTB
                case('PrmFowler.Reb.ChildcareDis')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('MaxAge')
                                sys%rebateSys%maxAgeCC = strToInt(cat2%value)
                        end select
                    end do

                case('PrmFowler.Reb.ChildcareDis.Values')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('1')
                                sys%rebateSys%maxCC1 = strToDouble(cat2%value)
                            case('2')
                                sys%rebateSys%maxCC2 = strToDouble(cat2%value)
                        end select
                    end do


                ! State pension (at the moment, it's just the state pension age)
                case('PrmNIBen')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                            case('Abolished')
                                sys%statepen%doStatePen = strToLogical(cat2%value)
                            case('RePenAgeMan')
                                sys%statepen%penAgeMan = strToDouble(cat2%value)
                            case('RetPenAgeWoman')
                                sys%statepen%penAgeWoman = strToDouble(cat2%value)
                        end select
                    end do


                !Not used
                case('Template')
                    nfield = size(cat%field)
                    do j = 1, nfield
                        cat2 => cat%field(j)
                        select case(cat2%name)
                        end select
                    end do

            end select
        end do

        if (sys%rebateSys%numAgeRng>0) then
            do i = 2, sys%rebateSys%numAgeRng
                sys%rebateSys%ageRngL(i) = sys%rebateSys%ageRngU(i-1)+1
            end do
        end if

        if (sys%incSup%numAgeRng>0) then
            do i = 2, sys%incSup%numAgeRng
                sys%incSup%ageRngL(i) = sys%incSup%ageRngU(i-1)+1
            end do
        end if

        if (sys%fc%numAgeRng>0) then
            do i = 2, sys%fc%numAgeRng
                sys%fc%kidAgeL(i) = sys%fc%kidAgeU(i-1)+1
            end do
        end if

        sys%natIns%bands(sys%natIns%numRates) = sys%natIns%Ceiling

!                sys%incTax%ctctaper = 1.0_dp/15.0_dp
!                sys%incSup%hours    = sys%fc%hours1
!                sys%ntc%MinAmt = 0.5_dp

        !free up memory
        do i = 1, nnamedFields
            if (associated(object%namedFields(i)%field)) then
                deallocate(object%namedFields(i)%field)
            end if
        end do

        if (associated(object%namedFields)) then
            deallocate(object%namedFields)
        end if

        !default is to call taxbensysfix
        if (present(sysFix)) then
            if (sysFix) then
                if (present(sysDate)) then
                    call taxbenSysFix(sys,sysDate)
                else
                    call taxbenSysFix(sys)
                end if
            end if
        else
            if (present(sysdate)) then
                call taxbenSysFix(sys,sysDate)
            else
                call taxbenSysFix(sys)
            end if
        end if

        if (present(sysname)) then
            sys%sysname = sysname
        end if

        if (present(sysdesc)) then
            sys%sysdesc = sysdesc
        end if

        if (present(prices)) sys%extra%prices = prices

    end subroutine readTaxbenParams


    ! batchConvertTaxben
    ! -----------------------------------------------------------------------
    ! this finds all taxben system files in path fpathin and saves them as
    ! fortax system files in fpathout. it attempts to interpret date based
    ! on the file name. this uses system calls to get the folder listing
    ! and at the moment only will not work on windows systems

    subroutine batchConvertTaxben(fpathin,fpathout,fname)
        use fortax_util
        use fortax_type
        use fortax_write
        implicit none
        character(len=*), intent(in)           :: fpathin
        character(len=*), intent(in)           :: fpathout
        character(len=*), intent(in), optional :: fname

        type(sys_t) :: sys

        integer :: tmpunit, funit, istat, pos, ifail
        character(len=256) :: tmpname
        character(len=256) :: fileline
        character(len=256) :: yr
        integer :: season, yrlen, yrint, yyyy, yyyymmdd
        logical :: err

        ! first, use system call to get contents of directory
        if ( present(fname) ) then
#           ifdef _WIN32
                call system( 'dir "'//trim(fpathin)//'\*.bp3" /b > "'//fname//'"' )
#           endif
#           ifdef __linux
                call system('find "'//trim(fpathin)//'" -name "*.bp3" -printf "%f\n" > "'//fname//'"' )
#           endif
            call getUnit(funit)
            open( funit, file=fname, action='read', status='old', iostat=istat )
        else
            call getUnit(funit)
            open( unit=funit, status='scratch' )
            inquire(unit=funit,name=tmpname)
            close(funit)
#           ifdef _WIN32
                call system( 'dir "'//trim(fpathin)//'\*.bp3" /b > "'//tmpname//'"' )
#           endif
#           ifdef __linux
                call system('find "'//trim(fpathin)//'" -name "*.bp3" -printf "%f\n" > '//tmpname)
#           endif
            open(unit=funit,file=tmpname,action='read',status='old',iostat=istat)
        end if

        err = .false.

        ! now read file created and convert
        do

            read(funit,*,iostat=istat) fileline

            if (istat==-1) then !eof
                exit
            else if (istat>0) then !error
                err = .true.
                write(*,*)  "error reading file list"
                exit
            else
                ! find position of file extension in original name
                ! this will always be present given how we constructed
                ! the file
                pos = index( fileline, '.bp3', back=.true. )

                ! we understand april and autumn (not case sensitive)
                if ( lower(fileline(1:5))=='april' ) then
                    season = 1
                else if ( lower(fileline(1:6))=='autumn' ) then
                    season = 2
                else
                    season = 0
                end if

                if (season>0) then
                    ! extract the character string for year
                    if (season == 1) then
                        yr = fileline(6:pos-1)
                        yrlen = len_trim(yr)
                    else if (season == 2) then
                        yr = fileline(7:pos-1)
                        yrlen = len_trim(yr)
                    end if

                    if ( (yrlen==2) .and. yr(1:1)=='0' ) then
                        yr = '1'//trim(yr)
                    end if

                    call strToIntCheck(yr,yrint,ifail)

                    if (ifail==0) then
                        if (yrint>0) then
                            if (yrint<50) then
                                yyyy = 2000 + yrint
                            else if (yrint>=50 .and. yrint<1000) then
                                yyyy = 1900 + yrint
                            else
                                yyyy = yrint
                            end if
                            yyyymmdd = yyyy*10000 + merge(400,1200,season==1) +1
                        else
                            yyyymmdd = -1
                        end if
                    else
                        yyyymmdd = -1
                    end if

                else

                    yyyymmdd = -1

                end if

                if (yyyymmdd>0) then
                    call readTaxbenParams( sys, trim(fpathin)//'/'//trim(fileline), prices=yyyymmdd, sysDate=yyyymmdd )
                    write(*,*) trim(fileline), yyyymmdd
                else
                    call readTaxbenParams( sys, trim(fpathin)//'/'//trim(fileline) )
                    write(*,*) trim(fileline), -1
                end if

                fileline(pos:pos+3) = '.xml'
                call fortaxWrite( sys, trim(fpathout)//'/'//trim(fileline) )

            end if

        end do

        close(funit)

        if (err) then
            stop
        end if

    end subroutine batchConvertTaxben


    ! taxbenSysFix
    ! -----------------------------------------------------------------------
    ! this fix routine is used when reading taxben system files because
    ! some of the necessary parameters are not included in the system file

    subroutine taxbenSysFix(sys,sysDate)

        use fortax_type, only : sys_t, sysHuge
!        use params,      only : tol

        implicit none

        type(sys_t), intent(inout)    :: sys
        integer, intent(in), optional :: sysDate
        real(dp),    parameter     :: tol = 0.0_dp

        ! impose consistent large value for unbounded values
        call taxbenLargeValue(sys)

        !50p rules for FC/WFTC and NTC's, and different rules under tax credits
        if (sys%fc%doFamCred) then
            sys%fc%minAmt = 0.50_dp
!             if (sys%fc%MaxCC1 > tol) then
            if (sys%fc%maintDisreg < sysHuge) then
              sys%rebateSys%rulesUnderFC   = .true.
              sys%rebateSys%rulesUnderWFTC = .false.
            else
              sys%rebateSys%rulesUnderFC   = .false.
              sys%rebateSys%rulesUnderWFTC = .true.
            end if
            sys%rebateSys%rulesUnderNTC = .false.
            sys%incSup%incChBen         = .true.
        end if

        if (sys%ntc%donewtaxcred) then
            sys%ntc%MinAmt = 0.5_dp
            sys%rebateSys%rulesUnderFC   = .false.
            sys%rebateSys%rulesUnderWFTC = .false.
            sys%rebateSys%rulesUnderNTC  = .true.
            sys%incSup%incChBen          = .false.
        end if

        !taper rate for CHILDREN's tax credit
        if (sys%incTax%ctc>tol) sys%incTax%ctcTaper = 1.0_dp/15.0_dp

        !hours for IS
        sys%incSup%hours = sys%fc%hours1

        ! Minimum age for entitlement to the IS main allowance rate for all but childless singles
        sys%incSup%MinAgeMain = 18

        if (present(sysDate)) then

            ! IS/IB-JSA: is disregard shared for couples?
            ! I think that the date from which the disregard was pooled across couples is 7th Oct 1996
            if (sysdate < 19961007) then
                sys%incSup%disregShared = .false.
            else
                sys%incSup%disregShared = .true.
            end if

            ! From Jul 00, childcare disregard set against WFTC/WTC/CTC as well as earnings (TAXBEN says Apr 00, so this is a
            ! correction)
            if ((sysdate >= 20000401) .and. (sysdate < 20000703)) then
                sys%rebateSys%credInDisregCC = .false.
            end if


        else
            ! Note: if taxbensysfix is not called, then ...%DisregShared = .true.
            sys%incSup%disregShared = .true.
        end if

        ! Minimum age for FSM
        sys%incSup%MinAgeFSM = 5

        ! Set child benefit taper to be recorded as a reduction in child benefit (rather than an increase in income tax)
        if (sys%chBen%doTaper) then
          sys%chBen%taperIsIncTax = .false.
        end if

        ! Minimum age for entitlement to the HB/CTB main allowance rate for all but childless singles
        sys%rebatesys%MinAgeMain = 18

        ! Reduce maximum entitlement for council tax benefit (April 2013)
        if (present(sysDate)) then
            if (sysdate >= 20130401) then
                sys%ctaxben%doEntitlementCut = .true.
                sys%ctaxben%entitlementShare = 0.896_dp
            else
                sys%ctaxben%doEntitlementCut = .false.
            end if
        else
            sys%ctaxben%doEntitlementCut = .false.
        end if


        ! Impose benefit cap through UC if UC is to be calculated
        if (sys%uc%doUnivCred == .true.) then
            sys%bencap%doThruUC = .true.
        else
            sys%bencap%doThruUC = .false.
        end if

    end subroutine taxbenSysFix


    ! taxbenLargeValue
    ! -----------------------------------------------------------------------
    ! this imposes a consistent large value the unbounded values. it is just
    ! applied to components of sys manually.

    subroutine taxbenLargeValue(sys)
        use fortax_type, only : sys_t, sysHuge
        implicit none
        type(sys_t), intent(inout) :: sys
        integer :: i

        if (sys%natins%ceiling>9999998.0_dp) then
            sys%natins%ceiling = sysHuge
        end if

        do i = 1, sys%natins%numrates
            if ( sys%natins%bands(i)>9999998.0_dp ) then
                sys%natins%bands(i) = sysHuge
            end if
        end do

        if ( sys%fc%maintDisreg>99998.0_dp ) then
            sys%fc%maintDisreg = sysHuge
        end if

        if ( sys%incsup%maintDisreg>99998.0_dp ) then
            sys%incsup%maintDisreg = sysHuge
        end if

        if ( sys%rebatesys%maintDisreg>99998.0_dp ) then
            sys%rebatesys%maintDisreg = sysHuge
        end if

        if ( sys%bencap%sinNoKids>99998.0_dp ) then
            sys%bencap%sinNoKids = sysHuge
        end if

        if ( sys%bencap%sinKids>99998.0_dp ) then
            sys%bencap%sinKids = sysHuge
        end if

        if ( sys%bencap%couNoKids>99998.0_dp ) then
            sys%bencap%couNoKids = sysHuge
        end if

        if ( sys%bencap%couKids>99998.0_dp ) then
            sys%bencap%couKids = sysHuge
        end if

    end subroutine taxbenLargeValue

end module fortax_taxbenread
