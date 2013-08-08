
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




! fortax_calc
! -----------------------------------------------------------------------
! module provides the main calculation routines (given fam and sys it
! returns net), AS/JS

! macro definitions for application specific optimizations (define
! these at compile time)

#ifndef _famcouple_
#define _famcouple_ fam%couple
#endif

#ifndef _fammarried_
#define _fammarried_ fam%married
#endif

#ifndef _famkids_
#define _famkids_ fam%nkids > 0
#endif

module fortax_calc

    use fortax_realtype, only : dp

    private

    real(dp), parameter :: tol = 0.0_dp

    public :: calcNetInc

contains

    ! ----------------------INCOME TAX----------------------
    
    ! tearn       - Calculates taxable earnings of family
    ! inctax      - Calculates income tax of individual
    ! taxafterctc - Adjusts income tax of family for children’s tax credit
    ! taxaftermca - Adjusts income tax of family for MCA

    ! ----------------------INCOME TAX----------------------


    ! tearn
    ! -----------------------------------------------------------------------
    ! Taxable earnings (calculated for TU). Depends on couple, married, 
    ! nkids, and earn

    !DEC$ ATTRIBUTES FORCEINLINE :: tearn
    pure subroutine tearn(sys,fam,net)

        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        
        real(dp)                   :: earningsOverThresh
        real(dp)                   :: persAllow
        
        !integer                    :: pe !pe = primary earner

        !Personal allowance

        ! Calculate personal allowance for adult 1 (tapering it away from high income individuals from April 2010 onwards)
        if (sys%inctax%doPATaper) then

            ! Calculate earnings over threshold
            earningsOverThresh = max(fam%ad(1)%earn - sys%inctax%paTaperThresh, 0.0_dp)
            if (earningsOverThresh > tol) then
                ! Taper personal allowance away
                persAllow =  max(sys%inctax%pa - earningsOverThresh * sys%inctax%paTaperRate, 0.0_dp)
                ! Round up to nearest pound (rounding done on annual basis)
                persAllow = real(ceiling(persAllow*52.0_dp), dp) / 52.0_dp
            else
                persAllow = sys%inctax%pa
            end if

        else
            persAllow = sys%inctax%pa
        
        end if

        ! Calculate taxable income
        net%ad(1)%taxable = max(fam%ad(1)%earn-persAllow, 0.0_dp)

        
        ! Calculate personal allowance for adult 2 if present (tapering it away from high income individuals from April 2010 onwards)
        if (_famcouple_) then

            if (sys%inctax%doPATaper) then

                ! Calculate earnings over threshold
                earningsOverThresh = max(fam%ad(2)%earn - sys%inctax%paTaperThresh, 0.0_dp)
                if (earningsOverThresh > tol) then
                    ! Taper personal allowance away
                    persAllow =  max(sys%inctax%pa - earningsOverThresh * sys%inctax%paTaperRate, 0.0_dp)
                    ! Round up to nearest pound (rounding done on annual basis)
                    persAllow = real(ceiling(persAllow*52.0_dp), dp) / 52.0_dp
                else
                    persAllow = sys%inctax%pa
                end if

            else
                persAllow = sys%inctax%pa
            
            end if

            ! Calculate taxable income
            net%ad(2)%taxable = max(fam%ad(2)%earn-persAllow, 0.0_dp)


        ! If no partner
        else
            net%ad(2)%taxable = 0.0_dp
        end if


        ! Rebate for Class 4 NI contributions (1985/86-1995/96)
        if (sys%inctax%c4rebate > tol) then
            if ((net%ad(1)%natinsc4) > tol) net%ad(1)%taxable &
                & = max(net%ad(1)%taxable - sys%inctax%c4rebate*net%ad(1)%natinsc4, 0.0_dp)
            if ((_famcouple_) .and. (net%ad(2)%natinsc4 > tol)) net%ad(2)%taxable = &
                & max(net%ad(2)%taxable - sys%inctax%c4rebate*net%ad(2)%natinsc4, 0.0_dp)
        end if

        !MCA/APA pre-Apr 94 (when it worked like an allowance)
        if ((sys%inctax%mma > tol) .and. (sys%inctax%mmarate <= tol)) then

            !Lone parents
            if ((.not. _famcouple_) .and. (_famkids_)) then
                net%ad(1)%taxable = max(net%ad(1)%taxable-sys%inctax%mma, 0.0_dp)
            
            !Couples (married or with kids)
            else if ((_famcouple_) .and. ((_fammarried_) .or. (_famkids_))) then
                !Identify primary earner
                if (fam%ad(1)%earn >= fam%ad(2)%earn) then
                    !Subtract MCA/APA
                    net%ad(1)%taxable = net%ad(1)%taxable - sys%inctax%mma
                    if (net%ad(1)%taxable < 0.0_dp) then
                        net%ad(2)%taxable = max(net%ad(2)%taxable+net%ad(1)%taxable, 0.0_dp)
                        net%ad(1)%taxable = 0.0_dp
                    end if
                else
                    !Subtract MCA/APA
                    net%ad(2)%taxable = net%ad(2)%taxable - sys%inctax%mma
                    if (net%ad(2)%taxable < 0.0_dp) then
                        net%ad(1)%taxable = max(net%ad(1)%taxable+net%ad(2)%taxable, 0.0_dp)
                        net%ad(2)%taxable = 0.0_dp
                    end if
                end if
            end if

        end if

    end subroutine tearn


    ! inctax
    ! -----------------------------------------------------------------------
    ! Income tax (calculated for individual). Depends on tearn

    !DEC$ ATTRIBUTES FORCEINLINE :: inctax
    pure subroutine inctax(sys,net,i)

        use fortax_type,   only : sys_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(net_t), intent(inout) :: net
        integer,     intent(in)    :: i
        !real(dp),       intent(in) :: tearn
        
        integer                    :: j

        net%ad(i)%inctax = 0.0_dp
        
        if (net%ad(i)%taxable > tol .and. sys%inctax%numbands>0) then
        
            !1st band
            net%ad(i)%inctax = net%ad(i)%inctax + min(net%ad(i)%taxable,sys%inctax%bands(1))*sys%inctax%rates(1)

            !2nd to penultimate bands
            if (sys%inctax%numbands > 2) then
                do j = 2, sys%inctax%numbands-1
                    net%ad(i)%inctax = net%ad(i)%inctax + max(min(net%ad(i)%taxable-sys%inctax%bands(j-1),sys%inctax%bands(j) &
                        & -sys%inctax%bands(j-1)),0.0_dp)*sys%inctax%rates(j)
                end do
            end if

            !Last band
            if (sys%inctax%numbands > 1) net%ad(i)%inctax = net%ad(i)%inctax &
                & + max(net%ad(i)%taxable-sys%inctax%bands(sys%inctax%numbands-1), &
                & 0.0_dp)*sys%inctax%rates(sys%inctax%numbands)

        end if

    end subroutine inctax


    ! taxafterctc
    ! -----------------------------------------------------------------------
    ! Children's tax credit (calculated for TU). depends on couple, nkids, 
    ! yngkid, earn, tearn, and tax
    
    !DEC$ ATTRIBUTES FORCEINLINE :: taxafterctc
    pure subroutine taxafterctc(sys,fam,net)

        use fortax_type,   only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net

        integer                    :: pe
        real(dp)                   :: ctc
        
        if ((sys%inctax%ctc > tol) .and. (_famkids_)) then
        
            if (fam%yngkid < 16) then

                !Maximum ctc
                ctc = sys%inctax%ctc
                if (fam%yngkid == 0) ctc = ctc + sys%inctax%ctcyng

                !Primary earner
                if (.not. _famcouple_) then
                    pe = 1
                else
                    if (fam%ad(1)%earn >= fam%ad(2)%earn) then
                        pe = 1
                    else
                        pe = 2
                    end if
                end if

                if (pe==1) then
                    !Taper away from higher-rate taxpayers (never tapered away from secondary earner)
                    if (net%ad(1)%taxable > sys%inctax%bands(sys%inctax%numbands-1)) then
                        ctc = max(ctc - sys%inctax%ctctaper*(net%ad(1)%taxable-sys%inctax%bands(sys%inctax%numbands-1)),0.0_dp)
                    end if
                                    
                    !Now reduce tax due
                    net%ad(1)%inctax = net%ad(1)%inctax - ctc
                    if (net%ad(1)%inctax < 0.0_dp) then
                        if (_famcouple_) net%ad(2)%inctax = max(net%ad(2)%inctax+net%ad(1)%inctax, 0.0_dp)
                        net%ad(1)%inctax = 0.0_dp
                    end if
                else
                    !Taper away from higher-rate taxpayers (never tapered away from secondary earner)
                    if (net%ad(2)%taxable > sys%inctax%bands(sys%inctax%numbands-1)) then
                        ctc = max(ctc - sys%inctax%ctctaper*(net%ad(2)%taxable-sys%inctax%bands(sys%inctax%numbands-1)),0.0_dp)
                    end if
                                    
                    !Now reduce tax due
                    net%ad(2)%inctax = net%ad(2)%inctax - ctc
                    if (net%ad(2)%inctax < 0.0_dp) then
                        if (_famcouple_) net%ad(1)%inctax = max(net%ad(1)%inctax+net%ad(2)%inctax, 0.0_dp)
                        net%ad(2)%inctax = 0.0_dp
                    end if
                end if

            end if

        end if

    end subroutine taxafterctc


    ! taxaftermca
    ! -----------------------------------------------------------------------
    ! Post Apr-94 MCA/APA (calculated for TU). Depends on couple, married,
    ! nkids, earn, and net

    !DEC$ ATTRIBUTES FORCEINLINE :: taxaftermca
    pure subroutine taxaftermca(sys,fam,net)

        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        
        !integer                    :: pe

        !MCA/APA post-Apr 94 (when it reduced tax due)
        if ((sys%inctax%mma > tol) .and. (sys%inctax%mmarate > tol)) then

            !Lone parents
            if ((.not. _famcouple_) .and. (_famkids_)) then
                net%ad(1)%inctax = max(net%ad(1)%inctax-sys%inctax%mma*sys%inctax%mmarate, 0.0_dp)
            
            !Couples
            else if ((_famcouple_) .and. ((_fammarried_) .or. (_famkids_))) then

                !Identify primary (pre-tax) earner
                if (fam%ad(1)%earn >= fam%ad(2)%earn) then
                    !Subtract MCA/APA
                    net%ad(1)%inctax = net%ad(1)%inctax - sys%inctax%mma*sys%inctax%mmarate
                    if ((net%ad(1)%inctax) < 0.0_dp) then
                        net%ad(2)%inctax = max(net%ad(2)%inctax+net%ad(1)%inctax, 0.0_dp)
                        net%ad(1)%inctax = 0.0_dp
                    end if
                else
                    !Subtract MCA/APA
                    net%ad(2)%inctax = net%ad(2)%inctax - sys%inctax%mma*sys%inctax%mmarate
                    if ((net%ad(2)%inctax) < 0.0_dp) then
                        net%ad(1)%inctax = max(net%ad(1)%inctax+net%ad(2)%inctax, 0.0_dp)
                        net%ad(2)%inctax = 0.0_dp
                    end if
                end if

            end if

        end if
    
    end subroutine taxaftermca


    ! ----------------------NATIONAL INSURANCE----------------------

    ! NatIns - Calculates National Insurance of individual

    ! ----------------------NATIONAL INSURANCE----------------------


    ! NatIns
    ! -----------------------------------------------------------------------
    ! National Insurance. Contracted in employees only. Note that
    ! sys%natins%rates(1) is the "entry fee" (cliff edge) if earnings exceed 
    ! sys%natins%bands(1). Depends on earn,selfemp
    
    !DEC$ ATTRIBUTES FORCEINLINE :: NatIns
    pure subroutine NatIns(sys,fam,net,i)
        
        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        integer,     intent(in)    :: i

        integer                    :: j

        ! Employed
        if ((.not. fam%ad(i)%selfemp) .and. sys%natins%numrates>0) then
        
            ! Class 1 contributions
            if (fam%ad(i)%earn >= sys%natins%bands(1) - tol) then
                net%ad(i)%natinsc1 = sys%natins%bands(1)*sys%natins%rates(1)
                if (sys%natins%numrates > 1) then
                    do j = 2, sys%natins%numrates
                        net%ad(i)%natinsc1 = net%ad(i)%natinsc1 + max(min(fam%ad(i)%earn-sys%natins%bands(j-1),sys%natins%bands(j) &
                            & - sys%natins%bands(j-1)),0.0_dp)*sys%natins%rates(j)
                    end do
                end if
            else
                net%ad(i)%natinsc1 = 0.0_dp
            end if
            
            ! Class 2 and 4 contributions
            net%ad(i)%natinsc2 = 0.0_dp
            net%ad(i)%natinsc4 = 0.0_dp

        ! Self-employed
        else if (fam%ad(i)%selfemp .and. sys%natins%c4nrates>0) then

            ! Class 1 contributions
            net%ad(i)%natinsc1 = 0.0_dp

            !Class 2 contributions
            if (fam%ad(i)%earn >= sys%natins%c2floor - tol) then
                net%ad(i)%natinsc2 = sys%natins%c2rate
            else
                net%ad(i)%natinsc2 = 0.0_dp
            end if
            
            ! Class 4 contributions
            net%ad(i)%natinsc4 = 0.0_dp
            net%ad(i)%natinsc4 = net%ad(i)%natinsc4 + min(max(fam%ad(i)%earn,0.0_dp), sys%natins%c4bands(1))*sys%natins%c4rates(1) 
            if (sys%natins%c4nrates > 2) then
                do j = 2, sys%natins%c4nrates-1
                    net%ad(i)%natinsc4 = net%ad(i)%natinsc4 + max(min(fam%ad(i)%earn-sys%natins%c4bands(j-1),sys%natins%c4bands(j) &
                        & - sys%natins%c4bands(j-1)),0.0_dp)*sys%natins%c4rates(j)
                end do
            end if
            if (sys%natins%c4nrates > 1) net%ad(i)%natinsc4 = net%ad(i)%natinsc4 &
                & + max(fam%ad(i)%earn-sys%natins%c4bands(sys%natins%c4nrates-1),0.0_dp) &
                & *sys%natins%c4rates(sys%natins%c4nrates)

        else

            net%ad(i)%natinsc1 = 0.0_dp
            net%ad(i)%natinsc2 = 0.0_dp
            net%ad(i)%natinsc4 = 0.0_dp

        end if

        !total national insurance
        net%ad(i)%natins = net%ad(i)%natinsc1 + net%ad(i)%natinsc2 + net%ad(i)%natinsc4

    end subroutine NatIns


    ! ----------------------INCOME SUPPORT----------------------

    ! IncSup   - Calculates income support of family
    ! ISAppAmt - Calculates IS applicable amount (for IncSup calculation)
    ! ISDisreg - Calculates IS disregard (for IncSup calculation)

    ! ----------------------INCOME SUPPORT----------------------



    ! IncSup
    ! -----------------------------------------------------------------------
    ! Income support. Age range: works for 16-59; note additional conditions 
    ! for those 16-17 up to 1996/97. note that netearn, maint and othinc must 
    ! be in weekly terms. depends on couple, adage, hrs, nkids, kidage,
    ! netearn, maint, othinc

    !DEC$ ATTRIBUTES FORCEINLINE :: IncSup    
    pure subroutine IncSup(sys,fam,net)
        
        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        
        real(dp)                   :: disreg, appamt, othinc, MatGrInIS
        integer                    :: i
    
        integer                    :: maxage
        real(dp)                   :: maxhrs, MaintDisreg

        if (_famcouple_) then
            maxage = max(fam%ad(1)%age,fam%ad(2)%age)
            maxhrs = max(fam%ad(1)%hrs,fam%ad(2)%hrs)
        else
            maxage = fam%ad(1)%age
            maxhrs = fam%ad(1)%hrs
        end if

        !From 1997, partner can work up to 24 hrs, but code below requires them to work less than 16
        if ((((maxage) >= 18) .or. (_famkids_)) .and. (maxhrs < sys%incsup%hours-tol)) then

            appamt = ISAppAmt(sys,fam)
            disreg = ISDisreg(sys,fam)

            if (sys%incsup%incchben) then
                othinc = net%tu%chben + net%tu%fc + net%tu%wtc
            else 
                othinc = net%tu%fc + net%tu%wtc
            end if
!             if (sys%incsup%incchben) then
!                 othinc = tax%chben + tax%fc
!             else if (sys%ntc%donewtaxcred) then
!                 othinc = tax%fc
!             else
!                 othinc = 0.0_dp
!             end if

            if (sys%extra%fsminappamt) then
                !add fsm to applicable amount
                do i = 1, fam%nkids
                    if (fam%kidage(i)>4) appamt = appamt + sys%incsup%ValFSM
                end do
            end if
            
            if (sys%extra%matgrant .and. fam%yngkid==0) then
                !add maternity grant to applicable amount
                !call MatGrant(sys,fam,net,.true.)
                !MatGrInIS = net%matgrant
                MatGrInIS = 0.0_dp
                do i=1,fam%nkids
                    if (fam%kidage(i) == 0) MatGrInIS = MatGrInIS + (sys%chben%MatGrantVal/52.0_dp)
                end do                
                appamt = appamt + MatGrInIS
            end if

             if (_famkids_) then
                 MaintDisreg = sys%incsup%MaintDisreg
             else
                 MaintDisreg = 0.0_dp
             end if

!            net%tu%incsup = max(appamt - max((net%tu%posttaxearn-disreg),0.0_dp) &
!                & - max((fam%maint-MaintDisreg),0.0_dp) - othinc, 0.0_dp)
        
            if ((.not. _famcouple_) .or. (sys%incsup%disregShared)) then
                net%tu%incsup = max(appamt - max((net%tu%posttaxearn-disreg),0.0_dp) &
                    & - max((fam%maint-MaintDisreg),0.0_dp) - othinc, 0.0_dp)
            else
                net%tu%incsup = max(appamt - max(net%ad(1)%posttaxearn - 0.5_dp*disreg,0.0_dp) &
                    & - max(net%ad(2)%posttaxearn - 0.5_dp*disreg,0.0_dp) &
                    & - max(fam%maint-MaintDisreg,0.0_dp) - othinc, 0.0_dp)
            end if

            !re-assign income to correct categories, AS
            if (sys%extra%matgrant .and. fam%yngkid==0) then
                net%tu%matgrant = min(net%tu%incsup,MatGrInIS)
                net%tu%incsup   = max(0.0_dp,net%tu%incsup - MatGrInIS)
            end if

        else
        
            net%tu%incsup = 0.0_dp
            !if (sys%extra%matgrant) net%tu%matgrant = 0.0_dp

        end if

    end subroutine IncSup


    ! ISAppAmt
    ! -----------------------------------------------------------------------
    ! IS/IB-JSA: applicable amount ("needs") - identical to first part of 
    ! HB/CTB needs (except for parameters). Doesn't do housing costs, should 
    ! it? depends on couple, adage, nkids, kidage

    !DEC$ ATTRIBUTES FORCEINLINE :: ISAppAmt    
    real(dp) pure function ISAppAmt(sys,fam)

        use fortax_type, only : sys_t, fam_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam

        integer                    :: i, j

        !Note: LP premium abolished Apr 98 except for existing claimants. 
        !TAXBEN parameter is zero for later years, so we effectively ignore 
        !the "existing claimants" condition
        !Note: Family allowance and child additions transferred to CTC from Apr 03. 
        !TAXBEN parameters are zero for later years, so no need to worry
        !In both cases, could simplify by adding year conditions

        !Allowances and family/LP premiums
        if (.not. _famcouple_) then
        
            if (_famkids_) then
                !Lone parent
                if (fam%ad(1)%age < 18) then
                    ISAppAmt = sys%incsup%YngLP + sys%incsup%PremFam + sys%incsup%PremLP
                else
                    ISAppAmt = sys%incsup%MainLP + sys%incsup%PremFam + sys%incsup%PremLP
                end if
            else
                !Single childless
                if (fam%ad(1)%age < 25) then
                    ISAppAmt = sys%incsup%YngSin
                else
                    ISAppAmt = sys%incsup%MainSin
                end if
            end if

        else
            !Couples (ignore cases: one over 18, one under 18)
            if ((fam%ad(1)%age < 18) .and. (fam%ad(2)%age < 18)) then
                ISAppAmt = sys%incsup%YngCou
            else
                ISAppAmt = sys%incsup%MainCou
            end if
            
            if (_famkids_) ISAppAmt = ISAppAmt + sys%incsup%PremFam
            
        end if

        !Child additions (this could be more efficient if we required that kids age structure is sorted)
        if (_famkids_) then
            do i = 1, fam%nkids
                do j = 1, sys%incsup%NumAgeRng
                    if ((fam%kidage(i) >= sys%incsup%AgeRngl(j)) .and. (fam%kidage(i) <= sys%incsup%AgeRngu(j))) then
                        ISAppAmt = ISAppAmt + sys%incsup%AddKid(j)
                        exit
                    end if
                end do
            end do
        end if

    end function ISAppAmt


    ! ISDisreg
    ! -----------------------------------------------------------------------
    ! IS/IB-JSA: earnings disregard. Depends on couple and nkids
    
    !DEC$ ATTRIBUTES FORCEINLINE :: ISDisreg
    real(dp) pure function ISDisreg(sys,fam)
    
        use fortax_type, only : sys_t, fam_t
        
        implicit none
        
        type(sys_t), intent(in) :: sys
        type(fam_t), intent(in) :: fam

        if (.not. _famcouple_) then
            if (_famkids_) then
                ISDisreg = sys%incsup%DisregLP
            else
                ISDisreg = sys%incsup%DisregSin
            end if
        else
            ISDisreg = sys%incsup%DisregCou
        end if

    end function ISDisreg


    ! ----------------------REBATE SYSTEM----------------------

    ! ctax         - Calculates council tax of family
    ! polltax      - Calculates community charge of family
    ! prelimcalc   - Preliminary calculations for HBen, CTBen and CCBen
    ! HBen         - Calculates housing benefit for family
    ! HBFull       - Works out whether family is entitled to full HB
    ! ctaxBen      - Calculates council tax benefit for family
    ! polltaxBen   - Calculates community charge benefit for family
    ! HBAppAmt     - Calculates applicable amount for HB/CTB/CCB (called by prelimcalc)
    ! StdDisreg    - Calculates standard earnings disregard of family for HB/CTB/CCB (called by prelimcalc)
    ! FTDisreg     - Calculates disregard for workers for HB/CTB/CCB (called by prelimcalc)
    ! ChCareDisreg - Calculates childcare disregard for HB/CTB/CCB (called by prelimcalc)
    ! MaintDisreg  - Calculates maintenance disregard for HB/CTB/CCB (called by prelimcalc)
    ! RebateDisreg - Calculates total disregard for HB/CTB/CCB (called by prelimcalc)

    ! ----------------------REBATE SYSTEM----------------------


    ! ctax
    ! -----------------------------------------------------------------------
    ! Simplified council tax liability (e.g. doesn't deal with accomodation 
    ! type, students). Depends on couple, adage, nothads, band

    !DEC$ ATTRIBUTES FORCEINLINE :: ctax
    pure subroutine ctax(sys,fam,net)

        use fortax_type, only : sys_t, fam_t, net_t, &
            ctax_banda, ctax_bandb, ctax_bandc, ctax_bandd, &
            ctax_bande, ctax_bandf, ctax_bandg, ctax_bandh
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        
        real(dp)                   :: ratio, fracbu
        integer                    :: nliabbu, nliabhh

        if (sys%ctax%bandD > tol) then
        
            ! Work out number of liable adults in TU
            nliabbu = 0
        
            if (fam%ad(1)%age >= 18) nliabbu = 1
        
            if (_famcouple_) then
                if (fam%ad(2)%age >= 18) nliabbu = nliabbu + 1
            end if

            if (nliabbu > 0) then

                !Work out number of liable adults in hh (assume all other adults are 18+)
                nliabhh = nliabbu + fam%nothads
                fracbu = real(nliabbu,dp)/real(nliabhh,dp)
                !Calculate CT                    
                select case (fam%ctband)
                    case (ctax_banda)
                        ratio = sys%ctax%RatioA
                    case (ctax_bandb)
                        ratio = sys%ctax%RatioB
                    case (ctax_bandc)
                        ratio = sys%ctax%RatioC
                    case (ctax_bandd)
                        ratio = 1.0_dp
                    case (ctax_bande)
                        ratio = sys%ctax%RatioE
                    case (ctax_bandf)
                        ratio = sys%ctax%RatioF
                    case (ctax_bandg)
                        ratio = sys%ctax%RatioG
                    case (ctax_bandh)
                        ratio = sys%ctax%RatioH
                end select

                !we scale council
                if (nliabhh == 1) then
                    net%tu%ctax = sys%ctax%bandD*(1.0_dp-sys%ctax%SinDis)*fam%banddratio*ratio
                else
                    net%tu%ctax = sys%ctax%bandD*fam%banddratio*ratio*fracbu
                end if

            else
            
                net%tu%ctax = 0.0_dp

            end if

        else

            net%tu%ctax = 0.0_dp

        end if

    end subroutine ctax


    ! polltax
    ! -----------------------------------------------------------------------
    ! Simplified calculation of Community Charge liability. Depends on 
    ! couple, adage

    !DEC$ ATTRIBUTES FORCEINLINE :: polltax
    pure subroutine polltax(sys,fam,net)

        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net

        integer                    :: nliable

        if (sys%ccben%CCrate > tol) then

            ! Work out how many liable adults
            nliable = 0

            if (fam%ad(1)%age >= 18) nliable = 1

            if (_famcouple_) then
                if (fam%ad(2)%age >= 18) nliable = nliable + 1
            end if

            !CC liability
            net%tu%polltax = sys%ccben%CCrate*real(nliable,dp)

        else

            net%tu%polltax = 0.0_dp

        end if

    end subroutine polltax


    ! prelimcalc
    ! -----------------------------------------------------------------------
    ! Preliminary calculations HBen, CTBen and CCBen. Depends on couple, 
    ! adage, hrs, nkids, kidage, ccexp, incsup

    !DEC$ ATTRIBUTES FORCEINLINE :: prelimcalc
    pure subroutine prelimcalc(sys,fam,net,disregRebate)
    
        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        real(dp),    intent(out)   :: disregRebate
  
        real(dp) :: appamt, disregStd, disregFT, disregCC, disregMnt
        
        if (net%tu%incsup <= tol) then !JS: I changed ">" to "<" (I think this is what it should be)
            appamt       = HBAppAmt(sys,fam)
            disregStd    = StdDisreg(sys,fam)
            disregFT     = FTDisreg(sys,fam,net)
            disregCC     = ChCareDisreg(sys,fam,net)
            disregMnt    = MaintDisreg(sys,fam)
            disregRebate = RebateDisreg(sys,fam,net,appamt,disregStd,disregFT,disregCC,disregMnt)
        else
            disregRebate = 0.0_dp
        end if
        
    end subroutine prelimcalc
    
    ! disregRebate
    ! -----------------------------------------------------------------------
    ! Disregard used in calculation of Hben, ctaxBen, pollTaxBen. Depends on
    ! disregStd, disregFT, disregCC, disregMnt, appamt, posttaxearn, fc, wtc,
    ! ctc, maint, chben
    
    !DEC$ ATTRIBUTES FORCEINLINE :: RebateDisreg
    pure function RebateDisreg(sys,fam,net,appamt,disregStd,disregFT,disregCC,disregMnt)

        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none
        
        type(sys_t), intent(in) :: sys
        type(fam_t), intent(in) :: fam
        type(net_t), intent(in) :: net
        real(dp),    intent(in) :: appamt, disregStd, disregFT, disregCC, disregMnt
        
        real(dp) :: RebateDisreg

        real(dp) :: disregCC1, disregCC2, disregCC3
        
        !old calculation 
!        if (sys%rebatesys%CredInDisregCC) then        
!            ! From Oct 99, WFTC/WTC/CTC could be set against disregCC
!            RebateDisreg = max(max(max(max(net%tu%posttaxearn-disregStd,0.0_dp) &
!                & + net%tu%fc+net%tu%wtc-disregFT, 0.0_dp) + net%tu%ctc-disregCC, 0.0_dp) &
!                & + max(fam%maint-disregMnt, 0.0_dp) + net%tu%chben - appamt, 0.0_dp)
!        else
!            RebateDisreg = max(max(max(net%tu%posttaxearn-disregStd-disregCC,0.0_dp) &
!                & + net%tu%fc+net%tu%wtc-disregFT, 0.0_dp) &
!                & + max(fam%maint-disregMnt, 0.0_dp) + net%tu%chben+net%tu%ctc-appamt, 0.0_dp)           
!        end if    
        

        !corrected version
        if (sys%rebatesys%CredInDisregCC) then
            disregCC1 = 0.0_dp
            disregCC2 = net%tu%ctc-disregCC
            disregCC3 = 0.0_dp
        else
            disregCC1 = disregCC
            disregCC2 = 0.0_dp
            disregCC3 = net%tu%ctc
        end if  

        if (sys%rebatesys%rulesUnderWFTC .or. sys%rebatesys%rulesUnderNTC) then
            RebateDisreg = max(max(max(max(net%tu%posttaxearn-disregStd-disregCC1,0.0_dp) &
                & + net%tu%fc+net%tu%wtc-disregFT, 0.0_dp) + disregCC2, 0.0_dp) &
                & + max(fam%maint-disregMnt, 0.0_dp) + net%tu%chben + disregCC3 - appamt, 0.0_dp) 
        elseif (sys%rebatesys%rulesUnderFC) then
            RebateDisreg = max(max(max(max(net%tu%posttaxearn-disregStd-disregCC1,0.0_dp) &
                & + max(net%tu%fc+net%tu%wtc-disregFT,0.0_dp), 0.0_dp) + disregCC2, 0.0_dp) &
                & + max(fam%maint-disregMnt, 0.0_dp) + net%tu%chben + disregCC3 - appamt, 0.0_dp) 
        else
            RebateDisreg = 0.0_dp
        end if
                                        
    end function RebateDisreg
    
    ! HBen
    ! -----------------------------------------------------------------------
    ! Housing benefit. Depends on incsup, rent, disregRebate

    !DEC$ ATTRIBUTES FORCEINLINE :: HBen    
    pure subroutine HBen(sys,fam,net,disregRebate)

        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        real(dp),    intent(in)    :: disregRebate

        real(dp)                   :: eligrent
                
        if (fam%rent > 0.0_dp) then
        
            ! Rent cap (only implemented for PRIVATE renters (i.e. fam%tenure == 5))
            eligrent = fam%rent
            if ((sys%rebatesys%docap) .and. (fam%tenure == 5)) eligrent = min(fam%rent,fam%rentcap)

            !Passport to full entitlement if on IS or income-based JSA
            if (net%tu%incsup > tol) then

                net%tu%hben = eligrent
                !Zero if award < 50p
                if (net%tu%hben < sys%hben%MinAmt) net%tu%hben = 0.0_dp

            else
                    
                !HB taper
                net%tu%hben = max(eligrent - disregRebate*sys%hben%taper,0.0_dp)

                !Zero if award < 50p
                if (net%tu%hben < sys%hben%MinAmt) net%tu%hben = 0.0_dp

            end if

        else
        
            net%tu%hben = 0.0_dp
        
        end if

    end subroutine HBen
    

    ! HBFull
    ! -----------------------------------------------------------------------
    ! Tells you whether TU is entitled to full HB (i.e. whether any of the HB
    ! award has been tapered away). Depends on incsup, disregRebate

    !DEC$ ATTRIBUTES FORCEINLINE :: HBFull
    logical pure function HBFull(sys,fam,net,disregRebate)

        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in) :: sys
        type(fam_t), intent(in) :: fam
        type(net_t), intent(in) :: net
        real(dp),    intent(in) :: disregRebate
        
        if (net%tu%incsup > tol) then
            HBFull = .true.
        else

            if (disregRebate > tol) then
                HBFull = .false.
            else
                HBFull = .true.
            end if

        end if

    end function HBFull

    
    ! ctaxBen
    ! -----------------------------------------------------------------------
    ! Council Tax Benefit taper (what about NI?). Depends on couple, adage, 
    ! posttaxfamearn, fc, maint, othinc, incsup, ctax, band, appamt,
    ! disregStd, disregFT, disregCC, disregMnt

    !DEC$ ATTRIBUTES FORCEINLINE :: ctaxBen
    pure subroutine ctaxBen(sys,fam,net,disregRebate)

        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        real(dp),    intent(in)    :: disregRebate
        
        real(dp)                   :: maxctb
        integer                    :: maxage

        ! No CTB for under-18s
        if (_famcouple_) then
            maxage = max(fam%ad(1)%age,fam%ad(2)%age)
        else
            maxage = fam%ad(1)%age
        end if

        !if ((maxval(fam%age(1:_famcouple_+1)) < 18) .or. (net%tu%loctax < tol)) then
        if ((maxage < 18) .or. (net%tu%ctax <= tol)) then

            net%tu%ctaxben = 0.0_dp

        else
        
            !Cap CTB at band E from 1998 to 2003
            if ((fam%ctband > 5) .and. sys%rebatesys%Restrict) then
                select case (fam%ctband)
                    case (6)
                        maxctb = net%tu%ctax*sys%ctax%RatioE/sys%ctax%RatioF
                    case (7)
                        maxctb = net%tu%ctax*sys%ctax%RatioE/sys%ctax%RatioG
                    case (8)
                        maxctb = net%tu%ctax*sys%ctax%RatioE/sys%ctax%RatioH
                end select
            else
                maxctb = net%tu%ctax
            end if

            if (net%tu%incsup > tol) then
                !Passport to full entitlement if on IS or income-based JSA
                net%tu%ctaxben = maxctb
            else
                !CTB taper (no minimum award)
                ! From Oct 99, WFTC/WTC/CTC could be set against disregCC
                net%tu%ctaxben = max(maxctb - disregRebate*sys%ctaxben%taper,0.0_dp)

            end if

        end if

    end subroutine ctaxBen

    ! 
    ! pollTaxBen
    ! -----------------------------------------------------------------------
    ! Community Charge Benefit taper. Depends on couple, adage, netearn, fc,
    ! maint, othinc, incsup, cc, disregRebate
    
    !DEC$ ATTRIBUTES FORCEINLINE :: pollTaxBen
    pure subroutine pollTaxBen(sys,fam,net,disregRebate)

        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        real(dp),    intent(in)    :: disregRebate
        
        real(dp)                   :: eligcc
        integer                    :: maxage

        if (_famcouple_) then
            maxage = max(fam%ad(1)%age,fam%ad(2)%age)
        else
            maxage = fam%ad(1)%age
        end if

        ! No CCB for under-18s
        if (maxage < 18) then
            net%tu%polltaxben = 0.0_dp            
        else        
            !only 80% of CC eligible for CCB
            eligcc = net%tu%polltax*sys%ccben%PropElig

            if (net%tu%incsup > tol) then
                !Passport to full entitlement if on IS (income-based JSA didn't exist when CCB was around)
                net%tu%polltaxben = eligcc
                
            else             
                !CCB taper (I think disregFT and disregCC ares irrelevant here - they are only > 0 from 1995)
!                net%tu%polltaxben = max(eligcc - max(max(max(net%tu%posttaxearn-disreg1,0.0_dp)+net%tu%fc+net%tu%wtc-disreg2, 0.0_dp) &
!                    & + max(fam%maint-disreg3, 0.0_dp) + net%tu%chben+net%tu%ctc-appamt, 0.0_dp)*sys%ccben%taper,0.0_dp)

                net%tu%polltaxben = max(eligcc - disregRebate*sys%ccben%taper,0.0_dp)

                !Zero if award < 50p
                if (net%tu%polltaxben < sys%ccben%MinAmt) net%tu%polltaxben = 0.0_dp

            end if

        end if

    end subroutine polltaxBen

    
    ! HBAppAmt
    ! -----------------------------------------------------------------------
    ! HB/CCB/CTB: applicable amount ("needs"). Note: Premium for family with 
    ! child under 1 currently implemented as child addition - so error if 
    ! multiple kids under 1. Depends on couple, adage, nkids, kidage

    !DEC$ ATTRIBUTES FORCEINLINE :: HBAppAmt
    real(dp) pure function HBAppAmt(sys,fam)

        use fortax_type, only : sys_t, fam_t
        
        implicit none

        type(sys_t), intent(in) :: sys
        type(fam_t), intent(in) :: fam

        integer                 :: i, j

        !Allowances and family/LP premiums
        if (.not. _famcouple_) then
            if (_famkids_) then
                !Lone parent
                !sys%rebatesys%PremLP abolished Apr 98, but don't need to condition on year here because parameter should be zero for later years
                if (fam%ad(1)%age < 18) then
                    HBAppAmt = sys%rebatesys%YngLP + sys%rebatesys%PremFam + sys%rebatesys%PremLP
                else
                    HBAppAmt = sys%rebatesys%MainLP + sys%rebatesys%PremFam + sys%rebatesys%PremLP
                end if
            else
                !Single childless
                if (fam%ad(1)%age < 25) then
                    HBAppAmt = sys%rebatesys%YngSin
                else
                    HBAppAmt = sys%rebatesys%MainSin
                end if
            end if
        else
            !Couples
            if ((fam%ad(1)%age < 18) .and. (fam%ad(2)%age < 18)) then
                HBAppAmt = sys%rebatesys%YngCou
            else
                HBAppAmt = sys%rebatesys%MainCou
            end if

            if (_famkids_) HBAppAmt = HBAppAmt + sys%rebatesys%PremFam

        end if

        !Child additions
        if (_famkids_) then

            do i = 1, fam%nkids
                do j = 1, sys%rebatesys%NumAgeRng
                    if ((fam%kidage(i) >= sys%rebatesys%AgeRngl(j)) .and. (fam%kidage(i) <= sys%rebatesys%AgeRngu(j))) then
                        HBAppAmt = HBAppAmt + sys%rebatesys%AddKid(j)
                        exit
                    end if
                end do
            end do

        end if

    end function HBAppAmt

    
    ! StdDisreg
    ! -----------------------------------------------------------------------
    ! HB/CCB/CTB: standard earnings disregard. Depends on couple, nkids

    !DEC$ ATTRIBUTES FORCEINLINE :: StdDisreg    
    real(dp) pure function StdDisreg(sys,fam)
    
        use fortax_type, only : sys_t, fam_t
        
        implicit none

        type(sys_t), intent(in) :: sys
        type(fam_t), intent(in) :: fam

        !Main disregard
        if (.not. _famcouple_) then
            if (_famkids_) then
                StdDisreg = sys%rebatesys%DisregLP
            else
                StdDisreg = sys%rebatesys%DisregSin
            end if
        else
            StdDisreg = sys%rebatesys%DisregCou
        end if

    end function StdDisreg

    
    ! FTDisreg
    ! -----------------------------------------------------------------------
    ! HB/CCB/CTB: disregard for workers (originally for those getting FT 
    ! premium with FC/WFTC/WTC). Depends on couple, adage, hrs, nkids

    !DEC$ ATTRIBUTES FORCEINLINE :: FTDisreg    
    real(dp) pure function FTDisreg(sys,fam,net)
 
        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in) :: sys
        type(fam_t), intent(in) :: fam
        type(net_t), intent(in) :: net
        
        !I have changed this so it condtions on dofamcred (and others) so it does
        !not give the FT disregard under incomplete takeup, AS 16/06/10
        
        !Additional disregard for workers (up to 2003: those eligible for FC/WFTC/WTC FT premium; from 2004: those eligible for WTC)
        FTDisreg = 0.0_dp
        
        !if (sys%fc%dofamcred) then
        
            !under FC system, need actual receipt of Family Credit (pre-October 1999)
            if (sys%rebatesys%rulesunderFC) then

                if ((sys%fc%ftprem > tol) .and. (_famkids_)) then
                    if (net%tu%fc>tol) then
                        if (.not. _famcouple_) then
                            if (fam%ad(1)%hrs >= sys%fc%hours2-tol) FTDisreg = sys%fc%ftprem
                        else
                            if ((fam%ad(1)%hrs >= sys%fc%hours2-tol) .or. (fam%ad(2)%hrs >= sys%fc%hours2-tol)) &
                                & FTDisreg = sys%fc%ftprem
                        end if
                    end if
                end if

            end if
            
            !under WFTC system, need only to be working full-time
            if (sys%rebatesys%rulesunderWFTC) then

                if ((sys%fc%ftprem > tol) .and. (_famkids_)) then
                    if (.not. _famcouple_) then
                        if (fam%ad(1)%hrs >= sys%fc%hours2-tol) FTDisreg = sys%fc%ftprem
                    else
                        if ((fam%ad(1)%hrs >= sys%fc%hours2-tol) .or. (fam%ad(2)%hrs >= sys%fc%hours2-tol)) &
                            & FTDisreg = sys%fc%ftprem
                    end if
                end if

            end if
        !end if
        
        !WTC (2003 onwards)
        !if (sys%ntc%donewtaxcred) then
            if (sys%rebatesys%rulesunderNTC) then
                !Rules less generous in first year of WTC (2003)
                if (.not. sys%wtc%NewDisregCon) then
                    if (_famkids_) then
                        if (_famcouple_) then
                            if (((fam%ad(1)%hrs >= sys%wtc%MinHrsKids-tol) .or. &
                                & (fam%ad(2)%hrs >= sys%wtc%MinHrsKids-tol)) &
                                & .and. (fam%ad(1)%hrs + fam%ad(2)%hrs >= sys%wtc%FTHrs-tol)) FTDisreg = sys%wtc%FT
                            else
                                if (fam%ad(1)%hrs >= sys%wtc%FTHrs-tol) FTDisreg = sys%wtc%FT
                        end if
                    else
                        if (_famcouple_) then
                            if (((fam%ad(1)%hrs >= sys%wtc%MinHrsNoKids-tol) .and. &
                                & (fam%ad(1)%age >= sys%wtc%MinAgeNoKids)) .or. &
                                & ((fam%ad(2)%hrs >= sys%wtc%MinHrsNoKids-tol) .and. &
                                & (fam%ad(2)%age >= sys%wtc%MinAgeNoKids))) &
                                & FTDisreg = sys%wtc%FT
                        else
                            if ((fam%ad(1)%hrs >= sys%wtc%MinHrsNoKids-tol) .and. &
                                & (fam%ad(1)%age >= sys%wtc%MinAgeNoKids)) FTDisreg = sys%wtc%FT
                        end if

                    end if

                ! 2004 onwards
                else
                    if (_famkids_) then
                        if (_famcouple_) then
                            if ((fam%ad(1)%hrs >= sys%wtc%MinHrsKids-tol) &
                                & .or. (fam%ad(2)%hrs >= sys%wtc%MinHrsKids-tol)) &
                                & FTDisreg = sys%wtc%NewDisreg                       
                        else
                            if (fam%ad(1)%hrs >= sys%wtc%MinHrsKids-tol) FTDisreg = sys%wtc%NewDisreg
                        end if
                    else
                        if (_famcouple_) then
                            if ((fam%ad(1)%hrs >= sys%wtc%MinHrsNoKids-tol) .or. &
                                & (fam%ad(2)%hrs >= sys%wtc%MinHrsNoKids-tol)) &
                                & FTDisreg = sys%wtc%NewDisreg                        
                        else
                            if (fam%ad(1)%hrs >= sys%wtc%MinHrsNoKids-tol) FTDisreg = sys%wtc%NewDisreg
                        end if
                    end if

                end if
                
            end if
            
        !end if
        
    end function FTDisreg


    ! ChCareDisreg
    ! -----------------------------------------------------------------------
    ! HB/CCB/CTB: disregard for childcare costs. Depends on couple, hrs, 
    ! nkids, kidage, ccexp

    !DEC$ ATTRIBUTES FORCEINLINE :: ChCareDisreg    
    real(dp) pure function ChCareDisreg(sys,fam,net)
    
        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in) :: sys
        type(fam_t), intent(in) :: fam
        type(net_t), intent(in) :: net

        integer                 :: i, nkidscc
        logical                 :: elig

        ChCareDisreg = 0.0_dp
        
        if ((sys%rebatesys%MaxCC1 > tol) .and. (_famkids_) .and. (fam%ccexp > tol)) then

            !Check TU is working enough
            elig = .false.
            if (sys%rebatesys%rulesunderFC .or. sys%rebatesys%rulesunderWFTC) then
                if (_famcouple_) then
                    if ((fam%ad(1)%hrs >= sys%fc%hours1-tol) .and. (fam%ad(2)%hrs >= sys%fc%hours1-tol)) elig = .true.
                else               
                    if (fam%ad(1)%hrs >= sys%fc%hours1-tol) elig = .true.
                end if
            end if
            
            if (sys%rebatesys%rulesunderNTC) then
            
                if (_famcouple_) then
                    if ((fam%ad(1)%hrs >= sys%wtc%MinHrsKids-tol) &
                        & .and. (fam%ad(2)%hrs >= sys%wtc%MinHrsKids-tol)) elig = .true.                    
                else
                    if (fam%ad(1)%hrs >= sys%wtc%MinHrsKids-tol) elig = .true.
                end if

            end if

            !Count number of eligible kids
            nkidscc = 0
            do i = 1, fam%nkids
!                if (fam%kidage(i) <= sys%rebatesys%MaxAgeCC) nkidscc = nkidscc + 1
! Changed to strict inequality to compare with Taxben, JS 09/06/09
                if (fam%kidage(i) < sys%rebatesys%MaxAgeCC) nkidscc = nkidscc + 1
            end do

            if ((elig) .and. (nkidscc > 0)) then

                if (nkidscc == 1) then
                    ChCareDisreg = min(fam%ccexp,sys%rebatesys%MaxCC1)
                else
                    if (sys%rebatesys%MaxCC2 > tol) then
                        ChCareDisreg = min(fam%ccexp,sys%rebatesys%MaxCC2)
                    else
                        ChCareDisreg = min(fam%ccexp,sys%rebatesys%MaxCC1)
                    end if
                end if

            end if

        end if

    end function ChCareDisreg


    ! MaintDisreg
    ! -----------------------------------------------------------------------
    ! Maintenance disregard. Depends on nkids

    !DEC$ ATTRIBUTES FORCEINLINE :: MaintDisreg
    real(dp) pure function MaintDisreg(sys,fam)

        use fortax_type, only : sys_t, fam_t
        
        implicit none

        type(sys_t), intent(in) :: sys
        type(fam_t), intent(in) :: fam
 
        if (_famkids_) then
            MaintDisreg = sys%rebatesys%MaintDisreg
        else
            MaintDisreg = 0.0_dp
        end if

    end function MaintDisreg


    ! ----------------------TAX CREDITS----------------------

    ! NTC       - Calculates CTC and WTC award of family
    ! MaxCTCFam - Calculates pre-taper CTC family element
    ! MaxCTCKid - Calculates pre-taper CTC child element
    ! MaxWTC    - Calculates pre-taper WTC
    ! NTCTaper  - Performs tax credit taper calculation
    ! FamCred   - Calculates family credit
    ! MaxFC     - Calculates pre-taper FC (for FamCred)
    ! FCDisreg  - Calculates earnings disregard (for FamCred)

    ! ----------------------TAX CREDITS----------------------


    ! NTC
    ! -----------------------------------------------------------------------
    ! WTC/CTC entitlement. Depends on couple, age, hrs, nkids, kidage, ccexp,
    ! grearn

    !DEC$ ATTRIBUTES FORCEINLINE :: NTC
    pure subroutine NTC(sys,fam,net)

        use fortax_type, only : sys_t, fam_t, net_t
                
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        
        real(dp)                   :: MaxWTC

        call MaxWTCamt(sys,fam,net,MaxWTC)
        call NTCTaper(sys,fam,net,MaxWTC,MaxCTCFam(sys,fam),MaxCTCKid(sys,fam))

    end subroutine NTC


    ! MaxCTCFam
    ! -----------------------------------------------------------------------
    ! Family element of CTC (separate from child element for later taper 
    ! calculation). Depends on nkids, ageyng
    
    !DEC$ ATTRIBUTES FORCEINLINE :: MaxCTCFam    
    real(dp) pure function MaxCTCFam(sys,fam)

        use fortax_type, only : sys_t, fam_t
        
        implicit none
        
        type(sys_t), intent(in) :: sys
        type(fam_t), intent(in) :: fam
        
        select case (fam%nkids)

        case (0)
            MaxCTCFam = 0.0_dp
        case (1:)
            select case (fam%yngkid)
            case (0)
                MaxCTCFam = sys%ctc%fam + sys%ctc%baby
            case (1:)
                MaxCTCFam = sys%ctc%fam
            end select

        end select
        
    end function MaxCTCFam

    
    ! MaxCTCKid
    ! -----------------------------------------------------------------------
    ! Child element of CTC. Depends on nkids

    !DEC$ ATTRIBUTES FORCEINLINE :: MaxCTCKid
    real(dp) pure function MaxCTCKid(sys,fam)

        use fortax_type, only : sys_t, fam_t
        
        implicit none
        
        type(sys_t), intent(in) :: sys
        type(fam_t), intent(in) :: fam
        
        select case (fam%nkids)
        case (0)
            MaxCTCKid = 0.0_dp
        case (1:)
            MaxCTCKid = real(fam%nkids,dp)*sys%ctc%kid
        end select
        
    end function MaxCTCKid

    
    ! MaxWTCamt
    ! -----------------------------------------------------------------------
    ! Working Tax Credit (including childcare element). Depends on couple, 
    ! age, hrs, nkids, kidage, ccexp
    
    !DEC$ ATTRIBUTES FORCEINLINE :: MaxWTCamt
    pure subroutine MaxWTCamt(sys,fam,net,MaxWTC)

        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none
        
        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        real(dp),    intent(out)   :: MaxWTC
        
        integer                    :: i, nkidscc
        
        net%tu%chcaresub = 0.0_dp

        select case (fam%nkids)
        case (0)
                      
            if (_famcouple_) then
                ! Childless couples
                if ((fam%ad(1)%age >= sys%wtc%MinAgeNoKids .and. fam%ad(1)%hrs >= sys%wtc%MinHrsNoKids-tol) &
                    & .or. (fam%ad(2)%age >= sys%wtc%MinAgeNoKids &
                    & .and. fam%ad(2)%hrs >= sys%wtc%MinHrsNoKids-tol)) then
                    MaxWTC = sys%wtc%CouLP + sys%wtc%FT
                else
                    MaxWTC = 0.0_dp
                end if
            else
                ! Childless singles
                if (fam%ad(1)%age >= sys%wtc%MinAgeNoKids .and. fam%ad(1)%hrs >= sys%wtc%MinHrsNoKids-tol) then
                    MaxWTC = sys%wtc%Basic + sys%wtc%FT
                else
                    MaxWTC = 0.0_dp
                end if                    
            end if

        case (1:)

            if (_famcouple_) then
                ! Couple parents
                if (fam%ad(1)%hrs >= sys%wtc%MinHrsKids-tol .or. fam%ad(2)%hrs >= sys%wtc%MinHrsKids-tol) then
                    if (fam%ad(1)%hrs + fam%ad(2)%hrs >= sys%wtc%FTHrs-tol) then
                        MaxWTC = sys%wtc%CouLP + sys%wtc%FT
                    else
                        MaxWTC = sys%wtc%CouLP
                    end if
                else
                    MaxWTC = 0.0_dp
                end if
            else
                ! Lone parents
                if (fam%ad(1)%hrs >= sys%wtc%MinHrsKids-tol) then
                    if (fam%ad(1)%hrs >= sys%wtc%FTHrs-tol) then
                        MaxWTC = sys%wtc%CouLP + sys%wtc%FT
                    else
                        MaxWTC = sys%wtc%CouLP
                    end if
                else
                    MaxWTC = 0.0_dp
                end if
            end if
        case default
        end select

        ! Childcare element
        if ((MaxWTC > tol) .and. (_famkids_) .and. (fam%ccexp > tol)) then

            nkidscc = 0
            do i = 1, fam%nkids
                if (fam%kidage(i) <= sys%wtc%MaxAgeCC) nkidscc = nkidscc + 1
            end do

            if (nkidscc == 1) then
                if (.not. _famcouple_) then
                    if (fam%ad(1)%hrs >= sys%wtc%MinHrsKids-tol) &
                        & net%tu%chcaresub = min(fam%ccexp,sys%wtc%MaxCC1)*sys%wtc%PropCC
                
                else
                    if ((fam%ad(1)%hrs >= sys%wtc%MinHrsKids-tol) .and. (fam%ad(2)%hrs >= sys%wtc%MinHrsKids-tol)) &
                        & net%tu%chcaresub = min(fam%ccexp,sys%wtc%MaxCC1)*sys%wtc%PropCC
                end if

            else if (nkidscc > 1) then
                if (.not. _famcouple_) then
                    if (fam%ad(1)%hrs >= sys%wtc%MinHrsKids-tol) &
                        & net%tu%chcaresub = min(fam%ccexp,sys%wtc%MaxCC2)*sys%wtc%PropCC
                else
                    if ((fam%ad(1)%hrs >= sys%wtc%MinHrsKids-tol) .and. (fam%ad(2)%hrs >= sys%wtc%MinHrsKids-tol)) &
                        & net%tu%chcaresub = min(fam%ccexp,sys%wtc%MaxCC2)*sys%wtc%PropCC
                end if
            end if

            MaxWTC = MaxWTC + net%tu%chcaresub

        end if
        
    end subroutine MaxWTCamt


    ! NTCTaper
    ! -----------------------------------------------------------------------
    ! New Tax Credits taper. Assumes receipt for whole year. Calculation is 
    ! not on last year income (and 2500 disregard). Round(award/365)*365 not 
    ! done. Doesn't check that WTC and CTC award values are sensible 
    ! (hopefully they should be!)

    !DEC$ ATTRIBUTES FORCEINLINE :: NTCTaper
    pure subroutine NTCTaper(sys,fam,net,MaxWTC,MaxCTCFam,MaxCTCKid)

        use fortax_type, only : sys_t, fam_t, net_t
                
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        real(dp),    intent(in)    :: MaxWTC, MaxCTCFam, MaxCTCKid

        real(dp)                   :: Thr1, Thr2 !, famearn
                
!        famearn = fam%ad(1)%earn
!        if (_famcouple_) famearn = famearn + fam%ad(2)%earn

        if ((MaxCTCFam <= tol) .and. (MaxWTC <= tol)) then
            net%tu%wtc = 0.0_dp
            net%tu%ctc = 0.0_dp
        else
            ! Higher threshold if only entitled to CTC
            if ((MaxCTCFam > tol) .and. (MaxWTC <= tol)) then
                Thr1 = sys%ntc%thr1hi
            else
                ! Threshold lower in other cases
                Thr1 = sys%ntc%thr1lo
            end if

            ! Tapering calculations

            ! Taper WTC first
            net%tu%wtc = max(MaxWTC - max(net%tu%pretaxearn - Thr1, 0.0_dp)*sys%ntc%taper1, 0.0_dp)

            ! Next taper child elements of CTC
            net%tu%ctc = max(MaxCTCKid - max(net%tu%pretaxearn - Thr1 - MaxWTC/sys%ntc%taper1, 0.0_dp)*sys%ntc%taper1, 0.0_dp)
            
            ! Second threshold
            Thr2 = max((MaxWTC+MaxCTCKid)/sys%ntc%taper1 + Thr1, sys%ntc%thr2)
            
            ! Finally taper family element of CTC
            net%tu%ctc = net%tu%ctc + max(MaxCTCFam - max(net%tu%pretaxearn - Thr2, 0.0_dp)*sys%ntc%taper2, 0.0_dp)

            ! Award not made below a minimum level (50p)
            if (net%tu%wtc+net%tu%ctc < sys%ntc%MinAmt) then
                net%tu%wtc = 0.0_dp
                net%tu%ctc = 0.0_dp
            end if

        end if

    end subroutine NTCTaper


    ! FamCred
    ! -----------------------------------------------------------------------
    ! Family credit/WFTC awards. Depends on couple, hrs, nkids, kidage, 
    ! netearn, ccexp, maint
    
    !DEC$ ATTRIBUTES FORCEINLINE :: FamCred
    pure subroutine FamCred(sys,fam,net)

        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net

        real(dp)                    :: MaxFC, FCDisregAmt, MatGrInFC
        integer                     :: i

        call MaxFCamt(sys,fam,net,MaxFC)
        
        if (MaxFC > tol) then

            FCDisregAmt = FCDisreg(sys,fam)

            !calculate maximum maternity grant so we can taper it away with tax credits, AS
            if (sys%extra%matgrant .and. fam%yngkid==0) then
                !call MatGrant(sys,fam,net,.true.)
                !MatGrInFC = net%tu%matgrant
                MatGrInFC = 0.0_dp
                do i=1,fam%nkids
                    if (fam%kidage(i) == 0) MatGrInFC = MatGrInFC + (sys%chben%MatGrantVal/52.0_dp)
                end do                
            else
                MatGrInFC = 0.0_dp
            end if

            net%tu%fc = max(MaxFC + MatGrInFC - max(max(net%tu%posttaxearn-FCDisregAmt,0.0_dp) &
                & + max(fam%maint-sys%fc%MaintDisreg,0.0_dp) - sys%fc%thres,0.0_dp)*sys%fc%taper, 0.0_dp)

            !50p rule
            if (net%tu%fc < sys%fc%MinAmt) net%tu%fc = 0.0_dp

            !re-assign income to correct categories, AS
            if (sys%extra%matgrant .and. fam%yngkid==0) then
                net%tu%matgrant = min(net%tu%fc,MatGrInFC)
                net%tu%fc  = max(0.0_dp,net%tu%fc - MatGrInFC)
            end if

        else
            net%tu%fc = 0.0_dp
            if (sys%extra%matgrant) net%tu%matgrant = 0.0_dp
        end if

    end subroutine FamCred


    ! MaxFCamt
    ! -----------------------------------------------------------------------
    ! FC/WFTC maximum entitlement. Depends on couple, hrs, nkids, kidage,
    ! ccexp
    
    !DEC$ ATTRIBUTES FORCEINLINE :: MaxFCamt
    pure subroutine MaxFCamt(sys,fam,net,MaxFC)

        use fortax_type, only : sys_t, fam_t, net_t

        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        real(dp),    intent(out)   :: MaxFC

        integer                    :: i, j, nkidscc       

        net%tu%chcaresub = 0.0_dp

        if (fam%nkids == 0) then
            MaxFC = 0.0_dp

        else
            !couple
            if (_famcouple_) then
                if ((fam%ad(1)%hrs >= sys%fc%hours1-tol) .or. (fam%ad(2)%hrs >= sys%fc%hours1-tol)) then
                    MaxFC = sys%fc%adult
                    if ((fam%ad(1)%hrs >= sys%fc%hours2-tol) .or. (fam%ad(2)%hrs >= sys%fc%hours2-tol)) &
                        & MaxFC = MaxFC + sys%fc%ftprem
                else
                    MaxFC = 0.0_dp
                end if
           
            !single
            else
                if (fam%ad(1)%hrs >= sys%fc%hours1-tol) then
                    MaxFC = sys%fc%adult
                    if (fam%ad(1)%hrs >= sys%fc%hours2-tol) MaxFC = MaxFC + sys%fc%ftprem
                else
                    MaxFC = 0.0_dp
                end if            
            end if

            if (MaxFC > tol) then

                !Child credits (doesn't use info about age of kids)
                do i = 1, fam%nkids
                    do j = 1, sys%fc%NumAgeRng
                        if ((fam%kidage(i)>=sys%fc%kidagel(j)) .and. (fam%kidage(i)<=sys%fc%kidageu(j))) then
                            MaxFC = MaxFC + sys%fc%kidcred(j)
                            exit
                        end if
                    end do
                end do

                !Childcare credit
                if ((fam%ccexp > tol) .and. (sys%fc%WFTCMaxCC1 > tol)) then

                    nkidscc = 0
                    do i = 1, fam%nkids
                        if (fam%kidage(i) <= sys%fc%WFTCMaxAgeCC) nkidscc = nkidscc + 1
                    end do

                    if (nkidscc == 1) then
                        if (.not. _famcouple_) then
                            net%tu%chcaresub = min(fam%ccexp,sys%fc%WFTCMaxCC1)*sys%fc%WFTCPropCC
                        else if ((fam%ad(1)%hrs >= sys%fc%hours1-tol) .and. (fam%ad(2)%hrs >= sys%fc%hours1-tol)) then
                            net%tu%chcaresub = min(fam%ccexp,sys%fc%WFTCMaxCC1)*sys%fc%WFTCPropCC
                        end if

                    else if (nkidscc >= 2) then
                        if (.not. _famcouple_) then
                            net%tu%chcaresub = min(fam%ccexp,sys%fc%WFTCMaxCC2)*sys%fc%WFTCPropCC
                        else if ((fam%ad(1)%hrs >= sys%fc%hours1-tol) .and. (fam%ad(2)%hrs >= sys%fc%hours1-tol)) then
                            net%tu%chcaresub = min(fam%ccexp,sys%fc%WFTCMaxCC2)*sys%fc%WFTCPropCC
                        end if

                    end if

                    MaxFC = MaxFC + net%tu%chcaresub
                end if

            end if

        end if

    end subroutine MaxFCamt


    ! FCDisreg
    ! -----------------------------------------------------------------------
    ! FC: earnings disregard for childcare expenditure (only get this if all
    ! adults in TU work 16+ hrs). Depends on couple, hrs, nkids, kidage,
    ! ccexp
    
    !DEC$ ATTRIBUTES FORCEINLINE :: FCDisreg
    real(dp) pure function FCDisreg(sys,fam)

        use fortax_type, only : sys_t, fam_t

        implicit none

        type(sys_t), intent(in) :: sys
        type(fam_t), intent(in) :: fam
        
        integer                 :: i, nkidscc
        logical                 :: elig

        if ((sys%fc%MaxCC1 > tol) .and. (_famkids_) .and. (fam%ccexp > tol)) then

            !Check TU is working enough
            elig = .false.
            if (_famcouple_) then
                if ((fam%ad(1)%hrs >= sys%fc%hours1-tol) .and. (fam%ad(2)%hrs >= sys%fc%hours1-tol)) elig = .true.
            else
                if (fam%ad(1)%hrs >= sys%fc%hours1-tol) elig = .true.                
            end if

            !Number of children eligible for credit
            nkidscc = 0
            do i = 1, fam%nkids
!                if (fam%kidage(i) <= sys%fc%MaxAgeCC) nkidscc = nkidscc + 1
! Strict inequality to check with taxben, JS
                if (fam%kidage(i) < sys%fc%MaxAgeCC) nkidscc = nkidscc + 1
            end do

            if ((elig) .and. (nkidscc > 0))  then

                if (nkidscc == 1) then
                    FCDisreg = min(fam%ccexp,sys%fc%MaxCC1)
                else
                    if (sys%fc%MaxCC2 > tol) then
                        FCDisreg = min(fam%ccexp,sys%fc%MaxCC2)
                    else
                        FCDisreg = min(fam%ccexp,sys%fc%MaxCC1)
                    end if
                end if

            else
                FCDisreg = 0.0_dp

            end if

        else
            FCDisreg = 0.0_dp

        end if

    end function FCDisreg


    ! ----------------------CHILD BENEFIT/MAT GRANT/FSM----------------------

    ! ChBen    - Calculates child benefit
    ! MatGrant - Calculates maternity grant
    ! fsm      - Calculates free school meals

    ! ----------------------CHILD BENEFIT/MAT GRANT/FSM----------------------


    ! ChBen
    ! -----------------------------------------------------------------------
    ! Child benefit. LP rate was abolished in 1998. sys%chben%opf = 0 from 
    ! then on, even though existing claimants could still get it. Depends on
    ! couple, nkids

    !DEC$ ATTRIBUTES FORCEINLINE :: ChBen
    pure subroutine ChBen(sys,fam,net)

        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net

        if (_famkids_) then
            net%tu%chben = sys%chben%basic*fam%nkids + sys%chben%kid1xtr
            if (.not. _famcouple_) net%tu%chben = net%tu%chben + sys%chben%opf
        else
            net%tu%chben = 0.0_dp
        end if
    
    end subroutine ChBen


    ! MatGrant
    ! -----------------------------------------------------------------------
    ! Maternity grant. Depends on nkids, kidage, incsup, fc, ctcred
    
    !DEC$ ATTRIBUTES FORCEINLINE :: MatGrant
    pure subroutine MatGrant(sys,fam,net,calcmax)

        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)           :: sys
        type(fam_t), intent(in)           :: fam
        type(net_t), intent(inout)        :: net
        logical,     intent(in), optional :: calcmax

        integer                           :: i
        logical                           :: dogrant

        net%tu%matgrant = 0.0_dp
        dogrant = .false.

        if (present(calcmax)) dogrant = calcmax

        if (_famkids_) then
            !Pre-Apr-03: need to get IS/IB-JSA or FC/WFTC
            !Post-Apr-03: need to get IS/IB-JSA or > family element of CTC
            if (net%tu%incsup > tol) then
                dogrant = .true.
            else if (sys%rebatesys%rulesunderFC .or. sys%rebatesys%rulesunderWFTC) then
                if (net%tu%fc > tol) dogrant = .true.
            else if (sys%rebatesys%rulesunderNTC) then
                if (net%tu%ctc > MaxCTCFam(sys,fam) + tol) dogrant = .true.
            end if
            if (dogrant) then
                do i=1,fam%nkids
                    if (fam%kidage(i) == 0) net%tu%matgrant = net%tu%matgrant + (sys%chben%MatGrantVal/52.0_dp)
                end do
            end if
        end if
    
    end subroutine MatGrant


    ! fsm
    ! -----------------------------------------------------------------------
    ! Free school meals, Depends on kidage, incsup, ctc, wtc, pretaxearn

    !DEC$ ATTRIBUTES FORCEINLINE :: fsm
    pure subroutine fsm(sys,fam,net)
        
        use fortax_type, only : sys_t, fam_t, net_t
        
        implicit none

        type(sys_t), intent(in)    :: sys
        type(fam_t), intent(in)    :: fam
        type(net_t), intent(inout) :: net
        
        integer                     :: i

        ! Before April 2003, you get FSM if you're on IS/IB-JSA
        ! From April 2003, you also get it if you're on full CTC and zero WTC

        net%tu%fsm = 0.0_dp
        if (((net%tu%incsup > tol) .or. ((net%tu%ctc > tol) .and. (net%tu%wtc <= tol) &
            & .and. (net%tu%pretaxearn <= sys%ntc%thr1hi + tol))) .and. (.not. sys%extra%fsminappamt)) then
            do i = 1, fam%nkids
                if (fam%kidage(i)>4) net%tu%fsm = net%tu%fsm + sys%incsup%ValFSM
            end do
        end if


    end subroutine fsm



    ! ----------------------NET INCOME----------------------

    ! CalcNetInc - Calculates family net income

    ! ----------------------NET INCOME----------------------


    ! CalcNetInc
    ! -----------------------------------------------------------------------
    ! calculate overall family net income measure

    pure subroutine CalcNetInc(sys,fam,net)

        use fortax_type, only : sys_t, fam_t, net_t !, net_init

        implicit none

        type(sys_t), intent(in)  :: sys
        type(fam_t), intent(in)  :: fam
        type(net_t), intent(out) :: net
                
        real(dp)                 :: disregRebate

        !if CalcNetInit is called it will set all elements to zero.
        call CalcNetInit(net)

        ! Pre-tax summary measures
        !!!!!!!!!!!!!!!!!!!!!!!!!!
        net%ad%pretaxearn = fam%ad%earn
        net%tu%pretaxearn = net%ad(1)%pretaxearn
        if (_famcouple_) net%tu%pretaxearn = net%tu%pretaxearn + net%ad(2)%pretaxearn


        !1. NATIONAL INSURANCE
        !!!!!!!!!!!!!!!!!!!!!!
        
        ! Note: NI before IT because of relief on class 4 contributions

        call NatIns(sys,fam,net,i=1)
        
        if (_famcouple_) then
            call NatIns(sys,fam,net,i=2)
        else
            net%ad(2)%natins   = 0.0_dp
            net%ad(2)%natinsc1 = 0.0_dp
            net%ad(2)%natinsc2 = 0.0_dp
            net%ad(2)%natinsc4 = 0.0_dp
        end if


        !2. INCOME TAX
        !!!!!!!!!!!!!!

        call tearn(sys,fam,net)
        
        call inctax(sys,net,i=1)
        
        if (_famcouple_) then
            call inctax(sys,net,i=2)
        else
            net%ad(2)%inctax = 0.0_dp
        end if
        
        call taxafterctc(sys,fam,net)
        call taxaftermca(sys,fam,net)

        ! store post-tax earnings in net
        net%ad(1)%posttaxearn = fam%ad(1)%earn-net%ad(1)%inctax-net%ad(1)%natins
        net%tu%posttaxearn = net%ad(1)%posttaxearn

        if (_famcouple_) then
            net%ad(2)%posttaxearn = fam%ad(2)%earn-net%ad(2)%inctax-net%ad(2)%natins
            net%tu%posttaxearn = net%tu%posttaxearn + net%ad(2)%posttaxearn
        else
            net%ad(2)%posttaxearn = 0.0_dp
        end if

        
        !3. CHILD BENEFIT
        !!!!!!!!!!!!!!!!!
        
        call ChBen(sys,fam,net)

        
        !4. TAX CREDITS
        !!!!!!!!!!!!!!!       

        net%tu%wtc = 0.0_dp
        net%tu%ctc = 0.0_dp
        net%tu%fc  = 0.0_dp
        net%tu%chcaresub  = 0.0_dp
        net%tu%matgrant = 0.0_dp

        !FC and WFTC
        if (sys%fc%dofamcred) then
            call FamCred(sys,fam,net)
        end if
        
        !WTC and CTC
        if (sys%ntc%donewtaxcred) then
            call NTC(sys,fam,net)            
        end if


        !5. IS AND IB-JSA
        !!!!!!!!!!!!!!!!!
        
        call IncSup(sys,fam,net)

        ! Maternity grant
        if (.not. sys%extra%matgrant) call MatGrant(sys,fam,net)

        ! Free school meals
        call fsm(sys,fam,net)
        

        !6. HB, CTB AND CCB
        !!!!!!!!!!!!!!!!!!!

        ! Preliminary calculations (disregRebate passed to subsequent routines)
        call prelimcalc(sys,fam,net,disregRebate)
        
        ! Housing benefit
        call HBen(sys,fam,net,disregRebate)

        ! Poll tax (what about Northern Ireland?)
        if (sys%ccben%dopolltax) then
            call polltax(sys,fam,net)
            call polltaxBen(sys,fam,net,disregRebate)
        else
            net%tu%polltax    = 0.0_dp
            net%tu%polltaxben = 0.0_dp
        end if
        
        ! Council tax
        if (sys%ctax%docounciltax) then
            call ctax(sys,fam,net)
            call ctaxBen(sys,fam,net,disregRebate)
        else
            net%tu%ctax    = 0.0_dp
            net%tu%ctaxben = 0.0_dp
        end if


        ! Disposable income
        !!!!!!!!!!!!!!!!!!!

        net%tu%totben = net%tu%chben + net%tu%matgrant + net%tu%fc + net%tu%wtc + net%tu%ctc &
            & + net%tu%incsup + net%tu%fsm + net%tu%hben + net%tu%ctaxben + net%tu%polltaxben
            
        net%tu%nettax = net%ad(1)%inctax + net%ad(1)%natinsc1 + net%ad(1)%natinsc2 &
            & + net%ad(1)%natinsc4 + net%tu%ctax + net%tu%polltax &
            & - (net%tu%chben + net%tu%matgrant + net%tu%fc + net%tu%wtc + net%tu%ctc &
            & + net%tu%incsup + net%tu%fsm + net%tu%hben + net%tu%ctaxben + net%tu%polltaxben)

        if (_famcouple_) net%tu%nettax = net%tu%nettax + net%ad(2)%inctax &
            & + net%ad(2)%natinsc1 + net%ad(2)%natinsc2 + net%ad(2)%natinsc4

        !family pre-tax income
        net%tu%pretax = fam%ad(1)%earn + fam%maint
        if (_famcouple_) net%tu%pretax = net%tu%pretax + fam%ad(2)%earn

        !family post-tax income (main disposable income measure)
        net%tu%dispinc = net%tu%pretax - net%tu%nettax

        !set childcare expenditure
        net%tu%ccexp = fam%ccexp
        
    end subroutine CalcNetInc

    ! CalcNetInit
    ! -----------------------------------------------------------------------
    ! intializes net_t type. unless defaults are coded here, integers are
    ! set to 0, doubles to 0.0_dp and logicals to .false. (and similarly
    ! for arrays)
    ! 
    ! this is the same as net_init in fortax_type, it is reproduced here so
    ! we may inline without inter procedural optimization enabled. AS

    !DEC$ ATTRIBUTES FORCEINLINE :: CalcNetInit
    elemental subroutine CalcNetInit(net)

        use fortax_type, only : net_t

        implicit none

        type(net_t), intent(inout) :: net
        integer                    :: ad

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$header
#       define _$footer
#       define _$integer(x,lab,y) net%_$level(ad)%x = 0
#       define _$double(x,lab,y)  net%_$level(ad)%x = 0.0_dp
#       define _$logical(x,lab,y) net%_$level(ad)%x = .false.
#       define _$integerarray(x,lab,y,z) net%_$level(ad)%x = 0
#       define _$doublearray(x,lab,y,z)  net%_$level(ad)%x = 0.0_dp
#       define _$logicalarray(x,lab,y,z) net%_$level(ad)%x = .false.

#       define _$level ad
        do ad = 1, 2
#           include 'includes/netad_t.inc'
        end do
#       undef _$level

#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

#       define _$integer(x,lab,y) net%_$level%x = 0
#       define _$double(x,lab,y)  net%_$level%x = 0.0_dp
#       define _$logical(x,lab,y) net%_$level%x = .false.
#       define _$integerarray(x,lab,y,z) net%_$level%x = 0
#       define _$doublearray(x,lab,y,z)  net%_$level%x = 0.0_dp
#       define _$logicalarray(x,lab,y,z) net%_$level%x = .false.

#       define _$level tu
#       include 'includes/nettu_t.inc'
#       undef _$level

#       undef  _$header
#       undef  _$footer
#       undef  _$integer
#       undef  _$double
#       undef  _$logical
#       undef  _$doublearray
#       undef  _$integerarray
#       undef  _$logicalarray

    end subroutine CalcNetInit

end module fortax_calc
