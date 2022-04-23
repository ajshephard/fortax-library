
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




! fortax_extra
! -----------------------------------------------------------------------
! module provides various additional functionality, operating on systems,
! families, or income measures, AS


module fortax_extra

    use fortax_realtype, only : dp
    implicit none
    private :: dp
    private
    public :: setMinAmount, abolishNIFee, fsMinAppAmt, taperMatGrant, &
              imposeUC, disableTaperRounding
              ! , netoutDesc, netoutDescNoName

    integer, parameter :: netoutSize = 100

contains

    ! setMinAmount
    ! -----------------------------------------------------------------------
    ! sets the minimum amount for all benefits to the specified amount

    subroutine setMinAmount(sys, minamt)

        use fortax_type, only : sys_t

        implicit none

        type(sys_t), intent(inout) :: sys
        real(dp),    intent(in)    :: minamt

        !done automatically for entire system, AS
sys%fc%MinAmt = minamt
            sys%ntc%MinAmt = minamt
            sys%hben%MinAmt = minamt
            sys%ccben%MinAmt = minamt
            sys%uc%MinAmt = minamt

    end subroutine setMinAmount


    ! abolishNIFee
    ! -----------------------------------------------------------------------
    ! abolish national insurance entry fee (pre-99 there NI was paid on total
    ! earnings once threshold was reached. this removes that

    subroutine abolishNIFee(sys)

        use fortax_type, only : sys_t
        use fortax_util, only : fortaxwarn

        implicit none

        type(sys_t), intent(inout) :: sys
        real(dp) :: amt
        real(dp), parameter :: tol = 1e-5_dp

        if (sys%natins%numrates < 2) then
            call fortaxwarn('warning in abolishnifee: require sys%natins%numrates>=2')
        end if

        if (sys%natins%rates(1) > tol) then
            amt = sys%natins%rates(1) * sys%natins%bands(1)
            sys%natins%rates(1) = 0.0_dp
            sys%natins%bands(1) = sys%natins%bands(1) - amt / sys%natins%rates(2)
        end if

    end subroutine abolishNIFee

    ! disableTaperRounding
    ! -----------------------------------------------------------------------
    ! disables the rounding rules when tapering to stop introducing a large
    ! number of small discountinuities

    pure subroutine disableTaperRounding(sys)

        use fortax_type, only : sys_t

        type(sys_t), intent(inout) :: sys

        sys%chben%disableTaperRounding = 1
        sys%inctax%disablePATaperRounding = 1

    end subroutine disableTaperRounding

    ! fsMinAppAmt
    ! -----------------------------------------------------------------------
    ! high wage parents will lose entitlement to FSM when they come off IS
    ! smooth this by adding fsm in appamt. this works through sys%extra

    pure subroutine fsMinAppAmt(sys, inappamt)

        use fortax_type, only : sys_t

        implicit none

        type(sys_t), intent(inout) :: sys
        integer, intent(in) :: inappamt

        sys%extra%fsMinAppAmt = inappamt

    end subroutine fsMinAppAmt


    ! taperMatGrant
    ! -----------------------------------------------------------------------
    ! high wage parents will lose entitlement maternity grant when they come
    ! off IS. smooth this by adding matgr in appamt. Also, lose when they
    ! come off FC/WFTC (in this case, taper with tax credits)

    pure subroutine taperMatGrant(sys, taper)

        use fortax_type, only : sys_t

        implicit none

        type(sys_t), intent(inout) :: sys
        integer, intent(in) :: taper

        sys%extra%matgrant = taper

    end subroutine taperMatGrant


    ! imposeUC
    !----------------------------------------------------
    ! Imposes UC onto sys, eliminating entitlement to benefits it replaces

    pure subroutine imposeUC(sys)

        use fortax_type, only : sys_t

        implicit none

        type(sys_t), intent(inout)    :: sys

        ! UC replaces IS, HB, CTC, WTC

        ! Standard allowance (from IS)
        sys%uc%MainCou = sys%incsup%MainCou
        sys%uc%YngCou = sys%incsup%YngCou
        ! Check whether these single rates are the right ones TODO
        sys%uc%MainSin = sys%incsup%MainSin
        sys%uc%YngSin = sys%incsup%YngSin
        sys%uc%MinAgeMain = 25  ! IS has this information hard-coded (should change this)

        ! Child element (from CTC)
        sys%uc%FirstKid = sys%ctc%fam + sys%ctc%kid
        sys%uc%OtherKid = sys%ctc%kid

        ! Childcare element (from WTC)
        sys%uc%MaxCC1 = sys%wtc%MaxCC1
        sys%uc%MaxCC2 = sys%wtc%MaxCC2
        sys%uc%PropCC = sys%wtc%PropCC
        sys%uc%MaxAgeCC = sys%wtc%MaxAgeCC

        ! Housing costs components
        sys%uc%doRentCap = 0

        ! Disregards (monthly figures from legislation - need to be weeklyised)
        sys%uc%DisregSinNoKidsHi  = 111.0_dp * 12.0_dp / 52.0_dp
        sys%uc%DisregSinNoKidsLo  = 111.0_dp * 12.0_dp / 52.0_dp
        sys%uc%DisregSinKidsHi    = 734.0_dp * 12.0_dp / 52.0_dp
        sys%uc%DisregSinKidsLo    = 263.0_dp * 12.0_dp / 52.0_dp
        sys%uc%DisregCouNoKidsHi  = 111.0_dp * 12.0_dp / 52.0_dp
        sys%uc%DisregCouNoKidsLo  = 111.0_dp * 12.0_dp / 52.0_dp
        sys%uc%DisregCouKidsHi    = 536.0_dp * 12.0_dp / 52.0_dp
        sys%uc%DisregCouKidsLo    = 222.0_dp * 12.0_dp / 52.0_dp

        ! Disregards (annual figures from TAXBEN - need to be weeklyised)
        sys%uc%DisregSinNoKidsHi  = 1330.0_dp / 52.0_dp
        sys%uc%DisregSinNoKidsLo  = 1330.0_dp / 52.0_dp
        sys%uc%DisregSinKidsHi    = 8812.0_dp / 52.0_dp
        sys%uc%DisregSinKidsLo    = 3159.0_dp / 52.0_dp
        sys%uc%DisregCouNoKidsHi  = 1330.0_dp / 52.0_dp
        sys%uc%DisregCouNoKidsLo  = 1330.0_dp / 52.0_dp
        sys%uc%DisregCouKidsHi    = 6429.0_dp / 52.0_dp
        sys%uc%DisregCouKidsLo    = 2660.0_dp / 52.0_dp

        ! Taper
        sys%uc%taper = sys%hben%taper

        ! Minimum UC (monthly figure from legislation - need to be weeklyised)
        sys%uc%MinAmt = 0.01_dp * 12.0_dp / 52.0_dp

        ! Also need to set some benefit cap parameters
        sys%bencap%doThruUC = 1
        sys%bencap%UCEarnThr = 430.0_dp * 12.0_dp / 52.0_dp

        ! Turn UC on
        sys%uc%doUnivCred = 1

        ! Turn IS, HB and all tax credits (FC/WFTC/WTC/CTC) off
        sys%incsup%doIncSup   = 0
        sys%hben%doHBen       = 0
        sys%fc%dofamcred      = 0
        sys%ntc%doNewTaxCred  = 0

        ! Change CTB to operate under UC rules
        sys%rebatesys%rulesUnderUC    = 1
        sys%rebatesys%rulesUnderNTC   = 0
        sys%rebatesys%rulesUnderWFTC  = 0
        sys%rebatesys%rulesUnderFC    = 0


    end subroutine imposeUC

    ! netoutDesc
    ! -----------------------------------------------------------------------
    ! provides the shortname, level and amount of net in arrays of size
    ! netoutSize. Useful for passing data without using derived types

!     pure subroutine netoutDesc(net,netoutLevel,netoutName,netoutAmt,netoutNum)

!         use fortax_type, only : net_t

!         implicit none

!         type(net_t),       intent(in)  :: net
!         character(len=16), intent(out) :: netoutLevel(netoutSize), netoutName(netoutSize)
!         real(dp),          intent(out) :: netoutAmt(netoutSize)
!         integer,           intent(out) :: netoutNum

!         integer :: i

!         i = 1
! #       undef  _$header
! #       undef  _$footer
! #       undef  _$double
! #       define _$header
! #       define _$footer
! #       define _$double(x,lab,y) netoutLevel(i) ='tu'; netoutName(i) = #x; netoutAmt(i) = net%tu%x; i=i+1
! #       include "includes/nettu_t.inc"
! #       undef  _$header
! #       undef  _$footer
! #       undef  _$double

! #       undef  _$header
! #       undef  _$footer
! #       undef  _$double
! #       define _$header
! #       define _$footer
! #       define _$double(x,lab,y) netoutLevel(i) ='ad1'; netoutName(i) = #x; netoutAmt(i) = net%ad(1)%x; i=i+1
! #       include "includes/netad_t.inc"
! #       undef  _$header
! #       undef  _$footer
! #       undef  _$double

! #       undef  _$header
! #       undef  _$footer
! #       undef  _$double
! #       define _$header
! #       define _$footer
! #       define _$double(x,lab,y) netoutLevel(i) ='ad2'; netoutName(i) = #x; netoutAmt(i) = net%ad(2)%x; i=i+1
! #       include "includes/netad_t.inc"
! #       undef  _$header
! #       undef  _$footer
! #       undef  _$double

!         netoutNum = i-1

!     end subroutine netoutDesc

    ! netoutDesc
    ! -----------------------------------------------------------------------
    ! provides the shortname, level and amount of net in arrays of size
    ! netoutSize. Useful for passing data without using derived types

!     pure subroutine netoutDescNoName(net,netoutAmt)

!         use fortax_type, only : net_t

!         implicit none

!         type(net_t),   intent(in)  :: net
!         real(dp),      intent(out) :: netoutAmt(netoutSize)

!         integer :: i

!         i = 1
! #       undef  _$header
! #       undef  _$footer
! #       undef  _$double
! #       define _$header
! #       define _$footer
! #       define _$double(x,lab,y) netoutAmt(i) = net%tu%x; i=i+1
! #       include "includes/nettu_t.inc"
! #       undef  _$header
! #       undef  _$footer
! #       undef  _$double

! #       undef  _$header
! #       undef  _$footer
! #       undef  _$double
! #       define _$header
! #       define _$footer
! #       define _$double(x,lab,y) netoutAmt(i) = net%ad(1)%x; i=i+1
! #       include "includes/netad_t.inc"
! #       undef  _$header
! #       undef  _$footer
! #       undef  _$double

! #       undef  _$header
! #       undef  _$footer
! #       undef  _$double
! #       define _$header
! #       define _$footer
! #       define _$double(x,lab,y) netoutAmt(i) = net%ad(2)%x; i=i+1
! #       include "includes/netad_t.inc"
! #       undef  _$header
! #       undef  _$footer
! #       undef  _$double

!     end subroutine netoutDescNoName

end module fortax_extra
