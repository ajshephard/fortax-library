
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




! fortax_kinks3d
! -----------------------------------------------------------------------
! module calculates income measures as the earnings or hours of both
! adults in a couple is varied

module fortax_kinks3d

    use fortax_realtype, only : dp
    implicit none
    private :: dp
    private
#   ifndef _maxkinks3d_
    integer, parameter :: maxkinks = 200
#   else
    integer, parameter :: maxkinks = _maxkinks3d_
#   endif /* _maxkinks3d_ */
#   undef _maxkinks3d_

    type :: bcout3d_t
        integer :: kinks_num
        real(dp), dimension(maxkinks) :: kinks_hrs, kinks_earn, kinks_net, kinks_mtr
    end type bcout3d_t

    public :: bcout_t, evalKinksHours, evalKinksEarn, kinkshours, kinksearn, kinksccexp, maxkinks

contains

    pure subroutine calcTaxCoef(tax)
        implicit none
        type(tax_t), intent(inout) :: tax
        integer :: iX1, iX2
        real(dp) :: P(3), Q(3), R(3)
        real(dp) :: u(3)
        real(dp) :: v(3)
        real(dp) :: c(3)

        ! couples, 3d surface
        do iX2 = 1, NTAXBR-1
            do iX1 = 1, NTAXBR-1

                ! lower triangle
                P(1) = TAXBR(iX1)
                P(2) = TAXBR(iX2)
                P(3) = tax%tCou(iX1,iX2)
                Q(1) = TAXBR(iX1)
                Q(2) = TAXBR(iX2+1)
                Q(3) = tax%tCou(iX1,iX2+1)
                R(1) = TAXBR(iX1+1)
                R(2) = TAXBR(iX2)
                R(3) = tax%tCou(iX1+1,iX2)
                
                u = Q-P
                v = R-P
                
                c(1) = u(2)*v(3)-u(3)*v(2)
                c(2) = u(3)*v(1)-u(1)*v(3)
                c(3) = u(1)*v(2)-u(2)*v(1)

                tax%tCouL(1,iX1,iX2) = P(3) + (c(1)*P(1) + c(2)*P(2))/c(3)
                tax%tCouL(2,iX1,iX2) = -c(1)/c(3)
                tax%tCouL(3,iX1,iX2) = -c(2)/c(3)

                ! upper triangle
                P(1) = TAXBR(iX1+1)
                P(2) = TAXBR(iX2)
                P(3) = tax%tCou(iX1+1,iX2)
                Q(1) = TAXBR(iX1+1)
                Q(2) = TAXBR(iX2+1)
                Q(3) = tax%tCou(iX1+1,iX2+1)
                R(1) = TAXBR(iX1)
                R(2) = TAXBR(iX2+1)
                R(3) = tax%tCou(iX1,iX2+1)

                u = Q-P
                v = R-P
                
                c(1) = u(2)*v(3)-u(3)*v(2)
                c(2) = u(3)*v(1)-u(1)*v(3)
                c(3) = u(1)*v(2)-u(2)*v(1)

                tax%tCouU(1,iX1,iX2) = P(3) + (c(1)*P(1) + c(2)*P(2))/c(3)
                tax%tCouU(2,iX1,iX2) = -c(1)/c(3)
                tax%tCouU(3,iX1,iX2) = -c(2)/c(3)

            end do
        end do

    end subroutine calcTaxCoef

    pure function evalTaxCouScalar(tax,earn1,earn2) result(y)
        implicit none
        type(tax_t), intent(in) :: tax
        real(dp),    intent(in) :: earn1, earn2
        real(dp) :: y
        integer :: iX1, iX2

        if (earn1<=TAXBR(1) .and. earn2<=TAXBR(1)) then
            y = tax%tCouL(1,1,1)
        else if (earn1>=TAXBR(NTAXBR) .and. earn2>=TAXbr(NTAXBR)) then
            y = tax%tCouU(1,NTAXBR-1,NTAXBR-1) + tax%tCouU(2,NTAXBR-1,NTAXBR-1)*earn1 &
              + tax%tCouU(3,NTAXBR-1,NTAXBR-1)*earn2
        else
            do iX2 = 1, NTAXBR-1
                if ( earn2>=TAXBR(iX2) .and. ( earn2<TAXBR(iX2+1) .or. (iX2==NTAXBR-1 .and. earn2>=TAXBR(NTAXBR)) ) ) then
                    do iX1 = 1, NTAXBR-1
                        if ( earn1>=TAXBR(iX1) .and. ( earn1<TAXBR(iX1+1) .or. (iX1==NTAXBR-1 .and. earn1>=TAXBR(NTAXBR) ) ) ) then
                            if ( earn1-TAXBR(iX1) + (earn2-TAXBR(iX2))*TAXDBR(iX1)/TAXDBR(iX2) >=(TAXDBR(iX1)) ) then
                                y = tax%tCouU(1,iX1,iX2) + tax%tCouU(2,iX1,iX2)*earn1 + tax%tCouU(3,iX1,iX2)*earn2
                            else
                                y = tax%tCouL(1,iX1,iX2) + tax%tCouL(2,iX1,iX2)*earn1 + tax%tCouL(3,iX1,iX2)*earn2
                            end if
                            exit
                        end if
                    end do
                    exit
                end if
            end do
        end if
    end function evalTaxCouScalar


    pure subroutine setTax(x,tax,mode)
        implicit none
        real(dp), intent(in) :: x(NTAXPARAM)
        type(tax_t), intent(out) :: tax
        integer, intent(in) :: mode
        integer :: ixL0, ixL1, ixParam
        integer :: ixL0_1, ixParam_1

        ! initialize derivatives to zero
        tax%tDerSin  = 0.0_dp
        tax%tDerSinL = 0.0_dp
        tax%tDerCou   = 0.0_dp
        tax%tDerCouL  = 0.0_dp
        tax%tDerCouU  = 0.0_dp

        select case(mode)
        case(TAXLEVEL)
            tax%tSin = x(1:NTAXBR) ! tax parameters for singles
            do ixL0 = 1, NTAXBR
                tax%tDerSin(ixL0,ixL0) = 1.0_dp
            end do
            ixParam = NTAXBR
            do ixL1 = 1, NTAXBR
                do ixL0 = ixL1, NTAXBR
                    ixParam = ixParam+1
                    tax%tCou(ixL0,ixL1) = x(ixParam)
                    tax%tDerCou(ixParam,ixL0,ixL1) = 1.0_dp
!                     print *, ixL0,ixL1,tax%tCou(ixL0,ixL1)
                end do
            end do

            ! symmetry
            do ixL1 = 1, NTAXBR
                do ixL0 = 1, ixL1-1
                    tax%tCou(ixL0,ixL1) = tax%tCou(ixL1,ixL0)
                    tax%tDerCou(ixParam,ixL0,ixL1) = tax%tDerCou(ixParam,ixL1,ixL0)
                end do
            end do
        case(TAXRATE)

            ! reconstruct tax levels from marginal tax rates
            ! singles
            tax%tSin(1) = x(1)
            tax%tDerSin(1,1) = 1.0_dp
            do ixL0 = 2, NTAXBR
                tax%tSin(ixL0) = tax%tSin(ixL0-1) + x(ixL0)*TAXDBR(ixL0-1)
                do ixL0_1 = 1, ixL0-1
                    tax%tDerSin(ixL0_1,ixL0) = tax%tDerSin(ixL0_1,ixL0-1)
                end do
                tax%tDerSin(ixL0,ixL0) = TAXDBR(ixL0-1)
            end do
        

            ! couples
            tax%tCou(1,1) = x(NTAXBR+1)
            tax%tDerCou(NTAXBR+1,1,1) = 1.0_dp
            ixParam = NTAXBR+1
            do ixL1 = 1, NTAXBR-1

!                 if (ixL1>1) then
!                     tax%tCou(ixL0,ixL1) = tax%tCou(ixL0-1,ixL1) + x(ixParam)*TAXDBR(ixL0-1)                
!                 end if

                do ixL0 = ixL1+1, NTAXBR
                    ixParam = ixParam+1
                    tax%tCou(ixL0,ixL1) = tax%tCou(ixL0-1,ixL1) + x(ixParam)*TAXDBR(ixL0-1)
                    do ixParam_1 = 1, ixParam-1
                        tax%tDerCou(ixParam_1,ixL0,ixL1) = tax%tDerCou(ixParam_1,ixL0-1,ixL1)
                    end do
                    tax%tDerCou(ixParam,ixL0,ixL1) = TAXDBR(ixL0-1)
!                     print *, ixL0, ixL1, tax%tCou(ixL0,ixL1) ,x(ixParam)
!                     stop
                end do
                ixParam = ixParam+1
!                 here
                tax%tCou(ixL1+1,ixL1+1) = tax%tCou(ixL1+1,ixL1) + x(ixParam)*TAXDBR(ixL1)
                do ixParam_1 = 1, ixParam-1
                    tax%tDerCou(ixParam_1,ixL1+1,ixL1+1) = tax%tDerCou(ixParam_1,ixL1+1,ixL1)
                end do
                tax%tDerCou(ixParam,ixL1+1,ixL1+1) = TAXDBR(ixL1)

            end do
            ! symmetry
            do ixL1 = 1, NTAXBR
                do ixL0 = 1, ixL1-1
                    tax%tCou(ixL0,ixL1) = tax%tCou(ixL1,ixL0)
                    do ixParam = NTAXBR+1,NTAXPARAM
                        tax%tDerCou(ixParam,ixL0,ixL1) = tax%tDerCou(ixParam,ixL1,ixL0)
                    end do
                end do
            end do            
!             do ixL0 = 2, NTAXBR
!                 ixParam = ixParam+1
!                 tax%tCou(ixL0,1) = tax%tCou(ixL0-1,1) + x(ixParam)*TAXDBR(ixL0-1)
!             end do
!             ixParam = ixParam+1
!             tax%tCou(2,2) = tax%tCou(2,1) + x(ixParam)*TAXDBR(1)
!             do ixL0 = 3, NTAXBR
!                 ixParam = ixParam+1
!                 tax%tCou(ixL0,2) = tax%tCou(ixL0-1,1) + x(ixParam)*TAXDBR(ixL0-1)
!             end do
!             tax%tCou(3,3) = tax%tCou(2,1) + x(ixParam)*TAXDBR(1)
        end select

        call calcTaxCoef(tax)

    end subroutine setTax

    subroutine printTaxX(tax)
        implicit none
        type(tax_t), intent(in) :: tax
        integer :: ixL0, ixL1, ixParam

        print *, 'SINGLES'
        print *, 1, tax%tSin(1), tax%tSin(1)
        do ixL0 = 2, NTAXBR
            print *, ixL0, tax%tSin(ixL0), (tax%tSin(ixL0)-tax%tSin(ixL0-1))/TAXDBR(ixL0-1)
        end do

        ixParam = NTAXBR+1
        print *, 'COUPLES'
        print *, ixParam, tax%tCou(1,1),tax%tCou(1,1)
!         print *, ixParam, tax%tCou(ixL0,ixL1),tax%tCou(ixL0,ixL1)
        do ixL1 = 1, NTAXBR-1
            do ixL0 = ixL1+1, NTAXBR
                ixParam = ixParam+1
!                 if (ixL0==1) then
!                     print *, NTAXBR+1, tax%tCou(1,1), tax%tCou(1,1)
!                 else
!                     if (ixL1==1 .and. ixL0==1) then
!                         print *, ixParam, tax%tCou(ixL0,ixL1),tax%tCou(ixL0,ixL1)
!                     else
!                         stop
                        print *, ixParam, tax%tCou(ixL0,ixL1), (tax%tCou(ixL0,ixL1)-tax%tCou(ixL0-1,ixL1))/TAXDBR(ixL0-1)
!                     end if
!                 end if
            end do
            ixParam = ixParam+1
            print *, ixParam, tax%tCou(ixL1+1,ixL1+1), (tax%tCou(ixL1+1,ixL1+1)-tax%tCou(ixL1+1,ixL1))/TAXDBR(ixL1)
        end do
!         ixParam = ixParam+1
!         print *, ixParam, tax%tCou(NTAXBR,NTAXBR), (tax%tCou(NTAXBR,NTAXBR)-tax%tCou(NTAXBR,NTAXBR-1))/TAXDBR(NTAXBR-1)

    end subroutine printTaxX

    ! evalKinksHours
    ! -----------------------------------------------------------------------
    ! uses the piecewise linear budget constraint bcout as calculated in
    ! kinkshours to evaluate the respective income measure at hours

    pure subroutine evalKinksHours(bcout,hours,earn,net,mtr,iin,iout)

        implicit none

        type(bcout_t),  intent(in)  :: bcout
        real(dp),       intent(in)  :: hours
        real(dp),       intent(out) :: earn,net,mtr
        integer,        intent(in), optional :: iin
        integer,        intent(out), optional :: iout
        integer :: i, j, k
        real(dp) :: wage
        i = 1
        j = bcout%kinks_num

        if (hours>=bcout%kinks_hrs(bcout%kinks_num)) then
            i = bcout%kinks_num
        elseif (hours<=bcout%kinks_hrs(1)) then
            i = 1
        else

            if (present(iin)) then

                if (hours.ge.bcout%kinks_hrs(iin)) then
                    do j = iin+1,bcout%kinks_num
                        !print *,'ge',j
                        if (hours<bcout%kinks_hrs(j)) then
                            i=j-1
                            exit
                        end if
                    end do
                else
                    do j = iin-1,1,-1
                        !print *,'lt',j
                        if (hours>bcout%kinks_hrs(j)) then
                            i=j !+1
                            exit
                        end if
                    end do
                end if

            else

                do
                    k=(i+j)/2
                    if (hours<bcout%kinks_hrs(k)) then
                        j=k
                    else
                        i=k
                    end if
                    if (i+1>=j) exit
                end do
            end if
        end if

        wage = bcout%kinks_earn(2)/bcout%kinks_hrs(2)
        mtr  = bcout%kinks_mtr(i)
        net  = bcout%kinks_net(i) + mtr*wage*(hours-bcout%kinks_hrs(i))
        earn = wage*hours

        if (present(iout)) then
            iout = i
        end if

    end subroutine evalKinksHours


    ! evalKinksEarn
    ! -----------------------------------------------------------------------
    ! uses the piecewise linear budget constraint bcout as calculated in
    ! kinksearn to evaluate the respective income measure at earn

    pure subroutine evalKinksEarn(bcout,earn,hours,net,mtr,iin,iout)

        implicit none

        type(bcout_t),  intent(in)  :: bcout
        real(dp),       intent(in)  :: earn
        real(dp),       intent(out) :: hours,net,mtr
        integer,        intent(in), optional :: iin
        integer,        intent(out), optional :: iout
        integer :: i, j, k

        i = 1
        j = bcout%kinks_num

        if (earn>=bcout%kinks_earn(bcout%kinks_num)) then
            i = bcout%kinks_num
        elseif (earn<=bcout%kinks_earn(1)) then
            i = 1
        else

            if (present(iin)) then

                if (earn.ge.bcout%kinks_earn(iin)) then
                    do j = iin+1,bcout%kinks_num
                        !print *,'ge',j
                        if (earn<bcout%kinks_earn(j)) then
                            i=j-1
                            exit
                        end if
                    end do
                else
                    do j = iin-1,1,-1
                        !print *,'lt',j
                        if (earn>bcout%kinks_earn(j)) then
                            i=j !+1
                            exit
                        end if
                    end do
                end if

            else

                do
                    k=(i+j)/2
                    if (earn<bcout%kinks_earn(k)) then
                        j=k
                    else
                        i=k
                    end if
                    if (i+1>=j) exit
                end do
            end if
        end if

        mtr   = bcout%kinks_mtr(i)
        net   = bcout%kinks_net(i) + mtr*(earn-bcout%kinks_earn(i))
        hours = bcout%kinks_hrs(i)

        if (present(iout)) then
            iout = i
        end if

    end subroutine evalKinksEarn


    ! kinkshours
    ! -----------------------------------------------------------------------
    ! calculates a piecewise linear schedule under the a given tax system
    ! for a family by varying hours of work with a fixed hourly wage. can be
    ! performed for any income component (or linear combination of)

    subroutine kinkshours(sys,fam,ad,wage,hours1,hours2,bcout,taxlevel,taxout,correct,verbose)

#       undef  _ZEROWAGE_
#       define _ZEROWAGE_

        use fortax_type, only : fam_t, sys_t, net_t
        use fortax_util, only : lower, inttostr, fortaxerror, fortaxwarn
        use fortax_calc, only : calcnetinc

        implicit none

        type(sys_t),    intent(in)  :: sys
        type(fam_t),    intent(in)  :: fam
        integer,        intent(in)  :: ad
        real(dp),       intent(in)  :: wage
        real(dp),       intent(in)  :: hours1, hours2
        type(bcout_t),  intent(out) :: bcout

        character(len=*), intent(in), optional :: taxlevel
        character(len=*), intent(in), optional :: taxout(:)
        logical,      intent(in), optional :: correct
        logical,      intent(in), optional :: verbose

        !character(len(taxout))             :: ltaxout
        !character(len(taxlevel))           :: ltaxlevel
        character(len=32) :: ltaxout,ltaxlevel
        character(len=64) :: str
        type(fam_t)                :: fam0
        type(net_t), target        :: net
        real(dp)                   :: taxcomp0, taxcomp1
        real(dp)                   :: taxrate0, taxrate1
        real(dp)                   :: hrs,hrs0 !,hrsb !,hrs0b
        integer                    :: i !,step, stepb

        real(dp), parameter :: mtrtol  = 1.0e-5_dp
        real(dp), parameter :: distol  = 1.01_dp
        real(dp), parameter :: htol    = 0.00001_dp
        !maxstep is the main parameter that determines the number of evaluations,
        !if too large then may miss some discontinuities
        real(dp), parameter :: maxstep = 1.00_dp
        real(dp), parameter :: minstep = maxstep/10000.0_dp

        integer            :: kinkidx

        real(dp), dimension(maxkinks) :: kinks_hrs, kinks_earn, kinks_net, kinks_mtr
        logical,  dimension(maxkinks) :: kinks_dis
        integer                       :: kinks_num

        type real_pointer
            real(dp), pointer :: p => null()
        end type

        type(real_pointer), allocatable, dimension(:) :: taxpoint
        logical,            allocatable, dimension(:) :: taxadd

        !real(dp), pointer :: taxpoint

        logical           :: levelad, leveltu
        integer           :: taxad, taxsize
        real(dp)          :: temp

        real(dp) :: hrs_b, hrs_a, hrs_mid, temp_a, temp_b, dhrs, rate_a, rate_b

#       ifdef _TRACECOUNT_
            integer :: ev
            ev = 0
#       endif

        if ((.not. present(taxout)) .and. (.not. present(taxlevel))) then
            allocate(taxpoint(1))
            allocate(taxadd(1))
            taxadd = .true.
            taxsize = 1
            taxpoint(1)%p=>net%tu%dispinc
        else if (((.not. present(taxout)) .and. (present(taxlevel))) &
            .or. ((present(taxout)) .and. (.not. present(taxlevel)))) then
            call fortaxerror('if taxout or taxlevel is specified, both must be specified')
        else
            taxsize = size(taxout)
            allocate(taxpoint(taxsize))
            allocate(taxadd(taxsize))
            taxadd    = .true.
            ltaxlevel = lower(adjustl(taxlevel))
            if (adjustl(trim(ltaxlevel))=='tu') then
                leveltu = .true.
                levelad = .false.
            else if (adjustl(trim(ltaxlevel))=='ad1') then
                leveltu = .false.
                levelad = .true.
                taxad   = 1
            else if (adjustl(trim(ltaxlevel))=='ad2') then
                leveltu = .false.
                levelad = .true.
                taxad   = 2
            else
                call fortaxerror('taxlevel '//adjustl(trim(ltaxlevel))//' is unrecognized')
            end if

            do i = 1, taxsize

                ltaxout = lower(adjustl(taxout(i)))

                if (ltaxout(1:1)=='+') then
                    ltaxout = adjustl(ltaxout(2:))
                elseif (ltaxout(1:1)=='-') then
                    ltaxout = adjustl(ltaxout(2:))
                    taxadd(i) = .false.
                end if

#               undef _$header
#               undef _$footer
#               undef _$integer
#               undef _$double
#               undef _$logical
#               undef _$integerarray
#               undef _$doublearray
#               undef _$logicalarray

#               define _$header
#               define _$footer

#               define _$integer return
#               define _$logical return
#               define _$integerarray return
#               define _$logicalarray return

                if (levelad) then
#                   define _$double(x,lab,y) if ((ltaxout)==lower(#x)) taxpoint(i)%p=>net%ad(taxad)% x
#                   define _$doublearray(x,lab,y,z) if ((ltaxout)==lower(#x)) taxpoint(i)%p=>net%ad(taxad)% x
#                   include 'includes/netad_t.inc'
                else
#                   undef  _$double
#                   undef  _$doublearray
#                   define _$double(x,lab,y) if ((ltaxout)==lower(#x)) taxpoint(i)%p=>net%tu% x
#                   define _$doublearray(x,lab,y,z) if ((ltaxout)==lower(#x)) taxpoint(i)%p=>net%tu% x
#                   include 'includes/nettu_t.inc'
                end if

#               undef _$header
#               undef _$footer
#               undef _$integer
#               undef _$double
#               undef _$logical
#               undef _$integerarray
#               undef _$doublearray
#               undef _$logicalarray

                if (.not. associated(taxpoint(i)%p)) then
                    call fortaxerror(trim(ltaxout)//' does not exist')
                end if

            end do

        end if

        if (hours2-hours1<maxstep) return
        if (hours1<0.0_dp)         return
#       ifdef _ZEROWAGE_
            if (wage<0.0_dp) return
#       else
            if (wage<=0.0_dp) return
#       endif

        select case(ad)
            case(1)
            case(2)
                if (.not. fam%couple) return
            case default
                return
        end select

        !don't modify original structure
        fam0 = fam

        !calculate income at lower range
        fam0%ad(ad)%earn = wage*hours1
        fam0%ad(ad)%hrs  = hours1
        call calcNetInc(sys,fam0,net)

!        if (wage==0.0_dp) then
!            temp_a = 0.0_dp
!            do i=1,taxsize
!                temp_a = temp_a + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
!            end do
!            kinks_hrs(1)  = hours1
!            kinks_earn(1) = 0.0_dp
!            kinks_net(1)  = temp_a
!            kinks_dis(1)  = .false.
!        end if

#       ifdef _TRACECOUNT_
            ev = ev + 1
#       endif

        taxcomp0 = 0.0_dp
        do i=1,taxsize
            taxcomp0 = taxcomp0 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
        end do

        !step to get marginal rate
        hrs = hours1 + minstep
        fam0%ad(ad)%earn = wage*hrs
        fam0%ad(ad)%hrs  = hrs
        call calcNetInc(sys,fam0,net)

#       ifdef _TRACECOUNT_
            ev = ev + 1
#       endif

        taxcomp1 = 0.0_dp
        do i=1,taxsize
            taxcomp1 = taxcomp1 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
        end do

        taxrate1 = (taxcomp1-taxcomp0)/(wage*minstep)

        kinks_hrs(1)  = hours1
        kinks_earn(1) = wage*hours1
        kinks_net(1)  = taxcomp0
        kinks_mtr(1)  = taxrate1
        kinks_dis(1)  = .false.

#       ifdef _ZEROWAGE_
            if (wage==0.0_dp) then
                kinks_mtr(1) = 0.0_dp
            end if
#       endif

        kinkidx = 2

        taxrate0 = -999.0_dp

        hrs0 = hours1+minstep

        taxrate0 = taxrate1
        taxcomp0 = taxcomp1

loopmax : do

            if (kinkidx.ge.maxkinks) exit

            hrs = hrs + maxstep

            if (hrs>hours2) exit

            fam0%ad(ad)%earn = wage*hrs
            fam0%ad(ad)%hrs  = hrs

            call calcNetInc(sys,fam0,net)

#           ifdef _TRACECOUNT_
                ev = ev + 1
#           endif

#           ifdef _TRACE_
                print *, 'a', fam0%ad(ad)%hrs
#           endif

            taxcomp1 = 0.0_dp
            do i=1,taxsize
                taxcomp1 = taxcomp1 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
            end do

            taxrate1 = (taxcomp1-taxcomp0)/(wage*maxstep)

            !if zero wage, use level not slope
#           ifdef _ZEROWAGE_
                if (wage==0.0_dp) then
                    taxrate1 = 0.0_dp
                    do i=1,taxsize
                        taxrate1 = taxrate1 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                    end do
                end if
#           endif

            !if a mtr change detected
            if (abs(taxrate1-taxrate0)>mtrtol) then

                hrs_b  = hrs
                hrs_a  = hrs0
                rate_a = taxrate0
                temp_a = taxcomp0

                !use bisection to identify the change point more accurately
                do !while (abs(hrs_b-hrs_a)>0.00001_dp)

                    if (abs(hrs_b-hrs_a)>htol) then
                        !midpoint of domain
                        hrs_mid = 0.5_dp*(hrs_b+hrs_a)
                        dhrs    = 0.5_dp*(hrs_b-hrs_a)

                        fam0%ad(ad)%earn = wage*hrs_mid
                        fam0%ad(ad)%hrs  = hrs_mid
                        call calcNetInc(sys,fam0,net)

#                       ifdef _TRACECOUNT_
                            ev = ev + 1
#                       endif
#                       ifdef _TRACE_
                            print *, 'b', fam0%ad(ad)%hrs
#                       endif

                        temp_b = 0.0_dp
                        do i=1,taxsize
                            temp_b = temp_b + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                        end do
                        rate_b = (temp_b-temp_a)/(wage*dhrs)

                        !which direction to move?
                        if (abs(rate_b-rate_a)>mtrtol) then
                            hrs_b = hrs_mid
                        else
                            hrs_a = hrs_mid
                            temp_a = temp_b
                        end if
                    else

                        fam0%ad(ad)%earn = wage*hrs_a
                        fam0%ad(ad)%hrs  = hrs_a
                        call calcNetInc(sys,fam0,net)
#                       ifdef _TRACECOUNT_
                            ev = ev + 1
#                       endif
#                       ifdef _TRACE_
                            print *, 'c', fam0%ad(ad)%hrs
#                       endif
                        temp_a = 0.0_dp
                        do i=1,taxsize
                            temp_a = temp_a + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                        end do

                        fam0%ad(ad)%earn = wage*hrs_b
                        fam0%ad(ad)%hrs  = hrs_b
                        call calcNetInc(sys,fam0,net)
#                       ifdef _TRACECOUNT_
                            ev = ev + 1
#                       endif
#                       ifdef _TRACE_
                            print *, 'd', fam0%ad(ad)%hrs
#                       endif

                        temp_b = 0.0_dp
                        do i=1,taxsize
                            temp_b = temp_b + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                        end do

                        dhrs = hrs_b-hrs_a
                        taxrate1 = (temp_b-temp_a)/(wage*dhrs)

                        !is there a discontinuity?
                        if (abs(taxrate1)>distol) then
                            kinks_hrs(kinkidx)  = hrs_a
                            kinks_earn(kinkidx) = wage*hrs_a
                            kinks_net(kinkidx)  = temp_a
                            kinks_dis(kinkidx)  = .true.
                            if (taxcomp1>temp_b) then
                                kinks_mtr(kinkidx) = 9.999_dp
                            else
                                kinks_mtr(kinkidx) = -9.999_dp
                            end if
                            kinkidx = kinkidx + 1
                        end if
                        exit
                    end if

                end do

                !step to get marginal rate
                hrs = hrs_b + minstep
                fam0%ad(ad)%earn = wage*hrs
                fam0%ad(ad)%hrs  = hrs
                call calcNetInc(sys,fam0,net)

#               ifdef _TRACECOUNT_
                    ev = ev + 1
#               endif

                taxcomp1 = 0.0_dp
                do i=1,taxsize
                    taxcomp1 = taxcomp1 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                end do
#               ifdef _TRACE_
                    print *, 'e', fam0%ad(ad)%hrs
#               endif

                taxrate1 = (taxcomp1-temp_b)/(wage*minstep)

                kinks_hrs(kinkidx)  = hrs_b
                kinks_earn(kinkidx) = wage*hrs_b
                kinks_net(kinkidx)  = temp_b
                kinks_mtr(kinkidx)  = taxrate1
                kinks_dis(kinkidx)  = .false.

#               ifdef _ZEROWAGE_
                    if (wage==0.0_dp) then
                        kinks_mtr(kinkidx) = 0.0_dp
                    end if
#               endif

                kinkidx = kinkidx + 1

            end if

            taxrate0 = taxrate1
            taxcomp0 = taxcomp1
            hrs0     = hrs

        end do loopmax

        if (kinkidx<maxkinks) then
	        !end point
            fam0%ad(ad)%earn = wage*hours2
            fam0%ad(ad)%hrs  = hours2
            call CalcNetInc(sys,fam0,net)

#           ifdef _TRACECOUNT_
                ev = ev + 1
#           endif

            temp = 0.0_dp
            do i=1,taxsize
                temp = temp + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
            end do

            taxcomp1 = temp !taxpoint

            !kinkidx             = kinkidx + 1
            kinks_hrs(kinkidx)  = fam0%ad(ad)%hrs
            kinks_earn(kinkidx) = fam0%ad(ad)%earn
            kinks_net(kinkidx)  = taxcomp1 !b
            kinks_mtr(kinkidx)  = kinks_mtr(kinkidx-1)

        else
            if (present(verbose)) then
                if (verbose) call fortaxwarn('maxkinks is exceeded')
            end if
        end if

        kinks_num = min(kinkidx,maxkinks)

        !rounding correction to ensure consistency
        if (present(correct)) then
            if (correct) then
                do i = 1, kinks_num
                    !if (.not. kinks_dis(i)) then
                        !round to 5 decimal places
                        !using nint could cause overflow problems
!                        kinks_mtr(i)  = nint(kinks_mtr(i)*100000.0_dp)/100000.0_dp
!                        kinks_earn(i) = nint(kinks_earn(i)*1000.0_dp)/1000.0_dp
                        write(str,'(F18.5)') kinks_mtr(i)
                        read(str,*) kinks_mtr(i)
                        write(str,'(F18.5)') kinks_earn(i)
                        read(str,*) kinks_earn(i)
                    !end if
                end do

                kinks_earn(1) = nint(kinks_earn(1)*1000.0_dp)/1000.0_dp

                do i = 2, kinks_num
                    if (kinks_dis(i-1)) then
!                        kinks_net(i) = nint(kinks_net(i)*1000.0_dp)/1000.0_dp
                        write(str,'(F18.5)') kinks_net(i)
                        read(str,*) kinks_net(i)
                    else
                        kinks_net(i) = kinks_net(i-1) + kinks_mtr(i-1)*(kinks_earn(i)-kinks_earn(i-1))
                    end if
                end do

            end if
        end if

        bcout%kinks_num  = kinks_num
        bcout%kinks_hrs  = kinks_hrs
        bcout%kinks_earn = kinks_earn
        bcout%kinks_net  = kinks_net
        bcout%kinks_mtr  = kinks_mtr

        if (present(verbose)) then
            if (verbose) then
                do i = 1, kinks_num
                    write(*,'(F12.3,2X,F12.3,2X,F12.3,F12.5)') kinks_hrs(i),kinks_earn(i), kinks_net(i), kinks_mtr(i)
                end do
            end if
        end if

        do i=1,taxsize
            nullify(taxpoint(i)%p)
        end do

        !nullify(taxpoint)
#       ifdef _TRACECOUNT_
            print *, 'function calls:', ev
#       endif

    end subroutine kinkshours


    ! kinksearn
    ! -----------------------------------------------------------------------
    ! calculates a piecewise linear schedule under the a given tax system
    ! for a family by varying earnings of with fixed weekly hours of work.
    ! can be performed for any income component (or linear combination of)

    subroutine kinksearn3d(sys,fam,hours1,hours2,earn1,earn2,bcout,taxlevel,taxout,verbose)

        use fortax_type, only : fam_t, sys_t, net_t
        use fortax_util, only : lower, inttostr, fortaxerror, fortaxwarn
        use fortax_calc, only : calcnetinc

        implicit none

        type(sys_t),     intent(in)  :: sys
        type(fam_t),     intent(in)  :: fam
        real(dp),        intent(in)  :: hours1, hours2     ! fix hours of adult 1 and adult 2
        real(dp),        intent(in)  :: earn1(:), earn2(:) ! earnings grid for adult 1 and adult2
        type(bcout3d_t), intent(out) :: bcout

        character(len=*), intent(in), optional :: taxlevel
        character(len=*), intent(in), optional :: taxout(:)
        logical,      intent(in), optional :: verbose

        character(len=32)              :: ltaxout, ltaxlevel
        character(len=64) :: str

        integer :: nearn1, nearn2

        type(fam_t)                :: fam0
        type(net_t), target        :: net
        real(dp)                   :: taxcomp0, taxcomp1
        real(dp)                   :: taxrate0, taxrate1
        real(dp)                   :: earn, earn0 !,ern0,ernb !,ern0b
        integer                    :: i !,step, stepb, i

!        real(dp), parameter :: mtrtol  = 1.0e-8_dp
!        real(dp), parameter :: distol  = 1.01_dp
!
!        real(dp), parameter :: maxstep = 0.10_dp
!        real(dp), parameter :: minstep = maxstep/2000.0_dp


        !integer, parameter :: maxkinks = 200
        integer            :: kinkidx

        real(dp), dimension(maxkinks) :: kinks_hrs, kinks_earn, kinks_net, kinks_mtr
        logical, dimension(maxkinks)  :: kinks_dis
        integer                       :: kinks_num

        type real_pointer
            real(dp), pointer :: p => null()
        end type

        type(real_pointer), allocatable, dimension(:) :: taxpoint
        logical,            allocatable, dimension(:) :: taxadd
        !real(dp), pointer :: taxpoint

        !real(dp), pointer :: taxpoint => null()
        logical           :: levelad, leveltu
        integer           :: taxad, taxsize
        real(dp)          :: temp

        real(dp) :: earn_b, earn_a, earn_mid, temp_a, temp_b, dearn, rate_a, rate_b

        nearn1 = size(earn1)
        nearn2 = size(earn2)

        ! some checks
        if ( nearn1<2 ) then
            call fortaxerror('kinksearn3d: earn1 must have length of at least 2')
        end if

        if ( nearn2<2 ) then
            call fortaxerror('kinksearn3d: earn2 must have length of at least 2')
        end if

        if (.not. fam%couple) then
            call fortaxerror('kinksearn3d: must have two adults in family')
        end if

        if (hours1<0.0_dp) then
            call fortaxerror('kinksearn3d: hours1 must be non-negative')
        end if

        if (hours2<0.0_dp) then
            call fortaxerror('kinksearn3d: hours2 must be non-negative')
        end if

        if (earn1(1)<0.0_dp) then
            call fortaxerror('kinksearn3d: earn1 must always be non-negative')
        end if

        if (earn2(1)<0.0_dp) then
            call fortaxerror('kinksearn3d: earn2 must always be non-negative')
        end if

        if ( .not. isStrictlyIncreasing(earn1,nearn1) ) then
            call fortaxerror('kinksearn3d: earn1 must be strictly increasing')
        end if

        if ( .not. isStrictlyIncreasing(earn2,nearn2) ) then
            call fortaxerror('kinksearn3d: earn2 must be strictly increasing')
        end if

        if ((.not. present(taxout)) .and. (.not. present(taxlevel))) then
            taxsize = 1
            allocate(taxpoint(1))
            allocate(taxadd(1))
            taxadd = .true.
            taxpoint(1)%p=>net%tu%dispinc
        else if (((.not. present(taxout)) .and. (present(taxlevel))) &
            .or. ((present(taxout)) .and. (.not. present(taxlevel)))) then
            call fortaxerror('if taxout or taxlevel is specified, both must be specified')
        else
            taxsize = size(taxout)
            allocate(taxpoint(taxsize))
            allocate(taxadd(taxsize))
            taxadd    = .true.
            ltaxlevel = lower(taxlevel)
            if (adjustl(trim(ltaxlevel))=='tu') then
                leveltu = .true.
                levelad = .false.
            else if (adjustl(trim(ltaxlevel))=='ad1') then
                leveltu = .false.
                levelad = .true.
                taxad   = 1
            else if (adjustl(trim(ltaxlevel))=='ad2') then
                leveltu = .false.
                levelad = .true.
                taxad   = 2
            else
                call fortaxerror('taxlevel '//adjustl(trim(ltaxlevel))//' is unrecognized')
            end if

            do i = 1, taxsize

                ltaxout = lower(adjustl(taxout(i)))

                if (ltaxout(1:1)=='+') then
                    ltaxout = adjustl(ltaxout(2:))
                elseif (ltaxout(1:1)=='-') then
                    ltaxout = adjustl(ltaxout(2:))
                    taxadd(i) = .false.
                end if

#               undef _$header
#               undef _$footer
#               undef _$integer
#               undef _$double
#               undef _$logical
#               undef _$integerarray
#               undef _$doublearray
#               undef _$logicalarray

#               define _$header
#               define _$footer

#               define _$integer return
#               define _$logical return
#               define _$integerarray return
#               define _$logicalarray return

                if (levelad) then
#                   define _$double(x,lab,y) if ((ltaxout)==lower(#x)) taxpoint(i)%p=>net%ad(taxad)% x
#                   define _$doublearray(x,lab,y,z) if ((ltaxout)==lower(#x)) taxpoint(i)%p=>net%ad(taxad)% x
#                   include 'includes/netad_t.inc'
                else
#                   undef  _$double
#                   undef  _$doublearray
#                   define _$double(x,lab,y) if ((ltaxout)==lower(#x)) taxpoint(i)%p=>net%tu% x
#                   define _$doublearray(x,lab,y,z) if ((ltaxout)==lower(#x)) taxpoint(i)%p=>net%tu% x
#                   include 'includes/nettu_t.inc'
                end if

#               undef _$header
#               undef _$footer
#               undef _$integer
#               undef _$double
#               undef _$logical
#               undef _$integerarray
#               undef _$doublearray
#               undef _$logicalarray

                if (.not. associated(taxpoint(i)%p)) then
                    call fortaxerror(trim(ltaxout)//' does not exist')
                end if

            end do

        end if


        !don't modify original structure
        fam0 = fam

        !set hours at all points
        kinks_hrs1     = hours1
        kinks_hrs2     = hours2
        fam0%ad(1)%hrs = hours1
        fam0%ad(2)%hrs = hours2

        taxrate1 = -999.9_dp
        taxrate2 = -999.9_dp

        do iX2 = 1, nearn2
            do iX1 = 1, nearn1

                !calculate income at lower range
                fam0%ad(1)%earn = earn1(iX1)
                fam0%ad(2)%earn = earn2(iX2)

                call calcNetInc(sys,fam0,net)

                taxcomp0 = 0.0_dp
                do i=1,taxsize
                    taxcomp0 = taxcomp0 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                end do

                if ( domtr ) then
                
                    ! step to get marginal rate (adult 1)
                    fam0%ad(1)%earn = earn1(iX1) + minstep
                    !fam0%ad(ad)%hrs  = hrs
                    call calcNetInc(sys,fam0,net)

                    taxcomp1 = 0.0_dp
                    do i=1,taxsize
                        taxcomp1 = taxcomp1 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                    end do

                    ! step to get marginal rate (adult 2)
                    fam0%ad(1)%earn = earn1(iX1) + minstep
                    fam0%ad(2)%earn = earn2(iX2) + minstep
                    !fam0%ad(ad)%hrs  = hrs
                    call calcNetInc(sys,fam0,net)

                    taxcomp2 = 0.0_dp
                    do i=1,taxsize
                        taxcomp2 = taxcomp2 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                    end do

                    taxrate1 = (taxcomp1-taxcomp0)/minstep
                    taxrate2 = (taxcomp2-taxcomp0)/minstep

                    ! check for discontinuity
                    ! ...

                end if

                kinks_net(iX1,iX2) = taxcomp0
                kinks_mtr1(iX1,iX2) = taxrate1
                kinks_mtr2(iX1,iX2) = taxrate2
                kinks_dis1(iX1,iX2)  = .false.
                kinks_dis2(iX1,iX2)  = .false.

            end do !iX1
        end do !iX2

        bcout%kinks_hrs1  = kinks_hrs1
        bcout%kinks_hrs2  = kinks_hrs2
        bcout%kinks_earn1 = kinks_earn1
        bcout%kinks_earn2 = kinks_earn2
        bcout%kinks_net   = kinks_net
        bcout%kinks_mtr1  = kinks_mtr1
        bcout%kinks_mtr2  = kinks_mtr2

        if (present(verbose)) then
            if (verbose) then
                do i = 1, kinks_num
                    write(*,'(F12.3,2X,F12.3,2X,F12.3,F12.5)') kinks_hrs(i),kinks_earn(i), kinks_net(i), kinks_mtr(i)
                end do
            end if
        end if

        do i=1,taxsize
            nullify(taxpoint(i)%p)
        end do

    end subroutine kinksearn3d

    pure function isStrictlyIncreasing(x,n) result(y)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(in) :: x(:)
        logical :: y
        integer :: ix
        y = .true.
        do ix = 2, n
            if ( x(ix)<=x(ix-1) ) then
                y = .false.
                exit
            end if
        end do
    end function isStrictlyIncreasing

    ! kinksccexp
    ! -----------------------------------------------------------------------
    ! calculates a piecewise linear schedule under the a given tax system
    ! for a family by varying childcare expenditure of with fixed weekly
    ! hours of work and earnings. can be performed for any income component
    ! (or linear combination of)

    subroutine kinksccexp(sys,fam,ad,hours,earn,ccexp1,ccexp2,bcout,taxlevel,taxout,correct,verbose)

        use fortax_type, only : fam_t, sys_t, net_t
        use fortax_util, only : lower, inttostr, fortaxerror, fortaxwarn
        use fortax_calc, only : calcnetinc

        implicit none

        type(sys_t),    intent(in)  :: sys
        type(fam_t),    intent(in)  :: fam
        integer,        intent(in)  :: ad
        real(dp),       intent(in)  :: hours
        real(dp),       intent(in)  :: earn
        real(dp),       intent(in)  :: ccexp1, ccexp2
        type(bcout_t),  intent(out) :: bcout

        character(len=*), intent(in), optional :: taxlevel
        character(len=*), intent(in), optional :: taxout(:)
        logical,      intent(in), optional :: correct
        logical,      intent(in), optional :: verbose

        !character(len(taxout))             :: ltaxout
        !character(len(taxlevel))           :: ltaxlevel
        character(len=32)              :: ltaxout, ltaxlevel
        type(fam_t)                :: fam0
        type(net_t), target        :: net
        real(dp)                   :: taxcomp0, taxcomp1
        real(dp)                   :: taxrate0, taxrate1
        real(dp)                   :: ccexp, ccexp0 !,ern0,ernb !,ern0b
        integer                    :: i !,step, stepb, i

!        real(dp), parameter :: mtrtol  = 1.0e-8_dp
!        real(dp), parameter :: distol  = 1.01_dp
!
!        real(dp), parameter :: maxstep = 0.10_dp
!        real(dp), parameter :: minstep = maxstep/2000.0_dp

        real(dp), parameter :: mtrtol  = 1.0e-5_dp
        real(dp), parameter :: distol  = 1.50_dp

        real(dp), parameter :: etol    = 0.00001_dp
        !maxstep is the main parameter that determines the number of evaluations,
        !if too large then may miss some discontinuities
        real(dp), parameter :: maxstep = 5.00_dp
        real(dp), parameter :: minstep = maxstep/10000.0_dp

        !integer, parameter :: maxkinks = 200
        integer            :: kinkidx

        real(dp), dimension(maxkinks) :: kinks_hrs, kinks_ccexp, kinks_net, kinks_mtr
        logical, dimension(maxkinks)  :: kinks_dis
        integer                       :: kinks_num

        type real_pointer
            real(dp), pointer :: p => null()
        end type

        type(real_pointer), allocatable, dimension(:) :: taxpoint
        logical,            allocatable, dimension(:) :: taxadd
        !real(dp), pointer :: taxpoint

        !real(dp), pointer :: taxpoint => null()
        logical           :: levelad, leveltu
        integer           :: taxad, taxsize
        real(dp)          :: temp

        real(dp) :: ccexp_b, ccexp_a, ccexp_mid, temp_a, temp_b, dccexp, rate_a, rate_b

#       ifdef _TRACECOUNT_
            integer :: ev
            ev = 0
#       endif

        if ((.not. present(taxout)) .and. (.not. present(taxlevel))) then
            taxsize = 1
            allocate(taxpoint(1))
            allocate(taxadd(1))
            taxadd = .true.
            taxpoint(1)%p=>net%tu%dispinc
        else if (((.not. present(taxout)) .and. (present(taxlevel))) &
            .or. ((present(taxout)) .and. (.not. present(taxlevel)))) then
            call fortaxerror('if taxout or taxlevel is specified, both must be specified')
        else
            taxsize = size(taxout)
            allocate(taxpoint(taxsize))
            allocate(taxadd(taxsize))
            taxadd    = .true.
            ltaxlevel = lower(taxlevel)
            if (adjustl(trim(ltaxlevel))=='tu') then
                leveltu = .true.
                levelad = .false.
            else if (adjustl(trim(ltaxlevel))=='ad1') then
                leveltu = .false.
                levelad = .true.
                taxad   = 1
            else if (adjustl(trim(ltaxlevel))=='ad2') then
                leveltu = .false.
                levelad = .true.
                taxad   = 2
            else
                call fortaxerror('taxlevel '//adjustl(trim(ltaxlevel))//' is unrecognized')
            end if

            do i = 1, taxsize

                ltaxout = lower(adjustl(taxout(i)))

                if (ltaxout(1:1)=='+') then
                    ltaxout = adjustl(ltaxout(2:))
                elseif (ltaxout(1:1)=='-') then
                    ltaxout = adjustl(ltaxout(2:))
                    taxadd(i) = .false.
                end if

#               undef _$header
#               undef _$footer
#               undef _$integer
#               undef _$double
#               undef _$logical
#               undef _$integerarray
#               undef _$doublearray
#               undef _$logicalarray

#               define _$header
#               define _$footer

#               define _$integer return
#               define _$logical return
#               define _$integerarray return
#               define _$logicalarray return

                if (levelad) then
#                   define _$double(x,lab,y) if ((ltaxout)==lower(#x)) taxpoint(i)%p=>net%ad(taxad)% x

#                   define _$doublearray(x,lab,y,z) if ((ltaxout)==lower(#x)) taxpoint(i)%p=>net%ad(taxad)% x
#                   include 'includes/netad_t.inc'
                else
#                   undef  _$double
#                   undef  _$doublearray
#                   define _$double(x,lab,y) if ((ltaxout)==lower(#x)) taxpoint(i)%p=>net%tu% x
#                   define _$doublearray(x,lab,y,z) if ((ltaxout)==lower(#x)) taxpoint(i)%p=>net%tu% x
#                   include 'includes/nettu_t.inc'
                end if

#               undef _$header
#               undef _$footer
#               undef _$integer
#               undef _$double
#               undef _$logical
#               undef _$integerarray
#               undef _$doublearray
#               undef _$logicalarray

                if (.not. associated(taxpoint(i)%p)) then
                    call fortaxerror(trim(ltaxout)//' does not exist')
                end if

            end do

        end if

        !if (.not. associated(taxpoint)) return !NOT-SAFE
        if (ccexp2-ccexp1<maxstep)      return
        if (ccexp1<0.0_dp)              return
        if (earn<0.0_dp)                return
        if (hours<0.0_dp)               return

        select case(ad)
            case(1)
            case(2)
                if (.not. fam%couple) return
            case default
                return
        end select

        !don't modify original structure
        fam0 = fam

        !set hours at all points
        kinks_hrs        = hours
        fam0%ad(ad)%hrs  = hours
        !kinks_earn       = earn
        fam0%ad(ad)%earn = earn

        !calculate income at lower range
        fam0%ccexp = ccexp1

        call calcNetInc(sys,fam0,net)
#       ifdef _TRACECOUNT_
            ev = ev + 1
#       endif

        taxcomp0 = 0.0_dp
        do i=1,taxsize
            taxcomp0 = taxcomp0 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
        end do

        !step to get marginal rate
        ccexp = ccexp1 + minstep
        fam0%ccexp = ccexp
        !fam0%ad(ad)%hrs  = hrs
        call calcNetInc(sys,fam0,net)
#       ifdef _TRACECOUNT_
            ev = ev + 1
#       endif

        taxcomp1 = 0.0_dp
        do i=1,taxsize
            taxcomp1 = taxcomp1 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
        end do

        taxrate1 = (taxcomp1-taxcomp0)/minstep

        !kinks_earn(1) = ccexp1
        kinks_ccexp(1) = ccexp1
        kinks_net(1)  = taxcomp0
        kinks_mtr(1)  = taxrate1
        kinks_dis(1)  = .false.

        kinkidx = 2

        ccexp0 = ccexp1+minstep

        taxrate0 = taxrate1
        taxcomp0 = taxcomp1

loopmax : do

            if (kinkidx.ge.maxkinks) exit

            ccexp = ccexp + maxstep

            if (ccexp>ccexp2) exit

            fam0%ccexp = ccexp
            !fam0%ad(ad)%hrs  = hrs

            call calcNetInc(sys,fam0,net)
#           ifdef _TRACECOUNT_
                ev = ev + 1
#           endif
#           ifdef _TRACE_
                print *, 'a', fam0%ad(ad)%earn
#           endif

            taxcomp1 = 0.0_dp
            do i=1,taxsize
                taxcomp1 = taxcomp1 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
            end do

            taxrate1 = (taxcomp1-taxcomp0)/maxstep

            !if a mtr change detected
            if (abs(taxrate1-taxrate0)>mtrtol) then

                ccexp_b  = ccexp
                ccexp_a  = ccexp0
                rate_a  = taxrate0
                temp_a  = taxcomp0

                !use bisection to identify the change point more accurately
                do !while (abs(hrs_b-hrs_a)>0.00001_dp)

                    if (abs(ccexp_b-ccexp_a)>etol) then
                        !midpoint of domain
                        ccexp_mid = 0.5_dp*(ccexp_b+ccexp_a)
                        dccexp    = 0.5_dp*(ccexp_b-ccexp_a)

                        fam0%ccexp = ccexp_mid

                        call calcNetInc(sys,fam0,net)
#                       ifdef _TRACECOUNT_
                            ev = ev + 1
#                       endif
#                       ifdef _TRACE_
                            print *, 'b', fam0%ccexp
#                       endif

                        temp_b = 0.0_dp
                        do i=1,taxsize
                            temp_b = temp_b + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                        end do
                        rate_b = (temp_b-temp_a)/dccexp

                        !which direction to move?
                        if (abs(rate_b-rate_a)>mtrtol) then
                            ccexp_b = ccexp_mid
                        else
                            ccexp_a = ccexp_mid
                            temp_a = temp_b
                        end if
                    else

                        fam0%ccexp = ccexp_a

                        call calcNetInc(sys,fam0,net)
#                       ifdef _TRACECOUNT_
                            ev = ev + 1
#                       endif
#                       ifdef _TRACE_
                            print *, 'c', fam0%ccexp
#                       endif
                        temp_a = 0.0_dp
                        do i=1,taxsize
                            temp_a = temp_a + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                        end do

                        fam0%ccexp = ccexp_b
                        call calcNetInc(sys,fam0,net)
#                       ifdef _TRACECOUNT_
                            ev = ev + 1
#                       endif
#                       ifdef _TRACE_
                            print *, 'd', fam0%ccexp
#                       endif

                        temp_b = 0.0_dp
                        do i=1,taxsize
                            temp_b = temp_b + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                        end do

                        dccexp = ccexp_b-ccexp_a
                        taxrate1 = (temp_b-temp_a)/dccexp

                        !is there a discontinuity?
                        if (abs(taxrate1)>distol) then
                            kinks_ccexp(kinkidx) = ccexp_a
                            kinks_net(kinkidx)  = temp_a
                            kinks_dis(kinkidx)  = .true.
                            if (taxcomp1>temp_b) then
                                kinks_mtr(kinkidx) = 9.999_dp
                            else
                                kinks_mtr(kinkidx) = -9.999_dp
                            end if
                            kinkidx = kinkidx + 1
                        end if
                        exit
                    end if

                end do

                !step to get marginal rate
                ccexp = ccexp_b + minstep
                fam0%ccexp = ccexp
                call calcNetInc(sys,fam0,net)
#               ifdef _TRACECOUNT_
                    ev = ev + 1
#               endif

#               ifdef _TRACE_
                    print *, 'e', fam0%ccexp
#               endif

                taxcomp1 = 0.0_dp
                do i=1,taxsize
                    taxcomp1 = taxcomp1 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                end do

                taxrate1 = (taxcomp1-temp_b)/minstep

                kinks_ccexp(kinkidx) = ccexp_b
                kinks_net(kinkidx)  = temp_b
                kinks_mtr(kinkidx)  = taxrate1
                kinks_dis(kinkidx)  = .false.

                kinkidx = kinkidx + 1


            end if

            taxrate0 = taxrate1
            taxcomp0 = taxcomp1
            ccexp0    = ccexp

        end do loopmax

        if (kinkidx<maxkinks) then
	        !end point
            fam0%ccexp = ccexp2
            call CalcNetInc(sys,fam0,net)
#           ifdef _TRACECOUNT_
                ev = ev + 1
#           endif

            temp = 0.0_dp
            do i=1,taxsize
                temp = temp + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
            end do

            taxcomp1 = temp !taxpoint

            kinks_ccexp(kinkidx) = fam0%ccexp
            kinks_net(kinkidx)  = taxcomp1
            kinks_mtr(kinkidx)  = kinks_mtr(kinkidx-1)
        else
            if (present(verbose)) then
                if (verbose) call fortaxwarn('maxkinks is exceeded')
            end if
        end if

        kinks_num = min(kinkidx,maxkinks)

        !rounding correction to ensure consistency
        if (present(correct)) then
            if (correct) then
                do i = 1, kinks_num
                    !if (.not. kinks_dis(i)) then
                        !round to 5 decimal places
                        kinks_mtr(i)  = nint(kinks_mtr(i)*100000.0_dp)/100000.0_dp
                        kinks_ccexp(i) = nint(kinks_ccexp(i)*1000.0_dp)/1000.0_dp
                    !end if
                end do

                kinks_ccexp(1) = nint(kinks_ccexp(1)*1000.0_dp)/1000.0_dp

                do i = 2, kinks_num
                    if (kinks_dis(i-1)) then
                        kinks_net(i) = nint(kinks_net(i)*1000.0_dp)/1000.0_dp
                    else
                        kinks_net(i) = kinks_net(i-1) + kinks_mtr(i-1)*(kinks_ccexp(i)-kinks_ccexp(i-1))
                    end if
                end do

            end if
        end if

        bcout%kinks_num  = kinks_num
        bcout%kinks_hrs  = kinks_hrs
        bcout%kinks_earn = kinks_ccexp
        bcout%kinks_net  = kinks_net
        bcout%kinks_mtr  = kinks_mtr

        if (present(verbose)) then
            if (verbose) then
                do i = 1, kinks_num
                    write(*,'(F12.3,2X,F12.3,2X,F12.3,F12.5)') kinks_hrs(i),kinks_ccexp(i), kinks_net(i), kinks_mtr(i)
                end do
            end if
        end if

        do i=1,taxsize
            nullify(taxpoint(i)%p)
        end do

        !nullify(taxpoint)
#       ifdef _TRACECOUNT_
            print *, 'function calls:', ev
#       endif


    end subroutine kinksccexp

end module fortax_kinks
