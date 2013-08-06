
! This file is part of the FORTAX library;
! (c) 2009 Andrew Shephard; andrubuntu@gmail.com

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




! fortax_kinks
! -----------------------------------------------------------------------
! module calculates piecewise linear income schedules, AS

module fortax_kinks

    use fortax_realtype, only : dp

    implicit none

#   ifndef _maxkinks_
    integer, parameter :: maxkinks = 200
#   else
    integer, parameter :: maxkinks = _maxkinks_
#   endif /* _maxkinks_ */
#   undef _maxkinks_

    type :: bcout_t
        integer :: kinks_num
        real(dp), dimension(maxkinks) :: kinks_hrs, kinks_earn, kinks_net, kinks_mtr
    end type bcout_t

!#   define _TRACE_
!#   define _TRACECOUNT_

contains

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

        character(*), intent(in), optional :: taxlevel
        character(*), intent(in), optional :: taxout(:)
        logical,      intent(in), optional :: correct
        logical,      intent(in), optional :: verbose

        !character(len(taxout))             :: ltaxout
        !character(len(taxlevel))           :: ltaxlevel
        character(32) :: ltaxout,ltaxlevel

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
        if (wage<=0.0_dp)          return

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

        kinks_num = kinkidx

        !rounding correction to ensure consistency
        if (present(correct)) then
            if (correct) then
                do i = 1, kinks_num
                    !if (.not. kinks_dis(i)) then
                        !round to 5 decimal places
                        kinks_mtr(i)  = nint(kinks_mtr(i)*100000.0_dp)/100000.0_dp
                        kinks_earn(i) = nint(kinks_earn(i)*1000.0_dp)/1000.0_dp
                    !end if
                end do
                
                kinks_earn(1) = nint(kinks_earn(1)*1000.0_dp)/1000.0_dp

                do i = 2, kinks_num
                    if (kinks_dis(i-1)) then
                        kinks_net(i) = nint(kinks_net(i)*1000.0_dp)/1000.0_dp
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

    subroutine kinksearn(sys,fam,ad,hours,earn1,earn2,bcout,taxlevel,taxout,correct,verbose)

        use fortax_type, only : fam_t, sys_t, net_t
        use fortax_util, only : lower, inttostr, fortaxerror, fortaxwarn
        use fortax_calc, only : calcnetinc

        implicit none

        type(sys_t),    intent(in)  :: sys
        type(fam_t),    intent(in)  :: fam
        integer,        intent(in)  :: ad
        real(dp),       intent(in)  :: hours
        real(dp),       intent(in)  :: earn1, earn2
        type(bcout_t),  intent(out) :: bcout

        character(*), intent(in), optional :: taxlevel
        character(*), intent(in), optional :: taxout(:)
        logical,      intent(in), optional :: correct
        logical,      intent(in), optional :: verbose

        !character(len(taxout))             :: ltaxout
        !character(len(taxlevel))           :: ltaxlevel
        character(32)              :: ltaxout, ltaxlevel
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

        real(dp), parameter :: mtrtol  = 1.0e-5_dp
        real(dp), parameter :: distol  = 1.01_dp
        real(dp), parameter :: etol    = 0.00001_dp
        !maxstep is the main parameter that determines the number of evaluations, 
        !if too large then may miss some discontinuities
        real(dp), parameter :: maxstep = 5.00_dp
        real(dp), parameter :: minstep = maxstep/10000.0_dp
        
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
        if (earn2-earn1<maxstep)        return
        if (earn1<0.0_dp)               return
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
        kinks_hrs       = hours
        fam0%ad(ad)%hrs = hours
        
        !calculate income at lower range
        fam0%ad(ad)%earn = earn1

        call calcNetInc(sys,fam0,net)
#       ifdef _TRACECOUNT_              
            ev = ev + 1
#       endif            
                
        taxcomp0 = 0.0_dp
        do i=1,taxsize
            taxcomp0 = taxcomp0 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
        end do
        
        !step to get marginal rate
        earn = earn1 + minstep
        fam0%ad(ad)%earn = earn
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

        kinks_earn(1) = earn1
        kinks_net(1)  = taxcomp0
        kinks_mtr(1)  = taxrate1
        kinks_dis(1)  = .false.
                            
        kinkidx = 2
        
        earn0 = earn1+minstep
        
        taxrate0 = taxrate1
        taxcomp0 = taxcomp1
        
loopmax : do                
           
            if (kinkidx.ge.maxkinks) exit

            earn = earn + maxstep
            
            if (earn>earn2) exit
            
            fam0%ad(ad)%earn = earn
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

                earn_b  = earn
                earn_a  = earn0
                rate_a  = taxrate0
                temp_a  = taxcomp0
                 
                !use bisection to identify the change point more accurately
                do !while (abs(hrs_b-hrs_a)>0.00001_dp)
                
                    if (abs(earn_b-earn_a)>etol) then
                        !midpoint of domain
                        earn_mid = 0.5_dp*(earn_b+earn_a)
                        dearn    = 0.5_dp*(earn_b-earn_a)
                        
                        fam0%ad(ad)%earn = earn_mid
                        
                        call calcNetInc(sys,fam0,net)
#                       ifdef _TRACECOUNT_              
                            ev = ev + 1
#                       endif
#                       ifdef _TRACE_
                            print *, 'b', fam0%ad(ad)%earn
#                       endif

                        temp_b = 0.0_dp
                        do i=1,taxsize
                            temp_b = temp_b + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                        end do
                        rate_b = (temp_b-temp_a)/dearn
                        
                        !which direction to move?
                        if (abs(rate_b-rate_a)>mtrtol) then
                            earn_b = earn_mid
                        else
                            earn_a = earn_mid
                            temp_a = temp_b
                        end if
                    else
                    
                        fam0%ad(ad)%earn = earn_a
                        
                        call calcNetInc(sys,fam0,net)
#                       ifdef _TRACECOUNT_              
                            ev = ev + 1
#                       endif
#                       ifdef _TRACE_
                            print *, 'c', fam0%ad(ad)%earn
#                       endif
                        temp_a = 0.0_dp
                        do i=1,taxsize
                            temp_a = temp_a + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                        end do  
                        
                        fam0%ad(ad)%earn = earn_b
                        call calcNetInc(sys,fam0,net)
#                       ifdef _TRACECOUNT_              
                            ev = ev + 1
#                       endif                            
#                       ifdef _TRACE_
                            print *, 'd', fam0%ad(ad)%earn
#                       endif
                        
                        temp_b = 0.0_dp
                        do i=1,taxsize
                            temp_b = temp_b + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                        end do
                        
                        dearn = earn_b-earn_a
                        taxrate1 = (temp_b-temp_a)/dearn
                        
                        !is there a discontinuity?
                        if (abs(taxrate1)>distol) then
                            kinks_earn(kinkidx) = earn_a
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
                earn = earn_b + minstep
                fam0%ad(ad)%earn = earn
                call calcNetInc(sys,fam0,net)
#               ifdef _TRACECOUNT_              
                    ev = ev + 1
#               endif                    

#               ifdef _TRACE_
                    print *, 'e', fam0%ad(ad)%earn
#               endif        
        
                taxcomp1 = 0.0_dp
                do i=1,taxsize
                    taxcomp1 = taxcomp1 + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
                end do

                taxrate1 = (taxcomp1-temp_b)/minstep
        
                kinks_earn(kinkidx) = earn_b
                kinks_net(kinkidx)  = temp_b
                kinks_mtr(kinkidx)  = taxrate1
                kinks_dis(kinkidx)  = .false.
                    
                kinkidx = kinkidx + 1
                              
                
            end if
            
            taxrate0 = taxrate1
            taxcomp0 = taxcomp1
            earn0    = earn
                                    
        end do loopmax

        if (kinkidx<maxkinks) then        
	        !end point
            fam0%ad(ad)%earn = earn2
            call CalcNetInc(sys,fam0,net)
#           ifdef _TRACECOUNT_              
                ev = ev + 1
#           endif

            temp = 0.0_dp
            do i=1,taxsize
                temp = temp + merge(taxpoint(i)%p,-taxpoint(i)%p,taxadd(i))
            end do

            taxcomp1 = temp !taxpoint

            kinks_earn(kinkidx) = fam0%ad(ad)%earn
            kinks_net(kinkidx)  = taxcomp1
            kinks_mtr(kinkidx)  = kinks_mtr(kinkidx-1)
        else
            if (present(verbose)) then
                if (verbose) call fortaxwarn('maxkinks is exceeded')
            end if
        end if

        kinks_num = kinkidx

        !rounding correction to ensure consistency
        if (present(correct)) then
            if (correct) then
                do i = 1, kinks_num
                    !if (.not. kinks_dis(i)) then
                        !round to 5 decimal places
                        kinks_mtr(i)  = nint(kinks_mtr(i)*100000.0_dp)/100000.0_dp
                        kinks_earn(i) = nint(kinks_earn(i)*1000.0_dp)/1000.0_dp
                    !end if
                end do
                
                kinks_earn(1) = nint(kinks_earn(1)*1000.0_dp)/1000.0_dp

                do i = 2, kinks_num
                    if (kinks_dis(i-1)) then
                        kinks_net(i) = nint(kinks_net(i)*1000.0_dp)/1000.0_dp
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

    end subroutine kinksearn
    
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

        character(*), intent(in), optional :: taxlevel
        character(*), intent(in), optional :: taxout(:)
        logical,      intent(in), optional :: correct
        logical,      intent(in), optional :: verbose

        !character(len(taxout))             :: ltaxout
        !character(len(taxlevel))           :: ltaxlevel
        character(32)              :: ltaxout, ltaxlevel
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

        kinks_num = kinkidx

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
