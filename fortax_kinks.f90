
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




! fortax_kinks
! -----------------------------------------------------------------------
! module calculates piecewise linear income schedules, AS


module fortax_kinks

    use fortax_realtype, only : dp
    use fortax_type, only : maxkinks, bcout_t
    implicit none
    private :: dp
    private

    public :: evalKinksHours, evalKinksEarn, kinkshours, kinksearn, kinksccexp, kinks_desc

    logical, parameter, private :: zeroWage = .true.

contains

    ! evalKinksHours
    ! -----------------------------------------------------------------------
    ! uses the piecewise linear budget constraint bcout as calculated in
    ! kinkshours to evaluate the respective income measure at hours

    pure subroutine evalKinksHours(bcout, hours, earn, net, mtr, iin, iout)

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
                if (hours >= bcout%kinks_hrs(iin)) then
                    do j = iin + 1, bcout%kinks_num
                        if (hours < bcout%kinks_hrs(j)) then
                            i = j - 1
                            exit
                        end if
                    end do
                else
                    do j = iin - 1, 1, -1
                        if (hours > bcout%kinks_hrs(j)) then
                            i = j
                            exit
                        end if
                    end do
                end if

            else

                do
                    k = (i + j) / 2
                    if (hours < bcout%kinks_hrs(k)) then
                        j = k
                    else
                        i = k
                    end if
                    if (i + 1 >= j) exit
                end do
            end if
        end if

        wage = bcout%kinks_earn(2) / bcout%kinks_hrs(2)
        mtr  = bcout%kinks_mtr(i)
        net  = bcout%kinks_net(i) + mtr * wage * (hours - bcout%kinks_hrs(i))
        earn = wage * hours

        if (present(iout)) then
            iout = i
        end if

    end subroutine evalKinksHours


    ! evalKinksEarn
    ! -----------------------------------------------------------------------
    ! uses the piecewise linear budget constraint bcout as calculated in
    ! kinksearn to evaluate the respective income measure at earn

    pure subroutine evalKinksEarn(bcout, earn, hours, net, mtr, iin, iout)

        implicit none

        type(bcout_t),  intent(in)  :: bcout
        real(dp),       intent(in)  :: earn
        real(dp),       intent(out) :: hours, net, mtr
        integer,        intent(in), optional :: iin
        integer,        intent(out), optional :: iout
        integer :: i, j, k

        i = 1
        j = bcout%kinks_num

        if (earn >= bcout%kinks_earn(bcout%kinks_num)) then
            i = bcout%kinks_num
        elseif (earn <= bcout%kinks_earn(1)) then
            i = 1
        else

            if (present(iin)) then
                if (earn >= bcout%kinks_earn(iin)) then
                    do j = iin + 1, bcout%kinks_num
                        if (earn < bcout%kinks_earn(j)) then
                            i = j - 1
                            exit
                        end if
                    end do
                else
                    do j = iin - 1, 1, -1
                        if (earn>bcout%kinks_earn(j)) then
                            i = j
                            exit
                        end if
                    end do
                end if

            else

                do
                    k = (i + j) / 2
                    if (earn < bcout%kinks_earn(k)) then
                        j = k
                    else
                        i = k
                    end if
                    if (i + 1 >= j) exit
                end do
            end if
        end if

        mtr   = bcout%kinks_mtr(i)
        net   = bcout%kinks_net(i) + mtr * (earn - bcout%kinks_earn(i))
        hours = bcout%kinks_hrs(i)

        if (present(iout)) then
            iout = i
        end if

    end subroutine evalKinksEarn


    ! kinks_desc
    ! -----------------------------------------------------------------------
    ! prints the budget constraitn in bcout

    subroutine kinks_desc(bcout, fname)
        use, intrinsic :: iso_fortran_env
        use fortax_util, only : strCentre, fortaxerror
        use fortax_type, only : len_bcdesc
        implicit none
        type(bcout_t), intent(in) :: bcout
        character(len = *), optional :: fname
        character(len = len_bcdesc) :: bc_desc
        integer :: funit, i, ios

        if (present(fname)) then
            open(newunit = funit, file = fname, action = 'write', status = 'replace', iostat = ios)
            if (ios .ne. 0) call fortaxError('error opening file for writing in kinks_desc')
        else
            funit = output_unit
        end if

        bc_desc = transfer(bcout%bc_desc, bc_desc)

        write(funit, *)
        write(funit, '(A)') repeat("=", 62)
        ! write(funit, '(A)') strCentre('kinks_desc (' // trim(adjustl(bc_desc)) // '):', 62)
        write(funit, '(A)') strCentre(trim(adjustl(bc_desc)), 62)
        write(funit, '(A)') repeat("=", 62)
        write(funit, '(A14, 2X, A14, 2X, A14, 2X, A13)') "Hours", "Earnings", "Income", "Rate"
        write(funit, '(A)') repeat("=", 62)
        do i = 1, bcout%kinks_num
            if (abs(bcout%kinks_mtr(i)) >= 9.998_dp) then
                write(*,'(F14.3, 2X, F14.3, 2X, F14.3, 2X, F13.5,"*")') &
                    bcout%kinks_hrs(i), bcout%kinks_earn(i), bcout%kinks_net(i), bcout%kinks_mtr(i)
            else
                write(*,'(F14.3, 2X, F14.3, 2X, F14.3, 2X, F13.5)') &
                    bcout%kinks_hrs(i), bcout%kinks_earn(i), bcout%kinks_net(i), bcout%kinks_mtr(i)
            end if
        end do
        write(funit, '(A)') repeat("=", 62)

        if (present(fname)) then
            close(funit)
        end if

    end subroutine kinks_desc


    ! kinkshours
    ! -----------------------------------------------------------------------
    ! calculates a piecewise linear schedule under the a given tax system
    ! for a family by varying hours of work with a fixed hourly wage. can be
    ! performed for any income component (or linear combination of)

    subroutine kinkshours(sys, fam, ad, wage, hours1, hours2, bcout, taxlevel, taxout, correct)

        use fortax_type, only : fam_t, sys_t, net_t, len_bcdesc
        use fortax_util, only : lower, inttostr, fortaxerror, fortaxwarn
        use fortax_calc, only : calcnetinc

        implicit none

        type(sys_t),    intent(in)  :: sys
        type(fam_t),    intent(in)  :: fam
        integer,        intent(in)  :: ad
        real(dp),       intent(in)  :: wage
        real(dp),       intent(in)  :: hours1, hours2
        type(bcout_t),  intent(out) :: bcout

        character(len = *), intent(in), optional :: taxlevel
        character(len = *), intent(in), optional :: taxout(:)
        logical, intent(in), optional :: correct

        character(len = :), allocatable :: ltaxout, ltaxlevel, bc_desc
        character(len = len_bcdesc) :: temp_bcstr
        character(len = 64) :: str
        type(fam_t)  :: fam0
        type(net_t), target :: net
        real(dp) :: taxcomp0, taxcomp1
        real(dp) :: taxrate0, taxrate1
        real(dp) :: hrs, hrs0
        integer  :: i

        real(dp), parameter :: mtrtol = 1.0e-5_dp
        real(dp), parameter :: distol = 1.01_dp
        real(dp), parameter :: htol   = 0.00001_dp

        !maxstep is the main parameter that determines the number of evaluations,
        !if too large then may miss some discontinuities
        real(dp), parameter :: maxstep = 1.00_dp
        real(dp), parameter :: minstep = maxstep / 10000.0_dp

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

        logical :: levelad, leveltu
        integer :: taxad, taxsize
        real(dp) :: temp

        real(dp) :: hrs_b, hrs_a, hrs_mid, temp_a, temp_b, dhrs, rate_a, rate_b

        if ((.not. present(taxout)) .and. (.not. present(taxlevel))) then
            allocate(taxpoint(1))
            allocate(taxadd(1))
            taxadd = .true.
            taxsize = 1
            taxpoint(1)%p => net%tu%dispinc
            bc_desc = "tu%dispinc"
        else if (((.not. present(taxout)) .and. (present(taxlevel))) &
            .or. ((present(taxout)) .and. (.not. present(taxlevel)))) then
            call fortaxerror('if taxout or taxlevel is specified, both must be specified')
        else
            taxsize = size(taxout)
            allocate(taxpoint(taxsize))
            allocate(taxadd(taxsize))
            taxadd    = .true.
            ltaxlevel = lower(adjustl(taxlevel))
            if (trim(adjustl(ltaxlevel)) == 'tu') then
                leveltu = .true.
                levelad = .false.
                bc_desc = "tu%"
            else if (trim(adjustl(ltaxlevel)) == 'ad1') then
                leveltu = .false.
                levelad = .true.
                taxad   = 1
                bc_desc = "ad(1)%"
            else if (trim(adjustl(ltaxlevel)) == 'ad2') then
                leveltu = .false.
                levelad = .true.
                taxad   = 2
                bc_desc = "ad(2)%"
            else
                call fortaxerror('taxlevel ' // trim(adjustl(ltaxlevel)) // ' is unrecognized')
            end if

            if (taxsize > 1) then
                bc_desc = bc_desc // "("
            end if

            do i = 1, taxsize
                ltaxout = lower(trim(adjustl(taxout(i))))

                if (ltaxout(1:1) == '+') then
                    ltaxout = adjustl(ltaxout(2:))
                elseif (ltaxout(1:1) == '-') then
                    ltaxout = adjustl(ltaxout(2:))
                    taxadd(i) = .false.
                end if

                if (taxadd(i)) then
                    if (i == 1) then
                        bc_desc = bc_desc // ltaxout
                    else
                        bc_desc = bc_desc // " + " // ltaxout
                    end if
                else
                    if (i == 1) then
                        if (taxsize == 1) then
                            bc_desc = "-" // bc_desc // ltaxout
                        else
                        bc_desc = bc_desc // "-" // ltaxout
                        end if
                    else
                        bc_desc = bc_desc // " - " // ltaxout
                    end if
                end if

                if (i == taxsize) then
                    bc_desc = bc_desc // ")"
                end if

                if (levelad) then
        if (ltaxout == "taxable") taxpoint(i)%p => net%ad(taxad)%taxable
        if (ltaxout == "inctax") taxpoint(i)%p => net%ad(taxad)%inctax
        if (ltaxout == "natins") taxpoint(i)%p => net%ad(taxad)%natins
        if (ltaxout == "natinsc1") taxpoint(i)%p => net%ad(taxad)%natinsc1
        if (ltaxout == "natinsc2") taxpoint(i)%p => net%ad(taxad)%natinsc2
        if (ltaxout == "natinsc4") taxpoint(i)%p => net%ad(taxad)%natinsc4
        if (ltaxout == "pretaxearn") taxpoint(i)%p => net%ad(taxad)%pretaxearn
        if (ltaxout == "posttaxearn") taxpoint(i)%p => net%ad(taxad)%posttaxearn
                else
        if (ltaxout == "pretaxearn") taxpoint(i)%p => net%tu%pretaxearn
        if (ltaxout == "posttaxearn") taxpoint(i)%p => net%tu%posttaxearn
        if (ltaxout == "chben") taxpoint(i)%p => net%tu%chben
        if (ltaxout == "matgrant") taxpoint(i)%p => net%tu%matgrant
        if (ltaxout == "fc") taxpoint(i)%p => net%tu%fc
        if (ltaxout == "wtc") taxpoint(i)%p => net%tu%wtc
        if (ltaxout == "ctc") taxpoint(i)%p => net%tu%ctc
        if (ltaxout == "ccexp") taxpoint(i)%p => net%tu%ccexp
        if (ltaxout == "incsup") taxpoint(i)%p => net%tu%incsup
        if (ltaxout == "hben") taxpoint(i)%p => net%tu%hben
        if (ltaxout == "polltax") taxpoint(i)%p => net%tu%polltax
        if (ltaxout == "polltaxben") taxpoint(i)%p => net%tu%polltaxben
        if (ltaxout == "ctax") taxpoint(i)%p => net%tu%ctax
        if (ltaxout == "ctaxben") taxpoint(i)%p => net%tu%ctaxben
        if (ltaxout == "maxuc") taxpoint(i)%p => net%tu%maxuc
        if (ltaxout == "uc") taxpoint(i)%p => net%tu%uc
        if (ltaxout == "dispinc") taxpoint(i)%p => net%tu%dispinc
        if (ltaxout == "pretax") taxpoint(i)%p => net%tu%pretax
        if (ltaxout == "nettax") taxpoint(i)%p => net%tu%nettax
        if (ltaxout == "chcaresub") taxpoint(i)%p => net%tu%chcaresub
        if (ltaxout == "fsm") taxpoint(i)%p => net%tu%fsm
        if (ltaxout == "totben") taxpoint(i)%p => net%tu%totben
                end if

                if (.not. associated(taxpoint(i)%p)) then
                    call fortaxerror(ltaxout // ' does not exist')
                end if

            end do

        end if

        if (hours2 - hours1 < maxstep) return
        if (hours1 < 0.0_dp)           return
        if (zeroWage) then
            if (wage < 0.0_dp) return
        else
            if (wage <= 0.0_dp) return
        end if

        select case(ad)
            case(1)
            case(2)
                if (fam%couple == 0) return
            case default
                return
        end select

        !don't modify original structure
        fam0 = fam

        !calculate income at lower range
        fam0%ad(ad)%earn = wage * hours1
        fam0%ad(ad)%hrs = hours1
        call calcNetInc(sys, fam0, net)

        taxcomp0 = 0.0_dp
        do i = 1, taxsize
            taxcomp0 = taxcomp0 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
        end do

        !step to get marginal rate
        hrs = hours1 + minstep
        fam0%ad(ad)%earn = wage * hrs
        fam0%ad(ad)%hrs = hrs
        call calcNetInc(sys, fam0, net)

        taxcomp1 = 0.0_dp
        do i = 1, taxsize
            taxcomp1 = taxcomp1 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
        end do

        taxrate1 = (taxcomp1 - taxcomp0) / (wage * minstep)

        kinks_hrs(1) = hours1
        kinks_earn(1) = wage * hours1
        kinks_net(1) = taxcomp0
        kinks_mtr(1) = taxrate1
        kinks_dis(1) = .false.

        if (zeroWage) then
            if (wage == 0.0_dp) then
                kinks_mtr(1) = 0.0_dp
            end if
        end if

        kinkidx = 2
        taxrate0 = -999.0_dp

        hrs0 = hours1 + minstep

        taxrate0 = taxrate1
        taxcomp0 = taxcomp1

loopmax : do

            if (kinkidx >= maxkinks) exit

            hrs = hrs + maxstep

            if (hrs > hours2) exit

            fam0%ad(ad)%earn = wage * hrs
            fam0%ad(ad)%hrs  = hrs

            call calcNetInc(sys, fam0, net)

            taxcomp1 = 0.0_dp
            do i = 1, taxsize
                taxcomp1 = taxcomp1 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
            end do

            taxrate1 = (taxcomp1 - taxcomp0) / (wage * maxstep)

            !if zero wage, use level not slope
            if (zeroWage) then
                if (wage == 0.0_dp) then
                    taxrate1 = 0.0_dp
                    do i = 1, taxsize
                        taxrate1 = taxrate1 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                    end do
                end if
            end if

            !if a mtr change detected
            if (abs(taxrate1 - taxrate0) > mtrtol) then

                hrs_b  = hrs
                hrs_a  = hrs0
                rate_a = taxrate0
                temp_a = taxcomp0

                !use bisection to identify the change point more accurately
                do !while (abs(hrs_b-hrs_a)>0.00001_dp)

                    if (abs(hrs_b - hrs_a) > htol) then
                        !midpoint of domain
                        hrs_mid = 0.5_dp * (hrs_b + hrs_a)
                        dhrs    = 0.5_dp * (hrs_b - hrs_a)

                        fam0%ad(ad)%earn = wage * hrs_mid
                        fam0%ad(ad)%hrs = hrs_mid
                        call calcNetInc(sys, fam0, net)

                        temp_b = 0.0_dp
                        do i = 1, taxsize
                            temp_b = temp_b + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                        end do
                        rate_b = (temp_b - temp_a) / (wage * dhrs)

                        !which direction to move?
                        if (abs(rate_b - rate_a) > mtrtol) then
                            hrs_b = hrs_mid
                        else
                            hrs_a = hrs_mid
                            temp_a = temp_b
                        end if
                    else

                        fam0%ad(ad)%earn = wage * hrs_a
                        fam0%ad(ad)%hrs = hrs_a
                        call calcNetInc(sys, fam0, net)

                        temp_a = 0.0_dp
                        do i = 1, taxsize
                            temp_a = temp_a + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                        end do

                        fam0%ad(ad)%earn = wage * hrs_b
                        fam0%ad(ad)%hrs = hrs_b
                        call calcNetInc(sys, fam0, net)

                        temp_b = 0.0_dp
                        do i = 1, taxsize
                            temp_b = temp_b + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                        end do

                        dhrs = hrs_b - hrs_a
                        taxrate1 = (temp_b - temp_a) / (wage * dhrs)

                        !is there a discontinuity?
                        if (abs(taxrate1) > distol) then
                            kinks_hrs(kinkidx) = hrs_a
                            kinks_earn(kinkidx) = wage * hrs_a
                            kinks_net(kinkidx) = temp_a
                            kinks_dis(kinkidx) = .true.
                            if (taxcomp1 > temp_b) then
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
                fam0%ad(ad)%earn = wage * hrs
                fam0%ad(ad)%hrs  = hrs
                call calcNetInc(sys, fam0, net)

                taxcomp1 = 0.0_dp
                do i = 1, taxsize
                    taxcomp1 = taxcomp1 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                end do

                taxrate1 = (taxcomp1 - temp_b) / (wage * minstep)

                kinks_hrs(kinkidx) = hrs_b
                kinks_earn(kinkidx) = wage * hrs_b
                kinks_net(kinkidx)  = temp_b
                kinks_mtr(kinkidx)  = taxrate1
                kinks_dis(kinkidx)  = .false.

                if (zeroWage) then
                    if (wage == 0.0_dp) then
                        kinks_mtr(kinkidx) = 0.0_dp
                    end if
                end if

                kinkidx = kinkidx + 1

            end if

            taxrate0 = taxrate1
            taxcomp0 = taxcomp1
            hrs0     = hrs

        end do loopmax

        if (kinkidx < maxkinks) then
            !end point
            fam0%ad(ad)%earn = wage*hours2
            fam0%ad(ad)%hrs = hours2
            call CalcNetInc(sys, fam0, net)

            temp = 0.0_dp
            do i = 1, taxsize
                temp = temp + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
            end do

            taxcomp1 = temp !taxpoint

            !kinkidx             = kinkidx + 1
            kinks_hrs(kinkidx) = fam0%ad(ad)%hrs
            kinks_earn(kinkidx) = fam0%ad(ad)%earn
            kinks_net(kinkidx) = taxcomp1 !b
            kinks_mtr(kinkidx) = kinks_mtr(kinkidx - 1)

        else
            call fortaxwarn('maxkinks is exceeded')
        end if

        kinks_num = min(kinkidx, maxkinks)

        !rounding correction to ensure consistency
        if (present(correct)) then
            if (correct) then
                do i = 1, kinks_num
                    !if (.not. kinks_dis(i)) then
                        !round to 5 decimal places
                        !using nint could cause overflow problems
!                        kinks_mtr(i)  = nint(kinks_mtr(i)*100000.0_dp)/100000.0_dp
!                        kinks_earn(i) = nint(kinks_earn(i)*1000.0_dp)/1000.0_dp
                        write(str, '(F18.5)') kinks_mtr(i)
                        read(str, *) kinks_mtr(i)
                        write(str, '(F18.5)') kinks_earn(i)
                        read(str, *) kinks_earn(i)
                    !end if
                end do

                kinks_earn(1) = nint(kinks_earn(1) * 1000.0_dp) / 1000.0_dp

                do i = 2, kinks_num
                    if (kinks_dis(i - 1)) then
!                        kinks_net(i) = nint(kinks_net(i)*1000.0_dp)/1000.0_dp
                        write(str, '(F18.5)') kinks_net(i)
                        read(str, *) kinks_net(i)
                    else
                        kinks_net(i) = kinks_net(i - 1) + kinks_mtr(i - 1) * (kinks_earn(i) - kinks_earn(i - 1))
                    end if
                end do

            end if
        end if

        temp_bcstr = bc_desc
        bcout%kinks_num  = kinks_num
        bcout%kinks_hrs  = kinks_hrs
        bcout%kinks_earn = kinks_earn
        bcout%kinks_net  = kinks_net
        bcout%kinks_mtr  = kinks_mtr
        bcout%bc_desc = transfer(temp_bcstr, bcout%bc_desc)

        do i = 1, taxsize
            nullify(taxpoint(i)%p)
        end do

        !nullify(taxpoint)

    end subroutine kinkshours


    ! kinksearn
    ! -----------------------------------------------------------------------
    ! calculates a piecewise linear schedule under the a given tax system
    ! for a family by varying earnings of with fixed weekly hours of work.
    ! can be performed for any income component (or linear combination of)

    subroutine kinksearn(sys, fam, ad, hours, earn1, earn2, bcout, taxlevel, taxout, correct)

        use fortax_type, only : fam_t, sys_t, net_t, len_bcdesc
        use fortax_util, only : lower, inttostr, fortaxerror, fortaxwarn
        use fortax_calc, only : calcnetinc

        implicit none

        type(sys_t),    intent(in)  :: sys
        type(fam_t),    intent(in)  :: fam
        integer,        intent(in)  :: ad
        real(dp),       intent(in)  :: hours
        real(dp),       intent(in)  :: earn1, earn2
        type(bcout_t),  intent(out) :: bcout

        character(len = *), intent(in), optional :: taxlevel
        character(len = *), intent(in), optional :: taxout(:)
        logical, intent(in), optional :: correct

        !character(len(taxout))             :: ltaxout
        !character(len(taxlevel))           :: ltaxlevel
        character(len = :), allocatable :: ltaxout, ltaxlevel, bc_desc
        character(len = len_bcdesc) :: temp_bcstr
        character(len = 64) :: str

        type(fam_t) :: fam0
        type(net_t), target :: net
        real(dp) :: taxcomp0, taxcomp1
        real(dp) :: taxrate0, taxrate1
        real(dp) :: earn, earn0 !,ern0,ernb !,ern0b
        integer :: i !,step, stepb, i

!        real(dp), parameter :: mtrtol  = 1.0e-8_dp
!        real(dp), parameter :: distol  = 1.01_dp
!
!        real(dp), parameter :: maxstep = 0.10_dp
!        real(dp), parameter :: minstep = maxstep/2000.0_dp

        real(dp), parameter :: mtrtol = 1.0e-5_dp
        real(dp), parameter :: distol = 1.01_dp
        real(dp), parameter :: etol = 0.00001_dp
        !maxstep is the main parameter that determines the number of evaluations,
        !if too large then may miss some discontinuities
        real(dp), parameter :: maxstep = 5.00_dp
        real(dp), parameter :: minstep = maxstep / 10000.0_dp

        !integer, parameter :: maxkinks = 200
        integer :: kinkidx

        real(dp), dimension(maxkinks) :: kinks_hrs
        real(dp), dimension(maxkinks) :: kinks_earn
        real(dp), dimension(maxkinks) :: kinks_net
        real(dp), dimension(maxkinks) :: kinks_mtr
        logical, dimension(maxkinks) :: kinks_dis
        integer :: kinks_num

        type real_pointer
            real(dp), pointer :: p => null()
        end type

        type(real_pointer), allocatable, dimension(:) :: taxpoint
        logical, allocatable, dimension(:) :: taxadd
        !real(dp), pointer :: taxpoint

        !real(dp), pointer :: taxpoint => null()
        logical :: levelad, leveltu
        integer :: taxad, taxsize
        real(dp) :: temp

        real(dp) :: earn_b, earn_a, earn_mid, temp_a, temp_b, dearn, rate_a, rate_b

        if ((.not. present(taxout)) .and. (.not. present(taxlevel))) then
            taxsize = 1
            allocate(taxpoint(1))
            allocate(taxadd(1))
            taxadd = .true.
            taxpoint(1)%p => net%tu%dispinc
            bc_desc = "tu%dispinc"
        else if (((.not. present(taxout)) .and. (present(taxlevel))) &
            .or. ((present(taxout)) .and. (.not. present(taxlevel)))) then
            call fortaxerror('if taxout or taxlevel is specified, both must be specified')
        else
            taxsize = size(taxout)
            allocate(taxpoint(taxsize))
            allocate(taxadd(taxsize))
            taxadd = .true.
            ltaxlevel = lower(taxlevel)
            if (trim(adjustl(ltaxlevel))=='tu') then
                leveltu = .true.
                levelad = .false.
                bc_desc = "tu%"
            else if (trim(adjustl(ltaxlevel))=='ad1') then
                leveltu = .false.
                levelad = .true.
                taxad   = 1
                bc_desc = "ad(1)%"
            else if (trim(adjustl(ltaxlevel))=='ad2') then
                leveltu = .false.
                levelad = .true.
                taxad   = 2
                bc_desc = "ad(2)%"
            else
                call fortaxerror('taxlevel '//trim(adjustl(ltaxlevel))//' is unrecognized')
            end if

            if (taxsize > 1) then
                bc_desc = bc_desc // "("
            end if

            do i = 1, taxsize

                ltaxout = lower(trim(adjustl(taxout(i))))

                if (ltaxout(1:1) == '+') then
                    ltaxout = adjustl(ltaxout(2:))
                elseif (ltaxout(1:1) == '-') then
                    ltaxout = adjustl(ltaxout(2:))
                    taxadd(i) = .false.
                end if

                if (taxadd(i)) then
                    if (i == 1) then
                        bc_desc = bc_desc // ltaxout
                    else
                        bc_desc = bc_desc // " + " // ltaxout
                    end if
                else
                    if (i == 1) then
                        if (taxsize == 1) then
                            bc_desc = "-" // bc_desc // ltaxout
                        else
                        bc_desc = bc_desc // "-" // ltaxout
                        end if
                    else
                        bc_desc = bc_desc // " - " // ltaxout
                    end if
                end if

                if (i == taxsize) then
                    bc_desc = bc_desc // ")"
                end if

                if (levelad) then
        if (ltaxout == "taxable") taxpoint(i)%p => net%ad(taxad)%taxable
        if (ltaxout == "inctax") taxpoint(i)%p => net%ad(taxad)%inctax
        if (ltaxout == "natins") taxpoint(i)%p => net%ad(taxad)%natins
        if (ltaxout == "natinsc1") taxpoint(i)%p => net%ad(taxad)%natinsc1
        if (ltaxout == "natinsc2") taxpoint(i)%p => net%ad(taxad)%natinsc2
        if (ltaxout == "natinsc4") taxpoint(i)%p => net%ad(taxad)%natinsc4
        if (ltaxout == "pretaxearn") taxpoint(i)%p => net%ad(taxad)%pretaxearn
        if (ltaxout == "posttaxearn") taxpoint(i)%p => net%ad(taxad)%posttaxearn
                else
        if (ltaxout == "pretaxearn") taxpoint(i)%p => net%tu%pretaxearn
        if (ltaxout == "posttaxearn") taxpoint(i)%p => net%tu%posttaxearn
        if (ltaxout == "chben") taxpoint(i)%p => net%tu%chben
        if (ltaxout == "matgrant") taxpoint(i)%p => net%tu%matgrant
        if (ltaxout == "fc") taxpoint(i)%p => net%tu%fc
        if (ltaxout == "wtc") taxpoint(i)%p => net%tu%wtc
        if (ltaxout == "ctc") taxpoint(i)%p => net%tu%ctc
        if (ltaxout == "ccexp") taxpoint(i)%p => net%tu%ccexp
        if (ltaxout == "incsup") taxpoint(i)%p => net%tu%incsup
        if (ltaxout == "hben") taxpoint(i)%p => net%tu%hben
        if (ltaxout == "polltax") taxpoint(i)%p => net%tu%polltax
        if (ltaxout == "polltaxben") taxpoint(i)%p => net%tu%polltaxben
        if (ltaxout == "ctax") taxpoint(i)%p => net%tu%ctax
        if (ltaxout == "ctaxben") taxpoint(i)%p => net%tu%ctaxben
        if (ltaxout == "maxuc") taxpoint(i)%p => net%tu%maxuc
        if (ltaxout == "uc") taxpoint(i)%p => net%tu%uc
        if (ltaxout == "dispinc") taxpoint(i)%p => net%tu%dispinc
        if (ltaxout == "pretax") taxpoint(i)%p => net%tu%pretax
        if (ltaxout == "nettax") taxpoint(i)%p => net%tu%nettax
        if (ltaxout == "chcaresub") taxpoint(i)%p => net%tu%chcaresub
        if (ltaxout == "fsm") taxpoint(i)%p => net%tu%fsm
        if (ltaxout == "totben") taxpoint(i)%p => net%tu%totben
                end if

                if (.not. associated(taxpoint(i)%p)) then
                    call fortaxerror(trim(ltaxout)//' does not exist')
                end if

            end do

        end if

        !if (.not. associated(taxpoint)) return !NOT-SAFE
        if (earn2 - earn1 < maxstep) return
        if (earn1 < 0.0_dp) return
        if (hours < 0.0_dp) return

        select case(ad)
            case(1)
            case(2)
                if (fam%couple == 0) return
            case default
                return
        end select

        !don't modify original structure
        fam0 = fam

        !set hours at all points
        kinks_hrs = hours
        fam0%ad(ad)%hrs = hours

        !calculate income at lower range
        fam0%ad(ad)%earn = earn1

        call calcNetInc(sys, fam0, net)

        taxcomp0 = 0.0_dp
        do i = 1, taxsize
            taxcomp0 = taxcomp0 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
        end do

        !step to get marginal rate
        earn = earn1 + minstep
        fam0%ad(ad)%earn = earn
        !fam0%ad(ad)%hrs  = hrs
        call calcNetInc(sys, fam0, net)

        taxcomp1 = 0.0_dp
        do i = 1, taxsize
            taxcomp1 = taxcomp1 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
        end do

        taxrate1 = (taxcomp1 - taxcomp0) / minstep

        kinks_earn(1) = earn1
        kinks_net(1) = taxcomp0
        kinks_mtr(1) = taxrate1
        kinks_dis(1) = .false.

        kinkidx = 2

        earn0 = earn1 + minstep

        taxrate0 = taxrate1
        taxcomp0 = taxcomp1

loopmax : do

            if (kinkidx >= maxkinks) exit

            earn = earn + maxstep

            if (earn > earn2) exit

            fam0%ad(ad)%earn = earn
            !fam0%ad(ad)%hrs  = hrs

            call calcNetInc(sys, fam0, net)

            taxcomp1 = 0.0_dp
            do i = 1, taxsize
                taxcomp1 = taxcomp1 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
            end do

            taxrate1 = (taxcomp1 - taxcomp0) / maxstep

            !if a mtr change detected
            if (abs(taxrate1 - taxrate0) > mtrtol) then

                earn_b = earn
                earn_a = earn0
                rate_a = taxrate0
                temp_a = taxcomp0

                !use bisection to identify the change point more accurately
                do !while (abs(hrs_b-hrs_a)>0.00001_dp)

                    if (abs(earn_b - earn_a) > etol) then
                        !midpoint of domain
                        earn_mid = 0.5_dp * (earn_b + earn_a)
                        dearn = 0.5_dp * (earn_b - earn_a)

                        fam0%ad(ad)%earn = earn_mid

                        call calcNetInc(sys, fam0, net)

                        temp_b = 0.0_dp
                        do i = 1, taxsize
                            temp_b = temp_b + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                        end do
                        rate_b = (temp_b - temp_a) / dearn

                        !which direction to move?
                        if (abs(rate_b - rate_a) > mtrtol) then
                            earn_b = earn_mid
                        else
                            earn_a = earn_mid
                            temp_a = temp_b
                        end if
                    else

                        fam0%ad(ad)%earn = earn_a

                        call calcNetInc(sys, fam0, net)

                        temp_a = 0.0_dp
                        do i = 1, taxsize
                            temp_a = temp_a + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                        end do

                        fam0%ad(ad)%earn = earn_b
                        call calcNetInc(sys, fam0, net)

                        temp_b = 0.0_dp
                        do i = 1, taxsize
                            temp_b = temp_b + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                        end do

                        dearn = earn_b - earn_a
                        taxrate1 = (temp_b - temp_a) / dearn

                        !is there a discontinuity?
                        if (abs(taxrate1) > distol) then
                            kinks_earn(kinkidx) = earn_a
                            kinks_net(kinkidx) = temp_a
                            kinks_dis(kinkidx) = .true.
                            if (taxcomp1 > temp_b) then
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
                call calcNetInc(sys, fam0, net)

                taxcomp1 = 0.0_dp
                do i = 1, taxsize
                    taxcomp1 = taxcomp1 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                end do

                taxrate1 = (taxcomp1 - temp_b) / minstep

                kinks_earn(kinkidx) = earn_b
                kinks_net(kinkidx) = temp_b
                kinks_mtr(kinkidx) = taxrate1
                kinks_dis(kinkidx) = .false.

                kinkidx = kinkidx + 1

            end if

            taxrate0 = taxrate1
            taxcomp0 = taxcomp1
            earn0 = earn

        end do loopmax

        if (kinkidx < maxkinks) then
            !end point
            fam0%ad(ad)%earn = earn2
            call CalcNetInc(sys, fam0, net)

            temp = 0.0_dp
            do i = 1, taxsize
                temp = temp + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
            end do

            taxcomp1 = temp !taxpoint

            kinks_earn(kinkidx) = fam0%ad(ad)%earn
            kinks_net(kinkidx) = taxcomp1
            kinks_mtr(kinkidx) = kinks_mtr(kinkidx - 1)
        else
            call fortaxwarn('maxkinks is exceeded')
        end if

        kinks_num = min(kinkidx, maxkinks)

        !rounding correction to ensure consistency
        if (present(correct)) then
            if (correct) then
                do i = 1, kinks_num
                    !if (.not. kinks_dis(i)) then
                        !round to 5 decimal places
!                        kinks_mtr(i)  = nint(kinks_mtr(i)*100000.0_dp)/100000.0_dp
!                        kinks_earn(i) = nint(kinks_earn(i)*1000.0_dp)/1000.0_dp
                        write(str, '(F18.5)') kinks_mtr(i)
                        read(str, *) kinks_mtr(i)
                        write(str, '(F18.5)') kinks_earn(i)
                        read(str, *) kinks_earn(i)

                    !end if
                end do

                kinks_earn(1) = nint(kinks_earn(1) * 1000.0_dp) / 1000.0_dp

                do i = 2, kinks_num
                    if (kinks_dis(i - 1)) then
!                        kinks_net(i) = nint(kinks_net(i)*1000.0_dp)/1000.0_dp
                        write(str, '(F18.5)') kinks_net(i)
                        read(str, *) kinks_net(i)
                    else
                        kinks_net(i) = kinks_net(i - 1) + kinks_mtr(i - 1) * (kinks_earn(i) - kinks_earn(i - 1))
                    end if
                end do

            end if
        end if

        temp_bcstr = bc_desc
        bcout%kinks_num = kinks_num
        bcout%kinks_hrs = kinks_hrs
        bcout%kinks_earn(1:kinks_num) = kinks_earn(1:kinks_num)
        bcout%kinks_net(1:kinks_num) = kinks_net(1:kinks_num)
        bcout%kinks_mtr(1:kinks_num) = kinks_mtr(1:kinks_num)
        bcout%bc_desc = transfer(temp_bcstr, bcout%bc_desc)

        do i = 1, taxsize
            nullify(taxpoint(i)%p)
        end do

        !nullify(taxpoint)

    end subroutine kinksearn

    ! kinksccexp
    ! -----------------------------------------------------------------------
    ! calculates a piecewise linear schedule under the a given tax system
    ! for a family by varying childcare expenditure of with fixed weekly
    ! hours of work and earnings. can be performed for any income component
    ! (or linear combination of)

    subroutine kinksccexp(sys, fam, ad, hours, earn, ccexp1, ccexp2, bcout, taxlevel, taxout, correct)

        use fortax_type, only : fam_t, sys_t, net_t, len_bcdesc
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

        character(len = *), intent(in), optional :: taxlevel
        character(len = *), intent(in), optional :: taxout(:)
        logical, intent(in), optional :: correct

        !character(len(taxout))             :: ltaxout
        !character(len(taxlevel))           :: ltaxlevel
        character(len = :), allocatable :: ltaxout, ltaxlevel, bc_desc
        character(len = len_bcdesc) :: temp_bcstr
        type(fam_t) :: fam0
        type(net_t), target :: net
        real(dp) :: taxcomp0, taxcomp1
        real(dp) :: taxrate0, taxrate1
        real(dp) :: ccexp, ccexp0
        integer :: i

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
        real(dp), parameter :: minstep = maxstep / 10000.0_dp

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
        logical :: levelad, leveltu
        integer :: taxad, taxsize
        real(dp) :: temp

        real(dp) :: ccexp_b, ccexp_a, ccexp_mid, temp_a, temp_b, dccexp, rate_a, rate_b

        if ((.not. present(taxout)) .and. (.not. present(taxlevel))) then
            taxsize = 1
            allocate(taxpoint(1))
            allocate(taxadd(1))
            taxadd = .true.
            taxpoint(1)%p => net%tu%dispinc
            bc_desc = "tu%dispinc"            
        else if (((.not. present(taxout)) .and. (present(taxlevel))) &
            .or. ((present(taxout)) .and. (.not. present(taxlevel)))) then
            call fortaxerror('if taxout or taxlevel is specified, both must be specified')
        else
            taxsize = size(taxout)
            allocate(taxpoint(taxsize))
            allocate(taxadd(taxsize))
            taxadd = .true.
            ltaxlevel = lower(taxlevel)
            if (trim(adjustl(ltaxlevel)) == 'tu') then
                leveltu = .true.
                levelad = .false.
                bc_desc = "tu%"                
            else if (trim(adjustl(ltaxlevel)) == 'ad1') then
                leveltu = .false.
                levelad = .true.
                taxad = 1
                bc_desc = "ad(1)%"                
            else if (trim(adjustl(ltaxlevel)) == 'ad2') then
                leveltu = .false.
                levelad = .true.
                taxad = 2
                bc_desc = "ad(2)%"                
            else
                call fortaxerror('taxlevel ' // trim(adjustl(ltaxlevel)) // ' is unrecognized')
            end if

            if (taxsize > 1) then
                bc_desc = bc_desc // "("
            end if

            do i = 1, taxsize

                ltaxout = lower(trim(adjustl(taxout(i))))

                if (ltaxout(1:1) == '+') then
                    ltaxout = adjustl(ltaxout(2:))
                elseif (ltaxout(1:1) == '-') then
                    ltaxout = adjustl(ltaxout(2:))
                    taxadd(i) = .false.
                end if

                if (taxadd(i)) then
                    if (i == 1) then
                        bc_desc = bc_desc // ltaxout
                    else
                        bc_desc = bc_desc // " + " // ltaxout
                    end if
                else
                    if (i == 1) then
                        if (taxsize == 1) then
                            bc_desc = "-" // bc_desc // ltaxout
                        else
                        bc_desc = bc_desc // "-" // ltaxout
                        end if
                    else
                        bc_desc = bc_desc // " - " // ltaxout
                    end if
                end if

                if (i == taxsize) then
                    bc_desc = bc_desc // ")"
                end if

                if (levelad) then
        if (ltaxout == "taxable") taxpoint(i)%p => net%ad(taxad)%taxable
        if (ltaxout == "inctax") taxpoint(i)%p => net%ad(taxad)%inctax
        if (ltaxout == "natins") taxpoint(i)%p => net%ad(taxad)%natins
        if (ltaxout == "natinsc1") taxpoint(i)%p => net%ad(taxad)%natinsc1
        if (ltaxout == "natinsc2") taxpoint(i)%p => net%ad(taxad)%natinsc2
        if (ltaxout == "natinsc4") taxpoint(i)%p => net%ad(taxad)%natinsc4
        if (ltaxout == "pretaxearn") taxpoint(i)%p => net%ad(taxad)%pretaxearn
        if (ltaxout == "posttaxearn") taxpoint(i)%p => net%ad(taxad)%posttaxearn
                else
        if (ltaxout == "pretaxearn") taxpoint(i)%p => net%tu%pretaxearn
        if (ltaxout == "posttaxearn") taxpoint(i)%p => net%tu%posttaxearn
        if (ltaxout == "chben") taxpoint(i)%p => net%tu%chben
        if (ltaxout == "matgrant") taxpoint(i)%p => net%tu%matgrant
        if (ltaxout == "fc") taxpoint(i)%p => net%tu%fc
        if (ltaxout == "wtc") taxpoint(i)%p => net%tu%wtc
        if (ltaxout == "ctc") taxpoint(i)%p => net%tu%ctc
        if (ltaxout == "ccexp") taxpoint(i)%p => net%tu%ccexp
        if (ltaxout == "incsup") taxpoint(i)%p => net%tu%incsup
        if (ltaxout == "hben") taxpoint(i)%p => net%tu%hben
        if (ltaxout == "polltax") taxpoint(i)%p => net%tu%polltax
        if (ltaxout == "polltaxben") taxpoint(i)%p => net%tu%polltaxben
        if (ltaxout == "ctax") taxpoint(i)%p => net%tu%ctax
        if (ltaxout == "ctaxben") taxpoint(i)%p => net%tu%ctaxben
        if (ltaxout == "maxuc") taxpoint(i)%p => net%tu%maxuc
        if (ltaxout == "uc") taxpoint(i)%p => net%tu%uc
        if (ltaxout == "dispinc") taxpoint(i)%p => net%tu%dispinc
        if (ltaxout == "pretax") taxpoint(i)%p => net%tu%pretax
        if (ltaxout == "nettax") taxpoint(i)%p => net%tu%nettax
        if (ltaxout == "chcaresub") taxpoint(i)%p => net%tu%chcaresub
        if (ltaxout == "fsm") taxpoint(i)%p => net%tu%fsm
        if (ltaxout == "totben") taxpoint(i)%p => net%tu%totben
                end if

                if (.not. associated(taxpoint(i)%p)) then
                    call fortaxerror(trim(ltaxout) // ' does not exist')
                end if

            end do

        end if

        !if (.not. associated(taxpoint)) return !NOT-SAFE
        if (ccexp2 - ccexp1 < maxstep) return
        if (ccexp1 < 0.0_dp) return
        if (earn < 0.0_dp) return
        if (hours < 0.0_dp) return

        select case(ad)
            case(1)
            case(2)
                if (fam%couple == 0) return
            case default
                return
        end select

        !don't modify original structure
        fam0 = fam

        !set hours at all points
        kinks_hrs = hours
        fam0%ad(ad)%hrs = hours
        !kinks_earn       = earn
        fam0%ad(ad)%earn = earn

        !calculate income at lower range
        fam0%ccexp = ccexp1

        call calcNetInc(sys, fam0, net)

        taxcomp0 = 0.0_dp
        do i = 1, taxsize
            taxcomp0 = taxcomp0 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
        end do

        !step to get marginal rate
        ccexp = ccexp1 + minstep
        fam0%ccexp = ccexp
        !fam0%ad(ad)%hrs  = hrs
        call calcNetInc(sys,fam0,net)

        taxcomp1 = 0.0_dp
        do i = 1, taxsize
            taxcomp1 = taxcomp1 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
        end do

        taxrate1 = (taxcomp1 - taxcomp0) / minstep

        !kinks_earn(1) = ccexp1
        kinks_ccexp(1) = ccexp1
        kinks_net(1) = taxcomp0
        kinks_mtr(1) = taxrate1
        kinks_dis(1) = .false.

        kinkidx = 2

        ccexp0 = ccexp1 + minstep

        taxrate0 = taxrate1
        taxcomp0 = taxcomp1

loopmax : do

            if (kinkidx >= maxkinks) exit

            ccexp = ccexp + maxstep

            if (ccexp > ccexp2) exit

            fam0%ccexp = ccexp
            !fam0%ad(ad)%hrs  = hrs

            call calcNetInc(sys, fam0, net)

            taxcomp1 = 0.0_dp
            do i = 1, taxsize
                taxcomp1 = taxcomp1 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
            end do

            taxrate1 = (taxcomp1 - taxcomp0) / maxstep

            !if a mtr change detected
            if (abs(taxrate1 - taxrate0) > mtrtol) then

                ccexp_b = ccexp
                ccexp_a = ccexp0
                rate_a = taxrate0
                temp_a = taxcomp0

                !use bisection to identify the change point more accurately
                do !while (abs(hrs_b-hrs_a)>0.00001_dp)

                    if (abs(ccexp_b - ccexp_a) > etol) then
                        !midpoint of domain
                        ccexp_mid = 0.5_dp * (ccexp_b + ccexp_a)
                        dccexp = 0.5_dp * (ccexp_b - ccexp_a)

                        fam0%ccexp = ccexp_mid

                        call calcNetInc(sys, fam0, net)

                        temp_b = 0.0_dp
                        do i = 1, taxsize
                            temp_b = temp_b + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                        end do
                        rate_b = (temp_b - temp_a) / dccexp

                        !which direction to move?
                        if (abs(rate_b - rate_a) > mtrtol) then
                            ccexp_b = ccexp_mid
                        else
                            ccexp_a = ccexp_mid
                            temp_a = temp_b
                        end if
                    else

                        fam0%ccexp = ccexp_a

                        call calcNetInc(sys, fam0, net)

                        temp_a = 0.0_dp
                        do i = 1, taxsize
                            temp_a = temp_a + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                        end do

                        fam0%ccexp = ccexp_b
                        call calcNetInc(sys, fam0, net)

                        temp_b = 0.0_dp
                        do i = 1, taxsize
                            temp_b = temp_b + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                        end do

                        dccexp = ccexp_b - ccexp_a
                        taxrate1 = (temp_b - temp_a) / dccexp

                        !is there a discontinuity?
                        if (abs(taxrate1) > distol) then
                            kinks_ccexp(kinkidx) = ccexp_a
                            kinks_net(kinkidx) = temp_a
                            kinks_dis(kinkidx) = .true.
                            if (taxcomp1 > temp_b) then
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
                call calcNetInc(sys, fam0, net)

                taxcomp1 = 0.0_dp
                do i = 1, taxsize
                    taxcomp1 = taxcomp1 + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
                end do

                taxrate1 = (taxcomp1 - temp_b) / minstep

                kinks_ccexp(kinkidx) = ccexp_b
                kinks_net(kinkidx) = temp_b
                kinks_mtr(kinkidx) = taxrate1
                kinks_dis(kinkidx) = .false.

                kinkidx = kinkidx + 1


            end if

            taxrate0 = taxrate1
            taxcomp0 = taxcomp1
            ccexp0    = ccexp

        end do loopmax

        if (kinkidx < maxkinks) then
            !end point
            fam0%ccexp = ccexp2
            call CalcNetInc(sys, fam0, net)

            temp = 0.0_dp
            do i = 1, taxsize
                temp = temp + merge(taxpoint(i)%p, -taxpoint(i)%p, taxadd(i))
            end do

            taxcomp1 = temp !taxpoint

            kinks_ccexp(kinkidx) = fam0%ccexp
            kinks_net(kinkidx) = taxcomp1
            kinks_mtr(kinkidx) = kinks_mtr(kinkidx - 1)
        else
            call fortaxwarn('maxkinks is exceeded')
        end if

        kinks_num = min(kinkidx, maxkinks)

        !rounding correction to ensure consistency
        if (present(correct)) then
            if (correct) then
                do i = 1, kinks_num
                    !if (.not. kinks_dis(i)) then
                        !round to 5 decimal places
                        kinks_mtr(i) = nint(kinks_mtr(i) * 100000.0_dp) / 100000.0_dp
                        kinks_ccexp(i) = nint(kinks_ccexp(i) * 1000.0_dp) / 1000.0_dp
                    !end if
                end do

                kinks_ccexp(1) = nint(kinks_ccexp(1) * 1000.0_dp) / 1000.0_dp

                do i = 2, kinks_num
                    if (kinks_dis(i - 1)) then
                        kinks_net(i) = nint(kinks_net(i) * 1000.0_dp) / 1000.0_dp
                    else
                        kinks_net(i) = kinks_net(i - 1) + kinks_mtr(i - 1) * (kinks_ccexp(i) - kinks_ccexp(i - 1))
                    end if
                end do

            end if
        end if

        temp_bcstr = bc_desc
        bcout%kinks_num = kinks_num
        bcout%kinks_hrs = kinks_hrs
        bcout%kinks_earn(1:kinks_num) = kinks_ccexp(1:kinks_num)
        bcout%kinks_net(1:kinks_num) = kinks_net(1:kinks_num)
        bcout%kinks_mtr(1:kinks_num) = kinks_mtr(1:kinks_num)
        bcout%bc_desc = transfer(temp_bcstr, bcout%bc_desc)

        do i = 1, taxsize
            nullify(taxpoint(i)%p)
        end do

        !nullify(taxpoint)

    end subroutine kinksccexp

end module fortax_kinks
