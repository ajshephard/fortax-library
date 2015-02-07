program test3d

	use fortax_library
	use fortax_kinks3d

	implicit none
	
	type(fam_t) :: fam
	type(net_t) :: net
	type(sys_t) :: sys
	type(bcout3d_t) :: bcout
	integer, parameter :: dp = 8
	real(dp) :: earn1(100)
	real(dp) :: earn2(100)

	integer :: iX, kX, iX1, iX2

	real(dp) :: eval(100,100,1)
	real(dp) :: calc(100,100,1)
	
	do iX = 1, 100
		earn1(iX) = (iX-1)*10.0_dp
		earn2(iX) = (iX-1)*20.0_dp
	end do

    call FORTAX_readFortaxParams(sys,'systems/fortax/April03.xml')

    fam = FORTAX_fam_gen( couple=.true., age1=25, hrs1=30.0_dp, earn1=0.0_dp, &
       age2=25, hrs2=30.0_dp, earn2=0.0_dp, &
       rent=0.0_dp, ctband=3, banddratio=1.0_dp, kidage = (/10,10/) )

    call kinksearn3d(sys,fam,30.0_dp,20.0_dp,earn1,earn2,bcout)
! print *,size(earn1)

	do kX = 1, 1
	    do iX2 = 1, size(earn1)
		    do iX1 = 1, size(earn2)
		    	fam%ad(1)%earn = real(iX1,dp)/10.0_dp !earn1(iX1)
		    	fam%ad(2)%earn = real(iX2,dp)/10.0_dp !earn2(iX2)
		    	fam%ad(1)%hrs = 30.0_dp
		    	fam%ad(2)%hrs = 30.0_dp
		    	call FORTAX_calcNetInc(sys,fam,net)
		    	calc(iX1,iX2,kX) = net%tu%dispinc
		    	eval(iX1,iX2,kX) = evalTaxCouScalar(bcout,fam%ad(1)%earn,fam%ad(2)%earn)

				!write(*,'(4(F18.8,4X))')  fam%ad(1)%earn, fam%ad(2)%earn, evalTaxCouScalar(bcout,fam%ad(1)%earn,fam%ad(2)%earn), net%tu%dispinc
		    end do
		end do
	end do

	print *, sum(abs(calc-eval))

end program test3d