
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




! fortax_extra
! -----------------------------------------------------------------------
! module provides various additional functionality, operating on systems,
! families, or income measures, AS

module fortax_extra

    use fortax_realtype, only : dp

    implicit none

    integer,  parameter :: netoutSize = 100

contains

    ! setminamount
    ! -----------------------------------------------------------------------
    ! sets the minimum amount for all benefits to the specified amount

    subroutine setminamount(sys,minamt)
            
        use fortax_type, only : sys_t
        
        implicit none
        
        type(sys_t), intent(inout) :: sys
        real(dp),    intent(in)    :: minamt

        logical, parameter :: null      = .false.          
        logical, parameter :: range     = .false.
        logical, parameter :: scale     = .false.          
        logical, parameter :: rate      = .false.          
        logical, parameter :: amount    = .false.
        logical, parameter :: minamount = .true.
                
        !done automatically for entire system via FPP, AS       
#       include 'includes/fortax_minamt.inc'
        
    end subroutine setminamount
    

    ! abolishnifee
    ! -----------------------------------------------------------------------
    ! abolish national insurance entry fee (pre-99 there NI was paid on total
    ! earnings once threshold was reached. this removes that

    subroutine abolishnifee(sys)
    
        use fortax_type, only : sys_t
        use fortax_util, only : fortaxwarn
        
        implicit none
        
        type(sys_t), intent(inout) :: sys
        real(dp)                   :: amt
        real(dp), parameter        :: tol = 1e-5_dp
                
        if (sys%natins%numrates<2) then
            call fortaxwarn('warning in abolishnifee: require sys%natins%numrates>=2')
        end if
        
        if (sys%natins%rates(1)>tol) then
            amt = sys%natins%rates(1)*sys%natins%bands(1)
            sys%natins%rates(1) = 0.0_dp
            sys%natins%bands(1) = sys%natins%bands(1) - amt/sys%natins%rates(2)
        end if
        
    end subroutine abolishnifee
        

    ! fsminappamt
    ! -----------------------------------------------------------------------
    ! high wage parents will lose entitlement to FSM when they come off IS
    ! smooth this by adding fsm in appamt. this works through sys%extra

    subroutine fsminappamt(sys,inappamt)
    
        use fortax_type, only : sys_t
        
        implicit none
       
        type(sys_t), intent(inout) :: sys
        logical,     intent(in)    :: inappamt
        
        sys%extra%fsminappamt = inappamt
        
    end subroutine fsminappamt
    

    ! tapermatgrant
    ! -----------------------------------------------------------------------
    ! high wage parents will lose entitlement maternity grant when they come 
    ! off IS. smooth this by adding matgr in appamt. Also, lose when they 
    ! come off FC/WFTC (in this case, taper with tax credits)

    subroutine tapermatgrant(sys,taper)
    
        use fortax_type, only : sys_t
        
        implicit none
       
        type(sys_t), intent(inout) :: sys
        logical,     intent(in)    :: taper
        
        sys%extra%matgrant = taper
        
    end subroutine tapermatgrant

    
    ! netoutDesc
    ! -----------------------------------------------------------------------
    ! provides the shortname, level and amount of net in arrays of size
    ! netoutSize. Useful for passing data without using derived types

    pure subroutine netoutDesc(net,netoutLevel,netoutName,netoutAmt,netoutNum)

        use fortax_type, only : net_t

        implicit none
        
        type(net_t),   intent(in)  :: net
        character(16), intent(out) :: netoutLevel(netoutSize), netoutName(netoutSize)
        real(dp),      intent(out) :: netoutAmt(netoutSize)
        integer,       intent(out) :: netoutNum

        integer :: i
        
        i = 1
#       undef  _$header
#       undef  _$footer
#       undef  _$double
#       define _$header
#       define _$footer
#       define _$double(x,lab,y) netoutLevel(i) ='tu'; netoutName(i) = #x; netoutAmt(i) = net%tu%x; i=i+1
#       include 'includes/nettu_t.inc'
#       undef  _$header
#       undef  _$footer
#       undef  _$double

#       undef  _$header
#       undef  _$footer
#       undef  _$double
#       define _$header
#       define _$footer
#       define _$double(x,lab,y) netoutLevel(i) ='ad1'; netoutName(i) = #x; netoutAmt(i) = net%ad(1)%x; i=i+1
#       include 'includes/netad_t.inc'
#       undef  _$header
#       undef  _$footer
#       undef  _$double

#       undef  _$header
#       undef  _$footer
#       undef  _$double
#       define _$header
#       define _$footer
#       define _$double(x,lab,y) netoutLevel(i) ='ad2'; netoutName(i) = #x; netoutAmt(i) = net%ad(2)%x; i=i+1
#       include 'includes/netad_t.inc'
#       undef  _$header
#       undef  _$footer
#       undef  _$double

        netoutNum = i-1

    end subroutine netoutDesc

end module
