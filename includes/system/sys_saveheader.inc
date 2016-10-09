subroutine concat(_$typelist,_save(sys,funit))
use fortax_util, only : intToStr, dblToStr
implicit none
type(concat(_$typelist,_t)), intent(in) :: sys
integer, intent(in) :: funit; integer :: i
write(funit,*); write(funit,'(a)') "!"//_$makestr(_$typelist)