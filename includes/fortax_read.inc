do i = 1, size(system)
select case(system(i)%basename)
#include 'includes/system/syslist.inc'
case default
print *, 'warning: basename '//trim(adjustl(system(i)%basename))//' is unrecognized'
end select
end do