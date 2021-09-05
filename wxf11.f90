program name
    implicit none
    integer a
    integer, external :: fact
    read(*, *) a
    write(*, *) fact(a)
end program name

recursive function fact(x) result(y)
integer x, y
if ( x < 0 ) then
    y = -9999
else if (x == 0) then
    y = 1
else 
    y = x * fact( x - 1)
end if 
end function