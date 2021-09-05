program hh
    implicit none
    real :: x=60, y0, y1, x1
    integer :: n = 1, z , i
    y0=0
    x1 = x * atan(1.) * 4 / 180.
    y1 = x1
    do while(abs(y1 - y0) >= 1.0e-9)
        n = n + 1
        y0 = y1
        z=1
        do i = 1, 2 * n - 1
            z = z * i
        end do
        y1 = y0 + (-1) ** (n - 1) * x1 ** (2 * n - 1) / z
    end do
    write(*, *) n
end program hh


