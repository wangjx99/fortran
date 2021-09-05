program name
    implicit none
    !real y(9), x(9)
    integer i
    real,DIMENSION(9)::x,y
    x = (/ (i, i = -40, 40, 10) /)
    WHERE(x>=0)
    y=6.108 * EXP(17.27*x/(x+237.3))
else WHERE(x<0)
    y=6.112 * EXP(17.67*x/(x+243.5))
    end where
    write(*,'(9(f10.3))')x, y
end program name