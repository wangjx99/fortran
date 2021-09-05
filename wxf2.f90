program dq
    implicit none
    real::s=1367,r=0.3,b=5.6696e-10,y
    y=(s*(1-r)/4/b)**0.25
    write(*,'(f10.2)')y
end program dq