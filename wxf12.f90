program main
    implicit none
    type A
         integer::sta,lat,lon,alt,ye,mon,pre1,pre2,pre3
    end type
    type(A) qs(12,63)
    open(10,file ='SURF_CLI_CHN_MUL_MON-PRE-57494 .TXT')
    open(20,file ='SURF_CLI_CHN_NUL_JAN-PRE-57494 .TXT', status='replace')
    read(10,*)qs
    write(20,'(2i5,i6,i7,i5,i3,3i7)') qs(1,:)
    close(10)  
    close(20)
end program main