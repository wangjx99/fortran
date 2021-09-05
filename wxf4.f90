program ss
    implicit none
    real in, out
    10 write(*,'(a13)')'please input:'
    read(*,*) in
    if(in>=6001.and.in<=6000)then
        out=0
    else if(in>=6001.and.in<=20000)then
        out=0.17*(in-6000)
    else if(in>=20001.and.in<=50000)then
        out=2380+0.3*(in-20000)
    else if(in>=50001.and.in<=60000)then
        out=11380+ 0.42*(in-50000) 
    else if(in>=60001)then
        out=15580+0.47*(in-60000) 
    else
        write(*,'(a34)')'invalid number,check and retype.'
        goto 10
    end if 
    write(*,'(a13,f10.2,a3)')'result:',out,'AUD  '                  
end program ss 