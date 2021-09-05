program AA
    implicit none
    character(len=10)::c='helleword'
    real::d=1.1
    integer::e=1
    logical::k=.true.
    write(*,'(a)')c
    write(*,'(f9.1)')d
    write(*,'(i9)')e
    write(*,'(l9)')k
end program AA