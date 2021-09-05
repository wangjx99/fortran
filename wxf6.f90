program name
    implicit none
    integer::stat
    character(len=50):: line
    open(100,file='new.txt',status='replace',action='readwrite', access = 'sequential') 
    write(100,*) 'to see a world in a grain of sand'
    write(100,*) 'and a heaven in a wild flower'
    write(100,*)'hold infinity in the palm of your hand'
    write(100,*)'and eternity in an hour'
    rewind(100)
    do
        read(100, '(a50)', iostat = stat) line
        if(stat /= 0) exit
        print *, line
    end do
    close(100)
end program name