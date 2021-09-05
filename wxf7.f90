program name
    implicit none
    integer st, ye
    real data(12)
    open(10, file = 'data-asci-seq.dat', access = 'sequential')
    open(11, file = 'data-binary.dat', form = 'unformatted', access = 'stream', status = 'replace')
    do
        read(10, 100, iostat = st) ye, data
        if(st /= 0) exit
        write(11) ye, data
    end do
    close(10)
    close(11)
    open(20, file = 'data-binary.dat', form = 'unformatted', access = 'direct', recl = 52)
    read(20, rec = 5) ye, data
    write(*, 100) ye, data
    100 format(i6,12f6.2)
end program name
