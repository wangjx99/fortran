program zzz
    implicit none
    character(100) j,abbb
    read(*,'(a100)')j
    write(*,'(a100)')adjustl(abbb(j))
end program zzz

character(*) FUNCTION abbb(j) 
    implicit none
    character(*),intent(in)::j
    integer ::i,k=1
    do i=len(j),1,-1
        abbb(k:k)=j(i:i)
        k=k+1
    end do     
end function abbb