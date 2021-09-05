module wx
    implicit none
    save 
    contains   
    real function average(s) 
        implicit none
        real s(:)
        average = sum(s) / size(s)
    end function
    real function digui(t)
        implicit none
        real x(63, 2), y(63), c(2, 2), temp0(12,63)
        real t(:)
        x(:, 1) = 1
         x(:, 2) = (/(temp0(1, i), i = 1, 63)/)
        y = (/(average((:, i)), i = 1, 63)/)
        call inverse(matmul(transpose(x), x), c, 2)
        digui = matmul(matmul(c, transpose(x)), y)
    end function    
    subroutine inverse(a,c,n)
        implicit none
        integer n
        real  a(n,n), c(n,n)
        real  L(n,n), U(n,n), b(n), d(n), x(n)
        real  coeff
        integer i, j, k
        L=0.0
        U=0.0
        b=0.0
        do k=1, n-1
        do i=k+1,n
        coeff=a(i,k)/a(k,k)
        L(i,k) = coeff
        do j=k+1,n
        a(i,j) = a(i,j)-coeff*a(k,j)
        end do
        end do
        end do
        do i=1,n
        L(i,i) = 1.0
        end do
        do j=1,n
        do i=1,j
        U(i,j) = a(i,j)
        end do
        end do
        do k=1,n
        b(k)=1.0
        d(1) = b(1)
        do i=2,n
        d(i)=b(i)
        do j=1,i-1
        d(i) = d(i) - L(i,j)*d(j)
        end do
        end do
        x(n)=d(n)/U(n,n)
        do i = n-1,1,-1
        x(i) = d(i)
        do j=n,i+1,-1
        x(i)=x(i)-U(i,j)*x(j)
        end do
        x(i) = x(i)/u(i,i)
        end do
        do i=1,n
        c(i,k) = x(i)
        end do
        b(k)=0.0
        end do
        end subroutine inverse
    end module wx    
program name
    use wx
    implicit none
    type B
         real::sta,lat,lon,alt,ye,mon,pre1,pre2,pre3
    end type
    type(B)arr(12,63)
    integer i
    real x(63, 2), y(63), c(2, 2)
    real temp0(12,63),temp1(12,63),temp2(12,63),temp3(12,63)
    open(10,file='SURF_CLI_CHN_MUL_MON-TEM-59287.TXT')
    open(20,file='20181003377wxf_1.txt',status='replace')
    open(30,file='20181003377wxf_2.txt',status='replace')
    read(10, *)
    READ(10,*)arr
    temp0 = arr%ye
    temp1 = arr%pre1    
    temp2 = arr%pre2        
    temp3 = arr%pre3    
    do i = 1, 63
        write(20, '(i5, 3f10.2)')  nint(temp0(1, i)), average(temp1(:, i)), average(temp2(:, i)), average(temp3(:, i))
    end do
    x(:, 1) = 1
    x(:, 2) = (/(temp0(1, i), i = 1, 63)/)
    y = (/(average(temp1(:, i)), i = 1, 63)/)
    !call inverse(matmul(transpose(x), x), c, 2)
    write(30,1000) digui(temp1)! matmul(matmul(c, transpose(x)), y)
    !y = (/(average(temp2(:, i)), i = 1, 63)/)
   ! call inverse(matmul(transpose(x), x), c, 2)
     write(30,1000)  digui(temp2)!matmul(matmul(c, transpose(x)), y)
   ! y = (/(average(temp3(:, i)), i = 1, 63)/)
    !call inverse(matmul(transpose(x), x), c, 2)
    write(30,1000) digui(temp3)! matmul(matmul(c, transpose(x)), y)
    1000 format (2f7.2)
end program name


    