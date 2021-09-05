program name
    implicit none
    type B
         real::sta,lat,lon,alt,ye,mon,pre1,pre2,pre3
    end type
    real, external :: average
    type(B)arr(12,63)
    integer i,j
    real x(63, 2)
    real temp0(12,63),temp1(12,63),temp2(12,63),temp3(12,63)
    open(10,file='SURF_CLI_CHN_MUL_MON-TEM-59287.TXT')
    open(20,file='20181003377wxf_1.txt',status='replace')
    open(30,file='20181003377wxf_2.txt',status='replace')
    read(10, *)
    READ(10,*)arr
    temp0 = arr%ye
    do i = 1, 63
        write(*,'(f6.0)') average(temp0(:, i))
    end do
    temp1 = arr%pre1
    do i = 1, 63
        !print *, temp1(:, i)
        write(*, '(f6.2)') average(temp1(:, i))
    end do
    100 format(f6.2)
    temp2 = arr%pre2
    do i = 1, 63
        write(*, 100) average(temp2(:, i))
    end do
    temp3 = arr%pre3
    do i = 1, 63
        write(*, 100) average(temp3(:, i))
    end do
do j = 1, 63
    write(30,'(f6.0,5x,f6.2,5x,f6.2,5x,f6.2)')temp0(:,j),temp1(:,j),temp2(:,j),temp3(:,j)
end do

x(:, 1) = 1
x(:, 2) = (/(i, i = 1951, 2013)/)
y = temp1(:, j)
        
    
end program name

real function average(s) 
    implicit none
    real s(12)
    average = sum(s) / 12
end function


subroutine inverse(a,c,n)
    !============================================================
    ! Inverse matrix
    ! Method: Based on Doolittle LU factorization for Ax=b
    ! Alex G. December 2009
    !-----------------------------------------------------------
    ! input ...
    ! a(n,n) - array of coefficients for matrix A
    ! n - dimension
    ! output ...
    ! c(n,n) - inverse matrix of A
    ! comments ...
    ! the original matrix a(n,n) will be destroyed
    ! during the calculation
    !===========================================================
    implicit none
    integer n
    real  a(n,n), c(n,n)
    real  L(n,n), U(n,n), b(n), d(n), x(n)
    real  coeff
    integer i, j, k
    ! step 0: initialization for matrices L and U and b
    ! Fortran 90/95 aloows such operations on matrices
    L=0.0
    U=0.0
    b=0.0
    ! step 1: forward elimination
    do k=1, n-1
    do i=k+1,n
    coeff=a(i,k)/a(k,k)
    L(i,k) = coeff
    do j=k+1,n
    a(i,j) = a(i,j)-coeff*a(k,j)
    end do
    end do
    end do
    ! Step 2: prepare L and U matrices
    ! L matrix is a matrix of the elimination coefficient
    ! + the diagonal elements are 1.0
    do i=1,n
    L(i,i) = 1.0
    end do
    ! U matrix is the upper triangular part of A
    do j=1,n
    do i=1,j
    U(i,j) = a(i,j)
    end do
    end do
    ! Step 3: compute columns of the inverse matrix C
    do k=1,n
    b(k)=1.0
    d(1) = b(1)
    ! Step 3a: Solve Ld=b using the forward substitution
    do i=2,n
    d(i)=b(i)
    do j=1,i-1
    d(i) = d(i) - L(i,j)*d(j)
    end do
    end do
    ! Step 3b: Solve Ux=d using the back substitution
    x(n)=d(n)/U(n,n)
    do i = n-1,1,-1
    x(i) = d(i)
    do j=n,i+1,-1
    x(i)=x(i)-U(i,j)*x(j)
    end do
    x(i) = x(i)/u(i,i)
    end do
    ! Step 3c: fill the solutions x(n) into column k of C
    do i=1,n
    c(i,k) = x(i)
    end do
    b(k)=0.0
    end do
    end subroutine inverse
    