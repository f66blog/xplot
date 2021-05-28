program main
    implicit none  
    real, allocatable :: x(:), y(:)
    integer :: n
    n = 10**3
    allocate(x(n), y(n))
    call random_seed()
    call random_number(x)
    call random_number(y)  

uplot: block
        use  uniplot
        type(fig_t) :: fig1
        integer :: i, ix0, iy0, ix1, iy1, k
        k = 100
        print *
        print *, 'Monte Carlo: estimated pi =', 4.0 * count(x**2 + y**2 < 1.0) / n 
        call fig1%init(k, k)
        ! draw box
        call fig1%line0(0, 0, k-1, 0)
        call fig1%line0(0, 0, 0, k-1)
        call fig1%line0(0, k-1, k-1, k-1)
        call fig1%line0(k-1, 0, k-1, k-1)
        ! draw 1/4 circle
        ix0 = 0
        iy0 = 0
        do ix1 = 0, k - 1
            iy1 = k - 1 - int(sqrt(real((k-1)**2 - ix1**2)))
            call fig1%line0(ix0, iy0, ix1, iy1)
            ix0 = ix1
            iy0 = iy1
        end do       

        do i = 1, n 
            call fig1%point(int(k * x(i)), int(k * y(i)))
        end do 
        call fig1%show()

    end block uplot

xplot: block
        use xplot
        type(fig_t) :: fig2
        integer :: i, ix0, iy0, ix1, iy1, k
        k = 450
        print *
        print *, 'Monte Carlo: estimated pi =', 4.0 * count(x**2 + y**2 < 1.0) / n 
        call fig2%init(k, k)
        ! draw box
        call fig2%line0(0, 0, k-1, 0)
        call fig2%line0(0, 0, 0, k-1)
        call fig2%line0(0, k-1, k-1, k-1)
        call fig2%line0(k-1, 0, k-1, k-1)
        ! draw 1/4 circle
        ix0 = 0
        iy0 = 0
        do ix1 = 0, k - 1
            iy1 = k - 1 - int(sqrt(real((k-1)**2 - ix1**2)))
            call fig2%line0(ix0, iy0, ix1, iy1)
            ix0 = ix1
            iy0 = iy1
        end do           

        do i = 1, n 
            call fig2%point(int(k * x(i)), int(k * y(i)))
        end do 
        call fig2%show()
    end block xplot




end program main