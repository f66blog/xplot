program main
    use device
    implicit none  
    class(device_t), allocatable :: fig
    real, allocatable :: x(:), y(:)
    integer :: n = 10**3
    allocate(x(n), y(n))
    call random_seed()
    call random_number(x)
    call random_number(y)  

uplot: block
        use  uniplot
        allocate(fig_t::fig)
        call fig%init(100, 100, 'MonteCarlo')
        call monte_carlo(fig, x, y) 
        deallocate(fig)
    end block uplot

xplot: block
        use xplot
        allocate(fig_t::fig)
        call fig%init(450, 450, 'MonteCarlo')
        call monte_carlo(fig, x, y) 
        deallocate(fig)
    end block xplot

contains
    subroutine monte_carlo(fig, x, y)
        class(device_t), intent(in out) :: fig
        real, intent(in) :: x(:), y(:)
        integer :: i, ix0, iy0, ix1, iy1, k
        k = fig%nx
        print *
        print *, 'Monte Carlo: estimated pi =', 4.0 * count(x**2 + y**2 < 1.0) / size(x) 
        !draw box
        call fig%line0(0, 0, k-1, 0)
        call fig%line0(0, 0, 0, k-1)
        call fig%line0(0, k-1, k-1, k-1)
        call fig%line0(k-1, 0, k-1, k-1)
        ! draw 1/4 circle
        ix0 = 0
        iy0 = 0
        do ix1 = 0, k - 1
            iy1 = k - 1 - int(sqrt(real((k-1)**2 - ix1**2)))
            call fig%line0(ix0, iy0, ix1, iy1)
            ix0 = ix1
            iy0 = iy1
        end do           
  
        do i = 1, n 
            call fig%point(int(k * x(i)), int(k * y(i)))
        end do 
        call fig%show()
    end subroutine monte_carlo

end program main
