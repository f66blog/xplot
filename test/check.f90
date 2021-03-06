program check
    use device
    implicit none

    logistic_map: block
        use uniplot
!        use xplot
!        use psplot
        class(device_t), allocatable :: fig
        integer :: ix, i, nx = 150, ny = 80
        real :: p, x, y
        allocate(fig_t::fig)
        print *,  'logistic map'
        call fig%init(nx, ny, 'Loggistic_map')
        do ix = 1, nx
            p = 0.3
            x = ix * (3.0 - 1.5) / nx + 1.5 
            do i = 1, 50
                p = p + x * p * (1.0 - p)
            end do
            do i = 51, 100
                y = p / 1.5 * ny
                call fig%point(ix, nint(1.0*(ny - y)))
                p = p + x * p * (1 - p)
            end do
        end do
        call fig%show()
    end block logistic_map

    Lorenz_attractor: block
        use uniplot
!        use xplot
!        use psplot
        class(device_t), allocatable :: fig
        integer, parameter :: kd = kind(1.0d0)
        real (kd) :: x, y, z, dx, dy, dz, a, b, c, d
        integer :: k, nx = 200, ny = 200
        real :: scale
        allocate(fig_t::fig)
        print *,  'Lorenz attractor'
        scale = 2.0
        call fig%init(nx, ny, 'Lorenz_attractor')
        a = 10.0_kd
        b = 28.0_kd
        c = 8.0_kd / 3.0_kd
        d = 0.01_kd
        x = 1.0_kd
        y = 1.0_kd
        z = 1.0_kd
        do k = 1, 3000
            dx = a * (y - x)
            dy = x * (b - z) - y
            dz = x * y - c * z
            x = x + d * dx
            y = y + d * dy
            z = z + d * dz
            if (k < 100) then 
                call fig%line( scale * real(x), scale * real(z), 0)
            else
                call fig%line( scale * real(x), scale * real(z), 1)
            end if 
        end do
        call fig%show()
    end block Lorenz_attractor

    Laplaces_equation: block
!        use uniplot
!        use xplot
        use psplot 
!        use htmlplot 
        class(device_t), allocatable :: fig
        real :: x, y, v(-50:50, -50:50)
        integer :: i, j
        allocate(fig_t::fig)               !
        print *,  'Laplace''s equation'
        call fig%init(150, 170, 'Laplaces_equation')
        call laplace(v)
! x-direction  
        do i = lbound(v, 2), ubound(v, 2), 10
            x = lbound(v, 2) + 0.2 * i
            y = 30.0 - 0.4 * i 
            call fig%line(x, y, 0)
            do j = lbound(v, 1), ubound(v, 1), 10
                x = j + 0.2 * i 
                y = 30.0 - 0.4 * i - 2.0 * v(j, i) 
                call fig%line(x, y, 1)
            end do
        end do
! y-direction
        do j = lbound(v, 1), ubound(v, 1), 10
            x = j + 0.2 * lbound(v, 2) 
            y = 30.0 - 0.4 * lbound(v, 2)
            call fig%line(x, y, 0)
            do i = lbound(v, 2), ubound(v, 2), 10
                x = j + 0.2 * i 
                y = 30.0 - 0.4 * i - 2.0 * v(j, i) 
                call fig%line(x, y, 1)
            end do
        end do
        call fig%show()
    end block Laplaces_equation 
    
    Abe_san: block 
!        use uniplot
        !use htmlplot
        use xplot
        class(device_t), allocatable :: fig1
        allocate(fig_t::fig1)    
        call fig1%init(450, 450, 'Abe_san')
        ! chin chin
        call fig1%line(  0.0,  11.0, 0)
        call fig1%line(  0.0,   8.0, 1)    
    
        call fig1%line( -8.0, -26.5, 0)
        call fig1%line(-11.0, -24.0, 1)
        call fig1%line(  8.0, -26.5, 0)
        call fig1%line( 11.0, -24.0, 1)    
    
        call fig1%line(  5.5, -26.0, 0)
        call fig1%line(  2.0, -36.0, 1)
        call fig1%line( -5.5, -26.0, 0)
        call fig1%line( -2.0, -36.0, 1)    
    
        call fig1%line( 20.0, -18.0, 0)
        call fig1%line(  8.5, -19.0, 1)
        call fig1%line(  4.0, -21.5, 1)
        call fig1%line(  0.0, -23.0, 1)
        call fig1%line( -4.0, -21.5, 1)
        call fig1%line( -8.5, -19.0, 1)
        call fig1%line(-20.0, -18.0, 1)    
    
        call fig1%line( 12.0, -16.0, 0)
        call fig1%line( 22.0,  14.0, 1)
        call fig1%line(-12.0, -16.0, 0)
        call fig1%line(-22.0,  14.0, 1)
         
        call fig1%line( 53.0,  -9.0, 0)
        call fig1%line( 28.5,   1.0, 1)
        call fig1%line( 28.5, -14.0, 0)
        call fig1%line( 28.5,  25.0, 1)
        call fig1%line( 28.5,  33.0, 0)
        call fig1%line( 28.5,  76.0, 1)
        call fig1%line( -2.5,  76.0, 1)
        call fig1%line( -2.5,  72.0, 1)
        call fig1%line( -0.5,  68.0, 1)
        call fig1%line(  1.0,  66.0, 1)
        call fig1%line( -1.5,  67.0, 1)
        call fig1%line( -4.0,  72.0, 1)
        call fig1%line( -4.0,  76.0, 1)
        call fig1%line(-28.5,  76.0, 1)
        call fig1%line(-28.5,  33.0, 1)    
    
        call fig1%line(-28.5,  25.0, 0)
        call fig1%line(-28.5, -14.0, 1)
        call fig1%line(-53.0,  -9.0, 0)
        call fig1%line(-28.5,   1.0, 1)     
    
        call fig1%line(  0.0,   0.0, 0)
        call fig1%line(  6.5,   0.0, 1)
        call fig1%line( 10.0,   3.0, 1)
        call fig1%line( 15.0,   7.0, 1)
        call fig1%line( 22.0,  14.0, 1)
        call fig1%line( 28.5,  25.0, 1)
        call fig1%line( 31.0,  26.0, 1)
        call fig1%line( 35.0,  34.0, 1)
        call fig1%line( 38.0,  44.0, 1)
        call fig1%line( 38.0,  53.0, 1)
        call fig1%line( 36.0,  55.0, 1)
        call fig1%line( 32.0,  55.0, 1)
        call fig1%line( 28.5,  51.0, 1)    
    
        call fig1%line(  0.0,   0.0, 0)
        call fig1%line( -6.5,   0.0, 1)
        call fig1%line(-10.0,   3.0, 1)
        call fig1%line(-15.0,   7.0, 1)
        call fig1%line(-22.0,  14.0, 1)
        call fig1%line(-28.5,  25.0, 1)
        call fig1%line(-31.0,  26.0, 1)
        call fig1%line(-35.0,  34.0, 1)
        call fig1%line(-38.0,  44.0, 1)
        call fig1%line(-38.0,  53.0, 1)
        call fig1%line(-36.0,  55.0, 1)
        call fig1%line(-32.0,  55.0, 1)
        call fig1%line(-28.5,  51.0, 1)    
    
        call fig1%line( 34.0,  55.0, 0)
        call fig1%line( 34.0,  76.0, 1)
        call fig1%line( 31.0,  82.0, 1)
        call fig1%line( 26.0,  87.0, 1)
        call fig1%line( 21.0,  91.0, 1)
        call fig1%line( 15.0,  95.0, 1)
        call fig1%line(  0.0,  95.0, 1)    
    
        call fig1%line(-34.0,  55.0, 0)
        call fig1%line(-34.0,  76.0, 1)
        call fig1%line(-31.0,  82.0, 1)
        call fig1%line(-26.0,  87.0, 1)
        call fig1%line(-21.0,  91.0, 1)
        call fig1%line(-15.0,  95.0, 1)
        call fig1%line(  0.0,  95.0, 1)    
    
        ! nose
        call fig1%line( -5.0, 41.0, 0)
        call fig1%line( -4.0, 40.0, 1)
        call fig1%line(  0.0, 40.0, 1)
        call fig1%line(  2.0, 42.0, 1)
        call fig1%line(  3.0, 42.0, 1)
        call fig1%line(  5.5, 37.0, 1)
        call fig1%line(  5.0, 37.0, 1)
        call fig1%line(  4.0, 38.5, 1)
        call fig1%line(  0.5, 38.5, 1)
        call fig1%line(  0.0, 37.0, 1)
        call fig1%line( -2.0, 37.0, 1)
        call fig1%line( -3.0, 38.5, 1)
        call fig1%line( -7.0, 38.5, 1)
        call fig1%line( -8.0, 37.0, 1)
        call fig1%line( -8.5, 40.0, 1)
        call fig1%line( -4.0, 45.0, 1)
        call fig1%line( -4.0, 54.0, 1)
        call fig1%line( -5.0, 55.0, 1)    
    
        call fig1%line( -6.0, 53.0, 0)
        call fig1%line( -5.0, 53.0, 1)
        call fig1%line( -5.0, 47.0, 1)
        call fig1%line( -7.5, 46.0, 1)
        call fig1%line( -6.0, 53.0, 1)    
    
        ! left eye
        call fig1%line(-24.0, 55.0, 0)
        call fig1%line(-22.0, 53.0, 1)
        call fig1%line(-17.0, 54.5, 1)
        call fig1%line( -8.0, 55.0, 1)
        call fig1%line( -7.0, 55.5, 1)
        call fig1%line( -8.5, 56.5, 1)
        call fig1%line(-24.0, 55.0, 1)    
    
        call fig1%line( -8.0, 54.5, 0)
        call fig1%line(-12.0, 52.5, 1)    
    
        call fig1%line(-23.0, 56.0, 0)
        call fig1%line(-21.5, 57.0, 1)
        call fig1%line(-10.0, 58.0, 1)
        call fig1%line( -9.0, 57.0, 1)    
    
        ! left eyebrow
        call fig1%line(-27.5, 56.5, 0)
        call fig1%line(-24.0, 59.0, 1)
        call fig1%line(-11.0, 59.5, 1)
        call fig1%line( -7.0, 61.0, 1)
        call fig1%line( -4.0, 65.0, 1)
        call fig1%line( -9.0, 63.0, 1)
        call fig1%line(-25.0, 62.0, 1)
        call fig1%line(-27.5, 56.5, 1)    
    
        ! right eyebrow
        call fig1%line( 27.5, 56.5, 0)
        call fig1%line( 24.0, 59.0, 1)
        call fig1%line( 11.0, 59.5, 1)
        call fig1%line(  7.0, 61.0, 1)
        call fig1%line(  4.0, 65.0, 1)
        call fig1%line(  9.0, 63.0, 1)
        call fig1%line( 25.0, 62.0, 1)
        call fig1%line( 27.5, 56.5, 1)    
    
        ! right eye
        call fig1%line( 19.0, 53.0, 0)
        call fig1%line( 23.0, 55.0, 1)
        call fig1%line( 16.0, 55.0, 1)
        call fig1%line(  9.0, 56.0, 1)
        call fig1%line(  9.5, 55.0, 1)
        call fig1%line( 19.0, 53.0, 1)    
    
        call fig1%line(  9.0, 58.0, 0)
        call fig1%line( 12.0, 58.0, 1)
        call fig1%line( 19.0, 56.0, 1)
        call fig1%line( 21.5, 56.0, 1)    
    
        call fig1%line(  0.0, 29.0, 0)
        call fig1%line(  5.0, 29.0, 1)
        call fig1%line( 11.0, 27.0, 1)
        call fig1%line(  6.0, 32.0, 1)
        call fig1%line(  0.0, 30.0, 1)
        call fig1%line( -6.0, 32.0, 1)
        call fig1%line(-11.0, 27.0, 1)
        call fig1%line( -5.0, 29.0, 1)
        call fig1%line(  0.0, 29.0, 1)    
    
        call fig1%line(-6.5, 21.5, 0)
        call fig1%line(-3.5, 20.0, 1)
        call fig1%line( 3.5, 20.0, 1)
        call fig1%line( 6.5, 21.5, 1)
        call fig1%line(-5.0, 21.0, 0)
        call fig1%line( 5.0, 21.0, 1)
        call fig1%show()
    end block Abe_san

contains

    subroutine laplace(v) 
        real, intent(out) :: v(-50:50, -50:50)
        logical :: mask(lbound(v, 1):ubound(v, 1), lbound(v, 2):ubound(v, 2))
        integer :: i, j, iter
        mask = .true.
        mask(-10, -25:25) = .false. ! static voltage
        mask( 10, -25:25) = .false. 
        v = 0.0
        v(-10, -25:25) =  10.0 
        v( 10, -25:25) = -10.0
      !
        do iter = 1, 200 
            forall (i = lbound(v, 1) + 1:ubound(v, 1) - 1, j = lbound(v, 2) + 1:ubound(v, 2) - 1, mask(i, j)) 
                v(i, j) = 0.25 * ( v(i - 1, j) + v(i + 1, j) + v(i, j - 1) + v(i, j + 1) )
            end forall
        end do   
    end subroutine laplace    
end program check
