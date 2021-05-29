module device
    implicit none
    type, abstract :: device_t
        integer :: nx, ny
        procedure (device_line0), pointer, pass :: line0 => device_line0
        procedure (device_line ), pointer, pass :: line  => device_line
    contains
        procedure (device_init) , deferred, pass :: init     
        procedure (device_point), deferred, pass :: point 
        procedure (device_show) , deferred, pass :: show  
    end type device_t
    
    abstract interface
        subroutine device_init(fig)
            import :: device_t
            class(device_t), intent(in out) :: fig 
        end subroutine device_init  
        
        subroutine device_point(fig, ix, iy)
            import :: device_t
            class(device_t), intent(in out) :: fig 
            integer, intent(in) :: ix, iy
        end subroutine device_point
        
        subroutine device_show(fig)
            import :: device_t
            class(device_t), intent(in) :: fig
        end subroutine device_show    
    end interface

contains
    
    subroutine device_line0(fig, ix0, iy0, ix1, iy1)
        class(device_t), intent(in out) :: fig 
        integer, intent(in) :: ix0, iy0, ix1, iy1
        integer :: nx, ny, i
        real  :: d
        nx = ix1 - ix0
        ny = iy1 - iy0       
        if (nx == 0 .and. ny == 0) then
            call fig%point(ix0, iy0)
        else if (abs(nx) < abs(ny)) then
            d = nx / real(ny)
            do i = 0, ny, sign(1, ny) 
                call fig%point(nint(ix0 + i * d), iy0 + i)
            end do    
        else
            d = ny / real(nx)
            do i = 0, nx, sign(1, nx)
                call fig%point(ix0 + i, nint(iy0 + i * d))
            end do    
        end if
    end subroutine device_line0
   
    subroutine device_line(fig, x, y, ipen)
        class(device_t), intent(in out) :: fig
        real, intent(in) :: x, y
        integer, intent(in) :: ipen
        integer, save :: ix0 = 0, iy0 = 0 
        integer :: ix, iy
        real :: xn, yn, fx, fy
        xn = fig%nx / 2.0
        yn = fig%ny / 2.0 * 1.5
        fx = fig%nx / 150.0
        fy = fig%ny / 150.0
        ix = nint( fx * x + xn)      
        iy = nint(-fy * y + yn)
        if (ipen == 1) call fig%line0(ix0, iy0, ix, iy)
        ix0 = ix
        iy0 = iy
    end subroutine device_line
   
end module device