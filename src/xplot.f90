module xplot
    implicit none
    !private
    public :: fig_t

    interface
        subroutine Xopen(nx, ny) bind(c, name = 'X_open')
            integer, value :: nx, ny
        end subroutine Xopen

        subroutine Xpoint(ix, iy) bind(c, name = 'X_point')
            integer, value :: ix, iy
        end subroutine Xpoint

        subroutine Xclose() bind(c, name = 'X_close')
        end subroutine Xclose

        subroutine Xflush() bind(c, name = 'X_flush')
        end subroutine Xflush
    end interface

    type :: fig_t
        private
        integer :: nx, ny
    contains
        procedure :: init     
        procedure :: point 
        procedure :: show  
        procedure :: line0
        procedure :: line
        final :: x_close
    end type fig_t

contains

    subroutine init(fig, nx, ny)
        class(fig_t), intent(in out) :: fig 
        integer, intent(in) :: nx, ny
        fig%nx = nx
        fig%ny = ny
        call Xopen(nx, ny)  
     !   call sleep(1)  ! non-standard
    end subroutine init  

    subroutine point(fig, ix, iy)
        class(fig_t), intent(in out) :: fig 
        integer, intent(in) :: ix, iy
        call Xpoint(ix, iy)
    end subroutine point

    subroutine show(fig)
       class(fig_t), intent(in) :: fig
       call XFlush() 
    end subroutine show    

    subroutine line0(fig, ix0, iy0, ix1, iy1)
        class(fig_t), intent(in out) :: fig 
        integer, intent(in) :: ix0, iy0, ix1, iy1
        integer :: nx, ny, i
        real  :: d
        nx = ix1 - ix0
        ny = iy1 - iy0       
        if (nx == 0 .and. ny == 0) then
            call Xpoint(ix0, iy0)
        else if (abs(nx) < abs(ny)) then
            d = nx / real(ny)
            do i = 0, ny, sign(1, ny) 
                call Xpoint(nint(ix0 + i * d), iy0 + i)
            end do    
        else
            d = ny / real(nx)
            do i = 0, nx, sign(1, nx)
                call Xpoint(ix0 + i, nint(iy0 + i * d))
            end do    
        end if
    end subroutine line0

    subroutine x_close(fig)
        type(fig_t), intent(in out) :: fig
        print *, 'press ENTER to continue'
        read *
        call Xclose()
    end subroutine x_close

  
    subroutine line(fig, x, y, ipen)
        class(fig_t), intent(in out) :: fig
        real, intent(in) :: x, y
        integer, intent(in) :: ipen
        integer, save :: ix0 = 0, iy0 = 0 
        integer :: ix, iy
        real, parameter :: xn = 250.0, yn = 600.0, fx = 4.0, fy = 4.0
        ix = nint( fx * x + xn)      
        iy = nint(-fy * y + yn)
        if (ipen == 1) call fig%line0(ix0, iy0, ix, iy)
        ix0 = ix
        iy0 = iy
    end subroutine line

end module xplot