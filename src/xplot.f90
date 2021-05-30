module xplot
    use, intrinsic :: iso_c_binding
    use device
    implicit none
    private
    public :: fig_t

    interface
        subroutine Xopen(nx, ny) bind(c, name = 'X_open')
            use, intrinsic :: iso_c_binding , only : c_int
            integer(c_int), value :: nx, ny
        end subroutine Xopen

        subroutine Xpoint(ix, iy) bind(c, name = 'X_point')
            use, intrinsic :: iso_c_binding, only : c_int
            integer(c_int), value :: ix, iy
        end subroutine Xpoint

        subroutine Xclose() bind(c, name = 'X_close')
        end subroutine Xclose

        subroutine Xflush() bind(c, name = 'X_flush')
        end subroutine Xflush
    end interface

    type, extends(device_t) :: fig_t
        private
    contains
        procedure :: init     
        procedure :: point 
        procedure :: show  
        final :: x_close
    end type fig_t

contains

    subroutine init(fig, nx, ny, title)
        class(fig_t), intent(in out) :: fig
        integer, intent(in) :: nx, ny
        character(len = *), intent(in) :: title
        fig%nx = nx
        fig%ny = ny
        fig%title = title
        call Xopen(int(fig%nx, c_int), int(fig%ny, c_int))  
        call sleep(1)  ! non-standard : sleeps 1 sec
    end subroutine init  

    subroutine point(fig, ix, iy)
        class(fig_t), intent(in out) :: fig 
        integer, intent(in) :: ix, iy
        call Xpoint(int(ix, c_int), int(iy, c_int))
    end subroutine point

    subroutine show(fig)
       class(fig_t), intent(in) :: fig
       call XFlush() 
    end subroutine show    

    subroutine x_close(fig)
        type(fig_t), intent(in out) :: fig
        print *, 'press ENTER to continue'
        read *
        call Xclose()
    end subroutine x_close

end module xplot
