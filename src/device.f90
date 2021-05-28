module device
    implicit none
type, abstract :: device_t
    integer :: nx, ny
contains
    procedure (device_init) , deferred, pass :: init     
    procedure (device_point), deferred, pass :: point 
    procedure (device_show) , deferred, pass :: show  
    procedure (device_line0), deferred, pass :: line0
    procedure (device_line) , deferred, pass :: line
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

subroutine device_line0(fig, ix0, iy0, ix1, iy1)
    import :: device_t
    class(device_t), intent(in out) :: fig 
    integer, intent(in) :: ix0, iy0, ix1, iy1
end subroutine device_line0

subroutine device_line(fig, x, y, ipen)
    import :: device_t
    class(device_t), intent(in out) :: fig
    real, intent(in) :: x, y
    integer, intent(in) :: ipen
end subroutine device_line
end interface

end module device