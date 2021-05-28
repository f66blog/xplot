module uniplot
    use device
    implicit none
    private
    public :: fig_t
    type, extends(device_t) :: fig_t
        private
        integer, allocatable :: array(:, :)
    contains
        procedure :: init  
        procedure :: point 
        procedure :: show  
        procedure :: line0
        procedure :: line
    end type fig_t
contains
    subroutine init(fig)
        class(fig_t), intent(in out) :: fig
        allocate(fig%array(0:(fig%nx+1)/2, 0:(fig%ny+3)/4) )
    end subroutine init  
  
    subroutine point(fig, ix, iy)
        class(fig_t), intent(in out) :: fig 
        integer, intent(in) :: ix, iy
        integer :: iax, iay
        iax = ix / 2
        iay = iy / 4
        ! clipping
        if (0<=ix .and. ix<fig%nx .and. 0<=iy .and. iy<fig%ny) then
            fig%array(iax, iay) = ior(fig%array(iax, iay), icode(mod(ix, 2), mod(iy, 4)))
        end if     
    end subroutine point
  
    pure elemental integer function icode(kx, ky)
        integer, intent(in) :: kx, ky
        if (ky == 3) then
            icode = 64 + 64 * kx
        else ! 0, 1, 2
            icode = 2**(ky + 3*kx)  
        end if
    end function icode
  
    subroutine line0(fig, ix0, iy0, ix1, iy1)
        class(fig_t), intent(in out) :: fig 
        integer, intent(in) :: ix0, iy0, ix1, iy1
        integer :: i, ix, iy, nx, ny
        real :: d
        nx = ix1 - ix0
        ny = iy1 - iy0
        if (nx == 0 .and. ny ==0) then
            call fig%point(ix, iy) 
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
    end subroutine line0
  
    subroutine show(fig)
        class(fig_t), intent(in) :: fig 
        integer :: iy
        do iy = 0, ubound(fig%array, 2)
            print '(*(a))', reverse_endian(shift_code(fig%array(:, iy)))
        end do
    end subroutine show    
  
    pure elemental integer function shift_code(k)
        integer, intent(in) :: k
        integer, parameter :: n0 = Z'E2A080' !14852224
        shift_code = n0 + 256 * (k /64) + mod(k, 64)  !E2A180, E2A280, E2A380      
    end function shift_code    
    
    pure elemental character(len = 4) function reverse_endian(i)
        integer, intent(in) :: i
        character:: tmp(4)
        tmp = transfer(i, ' ', size = 4)
        reverse_endian = transfer(tmp(4:1:-1), '    ')  !array 4 to len 4
    end function reverse_endian
    
    
    subroutine line(fig, x, y, ipen)
        class(fig_t), intent(in out) :: fig
        real, intent(in) :: x, y
        integer, intent(in) :: ipen
        integer, save :: ix0 = 0, iy0 = 0 
        integer :: ix, iy
        real, parameter :: xn = 80.0, yn = 100.0, fx = 1.0, fy = 0.85
        ix = nint( fx * x + xn)      
        iy = nint(-fy * y + yn)
        if (ipen == 1) call fig%line0(ix0, iy0, ix, iy)
        ix0 = ix
        iy0 = iy
    end subroutine line
  
end module uniplot