module psplot
    use device
    implicit none
    private
    public :: fig_t
    type, extends(device_t) :: fig_t
        private
        character(len = :), allocatable, public :: fn
        integer, allocatable :: iw 
    contains 
        procedure :: init  
        procedure :: point 
        procedure :: show
        procedure :: filename
        final :: off  
    end type fig_t
contains

    subroutine init(fig)
        class(fig_t), intent(in out) :: fig
        allocate(fig%iw)
        fig%line0 => line0
        fig%line  => line
        if (.not. allocated(fig%fn)) fig%fn = 'figure'
        associate (iw => fig%iw, fn => fig%fn)
            open(newunit = iw, file = trim(fn) // '.ps')
            write(iw, '(a)') '%!PS-Adobe-3.0 EPSF-3.0'
            write(iw, '(a, 2i8)') '%%BoundingBox: 0 0 ', fig%nx, fig%ny
            write(iw, '(2a)') '%%Title: ', trim(fn)
            write(iw, '(a)') '%%EndComments'
            write(iw, '(a)') 'gsave'
            write(iw, '(a)') '1 1 scale'    !'0.8 0.8 scale'
            write(iw, '(a)') '1 setlinewidth'
            write(iw, '(a)') '0.0 0.0 0.0 setrgbcolor'
            write(iw, '(a)') '2 setlinejoin'
            write(iw, '(a, i8, a)') '0 ', fig%ny, ' translate'
            write(iw, '(a)') 'newpath'
        end associate
    end subroutine init

    subroutine filename(fig, fn)
        class(fig_t), intent(in out) :: fig
        character(*), intent(in) :: fn
        fig%fn = fn
    end subroutine filename

    subroutine off(fig)
        type(fig_t), intent(in) :: fig
        write(fig%iw, '(a)') 'stroke'
        write(fig%iw, '(a)') 'showpage'
        write(fig%iw, '(a)') 'grestore'
        write(fig%iw, '(a)') '%%EOF'
        close(fig%iw)
    end subroutine off

    subroutine point(fig, ix, iy)
        class(fig_t), intent(in out) :: fig 
        integer, intent(in) :: ix, iy
        write(fig%iw, '(a)') 'newpath'
        write(fig%iw, '(*(g0))') ix, ' ', -iy, ' 0.5 0 360 arc fill'
    end subroutine point
 
    subroutine show(fig)
        class(fig_t), intent(in) :: fig
        write(fig%iw, '(a)') 'stroke'
        write(fig%iw, '(a)') 'newpath'
    end subroutine show

    subroutine line0(fig, ix0, iy0, ix1, iy1)
        class(device_t), intent(in out) :: fig
        integer, intent(in) :: ix0, iy0, ix1, iy1
        select type (fig)
        type is (fig_t)
            write(fig%iw, '(2i7, a)') ix0, -iy0, ' moveto'
            write(fig%iw, '(2i7, a)') ix1, -iy1, ' lineto'
        end select
      end subroutine line0

    subroutine line(fig, x, y, ipen)
        class(device_t), intent(in out) :: fig
        real, intent(in) :: x, y
        integer, intent(in) :: ipen
        real :: xn, yn, fx, fy
        select type (fig)
        type is (fig_t)
            xn = fig%nx / 2.0
            yn = fig%ny / 2.0 * 1.5
            fx = fig%nx / 150.0
            fy = fig%ny / 150.0
            if (ipen == 1) then 
                write(fig%iw, '(2f10.3, a)') fx * x + xn, fy * y - yn, ' lineto'
            else 
                write(fig%iw, '(2f10.3, a)') fx * x + xn, fy * y - yn, ' moveto'
            end if 
        end select  
    end subroutine line

  end module psplot    