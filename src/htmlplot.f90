module htmlplot
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
        associate (iw => fig%iw, title => fig%fn)
            open(newunit = iw, file = trim(title) // '.html')
            write(iw, '(a)') '<!DOCTYPE html>'
            write(iw, '(a)') '<html>'
            write(iw, '(a)') '<head>'
            write(iw, '(a)') '<meta charset="Shift_JIS">'
            write(iw, '(3a)') '<title>', trim(title), '</title>'
            write(iw, '(a)')  '<script type="text/javascript">'
            write(iw, '(a)') '<!--'
            write(iw, '(a)') 'function plotter() {'
            write(iw, '(3a)') "var canvas = document.getElementById('", trim(title), "');"
            write(iw, '(a)') "var context = canvas.getContext('2d');"
            write(iw, '(a)') '//'
            write(iw, '(a)') 'context.scale(1, 1);'
            write(iw, '(a)') 'context.lineWidth = 1;'                 ! pen default
            write(iw, '(a)') "context.strokeStyle = 'rgb(0, 0, 0)';"  ! pen default
            write(iw, '(a)') 'context.lineCap = "butt";'
            write(iw, '(a)') 'context.beginPath();'
        end associate
    end subroutine init

    subroutine filename(fig, fn)
        class(fig_t), intent(in out) :: fig
        character(*), intent(in) :: fn
        fig%fn = fn
    end subroutine filename

    subroutine off(fig)
        type(fig_t), intent(in) :: fig
        associate (iw => fig%iw, title => fig%fn, nx => fig%nx, ny => fig%ny)
            write(iw, '(a)') 'context.stroke();'
            write(iw, '(a)') '}'
            write(iw, '(a)') '//-->'
            write(iw, '(a)') '</script>'
            write(iw, '(a)') '</head>'
            write(iw, '(a)') '<body onLoad="plotter()">'
            write(iw, '(3a, i6, a, i6, a)') '<canvas id="', trim(title) , '" width="', nx, '" height="', ny, '">'
            write(iw, '(a)') '</canvas>'
            write(iw, '(a)') '</body>'
            write(iw, '(a)') '</html>' 
            close(iw)
        end associate
    end subroutine off

    subroutine point(fig, ix, iy)
        class(fig_t), intent(in out) :: fig 
        integer, intent(in) :: ix, iy
        write(fig%iw, '(a, i7, a, i7, a)') 'context.fillRect(', ix, ',', iy, ', 0.5, 0.5);'
    end subroutine point
 
    subroutine show(fig)
        class(fig_t), intent(in) :: fig
        write(fig%iw, '(a)') 'context.stroke();'
        write(fig%iw, '(a)') 'context.beginPath();'
    end subroutine show

    subroutine line0(fig, ix0, iy0, ix1, iy1)
        class(device_t), intent(in out) :: fig
        integer, intent(in) :: ix0, iy0, ix1, iy1
        select type (fig)
        type is (fig_t)
            write(fig%iw, '(a, i7, a, i7, a)') 'context.moveTo(', ix0, ',', iy0, ');'
            write(fig%iw, '(a, i7, a, i7, a)') 'context.lineTo(', ix1, ',', iy1, ');'
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
                write(fig%iw, '(a, f10.3, a, f10.3, a)') 'context.lineTo(', fx * x + xn, ',', -fy * y + yn, ');'
            else 
                write(fig%iw, '(a, f10.3, a, f10.3, a)') 'context.moveTo(', fx * x + xn, ',', -fy * y + yn, ');'
            end if 
        end select  
    end subroutine line

  end module htmlplot    