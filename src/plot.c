#include <X11/Xlib.h>
#include <X11/Xutil.h>
static Display* d;
static Window   w;
static GC       gc;
unsigned long white, black;

void X_open(int nx, int ny){
    // Open a display
    d = XOpenDisplay(0);
    if ( !d ) return;
    //
    white = WhitePixel(d, DefaultScreen(d));
    black = BlackPixel(d, DefaultScreen(d));
    // Create a window
    w = XCreateSimpleWindow(d, DefaultRootWindow(d), 0, 0, nx, ny, 0, black, white);
    XMapWindow(d, w);
    gc = XCreateGC(d, w, 0, 0);
    XFlush(d);
}

void X_point(int ix, int iy){
     XDrawPoint(d, w, gc, ix, iy);
     XFlush(d);
}

void X_flush(){
     XFlush(d);
}

void X_close(void){
    XFreeGC(d, gc);
    XDestroyWindow(d, w);
    XFlush(d);
    XCloseDisplay(d);
}