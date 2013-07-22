;--------------------------------
; Contents \\\\\\\\\\\\\\\\\\\\\\
;--------------------------------

I.      License
II.     Getting Started
III.    Display choices
IV.     Window Creation
V.      Build & Realize
VI.     Example


;--------------------------------
; LICENSE \\\\\\\\\\\\\\\\\\\\\\\
;--------------------------------

MrWindow and its components are licensed under the New BSD license.

;*****************************************************************************************
;   Copyright (c) 2013, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************

;--------------------------------
; Getting Started \\\\\\\\\\\\\\\
;--------------------------------

The first step is to let IDL know where MrWindow is. There are several options.

    1. Restore 'MrWindow.sav'
            IDL> restore, '[path_to_MrWindow_Directory]/MrWindow.sav'
    2. Compile all of the files individually
        a. Change directories to the MrWindow directory
        b. Start idl with the following command:
                idl compile_mrwindow
    3. Add the MrWindow directory and its subdirectories to the IDL path
        a. Unix:
                IDL> !path = !path + ':' + expand_path('+/[path]/MrWindow/')
        b. Windows:
                IDL> !path = !path + ';' + expand_path('+[path]\MrWindow\')
    4. Edit the system variable IDL_PATH to include the MrWindow directory
    5. Create a startup.pro file containing the lines in step (2)


;--------------------------------
; III. DISPLAY CHOICES \\\\\\\\\\
;--------------------------------

There are several ways you can choose to display your data.
    1. In a normal IDL window
    2. In a PixMap window
    3. In a MrWindow widget
    4. In your own widget

1. To display data in a normal IDL window, set CreateGUI=0 when first calling MrWindow()
    
        MyWin = MrWindow(CreateGUI=0)

2. If you want to supress graphics output, then data can be drawn in a pixmap window.
   To do so, set Display=0 when calling MrWindow(). This will automatically set
   CreateGUI=0.

        MyWin = MrWindow(DISPLAY=0)
        
3. If you want all of the capabilities associated with a MrWindow widget, then call
   MrWindow as normal.
   
        MyWin = MrWindow()

4. If you have your own widget and would like to incorporate a MrWindow Draw widget into
   it, then pass the widget ID of the parent widget to MrWindow(). Note that with the 
   proper Zoom, Cursor, etc. flags set, all of the interactive capabilities of MrWindow
   will continue to work properly in your own widget.
   
   As an example, say you want the left mouse button to box zoom, the right mouse button
   to pan zoom, the wheel to zoom in the x- and y-direction by 5% of the total range, and
   for a cross-hairs to be displayed on the plot, then call MrWindow as
        
        parent = widget_id_of_your_parent_widget
        MyWin = MrWindow(parent, CMODE=2, RMODE=4, LMODE=8, WMODE=1, ZOOMFACTOR=[0.05, 0.05])


;--------------------------------
; IV. WINDOW CREATION \\\\\\\\\\\
;--------------------------------

In order to function, MrWindow needs a pixmap window and a display window (which can also
be a pixmap window -- see Section III). Graphics are initially drawn to the pixmap window,
then are copied to the diplay window. When these windows are created depends on the
desired display choice, as outlined in Section III.

1. Pixmap
    The pixmap for all display choices is created in the INIT method.

2. Display window
    A. Normal IDL Window
        CreateGUI=0 -- If no GUI is being created then the display window will be created
                       in the INIT method along with the pixmap window.
                       
    B. In a Pixmap Window
        Display=0   -- If graphics output is being surpressed, then a pixmap window will
                       be created in the INIT method along with the pixmap window
                       
    C. In a MrWindow Widget
        For draw widgets, the window associated with the draw widget is not created until
        the widget is realized. Upon realization, the Notify_Realize method is called and
        the window ID associated with the draw window is retrieved and assigned to the
        object's winID property.
        
        Here, there are some subtleties. For explicit details see Section V. on building
        and realizing the widget. The jist is that you can choose not to build or realize
        the widget. Without realizing the widget, no window ID is assigned to the object
        property, so screen captures are not possible. As a result, a temporary pixmap
        window is created that exists until the widget is realized, at which point, it
        is deleted inside the Notify_Realize method.
        
        Note that in order to do a screen capture, the graphics must first be drawn to
        a window. Then, the contents of the window are read with the TVRD procedure.
        
    D. In your own widget
        See C, first paragraph.


;--------------------------------
; V. BUILD & REALIZE \\\\\\\\\\\\
;--------------------------------

Building the widget and realizing the widget are handled independently and you can opt
not to do them.

    MyWin = MrWindow(BUILD=0)
    MyWin = MrWindow(REALIZE=0)
    
Setting BUILD=0 automatically sets REALIZE=0. One reason for this is so that the Build
method could be overriden. It allows for a custom window layout. Another reason is so
that, if the window is closed, it may be reopened again without having to redo everything.
This is especially important if you create an IDL save file containing the object. Once
restored, one can call the RealizeGUI method to display the plot again.


;--------------------------------
; VI. EXAMPLES \\\\\\\\\\\\\\\\\\
;--------------------------------

;
; The following example shows how to:
;   - Create a window using MrWindow
;   - Use the "Plot" method
;   - Determine which objects are contained within the window
;   - Set properties of the window and of the objects contained within it
;   - Bind axis ranges of different objects for zooming purposes
;   - Use the "Image" method
;   - Use the "LOCATE" keyword to place plots anywhere within a 2D grid
;   - Add an instance of /any/ plot or image class.
;

; 1. Creating an empty MrPlotWindow

    myWindow = MrWindow()

; 2. Plot a sine wave

    x = findgen(101)/100
    y = sin(2*!pi*x)
    myWindow -> Plot, x, y, TITLE='Sin(x)', YTITLE='Amplitude', XTITLE='Time (s)'

; 3. Add a cosine wave with the wrong title to the plot.

    x = findgen(101)/100
    y = cos(2*!pi*x)
    myWindow -> Plot, x, y, TITLE='Oops! Wrong Title!', YTITLE='Amplitude', XTITLE='Time (s)'
    
; 4. Change the title and other properties of the cosine plot

    ;First we need to know the index at which the plot is stored.
    myWindow -> whichObjects
    
    ;It is at index 1 and it is a plot
    myWindow -> SetProperty, 1, /PLOT, TITLE='Cos(x)', XRANGE=[0.25, 0.75], XSTYLE=1, /DRAW

; 5. Get the object references for each plot and bind their x-axes together so that zoom
;    events for one plot apply to all of the bound plots.
   
    ;Get the object reference for each (they are at indices 0 and 1)
    myWindow -> GetProperty, 0, /PLOT, OREF=oSin
    myWindow -> GetProperty, 1, /PLOT, OREF=oCos
    myWindow -> Bind, oSin, oCos, /XAXIS
    
    ;Now select an option from the "Zoom" menu and try zooming in the X-direction.

; 6. Add an image with axes to column 2, row 1

    x = findgen(256)
    y = findgen(256)
    image = dist(256)
    myWindow -> Image, image, x, y, LOCATION=[2,1], CTINDEX=17, /SCALE, /IMAXES, $
                                    TITLE='Dist(256)', XTITLE='X Title', YTITLE='Y Title'
    
; 7. Increase the x-margin to make room for a colorbar, decrease the x-gap between plots

    myWindow -> SetProperty, XMARGIN=[10,15], XGAP=8, YGAP=6, /DRAW

; 8. Add a colorbar to the right of the image

    myWindow -> Colorbar, [2,1], CTINDEX=17, RANGE=[min(image), max(image)], /RIGHT, /DRAW

; 9. Bind the colorbar to the image

    ;Figure out the indices at which the objects are located
    self -> whichObjects
    
    ;Get their object references and bind them
    myWindow -> GetProperty, 0, /IMAGE, OREF=oImage
    myWindow -> GetProperty, 0, /COLORBAR, OREF=oCB
    myWindow -> Bind, oImage, oCB, /CAXIS
    
    ;Turn on "Focus" from the "Cursor" menu
    ;Turn on "Wheel Zoom: Color" from the "Zoom | Wheel Zoom" menu
    ;Click on the image
    ;Make a scroll event with the mouse wheel
    
; 10. Add a plot using any object, not with the Plot method

    x = findgen(101)/100
    y = x^2
    myPlot = obj_new('MrPlotObject', x, y, TITLE='y = x^2', XTITLE=x, YTITLE='y', DRAW=0)
    myWindow -> addPlots, myPlot, /DRAW
    
    