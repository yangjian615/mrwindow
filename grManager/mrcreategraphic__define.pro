; docformat = 'rst'
;
; NAME:
;       MrCreateGraphic__Define
;
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
;
;+
;   The purpose of this class is provide a means of having the capability to create any
;   type of graphic via a method call. This is an abstract method and should be inherited.
;   It also assumes that the superclass also inherits IDL_Container or MrIDL_Container.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Copyright:
;       Matthew Argall 2013
;
; :History:
;	Modification History::
;       08/15/2013  -   Written by Matthew Argall.
;       08/23/2013  -   Draw and Add by default. If not adding, call the particular
;                           object's draw method. - MRA
;       08/27/2013  -   Legend and overplots are now drawn and added by default.
;-
;*****************************************************************************************
;+
;   Create a weArrow object. It can be drawn to the display and/or
;   added to the container
;
; :Params:
;       X0:             in, required, type=integer/float
;                       The x location of the blunt end of the arrow. May be a vector.
;                           Assumes device coordinates.
;       X1:             in, required, type=integer/float
;                       The x location of the sharp end of the arrow. May be a vector.
;                           Assumes device coordinates.
;       Y0:             in, required, type=integer/float
;                       The y location of the blunt end of the arrow. May be a vector.
;                           Assumes device coordinates.
;       Y1:             in, required, type=integer/float
;                       The y location of the sharp end of the arrow. May be a vector.
;                           Assumes device coordinates.
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weArrow__define or cgArrow is also
;                               accepted for keyword inheritance.
;-
function MrCreateGraphic::Arrow, x0, y0, x1, y1, $
CURRENT=current, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Create a cgOverPlot object
    theArrow = obj_new('weArrow', x0, y0, x1, y1, /CURRENT, DRAW=draw, _STRICT_EXTRA=extra)
    
    return, theArrow
end


;+
;   Create a weAxis object. It can be drawn to the display and/or
;   added to the container
;
; :Params:
;       XLOC:               in, optional, type=depends
;                           The X location of the axis. 
;       YLOC:               in, optional, type=depends
;                           The Y location of the axis. 
;       ZLOC:               in, optional, type=depends
;                           The Z location of the axis. 
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weOverPlot__define or cgOverPlot__define
;
;   
;-
function MrCreateGraphic::Axis, xloc, yloc, zloc, $
CURRENT=current, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Create a cgOverPlot object
    theAxis = obj_new('weAxis', xloc, yloc, zloc, /CURRENT, _STRICT_EXTRA=extra)

    return, theAxis
end


;+
;   Create a weColorBar object. It can be drawn to the display and/or
;   added to the container
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrImagePlot__define.
;
; :Returns:
;       THECOLORBAR:        out, required, type=object
;                           An object reference to the colorbar.
;   
;-
function MrCreateGraphic::Colorbar, $
CURRENT=current, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Create the color bar
    theColorBar = obj_new('weColorBar', /CURRENT, _STRICT_EXTRA=extra)
    
    return, theColorBar
end


;+
;   Create a MrContour object. It can be drawn to the display and/or
;   added to the container
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrImagePlot__define.
;
; :Returns:
;       THECONTOUR:         out, required, type=object
;                           An object reference to the contour.
;   
;-
function MrCreateGraphic::Contour, data, x, y, $
CURRENT=current, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Create the color bar
    theContour = obj_new('MrContour', data, x, y, /CURRENT, _STRICT_EXTRA=extra)
    
    return, theContour
end


;+
;   Create a MrImagePlot object. It can be drawn to the display and/or
;   added to the container
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrImagePlot__define.
;
; :Returns:
;       THEIMAGE:           out, required, type=object
;                           An object reference to the image.
;   
;-
function MrCreateGraphic::Image, image, x, y, $
CURRENT=current, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Create the color bar
    theImage = obj_new('MrImage', image, x, y, /CURRENT, _STRICT_EXTRA=extra)
    
    return, theImage
end


;+
;   Create a cgLegendItem object. It can be drawn to the display and/or
;   added to the container
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weLegendItem.
;-
function MrCreateGraphic::Legend, $
CURRENT=current, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Create the legend
    theLegend = obj_new('weLegendItem', /CURRENT, _STRICT_EXTRA=extra)
    
    return, theLegend
end


;+
;   Create a cgOverPlot object. It can be drawn to the display and/or
;   added to the container
;
; :Params:
;       X:                  in, required, type=any
;                           If Y is given, a vector representing the independent variable
;                               to be plotted. If Y is not given, a vector or array of
;                               representing the dependent variable to be plotted. Each
;                               column of X will then be overplotted as individual vectors
;                               in the same set of axes.
;       Y:                  in, optional, type=any
;                           A vector or array of representing the dependent variable to be
;                               plotted. Each column of Y will then be overplotted
;                               as individual vectors in the same set of axes.
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weOverPlot__define or cgOverPlot__define
;
;   
;-
function MrCreateGraphic::OverPlot, x, y, $
CURRENT=current, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Create a cgOverPlot object
    theOverplot = obj_new('weOverPlot', x, y, /CURRENT, _STRICT_EXTRA=extra)
    
    return, theOverplot
end


;+
;   Create a MrPlotObject object. It can be drawn to the display and/or
;   added to the container
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrPlotObject__define.
;
; :Returns:
;       THEPLOT:            out, required, type=object
;                           An object reference to the plot.
;   
;-
function MrCreateGraphic::Plot, x, y, $
CURRENT=current, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Create the plot
    thePlot = obj_new('MrPlot', x, y, /CURRENT, _STRICT_EXTRA=extra)
    
    return, thePlot
end


;+
;   Create a MrPlotObject object. It can be drawn to the display and/or
;   added to the container
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrPlotObject__define.
;
; :Returns:
;       THEPLOTS:           out, required, type=object
;                           A reference to the PlotS object.
;   
;-
function MrCreateGraphic::PlotS, x, y, z, $
CURRENT=current, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;Create the plot
    thePlotS = obj_new('MrPlotS', x, y, z, /CURRENT, _STRICT_EXTRA=extra)
    
    return, thePlotS
end



;+
;   Create a weText object. It can be drawn to the display and/or
;   added to the container
;
; :Params:
;       XLOC:               in, optional, type=depends
;                           The X location of the axis. If `PLACE` is set, then this is
;                               the text to be drawn on the plot.
;       YLOC:               in, optional, type=depends
;                           The Y location of the axis.
;       TEXT:               in, optional, type=string
;                           The text to be put on the axis.
;
; :Keywords:
;       OUTLOC:             out, optional, type=fltarr(2)
;                           If `PLACE` is set, then this will return the location at which
;                               the text was placed in the window.
;       PLACE:              in, optional, type=boolean, default=0
;                           Indicate that you want to click on the plot in order to
;                               determine the location of the text. In this case, `XLOC`
;                               and `YLOC` are not given.
;       WIDTH:              out, optional, type=float
;                           A named variable into which the width of the text, in
;                               normalized units, will be returned.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weText__define or cgText is also
;                               accepted for keyword inheritance.
;-
function MrCreateGraphic::Text, xloc, yloc, text, $
CURRENT=current, $
DRAW = draw, $
OUTLOC = outloc, $
PLACE = place, $
WIDTH = width, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

    ;If PLACE is set, then no location was given. Shuffle
    if keyword_set(place) then text = temporary(xloc)

    ;Create a cgOverPlot object
    theText = obj_new('weText', xloc, yloc, text, /CURRENT, $
                      PLACE=place, WIDTH=width, OUTLOC=outloc, $
                      _STRICT_EXTRA=extra)

    return, theText
end


;+
;   Clean up after the object is destroy
;-
pro MrCreateGraphic::cleanup
    ;Nothing to cleanup
end


;+
;   The initialization method. Because MrCreateGraphic is an abstract class, it must
;   be inherited. Any attempts to instantiate a MrCreateGraphic object will result
;   in an error.
;-
function MrCreateGraphic::init
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    message, 'This is an abstract class and must be inherited.'
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrCreateGraphic__define, class
    compile_opt idl2
    
    class = {MrCreateGraphic, $
              _CreateGraphic: 0 $       ;Not used, but cannot create empty classes.
            }
end