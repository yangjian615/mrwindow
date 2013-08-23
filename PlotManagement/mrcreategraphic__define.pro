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
;   type of graphic via a method call.
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
;       ADD:                in, optional, type=boolean, default=0
;                           Add the arrow object to the container. This assumes that
;                               the IDL_Container class or MrIDL_Container class is also a
;                               subclass.
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the overplot to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weArrow__define or cgArrow is also
;                               accepted for keyword inheritance.
;-
function MrCreateGraphic::Arrow, x0, y0, x1, y1, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, obj_new()
    endif

    ;Create a cgOverPlot object
    theArrow = obj_new('weArrow', x0, y0, x1, y1, _STRICT_EXTRA=extra)

    ;Add the overplot to the array of overplots
    if keyword_set(add) then self -> Add, theArrow
    
    ;Re-Draw
    if keyword_set(draw) then self -> Draw
    
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
;       ADD:                in, optional, type=boolean, default=0
;                           Add the axis object to the container. This assumes that
;                               the IDL_Container class or MrIDL_Container class is also a
;                               subclass.
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the overplot to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weOverPlot__define or cgOverPlot__define
;
;   
;-
function MrCreateGraphic::Axis, xloc, yloc, zloc, $
ADD = add, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, obj_new()
    endif

    ;Create a cgOverPlot object
    theAxis = obj_new('weAxis', xloc, yloc, zloc, _STRICT_EXTRA=extra)

    ;Add the overplot to the array of overplots
    if keyword_set(add) then self -> Add, theAxis
    
    ;Re-Draw
    if keyword_set(draw) then self -> Draw
    
    return, theAxis
end


;+
;   Create a weColorBar object. It can be drawn to the display and/or
;   added to the container
;
; :Keywords:
;       ADD:                in, optional, type=boolean, default=0
;                           Add the colorbar object to the container. This assumes that
;                               the IDL_Container class or MrIDL_Container class is also a
;                               subclass.
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after creating the colorbar.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrImagePlot__define.
;
; :Returns:
;       THECOLORBAR:        out, required, type=object
;                           An object reference to the colorbar.
;   
;-
function MrCreateGraphic::ColorBar, $
ADD = add, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, obj_new()
    endif

    ;Set Defaults
    add = keyword_set(add)
    draw = keyword_set(draw)

    ;Create the color bar
    theColorBar = obj_new('weColorBar', _STRICT_EXTRA=extra)
    
    ;Add the image
    if keyword_set(add) then self -> Add, theColorBar
    
    ;Draw    
    if keyword_set(draw) then self -> Draw
    
    return, theColorBar
end


;+
;   Create a MrContour object. It can be drawn to the display and/or
;   added to the container
;
; :Keywords:
;       ADD:                in, optional, type=boolean, default=0
;                           Add the coutour object to the container. This assumes that the
;                               IDL_Container class or MrIDL_Container class is also a
;                               subclass.
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the image to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrImagePlot__define.
;
; :Returns:
;       THECONTOUR:         out, required, type=object
;                           An object reference to the contour.
;   
;-
function MrCreateGraphic::Contour, data, x, y, $
ADD = add, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, obj_new()
    endif

    ;Set Defaults
    add = keyword_set(add)
    draw = keyword_set(draw)

    ;Create the color bar
    theContour = obj_new('MrContour', data, x, y, _STRICT_EXTRA=extra)
    
    ;Add the image
    if keyword_set(add) then self -> Add, theContour
    
    ;Draw    
    if keyword_set(draw) then self -> Draw
    
    return, theContour
end


;+
;   Create a MrImagePlot object. It can be drawn to the display and/or
;   added to the container
;
; :Keywords:
;       ADD:                in, optional, type=boolean, default=0
;                           Add the image object to the container. This assumes that the
;                               IDL_Container class or MrIDL_Container class is also a
;                               subclass.
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the image to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrImagePlot__define.
;
; :Returns:
;       THEIMAGE:           out, required, type=object
;                           An object reference to the image.
;   
;-
function MrCreateGraphic::Image, image, x, y, $
ADD = add, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, obj_new()
    endif

    ;Set Defaults
    add = keyword_set(add)
    draw = keyword_set(draw)

    ;Create the color bar
    theImage = obj_new('MrImagePlot', image, x, y, _STRICT_EXTRA=extra)
    
    ;Add the image
    if keyword_set(add) then self -> Add, theImage
    
    ;Draw    
    if keyword_set(draw) then self -> Draw
    
    return, theImage
end


;+
;   Create a cgLegendItem object. It can be drawn to the display and/or
;   added to the container
;
; :Keywords:
;       ADD:                in, optional, type=boolean, default=0
;                           Add the legend object to the container. This assumes that the
;                               IDL_Container class or MrIDL_Container class is also a
;                               subclass.
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the legend to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weLegendItem.
;-
function MrCreateGraphic::Legend, $
ADD = add, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, obj_new()
    endif
    
    ;Create the legend
    theLegend = obj_new('weLegendItem', _STRICT_EXTRA=extra)
    
    ;Add the legend?
    if keyword_set(add) then self -> Add, theLegend
    
    ;Draw?
    if keyword_set(draw) then self -> Draw
    
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
;       ADD:                in, optional, type=boolean, default=0
;                           Add the overplot object to the container. This assumes that
;                               the IDL_Container class or MrIDL_Container class is also a
;                               subclass.
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the overplot to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by weOverPlot__define or cgOverPlot__define
;
;   
;-
function MrCreateGraphic::OverPlot, x, y, $
ADD = add, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, obj_new()
    endif

    ;Create a cgOverPlot object
    theOverplot = obj_new('weOverPlot', x, y, _STRICT_EXTRA=extra)

    ;Add the overplot to the array of overplots
    if keyword_set(add) then self -> Add, theOverplot
    
    ;Re-Draw
    if keyword_set(draw) then self -> Draw
    
    return, theOverplot
end


;+
;   Create a MrPlotObject object. It can be drawn to the display and/or
;   added to the container
;
; :Keywords:
;       ADD:                in, optional, type=boolean, default=0
;                           Add the plot object to the container. This assumes that the
;                               IDL_Container class or MrIDL_Container class is also a
;                               subclass.
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the plot to the list.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrPlotObject__define.
;
; :Returns:
;       THEPLOT:            out, required, type=object
;                           An object reference to the plot.
;   
;-
function MrCreateGraphic::Plot, x, y, $
ADD = add, $
DRAW = draw, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, obj_new()
    endif

    ;Set Defaults
    add = keyword_set(add)
    draw = keyword_set(draw)

    ;Create the plot
    thePlot = obj_new('MrPlotObject', x, y, _STRICT_EXTRA=extra)
    
    ;Add the plot
    if keyword_set(add) then self -> Add, thePlot
    
    ;Draw    
    if keyword_set(draw) then self -> Draw
    
    return, thePlot
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
;       ADD:                in, optional, type=boolean, default=0
;                           Add the plot object to the container. This assumes that the
;                               IDL_Container class or MrIDL_Container class is also a
;                               subclass.
;       DRAW:               in, optional, type=boolean, default=0
;                           Call the Draw method after adding the overplot to the list.
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
ADD = add, $
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
        void = error_message()
        return, obj_new()
    endif
    
    ;If PLACE is set, then no location was given. Shuffle
    if keyword_set(place) then text = temporary(xloc)

    ;Create a cgOverPlot object
    theText = obj_new('weText', xloc, yloc, text, PLACE=place, WIDTH=width, OUTLOC=outloc, $
                      _STRICT_EXTRA=extra)

    ;Add the text to the array of text objects
    if keyword_set(add) then self -> Add, theText
    
    ;Re-Draw
    if keyword_set(draw) then self -> Draw
    
    return, theText
end


;+
;   Clean up after the object is destroy
;-
pro MrCreateGraphic::cleanup
    compile_opt idl2
    
    ;Destroy all weLegendItem objects
    if ptr_valid(self.plotObjects) then begin
        for i = 0, n_elements(*self.plotObjects)-1 do begin
            obj_destroy, (*self.plotObjects)[i]
        endfor
        ptr_free, self.plotObjects
    endif
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
        void = error_message()
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
             plotObjects: ptr_new()}
end