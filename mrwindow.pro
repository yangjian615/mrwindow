; docformat = 'rst'
;
; NAME:
;       MrWindow
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
; PURPOSE:
;+
;   The purpose of this program is to create zoomable, resizeable window in which plottable
;   objects (line plots, images, colorbars, text, arrows, etc.) can be held. Zooming and
;   analytical tools are available. CDF files can be plotted, as well.
;
; :Params:
;       PARENT:             in, optional, type=int
;                           The widget ID of a parent widget in which to place the draw
;                               window. If not provided, the draw window will be placed
;                               in a MrWindow widget unless `NOGUI`=1.
;
; :Keywords:
;       ARROWS:             in, optional, type=object/objarr
;                           MrArrow object(s) to be added to the diplay window.
;       BUFFER:             in, optional, type=boolean, default=0
;                           If set, graphics will be directed to an invisible pixmap window
;                               instead of generating a widget window.
;       DRAW:               in, optional, type=boolean, default=1
;                           If set, the Draw method will be called. Widgets will be
;                               realized at this time.
;       NAME:               in, optional, type=string, default='MrWindow'
;                           Name to be given to the window. Windows can be accessed by
;                               name via the GetMrWindows function.
;       NOGUI:              in, optional, type=boolean, default=0
;                           If set, graphics will be created in a normal IDL window, unless
;                               `PARENT` is provided.
;       PLOTOBJECTS:        in, optional, type=object/objarr
;                           MrPlot object(s) to be added to the display.
;       REFRESH:            in, optional, type=boolean, default=0
;                           If set, the display will be refreshed after the window is
;                               created. This will cause the widget to be realized and
;                               the contents to be drawn.
;       SAVEDIR:            in, optional, type=string, default=current
;                           The directory in which to save files. When the display is
;                               saved via the ::saveImage method, if no filename is given,
;                               a dialog will open to this directory.
;       TEXT:               in, optional, type=object
;                           A weText object or an array of weText objects to be added
;                               to the draw window.
;       XSIZE:              in, optional, type=long, default=512
;                           The width of the draw window in pixels
;       YSIZE:              in, optional, type=long, default=512
;                           The height of the draw window in pixels
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by the superclasses is also accepted for
;                               keyword inheritance.
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
;       06/29/2013  -   Written by Matthew Argall
;       07/05/2013  -   Was accidentally calling MrWindow::Init with _REF_EXTRA instead
;                           of _EXTRA. Fixed.
;       2014/01/22  -   Removed unused TEXT, PLOTOBJECTS, and ARROWS keywords - MRA
;-
function MrWindow, parent, $
;MrWindow Keywords
DRAW = draw, $
NAME = name, $
NOGUI = noGUI, $
BUFFER = buffer, $
REFRESH = refresh, $
SAVEDIR = savedir, $
XSIZE = xsize, $
YSIZE = ysize, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Create a MrWindow object reference.
    oMrWindow = obj_new('MrWindow', parent, $
                                    DRAW = draw, $
                                    NOGUI = noGUI, $
                                    BUFFER = buffer, $
                                    REFRESH = refresh, $
                                    SAVEDIR = savedir, $
                                    XSIZE = xsize, $
                                    YSIZE = ysize, $
                                    _EXTRA = extra)
                                    
    return, oMrWindow
end