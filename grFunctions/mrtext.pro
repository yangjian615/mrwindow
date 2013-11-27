; docformat = 'rst'
;
; NAME:
;       MrText
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
;   Create a weText object.
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
;       CURRENT:            in, optional, type=boolean, default=0
;                           If set, the plot will be added to the current MrWindow
;                               graphics window.
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
;                           Any keyword accepted by weText__define.
;
; :Returns:
;       THETEXT:            out, required, type=object
;                           A weText object reference.
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
;       2013/11/27  -   Written by Matthew Argall.
;-
;*****************************************************************************************
function MrText, xloc, yloc, text, $
 CURRENT=current, $
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

    ;Add to the current window?
    current = keyword_set(current)

    ;If PLACE is set, then no location was given. Shuffle
    if keyword_set(place) then text = temporary(xloc)

    ;Create a cgOverPlot object
    theText = obj_new('weText', xloc, yloc, text, CURRENT=current, $
                      PLACE=place, WIDTH=width, OUTLOC=outloc, $
                      _STRICT_EXTRA=extra)

    return, theText
end