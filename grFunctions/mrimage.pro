; docformat = 'rst'
;
; NAME:
;       MrImage
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
;   Create a MrImage object.
;
; :Params:
;       IMAGE:          in, required, type=NxM numeric array
;                       Image to be displayed
;       X:              in, optional, type=scalar, N-elements numeric
;                       If a scalar, then positioning is that of IDL's TV function. If a
;                           vector the same size as the first dimension of `IMAGE`, then
;                           the data coordinates of each pixel. See also, `PAINT`.
;       Y:              in, optional, type=scalar, N-elements numeric
;                       If a scalar, then positioning is that of IDL's TV function. If a
;                           vector the same size as the second dimension of `IMAGE`, then
;                           the data coordinates of each pixel. See also, `PAINT`.
;       X0:             in, optional, type=1D vector/2D array
;                       The x-coordinate, in data coordinates, of the upper-right corner
;                           if each pixel in `IMAGE`. If supplied, then `X` markes the
;                           x-coordinate of the lower-left corner of each pixel in `IMAGE`.
;                           Must be used with `Y0`. If given, `PAINT` will be set to 1.
;       Y0:             in, optional, type=1D vector/2D array
;                       The y-coordinate, in data coordinates, of the upper-right corner
;                           if each pixel in `IMAGE`. If supplied, then `Y` markes the
;                           y-coordinate of the lower-left corner of each pixel in `IMAGE`.
;                           Must be used with `X0`. If given, `PAINT` will be set to 1.
;       X1:             in, optional, type=scalar/1D vector/2D array
;                       If provided, then `X` and `Y` are the "center" locations of each
;                           pixel of `IMAGE`. `X0` and X1 mark the displacement to the
;                           left and right edges of each pixel, respectively. X1 and `Y1`
;                           must be supplied together. If provided, `PAINT` will be set.
;       Y1:             in, optional, type=scalar/1D vector/2D array
;                       If provided, then `X` and `Y` are the "center" locations of each
;                           pixel of `IMAGE`. `Y0` and Y1 mark the displacement to the
;                           bottom and top edges of each pixel, respectively. `X1` and Y1
;                           must be supplied together. If provided, `PAINT` will be set.
;
; :Keywords:
;       CURRENT:            in, optional, type=boolean, default=0
;                           If set, the plot will be added to the current MrWindow
;                               graphics window.
;       _REF_EXTRA:         in, optional, type=structure
;                           Any keyword accepted by MrImage__define.
;
; :Returns:
;       THEIMAGE:           out, required, type=object
;                           A MrImage object reference.
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
function MrImage, image, x, y, $
 CURRENT=current, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, obj_new()
    endif

    ;Create the color bar
    theImage = obj_new('MrImage', image, x, y, CURRENT=keyword_set(current), _STRICT_EXTRA=extra)
    
    return, theImage
end
