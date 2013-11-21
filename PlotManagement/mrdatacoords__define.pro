; docformat = 'rst'
;
; NAME:
;       MrDataCoords__Define
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
;   A data coordinate object used to convert between data, device, and normal coordinates.
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
;   Modification History::
;       2013/11/20  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Convert between data, normal, and device coordinates.
;
; :Params:
;       X:                      in, required, type=numeric scalar/array
;                               X components of the input coordinates. If only one argument
;                                   is specified, then X[0,*] represents the X-coordinates,
;                                   X[1,*] represents the Y-coordinates, and X[2,*]
;                                   represents the Z-coordinates (if present).
;       Y:                      in, optional, type=numeric scalar/array
;                               Y components of the input coordinates.
;       Z:                      in, optional, type=numeric scalar/array
;                               Z components of the input coordinates.
;
; :Keywords:
;       _REF_EXTRA:             in, optional, type=any
;                               Any keyword accepted by IDL's Convert_Coord function is
;                                   also accepted for keyword inheritance.
;-
function MrDataCoords::ConvertCoord, x, y, z, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        
        if n_elements(X_current) gt 0 then !X = X_current
        if n_elements(Y_current) gt 0 then !Y = Y_current
        if n_elements(Z_current) gt 0 then !Z = Z_current
        return, MrNull(-1)
    endif
    
    ;Get the current P, X, Y system variables
    X_current = !X
    Y_current = !Y
    Z_current = !Z
    
    ;Load the syetem variable states as they relate to this plot
    self -> RestoreCoords
    
    ;Convert coordinates
    case n_params() of
        1: coords = convert_coord(x, _STRICT_EXTRA=extra)
        2: coords = convert_coord(x, y, _STRICT_EXTRA=extra)
        3: coords = convert_coord(x, y, z, _STRICT_EXTRA=extra)
        else: message, 'Incorrect number of parameters.'
    endcase
    
    ;Reset the system variables
    !X = X_current
    !Y = Y_current
    !Z = Z_current
    
    return, coords
end


;+
;   The purpose of this method is to restore the saved system variables required
;   for converting coordinates
;-
pro MrDataCoords::RestoreCoords
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Message if there is nothing to restore.
    if self._data_saved eq 0 then message, 'No data coordinates have been saved.'
    
    ;Restore the saved system variables required for converting coordinates
    !X.CRange  = self._x_crange
    !X.S       = self._x_s
    !X.Type    = self._x_type
    !Y.CRange  = self._y_crange
    !Y.S       = self._y_s
    !Y.Type    = self._y_type
    !Z.CRange  = self._z_crange
    !Z.S       = self._z_s
    !Z.Type    = self._z_type
end


;+
;   Save the current system variables necessary for conveting between coordinate sytems.
;-
pro MrDataCoords::SaveCoords
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Save the sytem variables necessary for converting coordinates.
    self._d_x_vsize = !D.X_VSize
    self._d_y_vsize = !D.Y_VSize
    self._x_crange  = !X.CRange
    self._x_s       = !X.S
    self._x_type    = !X.Type
    self._y_crange  = !Y.CRange
    self._y_s       = !Y.S
    self._y_type    = !Y.Type
    self._z_crange  = !Z.CRange
    self._z_s       = !Z.S
    self._z_type    = !Z.Type
    
    ;Indicate that coordinates have been saved
    self._data_saved = 1B
end


;+
;   Clean up after the object is destroy
;-
pro MrDataCoords::cleanup
    ;Nothing to clean up
end


;+
;   The initialization method.
;-
function MrDataCoords::init
    ;Does not need to be initialized
    return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;
; :Fields:
;       _DATA_SAVED     Indicates that coordinates have been saved.
;       _D_X_VSIZE:     The !D.X_VSize system variable
;       _D_Y_VSIZE:     The !D.Y_VSize system variable
;       _X_CRANGE:      The !X.CRange sytem variable
;       _X_S:           The !X.S sytem variable
;       _X_TYPE:        The !X.Type system variable
;       _Y_CRANGE:      The !Y.CRange sytem variable
;       _Y_S:           The !Y.S sytem variable
;       _Y_TYPE:        The !Y.Type system variable
;       _Z_CRANGE:      The !Z.CRange sytem variable
;       _Z_S:           The !Z.S sytem variable
;       _Z_TYPE:        The !Z.Type system variable
;-
pro MrDataCoords__define, class
    compile_opt strictarr
    
    define = { MrDataCoords, $
               _data_saved: 0B, $
               _d_x_vsize: 0L, $
               _d_y_vsize: 0L, $
               _x_crange: [0D, 0D], $
               _x_s: [0D, 0D], $
               _x_type: 0L, $
               _y_crange: [0D, 0D], $
               _y_s: [0D, 0D], $
               _y_type: 0L, $
               _z_crange: [0D, 0D], $
               _z_s: [0D, 0D], $
               _z_type: 0L $
             }
end