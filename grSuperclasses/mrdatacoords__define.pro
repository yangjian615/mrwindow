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
;       2013/11/21  -   Added the _P_CLIP, _P_T, _P_T3D, _[XYZ]_WINDOW, and _[XYZ]_REGION
;                           properties so that overplotting and 3D transforms work. - MRA
;       2014/01/26  -   Added the ApplyCoords method and now set up the data coordinate
;                           system in the INIT method. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of the method is to set-up and save the data coordinate system. To
;   do so, an invisible plotting region is created so that the ![PDXY] system variables
;   update to the proper data space.
;
; :Keywords:
;       CLIP:           in, optional, type=fltarr(4)
;                       The coordinate rectangle at which data will be clipped.
;       POSITION:       in, optional, type=fltarr(4)
;                       Position of the plot in normal coordinates.
;       XRANGE:         in, optional, type=numeric(2)
;                       Min and Max of the data range for the x-axis
;       XLOG:           in, optional, type=boolean
;                       If set, the x-axis will have a log scale.
;       YRANGE:         in, optional, type=numeric(2)
;                       Min and Max of the data range for the y-axis
;       YLOG:           in, optional, type=boolean
;                       If set, the y-axis will have a log scale.
;       ZRANGE:         in, optional, type=numeric(2)
;                       Min and Max of the data range for the z-axis
;       ZLOG:           in, optional, type=boolean
;                       If set, the z-axis will have a log scale.
;-
pro MrDataCoords::ApplyCoords, $
CLIP = clip, $
POSITION = position, $
T3D = t3d, $
XRANGE = xrange, $
XLOG = xlog, $
YRANGE = yrange, $
YLOG = ylog, $
ZRANGE = zrange, $
ZLOG = zlog
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if n_elements(newWin) gt 0 then wdelete, newWin
        void = cgErrorMsg()
        return
    endif

    ;Defaults
    if n_elements(clip)     eq 0 then clip     = self._p_clip[0:3]
    if n_elements(position) eq 0 then position = [self._x_region[0], self._y_region[0], $
                                                  self._x_region[1], self._y_region[1]]
    if n_elements(t3d)      eq 0 then t3d      = self._p_t3d
    if n_elements(xrange)   eq 0 then xrange   = self._x_crange
    if n_elements(xlog)     eq 0 then xlog     = self._x_type
    if n_elements(yrange)   eq 0 then yrange   = self._y_crange
    if n_elements(ylog)     eq 0 then ylog     = self._y_type
    if n_elements(zrange)   eq 0 then zrange   = self._z_crange
    if n_elements(zlog)     eq 0 then zlog     = self._z_type
    
    ;Create a window if one is not already open
    if !d.window eq -1 and ((!d.flags and 256) gt 0) $
        then newWin = MrGetWindow(/FREE, /PIXMAP)
    
    ;Create an invisible plot to set up the system variables
    plot, xrange, yrange, CLIP=clip, POSITION=position, T3D=t3d, /NODATA, /NOERASE, $
          XLOG=xlog, XSTYLE=5, $
          YLOG=ylog, YSTYLE=21
    
    ;Save the system variables
    self -> SaveCoords
    
    ;Destroy the window if it was created here
    if n_elements(newWin) gt 0 then wdelete, newWin
end


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
        void = cgErrorMsg()
        if n_elements(P_current) gt 0 then !P = P_current
        if n_elements(X_current) gt 0 then !X = X_current
        if n_elements(Y_current) gt 0 then !Y = Y_current
        if n_elements(Z_current) gt 0 then !Z = Z_current
        return, MrNull(-1)
    endif
    
    ;Get the current P, X, Y system variables
    P_current = !P
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
    !P = P_current
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
        void = cgErrorMsg()
        return
    endif
    
    ;Message if there is nothing to restore.
    if self._data_saved eq 0 then $
        message, 'Data coordinate system not established.'
    
    ;Restore the saved system variables required for converting coordinates
    !P.Clip   = self._p_clip
    !P.T      = self._p_T
    !P.T3D    = self._p_T3D
    !X.CRange = self._x_crange
    !X.Region = self._x_region
    !X.S      = self._x_s
    !X.Type   = self._x_type
    !X.Window = self._x_window
    !Y.CRange = self._y_crange
    !Y.Region = self._y_region
    !Y.S      = self._y_s
    !Y.Type   = self._y_type
    !Y.Window = self._y_window
    !Z.CRange = self._z_crange
    !Z.Region = self._z_region
    !Z.S      = self._z_s
    !Z.Type   = self._z_type
    !Z.Window = self._z_window
end


;+
;   Save the current system variables necessary for conveting between coordinate sytems.
;-
pro MrDataCoords::SaveCoords
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Save the sytem variables necessary for converting coordinates.
    self._d_x_vsize = !D.X_VSize
    self._d_y_vsize = !D.Y_VSize
    self._p_clip    = !P.Clip
    self._p_T       = !P.T
    self._p_T3D     = !P.T3D
    self._x_crange  = !X.CRange
    self._x_region  = !X.Region
    self._x_s       = !X.S
    self._x_type    = !X.Type
    self._x_window  = !X.Window
    self._y_crange  = !Y.CRange
    self._y_region  = !Y.Region
    self._y_s       = !Y.S
    self._y_type    = !Y.Type
    self._y_window  = !Y.Window
    self._z_crange  = !Z.CRange
    self._z_region  = !Z.Region
    self._z_s       = !Z.S
    self._z_type    = !Z.Type
    self._z_window  = !Z.Window
    
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
;
; :Keywords:
;       CLIP:           in, optional, type=fltarr(4)
;                       The coordinate rectangle at which data will be clipped.
;       POSITION:       in, optional, type=fltarr(4), default="[0,0,1,1]"
;                       Position of the plot in normal coordinates.
;       XRANGE:         in, optional, type=numeric(2), default="[0,1]"
;                       Min and Max of the data range for the x-axis
;       XLOG:           in, optional, type=boolean
;                       If set, the x-axis will have a log scale.
;       YRANGE:         in, optional, type=numeric(2), default="[0,1]"
;                       Min and Max of the data range for the y-axis
;       YLOG:           in, optional, type=boolean
;                       If set, the y-axis will have a log scale.
;       ZRANGE:         in, optional, type=numeric(2)
;                       Min and Max of the data range for the z-axis
;       ZLOG:           in, optional, type=boolean
;                       If set, the z-axis will have a log scale.
;-
function MrDataCoords::init, $
CLIP = clip, $
POSITION = position, $
T3D = t3d, $
XRANGE = xrange, $
XLOG = xlog, $
YRANGE = yrange, $
YLOG = ylog, $
ZRANGE = zrange, $
ZLOG = zlog
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Defaults
    if n_elements(position) eq 0 then position = [0,0,1,1]
    if n_elements(xrange)   eq 0 then xrange = [0, 1]
    if n_elements(yrange)   eq 0 then yrange = [0, 1]

    ;Set up the data coordinates.
    self -> ApplyCoords, POSITION=position, $
                         CLIP=clip, $
                         T3D=t3d, $
                         XRANGE=xrange, $
                         XLOG=xlog, $
                         YRANGE=yrange, $
                         YLOG=ylog, $
                         ZRANGE=zrange, $
                         ZLOG=zlog

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
;       _P_CLIP:        The !P.Clip system variable
;       _P_T:           The !P.T system variable
;       _P_T3D:         The !P.T3D sytem variable
;       _X_CRANGE:      The !X.CRange sytem variable
;       _X_CRANGE:      The !X.CRange system variable
;       _X_S:           The !X.S sytem variable
;       _X_TYPE:        The !X.Type system variable
;       _X_WINDOW:      The !X.Window system variable
;       _Y_CRANGE:      The !Y.CRange system variable
;       _Y_REGION:      The !Y.Region system variable
;       _Y_S:           The !Y.S sytem variable
;       _Y_TYPE:        The !Y.Type system variable
;       _Y_WINDOW:      The !Y.Window system variable
;       _Z_CRANGE:      The !Z.CRange sytem variable
;       _Z_REGION:      The !Z.Region system variable
;       _Z_S:           The !Z.S sytem variable
;       _Z_TYPE:        The !Z.Type system variable
;       _Z_WINDOW:      The !Z.Window system variable
;-
pro MrDataCoords__define, class
    compile_opt strictarr
    
    define = { MrDataCoords, $
               _data_saved: 0B, $
               _d_x_vsize: 0L, $
               _d_y_vsize: 0L, $
               _p_clip: [0L, 0L, 0L, 0L, 0L, 0L], $
               _p_T: dblarr(4,4), $
               _p_T3D: 0L, $
               _x_crange: [0D, 0D], $
               _x_region: [0.0, 0.0], $
               _x_s: [0D, 0D], $
               _x_type: 0L, $
               _x_window: [0.0, 0.0], $
               _y_crange: [0D, 0D], $
               _y_region: [0.0, 0.0], $
               _y_s: [0D, 0D], $
               _y_type: 0L, $
               _y_window: [0.0, 0.0], $
               _z_crange: [0D, 0D], $
               _z_region: [0.0, 0.0], $
               _z_s: [0D, 0D], $
               _z_type: 0L, $
               _z_window: [0.0, 0.0] $
             }
end