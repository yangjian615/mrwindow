; docformat = 'rst'
;
; NAME:
;       MrDistFn
;
;*****************************************************************************************
;   Copyright (c) 2015, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
;   Create a pitch angle distribution given a 2D array of differential energy flux.
;
;   Calling Sequence::
;       img = MrDistFn(data, E, PA)
;       img = MrDistFn(data, E_lower, PA_lower, E_upper, PA_upper)
;       img = MrDistFn(data, E, PA, E_Delta_Minus, PA_Delta_Minus, E_Delta_Plus, PA_Delta_Plus)
;
; :Categories:
;       Cluster
;
; :Params:
;       DATA:           in, required, type=NxM fltarr
;                       Differential energy flux (keV / cm^2-s-str-keV)
;       E:              in, required, type=N-element fltarr
;                       Energy of each energy bin (keV)
;       PA:             in, required, type=M-element fltarr
;                       Pitch angle each pitch angle bin (radians)
;       E1:             in, optional, type=float/N-elements fltarr
;                       Energy ceiling of each energy bin. If provided, then `E`
;                           represents the energy floor of each bin, such that [E, E1]
;                           defines the energy coordinates.
;       PA1:            in, required, type=float/M-element fltarr
;                       Pitch angle ceiling of each pitch angle bin (radians). If provided,
;                           then `PA` represents the pitch angle floor of each bin, such
;                           that [PA, PA1] defines the pitch-angle coordinates. The
;                           coordinates of the lower-left and upper-right corner of each
;                           pixel are (x0, y0) = [E, PA] and (x1, y1) = [E1, PA1].
;       E2:             in, optional, type=float/N-elements fltarr
;                       If provided, then `E` represents the center of each energy bin,
;                           `E1` is the change in energy required to reach the floor of
;                           each bin, and E2 is the change in energy required to reach
;                           the ceiling of each bin. The energy coordinates, then, are
;                           [E - E1, E + E2].
;       PA2:            in, optional, type=float/M-elements fltarr
;                       If provided, then `PA` represents the center of each pitch angle bin,
;                           `PA1` is the change in energy required to reach the floor of
;                           each bin, and PA2 is the change in energy required to reach
;                           the ceiling of each bin. The energy coordinates, then, are
;                           [PA - PA1, PA + PA2]. The coordinates of the lower left and
;                           upper-right corner of each pixel are (x0, y0) = [E-E1, PA-PA1]
;                           and (x1, y1) = [E+E2, PA+PA2].
;
; :Keywords:
;       CENTER:         in, optional, type=boolean, default=0
;                       If set, and only 3 parameters are given, then `E` and `PA`
;                           represent the center of each energy and pitch angle bin. The
;                           default is to assume they represent the minimum values.
;       DEGREES:        in, optional, type=boolean, default=0
;                       If set, then `PA`, `PA1`, and `PA2` are given in degrees.
;       PSD:            in, optional, type=boolean, default=0
;                       If set, then differential energy flux is converted to phase space
;                           density (cm^6 s^-3).
;       REFLECT:        in, optional, type=boolean, default=0
;                       If set, then `PA`, `PA1`, and `PA2` are reflected into the lower-
;                           half plane. This assumes that the natural range of pitch angles
;                           is [0,!pi].
;       ELOG:           in, optional, type=boolean, default=0
;                       If set, then the energy is log-scaled.
;       OVERPLOT:       in, optional, type=object
;                       A MrGraphics data object on which the distribution function
;                           should be overplotted.
;       VELOCITY:       in, optional, type=boolean, default=0
;                       If set, energy is converted to velocity.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrImage is also accepted via keyword
;                           inheritance.
;                           
;
; :Uses:
;   Uses the following external programs::
;       constants.pro
;       MrImage.pro
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2015/02/23  -   Written by Matthew Argall
;-
;*****************************************************************************************
function MrDistFn, data, E, PA, E1, PA1, E2, PA2, $
CENTER=center, $
DEGREES=degrees, $
PSD=psd, $
REFLECT=reflect, $
ELOG=elog, $
OVERPLOT=target, $
VELOCITY=velocity, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2

	;Check number of parameters given
	;   - n_params() reports the number of arg_present().
	;   - Do not count undefined inputs.
	nParams = n_elements(data)    eq 0 ? 0 $
	            : n_elements(E)   eq 0 ? 1 $
	            : n_elements(PA)  eq 0 ? 2 $
	            : n_elements(E1)  eq 0 ? 3 $
	            : n_elements(PA1) eq 0 ? 4 $
	            : n_elements(E2)  eq 0 ? 5 $
	            : n_elements(PA2) eq 0 ? 6 $
	            : 7

	;Defaults
	degrees  = keyword_set(degrees)
	psd      = keyword_set(psd)
	reflect  = keyword_set(reflect)
	velocity = keyword_set(velocity)
	
	;Constants
	e_mass = constants('m_e', /DOUBLE)
	charge = constants('q',   /DOUBLE)

;---------------------------------------------------------------------
; Energy to Velocity? ////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;E = 0.5 * (m/q) * v^2
    ;v = sqrt(2 * (q/m) / E)
	coef = (2.0 * charge) / (eMass * 1.0e12)     ;1.0e7 to make units 10^4
	if nParams eq 3 then begin
	    _R = velocity ? sqrt(coef * E) : E
	endif else if nParams eq 5 then begin
		_R  = velocity ? sqrt(coef * E)  : E
		_R1 = velocity ? sqrt(coef * E1) : E1
	endif else if nParams eq 7 then begin
		;Find +/- ∆v... Would have to use quadratic formula
		;   E + ∆E = 0.5 * (q/m) + (v +/- ∆v)^2
		;
		;Instead of finding pixel deltas, find pixel corners
		_R  = velocity ? sqrt(coef * (E - E1)) : E - E1
		_R1 = velocity ? sqrt(coef * (E + E2)) : E + E2
		
		;Polar angle
		PA_temp = _PA
		_PA     = PA_temp - _PA1
		_PA1    = temporary(PA_temp) + temporary(_PA2)
	endif else begin
	    message, 'Incorrect number of parameters.'
	endelse

;---------------------------------------------------------------------
; Convert to Radians? ////////////////////////////////////////////////
;---------------------------------------------------------------------
	_PA = degrees ? PA * !dtor : PA
	if nParams eq 5 then _PA1 = degrees ? PA1 * !dtor : PA1
	if nParams eq 7 then _PA2 = degrees ? PA2 * !dtor : PA2

;---------------------------------------------------------------------
; Reflect from [0, 180] to [180, 360] ////////////////////////////////
;---------------------------------------------------------------------
	if reflect then begin
		;data and PA
		data = reverse(data, 2)
		_PA  = 2.0*!pi - reverse(_PA)
		
		;Pixel corners or deltas?
		if n_elements(_PA1) gt 0 then begin
			if nParams eq 5 $
				then _PA1 = 2.0*!pi - reverse(_PA1) $
				else _PA1 = 2.0*!pi - reverse(_PA1)
		endif
	
		;Pixel corners or deltas?
		if n_elements(_PA2) gt 0 $
			then _PA2 = 2.0*!pi - reverse(_PA2)
	endif

;---------------------------------------------------------------------
; Phase Space Density? ///////////////////////////////////////////////
;---------------------------------------------------------------------
	if psd then begin
		DEF_to_PSD  = (0.5 * e_mass^2) / r^2
		data       *= rebin(reform(DEF_to_PSD, dims[0], 1, dims[2]), dims)
	endif

;-------------------------------------------------------
; Distribution Functions ///////////////////////////////
;-------------------------------------------------------

	;Create the distribution function.
	img = MrImage(data, _R, _PA, _R1, _PA1, _STRICT_EXTRA=extra)
	
	return, img
end