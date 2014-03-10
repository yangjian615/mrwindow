; docformat = 'rst'
;
; NAME:
;       MrCorrPlot
;
;*****************************************************************************************
;   Copyright (c) 2013, University of New Hampshire                                      ;
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
;   The purpose of this program is to plot the auto- or cross-correlation of a set of
;   data. The final plot is determined by the LAG keyword.
;
; :Examples:
;   Auto-Correlation with lag example (see the A_CORRELATE help page)::
;       ;Define an n-element sample population:
;       X = [3.73, 3.67, 3.77, 3.83, 4.67, 5.87, 6.70, 6.97, 6.40, 5.57]
;       ;Compute the autocorrelation of X for LAG = -3, 0, 1, 3, 4, 8:
;       lag = [-3, 0, 1, 3, 4, 8]
;       a_plot = MrCorrPlot(X, LAG=lag, XRANGE=[-4,9], COEFFICIENT=coeff)
;       print, coeff
;
;   Cross-Correlation with lag example (see the C_CORRELATE help page)::
;       ;Define two n-element sample populations:
;       X = [3.73, 3.67, 3.77, 3.83, 4.67, 5.87, 6.70, 6.97, 6.40, 5.57]
;       Y = [2.31, 2.76, 3.02, 3.13, 3.72, 3.88, 3.97, 4.39, 4.34, 3.95]
;
;       ;Compute the cross correlation of X and Y with lag:
;       lag = [-5, 0, 1, 5, 6, 7]
;       cc_plot = MrCorrPlot(X, Y, LAG=lag, XRANGE=[-6,8], COEFFICIENT=coeff)
;       print, coeff
;
;   Cross-Correlation without lag example (see the CORRELATE help page)
;       X = [65, 63, 67, 64, 68, 62, 70, 66, 68, 67, 69, 71]
;       Y = [68, 66, 68, 65, 69, 66, 68, 65, 71, 67, 68, 70]
;       c_plot = MrCorrPlot(X, Y, COEFFICIENT=coeff)
;       print, coeff
;
; :Categories:
;       Function Graphics
;
; :Params:
;       X:                  in, required, type=1D intarr/fltarr/dblarr
;                           A vector of data.
;       Y:                  in, optional, type=1D
;                           A vector of data. If not supplied, the autocorrelation of `X`
;                               is computed.
;
; :Keywords:
;       CURRENT:            in, optional, type=boolean, default=0
;                           If set, `CORRPLOT` will be added to the current MrWindow
;                               graphics window.
;       COEFFICIENT:        out, optional, type=fltarr/dblarr
;                           Results of the auto- or cross-correlation.
;       COVARIANCE:         in, optional, type=boolean, default=0
;                           If set, compute the sample cross covariance rather than the
;                               sample cross correlation.
;       DOUBLE:             in, optional, type=boolean, default=0
;                           If set, computation is done in double-precision arithmetic.
;       LAG:                in, optional, type=string, default=0
;                           A scalar or n-element integer vector in the interval
;                               [-(n-2), (n-2)], specifying the signed distances between
;                               indexed elements of X. If LAG is a scalar, then a scatter
;                               plot of `Y` vs. `X` will be made, a best fit line will
;                               be drawn through the points, and the equation and
;                               coefficient will be included as a legend in the upper-left
;                               corner. If LAG has more then one element, a plot of
;                               `COEFFICIENT` vs. LAG will be made.
;                               
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by the MrPlot graphic object is also
;                               accepted via keyword inheritance.
;
; :Returns:
;       CORRPLOT:           out, required, type=object
;                           Object reference of the MrPlot object containing the plotted
;                               cross- or auto-correlation.
;
; :Uses:
;   Uses the following external programs::
;       cgRootName.pro
;       cgErrorMSG.pro
;       MrPlot.pro
;       MrLegend.pro
;
; :Author:
;       Matthew Argall::
;		University of New Hampshire
;		Morse Hall, Room 113
;       8 College Rd.
;		Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2014/02/26  -   Written by Matthew Argall
;-
function MrCorrPlot, x, y, $
LAG=lag, $
COEFFICIENT=coefficient, $
COVARIANCE=covariance, $
CURRENT=current, $
DOUBLE=double, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, obj_new()
    endif

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    nPrams = n_params()
    covariance = keyword_set(covariance)
    double = keyword_set(double)
    if n_elements(lag) eq 0 then lag = 0
    
    nX   = n_elements(x)
    nLag = n_elements(lag)
    
    ;Make sure -(n-2) < LAG < n-2
    lag = ((-nX+2) > lag) < (nX-2)

;-----------------------------------------------------
;Coefficients \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    case nPrams of
        1: coefficient = a_correlate(x, lag, COVARIANCE=covariance, DOUBLE=double)
        2: coefficient = c_correlate(x, y, lag, COVARIANCE=covariance, DOUBLE=double)
        else: message, 'Incorrect number of parameters.'
    endcase
        
;-----------------------------------------------------
;Prep \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Title
    title  = (nPrams      eq 1) ? 'Auto-'      : 'Cross-'
    title += (covariance  eq 1) ? 'Covariance' : 'Correlation'
        
;-----------------------------------------------------
;Best-Fit Line? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if n_elements(lag) eq 1 then begin
        ;Auto- or cross-correlation?
        indep = shift(x, lag)
        if nPrams eq 1 $
            then dep = x $
            else dep = y

        ;XY-Title            
        xtitle = 'X (Lag=' + string(lag, FORMAT='(i0)') + ')'
        ytitle = (nPrams eq 1) ? 'X' : 'Y'
            
        ;Create the plot
        corrPlot = MrPlot(indep, dep, CURRENT=current, $
                          TITLE=title, XTITLE=xtitle, YTITLE=ytitle, PSYM=1, _EXTRA=extra)
        
        ;Overplot the best-fit line
        params = linfit(indep, temporary(dep), YFIT=yPrime)
        bestFitLine = MrPlot(temporary(indep), yPrime, /CURRENT, OVERPLOT=corrPlot, $
                             COLOR='Blue')
        
        ;Create a Legend in the upper-left corner
        equation = string(FORMAT='(%"y = %0.4fx + %0.4f")', params[1], params[0])
        coeff    = 'R = ' + string(FORMAT='(f0.4)', coefficient)
        corrLegend = MrLegend(/CURRENT, TARGET=corrPlot, LOCATION=4, LENGTH=0, $
                              TITLE=[equation,coeff])
        
;-----------------------------------------------------
;Correlation vs. Lag \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else begin
        corrPlot = MrPlot(lag, coefficient, CURRENT=current, $
                          XTITLE='Lag', YTITLE='R', COLOR='Red', PSYM=1, _EXTRA=extra)
    endelse
    
    return, corrPlot
end