; docformat = 'rst'
;
; NAME:
;   weOverPlot
;
; PURPOSE:
;+
; The purpose of this program is to create a data object that can be plotted or drawn on
; a set of axes set up by another plotting command.
;
; :Categories:
;    Plot Utilities, Wrapper
;    
; :Examples:
;    Use, for example, with the cgPlot command::
;       oplotObj1 = weOverPlot(cgDemoData(17), Color='red', PSYM=-1, LINESTYLE=2)
;       oplotObj2 = weOverPlot(cgDemoData(17), Color='blue', PSYM=-2, LINESTYLE=4)
;       cgPlot, cgDemoData(17), Color='purple', PSYM=-4, OPLOTS=[oplotObj1, oplotObj2]
;       Obj_Destroy, [oplotObj1, oplotObj2]
;       
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Change History::
;       04/24/2013  -   Written by Matthew Argall
;       09/06/2013  -   Added the DIMENSION keyword
;       
;-
;*****************************************************************************************
;+
; The initialization method of the object. Called automatically when the object
; is created.
; 
; :Params:
;    x: in, required, type=any
;         If X is provided without Y, a vector representing the dependent values to be 
;         plotted If both X and Y are provided, X is the independent parameter and 
;         Y is the dependent parameter to be plotted.
;    y: in, optional, type=any
;         A vector representing the dependent values to be plotted.
;       
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to the resizeable graphics window cgWindow.
;     color: in, optional, type=string, default='opposite'
;        The name of the data color. This is the color of the data line.
;     dimension: in, optional, type=boolean, default=0
;        The dimension of data over which to plot. This applies only if the dependent
;        variable is 2D. Say a time series vector V is [3,N]. To plot V[0,*], V[1,*], and
;        V[2,*] independently within the same set of axes, set dimension=2.
;     draw: in, optional, type=boolean, default=0
;        If this keyword is set, the data is drawn as soon as the object is created.
;     linestyle: in, optional, type=integer, default=0
;        The line style for drawing the line.
;     psym: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     skip: in, optional, type=integer, default=1
;        The number of data points to skip when the line is drawn. The default is to
;        not skip any data points, but to plot them all.
;     symcolor: in, optional, type=string
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     symthick: in, optional, type=float, default=1.0
;        The thickness of the symbol.
;     thick: in, optional, type=float, default = 1.0
;        The thickness of the line.
;     visible: in, optional, type=boolean, default=1
;        Set this keyword to determine in the line should be drawn (visible=1), or
;        if the line should not be drawn (visible=0).
;-
FUNCTION weOverPlot, x, y, $
    ADDCMD=addcmd, $
    COLOR=color, $
    DRAW=draw, $
    DIMENSION=dimension, $
    LINESTYLE=linestyle, $
    PSYM=psym, $
    SKIP=skip, $
    SYMCOLOR=symcolor, $
    SYMSIZE=symsize, $
    SYMTHICK=symthick, $
    THICK=thick, $
    VISIBLE=visible
    
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        IF N_Elements(currentState) NE 0 THEN SetDecomposedState, currentState
        RETURN, 0
    ENDIF

    ;Figure out the dependent variable
    IF N_PARAMS() EQ 1 $
        THEN dep = x $
        ELSE dep = y
    
    n_dims = Size(dep, /N_DIMENSIONS)
    dep_sz = Size(dep, /DIMENSIONS)
    
    ;Make X the index values along a column of Y, since each column will be plotted
    ;independently.
    IF N_Params() EQ 1 THEN BEGIN
        ;scalar
        if n_dims[0] EQ 0 THEN BEGIN
            dep = [[1], [dep]]
            indep = [0]
        
        ;vector
        ENDIF ELSE IF n_dims[0] eq 1 THEN BEGIN
            indep = Findgen(n_elements(dep))
            dep = Transpose(dep)
        
        ;array of vectors
        ENDIF ELSE IF n_dims[0] EQ 2 THEN BEGIN
            indep = Findgen(dep_sz[1])
            
        ENDIF
        
    ENDIF ELSE BEGIN
        indep = x
        
        CASE n_dims OF
            0: dep = [[1], [dep]]
            1: dep = Transpose(dep)
            else: ;everthing is ok
        ENDCASE
    ENDELSE
    
    thisObject = obj_new('weOverPlot', indep, dep, $
        ADDCMD=addcmd, $
        COLOR=color, $
        DRAW=draw, $
        LINESTYLE=linestyle, $
        PSYM=psym, $
        SKIP=skip, $
        SYMCOLOR=symcolor, $
        SYMSIZE=symsize, $
        SYMTHICK=symthick, $
        THICK=thick, $
        VISIBLE=visible)
        
    return, thisObject

END