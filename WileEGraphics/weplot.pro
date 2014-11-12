; docformat = 'rst'
;
; NAME:
;   MrPlot
;
; PURPOSE:
;+
;   The purpose of this program is to serve as a wrapper for the cgPlot command. Additions
;   to cgPlot include:
;       - Plot multiple columns of data on the same axes with a single call.
;           o Each column of Y is plotted against X (which is 1D)
;       - Each line can have its own
;           o Color
;           o Linestyle
;           o Symbol
;           o Symbol Color
;
; :Categories:
;    Plot Utilities, Wrapper, Coyote Graphics
;    
; :Examples:
;   The following data will be used for the next example::
;       Bx = MrSigGen([0.852, 5.1, 2.33, 1.24], 2, 4, TIME=t)
;       By = MrSigGen([0.852, 5.1], 1, 3)
;       Bz = MrSigGen([0.852, 5.1, 1.21, 0.9], 5, 2)
;       B = transpose([[temporary(B_mag)], [temporary(Bx)], [temporary(By)], [temporary(Bz)]])
;
;   Plot 4 columns of data at the same time::
;       wePlot, t, B, COLOR=['black', 'blue', 'green', 'red'], $
;               LINESTYLE=[0,1,2,3], $
;               TITLE='3-Components of the Magnetic Field', $
;               XTITLE='Time (s)', YTITLE='B (nT)'
;
;   The same example, but with a legend::
;       legendItem = Obj_New('cgLegendItem', Color=['black', 'blue', 'green', 'red'], $
;           PSym=[6,15,4,8], Symsize=1.5, Location=[0.825, 0.875], Title=['|B|', 'Bx', 'By', 'Bz'], $
;           /CENTER_SYM, /Hardware, Length=0.05)
;
;       wePlot, t, B, COLOR=['black', 'blue', 'green', 'red'], $
;               LINESTYLE=[0,1,2,3], $
;               TITLE='3-Components of the Magnetic Field', $
;               LEGENDS = legendItem, $
;               XTITLE='Time (s)', YTITLE='B (nT)'
;       obj_destroy, legendItem
;
;
; :Params:
;     x: in, required, type=numeric
;        If x is the only parameter, then it is the independent variable to be plotted.
;        In this case, it will be plotted against an array of index values. If `y` is also
;        present, then x is the independent variable data to be plotted.
;     y: in, optional, type=numeric
;        The dependent variable data to be plotted against `x`
;
; :Keywords:
;     addcmd: in, optional, type=boolean, default=0
;        Set this keyword to add the command to the resizeable graphics window cgWindow.
;     clip: in, optional, type=integer
;        The coordinates of a rectangle used to clip the graphics output.
;     color: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the data color. By default, 'black'.
;        Color names are those used with cgColor. Otherwise, the keyword is assumed 
;        to be a color index into the current color table.
;     charsize: in, optional, type=float, default=cgDefCharSize()
;        The character size for axes annotations. Uses cgDefCharSize to select default
;        character size, unless !P.Charsize is set, in which case !P.Charsize is always used.
;     dimension: in, optional, type=int, default=0
;        The dimension over which to plot. As an example, say `Y` is an N1xN2 array and
;        settind DIMENSION=2. Then, N1 plots will be overplotted on top of each other,
;        one for each DATA[i,*]. If DIMENSION=0, then a single plot of all points will
;        be made.
;     legends: in, optional, type=object
;        One or more cgLegendItem objects that are to be drawn on the plot.
;     linestyle: out, optional, type=integer/intarr, default=0
;        The line style for drawing each line.
;     max_value: in, optional, type=float
;        The maximum value to be plotted. Larger values are treated as missing.
;     min_value: in, optional, type=float
;        The minimum value to be plotted. Smaller values are treated as missing.
;     noclip: in, optional, type=boolean, default=1
;        Set this keyword to suppress clipping of the plot.
;     nsum: in, optional, type=integer
;        The presence of this keyword indicates the number of data points to average
;        when plotting.
;     oplots: in, optional, type=object
;        A single cgOverPlot object, or an array of cgOverPlot objects that will be
;        overplot on the axes set up by the original data. The user will be responsible
;        for destroying the objects. The cgPlot program will simply draw the objects.
;     polar: in, optional, type=boolean, default=0
;        Set this keyword to produce polar plots.
;     psym: in, optional, type=integer, default=0
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: in, optional, type=string/integer, default='black'
;        If this keyword is a string, the name of the symbol color. By default, 'black'.
;        Otherwise, the keyword is assumed to be a color index into the current color table.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     xrange: in, optional, type=fltarr(2), default=[min(`X`)\, max(`X`)]
;        The range over which the independent variable will be displayed.
;     yrange: in, optional, type=fltarr(2), default=[min(`Y`)\, max(`Y`)]
;        The range over which the dependent variable will be displayed.
;     zvalue: in, optional, type=float, default=0.0
;        Sets the Z coordinate, in normalized coordinates in the range of 0 to 1, of
;        the axis and data output
;     _ref_extra: in, optional, type=struct
;        Any keyword accepted by cgPlot or cgOPlot.
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
;       05/04/2013  -   Written by Matthew Argall
;       05/06/2013  -   Include all of the keywords to cgOPlot as well as IDL's oplot
;                           so that _REF_EXTRA is passed to cgPlot, not cgOPlot. cgPlot
;                           really calls OPlot when the OVERPLOT keyword is set. - MRA
;       06/28/2013  -   Added the DIMENSION keyword. - MRA
;       07/08/2013  -   Do not transpose the data if DIMENSION=2. - MRA
;       09/05/2013  -   Pick the dimension properly when 1 parameter is given. - MRA
;-
pro wePlot, x, y, $
ADDCMD=addcmd, $
CLIP=clip, $
COLOR = color, $
CHARSIZE = charsize, $
DIMENSION = dimension, $
LEGENDS = legends, $
LINESTYLE = linestyle, $
MAX_VALUE=max_value, $
MIN_VALUE=min_value, $
NOCLIP=NOCLIP, $
NSUM=nsum, $
OPLOTS = oplots, $
POLAR=polar, $
PSYM = psym, $
SYMCOLOR = symcolor, $
SYMSIZE = symsize, $
XRANGE = xrange, $
YRANGE = yrange, $
ZVALUE=zvalue, $
_REF_EXTRA = extra
    compile_opt idl2
    
    ;Error handling
    Catch, the_error
    IF the_error NE 0 THEN BEGIN
        Catch, /cancel
        void = cgErrorMsg()
        RETURN
    ENDIF

    nx = N_Elements(x)
    ny = N_Elements(y)
    setDefaultValue, dimension, 0

;---------------------------------------------------------------------
;Dependent and Independent Variables /////////////////////////////////
;---------------------------------------------------------------------
    ;Figure out the dependent variable
    IF ny EQ 0 $
        THEN dep = x $
        ELSE dep = y

    ;Make the independent variable index the chosen DIMENSION of Y.
    IF ny EQ 0 THEN BEGIN
        IF dimension EQ 0 $
            THEN indep = LindGen(N_Elements(dep)) $
            ELSE indep = LindGen(dims[dimension-1])
    
    ;The independent variable was given.
    ENDIF ELSE BEGIN
        dims = Size(y, /DIMENSIONS)
        indep = x
    ENDELSE
    
    ;Make sure arrays were given, not scalars
    IF N_Elements(indep) EQ 1 THEN indep = [indep]
    IF N_Elements(dep) EQ 1 THEN dep = [dep]
    
    ;Size of each dimension
    dims = Size(dep, /DIMENSIONS)

;---------------------------------------------------------------------
;Set Defaults ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;The dimension not being plotted.
    CASE dimension OF
        0: xdim = 0
        1: xdim = 2
        2: xdim = 1
    ENDCASE
        
    ;Number of defaults to use.
    IF xdim EQ 0 THEN nDefaults = 1 ELSE nDefaults = dims[xdim-1]

    ;Pick a set of default colors so not everything is the same color.
    default_colors = ['opposite', 'Blue', 'Forest_Green', 'Red', 'Magenta', 'Orange']
    IF nDefaults EQ 1 THEN d_color = default_colors[0] ELSE d_color = default_colors[1:nDefaults]

    ;Set Defaults
    setDefaultValue, color, d_color
    setDefaultValue, linestyle, 0
    setDefaultValue, psym, 0
    setDefaultValue, symcolor, color
    setDefaultValue, xrange, [Min(indep, Max=indep_max), indep_max]
    setDefaultValue, yrange, [Min(dep, Max=dep_max), dep_max]*1.05

;---------------------------------------------------------------------
;Normal Plotting /////////////////////////////////////////////////////
;---------------------------------------------------------------------

    IF dimension EQ 0 THEN BEGIN
        cgPlot, indep, dep, $
                ADDCMD    = addcmd, $
                CLIP      = clip, $
                COLOR     = color, $
                CHARSIZE  = charsize, $
                LEGENDS   = legends, $
                LINESTYLE = linestyle, $
                MAX_VALUE = max_value, $
                MIN_VALUE = min_value, $
                NOCLIP    = NOCLIP, $
                NSUM      = nsum, $
                OPLOTS    = oplots, $
                POLAR     = polar, $
                PSYM      = psym, $
                SYMCOLOR  = symcolor, $
                SYMSIZE   = symsize, $
                XRANGE    = xrange, $
                YRANGE    = yrange, $
                ZVALUE    = zvalue, $
                _EXTRA    = extra
        RETURN
    ENDIF

;---------------------------------------------------------------------
;Multiple Line Plots /////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Set up the axes    
    cgPlot, 0, 0, /NODATA, $
                  ADDCMD=addcmd, $
                  CHARSIZE=charsize, $
                  CLIP=clip, $
                  LEGENDS=legends, $
                  NOCLIP=NOCLIP, $
                  OPLOTS=oplots, $
                  POLAR=polar, $
                  T3D=t3d, $
                  THICK=thick, $
                  XRANGE=xrange, $
                  YRANGE=yrange, $
                  ZVALUE=zvalue, $
                  _STRICT_EXTRA=extra

    ;Get number of elements to make keywords cyclic.
    nColor = n_elements(color)
    nLineStyle = n_elements(linestyle)
    nPSym = n_elements(psym)
    nSymColor = n_elements(symcolor)
    
    ;Plot each vector of data.
    FOR j = 0, dims[xdim-1]-1 DO BEGIN

        CASE xdim OF
            1: cgOPlot, indep, dep[j,*], $
                        ADDCMD=addcmd, $
                        COLOR=color[j mod nColor], $
                        CHARSIZE=charsize, $
                        CLIP=clip, $
                        LINESTYLE=linestyle[j mod nLineStyle], $
                        MAX_VALUE=max_value, $
                        MIN_VALUE=min_value, $
                        NOCLIP=NOCLIP, $
                        NSUM=nsum, $
                        POLAR=polar, $
                        PSYM=psym[j mod nPSym], $
                        SYMCOLOR=symcolor[j mod nSymColor], $
                        SYMSIZE=symsize, $
                        T3D=t3d, $
                        THICK=thick, $
                        ZVALUE=zvalue
                        
            2: cgOPlot, indep, dep[*,j], $
                        ADDCMD=addcmd, $
                        COLOR=color[j mod nColor], $
                        CHARSIZE=charsize, $
                        CLIP=clip, $
                        LINESTYLE=linestyle[j mod nLineStyle], $
                        MAX_VALUE=max_value, $
                        MIN_VALUE=min_value, $
                        NOCLIP=NOCLIP, $
                        NSUM=nsum, $
                        POLAR=polar, $
                        PSYM=psym[j mod nPSym], $
                        SYMCOLOR=symcolor[j mod nSymColor], $
                        SYMSIZE=symsize, $
                        T3D=t3d, $
                        THICK=thick, $
                        ZVALUE=zvalue
        ENDCASE
    ENDFOR
end