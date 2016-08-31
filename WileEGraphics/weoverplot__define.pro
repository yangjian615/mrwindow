; docformat = 'rst'
;
; NAME:
;   weOPlot__Define
;
; PURPOSE:
;+
;   The purpose of this program is to create a data object that can be plotted or drawn on
;   a set of axes set up by another plotting command.
;
;   This serves as a wrapper for the cgOverPlot__Define object class. Additions to said
;   class include::
;       - Multiple columns of data are plotted in the same set of axes without
;           separate calls.
;
; :Categories:
;    Plot Utilities, Wrapper
;    
; :Examples:
;    Use, for example, with the cgPlot command::
;       oplotObj1 = Obj_New('weOPlot', cgDemoData(17), Color='red', PSYM=-1, LINESTYLE=2)
;       oplotObj2 = Obj_New('weOPlot', cgDemoData(17), Color='blue', PSYM=-2, LINESTYLE=4)
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
;       04/23/2013  -   Written by Matthew Argall
;       05/08/2013  -   Each over plotted line can now have its own symcolor, psym,
;                           linestyle, and color. - MRA
;       05/12/2013  -   Use N_Elements instead of N_Params so undefined inputs can be
;                           provided (and ignored). Added a SetProperty and GetProperty
;                           method. - MRA
;       09/06/2013  -   Added the DIMENSION keyword. Replaced the contents of the Draw
;                           method with a call to weOPlot.pro. Removed the SYMTHICK
;                           and SKIP keywords because cgPlot does not support them.
;                           (Previous version was using cgSymCat to place each symbol. See
;                           cgOverplot__Define.) Added all of IDL's OPLOT keywords as
;                           object properties. - MRA
;       09/27/2013  -   Added the GRAPHIC keyword. - MRA
;       09/29/2013  -   GRAPHIC can accept more than one graphic object. - MRA
;       2013/10/25  -   Renamed GRAPHIC to TARGET and VISIBLE to HIDE to match Function
;                           Graphics conventions introduced in IDL8.0. - MRA
;       2013/11/21  -   Inherit MrGrAtom. - MRA
;-
;*****************************************************************************************
;+
; This method draws the overplot object. It assumes a set of axes has been
; established by some other graphics command.
;
; The intention is to over-ride cgOverPlot's Draw method so that multiple columns
; of data can be plotted at the same time. Basically, this involves throwing the
; draw method in a loop, but since there is an implicit use of *self.dep, I had
; to copy and paste everything.
;
; :Keywords:
;-
PRO weOverplot::Draw, $
NOERASE=noerase
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
    
        ;Reset the system variables
        IF N_Elements(p_orig) GT 0 THEN !P = p_orig
        IF N_Elements(x_orig) GT 0 THEN !X = x_orig
        IF N_Elements(y_orig) GT 0 THEN !Y = y_orig
        IF N_Elements(z_orig) GT 0 THEN !Z = z_orig
        
        MrPrintF, 'LogErr'
        RETURN
    ENDIF
    
    ;To draw, or not to draw?
    IF self.hide EQ 1 THEN RETURN

    ;Are there graphics present?
    IF N_Elements(*self.target) GT 0 THEN IF Max(Obj_Valid(*self.target)) EQ 1 THEN BEGIN
        
        ;Save the current sytem variables
        p_orig = !P
        x_orig = !X
        y_orig = !Y
        z_orig = !Z

        ;Step through each graphic
        FOR i = 0, N_Elements(*self.target) - 1 DO BEGIN
            IF Obj_Valid((*self.target)[i]) EQ 0 THEN CONTINUE
    
            ;Pull the system variables from the graphic
            (*self.target)[i] -> RestoreCoords

            ;Overplot onto said graphic
            self -> doOverplot
        ENDFOR
    
        ;Reset the system variables
        !P = p_orig
        !X = x_orig
        !Y = y_orig
        !Z = z_orig
    
    ;If not
    ENDIF ELSE self -> doOverplot
END


;+
;   The purpose of this method is to do the actual overplotting. Basically, having this
;   here merely to saves space in the Draw method.
;-
PRO weOverplot::doOverplot

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN
    ENDIF
    
    ;Get the dimensions of the independent variable.
    weoplot, *self.indep, *self.dep, $

             ;weOPlot Keywords
             COLOR     = *self.color, $
             DIMENSION =  self.dimension, $
             LINESTYLE = *self.linestyle, $
             PSYM      = *self.psym, $
             SYMCOLOR  = *self.symcolor, $
             SYMSIZE   = *self.symsize, $
             THICK     = *self.thick, $

             ;cgOPlot Keywords
             CHARSIZE  = *self.charsize, $

             ;OPlot Keywords
             NSUM      = *self. nsum, $
             POLAR     = *self. polar, $
             CLIP      = *self. clip, $
             NOCLIP    = *self. noclip, $
             T3D       = *self. t3d, $
             ZVALUE    = *self. zvalue
END
    

;+
; This method gets properties of the object.
; 
; :Keywords:
;     dep: out, required, type=any
;         A vector representing the independent values to be plotted
;     indep: out, optional, type=any
;         A vector representing the dependent values to be plotted.
;     color: out, optional, type=string
;        The name of the data color. This is the color of the data line.
;     dimension: out, optional, type=boolean
;        The dimension of data over which to plot.
;     draw: out, optional, type=boolean
;        If this keyword is set, the data is drawn as soon as the object is created.
;     target: out, optional, type=object
;        The graphic object on which the overplot is placed.
;     linestyle: out, optional, type=integer
;        The line style for drawing the line.
;     psym: out, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: out, optional, type=string
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: out, optional, type=float
;        The symbol size.
;     thick: out, optional, type=float
;        The thickness of the line.
;-
PRO weOverplot::GetProperty, $
;Data Keywords
DEP=dep, $
INDEP=indep, $

;weOverplot Keywords
TARGET=target, $

;weOPlot Keywords
COLOR=color, $
DIMENSION=dimension, $
DRAW=draw, $
LINESTYLE=linestyle, $
PSYM=psym, $
SYMCOLOR=symcolor, $
SYMSIZE=symsize, $
THICK=thick, $

;cgOPlot Keywords
CHARSIZE=charsize, $

;OPlot Keywords
NSUM = nsum, $
POLAR = polar, $
CLIP = clip, $
NOCLIP = noclip, $
T3D = t3d, $
ZVALUE = zvalue, $
_REF_EXTRA = extra
    Compile_Opt strictarr

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN
    ENDIF      
    
    ;weOPlot Properties
    IF Arg_Present(dep)       NE 0 THEN IF N_Elements(*self.dep)       NE 0 THEN dep       = *self.dep
    IF Arg_Present(indep)     NE 0 THEN IF N_Elements(*self.indep)     NE 0 THEN indep     = *self.indep
    IF Arg_Present(color)     NE 0 THEN IF N_Elements(*self.color)     NE 0 THEN color     = *self.color
    IF Arg_Present(linestyle) NE 0 THEN IF N_Elements(*self.linestyle) NE 0 THEN linestyle = *self.linestyle
    IF Arg_Present(psym)      NE 0 THEN IF N_Elements(*self.psym)      NE 0 THEN psym      = *self.psym
    IF Arg_Present(symcolor)  NE 0 THEN IF N_Elements(*self.symcolor)  NE 0 THEN symcolor  = *self.symcolor
    IF Arg_Present(symsize)   NE 0 THEN IF N_Elements(*self.symsize)   NE 0 THEN symsize   = *self.symsize
    IF Arg_Present(thick)     NE 0 THEN IF N_Elements(*self.thick)     NE 0 THEN thick     = *self.thick
    IF Arg_Present(dimension) NE 0 THEN dimension = self.dimension

    ;cgOPlot Properties
    IF Arg_Present(charsize)  NE 0 THEN IF N_Elements(*self.charsize)  NE 0 THEN charsize  = *self.charsize

    ;OPlot Properties
    IF Arg_Present(nsum)      NE 0 THEN IF N_Elements(*self.nsum)      NE 0 THEN nsum      = *self.nsum
    IF Arg_Present(polar)     NE 0 THEN IF N_Elements(*self.polar)     NE 0 THEN polar     = *self.polar
    IF Arg_Present(clip)      NE 0 THEN IF N_Elements(*self.clip)      NE 0 THEN clip      = *self.clip
    IF Arg_Present(noclip)    NE 0 THEN IF N_Elements(*self.noclip)    NE 0 THEN noclip    = *self.noclip
    IF Arg_Present(t3d)       NE 0 THEN IF N_Elements(*self.t3d)       NE 0 THEN t3d       = *self.t3d
    IF Arg_Present(zvalue)    NE 0 THEN IF N_Elements(*self.zvalue)    NE 0 THEN zvalue    = *self.zvalue

    ;Graphic Object
    IF Arg_Present(target)   NE 0 THEN IF Obj_Valid(self.target) $
        THEN target = self.target $
        ELSE target = Obj_New()

    ;Draw?
    IF Keyword_Set(draw) THEN self -> Draw
    
    IF N_Elements(extra) GT 0 THEN self -> GetProperty, _STRICT_EXTRA=extra
END


;+
; This method sets properties of the object.
; 
; :Keywords:
;     dep: in, required, type=any
;         A vector representing the independent values to be plotted
;     indep: in, optional, type=any
;         A vector representing the dependent values to be plotted.
;     color: in, optional, type=string, default='opposite'
;        The name of the data color. This is the color of the data line.
;     dimension: in, optional, type=boolean, default=0
;        The dimension of data over which to plot.
;     draw: in, optional, type=boolean, default=0
;        If this keyword is set, the data is drawn as soon as the object is created.
;     target: in, optional, type=object
;        The graphic object on which to overplot.
;     linestyle: in, optional, type=integer, default=0
;        The line style for drawing the line.
;     psym: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: in, optional, type=string
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     thick: in, optional, type=float, default = 1.0
;        The thickness of the line.
;-
PRO weOverplot::SetProperty, $
;Data Keywords
DEP=dep, $
INDEP=indep, $

;weOverplot Keywords
TARGET=target, $

;weOPlot Keywords
COLOR=color, $
DIMENSION=dimension, $
DRAW=draw, $
LINESTYLE=linestyle, $
PSYM=psym, $
SYMCOLOR=symcolor, $
SYMSIZE=symsize, $
THICK=thick, $

;cgOPlot Keywords
CHARSIZE=charsize, $

;OPlot Keywords
NSUM = nsum, $
POLAR = polar, $
CLIP = clip, $
NOCLIP = noclip, $
T3D = t3d, $
ZVALUE = zvalue, $
_REF_EXTRA = extra
    Compile_Opt strictarr

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN
    ENDIF      
    
    ;Set Properties
    IF N_Elements(dep)       NE 0 THEN *self.dep = dep
    IF N_Elements(indep)     NE 0 THEN *self.indep = indep
    IF N_Elements(color)     NE 0 THEN *self.color = color
    IF N_Elements(dimension) NE 0 THEN  self.dimension = dimension
    IF N_Elements(linestyle) NE 0 THEN *self.linestyle = linestyle
    IF N_Elements(psym)      NE 0 THEN *self.psym = psym
    IF N_Elements(symcolor)  NE 0 THEN *self.symcolor = symcolor
    IF N_Elements(symsize)   NE 0 THEN *self.symsize = symsize
    IF N_Elements(thick)     NE 0 THEN *self.thick = thick
    IF N_Elements(hide)      NE 0 THEN  self.hide = keyword_set(hide)
    
    ;cgOPlot Properties
    IF N_Elements(charsize)  NE 0 THEN *self.charsize = charsize
    
    ;OPlot Properties
    IF N_Elements(nsum)   NE 0 THEN *self.nsum = nsum
    IF N_Elements(polar)  NE 0 THEN *self.polar = polar
    IF N_Elements(clip)   NE 0 THEN *self.clip = clip
    IF N_Elements(noclip) NE 0 THEN *self.noclip = noclip
    IF N_Elements(t3d)    NE 0 THEN *self.t3d = t3d
    IF N_Elements(zvalue) NE 0 THEN *self.zvalue = zvalue
    
    ;Graphic Object
    IF N_Elements(target) GT 0 THEN IF (Max(Obj_Valid(target)) EQ 1) THEN BEGIN
        IF N_Elements(*self.target) GT 0 $
            THEN *self.target = [*self.graphic, target] $
            ELSE *self.target = target
    ENDIF ELSE BEGIN
        ;Graphics are assumed to be destroyed elsewhere
        Ptr_Free, self.target
        self.target = Ptr_New(/ALLOCATE_HEAP)
    ENDELSE
    
    IF N_Elements(extra) GT 0 THEN self -> MrGrAtom::SetProperty, _STRICT_EXTRA=extra

    ;Draw?
    self.window -> Draw
END


;+
; The clean-up routine for the object. Destroy pointers, etc.
;-
PRO weOverplot::Cleanup
    Compile_Opt idl2

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN
    ENDIF
    
    ;Free pointers
    Ptr_Free, self.dep
    Ptr_Free, self.indep
    Ptr_Free, self.color
    Ptr_Free, self.psym
    Ptr_Free, self.symcolor
    Ptr_Free, self.symsize
    Ptr_free, self.linestyle
    Ptr_free, self.thick
    Ptr_free, self.nsum
    Ptr_free, self.polar
    Ptr_free, self.noclip
    Ptr_free, self.t3d
    Ptr_free, self.zvalue
    
    ;Graphics -- Only free the pointer. Graphics objects are assumed to be destroyed elsewhere. 
    Ptr_Free, self.target
    
    self -> MrGrAtom::CleanUp
END


;+
; The initialization method of the object. Called automatically when the object
; is created.
;
; This differs from cgOverPlot__Define only in that it accepts data with multiple
; columns. Doing so involves additional checks on the dependent and independent variables::
;   - A single row of data is transformed into a single column of data.
;   - 2D arrays are assumed to have the independent variable along the columns.
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
;     color: in, optional, type=string, default='opposite'
;        The name of the data color. This is the color of the data line.
;     dimension: in, optional, type=boolean, default=0
;        The dimension of data over which to plot. This applies only if the dependent
;        variable is 2D. Say a time series vector V is [3,N]. To plot V[0,*], V[1,*], and
;        V[2,*] independently within the same set of axes, set dimension=2.
;     target: in, optional, type=object, default=obj_new()
;        Normally, OPlot and cgOPlot use the ![PXYZ] system variables to determine how to
;        draw the overplot. If a graphics object is supplied to the `target` keyword,
;        then that information will be extracted and used. In this way, the order in which
;        things are plotted is no longer important.
;     linestyle: in, optional, type=integer, default=0
;        The line style for drawing the line.
;     psym: in, optional, type=integer
;        Any normal IDL PSYM values, plus any value supported by the Coyote Library
;        routine cgSYMCAT. An integer between 0 and 46.
;     symcolor: in, optional, type=string
;        The name of the symbol color. By default, the same as the `COLOR` keyword.
;     symsize: in, optional, type=float, default=1.0
;        The symbol size.
;     thick: in, optional, type=float, default = 1.0
;        The thickness of the line.
;-
FUNCTION weOverplot::INIT, x, y, $
;weOPlot Keywords
COLOR=color, $
CURRENT = current, $
DIMENSION=dimension, $
LINESTYLE=linestyle, $
PSYM=psym, $
SYMCOLOR=symcolor, $
SYMSIZE=symsize, $
TARGET=target, $
THICK=thick, $

;cgOPlot Keywords
CHARSIZE=charsize, $

;OPlot Keywords
NSUM = nsum, $
POLAR = polar, $
CLIP = clip, $
NOCLIP = noclip, $
T3D = t3d, $
ZVALUE = zvalue, $
_REF_EXTRA = extra
    Compile_Opt idl2
    
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        MrPrintF, 'LogErr'
        RETURN, 0
    ENDIF
    
    ; Check parameters.
    IF N_Params() EQ 0 THEN BEGIN
        Print, "USE SYNTAX: obj_new('weOPlot', x, y)"
        RETURN, 0
    ENDIF
    
    nx = N_Elements(x)
    ny = N_Elements(y)
    dims = Size(x, /DIMENSIONS)
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

    ;Get the size of the dependent variable
    dims = Size(dep, /DIMENSIONS)

;---------------------------------------------------------------------
;Set Defaults ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    ;The dimension not being plotted.
    case dimension of
        0: xdim = 0
        1: xdim = 2
        2: xdim = 1
    endcase
        
    ;Number of defaults to use.
    IF xdim EQ 0 THEN nDefaults = 1 ELSE nDefaults = dims[xdim-1]

    ;Pick a set of default colors so not everything is the same color.
    default_colors = ['opposite', 'Blue', 'Forest_Green', 'Red', 'Magenta', 'Orange']
    IF nDefaults GT 5 THEN default_colors = [default_colors, Replicate('opposite', nDefaults-5)]
    IF nDefaults EQ 1 THEN d_color = default_colors[0] ELSE d_color = default_colors[1:nDefaults]

    ;Set Defaults
    setDefaultValue, color, d_color
    setDefaultValue, linestyle, Replicate(0, nDefaults)
    setDefaultValue, psym, Replicate(0, nDefaults)
    setDefaultValue, symcolor, color
    setDefaultValue, symsize, Replicate(1.0, nDefaults)
    setDefaultValue, thick, Replicate(1.0, nDefaults)
    setDefaultValue, skip, 1, /BOOLEAN

;---------------------------------------------------------------------
;Set Parameters //////////////////////////////////////////////////////
;---------------------------------------------------------------------
        
    ; Load the object.
    self.dep       = Ptr_New(/ALLOCATE_HEAP)
    self.indep     = Ptr_New(/ALLOCATE_HEAP)
    self.charsize  = Ptr_New(/ALLOCATE_HEAP)
    self.clip      = Ptr_New(/ALLOCATE_HEAP)
    self.color     = Ptr_New(/ALLOCATE_HEAP)
    self.linestyle = Ptr_New(/ALLOCATE_HEAP)
    self.noclip    = Ptr_New(/ALLOCATE_HEAP)
    self.nsum      = Ptr_New(/ALLOCATE_HEAP)
    self.polar     = Ptr_New(/ALLOCATE_HEAP)
    self.psym      = Ptr_New(/ALLOCATE_HEAP)
    self.symcolor  = Ptr_New(/ALLOCATE_HEAP)
    self.symsize   = Ptr_New(/ALLOCATE_HEAP)
    self.t3d       = Ptr_New(/ALLOCATE_HEAP)
    self.thick     = Ptr_New(/ALLOCATE_HEAP)
    self.zvalue    = Ptr_New(/ALLOCATE_HEAP)
    
    ; Graphics objects
    self.target = Ptr_New(/ALLOCATE_HEAP)

    ;If REFRESH=1 three things happen: If the call to MrGrAtom is
    ;   1. before here, none of the pointers are valid and calls to SetProperty by MrGrAtom
    ;      cause errors.
    ;   2. here, then when MrGrAtom calls the SetProperty
    ;      method, none of the data will be loaded into the object.
    ;   3. after the call to SetProperty so that all of the data is loaded, the initial
    ;      call to SetProperty will not have a valid self.window property
    ;
    ;To fix problem 1 and 3, put the call to MrGrAtom here. To fix problem 2,
    ;turn Refresh off.
    if keyword_set(current) then begin
        theWin = GetMrWindows(/CURRENT)
        theWin -> GetProperty, REFRESH=init_refresh
        theWin -> Refresh, /DISABLE
    endif

    ;Graphic Atom
    if self -> MrGrAtom::INIT(CURRENT=current, _EXTRA=extra) eq 0 then $
        message, 'Unable to initialize MrGrAtom.'
    
    ;Set Object Properties
    self -> SetProperty, INDEP = indep, $
                         DEP = dep, $                         
                         ;weOPlot Keywords
                         COLOR=color, $
                         DIMENSION=dimension, $
                         LINESTYLE=linestyle, $
                         PSYM=psym, $
                         SYMCOLOR=symcolor, $
                         SYMSIZE=symsize, $
                         TARGET=target, $
                         THICK=thick, $
                         ;cgOPlot Keywords
                         CHARSIZE=charsize, $
                         ;OPlot Keywords
                         NSUM = nsum, $
                         POLAR = polar, $
                         CLIP = clip, $
                         NOCLIP = noclip, $
                         T3D = t3d, $
                         ZVALUE = zvalue
    
    ;Refresh the graphics?
    if keyword_set(current) $
        then theWin -> Refresh, DISABLE=~init_refresh $
        else self.window -> Draw
    
    RETURN, 1
END


;+
; A wrapper for the cgOverPlot class.
; 
; :Params:
;     class: out, optional, type=struct
;        The class definition as a structure variable. Occasionally useful.
;-
PRO weOverplot__Define, class

    class = { weOverplot, $
              INHERITS MrGrAtom, $
              
              ;Data Properties
              dep: Ptr_New(), $      ; The dependent data.
              indep: Ptr_New(), $    ; The independent data.
              
              ;weOPlot Properties
              color: Ptr_New(), $    ; The color of the overplotted lines.
              dimension: 0, $        ; The dimension over which to plot.
              target: Ptr_New(), $   ; The graphic on which to overplot.
              linestyle: Ptr_New(), $; The linestyle of the overplotted lines.
              psym: Ptr_New(), $     ; The symbol of the overplotted line.
              symcolor: Ptr_New(), $ ; The symbol color.
              symsize: Ptr_New(), $  ; The size of the line symbol.
              thick: Ptr_New(), $, $ ; The thickness of the overplotted line.
              
              ;cgOPlot Properties
              charsize: Ptr_New(), $
              
              ;oPlot Properties
              nsum: Ptr_New(), $
              polar: Ptr_New(), $
              clip: Ptr_New(), $
              noclip: Ptr_New(), $
              t3d: Ptr_New(), $
              zvalue: Ptr_New() $
            }
            
END