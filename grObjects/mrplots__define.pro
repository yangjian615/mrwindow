; docformat = 'rst'
;
; NAME:
;       MrPlotS__Define
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
;   The purpose of this method is to create an object out of the cgPlot routine.
;
; :Examples:
;   Plot an Nx2 array as two line plots on a single axis.
;       x = findgen(101)/100
;       y = sin(2*!pi*x)
;       z = cos(2*!pi*x)
;       a = obj_new('MrPlotS', x, [[y],[z]], DIMENSION=2, TITLE='Sin(x) & Cos(x)', $
;                                   COLOR=['black', 'blue'], XTITLE='Time (s)', $
;                                   YTITLE='Amplitude', /DRAW)
;       obj_destroy, a
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
;       05/18/2013  -   Written by Matthew Argall
;       06/28/2013  -   Added the DIMENSION keyword. Removed the AXES and COLORBARS
;                           properties because the former was not being used and the
;                           latter interfered with the COLOR keyword. - MRA
;       07/04/2013  -   Added the COLOR keyword so that each column or row in DIMENSION
;                           can be plotted in a different color. - MRA
;       07/31/2013  -   Added the ConvertCoord method. - MRA
;       08/09/2013  -   Inherit MrIDL_Container - MRA
;       08/10/2013  -   Added the LAYOUT keyword. - MRA
;       08/22/2013  -   Added the NOERASE keyword to Draw. Was forgetting to set the
;                           position property in SetProperty. Fixed. - MRA
;       08/23/2013  -   Added the IsInside method. - MRA
;       08/30/2013  -   Missed the SYMCOLOR keyword. Now included. - MRA
;       09/08/2013  -   Number of default colors now matches the number of dimensions
;                           being plotted. - MRA
;                           PLOT_INDEX keyword in IsAvailable now works. - MRA
;       09/27/2013  -   Use N_Elements instead of N_Params in case `Y` is present but 
;                           undefined. Position and layout properties are now handled
;                           by MrGraphicAtom. Renamed from MrPlotSObject to MrPlotS. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to draw the plot in the draw window. The plot is
;   first buffered into the pixmap for smoother opteration. (By allowing motion events
;   to copy from the pixmap instead of redrawing the plot, the image does not flicker.)
;-
pro MrPlotS::Draw, $
CONTINUE = continue, $
NOERASE  = noerase
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif

    ;Now draw the plot to the pixmap
    self -> doPlot, NOERASE=noerase, CONTINUE=continue
    
    ;Draw the other items
    oContained = self -> Get(/ALL, COUNT=count)
    if count gt 0 then for i = 0, count - 1 do oContained[i] -> Draw, /NOERASE
end


;+
;   The purpose of this method is to do the actual plotting. Basically, having this here
;   merely to saves space in the Draw method.
;-
pro MrPlotS::doPlot, $
CONTINUE=continue, $
NOERASE=noerase
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    if n_elements(noerase) eq 0 then noerase = *self.noerase

    ;Draw the plot.
    cgPlotS, *self.xcoords, *self.ycoords, *self.zcoords, $

             ;cgPlotS Properties
             COLOR      = *self.color, $
             MAP_OBJECT =  self.map_object, $
             PSYM       = *self.psym, $
             SYMCOLOR   = *self.symcolor, $
             SYMSIZE    = *self.symsize, $

             ;PlotS Properties
             CONTINUE   =       continue, $
             CLIP       = *self.clip, $
             DATA       = *self.data, $
             DEVICE     = *self.device, $
             NORMAL     = *self.normal, $
             LINESTYLE  = *self.linestyle, $
             NOCLIP     = *self.noclip, $
             T3D        = *self.t3d, $
             THICK      = *self.thick, $
             Z          = *self.zvalue
end


;+
;   The purpose of this method is to retrieve object properties
;
; :Keywords:
;-
pro MrPlotS::GetProperty, $
XCOORDS=xcoords, $
YCOORDS=ycoords, $
ZCOORDS=zcoords, $

;cgPlotS Properties
COLOR=color, $
MAP_OBJECT=map_object, $
PSYM=psym, $
SYMCOLOR=symcolor, $
SYMSIZE=symsize, $

;PlotS Properties
CLIP=clip, $
DATA=data, $
DEVICE=device, $
NORMAL=normal, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
T3D=t3d, $
THICK=thick, $
ZVALUE=zvalue

    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Data Properties
    if arg_present(xcoords) ne 0 then xcoords = *self.xcoords
    if arg_present(ycoords) ne 0 then ycoords = *self.ycoords
    if arg_present(zcoords) ne 0 then zcoords = *self.zcoords

    ;cgPlotS Properties
    if arg_present(color)     ne 0 then color     = *self.color
    if arg_present(psym)      ne 0 then psym      = *self.psym
    if arg_present(symcolor)  ne 0 then symcolor  = *self.symcolor
    if arg_present(symsize)   ne 0 then symsize   = *self.symsize
    
    ;PlotS Properties
    if arg_present(clip)      ne 0 then clip      = *self.clip
    if arg_present(data)      ne 0 then data      = *self.data
    if arg_present(device)    ne 0 then device    = *self.device
    if arg_present(normal)    ne 0 then normal    = *self.normal
    if arg_present(linestyle) ne 0 then linestyle = *self.linestyle
    if arg_present(noclip)    ne 0 then noclip    = *self.noclip
    if arg_present(t3d)       ne 0 then t3d       = *self.t3d
    if arg_present(thick)     ne 0 then thick     = *self.thick
    if arg_present(zvalue)    ne 0 then zvalue    = *self.zvalue
    
    ;Map Object
    if arg_present(map_object) ne 0 then if obj_valid(self.map_object) $
        then map_object = self.map_object $
        else map_object = obj_new()
end


;+
;   The purpose of this method is to set object properties. 
;
; :Keywords:
;-
pro MrPlotS::SetProperty, $
XCOORDS=xcoords, $
YCOORDS=ycoords, $
ZCOORDS=zcoords, $
DRAW=draw, $

;cgPlotS Properties
COLOR=color, $
MAP_OBJECT=map_object, $
PSYM=psym, $
SYMCOLOR=symcolor, $
SYMSIZE=symsize, $

;PlotS Properties
CLIP=clip, $
DATA=data, $
DEVICE=device, $
NORMAL=normal, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
T3D=t3d, $
THICK=thick, $
ZVALUE=zvalue
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;Data Properties
    if n_elements(xcoords) ne 0 then *self.xcoords = xcoords
    if n_elements(ycoords) ne 0 then *self.ycoords = ycoords
    if n_elements(zcoords) ne 0 then *self.zcoords = zcoords

    ;cgPlotS Properties
    if n_elements(color)     ne 0 then *self.color = color
    if n_elements(psym)      ne 0 then *self.psym = psym
    if n_elements(symcolor)  ne 0 then *self.symcolor = symcolor
    if n_elements(symsize)   ne 0 then *self.symsize = symsize
    
    ;PlotS Properties
    if n_elements(clip)      ne 0 then *self.clip = clip
    if n_elements(data)      ne 0 then *self.data = data
    if n_elements(device)    ne 0 then *self.device = device
    if n_elements(normal)    ne 0 then *self.normal = normal
    if n_elements(linestyle) ne 0 then *self.linestyle = linestyle
    if n_elements(noclip)    ne 0 then *self.noclip = noclip
    if n_elements(t3d)       ne 0 then *self.t3d = t3d
    if n_elements(thick)     ne 0 then *self.thick = thick
    if n_elements(zvalue)    ne 0 then *self.zvalue = zvalue
    
    ;Map Object
    if n_elements(map_object) ne 0 then begin
        ;Destroy the current map object
        obj_destroy, self.map_object
        
        ;Set the new object
        if obj_valid(map_object) $
            then self.map_object = map_object $
            else self.map_object = obj_new()
    endif
    
    ;Draw?
    if keyword_set(draw) then self -> draw
end


;+
;   Clean up after the object is destroyed -- destroy pointers and object references.
;-
pro MrPlotS::cleanup
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return
    endif
    
    ;free all pointers
    ptr_free, self.xcoords
    ptr_free, self.ycoords
    ptr_free, self.zcoords

    ;cgPlotS Properties
    ptr_free, self.color
    ptr_free, self.psym
    ptr_free, self.symcolor
    ptr_free, self.symsize

    ;PlotS Properties
    ptr_free, self.clip
    ptr_free, self.data
    ptr_free, self.device
    ptr_free, self.normal
    ptr_free, self.linestyle
    ptr_free, self.noclip
    ptr_free, self.t3d
    ptr_free, self.thick
    ptr_free, self.zvalue
    
    ;Destroy Map Objects
    if obj_valid(self.map_object) then obj_destroy, self.map_object

    ;Cleanup the superclass.
    self -> MrIDL_Container::Cleanup
end


;+
;
; :Params:
;       XCOORDS:            in, required, type=numeric
;                           A vector or scalar argument providing the X components of the
;                               points to be connected. If only one argument is specified,
;                               X must be an array of either two or three vectors
;                               (i.e., (2,*) or (3,*)). In this special case, X[0,*] are
;                               taken as the X values, X[1,*] are taken as the `Y` values,
;                               and X[2,*] are taken as the `Z` values.
;       YCOORDS:            in, optional, type=numeric
;                           Y coordinate(s) of the points to be connected.
;       ZCOORDS:            in, optional, type=numeric
;                           Z coordinate(s) of the points to be connected.
;
; :Uses:
;   Uses the following external programs::
;       MrIDL_Container__Define.pro
;       error_message.pro (Coyote Graphics)
;-
function MrPlotS::init, xcoords, ycoords, zcoords, $
;cgPlotS Properties
COLOR=color, $
DRAW=draw, $
MAP_OBJECT=map_object, $
PSYM=psym, $
SYMCOLOR=symcolor, $
SYMSIZE=symsize, $

;PlotS Properties
CLIP=clip, $
DATA=data, $
DEVICE=device, $
NORMAL=normal, $
LINESTYLE=linestyle, $
NOCLIP=noclip, $
T3D=t3d, $
THICK=thick, $
ZVALUE=zvalue
    compile_opt idl2

    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = error_message()
        return, 0
    endif

;---------------------------------------------------------------------
;Superclass Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------

    if self -> MrIDL_Container::Init() eq 0 then return, 0

;---------------------------------------------------------------------
;Keywords ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Allocate heap for the variables
    self.xcoords    = ptr_new(/ALLOCATE_HEAP)
    self.ycoords    = ptr_new(/ALLOCATE_HEAP)
    self.zcoords    = ptr_new(/ALLOCATE_HEAP)

    ;cgPlotS Properties
    self.color      = ptr_new(/ALLOCATE_HEAP)
    self.psym       = ptr_new(/ALLOCATE_HEAP)
    self.symcolor   = ptr_new(/ALLOCATE_HEAP)
    self.symsize    = ptr_new(/ALLOCATE_HEAP)
  
    ;PlotS Properties
    self.clip       = ptr_new(/ALLOCATE_HEAP)
    self.data       = ptr_new(/ALLOCATE_HEAP)
    self.device     = ptr_new(/ALLOCATE_HEAP)
    self.normal     = ptr_new(/ALLOCATE_HEAP)
    self.linestyle  = ptr_new(/ALLOCATE_HEAP)
    self.noclip     = ptr_new(/ALLOCATE_HEAP)
    self.t3d        = ptr_new(/ALLOCATE_HEAP)
    self.thick      = ptr_new(/ALLOCATE_HEAP)
    self.zvalue     = ptr_new(/ALLOCATE_HEAP)
    
    ;Objects
    self.map_object = obj_new()

;---------------------------------------------------------------------
;Set Object Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Set the object properties
    self -> setProperty, XCOORDS=xcoords, $
                         YCOORDS=ycoords, $
                         ZCOORDS=zcoords, $
                         ;cgPlotS Properties
                         COLOR=color, $
                         MAP_OBJECT=map_object, $
                         PSYM=psym, $
                         SYMCOLOR=symcolor, $
                         SYMSIZE=symsize, $
                         ;PlotS Properties
                         CLIP=clip, $
                         DATA=data, $
                         DEVICE=device, $
                         NORMAL=normal, $
                         LINESTYLE=linestyle, $
                         NOCLIP=noclip, $
                         T3D=t3d, $
                         THICK=thick, $
                         ZVALUE=zvalue

    ;Draw?
    if keyword_set(draw) then self -> draw

    return, 1
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrPlotS__define, class
    compile_opt idl2
    
    class = { MrPlotS, $
              inherits MrIDL_Container, $
             
              ;Data Properties
              xcoords: ptr_new(), $         ;x-coordinates
              ycoords: ptr_new(), $         ;y-coordinates
              zcoords: ptr_new(), $         ;z-coordinates

              ;cgPlotS Properties
              color: ptr_new(), $
              map_object: obj_new(), $
              psym: ptr_new(), $
              symcolor: ptr_new(), $
              symsize: ptr_new(), $
              
              ;PlotS Properties
              clip: ptr_new(), $
              data: ptr_new(), $
              device: ptr_new(), $
              normal: ptr_new(), $
              linestyle: ptr_new(), $
              noclip: ptr_new(), $
              t3d: ptr_new(), $
              thick: ptr_new(), $
              zvalue: ptr_new() $
            }
end