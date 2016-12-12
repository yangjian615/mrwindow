; docformat = 'rst'
;
; NAME:
;       MrGraphicAtom__Define
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
;   A set of method inherited by all graphic objects.
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
;       2013/11/22  -   Added the WINDOW property, the _SetWindow, Refresh, and Close
;                           methods, and the BUFFER, CURRENT, and NOGUI keywords. - MRA
;       2014/01/24  -   Added the _OverloadPrint method. - MRA
;       2014/03/09  -   Disinherit IDL_Object. Added the _ROI property. - MRA
;       2014/03/10  -   Removed content from _SetWindow into a new _GetWindow method.
;                           Added the HitTest and Select methods. - MRA
;       2014/03/12  -   Added the _GetTarget and Order methods. Removed the IsInside
;                           method. - MRA
;       2014/03/14  -   Added the Save method. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to print information about the object's properties
;   when the PRINT procedure is used.
;-
function MrGrAtom::_OverloadPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, "''"
    endif
    
    ;Class Properties
    hide = string('Hide', '=', self.hide, FORMAT='(a-26, a-2, i1)')
    name = string('Name', '=', self.name, FORMAT='(a-26, a-2, a0)')
    
    ;Combine the results
    result = [ [hide], $
               [name] $
             ]

    ;Return a column so that each property is printed on a separate line.
    return, '  ' + result
end


;+
;   The purpose of this method is to obtain targets for graphics objects. Targets are
;   pulled from the current window.
;
; :Keywords:
;       ALL:                in, optional, type=boolean, default=0
;                           If set, all matching graphics will be returned as targets.
;                               The default is to return a single target.
;       ANY:                in, optional, type=boolean, default=0
;                           If set, and no objects are selected in the window, the search
;                               for targets will be expanded to any available graphic within
;                               the current window. If at least one graphic has been
;                               selected, this keyword is ignored.
;       COUNT:              out, optional, type=integer
;                           Number of targets returned.
;
; :Returns:
;       TARGET:             A graphic (or graphics) that will be targets for other
;                               graphics objects.
;-
function MrGrAtom::_GetTarget, $
ALL=all, $
ANY=any, $
COUNT=count
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, obj_new()
    endif
    
    all = keyword_set(all)
    any = keyword_set(any)
    count = 0

    ;Make sure a window has been assigned.
    if obj_valid(self.window) eq 0 $
        then theWindow = GetMrWindows(/CURRENT) $
        else theWindow = self.window
    
    ;Make sure a window is open
    if obj_valid(theWindow) eq 0 then $
        message, 'No window is currently open. Cannot get targets.'
    
    ;Get selected objects
    target = theWindow -> GetSelect(COUNT=count)
    
    ;Get any available graphic?
    if count eq 0 then begin
        if any then $
            target = theWindow -> Get(/ALL, ISA=['MRPLOT', 'MRIMAGE', 'MRCONTOUR'], COUNT=count)
    endif
    
    ;Return a single graphic?
    if count gt 1 && all eq 0 then begin
        target = target[0]
        count = 1
    endif
    
    ;No target found?
    if count eq 0 then target = obj_new()

    return, target
end


;+
;   Gets the current or a new MrWindow graphics window. By default, the refresh state
;   of a new window is set to 0 so that it will not be created immediately.
;
; :Keywords:
;       CURRENT:            in, optional, type=boolean, default=0
;                           If set, the graphic will be added to the current window. If
;                               not, a new window will be created.
;       BUFFER:             in, optional, type=boolean, default=0
;                           If set, graphics will be directed to a buffer and a window
;                               will not be created. Ignored of `TARGET` or `CURRENT` are
;                               present.
;       TARGET:             in, optional, type=object
;                           A graphic that is to share the same window.
;-
function MrGrAtom::_GetWindow, $
BUFFER=buffer, $
CURRENT=current, $
NOGUI=noGUI, $
TARGET=target, $
WINDOW_TITLE=window_title
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, obj_new()
    endif

    ;Get a window -- from the target, the curent window, or a new window.
    if n_elements(target) gt 0 then begin
        if n_elements(target) eq 1 $
            then target    -> GetProperty, WINDOW=theWindow $
            else target[0] -> GetProperty, WINDOW=theWindow
    endif else if keyword_set(current) then begin
        theWindow = GetMrWindows(/CURRENT)
        if obj_valid(theWindow) eq 0 then theWindow = obj_new('MrWindow', REFRESH=0)
    endif else begin
        theWindow = obj_new('MrWindow', WINDOW_TITLE=window_title, BUFFER=buffer, REFRESH=0)
    endelse

    return, theWindow
end


;+
;   Set the MrWindow object in which the graphic will be displayed.
;
; :Private:
;
; :Params:
;       THEWINDOW:          in, required, type=object
;                           The MrWindow graphics window in which the graphic will be
;                               displayed.
;-
pro MrGrAtom::_SetWindow, theWindow
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif

    self.window = theWindow
end


;+
;   The purpose of this method is to close the window in which the graphic is displayed.
;   Doing so will::
;       - Close the window.
;       - Destroy the MrWindow object reference.
;       - Destroy all objects that were contained in the window.
;       - Destroy the widget, if there was one.
;-
pro MrGrAtom::Close
    obj_destroy, self.window
end


;+
;   Destroy the object.
;-
pro MrGrAtom::Destroy
    obj_destroy, self
end


;+
;   Return the name of the object.
;-
function MrGrAtom::GetName
    return, self.name
end


;+
;   Set properties of the object.
;
; :Keywords:
;       HIDE:           out, optional, type=boolean
;                       If set, the graphic will not be displayed.
;       NAME:           out, optional, type=string
;                       A string specifying the name of the graphic. It can be used
;                           retrieve the graphic using bracket array notation.
;       WINDOW:         out, optional, type=object
;                       The MrWindow object in which the graphic is displayed.
;-
pro MrGrAtom::GetProperty, $
HIDE=hide, $
NAME=name, $
WINDOW=window
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Set GraphicAtom properties
    if arg_present(hide) then hide = self.hide
    if arg_present(name) then name = self.name
    if arg_present(window) then window = self.window
end


;+
;   Determine if the coordinate made by X and Y lies within the object's position.
;
;   Note:
;       This method depends on the graphic having a Position property. Some graphics
;       do not have one and thus must over-ride this method, providing POLYX and POLYY.
;
; :Params:
;       X:              in, required, type=long/lonarr
;                       X coordinates to test
;       Y:              in, required, type=long/lonarr
;                       Y coordinates to test
;
; :Keywords:
;       DIMENSIONS:     in, optional, type=fltarr(2)
;                       The [X,Y] dimensions of a box centered on `X` and `Y` to use
;                           for the hit test. Device coordinates.
;       POLYX:          in, optional, type=lonarr(2)
;                       The x-vertices of a closed polygon, in device coordinates. The
;                           default is to use the Position property of the graphic. Note
;                           that some graphics do not have a Position property and must
;                           over-ride this method.
;       POLYY           in, optional, type=lonarr(2)
;                       The y-vertices of a closed polygon, in device coordinates. The
;                           default is to use the Position property of the graphic. Note
;                           that some graphics do not have a Position property and must
;                           over-ride this method.
;
; :Returns:
;       TF_INSIDE:      Returns true (1) if [x,y] lies within POSITION. False (0) otherwise.
;-
function MrGrAtom::HitTest, x, y, $
DIMENSIONS=dimensions, $
POLYX=polyX, $
POLYY=polyY
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, 0
    endif

    nDims = n_elements(dimensions)
    
    ;Default to the implicit position.
    if n_elements(polyX) eq 0 then begin
        self -> GetProperty, POSITION=position
        coords = self -> ConvertCoord(reform(position, 2, 2), /NORMAL, /TO_DEVICE)
        polyX = [coords[0,0], coords[0,1], coords[0,1], coords[0,0], coords[0,0]]
        polyY = [coords[1,0], coords[1,0], coords[1,1], coords[1,1], coords[1,0]]
    endif

    ;Were dimensions given?
    if nDims gt 0 then begin
        xpoints = [x - dimensions[0]/2, x + dimsnsions[0]/2]
        ypoints = [x - dimensions[1]/2, x + dimsnsions[1]/2]
    endif else begin
        xpoints = x
        ypoints = y
    endelse

    ;Test if the given points are inside the graphic
    tf_hit = inside(xpoints, ypoints, polyX, polyY)
    
    ;Hit?:
    ;   - At least one point fall within the region?
    ;   - Is the 2D polygon entirely within the box?
    ;   - Nothing is in the 2D polygon
    if max(tf_hit) eq 1 then begin
        tf_inside = 1
    endif else if nDims eq 2 && max(tf_hit) eq 0 then begin
        tf_hit = inside(polyX, polyY, xpoints, ypoints)
        if min(tf_hit) eq 1 then tf_inside = 1 else tf_inside = 0
    endif else tf_inside = 0
    
    return, tf_inside
end


;+
;   Set the viewing order of a graphic. Determines the order in which graphics will
;   be drawn. Graphics drawn last are placed on top of graphics drawn first. If no
;   keywords are set, the order will remain unchanged.
;
; :Keywords:
;       BRING_FORWARD:      in, optional, type=boolean, default=0
;                           If set, the graphic will be moved up in the viewing order.
;       BRING_TO_FRONT:     in, optional, type=boolean, defualt=0
;                           If set, the graphic will be brought to the front of the 
;                               viewing order, placing it on top of all others.
;       SEND_BACKWARD:      in, optional, type=boolean, defualt=0
;                           If set, the graphic will be moved down in the viewing order.
;       SEND_TO_BACK:       in, optional, type=boolean, default=0
;                           If set, the graphic will be dropped to the back of the viewing
;                               order, placing it below all others.
;-
pro MrGrAtom::Order, $
BRING_FORWARD=bring_forward, $
BRING_TO_FRONT=bring_to_front, $
SEND_BACKWARD=send_backward, $
SEND_TO_BACK=send_to_back
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif

    ;Get the number of graphics and the index location of the graphic
    nGraphics = self.window -> Count()
    tf_contained = self.window -> IsContained(self, POSITION=oldOrder)
    
    ;BRING FORWARD
    if keyword_set(bring_forward) then begin
        newOrder = oldOrder + 1
        if newOrder lt nGraphics then self.window -> Move, oldOrder, newOrder
        
    ;BRING TO FRONT
    endif else if keyword_set(bring_to_front) then begin
        if oldOrder ne nGraphics-1 then self.window -> Move, oldOrder, nGraphics-1
        
    ;SEND BACKWARD
    endif else if keyword_set(send_backward) then begin
        newOrder = oldOrder - 1
        if oldOrder ne 0 then self.window -> Move, oldOrder, newOrder
        
    ;SEND TO BACK
    endif else if keyword_set(send_to_back) then begin
        if oldOrder ne 0 then self.window -> Move, oldOrder, 0
    endif
end


;+
;   Turn refreshing of the graphics window on or off.
;
; :Keywords:
;       DISABLE:            in, optional, type=boolean, default=0
;                           If set, refreshing of the graphics window will be disabled.
;-
pro MrGrAtom::Refresh, $
DISABLE=disable
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Turn refresh on or off.
    if obj_valid(self.window) then self.window -> Refresh, DISABLE=disable
end


;+
;   Save the display to a file.
;
; :Params:
;       FILENAME:           in, optional, type=string, default='MrWindow.ps'
;                           Name of the file to which graphics output will be saved. The
;                               type of image file created is determined by the extension
;                               given. Options include "BMP", "EPS", "GIF", "JPEG", "JPG",
;                               "PDF", "PNG", "PS", "TIF", and "TIFF".
;-
pro MrGrAtom::Save, filename
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Save the file
    self.window -> Save, filename
end


;+
;   Translate or move a graphic in the x, y, or z direction.
;
; :Params:
;       X:              in, required, type=float/integer
;                       Move the graphic along the X dimension. A value of 0 results in
;                           no traslation.
;       Y:              in, required, type=float/integer
;                       Move the graphic along the Y dimension. A value of 0 results in
;                           no traslation.
;       Z:              in, required, type=float/integer, default=0
;                       Move the graphic along the Z dimension. A value of 0 results in
;                           no traslation.
;
; :Keywords:
;       DATA:           in, optional, type=1, default=0
;                       If set, `X`, `Y`, and `Z` are provided in data coordinates.
;       DEVICE:         in, optional, type=1, default=0
;                       If set, `X`, `Y`, and `Z` are provided in device coordinates.
;       NORMAL:         in, optional, type=1, default=0
;                       If set, `X`, `Y`, and `Z` are provided in normal coordinates.
;-
pro MrGrAtom::Translate, x, y, z, $
DATA=data, $
DEVICE=device, $
NORMAL=normal, $
RESET=reset
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Translate by (x,y,z)
    self.position[[0,2]] += x
    self.position[[1,3]] += y
    if n_elements(z) gt 0 then *self.z += z
end



;+
;   Set the selection state of the graphic.
;
; :Keywords:
;       ADD:            in, optional, type=boolean, default=0
;                       If set, the graphic will be added to the selected items. The
;                           default is to clear the list, then add.
;       ALL:            in, optional, type=boolean, default=0
;                       If set, all items in the graphic window will be selected.
;       CLEAR:          in, optional, type=boolean, default=0
;                       If set, all items will be unselected.
;       TOGGLE:         in, optional, type=boolean, default=0
;                       If set, the selection state of the grahpic will be toggled.
;       UNSELECT:       in, optional, type=boolean, default=0
;                       If set, the graphic will be unselected.
;-
function MrGrAtom::Select, $
ADD=add, $
ALL=all, $
CLEAR=clear, $
TOGGLE=toggle, $
UNSELECT=unselect
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, 0
    endif

    ;Change the selection
    self.window -> _SetSelect, self, ADD=add, ALL=all, CLEAR=clear, TOGGLE=toggle, $
                                     UNSELECT=unselect
end


;+
;   Set properties of the object.
;
; :Keywords:
;       HIDE:           in, optional, type=boolean
;                       If set, the graphic will not be displayed.
;       NAME:           in, optional, type=string
;                       A string specifying the name of the graphic. It can be used
;                           retrieve the graphic using bracket array notation.
;-
pro MrGrAtom::SetProperty, $
HIDE=hide, $
NAME=name
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Set object properties
    if n_elements(hide) gt 0 then self.hide = keyword_set(hide)
    if n_elements(name) gt 0 then self.name = name
end


;+
;   Set properties of the object.
;
; :Params:
;       NEWWINDOW:          in, required, type=object
;                           A valid MrWindow object reference into which the graphic
;                               will be moved.
;
; :Keywords:
;       DESTROY:            in, optional, type=boolean, default=0
;                           Destroy the old MrWindow object once the graphic is removed.
;-
pro MrGrAtom::SwitchWindows, newWindow, $
DESTROY=destroy
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Make sure a valid window was given
    if obj_valid(newWindow) eq 0 || obj_class(newWindow) ne 'MRWINDOW' then $
        message, 'NEWWINDOW must be a valid MrWindow object.'
    
    ;Remove SELF from the current window
    self.window -> Remove, self, DESTROY=0
    
    ;Switch to the new window
    if keyword_set(destroy) then obj_destroy, self.window
    self.window = newWindow
    
    ;Turn refresh off while we add the graphic to the window
    self -> Refresh, /DISABLE
    self.window -> Add, self
end


;+
;   Clean up after the object is destroy
;-
pro MrGrAtom::cleanup
    on_error, 2
    
    ;Cleanup the superclass
    self -> MrDataCoords::Cleanup
end


;+
;   The initialization method.
;
; :Keywords:
;       BUFFER:         in, optional, type=boolean, default=0
;                       If set, graphics will be directed to a buffer and a window
;                           will not be created.
;       CURRENT:        in, optional, type=boolean, default=0
;                       If set, the graphic will be added to the current window. If
;                           not, a new window will be created.
;       NOGUI:          in, optional, type=boolean, default=0
;                       If set, graphics will be displayed in a normal IDL window.
;       HIDE:           in, optional, type=boolean, default=0
;                       If set, the graphic will not be displayed.
;       NAME:           in, optional, type=string, default=Obj_Class(self)
;                       A string specifying the name of the graphic. It can be used
;                           retrieve the graphic using bracket array notation.
;       WINREFRESH:     out, optional, type=boolean
;                       The refresh state of the window must be turned off until the
;                           superclass has been fully initialized. This prevents the
;                           window from calling the Draw method, which would use
;                           properties that do not yet exist. If `CURRENT` is also set,
;                           then this is the initial reset state of the current graphics
;                           window. New windows are always generated with refresh off.
;       WINDOW_TITLE:   in, optional, type=string, default="MrWindow"
;                       Name to be placed on the title bar of the graphics window.
;-
function MrGrAtom::init, $
CURRENT=current, $
HIDE = hide, $
NAME = name, $
WINREFRESH = winRefresh, $
TARGET=target, $
WINDOW_TITLE=window_title
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, 0
    endif
    
    ;Initialize the data coordinate object
    if self -> MrDataCoords::Init() eq 0 then $
        message, 'Could not initialize MrDataCoords.'
    
    ;Defaults
    current = keyword_set(current)
    if n_elements(hide) eq 0 then hide = 0
    if n_elements(name) eq 0 then name = obj_class(self)

    ;Get a window
    theWindow = self -> _GetWindow(TARGET=target, CURRENT=current, WINDOW_TITLE=window_title)
    self -> _SetWindow, theWindow
    
    ;Turn off refreshing while the graphic is added to the window.
    winRefresh = theWindow -> GetRefresh()
    if winRefresh then theWindow -> Refresh, /DISABLE
    theWindow -> Add, self

    ;Set properties here, not via the SetProperty method. Subclasses need to establish
    ;a window beforehand.
    self.hide = hide
    self.name = name
    
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
;       HIDE:           Do not display the graphic?
;       NAME:           Name of the graphic.
;       WINDOW:         MrWindow object in which to display the graphic.
;-
pro MrGrAtom__define, class
    compile_opt strictarr
    
    define = { MrGrAtom, $
               inherits IDL_Object, $
               inherits MrDataCoords, $
               hide:    0B, $
               name:    '', $
               window:  obj_new() $
             }
end