;+
; This function returns a cgLegendItem object reference.
; 
; :Keywords:
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by cgLegendItem::Init is also accepted for
;                           keyword inheritance.
;
; :Returns:
;     thisLegend:       out, required, type=object
;                       An object reference to the cgLegendItem. If the object was intialized
;                           incorrectly, then 0 will be returned.
;-
FUNCTION weLegend, $
_REF_EXTRA = extra
    Compile_Opt idl2
    
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = cgErrorMsg()
        RETURN, 0
    ENDIF

    ;Create the legend object
    thisLegend = obj_new('cgLegendItem', _EXTRA=extra)
    
    ;Return the object reference
    return, thisLegend
end