;MrWindow
.comp ./mrwindow__define.pro
.comp ./mrwindow.pro

;Abstract Classes
.comp ./AbstractClasses/mrabstractanalysis__define.pro
.comp ./AbstractClasses/mrabstractarrow__define.pro
.comp ./AbstractClasses/mrabstractaxis__define.pro
.comp ./AbstractClasses/mrabstractcdf__define.pro
.comp ./AbstractClasses/mrabstractcolorbar__define.pro
.comp ./AbstractClasses/mrabstractimage__define.pro
.comp ./AbstractClasses/mrabstractlegend__define.pro
.comp ./AbstractClasses/mrabstractoverplot__define.pro
.comp ./AbstractClasses/mrabstractplot__define.pro
.comp ./AbstractClasses/mrsaveas__define.pro
.comp ./AbstractClasses/mrabstracttext__define.pro
.comp ./AbstractClasses/mrmanipulate__define.pro
.comp ./AbstractClasses/mrcursor__define.pro
.comp ./AbstractClasses/mrzoom__define.pro

;CDF Reader
.comp ./cdf-reader/cdf_info__define.pro
.comp ./cdf-reader/cdf_plot__define.pro
.comp ./cdf-reader/cdf_read__define.pro
.comp ./cdf-reader/cdf_read.pro
.comp ./cdf-reader/cdf_varselect__define.pro
.comp ./cdf-reader/utilities/convert_time.pro
.comp ./cdf-reader/utilities/datetime_to_epoch.pro
.comp ./cdf-reader/utilities/dissectdatetime.pro
.comp ./cdf-reader/utilities/dissectdate.pro
.comp ./cdf-reader/utilities/dissecttime.pro
.comp ./cdf-reader/utilities/has_tag.pro
.comp ./cdf-reader/utilities/isnumeric.pro
.comp ./cdf-reader/utilities/mrisa.pro
.comp ./cdf-reader/utilities/type_to_format_code.pro

;Graphic Manager
.comp ./grManager/mrcreategraphic__define.pro
.comp ./grManager/mrgrlayout__define.pro
.comp ./grManager/mridl_container__define.pro
.comp ./grManager/mrplotmanager__define.pro

;Graphic Superclasses
.comp ./grSuperclasses/mrdatacoords__define.pro
.comp ./grSuperclasses/mrgratom__define.pro
.comp ./grSuperclasses/mrlayout__define.pro
.comp ./grSuperclasses/wegraphicskeywords__define.pro

;Graphic Objects
.comp ./grObjects/mrcontour__define.pro
.comp ./grObjects/mrimage__define.pro
.comp ./grObjects/mrplot__define.pro
.comp ./grObjects/mrplots__define.pro
.comp ./WileE/wearrow__define.pro
.comp ./WileE/weaxis__define.pro
.comp ./WileE/wecolorbar__define.pro
.comp ./WileE/welegenditem__define.pro
.comp ./WileE/weoverplot__define.pro
.comp ./WileE/wetext__define.pro

;Graphic Functions
.comp ./mrarrow.pro
.comp ./mraxis.pro
.comp ./mrcolorbar.pro
.comp ./mrcontour.pro
.comp ./mrimage.pro
.comp ./mrlegend.pro
.comp ./mrplot.pro
.comp ./mrplots.pro
.comp ./mrtext.pro

;Wile E. Graphics utils
.comp ./WileE/welegend.pro
.comp ./WileE/weoplot.pro
.comp ./WileE/weoverplot.pro
.comp ./WileE/weplot.pro

;Utilities
.comp ./utilities/color_trip.pro
.comp ./utilities/dot_product.pro
.comp ./utilities/epoch_to_ssm.pro
.comp ./utilities/file_extension.pro
.comp ./utilities/getmrwindows.pro
.comp ./utilities/ht_velocity.pro
.comp ./utilities/ismember.pro
.comp ./utilities/load_color.pro
.comp ./utilities/moveplot_gui.pro
.comp ./utilities/mrimage.pro
.comp ./utilities/mrpixelcorners.pro
.comp ./utilities/mrpixeldeltas.pro
.comp ./utilities/mrpixelpoints.pro
.comp ./utilities/mrlayout.pro
.comp ./utilities/time_select.pro   ;Must be before mva.pro to compile correctly
.comp ./utilities/mva.pro
.comp ./utilities/plotpositions_gui.pro
.comp ./utilities/pwd.pro
.comp ./utilities/replace_fillval.pro
.comp ./utilities/rotate_vector.pro
.comp ./utilities/ssm_to_hms.pro
.comp ./utilities/text_gui.pro
.comp ./utilities/time_labels.pro
.comp ./utilities/twod_to_oned_index.pro