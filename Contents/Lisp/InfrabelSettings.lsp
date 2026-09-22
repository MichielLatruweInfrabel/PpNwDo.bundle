(defun C:InfrabelSetup(/ FilesObj paths mypaths newpath appType) 
	; Created by Wouter Van Assche, with help from Kurt Decock and the typical online specialists 
	; applicable for AutoCAD and Variants/Verticals (ACAD, LT, C3D, Map3D, Electrical, Mechanical currently supported)
	; lisp that sets certain paths correctly - it does not affect, nor is it affected by profiles or other personal settings
	; it will be reapplied after a software reset and can also be manually invoked from within AutoCAD
	; Desktop Shortcut for each program will invoke a firststart script on each launch running this lisp (the specific one needs to be copied to "Public Desktop" after install of the program)
	; The lisp and script are not specific (or specify internally) but desktop shortcuts are

	(vl-load-com)

	; make new support paths from existing + new

	(setq Filesobj  (vla-get-files  (vla-get-preferences (vlax-get-acad-object))))
  
        (setq paths (vla-get-SupportPath Filesobj))
	(setq mypaths "C:\\Autodesk_2025\\support;C:\\Autodesk_2025\\fonts;")
	(setq newpath (strcat mypaths paths))
	(vla-put-SupportPath Filesobj newpath)

	; savepath
	(vla-put-AutoSavepath Filesobj "C:\\Temp\\Autodesk_Temp")

	; printers config
	(vla-put-PrinterConfigPath Filesobj "C:\\Autodesk_2025\\Plotters")

	; printers style sheet
	(vla-put-PrinterStyleSheetPath Filesobj "C:\\Autodesk_2025\\Plotters\\Plot Styles")

	; printer drv's
	(vla-put-PrinterDescPath Filesobj "C:\\Autodesk_2025\\Plotters\\PMP Files")

	; print spooler
	(vla-put-PrintSpoolerPath Filesobj "C:\\Temp\\")

	; template location
	(vla-put-TemplateDwgPath Filesobj "C:\\Autodesk_2025\\template")

	; template preferred - currently not wanted
	; (vla-put-QnewTemplateFile Filesobj "C:\\Autodesk_2025\\template\\acadiso.dwt")

	; toolpalette location
	(vla-put-ToolpalettePath Filesobj "C:\\Autodesk_2025\\support\\ToolPalette")
	
	; Temp directory 
	(vla-put-TempFilePath Filesobj "C:\\Temp\\Autodesk_Temp")

	; xref temp path
	(vla-put-TempXrefPath Filesobj "C:\\Temp\\Autodesk_Temp")

	; Log file path
	(vla-put-LogFilePath Filesobj "C:\\Temp\\Autodesk_Temp")
	
	; xref plot-publish log path
	(vla-put-PlotLogFilePath Filesobj "C:\\Temp\\Autodesk_Temp")
  
;; Part where customization is filled in, based on which ACAD vertical is used
    ; Retrieve the AutoCAD version
    (setq appType (vlax-product-key))

	; Determine the AutoCAD variant and set main customization File accordingly -- Currently not wanted, issues after reset software for Mechanical and Civil - they start with plain ACAD profile
;    (cond 
;     ((vl-string-search "ACADLT" appType)(vla-put-MENUFILE Filesobj "C:\\Autodesk_2025\\support\\acadlt.cuix")) ;;AutoCAD LT overlaps with AutoCAD "01:" - only LT will be taken into account if this is "true"
;	  ((vl-string-search "00:" appType)(vla-put-MENUFILE Filesobj "C:\\Autodesk_2025\\support\\C3D.cuix")) ;;Civil3D
;	  ((vl-string-search "01:" appType)(vla-put-MENUFILE Filesobj "C:\\Autodesk_2025\\support\\acad.cuix")) ;;AutoCAD
;     ((vl-string-search "02:" appType)(vla-put-MENUFILE Filesobj "C:\\Autodesk_2025\\support\\map.cuix")) ;;AutoCAD Map3D
;     ((vl-string-search "05:" appType)(vla-put-MENUFILE Filesobj "C:\\Autodesk_2025\\support\\acadm.cuix")) ;;AutoCAD Mechanical
;     ((vl-string-search "07:" appType)(vla-put-MENUFILE Filesobj "C:\\Autodesk_2025\\support\\acade.cuix")) ;;AutoCAD Electrical
;     (t (alert"This version of AutoCAD is not recognized"))
;    )
	; confirm version in AutoCAD - check if it worked
  (princ appType)(princ)
	; end use of Filesobj
	(vlax-release-object Filesobj)

	(princ " -> All Done")(princ)
  );end defun
