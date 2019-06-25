;;GET PANELINFO
(defun SBS-GETPANELINFO ( OBJ / )
(VL-LOAD-COM)
	(SETQ ~PANEL~ nil)
	(FOREACH TAG (vlaX-invoke OBJ 'getattributes)
		(SETQ ~PANEL~ (APPEND ~PANEL~ (LIST (LIST (vla-get-TAGSTRING tag) (vla-get-TEXTSTRING tag)))))
	)
	(FOREACH TAG (vlaX-invoke OBJ 'GetConstantAttributes)
		(SETQ ~PANEL~ (APPEND ~PANEL~ (LIST (LIST (vla-get-TAGSTRING tag) (vla-get-TEXTSTRING tag)))))
	)
)
;;Panel Insertion tool
(defun SBS-PANELINSERT ( BP PL / OBJ PANEL )
(print "start panel insert")
	(setq PANEL (STRCAT(NTH 1 (assoc "PANEL" ~PANEL~))(NTH 1 (assoc "WIDTH" ~PANEL~))))
	(VL-LOAD-COM)
	(PRINT "2")
	(command "_.-insert" PANEL BP "" "" "")
	;|(vla-InsertBlock
		(vla-get-ModelSpace
			(vla-get-ActiveDocument
				(vlax-get-acad-object)
			)
		)
		;;(vlax-variant-value (vla-TranslateCoordinates (vla-get-Utility doc) (vlax-3d-point bp ) acUCS acWORLD :vlax-false))
		(vlax-3d-point bp )
		(if (not(tblsearch "block" PANEL ))
			(findfile (STRCAT PANEL ".dwg"))
			PANEL
		)
		1.0 1.0 1.0 0.0
	)|;
		(PRINT "3")
	(SETQ OBJ (VLAX-ENAME->VLA-OBJECT(ENTLAST)))
	(PRINT "TRANSFORM")
	;;(vla-TransformBy OBJ (vla-GetUCSMatrix (SAVEUCS)))
	;;(vla-update OBJ)
	(vl-some
		'(lambda ( x )
			(if (= "PANEL LENGTH" (strcase (vla-get-propertyname x)))
				(progn
					(vla-put-value x (vlax-make-variant PL (vlax-variant-type (vla-get-value x))))
					(cond (PL) (t))
				)
			)
		)
		(vlax-invoke OBJ 'getdynamicblockproperties)
	)
	(princ ".")
	(SBS-PANELINFO OBJ )
(print "end panel insert")
)

;;Panel info changing tool
(DEFUN SBS-PANELINFO (OBJ / LINE FirstColour CC CINFO POS RL acadObj FirstColour )
(print "start panel info")
	(VL-LOAD-COM)
	(FOREACH TAG (vlaX-invoke OBJ 'getattributes)
		(IF (assoc (vla-get-TAGSTRING tag) ~PANEL~)
		(vla-put-TEXTSTRING tag (NTH 1 (assoc (vla-get-TAGSTRING tag) ~PANEL~)))
		)
	)
	(setq 
		CINFO (VL-GET-RESOURCE "Colour_Chart")
		CC (NTH 1 (assoc "COLOUR" ~PANEL~))
		LINE (NTH 0 (READ CINFO))
		POS 1		
	)
	(while (/= CC LINE)
		(setq 
			RL (read (substr CINFO POS )) 
			LINE (nth 0 RL)
			POS (+ POS 2)
			POS (+ POS (VL-STRING-POSITION (ASCII "\n") (substr CINFO POS)))
		)
	)
	(setq CC (STRCAT (NTH 0 RL) " " (NTH 1 RL)))
	(setq acadObj (vlax-get-acad-object))
	(setq FirstColour (vlax-create-object (strcat "AutoCAD.AcCmColor." (substr (getvar "ACADVER")1 2))))
	(PRINT FIRSTCOLOUR)
	(print CC)
	(vla-SetColorBookColor FirstColour "Steelway Cladding Colourbook" CC)
    (vla-put-TrueColor OBJ FirstColour)
    (vlax-release-object FirstColour)
(print "end panel info")
)

(DEFUN SBS-PANELDEF ( PTV WV GV PV CV BSV PID MRK / )
(print "start part def")
	(setq ~PANEL~ (CONS "PANEL" (list(NTH (READ PTV) ~PANEL_LIST~))))
    (SETQ ~PANEL~ 
		(list ~PANEL~ 
			(cons "WIDTH" (list (NTH (READ WV) (NTH 1 ~PANELINFO~))))
			(cons "GAUGE" (list(NTH (READ GV) (NTH 2 ~PANELINFO~))))
			(cons "PROFILE" (list(NTH (READ PV) (NTH 3 ~PANELINFO~))))
			(cons "COLOUR"	(list(NTH (READ CV) ~COLOUR_LIST~)))
			(cons "BASE" (list BSV))
			(cons "PART" (list PID))
			(cons "MARK" MRK)
		)
	)
(print "end part def")
)

(DEFUN SBS-BITSET ( VAR BIT / )
(print "start bit set")
	(COND
		((AND (= VAR "1") (ZEROP (LOGAND ~PART~ BIT))) (SETQ ~PART~ (+ ~PART~ BIT)))
		((AND (= VAR "0") (NOT (ZEROP (LOGAND ~PART~ BIT)))) (SETQ ~PART~ (- ~PART~ BIT)))
	)
(print "start bit set")
)

(defun SBS-PART-LIST ( / LINE RL FEATURES-BITLIST_FILE)
(print "start part list")
	(SETQ
		FBINFO (VL-GET-RESOURCE "FEATURES-BITLIST")
		POS 1
	)
	(while (/= ~PART~ LINE)
		(setq RL (read (substr FBINFO POS )))
		(if (= RL NIL)
			(SETQ LINE ~PART~)
			(PROGN
				(SETQ
					LINE (nth 0 RL)
					POS (+ POS 2)
					POS (+ POS (VL-STRING-POSITION (ASCII "\n") (substr FBINFO POS)))
				)
			)
		)
	)
	(IF (= RL nil)
		(SET_TILE "PPID" "")
		(SET_TILE "PPID" (NTH 1 RL))
	)
(print "end part list")
)

;;dialog list tools
;;===================================

;;Panel List generation 
(DEFUN SBS_PANEL_LIST ( WTYPE / MLIST PPINFO POS )
(print "start panel list")
	(SETQ 
		~PANEL_LIST~ nil
		PPINFO (VL-GET-RESOURCE "panel_properties")
		MLIST ""
		POS 1
	)
	(while (/= MLIST NIL)
			(setq MLIST (READ (substr PPINFO POS )))
		(if (/= MLIST NIL)
			(PROGN
				(SETQ POS (+ POS 2))
				(IF (VL-SOME '(LAMBDA (x)  (= x WTYPE)) (NTH 5 MLIST))
					(SETQ
						MLIST (nth 0 MLIST)
						~PANEL_LIST~ (append ~PANEL_LIST~ (list MLIST))
					)
				)
				(SETQ POS (+ POS (VL-STRING-POSITION (ASCII "\n") (substr PPINFO POS))))
			)
		)
	)
	(START_LIST "Panel_type")
		(MAPCAR 'ADD_LIST ~PANEL_LIST~)
	(END_LIST)
(print "end panel list")
)
;;-----------------------------------------------------------------------------------------------------------------------
;;Panel Colour List Generation
(DEFUN SBS_COLOUR_LIST (PT WV GV / CLIST CCHART CDIA CD CINFO CC RL POS LINE COLOUR_LIST)
	(SETQ CLIST "")
	(SETQ CC 
		(COND 
			((= "StormSeal" PT) "SS")
			((AND (= "StrucSeal" PT) (= "36" (NTH (READ WV) (NTH 1 ~PANELINFO~)))) "SS")
			((AND (= "StrucSeal" PT) (= "30" (NTH (READ WV) (NTH 1 ~PANELINFO~)))) "SS30")
			((AND (OR (= "VersaSeal" PT) (= "DiamondSeal" PT)) (= "29" (NTH (READ GV) (NTH 2 ~PANELINFO~)))) "DS29")
			((AND (OR (= "VersaSeal" PT) (= "DiamondSeal" PT)) (= "26" (NTH (READ GV) (NTH 2 ~PANELINFO~)))) "DS26")
			((AND (OR (= "VersaSeal" PT) (= "DiamondSeal" PT)) (= "24" (NTH (READ GV) (NTH 2 ~PANELINFO~)))) "DS24")
			((AND (= "RTL-24" PT) (= "24" (NTH (READ GV) (NTH 2 ~PANELINFO~)))) "RTL24")
			((AND (= "RTL-24" PT) (= "22" (NTH (READ GV) (NTH 2 ~PANELINFO~)))) "RTL20")
			((AND (= "TechLoc" PT) (= "22" (NTH (READ GV) (NTH 2 ~PANELINFO~)))) "RTL24")
			((AND (= "TechLoc" PT) (= "20" (NTH (READ GV) (NTH 2 ~PANELINFO~)))) "TL20")
			((AND (= "TechLoc" PT) (= "24" (NTH (READ GV) (NTH 2 ~PANELINFO~)))) "TL24")
			((= "LinerSeal" PT) "LS")
			((AND (= PT "Trim") ( = WV "29")) "TRIM29")
			((AND (= PT "Trim") ( = WV "26")) "TRIM26")
			((AND (= PT "Trim") ( = WV "24")) "TRIM24")
			(t "All")
		)
	)
	(SETQ
		CINFO (VL-GET-RESOURCE "Colour_matrix")
		POS 1
	)
	(while (/= CC LINE)
		(setq RL (read (substr CINFO POS )))
		(if (= RL nil)
			(SETQ LINE CC)
			(PROGN
				(SETQ
					LINE (nth 0 RL)
					POS (+ POS 2)
					POS (+ POS (VL-STRING-POSITION (ASCII "\n") (substr CINFO POS)))
				)
			)
		)
	)
	(SETQ RL (NTH 1 RL))
	(SETQ ~COLOUR_LIST~ NIL)
	(SETQ
		CCHART (VL-GET-RESOURCE "Colour_Chart")
		POS 1
		LINE 0
	)
	(while (/= CLIST NIL)
		(setq CLIST (read (substr CCHART POS )))
		(if (AND (/= CLIST NIL) (VL-SOME '(LAMBDA (x)  (= x (ITOA LINE))) RL))
			(PROGN
				(SETQ
					CD (STRCAT (NTH 0 CLIST) " " (NTH 1 CLIST))
					CLIST (nth 0 CLIST)
					~COLOUR_LIST~ (append ~COLOUR_LIST~ (list CLIST))
					CDIA (APPEND CDIA (LIST CD))
				)
			)
		)
		(if (/= CLIST NIL)
			(SETQ
				LINE (1+ LINE)
				POS (+ POS 2)
				POS (+ POS (VL-STRING-POSITION (ASCII "\n") (substr CCHART POS)))
			)
		)	
	)
	(START_LIST "Colour")
		(MAPCAR 'ADD_LIST CDIA)
	(END_LIST)
)

;;Dialog tile population
;;-------------------------
(defun SBS_PANEL_PROPERTIES ( PT / PPINFO POS LINE WIDTH GAUGE PROFILE )
	(SETQ
		PPINFO (VL-GET-RESOURCE "panel_properties")
		POS 1
	)
	(while (/= PT LINE)
		(setq ~PANELINFO~ (read (substr PPINFO POS )))
		(if (= ~PANELINFO~ NIL)
			(SETQ LINE PT)
			(PROGN
				(SETQ
					LINE (nth 0 ~PANELINFO~)
					POS (+ POS 2)
					POS (+ POS (VL-STRING-POSITION (ASCII "\n") (substr PPINFO POS)))
				)
			)
		)
	)
	;;width
	(setq WIDTH  (nth 1 ~PANELINFO~))
	(START_LIST "Width")
		(MAPCAR 'ADD_LIST WIDTH)
	(END_LIST)
	(SET_TILE "Width" "0")
	(if (= (length WIDTH) 1)
		(mode_tile "Width" 1)
		(mode_tile "Width" 0)
	)
	;;Gauge
	(SETQ GAUGE (NTH 2 ~PANELINFO~))
	(START_LIST "Gauge")
		(MAPCAR 'ADD_LIST GAUGE)
	(END_LIST)
	;;Profile
	(SETQ PROFILE (NTH 3 ~PANELINFO~))
	(START_LIST "Profile")
		(MAPCAR 'ADD_LIST PROFILE)
	(END_LIST)
	(IF (= (LENGTH PROFILE) 1)
		(mode_tile "Profile" 1)
		(mode_tile "Profile" 0)
	)
)

(DEFUN SAVEUCS ( / DOC UTILITY CUUCS UCSS )
	(VL-LOAD-COM)
	(SETQ DOC (vla-get-ActiveDocument(vlax-get-acad-object)))
	(setq UCSs (vla-get-UserCoordinateSystems doc))
	(setq utility (vla-get-Utility DOC))
	(SETQ CUUCS (vla-item (vla-get-UserCoordinateSystems doc) 0)) 
	(PRINT CUUCS)
;;(SETQ 
;;	ORGIN (vla-GetVariable doc "UCSORG")
;;	XDIR (vla-TranslateCoordinates utility (vla-GetVariable doc "UCSXDIR") acUCS acWorld :vlax-false)
;;		YDIR (vla-TranslateCoordinates utility (vla-GetVariable doc "UCSYDIR") acUCS acWorld :vlax-false)
;;		)
;;	(vla-PUT-ActiveUCS doc (vla-item (vla-get-UserCoordinateSystems doc) 0))
	;;(setq CUUCS (vla-get-ActiveUCS doc))
;;	(SETQ CUUCS (vla-Add UCSs ORGIN XDIR YDIR "currentUCS"))
;;	(setq CUUCS (vla-get-ActiveUCS doc))
		
		
	;|(if (= (vlax-variant-value (vla-GetVariable doc "UCSNAME")) "")
        (progn
	        (setq utility (vla-get-Utility DOC))
			(SETQ CUUCS
				(vla-Add UCSs
					(vla-GetVariable doc "UCSORG")
					(vla-TranslateCoordinates utility (vla-GetVariable doc "UCSXDIR") acUCS acWorld :vlax-false)
					(vla-TranslateCoordinates utility (vla-GetVariable doc "UCSYDIR") acUCS acWorld :vlax-false)
					"currentUCS"
				)
			)
        )
    )|;
	(PRINT (strcat "The point has the following coordinates:"
                   "\nWCS: " (rtos (vlax-safearray-get-element (vlax-variant-value(vla-GetUCSMatrix CUUCS)) 0) 2) ", "
                             (rtos (vlax-safearray-get-element (vlax-variant-value(vla-GetUCSMatrix CUUCS)) 1) 2) ", "
                             (rtos (vlax-safearray-get-element (vlax-variant-value(vla-GetUCSMatrix CUUCS)) 2) 2)
							)
							
					)
	CUUCS
	)