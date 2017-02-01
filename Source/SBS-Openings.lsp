(DEFUN C:SBS-OPENING ( / CMD DTRIM PANELSET DWIDTH DHEIGHT IP URP )
	(DEFUN *error* ( msg )
		(if CMD (setvar 'CMDECHO CMD))
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
	(SETQ DTRIM (OPENING_DIA))
	(PRINT DTRIM)
	(SETQ CMD (GETVAR "CMDECHO"))
	(vla-StartUndoMark 
		(vla-get-ActiveDocument 
			(vlax-get-acad-object)
		)
	)
	(SETVAR "CMDECHO" 0)
	(setq
		DWIDTH (NTH 1 (NTH 0 DTRIM))
		DHEIGHT (NTH 2 (NTH 0 DTRIM))
	)
	(SETQ 
		IP (GETPOINT "\nSelect lower left corner of opening: ")
		URP (list (+ (car IP) DWIDTH) (+ (cadr IP) DHEIGHT) 0.0)
	)
	(PRINT (NTH 4 DTRIM))
	(IF (= (NTH 3 DTRIM) "1") (SBS-REMOVEPANEL IP URP DWIDTH DHEIGHT))
	(vla-EndUndoMark 
		(vla-get-ActiveDocument 
			(vlax-get-acad-object)
		)
	)
	(SETVAR "CMDECHO" CMD)
	(PRINC)
)


(DEFUN SBS-REMOVEPANEL ( IP URP OPENINGW OPENINGH / BP WIDTH BASE )
(PRINT "REMOVE")
	(command "_zoom" IP URP) 
	(if (SSGET "_C" IP URP '((0 . "INSERT")))
		(progn
			(vlax-for OBJECT (setq BLOCK (vla-get-activeselectionset (vla-get-activedocument (vlax-get-acad-object))))
				(SBS-GETPANELINFO OBJECT)
				(SETQ 
					BP (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint OBJECT)))
					WIDTH (distof (NTH 1 (assoc "WIDTH" ~PANEL~)) 4)
					BASE (distof (NTH 1 (assoc "BASE" ~PANEL~)) 4)
				)
				(IF (NOT(OR (> (DISTANCE (LIST (CAR BP) 0.0 0.0) (LIST (CAR IP) 0.0 0.0)) (- OPENINGW WIDTH)) (> (DISTANCE (LIST (CAR BP) 0.0 0.0) (LIST (+(CAR IP) OPENINGW) 0.0 0.0)) OPENINGW)))
					(PROGN 
						(vl-some
							'(lambda ( x )
								(if (= "PANEL LENGTH" (strcase (vla-get-propertyname x)))
										(vla-put-value x (vlax-make-variant (- (vlax-variant-value (vla-get-value x)) (- OPENINGH BASE))))
								)
							)
							(vlax-invoke OBJECT 'getdynamicblockproperties)
						)
						(VLA-MOVE OBJECT (vlax-3D-point BP) (vlax-3D-point (LIST (CAR BP) (+ (CADR BP) (- OPENINGH BASE)) (CADDR BP))))
						(SETQ ~PANEL~ (VL-REMOVE (ASSOC "BASE" ~PANEL~) ~PANEL~))
						(SETQ ~PANEL~ (APPEND ~PANEL~ (LIST (LIST "BASE" (RTOS (- OPENINGH BASE) 4)))))
						(SBS-PANELINFO OBJECT)
					)
				)
			)
			(vla-delete BLOCK)
        )
    )
	(command "_zoom" "P")
)

;;Dialog Control
;;------------------------------------------------

(DEFUN OPENING_DIA ( / DIA_ID GV DFlag PT1 PT2 PANEL DTRIM )
(print "Start Dia")
	(setq DIA_ID (load_dialog "SBS-Dialog.dcl"))
	(setq DFlag 2)	
	(while (>= DFlag 2)
		(IF (NOT (new_dialog "SBS_OPENING" DIA_ID))
			(EXIT)
		)
		(print "loaded")
		(START_LIST "TYPE")
			(ADD_LIST "Louver Trim")
			(ADD_LIST "Mandoor Trim")
			(ADD_LIST "Mandoor Trim /W Header")
			(ADD_LIST "OHD Trim")
			(ADD_LIST "OHD /W Wrap Trim")
			(ADD_LIST "X2 Mandoor Trim")
			(ADD_LIST "X2 Mandoor Trim /W Header")
		(END_LIST)
		(print "type list")
		(START_LIST "Gauge")
			(ADD_LIST "29")
			(ADD_LIST "26")
			(ADD_LIST "24")
		(END_LIST)
		(print "Gauge list")
		(SET_TILE "Gauge" "1")
		(Print "start colour list")
		(SBS_COLOUR_LIST "Trim" "26" "0")
		(print "end Colour list")
		(action_tile "MPG"
			(STRCAT
				"(if (= $value \"0\")(mode_tile \"Gauge\" 0)(mode_tile \"Gauge\" 1))"
			)
		)
		(action_tile "TYPE"
			(STRCAT
				"(IF (OR(= $value \"3\")(= $value \"4\"))(set_tile \"REMOVE\" \"1\")(set_tile \"REMOVE\" \"0\"))"
			)
		)
		(action_tile "MPC"
			(STRCAT
				"(if (= $value \"0\")
					(mode_tile \"Colour\" 0)
					(mode_tile \"Colour\" 1)
				)"
			)
		)
		(action_tile "Gauge"
			(STRCAT
				"(setq GV $value)"
				"(COND
					((= GV \"0\" ) (SBS_COLOUR_LIST \"Trim\" \"29\" \"0\"))
					((= GV \"1\" ) (SBS_COLOUR_LIST \"Trim\" \"26\" \"0\"))
					((= GV \"2\" ) (SBS_COLOUR_LIST \"Trim\" \"24\" \"0\"))
				)"
			)
		)
		(if (and (/= PT1 nil) (/= PT2 nil))
			(progn
				(set_tile "WIDTH" (rtos(distance (LIST (CAR PT1) 0.0 0.0) (LIST (CAR PT2) 0.0 0.0))))
				(SET_TILE "HEIGHT" (rtos(DISTANCE (LIST 0.0 (CADR PT1) 0.0) (LIST 0.0 (CADR PT2) 0.0 ))))
			)
		)
		(print "action set")
		(action_tile "Panel" "(done_dialog 4)")
		(action_tile "PICKP" "(done_dialog 5)")
		(action_tile "accept" 
			(STRCAT 
				"(SETQ DTRIM (SBS-DTRIM))"
				"(done_dialog 1)"
			)
		)
		(action_tile "cancel" "(done_dialog 0)")
		(print "start box")
		(setq DFlag (start_dialog))
		(cond
			((= DFlag 4) (SETQ PANEL (VLAX-ENAME->VLA-OBJECT(CAR(ENTSEL "Select Panel: ")))))
			((= DFlag 5) 
				(PROGN
					(SETQ 
						PT1 (GETPOINT "\nSelect Corner of Opening: ")
						PT2 (GETCORNER PT1 "\nSelect Opposite Corner: ")
					)
				)
			)
		)
	)
	(unload_dialog DIA_ID)
	(print "END box")
	DTRIM
)

(defun SBS-DTRIM ( / DTYPE DWIDTH DHEIGHT TGAUGE TCOLOUR RPANEL ASS )
	(SETQ
		DTYPE (GET_TILE "TYPE")
		DWIDTH (DISTOF (GET_TILE "WIDTH") 4)
		DHEIGHT (DISTOF (GET_TILE "HEIGHT") 4)
		TGAUGE (GET_TILE "Gauge")
		TCOLOUR (NTH (READ (GET_TILE "Colour")) ~COLOUR_LIST~)
		RPANEL (get_tile "REMOVE")
		ASS (GET_TILE "ASSEMBLY")
	)
	(LIST (LIST DTYPE DWIDTH DHEIGHT) TGAUGE TCOLOUR RPANEL ASS)
)