;;Wall Properties dialog function
(defun SBS_WallProperties ( PASSTHROUGH / PASSTHROUGH DFlag panel)
	;;(print "start SBS_WallProperties")
	(start_image "Wimage")
		(fill_image 0 0 
			(dimx_tile "Wimage")
			(dimy_tile "Wimage")
			-15
		)
		(if (null (cadr (assoc "Wall Points" PASSTHROUGH)))
			(progn
				(vector_image 0 0 (dimx_tile "Wimage") (dimy_tile "Wimage") -16)
				(vector_image (dimx_tile "Wimage") 0 0 (dimy_tile "Wimage") -16)
			)
			(SBS-WIMAGE (cadr (assoc "Wall Points" PASSTHROUGH)))
		)
	(end_image)
	(IF (assoc "Panel info" PASSTHROUGH)
		(CWL-DBLIST
			(CWL-BITTOLIST (CADR (assoc "Panel info" PASSTHROUGH)) "SBS-PANEL-INFO")
			"panelinfo" 1
		)
		(CWL-DBLIST (list '("No Panel info")) "panelinfo" 0)
	)
	(COND
		((NOT (assoc "Wall info" PASSTHROUGH))
			(SETQ PASSTHROUGH (APPEND PASSTHROUGH (list (LIST "Wall info" "l"))))
			(set_tile "Startp" "l")
		)
		((listp (CADR (assoc "Wall info" PASSTHROUGH)))
			(set_tile "Startp" "p")
		)
		(T
			(set_tile "Startp" (CADR (assoc "Wall info" PASSTHROUGH)))
		)
	)
	(action_tile "Startp" 
		"(SETQ PASSTHROUGH(SUBST (list \"Wall info\" $value) (ASSOC \"Wall info\" PASSTHROUGH) PASSTHROUGH))
		(IF (= $value \"p\")
			(Done_dialog 6)
		)"
	)
	(action_tile "Ppoints" "(Done_dialog 3)")
	(action_tile "Spoly" "(Done_dialog 4)")
	(action_tile "SPType" "(Done_dialog 5)")
	(action_tile "accept" "(done_dialog 1)")
	(action_tile "cancel" "(print \"Cancel\")(done_dialog 0)")
	(setq DFlag (start_dialog))
	(cond
		((= DFLAG 0)
			(setq PASSTHROUGH (LIST '("CANCLE")))
		)
		((= DFlag 3) 
			(setq PASSTHROUGH
				(IF (ASSOC '"Wall Points" PASSTHROUGH)
					(SUBST 
						(list '"Wall Points" (CWL-FPOINT (SBS_Wallpoints) "Left"))
						(ASSOC '"Wall Points" PASSTHROUGH)
					PASSTHROUGH)
					(APPEND (LIST (list '"Wall Points" (CWL-FPOINT (SBS_Wallpoints) "Left"))) PASSTHROUGH)
				)
			)
		)
		((= DFlag 4)
			(setq PASSTHROUGH
				(IF (ASSOC '"Wall Points" PASSTHROUGH)
					(SUBST 
						(list '"Wall Points" (CWL-FPOINT (CWL-PPOINTS) "Left"))
						(ASSOC '"Wall Points" PASSTHROUGH)
					PASSTHROUGH)
					(APPEND (LIST (list '"Wall Points" (CWL-FPOINT (CWL-PPOINTS) "Left"))) PASSTHROUGH)
				)
			)
		)
		((= Dflag 5)
			(SETQ PASSTHROUGH
				(IF (assoc "Panel info" PASSTHROUGH)
					(SUBST (CAR(CWL-START-DIA "SBS_Panel_info" "M" (assoc "Panel info" PASSTHROUGH))) (assoc "Panel info" PASSTHROUGH) PASSTHROUGH)
					(APPEND (LIST (CAR(CWL-START-DIA "SBS_Panel_info" "M" (assoc "Panel info" PASSTHROUGH)))) PASSTHROUGH)
				)
			)
		)
		((= Dflag 6)
			(SETQ PASSTHROUGH 
				(SUBST
					(list "Wall info" (getpoint "select the start point for the panels:"))
					(ASSOC "Wall info" PASSTHROUGH)
				PASSTHROUGH)
			)	
		)
	)
	;;(print "end SBS_WallProperties")
	(SUBST (list "DFlag" DFlag) (ASSOC "DFlag" PASSTHROUGH) PASSTHROUGH)
)

;;Wall image calculations
(DEFUN SBS-WImage ( Points / Points Dpoint IPList LPoints ct 2Ponts)
	;;(print "start SBS-WImage")
	(setq dpoint (CWL-MAXPOINT Points))
	(setq IPList (CWL-ALIST (car DPoint) (caddr DPoint) 0.0 points))
	(setq WX (1- (dimx_tile "Wimage")))
	(setq WY (1- (dimy_tile "Wimage")))
	(if (> (/ WX (- (CADR DPoint) (CAR Dpoint))) (/ WY (- (CADDDR DPoint) (CADDR DPoint))))
		(SETQ ct (/ WY (- (CADDDR DPoint) (CADDR DPoint))))
		(SETQ ct (/ WX (- (CADR DPoint) (CAR Dpoint))))
	)
	(foreach p IPlist
		(setq LPoints (cons (list (FIX(* (car p) ct)) (FIX(+ (* (* (cadr p) ct) -1)WY))) LPoints))
	)
		(setq ct 0)
		(while (/= ct "X")
			(SETQ 2Points (CWL-2POINT LPoints ct))
			(IF (= (NTH (1+ ct) LPoints) nil)
				(setq ct "X")
				(setq ct (1+ ct))
			)
			(vector_image  (car (car 2Points)) (cadr (car 2Points)) (car (cadr 2Points)) (cadr (cadr 2Points)) -16) 
		)
	;;(print "end SBS-WImage")
)

;;Panel information collection dialog Box
(defun SBS_Panel_info ( PASSTHROUGH /  PANEL_INFO ALLFLAG )
;;	(print "Start SBS_Panel_info")
	(CWL-DBLIST
		(CWL-BITTOLIST 
			(CADR (ASSOC 'PANEL 
				(CWL-CLIST "SBS-REFERENCE-LIST")))
			"SBS-PANEL-INFO")
		"Panel_type" 1
	)
	(MODE_TILE "OtherColour" 1)
	(IF (assoc "Panel info" PASSTHROUGH)
		(PROGN
			(SETQ PANEL_INFO (assoc "Panel info" PASSTHROUGH))
			(SET_TILE "Panel_type" 
				(CWL-TILESET
					(LOGAND
						(CADR PANEL_INFO)
						(CADR (ASSOC 'PANEL (CWL-CLIST "SBS-REFERENCE-LIST")))
					)	
						(CADR (ASSOC 'PANEL (CWL-CLIST "SBS-REFERENCE-LIST")))
						"SBS-PANEL-INFO"
				)
			)
			(SBS-DIA-PPSET PANEL_INFO)
		)
	)
	(SBS-DIA-PCHECK PANEL_INFO)
	(action_tile "Panel_type"
		(STRCAT
			"(IF (NULL PANEL_INFO)
				(SETQ PANEL_INFO (LIST \"Panel info\" (CAR (NTH (READ $VALUE) (CWL-BITTOLIST (CADR (ASSOC 'PANEL (CWL-CLIST \"SBS-REFERENCE-LIST\"))) \"SBS-PANEL-INFO\")))))
				(SETQ PANEL_INFO (SBS-DIA-GENSET (CADR (ASSOC 'PANEL (CWL-CLIST \"SBS-REFERENCE-LIST\"))) PANEL_INFO \"SBS-PANEL-INFO\" (READ $VALUE)))
			)"
			"(CWL_LISTRESET \"Colour\")"
			"(SETQ PANEL_INFO (SBS-DIA-PPSET PANEL_INFO))"
			"(SETQ PANEL_INFO (SBS-DIA-BITFIX PANEL_INFO))"
			"(SBS-DIA-PCHECK PANEL_INFO)"
		)
	)
	(action_tile "Width"
		(STRCAT 
			"(SETQ PANEL_INFO (SBS-DIA-GENSET (CADR (ASSOC 'WIDTH (CWL-CLIST \"SBS-REFERENCE-LIST\"))) PANEL_INFO \"SBS-PANEL-INFO\" (1- (READ $VALUE))))"
			"(CWL_LISTRESET \"Colour\")"
			"(SBS-DIA-PCHECK PANEL_INFO)"
			"(SBS-DIA-PPSET PANEL_INFO)"
		)
	)
	(action_tile "Profile"
		(STRCAT
			"(SETQ PANEL_INFO (SBS-DIA-GENSET (CADR (ASSOC 'PROFILE (CWL-CLIST \"SBS-REFERENCE-LIST\"))) PANEL_INFO \"SBS-PANEL-INFO\" (1- (READ $VALUE))))"
			"(SBS-DIA-PCHECK PANEL_INFO)"
		)
	)
	(action_tile "Gauge"
		(STRCAT
			"(SETQ PANEL_INFO (SBS-DIA-GENSET (CADR (ASSOC 'GAUGE (CWL-CLIST \"SBS-REFERENCE-LIST\"))) PANEL_INFO \"SBS-PANEL-INFO\" (1- (READ $VALUE))))"
			"(CWL_LISTRESET \"Colour\")"
			"(SBS-DIA-PCHECK PANEL_INFO)"
			"(SBS-DIA-PPSET PANEL_INFO)"
		)
	)
	(action_tile "Othertog" 
		"(if (= $value \"0\")
			(progn
				(MODE_TILE \"OtherColour\" 1)
				(MODE_TILE \"Colour\" 0)
			)
			(progn
				(MODE_TILE \"OtherColour\" 0)
				(MODE_TILE \"Colour\" 1)
			)
		)"
	)
	(action_tile "allColour"
		(strcat
			"(CWL_LISTRESET \"Colour\")"
			"(CWL-DBLIST (SBS-COLOUR-CHART (cadr PANEL_INFO) \"COLOUR_CHART\") \"Colour\" 0)"
		)
	)
	(action_tile "Colour"
		(STRCAT
			"(SETQ PANEL_INFO
				(LIST
					(CAR PANEL_INFO)
					(CADR PANEL_INFO)
					(VL-PRIN1-TO-STRING (READ (CAR (NTH (READ $VALUE) (SBS-COLOUR-CHART (CADR PANEL_INFO) \"COLOUR_CHART\")))))
				)
			)"
			"(SBS-DIA-PCHECK PANEL_INFO)"
		)
	)
	;;(action_tile "Feature_list"
	;;	(strcat
	;;		"(setq FBIT (SBS-DIA-GENSET PBIT RFBIT \"SBS-PANEL-INFO\"))"
	;;		"(CWL-DBLIST (SBS-COLOUR-CHART (cadr PANEL_INFO) \"COLOUR_CHART\" ALLFLAG) \"Colour\" 0)"
	;;	)
	;;)
	(action_tile "accept"
		(STRCAT
			"(PRINT \"Accept\")"
			"(SBS-DIA-PCHECK PANEL_INFO)"
			"(SETQ PASSTHROUGH
				(IF (assoc \"Panel info\" PASSTHROUGH)
					(SUBST PANEL_INFO (assoc \"Panel info\" PASSTHROUGH) PASSTHROUGH)
					(APPEND (LIST PANEL_INFO) PASSTHROUGH)
				)
			)"
			"(done_dialog 1)"
		)
	)
	(action_tile "Cancel"
		(strcat 
			"(print \"cancel\")"
			"(done_dialog 0)"
		)
	)
	(setq DFlag (start_dialog))
	(PRINT (SUBST (list "DFlag" DFlag) (assoc "DFlag" PASSTHROUGH) PASSTHROUGH))
;;	(print "end SBS_Panel_info")
	(SUBST (list "DFlag" DFlag) (assoc "DFlag" PASSTHROUGH) PASSTHROUGH)
)

;; creates a list based on a set bit list and renters the first value of the item at $value in the list 
(defun SBS-DIA-GENSET ( GENBIT PANEL_INFO UTABLE VALUE / )
	(LIST (CAR PANEL_INFO)
		(LOGIOR
			(- (CADR PANEL_INFO)  (LOGAND (CADR PANEL_INFO) GENBIT))
			(CAR (NTH VALUE
				(IF (= (CADR (ASSOC 'PANEL (CWL-CLIST "SBS-REFERENCE-LIST"))) GENBIT)
					(CWL-BITTOLIST
						(CADR (ASSOC 'PANEL (CWL-CLIST "SBS-REFERENCE-LIST")))
						"SBS-PANEL-INFO"
					)
					(CWL-BITTOLIST
						(LOGAND
							(LAST
								(CAR
									(CWL-BITTOLIST
										(LOGAND
											(CADR (ASSOC 'PANEL (CWL-CLIST "SBS-REFERENCE-LIST")))
											(CADR PANEL_INFO)
										)
										"SBS-PANEL-INFO"
									)
								)
							)
							GENBIT
						)
						"SBS-PANEL-INFO"
					)
				)
			))
		)
	)
)

;;POPULATES DROPDOWN LISTS BASED ON A BIT MASK
(DEFUN SBS-DIA-DLIST (LISTKEY MASK VALUE / ELIST )
	(START_LIST LISTKEY)
		(ADD_LIST " ")
	(END_LIST)
	(CWL-DBLIST
		(SETQ ELIST
			(CWL-BITTOLIST
				(LOGAND
					(LAST
						(NTH VALUE
							(CWL-BITTOLIST
								(CADR
									(ASSOC 'PANEL
										(CWL-CLIST "SBS-REFERENCE-LIST")
									)
								)
							"SBS-PANEL-INFO")
						)
					)
					MASK
				)
			"SBS-PANEL-INFO")
		)
	LISTKEY 1)
	(IF (< (LENGTH ELIST) 2)
		(PROGN
			(MODE_TILE LISTKEY 1)
			(SET_TILE LISTKEY "1")
		)
		(MODE_TILE LISTKEY 0)
	)
)

;;checks for problems with the panel properties dialog
(DEFUN SBS-DIA-PCHECK ( PANEL_INFO / ALERTW )
;;(PRINT "START SBS-DIA-PCHECK")
(SETQ ALERTTEXT "ERROR:\n")
	(IF (NULL PANEL_INFO)
		(SETQ ALERTTEXT (STRCAT ALERTTEXT "No Panel Type Selected\n"))
		(PROGN
			(IF (= (LOGAND (CADR PANEL_INFO) (CADR (ASSOC 'WIDTH (CWL-CLIST "SBS-REFERENCE-LIST")))) 0)
				(SETQ ALERTTEXT (STRCAT ALERTTEXT "No Panel Width Selected\n"))
			)
			(IF (= (LOGAND (CADR PANEL_INFO) (CADR (ASSOC 'GAUGE (CWL-CLIST "SBS-REFERENCE-LIST")))) 0)
				(SETQ ALERTTEXT (STRCAT  ALERTTEXT "No Panel Gauge Selected\n"))
			)
			(IF (= (LOGAND (CADR PANEL_INFO) (CADR (ASSOC 'PROFILE (CWL-CLIST "SBS-REFERENCE-LIST")))) 0)
				(SETQ ALERTTEXT (STRCAT ALERTTEXT "No Panel Profile Selected\n"))
			)
			(if (null (caddr PANEL_INFO))
				(SETQ ALERTTEXT (STRCAT ALERTTEXT "No Colour Selected\n"))
			)
		)
	)
	(IF (= ALERTTEXT "ERROR:\n")
		(PROGN
			(SETQ ALERTTEXT (NULL ALERTTEXT))
			(SET_TILE "Alerttext" " ")
		)
		(SET_TILE "Alerttext" (STRCAT ALERTTEXT))
	)
;;(PRINT "END SBS-DIA-PCHECK")
)

;;FIXIS THE PANEL BIT FOR UNSET BITS
(DEFUN SBS-DIA-BITFIX ( PANEL_INFO / )
	(IF (= (GET_TILE "Width") "1")
		(SETQ PANEL_INFO (SBS-DIA-GENSET (CADR (ASSOC 'WIDTH (CWL-CLIST "SBS-REFERENCE-LIST"))) PANEL_INFO "SBS-PANEL-INFO" 0))
	)
	(IF (= (GET_TILE "Gauge") "1")
		(SETQ PANEL_INFO (SBS-DIA-GENSET (CADR (ASSOC 'GAUGE (CWL-CLIST "SBS-REFERENCE-LIST"))) PANEL_INFO "SBS-PANEL-INFO" 0))
	)
	(IF (= (GET_TILE "Profile") "1")
		(SETQ PANEL_INFO (SBS-DIA-GENSET (CADR (ASSOC 'PROFILE (CWL-CLIST "SBS-REFERENCE-LIST"))) PANEL_INFO "SBS-PANEL-INFO" 0))
	)
PANEL_INFO
)

;;SETS PANAL PROPERTIES LISTS AND SETS TO EXISTING VALUES
(DEFUN SBS-DIA-PPSET (PANEL_INFO / )
;;(print "start SBS-DIA-PPSET")
	(FOREACH l '((WIDTH "Width") (GAUGE "Gauge") (PROFILE "Profile"))
		(SBS-DIA-DLIST (CADR l)
			(CADR (ASSOC (CAR l) (CWL-CLIST "SBS-REFERENCE-LIST")))
			(ATOI (GET_TILE "Panel_type"))
		)
		(IF 
			(> 
				(LOGAND
					(CADR PANEL_INFO)
					(LOGAND	
						(LAST (CAR
							(CWL-BITTOLIST
								(LOGAND
									(CADR (ASSOC 'PANEL (CWL-CLIST "SBS-REFERENCE-LIST")))
									(CADR PANEL_INFO)
								)
								"SBS-PANEL-INFO"
							)
						))
						(CADR (ASSOC (CAR l) (CWL-CLIST "SBS-REFERENCE-LIST")))
					)
				)
			
			0)
			(SET_TILE (CADR l) 
				(ITOA (1+ (ATOI (CWL-TILESET
					(LOGAND
						(CADR PANEL_INFO)
						(CADR (ASSOC (CAR l) (CWL-CLIST "SBS-REFERENCE-LIST")))
					)
					(LOGAND	
						(LAST (CAR
							(CWL-BITTOLIST
								(LOGAND
									(CADR (ASSOC 'PANEL (CWL-CLIST "SBS-REFERENCE-LIST")))
									(CADR PANEL_INFO)
								)
								"SBS-PANEL-INFO"
							)
						))
						(CADR (ASSOC (CAR l) (CWL-CLIST "SBS-REFERENCE-LIST")))
					)
					"SBS-PANEL-INFO"
				))))
			)
			(SETQ PANEL_INFO
				(LIST
					(CAR PANEL_INFO)
					(- 
						(CADR PANEL_INFO)
						(LOGAND
							(CADR PANEL_INFO)
							(LOGAND
								(CADR PANEL_INFO)
								(CADR (ASSOC (CAR l) (CWL-CLIST "SBS-REFERENCE-LIST")))
							)
						)
					)
				)
			)
		)
	)
	(CWL_LISTRESET "Colour")
	(CWL-DBLIST (SBS-COLOUR-CHART (cadr PANEL_INFO) "COLOUR_CHART") "Colour" 0)
	
;;	(print "end SBS-DIA-PPSET")
	PANEL_INFO
)
	 