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
	 