;;Panal dialog inishilaization 
(defun SBS_PANEL_INFO ( PASSTHROUGH / PANEL-INFO PASSTHROUGH)
	(print "Start SBS-PANEL-INFO")
	(if (null (setq PANEL-INFO (cadr (assoc "Panel info" PASSTHROUGH))))
		(setq PANEL-INFO (car (CWL-START-DIA "SBS_PANEL" "S" PANEL-INFO)))
	)
	(if (not (null PANEL-INFO)) 
		(setq PANEL-INFO
			(CWL-START-DIA 
				(nth 2 (car (CWL-BITTOLIST (cadr PANEL-INFO) "SBS-PANEL-INFO" )))
				"M"
				(list PANEL-INFO)
			)
		)
	)
	(print "End SBS-PANEL-INFO")
	PASSTHROUGH
)
;; panel selection dialog Box
(defun SBS_PANEL ( PANEL-INFO / PANEL-INFO TOKEN)
(print "Start SBS_PANEL")
	(CWL-DBLIST (CWL-BITTOLIST "all" "SBS-PANEL-INFO") "Panel_type" 1)
	(action_tile "Panel_type" "(setq TOKEN (nth (1-(atoi $value)) (CWL-BITTOLIST \"all\" \"SBS-PANEL-INFO\")))")
	(action_tile "accept" "(setq PANEL-INFO (list \"Panel info\" (car TOKEN))) (done_dialog)")
	(start_dialog)
	(PRINT TOKEN)
(print "End SBS_PANEL")
(list PANEL-INFO)
)

;;Panel information collection dialog Box
(defun SBS_STD_PANEL ( PANEL-INFO / PANEL-INFO )
	(print "Start SBS_STD_PANEL")
	(setq PANEL-INFO (car PANEL-INFO))
	(print PANEL-INFO)
	(CWL-DBLIST (CWL-BITTOLIST "all" "SBS-PANEL-INFO") "Panel_type" 1)
	(set_tile "Panel_type"
		(itoa (1+ (atoi (CWL-TILESET
			(cadr PANEL-INFO)
			"all"
			"SBS-PANEL-INFO"
		))))
	)
	(mode_tile "OtherColour" 1)
	(action_tile "Panel_type" "(setq VALUE $value) (done_dialog 2)")
	(action_tile "accept"
		(strcat
			"(print \"Accept\")"
			"(done_dialog 1)"
		)
	)
	(action_tile "Cancel"
		(strcat 
			"(print \"cancel\")"
			"(done_dialog 0)"
		)
	)
	(setq DFLAG (start_dialog))
	(cond
		((= DFLAG 2)
			(print (NTH (READ VALUE) (CWL-BITTOLIST 2147483647 "SBS-PANEL-INFO"))))
		(T (PRINT "SHIT"))
	)
			
	(print (subst (list "Flag" DFLAG) (assoc "Flag" PASSTHROUGH) PASSTHROUGH))
	(print "end SBS_STD_PANEL")
	(subst (list "Flag" DFLAG) (assoc "Flag" PASSTHROUGH) PASSTHROUGH)
)



;;Panel information collection dialog Box
(defun SBS_STD_PANEL_OLD ( PASSTHROUGH /  PANEL_INFO ALLFLAG )
	(print "Start SBS_STD_PANEL")
	(CWL-DBLIST (CWL-BITTOLIST 2147483647 "SBS-PANEL-INFO") "Panel_type" 1)
	(mode_tile "OtherColour" 1)
	;|(if (assoc "Panel info" PASSTHROUGH)
		(progn
			(set_tile "Panel_type" 
				(CWL-TILESET
					(logand
						(cadr (assoc "Panel info" PASSTHROUGH))
						(cadr (assoc 'PANEL (CWL-CLIST "SBS-REFERENCE-LIST")))
					)	
						(cadr (assoc 'PANEL (CWL-CLIST "SBS-REFERENCE-LIST")))
						"SBS-PANEL-INFO"
				)
			)
			(SBS-DIA-PPSET (assoc "Panel info" PASSTHROUGH))
		)
	)|;
;;	(SBS-DIA-PCHECK PANEL_INFO)
	(action_tile "Panel_type"
		"(if (assoc "Panel info" PASSTHROUGH)
			(print "yes")
			(print "no")
		)"
		;|(strcat
			"(if (null PANEL_INFO)
				(setq PANEL_INFO (list \"Panel info\" (car (nth (read $VALUE) (CWL-BITTOLIST (cadr (assoc 'PANEL (CWL-CLIST \"SBS-REFERENCE-LIST\"))) \"SBS-PANEL-INFO\")))))
				(setq PANEL_INFO (SBS-DIA-GENSET (cadr (assoc 'PANEL (CWL-CLIST \"SBS-REFERENCE-LIST\"))) PANEL_INFO \"SBS-PANEL-INFO\" (read $VALUE)))
			)"
			"(CWL_LISTRESET \"Colour\")"
			"(setq PANEL_INFO (SBS-DIA-PPSET PANEL_INFO))"
			"(setq PANEL_INFO (SBS-DIA-BITFIX PANEL_INFO))"
			"(SBS-DIA-PCHECK PANEL_INFO)"
		)|;
	)
	;|(action_tile "Width"
		(strcat 
			"(setq PANEL_INFO (SBS-DIA-GENSET (cadr (assoc 'WIDTH (CWL-CLIST \"SBS-REFERENCE-LIST\"))) PANEL_INFO \"SBS-PANEL-INFO\" (1- (read $VALUE))))"
			"(CWL_LISTRESET \"Colour\")"
			"(SBS-DIA-PCHECK PANEL_INFO)"
			"(SBS-DIA-PPSET PANEL_INFO)"
		)
	)
	(action_tile "Profile"
		(strcat
			"(setq PANEL_INFO (SBS-DIA-GENSET (cadr (assoc 'PROFILE (CWL-CLIST \"SBS-REFERENCE-LIST\"))) PANEL_INFO \"SBS-PANEL-INFO\" (1- (read $VALUE))))"
			"(SBS-DIA-PCHECK PANEL_INFO)"
		)
	)
	(action_tile "Gauge"
		(strcat
			"(setq PANEL_INFO (SBS-DIA-GENSET (cadr (assoc 'GAUGE (CWL-CLIST \"SBS-REFERENCE-LIST\"))) PANEL_INFO \"SBS-PANEL-INFO\" (1- (read $VALUE))))"
			"(CWL_LISTRESET \"Colour\")"
			"(SBS-DIA-PCHECK PANEL_INFO)"
			"(SBS-DIA-PPSET PANEL_INFO)"
		)
	)
	(action_tile "Othertog" 
		"(if (= $value \"0\")
			(progn
				(mode_tile \"OtherColour\" 1)
				(mode_tile \"Colour\" 0)
			)
			(progn
				(mode_tile \"OtherColour\" 0)
				(mode_tile \"Colour\" 1)
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
		(strcat
			"(setq PANEL_INFO
				(list
					(car PANEL_INFO)
					(cadr PANEL_INFO)
					(vl-prin1-to-string (read (car (nth (read $VALUE) (SBS-COLOUR-CHART (cadr PANEL_INFO) \"COLOUR_CHART\")))))
				)
			)"
			"(SBS-DIA-PCHECK PANEL_INFO)"
		)
	)|;
	;;(action_tile "Feature_list"
	;;	(strcat
	;;		"(setq FBIT (SBS-DIA-GENSET PBIT RFBIT \"SBS-PANEL-INFO\"))"
	;;		"(CWL-DBLIST (SBS-COLOUR-CHART (cadr PANEL_INFO) \"COLOUR_CHART\" ALLFLAG) \"Colour\" 0)"
	;;	)
	;;)
	(action_tile "accept"
		(strcat
			"(print \"Accept\")"
		;;	"(SBS-DIA-PCHECK PANEL_INFO)"
			"(setq PASSTHROUGH
				(if (assoc \"Panel info\" PASSTHROUGH)
					(subst PANEL_INFO (assoc \"Panel info\" PASSTHROUGH) PASSTHROUGH)
					(append (LIST PANEL_INFO) PASSTHROUGH)
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
	(print (subst (list "DFlag" DFlag) (assoc "DFlag" PASSTHROUGH) PASSTHROUGH))
	(print "end SBS_STD_PANEL")
	(subst (list "DFlag" DFlag) (assoc "DFlag" PASSTHROUGH) PASSTHROUGH)
)