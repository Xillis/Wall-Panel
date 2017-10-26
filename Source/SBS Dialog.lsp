;;Wall Properties dialog function
(defun SBS_WallProperties ( PASSTHROUGH / PASSTHROUGH DFlag Spoints panel Wpoints startp)
	;;(print "start SBS_WallProperties")
	(start_image "Wimage")
		(fill_image 0 0 
			(dimx_tile "Wimage")
			(dimy_tile "Wimage")
			-15
		)
		(if (null (setq Wpoints(cadr (assoc "Wall Points" PASSTHROUGH))))
			(progn
				(vector_image 0 0 (dimx_tile "Wimage") (dimy_tile "Wimage") -16)
				(vector_image (dimx_tile "Wimage") 0 0 (dimy_tile "Wimage") -16)
			)
			(SBS-WIMAGE Wpoints)
		)
	(end_image)
	(IF (NULL (setq PANEL (assoc "Panel info" PASSTHROUGH)))
		(SETQ PANEL '("Panel info" 663553 "QC8317"))
	)
	(CWL-DBLIST
		(CWL-BITTOLIST (CADR PANEL) "SBS-PANEL-INFO")
		"panelinfo" 1
	)
	;;(CWL-DDBCOAD (CADR PANEL) "SBS-PANEL-INFO" "panelinfo")
	(if (null (setq startp (cadr (assoc "Wall info" PASSTHROUGH))))
		(SETQ STARTP "l")
	)
	(if (listp startp)
		(set_tile "Startp" "p")
		(set_tile "Startp" startp)
	)
	(action_tile "Startp" 
		"(if (or (= $value \"l\") (= $value \"r\"))
			(setq startp $value)
			(progn
				(setq v1 $value)
				(Done_dialog 6)
			)
		)"
	)
	(action_tile "Ppoints" "(Done_dialog 3)")
	(action_tile "Spoly" "(Done_dialog 4)")
	(action_tile "SPType" "(Done_dialog 5)")
	(action_tile "accept" "(done_dialog 1)")
	(action_tile "cancel" "(print \"Cancel\")(done_dialog 0)")
	(setq DFlag (start_dialog))
	(cond
		((= DFlag 3) 
			(setq Wpoints (CWL-FPOINT (SBS_Wallpoints) "Left"))
		)
		((= DFlag 4)
			(setq Wpoints (CWL-FPOINT (CWL-PPOINTS) "Left"))
		)
		((= Dflag 5)
			(setq Panel (CAR(CWL-START-DIA "SBS_Panel_info" "M" Panel )))
		)
		((= Dflag 6)
			(SETQ startp (SBS-STARTP v1))
		)
	)
	(setq PASSTHROUGH (list (list '"Wall Points" Wpoints) PANEL (list '"Wall info" startp )))
	(if (= Dflag 1)
			(SBS-WALL-PANEL-CALC PASSTHROUGH)
	)
	;;(print "end SBS_WallProperties")
	(append PASSTHROUGH (list (list "DFlag" DFlag)))
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
(defun SBS_Panel_info ( Panel / RPBIT RWBIT RGBIT PBIT PLIST WBIT PRBIT DFLAG GBIT BITREFLIST ALLFLAG)
	;;(print "Start SBS_Panel_info")
	(SETQ BITREFLIST (CWL-CLIST "SBS-REFERENCE-LIST"))
	(SETQ
		RPBIT (CADR (ASSOC 'PANEL BITREFLIST)) ;;Panel bit Reference
		RWBIT (CADR (ASSOC 'WIDTH BITREFLIST)) ;;Width Bit Reference
		RGBIT (CADR (ASSOC 'GAUGE BITREFLIST)) ;;Gauge Bit Reference
		RSBIT (CADR (ASSOC 'PROFILE BITREFLIST)) ;;Profile Bit Reference
		RFBIT (CADR (ASSOC 'FEATURS BITREFLIST)) ;;Features Bit Reference
	)
	(setq 
		PBIT (LOGAND (cadr (assoc "Panel info" Panel)) RPBIT)
		WBIT (LOGAND (cadr (assoc "Panel info" Panel)) RWBIT)
		PRBIT (LOGAND (cadr (assoc "Panel info" Panel)) RSBIT)
		GBIT (LOGAND (cadr (assoc "Panel info" Panel)) RGBIT)
		FBIT (LOGAND (cadr (assoc "Panel info" Panel)) RFBIT)
	)
	(CWL-DBLIST
		(CWL-BITTOLIST RPBIT "SBS-PANEL-INFO")
		"Panel_type" 1
	)
	(set_tile "Panel_type"
		(CWL-TILESET PBIT RPBIT "SBS-PANEL-INFO")
	)
	(SBS-DIA-PAINSET
		(READ (CWL-TILESET PBIT RPBIT "SBS-PANEL-INFO"))
		RPBIT RWBIT  WBIT RGBIT GBIT RSBIT PRBIT RFBIT FBIT "SBS-PANEL-INFO")
	(CWL-DBLIST
		(SBS-COLOUR-CHART (CADR (CAR PANEL)) "COLOUR_CHART" ALLFLAG)
		"Colour" 0
	)
	(action_tile "Panel_type"
		(STRCAT
			"(SETQ PBIT (SBS-DIA-PAINSET (READ $value) RPBIT RWBIT WBIT RGBIT GBIT RSBIT PRBIT RFBIT FBIT \"SBS-PANEL-INFO\"))"
			"(CWL-DBLIST (SBS-COLOUR-CHART (+ PBIT WBIT PRBIT GBIT FBIT) \"COLOUR_CHART\" ALLFLAG) \"Colour\" 0)"
		)
	)
	(action_tile "Width"
		(STRCAT
			"(SETQ WBIT (SBS-DIA-GENSET PBIT RWBIT \"SBS-PANEL-INFO\"))"
			"(CWL-DBLIST (SBS-COLOUR-CHART (+ PBIT WBIT PRBIT GBIT FBIT) \"COLOUR_CHART\" ALLFLAG) \"Colour\" 0)"
		)
	)
	(action_tile "Profile"
		"(SETQ PRBIT (SBS-DIA-GENSET PBIT RSBIT \"SBS-PANEL-INFO\"))"
	)
	(action_tile "Gauge"
		(STRCAT
			"(SETQ GBIT (SBS-DIA-GENSET PBIT RGBIT \"SBS-PANEL-INFO\"))"
			"(CWL-DBLIST (SBS-COLOUR-CHART (+ PBIT WBIT PRBIT GBIT FBIT) \"COLOUR_CHART\" ALLFLAG) \"Colour\" 0)"
		)
	)
	(action_tile "Feature_list"
		(strcat
			"(setq FBIT (SBS-DIA-GENSET PBIT RFBIT \"SBS-PANEL-INFO\"))"
			"(CWL-DBLIST (SBS-COLOUR-CHART (+ PBIT WBIT PRBIT GBIT FBIT) \"COLOUR_CHART\" ALLFLAG) \"Colour\" 0)"
		)
	)
	(action_tile "accept"
		(STRCAT
			"(PRINT \"accept\")
			(SETQ PANEL (list '\"Panel info\"  (+ PBIT WBIT PRBIT GBIT FBIT)))
			(done_dialog 1)"
		)
	)
	(action_tile "cancel"
		(strcat 
			"(print \"cancel\")
			(SETQ PANEL (assoc \"Panel info\" Panel))
			(done_dialog 0)"
		)
	)
	(setq DFlag (start_dialog))
	;;(print "end SBS_Panel_info")
	(list (list "DFlag" DFlag) PANEL)
)

;;Generates the list data for gauge width and profile based on the panel chosen. renters the BIT coad for the panel
(defun SBS-DIA-PAINSET (PVALUE RPBIT RWBIT WBIT RGBIT GBIT RSBIT PRBIT RFBIT FBIT UTABLE / LINE)
	;;(PRINT "START SBS-DIA-PAINSET")
	(SETQ LINE (NTH PVALUE (CWL-BITTOLIST RPBIT UTABLE)))
	(CWL-DBLIST
		(CWL-BITTOLIST (LOGAND (LAST LINE) RWBIT) "SBS-PANEL-INFO")
		"Width" 1
	)
	(IF (= (LOGAND (LAST LINE) WBIT) 0)
		(SET_TILE "Width" "0")
		(SET_TILE "Width" (CWL-TILESET WBIT (logand (LAST LINE) RWBIT) "SBS-PANEL-INFO"))
	)
	(CWL-DBLIST
		(CWL-BITTOLIST (LOGAND (LAST LINE) RGBIT) "SBS-PANEL-INFO")
		"Gauge" 1
	)
	(IF (= (LOGAND (LAST LINE) GBIT) 0)
		(SET_TILE "Gauge" "0")
		(SET_TILE "Gauge" (CWL-TILESET GBIT (logand (LAST LINE) RGBIT) "SBS-PANEL-INFO"))
	)
	(CWL-DBLIST
		(CWL-BITTOLIST (LOGAND (LAST LINE) RSBIT) "SBS-PANEL-INFO")
		"Profile" 1
	)
	(IF (= (LOGAND (LAST LINE) PRBIT) 0)
		(SET_TILE "Profile" "0")
		(SET_TILE "Profile" (CWL-TILESET PRBIT (logand (LAST LINE) RSBIT) "SBS-PANEL-INFO"))
	)
	(CWL-DBLIST
		(CWL-BITTOLIST (LOGAND (LAST LINE) RFBIT) "SBS-PANEL-INFO")
		"Feature_list" 1
	)
	;;(PRINT "END SBS-DIA-PAINSET")
	(CAR LINE)
)

;; creates a list based on a set bit list and renters the first value of the item at $value in the list 
(defun SBS-DIA-GENSET (PBIT GENBIT UTABLE / LINE VALUE)
;;(PRINT "START SBS-DIA-GENSET")
	(IF (NOT (= $VALUE ""))
		(PROGN
			(SETQ
				VALUE $VALUE
				LINE (CAR (NTH (READ VALUE) (CWL-BITTOLIST (LOGAND GENBIT (LAST (CAR (CWL-BITTOLIST PBIT UTABLE)))) UTABLE)))
			)
			(WHILE (/= (STRLEN VALUE) 1)
				(SETQ 
					VALUE (SUBSTR VALUE 3)
					LINE (+ (CAR (NTH (READ VALUE) (CWL-BITTOLIST (LOGAND GENBIT (LAST (CAR (CWL-BITTOLIST PBIT UTABLE)))) UTABLE))) LINE))
			)
		)
	(SETQ LINE 0)
	)
;;(PRINT "END SBS-DIA-GENSET")
LINE
)

;; panel start position routine
(DEFUN SBS-STARTP ( V / V P)
	(setq P (getpoint "select the start point for the panels:")) 
	(print V)
	(print p)
	p
)