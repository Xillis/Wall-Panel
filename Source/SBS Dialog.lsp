;;Wall Properties dialog function
(defun SBS_WallProperties ( PASSTHROUGH / PASSTHROUGH DFlag Spoints panel Wpoints)
	(print "start SBS_WallProperties")
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
		(SETQ PANEL '("Panel info" 663553) 
		)
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
	)
	(setq PASSTHROUGH (list (list '"Wall Points" Wpoints) PANEL))
	(if (= Dflag 1)
			(SBS-WALL-PANEL-CALC PASSTHROUGH)
	)
	(print "end SBS_WallProperties")
	(append PASSTHROUGH (list (list "DFlag" DFlag)))
)

;;Wall image calculations
(DEFUN SBS-WImage ( Points / Points Dpoint IPList LPoints ct 2Ponts)
	(print "start SBS-WImage")
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
	(print "end SBS-WImage")
)

;;Panel information collection dialog Box
(defun SBS_Panel_info ( Panel / RPBIT RWBIT RGBIT PBIT PLIST WBIT PRBIT GBIT )
	(print "Start SBS_Panel_info")
	(SETQ
		RPBIT 47 ;;Panel bit Reference
		RWBIT 16128 ;;Width Bit Reference
		RGBIT 507904 ;;Gauge Bit Reference
		RSBIT 1572864 ;;Profile Bit Reference
	)
	(setq 
		PBIT (LOGAND (cadr (assoc "Panel info" Panel)) RPBIT)
		WBIT (LOGAND (cadr (assoc "Panel info" Panel)) RWBIT)
		PRBIT (LOGAND (cadr (assoc "Panel info" Panel)) RSBIT)
		GBIT (LOGAND (cadr (assoc "Panel info" Panel)) RGBIT)
	)
	(CWL-DDBCOAD RPBIT "SBS-PANEL-INFO" "Panel_type")
	(set_tile "Panel_type"
		(CWL-TILESET PBIT RPBIT "SBS-PANEL-INFO")
	)
	(SBS-DIA-PAINSET
		(READ (CWL-TILESET PBIT RPBIT "SBS-PANEL-INFO"))
	RPBIT RWBIT  WBIT RGBIT GBIT RSBIT PRBIT "SBS-PANEL-INFO")
	(action_tile "Panel_type"
		"(SETQ PBIT (SBS-DIA-PAINSET (READ $value) RPBIT RWBIT WBIT RGBIT GBIT RSBIT PRBIT \"SBS-PANEL-INFO\"))"
	)
	(action_tile "Width"
		"(SETQ WBIT (SBS-DIA-GENSET PBIT RWBIT \"SBS-PANEL-INFO\"))"
	)
	(action_tile "Profile"
		"(SETQ PRBIT (SBS-DIA-GENSET PBIT RSBIT \"SBS-PANEL-INFO\"))"
	)
	(action_tile "Gauge"
		"(SETQ GBIT (SBS-DIA-GENSET PBIT RGBIT \"SBS-PANEL-INFO\"))"
	)
	(action_tile "accept"
		(STRCAT
			"(PRINT \"accept\")
			(done_dialog 1)"
		)
	)
	(action_tile "cancel"
		(strcat 
			"(print \"cancel\")
			(done_dialog 0)"
		)
	)
	(setq DFlag (start_dialog))
	(print "end SBS_Panel_info")
	(list (list "DFlag" DFlag) (list '"Panel info"  (+ PBIT WBIT PRBIT GBIT)))
)

;;Generates the list data for gauge width and profile based on the panel chosen. renters the BIT coad for the panel
(defun SBS-DIA-PAINSET (PVALUE RPBIT RWBIT WBIT RGBIT GBIT RSBIT PRBIT UTABLE / LINE)
	(PRINT "START SBS-DIA-PAINSET")
	(SETQ LINE (NTH PVALUE (CWL-BITLIST RPBIT UTABLE)))
	(CWL-DDBCOAD (LOGAND (LAST LINE) RWBIT) "SBS-PANEL-INFO" "Width")
	(IF (= (LOGAND (LAST LINE) WBIT) 0)
		(SET_TILE "Width" "0")
		(SET_TILE "Width" (CWL-TILESET WBIT (logand (LAST LINE) RWBIT) "SBS-PANEL-INFO"))
	)
	(CWL-DDBCOAD (LOGAND (LAST LINE) RGBIT) "SBS-PANEL-INFO" "Gauge")
	(IF (= (LOGAND (LAST LINE) GBIT) 0)
		(SET_TILE "Gauge" "0")
		(SET_TILE "Gauge" (CWL-TILESET GBIT (logand (LAST LINE) RGBIT) "SBS-PANEL-INFO"))
	)
	(CWL-DDBCOAD (LOGAND (LAST LINE) RSBIT) "SBS-PANEL-INFO" "Profile")
	(IF (= (LOGAND (LAST LINE) PRBIT) 0)
		(SET_TILE "Profile" "0")
		(SET_TILE "Profile" (CWL-TILESET PRBIT (logand (LAST LINE) RSBIT) "SBS-PANEL-INFO"))
	)
	(PRINT "END SBS-DIA-PAINSET")
	(CAR LINE)
)

;; creates a list based on a set bit list and renters the first value of the item at $value in the list 
(defun SBS-DIA-GENSET (PBIT GENBIT UTABLE / )
(PRINT "START SBS-DIA-GENSET")
(SETQ LINE (NTH (READ $value) (CWL-BITLIST (LOGAND GENBIT (LAST (CAR (CWL-BITLIST PBIT UTABLE)))) UTABLE)))
(PRINT "END SBS-DIA-GENSET")
(CAR LINE)
)
