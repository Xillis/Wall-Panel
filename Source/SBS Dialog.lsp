;;Wall Properties dialog function
(defun SBS_WallProperties ( PassTrough / PassTrough DFlag Spoints panel)
	(print "start SBS_WallProperties")
	(start_image "Wimage")
		(fill_image 0 0 
			(dimx_tile "Wimage")
			(dimy_tile "Wimage")
			-15
		)
		(if (/= (setq Wpoints(cadr (assoc "Wall Points" PassTrough))) nil)
				(SBS-WIMAGE Wpoints)
			(progn
				(vector_image 0 0 (dimx_tile "Wimage") (dimy_tile "Wimage") -16)
				(vector_image (dimx_tile "Wimage") 0 0 (dimy_tile "Wimage") -16)
			)
		)
	(end_image)
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
			(setq Panel (CWL-START-DIA "SBS_Panel_info" "M"))
		)
	)
(setq passtrough (list (list '"Wall Points" Wpoints) (list '"Panel Info" Panel)))
	(if (= Dflag 1)
		(progn
			(print "accept")
			(print Passtrough)
			(SBS-WALL-PANEL-CALC passTrough)
		)
	)
	(print "end SBS_WallProperties")
	(append passTrough (list (list "DFlag" DFlag)))
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
(defun SBS_Panel_info ( Panel / RPBIT RWBIT RGBIT FINBIT PLIST)
	(print "Start SBS_Panel_info")
	(SETQ
		RPBIT 47 ;;Panel bit Reference
		RWBIT 16128 ;;Width Bit Reference
		RGBIT 507904 ;;Gauge Bit Reference
		RSBIT 1572864 ;;Profile Bit Reference
	)
	(print Panel)
	;;(if (null panel)
	(CWL-DDBCOAD RPBIT "SBS-PANEL-INFO" "Panel_type")
	(SETQ FINBIT 0)
	(action_tile "Panel_type"
		"(SBS-DIA-BITREST FINBIT RPBIT RWBIT RGBIT RSBIT \"SBS-PANEL-INFO\")"
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
	(list (list "DFlag" DFlag) (list '"Panel info" ))
)

;;Resets dialogue list based on the supplied FINBIT and reference bits RETERNS A NEW FINBIT
;;used with Panel information action_tile calls
(defun SBS-DIA-BITREST (FINBIT KEYBIT RWBIT RGBIT RSBIT UTABLE / CELLVALUE ELIST )
	(setq CELLVALUE $value)
	(setq ELIST (CWL-BITLIST KEYBIT UTABLE))
	(PRINT (SETQ LINE (NTH (READ CELLVALUE) ELIST))) 
	(CWL-DDBCOAD (LOGAND (LAST LINE) RWBIT) "SBS-PANEL-INFO" "Width")
	(CWL-DDBCOAD (LOGAND (LAST LINE) RGBIT) "SBS-PANEL-INFO" "Gauge")
	(CWL-DDBCOAD (LOGAND (LAST LINE) RSBIT) "SBS-PANEL-INFO" "Profile")
)
