;;Wall Properties dialog function
(defun SBS_WallProperties ( PassTrough / Wpoints PassTrough DFlag )
	(print "start SBS_WallProperties")
	(start_image "Wimage")
		(fill_image 0 0 
			(dimx_tile "Wimage")
			(dimy_tile "Wimage")
			-15
		)
		(if (/= (setq Wpoints (assoc "Wall Points" PassTrough)) nil)
			(progn 
				(setq Wpoints (cadr Wpoints))
				(SBS-WIMAGE Wpoints)
			)
			(progn
				(vector_image 0 0 (dimx_tile "Wimage") (dimy_tile "Wimage") -16)
				(vector_image (dimx_tile "Wimage") 0 0 (dimy_tile "Wimage") -16)
			)
		)
	(end_image)
	(action_tile "Ppoints" "(Done_dialog 3)")
	(action_tile "Spoly" "(Done_dialog 4)")
	(action_tile "accept"
		(STRCAT
		"(PRINT \"accept\")
		(done_dialog 1)"
		)
	)
	(action_tile "cancel"
		(strcat 
			"(print \"cancel\")
			(exit)
			(done_dialog 0)"
		)
	)
	(setq DFlag (start_dialog))
	(cond
		((= DFlag 3) 
			(setq Wpoints (list (SBS_Wallpoints)))
		)
		((= DFlag 4)
			(setq Wpoints (list (CWL-FPOINT (CWL-PPOINTS) "Left")))
		)
	)
	(print "end SBS_WallProperties")
	(cons (list "DFlag" DFlag) (list (append '("Wall Points") Wpoints))) 
)

;;Wall image calculations
(DEFUN SBS-WImage ( Points / Dpoint IPList LPoints ct 2Ponts)
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

