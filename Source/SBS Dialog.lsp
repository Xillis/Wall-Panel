;;Wall Properties dialog function
(defun SBS_WallProperties ( PassTrough / Wpoints PassTrough DFlag )
	(print "start SBS_WallProperties")
	(if (/= (setq Wpoints (assoc "Wall Points" PassTrough)) nil)
		(setq Wpoints (cadr Wpoints))
	)
	(start_image "Wimage") 
		(fill_image 0 0 
			(dimx_tile "Wimage")
			(dimy_tile "Wimage")
			-15
		)
		(if (/= Wpoints nil)
			(SBS-WImage Wpoints)
		)
;;		(vector_image  0  0
;;			(dimx_tile "Wimage")
;;			(dimy_tile "Wimage")
;;			-16
;;		)
	(end_image)
;;	(SBS-WImage Wpoints)
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
			(setq Wpoints (list (CWL-FPOINT (CWL-PPOINTS) "LL")))
		)
	)
	(print "end SBS_WallProperties")
	(cons (list "DFlag" DFlag) (list (append '("Wall Points") Wpoints))) 
)

;;Wall image calculations
(DEFUN SBS-WImage ( Points / x1 y1 z1 di IPList LPoints ct)
	(print "start SBS-WImage")
	(setq
		x1 (car (car points))
		y1 (cadr (car points))
		z1 (caddr (Car points))
	)
	(setq IPList (CWL-ALIST x1 y1 z1 points))
	(print IPList)
		(vector_image  5  5 10 10 4)
;;(print (dimx_tile "Wimage"))
;;	(print (distance '(0.0 0.0 0.0) (dimx_tile "Wimage")))
;;	(print (distance '(0.0 0.0 0.0) (dimy_tile "Wimage")))
;;	(if (> (distance '(0.0 0.0 0.0) (dimx_tile "Wimage")) (distance '(0.0 0.0 0.0) (dimy_tile "Wimage")))
;;		(setq di (distance '(0.0 0.0 0.0) (dimx_tile "Wimage")))
;;		(setq di (distance '(0.0 0.0 0.0) (dimy_tile "Wimage")))
;;	)
;;	(print di)
	;;(start_image "Wimage")
		(setq ct 0)
		(while (/= ct "X")
			(SETQ LPoints (CWL-2POINT IPList ct))
			(IF (= (NTH (1+ ct) IPList) nil)
				(setq ct "X")
				(setq ct (1+ ct))
			)
			(PRINT LPOINTS)
			(vector_image  (car (car IPlist)) (cadr (car IPlist)) (car (cadr IPlist)) (cadr (cadr IPlist)) -16) 
		)
	(print "end SBS-WImage")
)

