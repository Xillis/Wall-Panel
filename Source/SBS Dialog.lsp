;;Wall Properties dialog function
(defun SBS_WallProperties ( Dflag / Wpoints Dflag )
	(print "start SBS_WallProperties")
	(start_image "Wimage") 
		(fill_image 0 0 
			(dimx_tile "Wimage")
			(dimy_tile "Wimage")
			-15
		)
		(vector_image  0  0
			(dimx_tile "Wimage")
			(dimy_tile "Wimage")
			-16
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
			(done_dialog 0)"
		)
	)
	(setq Dflag (start_dialog))
	(cond
		((= DFlag 3) 
			(setq Wpoints (SBS_Wallpoints))
			(SBS-WImage Wpoints)
		)
		((= Dflag 4)
			(setq Wpoints (CWL-FPOINT (CWL-PPOINTS) "LL"))
			(SBS-WImage Wpoints)
		)
	)
	(print "end SBS_WallProperties")
	Dflag
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
	(start_image "Wimage")
		(vector_image  5  5 10 10 4)
	(end_image)
;;	(print (dimx_tile "Wimage"))
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
		)
	(print "end SBS-WImage")
)

