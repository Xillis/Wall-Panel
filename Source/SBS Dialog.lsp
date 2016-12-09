
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
	(action_tile "Ppoints" "(Done_dialog 3))")
	(action_tile "Spoly" "(Done_dialog 4))")
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

