
(defun SBS_WallProperties ( / Wallpoints Dflag)
	(setq
		width (dimx_tile "Wimage")
		hight (dimy_tile "Wimage")
	)
	(start_image "Wimage")
		(fill_image 0 0 width hight 250)
		(vector_image 5 5 30 15 7)
		(vector_image 30 15 55 5 7)
		(vector_image 5 25 30 35 7)
		(vector_image 30 35 55 25 7)
	(end_image)
	(action_tile "Ppoints" "(Done_dialog 3))")
	(action_tile "accept"
		"(PRINT \"accept\")"
	)
	(action_tile "cancel"
		(strcat 
			"(done_dialog 0)"
			"(exit)"
		)
	)
	(setq Dflag (start_dialog))
	(cond
			((= DFlag 3) (setq Wallpoints (SBS_Wallpoints)))
	)
)

