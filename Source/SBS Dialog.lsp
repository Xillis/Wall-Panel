
(defun SBS_WallProperties ( Dflag / Wpoints Dflag )
	(action_tile "Ppoints" "(Done_dialog 3))")
	(action_tile "Spoly" "(Done_dialog 4))")
	(action_tile "accept"
		(STRCAT
		"(PRINT \"accept\")
		(done_dialog 1)
		(PRINT Dflag)"
		)
	)
	(action_tile "cancel"
		(strcat 
			"(done_dialog 0)"
		)
	)
	(setq Dflag (start_dialog))
	(cond
		((= DFlag 3) (setq Wpoints (SBS_Wallpoints)))
		((= Dflag 4) (setq Wpoints (CWL-FPOINT (CWL-PPOINTS) "LL")))
	)
	Dflag
)

