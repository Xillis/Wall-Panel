;;wall creation function for Steelway building systems
(DEFUN C:SBS-WP (/ oldvar )
	(print "start SBS-WP")
	(setq oldvar (CWL-SVVCF (list '("CMDECHO" 0) ;|'("OSMODE" 16384)|;)))
	(CWL-START-DIA "SBS_WallProperties" "M")
	(CWL-SVVCF oldvar)
	(print "end SBS-WP")
	(princ)
)	

;;Wall points selection function
(DEFUN SBS_Wallpoints ( / WPOINT WPLIST )
	(print "start SBS_Wallpoints")
	(SETQ WPOINT (GETPOINT "\nSelect the base point on the start side of the wall"))
	(SETQ WPLIST (list WPOINT))
	(WHILE (/= WPOINT nil)
		(SETQ WPOINT (GETPOINT WPOINT "\nselect the next point"))
		(IF (/= WPOINT nil)
			(SETQ WPLIST (append WPLIST (list WPOINT)))
		)
	)
	(print "end SBS_Wallpoints")
	WPLIST
)
