;;wall creation function for Steelway building systems
(DEFUN C:SBS-WP (/ oldvar )
	;; set working environment
	(setq oldvar (CWL-SVVCF (list '("CMDECHO" 0) '("OSMODE" 16384))))
	;;Dialog Function
	(CWL-PANEL-DIA "SBS_WallProperties")
	;; re-set original working environment
	(CWL-SVVCF oldvar)
)	

;;Wall points selection function
(DEFUN SBS_Wallpoints ( / POINT WPLIST )
	(print "start Wall Points")
	(SETQ WPOINT (GETPOINT "\nSelect the base point on the start side of the wall"))
	(print WPOINT)
	(SETQ WPLIST (list WPOINT))
	(print WPLIST)
	(WHILE (/= WPOINT nil)
		(SETQ WPOINT (GETPOINT WPOINT "\nselect the next point"))
		(SETQ WPLIST (append WPLIST (list WPOINT)))
		(print WPOINT)
		(print WPLIST)
	)
)
