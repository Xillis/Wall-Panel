;;wall creation function for Steelway building systems
(DEFUN C:SBS-WP (/ oldvar )
	;; set working environment
	(setq oldvar (CWL-SVVCF (list '("CMDECHO" 0) '("OSMODE" 16384))))
	;;Dialog Function
	(CWL-PANEL-DIA "SBS_WallProperties" "M")
	;; re-set original working environment
	(CWL-SVVCF oldvar)
	(PRINT "END WP")
)	

;;Wall points selection function
(DEFUN SBS_Wallpoints ( / WPOINT WPLIST )
	(SETQ WPOINT (GETPOINT "\nSelect the base point on the start side of the wall"))
	(SETQ WPLIST (list WPOINT))
	(WHILE (/= WPOINT nil)
		(SETQ WPOINT (GETPOINT WPOINT "\nselect the next point"))
		(IF (/= WPOINT nil)
			(SETQ WPLIST (append WPLIST (list WPOINT)))
		)
	)
	(print WPlist)
	WPLIST
)
