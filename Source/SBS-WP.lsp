;;wall creation function for Steelway building systems
(DEFUN C:SBS-WP (/ oldvar )
	(print "start SBS-WP")
	;; set working environment
	(setq oldvar (CWL-SVVCF (list '("CMDECHO" 0) ;|'("OSMODE" 16384)|;)))
	;;Dialog Function
	(CWL-START-DIA "SBS_WallProperties" "M")
	;; re-set original working environment
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

;;Wall image calculations
(DEFUN SBS-WImage ( Points / x1 y1 z1 IPList LPoints)
	(print "start SBS-WImage")
	(setq
		x1 (car (car points))
		y1 (cadr (car points))
		z1 (caddr (Car points))
	)
	(setq IPList (CWL-ALIST x1 y1 z1 points))
	(print iplist)
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
