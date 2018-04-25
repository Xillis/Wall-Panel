;;wall creation function for Steelway building systems
(defun C:SBS-INSERT-WALL-PANEL (/ OLDVAR PASSTHROUGH )
	;;(print "Start SBS-WP")
	(setq OLDVAR (CWL-SVVCF (list '("CMDECHO" 0) ;|'("OSMODE" 16384)|;)))
	(setq PASSTHROUGH (CWL-START-DIA "SBS_DIA_WALL_PROPERTIES" "M" nil))
	(if (/= (cadr (assoc '"Wall Points" PASSTHROUGH)) nil)
			(progn
				(print (CWL-BITTOLIST (cadr (assoc "Panel info" PASSTHROUGH)) "SBS-PANEL-INFO"))
				(SBS-WALL-PANEL-CALC PASSTHROUGH)
			)
			(print "Operation Canceled")
	)
	(CWL-SVVCF OLDVAR)
	;;(print "End SBS-WP")
	(princ)
)

;;main wall calculation function
(defun SBS-WALL-PANEL-CALC ( PASSTHROUGH / PASSTHROUGH SPOINTS TOP BOTTOM PINFO OLDVAR SP PW)
	;;(print "Start SBS-WALL-PANEL-CALC")
	(setq OLDVAR (CWL-SVVCF (list '("OSMODE" 16384))))
	(setq SPOINTS (SBS-POINT-SPLIT (cadr (assoc "Wall Points" PASSTHROUGH))))
	(setq
		TOP (cadr (assoc 'TOP SPOINTS))
		BOTTOM (cadr (assoc 'BOTTOM SPOINTS))
		PINFO (CWL-BITTOLIST (cadr (assoc "Panel info" PASSTHROUGH)) "SBS-PANEL-INFO")
		SP (cadr (assoc "Wall info" PASSTHROUGH))
	)
	(cond 
		((listp SP)
			(setq BOTTOM (CWL-POINT-SPLIT BOTTOM SP))
			(setq TOP (CWL-POINT-SPLIT TOP SP))
			(SBS-CALC-LOOP (cadr TOP) (cadr BOTTOM) PINFO (atoi (cadr (cadr PINFO))))
			(SBS-CALC-LOOP (reverse (car TOP)) (reverse (car BOTTOM)) PINFO (* -1 (atoi (cadr (cadr PINFO)))))
		)
		((= SP "r") (SBS-CALC-LOOP (reverse TOP) (reverse BOTTOM) PINFO (* -1 (atoi (cadr (cadr PINFO))))))
		(T (SBS-CALC-LOOP TOP BOTTOM PINFO (atoi (cadr (cadr PINFO)))))
	)
	(CWL-SVVCF OLDVAR)
	;;(print "End SBS-WALL-PANEL-CALC")
)

;;Splits the wall polyline in to roof points and floor points. outputs list ((RoofL "points") (FloorL "points") 
(defun SBS-POINT-SPLIT ( POINTLIST / POINTLIST TEMPP RP TPOINTS BPOINTS COMBLIST)
	;;(print "Start CWL_POINT_SPLIT")
	(if (> (cadr (cadr POINTLIST)) (cadr (last POINTLIST)))
		(setq
			TEMPP (list (car POINTLIST))
			POINTLIST (cdr POINTLIST)
			POINTLIST (append POINTLIST TempP)
			POINTLIST (reverse POINTLIST)
		)
	)
	(setq
		RP (car (vl-sort-i POINTLIST (function (lambda (e1 e2) (> (car e1) (car e2))))))
		TPOINTS (reverse (member (nth RP POINTLIST) POINTLIST))
		BPOINTS (reverse (cdr (member (nth RP POINTLIST) (reverse POINTLIST))))
	)
	(cond
		((and (/= (rtos (car (car TPOINTS))) (rtos (car (car BPOINTS)))) (/= (rtos (car (last TPOINTS))) (rtos (car (last BPOINTS))))) 
			(setq COMBLIST (SBS-PS-FIXLEFT TPOINTS BPOINTS))
			(setq COMBLIST (SBS-PS-FIXRIGHT (cadr (assoc 'TOP COMBLIST)) (cadr (assoc 'BOTTOM COMBLIST))))
		)
		((/= (rtos (car (car TPOINTS))) (rtos (car (car BPOINTS))))
			(setq COMBLIST (SBS-PS-FIXLEFT TPOINTS BPOINTS))
		)
		((/= (rtos (car (last TPOINTS))) (rtos (car (last BPOINTS))))
			(setq COMBLIST (SBS-PS-FIXRIGHT TPOINTS BPOINTS))		
		)
		(t 
			(setq COMBLIST (SBS-PS-COMBLIST TPOINTS BPOINTS))
		)
	)
	;;(print "End CWL_POINT_SPLIT")
	COMBLIST
)

;;sorts the wall point based on orgin to be sorted from the left
(defun SBS-PS-FIXLEFT ( TPOINTS BPOINTS / TPOINTS BPOINTS COMBLIST )
	;;(print "Start SBS-PS-FIXLEFT")
	(if (< (car (car TPOINTS)) (car (car BPOINTS)))
		(setq BPOINTS (append (list (car TPOINTS)) BPOINTS))	
		(setq TPOINTS (append (list (car BPOINTS)) TPOINTS))
	)
	(setq COMBLIST (SBS_PS_COMBLIST TPOINTS BPOINTS))
	(print "End SBS-PS-FIXLEFT")
	COMBLIST
)

;;sorts the wall point based on orgin to be sorted from the right
(defun SBS-PS-FIXRIGHT ( TPOINTS BPOINTS / TPOINTS BPOINTS COMBLIST )
	;;(print "Start SBS-PS-FIXRIGHT")
	(if (> (car (last TPOINTS)) (car (last BPOINTS)))
		(setq BPOINTS (append BPOINTS (list (last TPOINTS))))
		(setq TPOINTS (append TPOINTS (list (last BPOINTS))))
	)
	(setq COMBLIST (SBS-PS-COMBLIST TPOINTS BPOINTS))
	;;(print "End SBS-PS-FIXRIGHT")
	COMBLIST
)

;;combines the sorted wall points in a list
(defun SBS-PS-COMBLIST ( TPOINTS BPOINTS / TPOINTS BPOINTS COMBLIST )
	;;(print "Start SBS-PS-COMBLIST")
	(setq COMBLIST (list (list 'Top TPOINTS) (list  'Bottom BPOINTS)))
	;;(print "End SBS-PS-COMBLIST")
	COMBLIST
)