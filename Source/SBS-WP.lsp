;;wall creation function for Steelway building systems
(DEFUN C:SBS-WP (/ oldvar PASSTHROUGH )
	(print "start SBS-WP")
	(setq oldvar (CWL-SVVCF (list '("CMDECHO" 0) ;|'("OSMODE" 16384)|;)))
	(setq PASSTHROUGH (CWL-START-DIA "SBS_WallProperties" "M" nil))
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

;;Splits the wall polyline in to roof points and floor points. outputs list ((RoofL "points") (FloorL "points") 
(DEFUN SBS_POINT_SPLIT ( pointlist / pointlist tempP RP Tpoints Bpoints comblist)
	(print "Start CWL_POINT_SPLIT")
	(if (> (cadr (cadr pointlist)) (cadr (last pointlist)))
		(SETQ
			TempP (list (car pointlist))
			pointlist (CDR pointlist)
			pointlist (APPEND pointlist TempP)
			Pointlist (reverse pointlist)
		)
	)
	(setq
		RP (car (vl-sort-i pointlist (function (lambda (e1 e2) (> (car e1) (car e2))))))
		Tpoints (reverse (member (nth RP pointlist) pointlist))
		Bpoints (reverse (cdr (member (nth RP pointlist) (reverse pointlist))))
	)
	(cond
		((and (/= (rtos (car (car Tpoints))) (rtos (car (car Bpoints)))) (/= (rtos (car (last Tpoints))) (rtos (car (last Bpoints))))) 
			(setq comblist (SBS_PS_FIXLEFT Tpoints Bpoints))
			(setq comblist (SBS_PS_FIXRIGHT (cadr (assoc 'TOP comblist)) (cadr (assoc 'BOTTOM comblist))))
		)
		((/= (rtos (car (car Tpoints))) (rtos (car (car Bpoints))))
			(setq comblist (SBS_PS_FIXLEFT Tpoints Bpoints))
		)
		((/= (rtos (car (last Tpoints))) (rtos (car (last Bpoints))))
			(setq comblist (SBS_PS_FIXRIGHT Tpoints Bpoints))		
		)
		(t 
			(setq Comblist (SBS_PS_COMBLIST tpoints Bpoints))
		)
	)
	(print "End CWL_POINT_SPLIT")
	Comblist
)

;;sorts the wall point based on orgin to be sorted from the left
(DEFUN SBS_PS_FIXLEFT ( Tpoints Bpoints / Tpoints Bpoints comblist )
	(print "start SBS_PS_FIXLEFT")
	(if (< (car (car Tpoints)) (car (car Bpoints)))
		(setq Bpoints (append (list (car tpoints)) Bpoints))	
		(setq Tpoints (append (list (car Bpoints)) Tpoints))
	)
	(setq Comblist (SBS_PS_COMBLIST tpoints Bpoints))
	(print "end SBS_PS_FIXLEFT")
	comblist
)

;;sorts the wall point based on orgin to be sorted from the right
(DEFUN SBS_PS_FIXRIGHT ( Tpoints Bpoints / Tpoints Bpoints comblist )
	(print "start SBS_PS_FIXRIGHT")
	(if (> (car (last Tpoints)) (car (last Bpoints)))
		(setq Bpoints (append Bpoints (list (last tpoints))))
		(setq Tpoints (append Tpoints (list (last Bpoints))))
	)
	(setq Comblist (SBS_PS_COMBLIST tpoints Bpoints))
	(print "end SBS_PS_FIXRIGHT")
	comblist
)

;;combines the sorted wall points in a list
(defun SBS_PS_COMBLIST ( Tpoints Bpoints / Tpoints Bpoints Comblist )
	(print "start SBS_PS_COMBLIST")
	(setq Comblist (list (list 'Top Tpoints) (list  'Bottom Bpoints)))
	(print "end SBS_PS_COMBLIST")
	Comblist
)

;;main wall calculation function
(defun SBS-WALL-PANEL-CALC ( PASSTHROUGH / PASSTHROUGH SPOINTS top bottom RUNLENGTH WLENGTH PINFO)
	(print "start SBS-WALL-PANEL-CALC")
(setq Spoints (SBS_POINT_SPLIT (CADR (assoc "Wall Points" PASSTHROUGH))))
	(setq
		Top (cadr (assoc 'TOP Spoints))
		Bottom (cadr (assoc 'BOTTOM SPoints))
		WLENGTH (DISTANCE (LIST (CAR (CAR BOTTOM)) 0.0 0.0) (LIST (CAR (LAST BOTTOM)) 0.0 0.0))
		PINFO (CWL-BITLIST (cadr (assoc "Panel info" PASSTHROUGH)) "SBS-PANEL-INFO")
	)
	(print "Panel Info")
	(print (cadr (car PINFO)))
	(print (strcat "Width: " (VL-PRINC-TO-STRING (CADR (CADR PINFO)))))
	(print (strcat "Profile: " (VL-PRINC-TO-STRING (CADR (CADDDR PINFO)))))
	(print (strcat "Gauge: " (VL-PRINC-TO-STRING (CADR (CADDR PINFO)))))
	(print "------------------------------")
	(print (STRCAT "Top points = " (VL-PRINC-TO-STRING Top)))
	(print (STRCAT "Bottom Points = " (VL-PRINC-TO-STRING Bottom)))
	(print (STRCAT "Total Wall Length = "(VL-PRINC-TO-STRING WLENGTH)))
	(SETQ
		COUNT 0
		RUNLENGTH 0
		BP1 (CAR BOTTOM)
		BP2 (CADR BOTTOM)
		BOTTOM (CDR BOTTOM)
	)
	(PRINT (STRCAT "Pitch =" (VL-PRINC-TO-STRING (abs(*(/ (- (CADR BP2) (CADR BP1)) (- (CAR BP2) (CAR BP1))) 12))) "/12"))
	(PRINT BOTTOM)
	(if (> (abs(*(/ (- (CADR BP2) (CADR BP1)) (- (CAR BP2) (CAR BP1))) 12)) 1.1)
		(setq
			BP1
				(LIST 
					(CAR BP1)
					(+ (CADR BP1) (*(/ (- (CADR BP2) (CADR BP1)) (- (CAR BP2) (CAR BP1))) (ATOI (CADR (CADR PINFO)))))
					(CADDR BP1)
				)
		)
	)
	(WHILE ( < RUNLENGTH WLENGTH)
		(PRINT BP1)
		(PRINT (*(/ (- (CADR BP2) (CADR BP1)) (- (CAR BP2) (CAR BP1))) (ATOI (CADR (CADR PINFO)))))
		(setq
			COUNT (1+ COUNT)
			RUNLENGTH (+ RUNLENGTH (ATOI (CADR (CADR PINFO))))
		)
		(if (> (DISTANCE (LIST (CAR BP1) 0.0 0.0) (LIST (CAR BP2) 0.0 0.0)) (ATOI (CADR (CADR PINFO))))
			(SETQ BP1
					(LIST 
						(+ (CAR BP1)(ATOI (CADR (CADR PINFO))))
						(+ (CADR BP1) (*(/ (- (CADR BP2) (CADR BP1)) (- (CAR BP2) (CAR BP1))) (ATOI (CADR (CADR PINFO)))))
						(CADDR BP1)
					)
			)
			(PROGN
				;;(SETQ BP1 
				(PRINT
						(LIST
							(+ (CAR BP2) (- (ATOI (CADR (CADR PINFO))) (- (CAR BP2)(CAR BP1))))
							
					
						)
				)
			)
		)
	)
	(print (strcat "Number of Panels = " (ITOA COUNT)))
	(print "end SBS-WALL-PANEL-CALC")
)
	

