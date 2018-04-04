;;Wall Properties dialog function
(defun SBS_DIA_WALL_PROPERTIES ( PASSTHROUGH / PASSTHROUGH DFLAG )
	(print "Start SBS-DIA-WALL-PROPERTIES")
	(start_image "Wimage")
		(fill_image 0 0 
			(dimx_tile "Wimage")
			(dimy_tile "Wimage")
			-15
		)
		(if (null (cadr (assoc "Wall Points" PASSTHROUGH)))
			(progn
				(vector_image 0 0 (dimx_tile "Wimage") (dimy_tile "Wimage") -16)
				(vector_image (dimx_tile "Wimage") 0 0 (dimy_tile "Wimage") -16)
			)
			(SBS-WIMAGE (cadr (assoc "Wall Points" PASSTHROUGH)))
		)
	(end_image)
;;	(if (assoc "Panel info" PASSTHROUGH)
;;		(CWL-DBLIST
;;			(CWL-BITTOLIST (cadr (assoc "Panel info" PASSTHROUGH)) "SBS-PANEL-INFO")
;;			"panelinfo" 1
;;		)
;;		(CWL-DBLIST (list '("No Panel info")) "panelinfo" 0)
;;	)
	(cond
		((not (assoc "Wall info" PASSTHROUGH))
			(setq PASSTHROUGH (append PASSTHROUGH (list (list "Wall info" "l"))))
			(set_tile "Startp" "l")
		)
		((listp (cadr (assoc "Wall info" PASSTHROUGH)))
			(set_tile "Startp" "p")
		)
		(T
			(set_tile "Startp" (cadr (assoc "Wall info" PASSTHROUGH)))
		)
	)
	(action_tile "Startp" 
		"(setq PASSTHROUGH(subst (list \"Wall info\" $value) (assoc \"Wall info\" PASSTHROUGH) PASSTHROUGH))
		(IF (= $value \"p\")
			(Done_dialog 6)
		)"
	)
	(action_tile "Ppoints" "(Done_dialog 3)")
	(action_tile "Spoly" "(Done_dialog 4)")
	(action_tile "SPType" "(Done_dialog 5)")
	(action_tile "accept" "(done_dialog 1)")
	(action_tile "cancel" "(print \"Cancel\")(done_dialog 0)")
	(setq DFLAG (start_dialog))
	(cond
		((= DFLAG 0)
			(setq PASSTHROUGH (list '("CANCLE")))
		)
		((= DFLAG 3) 
			(setq PASSTHROUGH
				(if (assoc '"Wall Points" PASSTHROUGH)
					(subst 
						(list '"Wall Points" (CWL-FPOINT (SBS_WALLPOINTS) "Left"))
						(assoc '"Wall Points" PASSTHROUGH)
					PASSTHROUGH)
					(append (list (list '"Wall Points" (CWL-FPOINT (SBS_WALLPOINTS) "Left"))) PASSTHROUGH)
				)
			)
		)
		((= DFLAG 4)
			(setq PASSTHROUGH
				(IF (assoc '"Wall Points" PASSTHROUGH)
					(subst 
						(list '"Wall Points" (CWL-FPOINT (CWL-PPOINTS) "Left"))
						(assoc '"Wall Points" PASSTHROUGH)
					PASSTHROUGH)
					(append (list (list '"Wall Points" (CWL-FPOINT (CWL-PPOINTS) "Left"))) PASSTHROUGH)
				)
			)
		)
		((= DFLAG 5)
			(setq PASSTHROUGH (SBS_PANEL_INFO PASSTHROUGH))
		)
		((= DFLAG 6)
			(setq PASSTHROUGH 
				(subst
					(list "Wall info" (getpoint "select the start point for the panels:"))
					(assoc "Wall info" PASSTHROUGH)
				PASSTHROUGH)
			)	
		)
	)
	(print (subst (list "Flag" DFLAG) (assoc "Flag" PASSTHROUGH) PASSTHROUGH))
	(print "End SBS-DIA-WALL-PROPERTIES")
	(subst (list "Flag" DFLAG) (assoc "Flag" PASSTHROUGH) PASSTHROUGH)
)

;;Wall image calculations
(defun SBS-WIMAGE ( POINTS / POINTS DPOINT WX WY IPLIST LPOINTS CT 2POINTS)
	(print "Start SBS-WIMAGE")
	(setq DPOINT (SBS-MAXPOINT POINTS))
	(setq IPLIST (SBS-ALIST (car DPOINT) (caddr DPOINT) 0.0 POINTS))
	(setq WX (1- (dimx_tile "Wimage")))
	(setq WY (1- (dimy_tile "Wimage")))
	(if (> (/ WX (- (cadr DPOINT) (car DPOINT))) (/ WY (- (cadddr DPOINT) (caddr DPOINT))))
		(setq CT (/ WY (- (cadddr DPOINT) (caddr DPOINT))))
		(setq CT (/ WX (- (cadr DPOINT) (car DPOINT))))
	)
	(foreach p IPLIST
		(setq LPOINTS (cons (list (fix(* (car p) CT)) (fix(+ (* (* (cadr p) CT) -1)WY))) LPOINTS))
	)
		(setq CT 0)
		(while (/= CT "X")
			(setq 2POINTS (SBS-2POINT LPOINTS CT))
			(if (= (nth (1+ CT) LPOINTS) nil)
				(setq CT "X")
				(setq CT (1+ CT))
			)
			(vector_image  (car (car 2POINTS)) (cadr (car 2POINTS)) (car (cadr 2POINTS)) (cadr (cadr 2POINTS)) -16) 
		)
	(print "End SBS-WIMAGE")
)

;;Reterns the Max and Min 'x' and 'y' of a givin point list (XMin XMax YMin YMax)
(defun SBS-MAXPOINT ( POINTS / XL YL POINTLIST POINTS)
	(print "Start CWL-MAXPOINT")
	(setq XL (vl-sort-i POINTS (function (lambda (e1 e2) (< (car e1) (car e2))))))
	(setq YL (vl-sort-i POINTS (function (lambda (e1 e2) (< (cadr e1) (cadr e2))))))
	(setq POINTLIST (list (car (nth (car XL) POINTS))(car (nth (last XL) POINTS))(cadr (nth (car YL) POINTS))(cadr (nth (last YL) POINTS))))
	(print "End CWL-MAXPOINT")
	POINTLIST
)

;;subtracts a list of points by the number specified with x,y,z
(defun SBS-ALIST ( X1 Y1 Z1 PLIST / PF X1 Y1 Z1 PLIST )
	(print "Start CWL-ALIST")
	(foreach p PLIST
		(setq PF (cons (list (- (car p) X1) (- (cadr p) Y1) (- (caddr p) Z1)) PF))
	)
	(setq PF (reverse PF))
	(print "End CWL-ALIST")
	PF
)

;;return 2 adjacent points in a list and loop to the first point if at the end of the list
(defun SBS-2POINT (PLIST LOC / 2POINT PLIST LOC)
	(print "Start CWL-2POINT")
	(if (= (nth (1+ LOC) PLIST) nil)
		(setq 2POINT (list (nth LOC PLIST)(car PLIST)))
		(setq 2POINT (list (nth LOC PLIST)(nth (1+ LOC) PLIST)))
	)
	(print "End CWL-2POINT")
	2POINT
)

;;Wall points selection function
(defun SBS_WALLPOINTS ( / WPOINT WPLIST )
	(print "Start SBS_Wallpoints")
	(setq WPOINT (getpoint "\nSelect the base point on the start side of the wall"))
	(setq WPLIST (list WPOINT))
	(while (/= WPOINT nil)
		(setq WPOINT (getpoint WPOINT "\nselect the next point"))
		(if (/= WPOINT nil)
			(setq WPLIST (append WPLIST (list WPOINT)))
		)
	)
	(print "end SBS_WALLPOINTS")
	WPLIST
)