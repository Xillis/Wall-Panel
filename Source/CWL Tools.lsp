;;Error handling Function 
(defun CWL-ERR (msg)
	(if (/= msg "function cancelled")
		(alert (stacat "/nError: " msg))
	)
	(command "._undo" "_e")
	(command "._undo" "_u")
;;	(setq *error* set in list passed from main function TBD
)

;;sets specified sys vars to values specified and returns the old values in a list
(defun CWL-SVVCF ( sysvar / oldvar)
	(print "start CWL-SVVCF")
	(foreach var sysvar
		(setq oldvar (append oldvar (list (list(nth 0 var) (getvar (nth 0 var))))))
		(setvar (nth 0 var) (nth 1 var))
	)
	(print "end CWL-SVVCF")
	oldvar
)

;;locats and starts a dialogue specified and executes a function of the same name 
(defun CWL-START-DIA ( DIA-Name DIA-M / DIA-ID )
	(print "start CWL-START_DIA")
	(setq DIA_ID (load_dialog "SBS-Dialog.dcl"))
	(cond
		((= DIA-M "M")
			(PROGN
				(setq DFlag 2)	
				(while (>= DFlag 2)
					(IF (NOT (new_dialog DIA-Name DIA_ID))
						(EXIT)
						(SETQ DFLAG (eval (list(read DIA-name) DFLAG )))
					)
				)
			)
		)
		((= DIA-M "S")
			(PROGN
				(IF (NOT (new_dialog DIA-Name DIA_ID))
					(EXIT)
					(eval (list(read DIA-name)))
				)
			)
		)
	)
	(unload_dialog DIA_ID)
	(print "end CWL-START_DIA")
)

;; extract points from a Polyline and returns a list of the points 
(defun CWL-PPOINTS ( / Pline PPoints CK )
	(print "start CWL-PPOINTS")
	(while (/= CK "LWPOLYLINE")
		(setq Pline (entget (car (entsel "Select Polyline:"))))
		(setq CK (cdr (assoc 0 pline)))
		(if (/= CK "LWPOLYLINE")
			(prompt "\rSelected Object is not a Polyline. ")
		)
	)
	(foreach  p Pline
		(if (= (car p) 10)
			(setq
				PPoints (append PPoints (list (append (cdr p) (list (cdr (assoc 38 pline))))))
			)
		)
	)
	(print "end CWL-PPOINTS")
	PPoints
)

;;finds the Left or right most points sorted by upper or lower "LL, UL, LR, UR" and returns a list sorted with the specified pint as the first point 
(defun CWL-FPOINT ( PList SP / SList CNT PList SP e1 e2 TPOINT)
	(PRINT "start CWL-FPOINT")
	(SETQ Slist(vl-sort-i PList
		(function
			(lambda (e1 e2)
				(IF (OR (= SP "LL" )(= SP "UL"))
					(< (car e1) (car e2))
					(> (car e1) (car e2))
				)
			)
		)
	))
	(IF (= (CAR (NTH (CAR SList) PList)) (CAR (NTH (CADR SList) PList)))
		(IF 
			(OR
				(AND
					(OR (= SP "UL") (= SP "UR")) (<(CADR (NTH (CAR SList) PList)) (CADR (NTH (CADR SList) PList)))
				)
				(AND
					(OR (= SP "LL") (= SP "LR")) (>(CADR (NTH (CAR SList) PList)) (CADR (NTH (CADR SList) PList)))
				)
			)
			(SETQ CNT (CADR SList))
			(SETQ CNT (CAR SList))
		)
		(setq CNT (CAR SList))
	)
	(REPEAT CNT
		(SETQ
			TPOINT (LIST (CAR PLIST))
			PLIST (CDR PLIST)
			PLIST (APPEND PLIST TPOINT)
		)
	)
	(PRINT "end CWL-FPOINT")
	PLIST
)

;;subtracts a list of points by the number specified with x,y,z
(defun CWL-ALIST ( x1 y1 z2 Plist / pf )
	(print "start CWL-ALIST")
	(foreach p Plist
		(setq pf (cons (list (- (car p) x1) (- (cadr p) y1) (- (caddr p) z1)) pf))
	)
	(setq pf (reverse pf))
	(print "end CWL-ALIST")
	pf
)

;;return 2 adjacent points in a list and loop to the first point if at the end of the list
(defun CWL-2POINT (PLIST LOC / 2POINT)
(print "start CWL-2POINT")
	(IF (= (NTH (1+ LOC) PList) nil)
		(SETQ 2POINT (LIST (NTH LOC PLIST)(CAR PLIST)))
		(SETQ 2POINT (LIST (NTH LOC PLIST)(NTH (1+ LOC) PLIST)))
	)
	(print "end CWL-2POINT")
	2Point
)