;;Error handling Function 
;|(defun CWL-ERR (msg)
	(if (/= msg "function cancelled")
		(alert (stacat "/nError: " msg))
	)
	(command "._undo" "_e")
	(command "._undo" "_u")
;;	(setq *error* set in list passed from main function TBD
)|;

(defun *error* (msg)
  (princ "error: ")
  (princ msg)
 (princ)
)

;;sets specified sys vars to values specified and returns the old values in a list
(defun CWL-SVVCF ( sysvar / sysvar oldvar)
	(print "start CWL-SVVCF")
	(foreach var sysvar
		(setq oldvar (append oldvar (list (list(nth 0 var) (getvar (nth 0 var))))))
		(setvar (nth 0 var) (nth 1 var))
	)
	(print "end CWL-SVVCF")
	oldvar
)

;;locates and starts a dialogue specified and executes a function of the same name 
(defun CWL-START-DIA ( DIA-Name DIA-M PassThrough / DIA-ID PassThrough DFlag)
	(print (strcat "start CWL-START_DIA - " DIA-NAME))
	(setq DIA-ID (load_dialog "SBS-Dialog.dcl"))
	(cond
		((= DIA-M "M")
			(PROGN
				(setq DFlag 2 )
				(if (null PassThrough)
					(setq PassThrough (list(list "DFlag" DFlag)))
					(setq PassThrough (append (list PassThrough) (list(list "DFlag" DFlag))))
				)
				(while (>= DFlag 2)
					(IF (NOT (new_dialog DIA-Name DIA-ID))
						(EXIT)
						(SETQ PassThrough (eval (list(read DIA-name) 'PassThrough)))
					)
					(SETQ DFlag (CADR (assoc "DFlag" PassThrough)))
				)
			)
		)
		((= DIA-M "S")
			(IF (NOT (new_dialog DIA-Name DIA-ID))
					(EXIT)
					(eval (list(read DIA-name)))
			)
		)
	)
	(unload_dialog DIA-ID)
	(print (strcat "end CWL-START_DIA - " DIA-NAME))
	(vl-remove (assoc "DFlag" PassThrough) Passthrough)
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
	(IF (= (cdr (assoc 70 pline)) 0)
		(SETQ PPoints (CDR PPoints))
	)
	(print "end CWL-PPOINTS")
	PPoints
)

;;finds the extreme lower left or lower right points then resorts the list with this point as the first point
(defun CWL-FPOINT ( PList SP / SList CNT PList SP e1 e2 TPOINT)
	(PRINT "start CWL-FPOINT")
	(SETQ Slist
		(vl-sort-i PList
			(function
				(lambda (e1 e2)
					(IF (= SP "Left" )
						(< (car e1) (car e2))
						(> (car e1) (car e2))
					)
				)
			)
		)
	)
	(IF (= (CAR (NTH (CAR SList) PList)) (CAR (NTH (CADR SList) PList)))
		(IF (>(CADR (NTH (CAR SList) PList)) (CADR (NTH (CADR SList) PList)))
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
(defun CWL-ALIST ( x1 y1 z1 Plist / pf x1 y1 z1 Plist )
	(print "start CWL-ALIST")
	(foreach p Plist
		(setq pf (cons (list (- (car p) x1) (- (cadr p) y1) (- (caddr p) z1)) pf))
	)
	(setq pf (reverse pf))
	(print "end CWL-ALIST")
	pf
)

;;return 2 adjacent points in a list and loop to the first point if at the end of the list
(defun CWL-2POINT (PLIST LOC / 2POINT PLIST LOC)
(print "start CWL-2POINT")
	(IF (= (NTH (1+ LOC) PList) nil)
		(SETQ 2POINT (LIST (NTH LOC PLIST)(CAR PLIST)))
		(SETQ 2POINT (LIST (NTH LOC PLIST)(NTH (1+ LOC) PLIST)))
	)
	(print "end CWL-2POINT")
	2Point
)

;;Reterns the Max and Min 'x' and 'x' of a givin point list (XMin XMax YMin YMax)
(defun CWL-MAXPOINT ( Points / XL YL PointList Points)
	(print "start CWL-MAXPOINT")
	(SETQ XL (vl-sort-i Points (function (lambda (e1 e2) (< (car e1) (car e2))))))
	(SETQ YL (vl-sort-i Points (function (lambda (e1 e2) (< (cadr e1) (cadr e2))))))
	(setq PointList (list (car (nth (car XL) points))(car (nth (last XL) points))(cadr (nth (car YL) points))(cadr (nth (last YL) points))))
	(print "end CWL-MAXPOINT")
	PointList
)

;;Creates a list using a bit code and a table
(defun CWL-BITLIST ( SBIT UTABLE / POS RL ELIST INFO )
	(print "start CWL-BITLIST")
	(SETQ 
		INFO (VL-GET-RESOURCE UTABLE)
		POS 1
		RL " "
	)
	(WHILE (not (null RL))
		(SETQ RL (read (substr INFO POS )))
		(IF (AND (not (null RL)) (/= (LOGAND SBIT (car RL)) 0))
			(SETQ ELIST (APPEND (LIST RL) ELIST))
		)
		(SETQ POS (+ POS 2))
		(if (VL-STRING-POSITION (ASCII "\n") (substr INFO POS))
			(SETQ POS (+ POS (VL-STRING-POSITION (ASCII "\n") (substr INFO POS))))
			(setq rl nil)
		)
	)
	(print "END CWL-BITLIST")
	(reverse ELIST)
)

;;populates a dialogue list from a list and bit table
(defun CWL-DDBCOAD ( SBIT UTABLE DIAKEY / ELIST )
	(print (strcat "start CWL-DDBCOAD - " DIAKEY))
	(setq ELIST (CWL-BITLIST SBIT UTABLE))
	(START_LIST DIAKEY)
		(MAPCAR 'ADD_LIST 
			(MAPCAR 
				'(LAMBDA (i)
					(CADR i)
				)
			ELIST)
		)
	(END_LIST)
	(IF (< (LENGTH ELIST) 2)
		(MODE_TILE DIAKEY 1)
		(MODE_TILE DIAKEY 0)
	)
	(print (strcat "end CWL-DDBCOAD - " DIAKEY))
)

;; creates a list based on a set bit list and renters the position of an item in the list based off the first value of the item
(DEFUN CWL-TILESET (BIT BITRANGE UTABLE / )
(PRINT "START CWL-TILESET")
(SETQ VLIST (CWL-BITLIST BITRANGE UTABLE))
(SETQ VALU (itoa (VL-POSITION (ASSOC BIT VLIST) VLIST)))
(PRINT "END CWL-TILESET")
VALU
)
