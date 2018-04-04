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
(defun CWL-SVVCF ( SYSVAR / SYSVAR OLDVAR)
	(print "Start CWL-SVVCF")
	(foreach var SYSVAR
		(setq OLDVAR (append OLDVAR (list (list(nth 0 var) (getvar (nth 0 var))))))
		(setvar (nth 0 var) (nth 1 var))
	)
	(print "End CWL-SVVCF")
	OLDVAR
)

;;locates and starts a dialogue specified and executes a function of the same name 
(defun CWL-START-DIA ( DIA-NAME DIA-M PASSTHROUGH / DIA-ID PASSTHROUGH DFLAG)
	(print (strcat "Start CWL-START_DIA - " DIA-NAME))
	(setq DIA-ID (load_dialog (STRCAT DIA-NAME ".DCL")))
	(cond
		((= DIA-M "M")
			(progn
				(setq DFLAG 2 )
				(if (null PASSTHROUGH)
					(setq PASSTHROUGH (list(list "Flag" DFLAG)))
					;;(setq PASSTHROUGH (append (list PASSTHROUGH) (list(list "Flag" DFLAG))))
					(subst (list "Flag" DFLAG) (assoc "Flag" PASSTHROUGH) PASSTHROUGH)
				)
				(while (>= DFLAG 2)
					(if (not (new_dialog DIA-NAME DIA-ID))
						(exit)
						(setq PASSTHROUGH (eval (list(read DIA-NAME) 'PASSTHROUGH)))
					)
					(setq DFLAG (cadr (assoc "Flag" PASSTHROUGH)))
				)
			)
		)
		((= DIA-M "S")
			(if (not (new_dialog DIA-NAME DIA-ID))
					(exit)
					(setq PASSTHROUGH (eval (list(read DIA-NAME) 'PASSTHROUGH)))
			)
		)
	)
	(unload_dialog DIA-ID)
	(print (strcat "End CWL-START_DIA - " DIA-NAME))
	(vl-remove (assoc "Flag" PASSTHROUGH) PASSTHROUGH)
)

;; extract points from a Polyline and returns a list of the points 
(defun CWL-PPOINTS ( / PLINE PPOINTS CK )
	(print "Start CWL-PPOINTS")
	(while (/= CK "LWPOLYLINE")
		(setq PLINE (entget (car (entsel "Select Polyline:"))))
		(setq CK (cdr (assoc 0 PLINE)))
		(if (/= CK "LWPOLYLINE")
			(prompt "\rSelected Object is not a Polyline. ")
		)
	)
	(foreach  p PLINE
		(if (= (car p) 10)
			(setq
				PPOINTS (append PPOINTS (list (append (cdr p) (list (cdr (assoc 38 PLINE))))))
			)
		)
	)
	(IF (= (cdr (assoc 70 PLINE)) 0)
		(SETQ PPOINTS (CDR PPOINTS))
	)
	(print "End CWL-PPOINTS")
	PPOINTS
)

;;finds the extreme lower left or lower right points then resorts the list with this point as the first point
(defun CWL-FPOINT ( PLIST SP / SLIST CNT PLIST SP TPOINT)
	(print "Start CWL-FPOINT")
	(setq SLIST
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
	(if (= (car (nth (car SLIST) PLIST)) (car (nth (cadr SLIST) PLIST)))
		(if (>(cadr (nth (car SLIST) PLIST)) (cadr (nth (cadr SLIST) PLIST)))
			(setq CNT (cadr SLIST))
			(setq CNT (car SLIST))
		)
		(setq CNT (car SLIST))
	)
	(repeat CNT
		(setq
			TPOINT (list (car PLIST))
			PLIST (cdr PLIST)
			PLIST (append PLIST TPOINT)
		)
	)
	(print "End CWL-FPOINT")
	PLIST
)

;;Creats a list from a table
(defun CWL-CLIST ( TABLE / CLIST INFO POS RL )
	;;(print "start CWL-CLIST")
	(SETQ 
		INFO (VL-GET-RESOURCE TABLE)
		POS 1
		RL " "
	)
	(WHILE (not (null RL))
		(SETQ RL (read (substr INFO POS )))
		(IF (not (null RL))
			(SETQ CLIST (APPEND (LIST RL) CLIST))
		)
		(SETQ POS (+ POS 2))
		(if (vl-string-position (ASCII "\n") (substr INFO POS))
			(SETQ POS (+ POS (VL-STRING-POSITION (ASCII "\n") (substr INFO POS))))
			(setq rl nil)
		)
	)
	;;(print "END CWL-CLIST")
	CLIST
)
;;Creates a list using a bit code and a table
(defun CWL-BITTOLIST ( SBIT UTABLE / POS RL ELIST INFO )
	(print "Start CWL-BITTOLIST")
	(IF (= SBIT "all")
		(SETQ SBIT 2147483647)
	)
	(setq 
		INFO (vl-get-resource UTABLE)
		POS 1
		RL " "
	)
	(while (not (null RL))
		(setq RL (read (substr INFO POS )))
		(if (and (not (null RL)) (/= (logand SBIT (car RL)) 0))
			(setq ELIST (append ELIST (list RL)))
		)
		(setq POS (+ POS 2))
		(if (vl-string-position (ascii "\n") (substr INFO POS))
			(setq POS (+ POS (vl-string-position (ascii "\n") (substr INFO POS))))
			(setq rl nil)
		)
	)
	(print "End CWL-BITTOLIST")
	ELIST
)

;;populates a dialogue list from a list
(defun CWL-DBLIST ( ELIST DIAKEY POSITION / DIAKEY ELIST POSITION)
	(print (strcat "Start CWL-DBLIST - " DIAKEY))
	(start_list DIAKEY 2 0)
		(add_list "")
		(mapcar 'add_list 
			(mapcar 
				'(lambda (i)
					(nth POSITION i)
				)
			ELIST)
		)
	(end_list)
	(print (strcat "End CWL-DBLIST - " DIAKEY))
)

;; creates a list based on a set bit list and renters the position of an item in the list based off the first value of the item
(defun CWL-TILESET (BIT BITRANGE UTABLE / BIT BITRANGE UTABLE VALUE VLIST)
(print "START CWL-TILESET")
	(if (= BITRANGE "all")
		(SETQ BITRANGE 214783647)
	)
	(setq VLIST (CWL-BITTOLIST BITRANGE UTABLE))
	(setq VALUE (itoa (vl-position (assoc BIT VLIST) VLIST)))
(print "END CWL-TILESET")
VALUE
)

;;SPLITS A LIST OF PONTS AT A SPECIFIED POINT IN THE "X" DIRECTION
(DEFUN CWL-POINT-SPLIT (POINTLIST POINT / POINTLIST POINT LLIST)
	;;(PRINT "START CWL-POINT-SPLIT")
	(FOREACH p POINTLIST
		(IF (< (CAR P) (CAR POINT))
			(SETQ LLIST (CONS p LLIST))
		)
	)
	(SETQ
		LLIST (REVERSE LLIST)
		RLIST (CDR (MEMBER (LAST LLIST) POINTLIST))
	)
	(IF (= (CAR POINT) (CAR (CAR RLIST)))
		(SETQ LLIST (APPEND LLIST (LIST (CAR RLIST))))
		(PROGN
			(SETQ POINT
				(LIST
					(CAR POINT)
						(+ (CADR (LAST LLIST)) (* (/ (- (CADR (LAST LLIST)) (CADR (CAR RLIST))) (- (CAR (LAST LLIST)) (CAR (CAR RLIST)))) (- (CAR POINT)(CAR (LAST LLIST)))))
					(CADDR POINT)
				)
			)
			(SETQ
				LLIST (APPEND LLIST (LIST POINT))
				RLIST (CONS POINT RLIST)
			)
		)
	)
	;;(PRINT "END CWL-POINT-SPLIT")
	(LIST LLIST RLIST)
)

(DEFUN CWL_LISTRESET (RLIST / )
(START_LIST RLIST 3 )
(END_LIST)
)
