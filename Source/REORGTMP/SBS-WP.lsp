








	
;;Panel insert loop
(defun SBS-CALC-LOOP (TOP BOTTOM PINFO PW / COUNT PINFO WLENGTH PW RUNLENGTH BPOINTS TPOINTS FLAG DELTALENGTHB PLENGTH DELTALENGTHT)
	;;(print "start SBS-CALC-LOOP")
	(SETQ
		WLENGTH (DISTANCE (LIST (CAR (CAR BOTTOM)) 0.0 0.0) (LIST (CAR (LAST BOTTOM)) 0.0 0.0))
		COUNT 0
		RUNLENGTH 0
		BPOINTS (CONS (CAR BOTTOM) (LIST (CADR BOTTOM)))
		TPOINTS (CONS (CAR TOP) (LIST (CADR TOP)))
	)
	(IF (MINUSP PW)
		(SETQ
			BPOINTS (SBS-POINT-CALC BPOINTS PW BOTTOM)
			TPOINTS (SBS-POINT-CALC TPOINTS PW TOP )
			WLENGTH (- WLENGTH PW)
		)
	)
	(WHILE ( < RUNLENGTH WLENGTH)
		(setq
			COUNT (1+ COUNT)
			RUNLENGTH (+ RUNLENGTH (ATOI (CADR (CADR PINFO))))
			DELTALENGTHB (/ (- (cadr (cadr bpoints)) (cadr (car bpoints)))(-  (car (cadr bpoints)) (car (car bpoints))))
			DELTALENGTHT (*(/ (- (CADR (CADR TPOINTS)) (CADR (CAR TPOINTS))) (- (CAR (CADR TPOINTS)) (CAR (CAR TPOINTS)))) PW)
			PLENGTH (- (CADR (CAR TPOINTS)) (CADR (CAR BPOINTS)))
			FLAG 0
		)
		(SBS-PANELINSERT 
			(COND
				((OR
					(AND (> (abs DELTALENGTHB) 0.084) (< (- (CAR (CADR BPOINTS)) (CAR (CAR BPOINTS))) (ABS PW)) (NOT (MINUSP PW))(< (CADR (CADR BPOINTS)) (CADR (CAR BPOINTS))))
					(AND (<= (abs DELTALENGTHB) 0.084) (< (- (CAR (CADR BPOINTS)) (CAR (CAR BPOINTS))) (ABS PW)) (NOT (MINUSP PW)) (> (CADR (CADR BPOINTS)) (CADR (CAR BPOINTS))))
				)
					(PROGN
						(SETQ FLAG (- (CADR (CADR BPOINTS)) (CADR (CAR BPOINTS))))
						(LIST
							(CAR (CAR BPOINTS))
							(CADR (CADR BPOINTS))
							(CADDR (CAR BPOINTS))
						)
					)
				)			
				((OR
					(AND (> (CADR (CADR BPOINTS)) (CADR (CAR BPOINTS))) (NOT (MINUSP PW)) (NOT (= (CAR (CAR BPOINTS)) (CAR (CADR BPOINTS)))) (<= (abs DELTALENGTHB) 0.084))
					(AND (< (CADR (CADR BPOINTS)) (CADR (CAR BPOINTS))) (NOT (MINUSP PW)) (NOT (= (CAR (CAR BPOINTS)) (CAR (CADR BPOINTS)))) (> (abs DELTALENGTHB) 0.084))
					)
					(LIST
						(CAR (CAR BPOINTS))
						(+ (CADR (CAR BPOINTS)) (SETQ FLAG (* DELTALENGTHB PW)))						
						(CADDR (CAR BPOINTS))
					)
				)
				((OR
					(AND (< (CADR (CADR BPOINTS)) (CADR (CAR BPOINTS))) (MINUSP PW) (NOT (= (CAR (CAR BPOINTS)) (CAR (CADR BPOINTS)))) (<= (abs DELTALENGTHB) 0.084))
					(AND (> (CADR (CADR BPOINTS)) (CADR (CAR BPOINTS))) (MINUSP PW) (NOT (= (CAR (CAR BPOINTS)) (CAR (CADR BPOINTS)))) (> (abs DELTALENGTHB) 0.084))
					)
					(LIST
						(CAR (CAR BPOINTS))
						(- (CADR (CAR BPOINTS)) (SETQ FLAG (* DELTALENGTHB PW))) 
						(CADDR (CAR BPOINTS))
					)
				)
				(T (CAR BPOINTS))
			)
			(COND
				((OR 
					(AND (< (CADR (CADR TPOINTS)) (CADR (CAR TPOINTS))) (NOT (MINUSP PW)) (NOT (= (CAR (CAR TPOINTS)) (CAR (CADR TPOINTS)))) (<= (abs DELTALENGTHT) (abs (* 0.084 PW))))
					(AND (> (CADR (CADR TPOINTS)) (CADR (CAR TPOINTS))) (NOT (MINUSP PW)) (NOT (= (CAR (CAR TPOINTS)) (CAR (CADR TPOINTS)))) (> (abs DELTALENGTHT) (abs (* 0.084 PW))))
					)
					(- (+ PLENGTH DELTALENGTHT) FLAG)
				)
				((OR 
					(AND (> (CADR (CADR TPOINTS)) (CADR (CAR TPOINTS))) (MINUSP PW) (NOT (= (CAR (CAR TPOINTS)) (CAR (CADR TPOINTS)))) (<= (abs DELTALENGTHT) (abs (* 0.084 PW))))
					(AND (< (CADR (CADR TPOINTS)) (CADR (CAR TPOINTS))) (MINUSP PW) (NOT (= (CAR (CAR TPOINTS)) (CAR (CADR TPOINTS)))) (> (abs DELTALENGTHT) (abs (* 0.084 PW))))
					)
					(+ (- PLENGTH DELTALENGTHT) FLAG)
				)
				((MINUSP PW) (+ PLENGTH flag))
				(T (- PLENGTH flag))
			)
		PINFO)
		;;Base Points
		;;(VL-CMDF "._CIRCLE" (CAR BPOINTS) "_D" (ATOI (CADR (CADR PINFO))))
		(SETQ BPOINTS (SBS-POINT-CALC BPOINTS PW BOTTOM ))
		;;Top points
		;;(VL-CMDF "._CIRCLE" (CAR TPOINTS) "_D" (ATOI (CADR (CADR PINFO))))
		(SETQ TPOINTS (SBS-POINT-CALC TPOINTS PW TOP ))
	)
	(print (strcat "Number of Panels = " (ITOA COUNT)))
	;;(print "end SBS-CALC-LOOP")
)

;;CALCULATES THE NEXT POINT ALONG A LINE FROM START TO END AT A GIVIN DISTANCE	
(DEFUN SBS-POINT-CALC (POINTS WIDTH PLIST / POINTS P1 P2 P3 WIDTH PLIST)
	;;(PRINT "Start SBS-POINT-CALC")
	(SETQ
		P1 (CAR POINTS)
		P2 (CADR POINTS)
	)
	(COND
		;;RUNNING CONDITION
		((> (DISTANCE (LIST (CAR P1) 0.0 0.0) (LIST (CAR P2) 0.0 0.0)) (ABS WIDTH))
			(SETQ P1
				(LIST 
					(+ (CAR P1) WIDTH)
					(+ (CADR P1) (*(/ (- (CADR P2) (CADR P1)) (- (CAR P2) (CAR P1))) WIDTH))
					(CADDR P1)
				)
			)
		)
		;;END CONDITION
		((NOT (CADR (MEMBER P2 PLIST))))
		;; VERTICAL CHANGE LESS THEN WIDTH RESET
		((<= (ABS (- (CAR (CADR (MEMBER P2 PLIST))) (CAR P1))) (ABS WIDTH))
			(SETQ P3 (CADR (MEMBER P2 PLIST)))
			(WHILE (< (ABS (- (CAR (CADR (MEMBER P3 PLIST))) (CAR P1))) (ABS WIDTH))
				(SETQ P3 (CADR (MEMBER P3 PLIST)))
			)
			(SETQ P1
				(LIST
					(+ (CAR P3) (- WIDTH (- (CAR P3)(CAR P1))))
					(+ (CADR P1) (+ (- (CADR P3) (CADR P1)) (*(/ (- (CADR (CADR (MEMBER P3 PLIST))) (CADR P3)) (- (CAR (CADR (MEMBER P3 PLIST))) (CAR P3))) (- WIDTH (- (CAR P2)(CAR P1))))))
					(CADDR P1)
				)
			)
			(SETQ P2 (CADR (MEMBER P3 PLIST)))				
		)
		;; SLOPE CHANGE RESET
		((CADR (MEMBER P2 PLIST))
			(SETQ P3 (CADR (MEMBER P2 PLIST)))
			(SETQ P1
				(LIST
					(+ (CAR P2) (- WIDTH (- (CAR P2)(CAR P1))))
					(+ (CADR P1) (+ (- (CADR P2) (CADR P1)) (*(/ (- (CADR P3) (CADR P2)) (- (CAR P3) (CAR P2))) (- WIDTH (- (CAR P2)(CAR P1))))))
					(CADDR P1)
				)
			)
			(SETQ P2 P3)
		)
		(T (PRINT "NO MATCH"))
	)
	;;(PRINT "End SBS-POINT-CALC")
	(cons p1 (LIST p2))
)

;;INSERTS A PANEL AND SETS ITS PROPERTIES
(defun SBS-PANELINSERT ( BP PL PINFO / )
	;;(print "Start SBS-PANELINSERT")
	(setq PANEL (STRCAT(CADR (CAR PINFO))(CADR (CADR PINFO))))
	(VL-LOAD-COM)
	(command "_.-insert" PANEL BP "" "" "")
	(SETQ OBJ (VLAX-ENAME->VLA-OBJECT(ENTLAST)))
		(vl-some
		'(lambda ( x )
			(if (= "PANEL LENGTH" (strcase (vla-get-propertyname x)))
				(progn
					(vla-put-value x (vlax-make-variant PL (vlax-variant-type (vla-get-value x))))
					(cond (PL) (t))
				)
			)
		)
		(vlax-invoke OBJ 'getdynamicblockproperties)
	)
	;;(print "End SBS-PANELINSERT")
)

;;SERCHES THE LIST OF COLOUR AVALABILITY BIT'S IN THE COLOUR CHART AGAINST THE CURRENT PANEL AND RETERNS A LIST OF AVALABLE COLOURS
(DEFUN SBS-COLOUR-CHART ( PBIT CTABLE / INFO POS RL PBIT CTABLE CLIST)
;;(print "Start SBS-COLOUR-CHART")
	(SETQ 
		INFO (VL-GET-RESOURCE CTABLE)
		POS 1
		RL " "
	)
	(IF (= (GET_TILE "allColour") "0")
		(progn
			(WHILE (not (null RL))
				(SETQ RL (NTH 2 (read (substr INFO POS ))))
				(FOREACH x RL
					(IF (= x(LOGAND x PBIT))
						(SETQ CLIST (APPEND CLIST (LIST (LIST (STRCAT (CAR (read (substr INFO POS ))) " " (CADR (read (substr INFO POS ))))))))
					)
				)
				(SETQ POS (+ POS 2))
				(if (VL-STRING-POSITION (ASCII "\n") (substr INFO POS))
					(SETQ POS (+ POS (VL-STRING-POSITION (ASCII "\n") (substr INFO POS))))
					(setq rl nil)
				)
			)
		)
		(progn
			(WHILE (not (null RL))
				(SETQ RL (NTH 2 (read (substr INFO POS ))))
				(SETQ CLIST (APPEND CLIST (LIST (LIST (STRCAT (CAR (read (substr INFO POS ))) " " (CADR (read (substr INFO POS ))))))))
				(SETQ POS (+ POS 2))
				(if (VL-STRING-POSITION (ASCII "\n") (substr INFO POS))
					(SETQ POS (+ POS (VL-STRING-POSITION (ASCII "\n") (substr INFO POS))))
					(setq rl nil)
				)
			)
		)
	)
;;(print "End SBS-COLOUR-CHART")
	CLIST
)

;;Checks the panel info bit for a specific bit then resets it to a new value
(defun SBS-BITSET ( PANEL_INFO MASK / )
	(SETQ PANEL_INFO
		(IF (assoc "Panel info" PANEL_INFO)
			(SUBST
				(LIST "Panel info"
					(CAR
						(NTH (READ $VALUE)
							(CWL-BITTOLIST MASK "SBS-PANEL-INFO")
						)
					)
				)
				(assoc "Panel info" PANEL_INFO)
			PANEL_INFO)
			(APPEND
				(LIST "Panel info"
					(CAR
						(NTH (READ $VALUE)
							(CWL-BITTOLIST MASK "SBS-PANEL-INFO")
						)
					)
				)
			PANEL_INFO)
		)
	)
	PANEL_INFO
)