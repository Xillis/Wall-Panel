;;Main command Functions
;;======================================================================
(DEFUN C:SBS-WP (/ SP EP WD LP HP PL RP PW PX PY PH OSM CMD BH PS BP CO *error* msg )
	(DEFUN *error* ( msg )
		(if CMD (setvar 'CMDECHO CMD))
		(if OSM (setvar 'OSMODE OSM))
		(if (not (member msg '("Function cancelled" "quit / exit abort")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
	;;Wall info Gathering
	(SETQ
		CMD (GETVAR "CMDECHO")
		OSM (GETVAR "OSMODE")
		SP (GETPOINT "\nSelect Wall Start Point:")
		EP (GETPOINT "\nSelect Wall End Point:")
		LP (GETPOINT "\nLow Eave hight:")
		HP (GETPOINT "\nHigh eave or peak")
	)
	(SBS-PANEL-DIA)
	(SETQ 
		PW (atoi(NTH 1 (assoc "WIDTH" ~PANEL~)))
		BH (distof (NTH 1 (assoc "BASE" ~PANEL~)) 4)
	)		
	;;Calculate Length or wall "x" and "y" of roof slope 
	(SETQ 
		WD (DISTANCE (LIST (CAR SP) 0.0 0.0) (LIST (CAR EP) 0.0 0.0))
		PH (DISTANCE (LIST 0.0 (CADR SP) 0.0) (LIST 0.0 (CADR HP) 0.0))
		PX (DISTANCE (LIST (CAR LP) 0.0 0.0) (LIST (CAR HP) 0.0 0.0))
		PY (DISTANCE (LIST 0.0 (CADR LP) 0.0) (LIST 0.0 (CADR HP) 0.0))
		PS (DISTANCE (LIST 0.0 (CADR SP) 0.0) (LIST 0.0 (CADR LP) 0.0))
	)
	;;Calculate Pitch
	(SETQ RP (*(/ PY PX) PW))
	(print (STRCAT "Pitch: " (VL-PRINC-TO-STRING (*(/ PY PX) 12)) "/12"))
	(print RP)
	;;Determine panel length
	(print "a")
	(print SP)
	(print EP)
	(print LP)
	(print HP)
	(print PW)
	(print BH)
	(cond 
		((= RP 0) (SETQ PL (1- (- PS BH))))
		((and (> (*(/ PY PX) 12) 1.1) (and (<(CAR SP) (1+(CAR HP))) (>(car SP)(1-(car HP))))) (SETQ PL (- PH BH)))
		((and (> (*(/ PY PX) 12) 1.1) (and (<(CAR SP) (1+(CAR LP))) (>(car SP)(1-(car LP))))) (SETQ PL (+ RP (- PS BH))))
		((and (<= (*(/ PY PX) 12) 1.1) (and (<(CAR SP) (1+(CAR HP))) (>(car SP)(1-(car HP))))) (SETQ PL (- (- PH BH) RP)))
		((and (<= (*(/ PY PX) 12) 1.1) (and (<(CAR SP) (1+(CAR LP))) (>(car SP)(1-(car LP))))) (SETQ PL (- PS BH)))
		(T (setq PL PS) (PRINT "Start point and High/Low point are not aligned in the X coordinate. Output may not be as expected"))
	)
	;;Set Start point
	(print "b")
	(IF (< (CAR EP) (CAR SP))
		(SETQ BP (LIST (- (CAR SP) PW) (+ (CADR SP) BH) (CADDR SP)))
		(SETQ BP (LIST (CAR SP) (+(CADR SP) BH) (CADDR SP)))	
	)	
	;; Panel wall creation Function  
	(SETQ CO 0)
	(vla-StartUndoMark 
		(vla-get-ActiveDocument 
			(vlax-get-acad-object)
		)
	)
	(SETVAR "CMDECHO" 0)
	(SETVAR "OSMODE" 16384)
	(WHILE (< CO WD )
		(IF (> PL (- PH BH))
			(SETQ PL (- PH BH))
		)
		(IF (AND 
				(< PL (- PS BH))
				(/= RP 0)
			)
			(SETQ PL (- PS BH))
		)
		(PRINT BP)
		(SBS-PANELINSERT BP PL )
		(COND
			((AND (< (- PX CO) PW) (> CO PX)) (SETQ PL (- PL RP)))
			((AND (< (- PX CO) PW) (<= CO PX) (> (*(/ PY PX) 12) 1.1)) (SETQ PL (- (- PH BH)(* (/ RP PW) (- PW(- PX CO))))))
			((AND (< (- PX CO) PW) (<= CO PX) (<= (*(/ PY PX) 12) 1.1)) (SETQ PL (- (- (- PH BH) RP)(* (/ RP PW) (- PW(- PX CO))))))
			((AND (>= (- PX CO) PW) (and (<(CAR SP) (1+(CAR HP))) (>(car SP)(1-(car HP))))) (SETQ PL (- PL RP)))
			((AND (>= (- PX CO) PW) (and (<(CAR SP) (1+(CAR LP))) (>(car SP)(1-(car LP))))) (SETQ PL (+ PL RP)))
		)
		(IF (> (CAR EP) (CAR SP))
			(SETQ BP (LIST (+ (CAR BP) PW) (CADR BP) (CADDR BP)))
			(SETQ BP (LIST (- (CAR BP) PW) (CADR BP) (CADDR BP)))
		)
		(SETQ CO (+ CO PW))
	)
	(vla-EndUndoMark 
		(vla-get-ActiveDocument 
			(vlax-get-acad-object)
		)
	)
	(SETVAR "CMDECHO" CMD)
	(SETVAR "OSMODE" OSM)
	(PRINC)
)

;;==================================================================================================
;;Dialog control 
;;==================================================================================================

;;Main Dialog function
;;-------------------------
(defun SBS-PANEL-DIA (/ DIA-ID PTV WV GV PV PT BSV PID CV COLOUR MRK MKL)
(print "start panel dia")
	(setq DIA_ID (load_dialog "OLD-SBS-Dialog.dcl"))
	(IF (NOT (new_dialog "SBS_PanelProperties" DIA_ID))
		(EXIT)
	)
	(PRINT "1")
	(SBS_PANEL_LIST "WALL")
	(if (AND(Listp ~PANEL~) (/= ~PANEL~ NIL))
		(PROGN
			(setq PTV  (ITOA(VL-POSITION (NTH 1 (assoc "PANEL" ~PANEL~)) ~PANEL_LIST~)))
			(SETQ PT (NTH (READ PTV) ~PANEL_LIST~))
			(SBS_PANEL_PROPERTIES PT)
			(SETQ 
				WV (ITOA (VL-POSITION (NTH 1 (assoc "WIDTH" ~PANEL~)) (NTH 1 ~PANELINFO~)))
				GV (ITOA (VL-POSITION (NTH 1 (assoc "GAUGE" ~PANEL~)) (NTH 2 ~PANELINFO~)))
				PV (ITOA (VL-POSITION (NTH 1 (assoc "PROFILE" ~PANEL~)) (NTH 3 ~PANELINFO~)))
				BSV (NTH 1 (assoc "BASE" ~PANEL~))
				PID (NTH 1 (ASSOC "PART" ~PANEL~))
			)
			(set_tile "Base" BSV)
			(SET_TILE "PPID" PID)
			(SBS_COLOUR_LIST PT WV GV)
			(setq CV (ITOA (VL-POSITION (NTH 1 (assoc "COLOUR" ~PANEL~)) ~COLOUR_LIST~)))
			(set_tile "Colour" CV)
		)
		(PROGN
			(set_tile "Panel_type" "0")
			(setq PTV "0")
			(SETQ PT (NTH (READ PTV) ~PANEL_LIST~))
			(SBS_PANEL_PROPERTIES PT)
			(SETQ 
				WV "0"
				GV "0"
				PV "0"
			)
			(set_tile "Base" "-1")
			(SBS_COLOUR_LIST PT WV GV)
			(SETQ ~PART~ (NTH 4 ~PANELINFO~))
			(SBS-PART-LIST)
		)
	)
	(SETQ MKL '("ESL-" "ESR-" "SSF-" "SSB-"))
	(START_LIST "BPV")
		(MAPCAR 'ADD_LIST MKL)
	(END_LIST)
	(set_tile "Panel_type" PTV)
	(set_tile "Width" WV)
	(set_tile "Gauge" GV)
	(set_tile "Profile" PV)
	(SBS-FCREATE)
	(action_tile "Panel_type" 
		(STRCAT
			"(setq PTV $value)"
			"(setq	PT (NTH (READ PTV) ~PANEL_LIST~))"
			"(SBS_PANEL_PROPERTIES PT)"
			"(SETQ 
				WV \"0\"
				GV \"0\"
				PV \"0\"
			)"
			"(START_LIST \"Feature_list\")"
			"(END_LIST)"
			"(SBS_COLOUR_LIST PT WV GV)"
			"(SETQ CV \"0\")"
			"(set_tile \"Width\" WV)"
			"(set_tile \"Gauge\" GV)"
			"(set_tile \"Profile\" PV)"
			"(SETQ ~PART~ (NTH 4 ~PANELINFO~))"
			"(SBS-PART-LIST)"
		)
	)
	(action_tile "Width" 
		(STRCAT
			"(setq WV $value)"
			"(SBS_COLOUR_LIST PT WV GV)"
			"(SBS-BITSET WV 4096)"
			"(SBS-PART-LIST)"
		)
	)
	(action_tile "Profile" 
		(STRCAT
			"(SETQ PV $value)"
			"(SETQ ~PART~ (- ~PART~ (LOGAND ~PART~ 65536)))"
			"(IF (=(LOGAND ~PART~ 2) 2)(SETQ ~PART~ (- ~PART~ (LOGAND ~PART~ 32768))))"
			"(SBS-BITSET PV 512)"
			"(SBS-PART-LIST)"
			"(SBS-FCREATE)"
		)
	)
	(action_tile "Colour" 
		(STRCAT
			"(SETQ CV $value)"
			"(SETQ COLOUR (list(NTH (READ CV) ~COLOUR_LIST~)))"
			"(IF (OR (= (nth 0 COLOUR) \"AZ165\") (= (nth 0 COLOUR) \"AZ75\") (= (nth 0 COLOUR) \"G90\"))
				(progn
					(SBS-BITSET \"1\" 1024)
					(SETQ ~PART~ (- ~PART~ (LOGAND ~PART~ 229376)))
				)
				(SBS-BITSET \"0\" 1024)
			)"
			"(SBS-PART-LIST)"
			"(SBS-FCREATE)"
		)
	)	
	(action_tile "Gauge" 
		(STRCAT
			"(setq GV $value)"
			"(SBS_COLOUR_LIST PT WV GV)"
		)
	)
	(action_tile "Fset" 
		(STRCAT
			"(setq CV (GET_TILE \"Colour\"))"
			"(setq PV (get_tile \"Profile\"))"		
			"(cond
				((= CV \"\") (alert \"Select a Panel Colour before proceeding.\"))
				((= PV \"\") (alert \"Select Panel Profile before proceeding.\"))
				(T (SBS_PFEATURES PT)(SBS-PART-LIST))
			)"
		)
	)
	(action_tile "accept" 
		(strcat
			"(setq 
				PTV (get_tile \"Panel_type\")
				WV (get_tile \"Width\")
				GV (get_tile \"Gauge\")
				PV (get_tile \"Profile\")
				CV (get_tile \"Colour\")
				BSV (get_tile \"Base\")
				PID (get_tile \"PPID\")
				MRK (LIST(STRCAT(NTH (READ (GET_TILE \"BPV\")) MKL) (GET_TILE \"BVS\")))
			)"
			"(if (= CV \"\") (alert \"Select a Panel Colour before proceeding.\")(done_dialog))"
		)
	)
	(action_tile "cancel"
		(strcat 
			"(done_dialog)"
			"(exit)"
		)
	)
	(setq State (start_dialog))
	(unload_dialog DIA_ID)
	(SBS-PANELDEF PTV WV GV PV CV BSV PID MRK)
(print "end panel dia")
)

;;panel Features Dialog
(defun SBS_PFEATURES ( PT / )
	(COND
		((= PT "StormSeal") (SBS-STORMSEAL-DIA))
		((= PT "StrucSeal") (SBS-STRUCSEAL-DIA))
		((= PT "DiamondSeal") (SBS-DVSEAL-DIA))
		((= PT "VersaSeal") (SBS-DVSEAL-DIA))
		(T (PRINT "NOPE"))
	)
)

(DEFUN SBS-FCREATE ( / MLIST FC FDLIST PFINFO POS )
	(SETQ
		PFINFO (VL-GET-RESOURCE "Panel_Finishes")
		POS 1
		MLIST " "
	)
	(while (/= MLIST NIL)
		(setq MLIST (read (substr PFINFO POS )))
		(if (/= MLIST NIL)
			(PROGN
				(IF (not(ZEROP (LOGAND ~PART~ (nth 0 MLIST))))
					(SETQ
						MLIST (nth 1 MLIST)
						FDLIST (append FDLIST (list MLIST))
					)	
				)
				(SETQ 
					POS (+ POS 2)
					POS (+ POS (VL-STRING-POSITION (ASCII "\n") (substr PFINFO POS)))
				)
			)
		)
	)
	(START_LIST "Feature_list")
		(MAPCAR 'ADD_LIST FDLIST)
	(END_LIST)
)

;;StormSeal dialg control
(DEFUN SBS-STORMSEAL-DIA ( / SFV COV DIA_ID1 )
	(setq DIA_ID1 (load_dialog "OLD-SBS-Dialog.dcl"))
		(IF (NOT (new_dialog "SBS_Stormseal" DIA_ID1))
			(EXIT)
		)
		(MODE_TILE "A01" 0)
		(MODE_TILE "A02" 0)
		(MODE_TILE "BC" 0)
		(MODE_TILE "UW" 0)
		(MODE_TILE "PFU" 0)
		(cond
			((=(LOGAND ~PART~ 65536) 65536) (SET_TILE "PFU" "1"))
			((=(LOGAND ~PART~ 131072) 131072) (SET_TILE "UW" "1"))
		)
		(cond
			((=(LOGAND ~PART~ 8192) 8192) (SET_TILE "A01" "1") (MODE_TILE "UW" 1) (SET_TILE "UW" "0"))
			((=(LOGAND ~PART~ 16384) 16384) (MODE_TILE "UW" 1) (MODE_TILE "PFU" 1) (SET_TILE "UW" "0") (SET_TILE "PFU" "0"))
			((=(LOGAND ~PART~ 32768) 32768) (SET_TILE "BC" "1") (MODE_TILE "UW" 1) (SET_TILE "UW" "0"))
		)
		(COND
			((NOT(ZEROP (LOGAND ~PART~ 1024)))(MODE_TILE "UW" 1)(MODE_TILE "PFU" 1)(MODE_TILE "BC" 1))
			((NOT(ZEROP (LOGAND ~PART~ 512))) (MODE_TILE "PFU" 1))
		)
		(Action_tile "Surf" 
			(STRCAT
				"(if (= SFV $value)
					(PROGN
						(set_tile \"A01\" \"0\")
						(set_tile \"A02\" \"0\")
						(set_tile \"BC\" \"0\")
						(setq SFV nil)
					)
					(SETQ SFV $value)
				)"
				"(COND
					((OR(= (GET_TILE \"A01\") \"1\")(= (GET_TILE \"BC\") \"1\"))(MODE_TILE \"UW\" 1)(SET_TILE \"UW\" \"0\")(MODE_TILE \"PFU\" 0))
					((= (get_tile \"A02\") \"1\")(MODE_TILE \"UW\" 1)(MODE_TILE \"PFU\" 1)(SET_TILE \"UW\" \"0\") (SET_TILE \"PFU\" \"0\"))
					(T (MODE_TILE \"UW\" 0)(MODE_TILE \"PFU\" 0))
				)"
				"(COND
					((NOT(ZEROP (LOGAND ~PART~ 1024)))(MODE_TILE \"UW\" 1)(MODE_TILE \"PFU\" 1))
					((NOT(ZEROP (LOGAND ~PART~ 512))) (MODE_TILE \"PFU\" 1))
				)"
			)
		)
		(Action_tile "Coat"
			(STRCAT
				"(if (= COV $value)
					(PROGN
						(set_tile \"UW\" \"0\")
						(set_tile \"PFU\" \"0\")
						(setq COV nil)
					)
					(SETQ COV $value)
				)"
			)
		)
		(ACTION_TILE "accept" 
			(strcat
				"(SBS-BITSET (GET_TILE \"A01\") 8192)"
				"(SBS-BITSET (GET_TILE \"A02\") 16384)"
				"(SBS-BITSET (GET_TILE \"BC\") 32768)"
				"(SBS-BITSET (GET_TILE \"PFU\") 65536)"
				"(SBS-BITSET (GET_TILE \"UW\") 131072)"
				"(done_dialog)"
			)
		)
		(setq State (start_dialog))
	(unload_dialog DIA_ID1)
	(SBS-FCREATE)
)

;;Strucseal 36 dialog control
(DEFUN SBS-STRUCSEAL-DIA ( / SFV COV FLIST DIA_ID1 FLU)
	(setq DIA_ID1 (load_dialog "OLD-SBS-Dialog.dcl"))
		(IF (NOT (new_dialog "SBS_StrucSeal" DIA_ID1))
			(EXIT)
		)
		(MODE_TILE "Flut" 0)
		(MODE_TILE "A01" 0)
		(MODE_TILE "A02" 0)
		(MODE_TILE "BC" 0)
		(MODE_TILE "UW" 0)
		(MODE_TILE "PFU" 0)
		
		(cond
			((=(LOGAND ~PART~ 65536) 65536) (SET_TILE "PFU" "1"))
			((=(LOGAND ~PART~ 131072) 131072) (SET_TILE "UW" "1"))
		)
		(cond
			((=(LOGAND ~PART~ 8192) 8192) (SET_TILE "A01" "1") (MODE_TILE "UW" 1) (SET_TILE "UW" "0") (MODE_TILE "PFU" 1) (SET_TILE "PRU" "0"))
			((=(LOGAND ~PART~ 16384) 16384) (SET_TILE "A02" "1") (MODE_TILE "UW" 1) (MODE_TILE "PFU" 1) (SET_TILE "UW" "0") (SET_TILE "PFU" "0"))
			((=(LOGAND ~PART~ 32768) 32768) (SET_TILE "BC" "1") (MODE_TILE "UW" 1) (SET_TILE "UW" "0") (MODE_TILE "PFU" 1) (SET_TILE "PRU" "0"))
		)
		(IF (=(LOGAND ~PART~ 262144) 262144)
			(progn
				(SET_TILE "Flut" "1")
				(MODE_TILE "A01" 1)
				(MODE_TILE "A02" 1)
			)
			
		)
		(COND
			((NOT(ZEROP (LOGAND ~PART~ 1024)))(MODE_TILE "UW" 1)(MODE_TILE "PFU" 1)(MODE_TILE "BC" 1))
			((NOT(ZEROP (LOGAND ~PART~ 512))) (MODE_TILE "PFU" 1)(MODE_TILE "BC" 1))
		)
		(action_tile "Flut"
			(strcat
				"(IF (= FLU $value)
					(PROGN
						(SET_TILE \"Flut\" \"0\")
						(SETQ FLU nil)
					)
					(SETQ FLU $value)
				)"
				"(if (= \"1\" (GET_TILE \"Flut\"))
					(progn
						(MODE_TILE \"A01\" 1)
						(set_tile \"A01\" \"0\")
						(MODE_TILE \"A02\" 1)
						(SET_TILE \"A02\" \"0\")
					)
					(PROGN
						(MODE_TILE \"A01\" 0)
						(MODE_TILE \"A02\" 0)
					)
				)"
			)
		)
		(Action_tile "Surf" 
			(STRCAT
				"(if (= SFV $value)
					(PROGN
						(set_tile \"A01\" \"0\")
						(set_tile \"A02\" \"0\")
						(set_tile \"BC\" \"0\")
						(setq SFV nil)
					)
					(SETQ SFV $value)
				)"
				"(IF (OR(= (GET_TILE \"A01\") \"1\")(= (GET_TILE \"BC\") \"1\")(= (get_tile \"A02\") \"1\"))
					(PROGN
						(MODE_TILE \"UW\" 1)
						(MODE_TILE \"PFU\" 1)
						(SET_TILE \"UW\" \"0\")
						(SET_TILE \"PFU\" \"0\")
					)
					(PROGN
						(MODE_TILE \"UW\" 0)
						(MODE_TILE \"PFU\" 0)
					)
				)"
				"(COND
					((NOT(ZEROP (LOGAND ~PART~ 1024)))(MODE_TILE \"UW\" 1)(MODE_TILE \"PFU\" 1))
					((NOT(ZEROP (LOGAND ~PART~ 512))) (MODE_TILE \"PFU\" 1))
				)"
			)
		)
		(Action_tile "Coat"
			(STRCAT
				"(if (= COV $value)
					(PROGN
						(set_tile \"UW\" \"0\")
						(set_tile \"PFU\" \"0\")
						(setq COV nil)
					)
					(SETQ COV $value)
				)"
			)
		)
		(ACTION_TILE "accept" 
			(strcat
				"(SBS-BITSET (GET_TILE \"A01\") 8192)"
				"(SBS-BITSET (GET_TILE \"A02\") 16384)"
				"(SBS-BITSET (GET_TILE \"BC\") 32768)"
				"(SBS-BITSET (GET_TILE \"PFU\") 65536)"
				"(SBS-BITSET (GET_TILE \"UW\") 131072)"
				"(SBS-BITSET (GET_TILE \"Flut\") 262144)"
				"(done_dialog)"
			)
		)
		(setq State (start_dialog))
	(unload_dialog DIA_ID1)
	(SBS-FCREATE)
)

(DEFUN SBS-DVSEAL-DIA ( / SFV COV DIA_ID1 )
	(setq DIA_ID1 (load_dialog "OLD-SBS-Dialog.dcl"))
		(IF (NOT (new_dialog "SBS_DVseal" DIA_ID1))
			(EXIT)
		)
		(MODE_TILE "UW" 0)
		(MODE_TILE "PFU" 0)
		(cond
			((=(LOGAND ~PART~ 65536) 65536) (SET_TILE "PFU" "1"))
			((=(LOGAND ~PART~ 131072) 131072) (SET_TILE "UW" "1"))
		)
		(COND
			((NOT(ZEROP (LOGAND ~PART~ 1024)))(MODE_TILE "UW" 1)(MODE_TILE "PFU" 1))
			((NOT(ZEROP (LOGAND ~PART~ 512))) (MODE_TILE "PFU" 1))
		)
		(Action_tile "Coat"
			(STRCAT
				"(if (= COV $value)
					(PROGN
						(set_tile \"UW\" \"0\")
						(set_tile \"PFU\" \"0\")
						(setq COV nil)
					)
					(SETQ COV $value)
				)"
			)
		)
		(ACTION_TILE "accept" 
			(strcat
				"(SBS-BITSET (GET_TILE \"PFU\") 65536)"
				"(SBS-BITSET (GET_TILE \"UW\") 131072)"
				"(done_dialog)"
			)
		)
		(setq State (start_dialog))
	(unload_dialog DIA_ID1)
	(SBS-FCREATE)
)


