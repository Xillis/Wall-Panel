;;Panal dialog inishilaization 
(defun SBS_PANEL_INFO ( PASSTHROUGH / PANEL-INFO PASSTHROUGH)
	;;(print "Start SBS-PANEL-INFO")
	(if (null (setq PANEL-INFO (cadr (assoc "Panel info" PASSTHROUGH))))
		(setq PANEL-INFO (car (CWL-START-DIA "SBS_PANEL" "S" PANEL-INFO)))
	)
	(if (not (null PANEL-INFO)) 
		(setq PANEL-INFO
			(CWL-START-DIA 
				(nth 2 (car (CWL-BITTOLIST (cadr PANEL-INFO) "SBS-PANEL-INFO" )))
				"M"
				(list PANEL-INFO)
			)
		)
	)
	;;(print "End SBS-PANEL-INFO")
	PASSTHROUGH
)

;; panel selection dialog Box
(defun SBS_PANEL ( PANEL-INFO / PANEL-INFO TOKEN)
;;(print "Start SBS_PANEL")
	(CWL-DBLIST (CWL-BITTOLIST "all" "SBS-PANEL-INFO") "Panel_type" 1 "s")
	(action_tile "Panel_type" "(setq TOKEN (nth (1-(atoi $value)) (CWL-BITTOLIST \"all\" \"SBS-PANEL-INFO\")))")
	(action_tile "accept" "(setq PANEL-INFO (list \"Panel info\" (car TOKEN))) (done_dialog)")
	(mode_tile "PPanel" 1)
	(start_dialog)
;;(print "End SBS_PANEL")
(list PANEL-INFO)
)

;;Panel information collection dialog Box
(defun SBS_STD_PANEL ( PANEL-INFO / PANEL-INFO MASK )
	(print "Start SBS_STD_PANEL")
	(setq PANEL-INFO (car PANEL-INFO))
	(CWL-DBLIST (CWL-BITTOLIST "all" "SBS-PANEL-INFO") "Panel_type" 1 "s")
	(set_tile "Panel_type"
		(itoa (1+ (atoi (CWL-TILESET
			(cadr PANEL-INFO)
			"all"
			"SBS-PANEL-INFO"
		))))
	)
	(PRINT PANEL-INFO)
	(setq MASK
		(SBS_SET_LIST 
			(SBS_GET_BIT MASK)
			(cadddr (nth (1- (cadr PANEL-INFO)) (CWL-BITTOLIST "all" "SBS-PANEL-INFO")))
		)
	)
	(action_tile "Panel_type" "(setq VALUE $value) (done_dialog 2)")
	(action_tile "Width"
		(strcat
			"(setq MASK (SBS_SET_LIST (SBS_GET_BIT MASK) MASK))"
		)
	)
	(action_tile "Gauge"
		(strcat
			"(setq MASK (SBS_SET_LIST (SBS_GET_BIT MASK) MASK))"
		)
	)
	(action_tile "Profile"
		(strcat
			"(setq MASK (SBS_SET_LIST (SBS_GET_BIT MASK) MASK))"
		)
	)
	(action_tile "Feature_list"
		(strcat
			"(setq MASK (SBS_SET_LIST (SBS_GET_BIT MASK) MASK))"
		)
	)
	(action_tile "accept"
		(strcat
			"(print \"Accept\")"
			"(SBS_GET_BIT)"
			"(done_dialog 1)"
		)
	)
	(action_tile "Cancel"
		(strcat 
			"(print \"cancel\")"
			"(done_dialog 0)"
		)
	)
	(setq DFLAG (start_dialog))
	(cond
		((= DFLAG 2)
			(print (NTH (READ VALUE) (CWL-BITTOLIST "all" "SBS-PANEL-INFO"))))
		(T (PRINT "SHIT"))
	)
			
	(print (subst (list "Flag" DFLAG) (assoc "Flag" PASSTHROUGH) PASSTHROUGH))
	(print "end SBS_STD_PANEL")
	(subst (list "Flag" DFLAG) (assoc "Flag" PASSTHROUGH) PASSTHROUGH)
)

;;Gather tile values and generate panel designation bit
(defun SBS_GET_BIT ( MASK / BIT PANEL COUNT)
;;(print "Start SBS_GET_BIT")
	(setq PANEL (nth (1-(atoi (get_tile "Panel_type"))) (CWL-BITTOLIST "all" "SBS-PANEL-INFO")))
	(setq BIT (car PANEL)
		COUNT 0
	)
	(foreach x '("Width" "Gauge" "Profile")
		(if (and (/= (get_tile x) "0") (/= (get_tile x) ""))
			(setq BIT (+ BIT (car (nth (1-(atoi (get_tile x))) (CWL-BITTOLIST (nth COUNT MASK) (cadr PANEL))))))
		)
		(setq COUNT (1+ COUNT))
	)
	(foreach i (CWL_NUMSTRING_LIST (get_tile "Feature_list"))
		(setq BIT (+ (car (nth i (CWL-BITTOLIST (nth COUNT MASK) (cadr PANEL)))) BIT))
	)
;;(print "End SBS_GET_BIT")
BIT
)

;;populates dropdown list boxes
(defun 	SBS_SET_LIST ( PANEL-BIT MASK-OLD / PANEL-BIT MASK PANEL COUNT FTLIST MASKBIT)
;;(print "Start SBS_SET_LIST")
	(setq PANEL (car (CWL-BITTOLIST PANEL-BIT "SBS-PANEL-INFO")))
	(setq MASKBIT 0)
	(foreach x (CWL-BITTOLIST PANEL-BIT (cadr PANEL))
		(if (not (null (caddr x)))
			(setq  MASKBIT (boole 7 (car (caddr x)) MASKBIT))
		)
	)
	(setq MASK 
		(if (null MASKBIT)
			(cadddr PANEL)
			(mapcar 
				'(lambda (x)
					(boole 2 x MASKBIT)
				)
				(cadddr PANEL)
			)
		)
		COUNT 0
	)
	(foreach x '("Width" "Gauge" "Profile")
		(CWL_LIST_LENGHT_CHECK (CWL-BITTOLIST (nth COUNT MASK) (strcase (cadr PANEL))) x 1)
		(if (/= (logand panel-bit (nth COUNT MASK-OLD)) 0)
			(set_tile x (itoa (1+ (atoi (CWL-TILESET (logand panel-bit (nth COUNT MASK-OLD)) (nth COUNT MASK) (cadr PANEL))))))
		)
		(setq COUNT (1+ COUNT))
	)
	(CWL-DBLIST (CWL-BITTOLIST (nth COUNT MASK) (strcase (cadr PANEL)))  "Feature_list" 1 "n")
	(foreach x (CWL-BITTOLIST (logand panel-bit (nth COUNT MASK-OLD)) (strcase (cadr PANEL)))
		(if (null FTLIST)
			(setq FTLIST (CWL-TILESET (car x) (nth COUNT MASK) (cadr PANEL)))
			(setq FTLIST (strcat(CWL-TILESET (car x) (nth COUNT MASK) (cadr PANEL)) " " FTLIST))
		)
	)
	(if (not (null FTLIST))
		(set_tile "Feature_list" FTLIST)
	)
	(SBS_PPID_LIST (boole 2 PANEL-BIT (cadr (cadddr PANEL))))
;;(print "End SBS_SET_LIST")
MASK
)

;;PPID List generation
(defun SBS_PPID_LIST (SBIT / SBIT INFO POS RL)
	;;(print "start SBS_PPID_LIST")
	(setq 
		INFO (vl-get-resource "SBS-PANEL-ID")
		POS 1
		RL " "
	)
	(while (not (null RL))
		(setq RL (read (substr INFO POS )))
		(if (= SBIT (car RL))
			(set_tile "PPID" (cadr RL))
		)
		(setq POS (+ POS 2))
		(if (vl-string-position (ascii "\n") (substr INFO POS))
			(setq POS (+ POS (vl-string-position (ascii "\n") (substr INFO POS))))
			(setq RL nil)
		)
	)
	;;(print "end SBS_PPID_LIST")
)