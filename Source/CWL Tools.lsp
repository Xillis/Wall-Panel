;;Error handling Function 

(defun CWL-ERR (msg)
	(if (/= msg "function cancelled")
		(alert (stacat "/nError: " msg))
	)
	(command "._undo" "_e")
	(command "._undo" "_u")
;;	(setq *error* set in list passed from main function TBD
)

;;system variable value capture Function
(defun CWL-SVVCF ( sysvar / oldvar)
		(foreach var sysvar
			(setq oldvar (append oldvar (list (list(nth 0 var) (getvar (nth 0 var))))))
			(setvar (nth 0 var) (nth 1 var))
		)
	oldvar
)

;;Dialog Pass through Function
(defun CWL-PANEL-DIA ( DIA-Name DIA-M / DIA-ID )
	(setq DIA_ID (load_dialog "SBS-Dialog.dcl"))
	(cond
		((= DIA-M "M") 	(PROGN
							(setq DFlag 2)	
							(while (>= DFlag 2)
								(IF (NOT (new_dialog DIA-Name DIA_ID))
									(EXIT)
									(SETQ DFLAG (eval (list(read DIA-name) DFLAG )))
								)
							)
						))
		((= DIA-M "S")	(PROGN
							(IF (NOT (new_dialog DIA-Name DIA_ID))
								(EXIT)
								(eval (list(read DIA-name)))
							)
						))
	)
	(unload_dialog DIA_ID)
)

;; extract points from a polyline
(defun CWL-PPOINTS ( / Pline PPoints CK )
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
	(print PPoints)
	PPoints
)