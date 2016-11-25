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
(defun CWL-PANEL-DIA ( DIA-Name / DIA-ID )
	(setq DIA_ID (load_dialog "SBS-Dialog.dcl"))
	(IF (NOT (new_dialog DIA-Name DIA_ID))
		(EXIT)
		(eval (list(read DIA-name)))
	)
	(unload_dialog DIA_ID)
)