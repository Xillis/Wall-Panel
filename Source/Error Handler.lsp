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
