;;Error handling Function 

(defun CWL-err (msg)
	(if (/= msg "function cancelled")
		(alert (stacat "/nError: " msg))
	)
	(command "._undo" "_e")
	(command "._undo" "_u")
;;	(setq *error* set in list passed from main function TBD
)

;;system variable value capture Function
(defun CWL-SVVCF ( sysvar values / oldvar)
	(foreach var sysvar
		(setq cnt 0)
		(setq oldvar (append oldvar (list getvar var)))
		(setq var (nth cnt Values))
	)
oldvar
)