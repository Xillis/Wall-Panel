(defun SBS-PANEL-DIA (/ DIA-ID PTV WV GV PV PT BSV PID CV COLOUR MRK MKL)
(print "start panel dia")
	(setq DIA_ID (load_dialog "SBS-Dialog.dcl"))
	(IF (NOT (new_dialog "SBS_PanelProperties" DIA_ID))
		(EXIT)
	)
)