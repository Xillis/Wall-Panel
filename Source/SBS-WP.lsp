;;wall creation function for Steelway building systems
(DEFUN C:SBS-WP (/ oldvar )
	;; set working environment
	(setq oldvar (CWL-SVVCF (list '("CMDECHO" 0) '("OSMODE" 16384))))
	(print oldvar)
	;;Dialog Function
	(SBS-PANEL-DIA "SBS_PanelProperties")
	;; re-set origanal working environment
	(CWL-SVVCF oldvar)
)	