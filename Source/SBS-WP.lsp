;;wall creation function for Steelway building systems
(DEFUN C:SBS-WP (/ oldvar )
	;; set working environment
	(setq oldvar (CWL-SVVCF (list '("CMDECHO" 0) '("OSMODE" 16384))))
	;;Dialog Function
	(CWL-PANEL-DIA "SBS_WallProperties")
	;; re-set original working environment
	(CWL-SVVCF oldvar)
)	
