;;wall creation function for steelway building systems
(DEFUN C:SBS-WP (/ oldvar )
	;; set working environment
	(setq oldvar (CWL-SVVCF (list '("CMDECHO" 0) '("OSMODE" 16384))))
	(print oldvar)
	;;main function
	
	;; re-set origanal working environment
	(CWL-SVVCF oldvar)
)	