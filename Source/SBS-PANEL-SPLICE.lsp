(defun C:SBS-WP-SPLIT ( / POS PANELS-SS INDEX P1x P2x ~Change_Panel_Length ~CWL-SVVCF OLDVAR FirstColour NEWPANEL PANEL MODE BPOINT TRIGER)
	(Prompt "C:SBS-WP-SPLIT V1.0.0")
	;;Define error handling 
	(defun *error* (msg)
		(vl-bt)
		(princ "error: ")
		(princ msg)
		(princ)
	)
	;;set undo point
	(vla-StartUndoMark 
		(vla-get-ActiveDocument 
			(vlax-get-acad-object)
		)
	)
	;;Define variable handling routine
	(defun ~CWL-SVVCF ( SYSVAR / SYSVAR OLDVAR)
		(foreach var SYSVAR
			(setq OLDVAR (append OLDVAR (list (list(nth 0 var) (getvar (nth 0 var))))))
			(setvar (nth 0 var) (nth 1 var))
		)
	OLDVAR
	)
	;;Get user selectionset filer for blocks
	(setq
		PANEL-SS(ssget '((0 . "INSERT")))
		INDEX 0
	)
	;;Filter selectionset for blocks not containing the panel length dynamic dimention
	(while (/= NIL (ssname PANEL-SS INDEX))
		(setq TRIGER nil)
		(vl-some
			'(lambda ( x )
				(if (= "PANEL LENGTH" (strcase (vla-get-propertyname x)))
					(setq TRIGER T)
				)
			)
			(vlax-invoke (VLAX-ENAME->VLA-OBJECT (ssname PANEL-SS INDEX)) 'getdynamicblockproperties)
		)
		(if (null TRIGER)
			(ssdel (ssname PANEL-SS INDEX) PANEL-SS)
		)
		(setq INDEX (1+ INDEX))
	)
	;;Save current ucs and set to world
	(setq INDEX 0)
	(command-s "ucs" "na" "s" "pstemp")
	(command-s "ucs" "w")
	;;get user defind point for lap location
	(setq pos (getpoint "Select a point along the lap line:"))
	;;Define sub routine to change panel length
	(defun ~Change_Panel_Length (CPL-PANEL PANEL-LENGTH / )
		(vl-some
			'(lambda ( x )
				(if (= "PANEL LENGTH" (strcase (vla-get-propertyname x)))
					(vla-put-value x (vlax-make-variant PANEL-LENGTH))
				)
			)
			(vlax-invoke (VLAX-ENAME->VLA-OBJECT CPL-PANEL) 'getdynamicblockproperties)
		)
	)
	;;Define sub routine to cahnge the panel Propertis
	(defun ~Change_Panel_Prop (CPP-PANEL TEMPLATE-PANEL / )
		(foreach x '("COLOUR" "PART" "PROFILE" "GAUGE")
			(setpropertyvalue CPP-PANEL x (getpropertyvalue TEMPLATE-PANEL x))
		)
	)
	;;get input form user for toe up or down lap
	(initget 1 "U D Up Down")
	(setq MODE (strcase (getkword "\nGirt toe Up or Down? ")))
	;;set variables for routine execution
	(setq OLDVAR (~CWL-SVVCF (list '("CMDECHO" 0) '("OSMODE" 16384))))
	(command-s "zoom" "e")
	;;exicute loop of items in selectionset
	(while (/= NIL(setq PANEL (ssname PANEL-SS INDEX)))
		(command "ucs" "ob" (ssname PANEL-SS INDEX)) ;;set ucs to ss object
		;;get insertion point of object
		(setq BPOINT
			(trans
				(list
					(getpropertyvalue PANEL "Position/X")
					(getpropertyvalue PANEL "Position/Y")
					(getpropertyvalue PANEL "Position/Z")
				)
			0 1)
		)
		(if (> (getpropertyvalue PANEL "AcDbDynBlockPropertyPanel Length") (- (cadr (trans POS 0 1)) (cadr BPOINT))) ;;cheack to see if lap position on panel is hight then panel length
			(progn
				(command "_.-insert" (getpropertyvalue PANEL "BlockTableRecord/Name") BPOINT "" "" "") ;;insert new panel at ss object insertion point
				;;set new panel length and reset existing panel length and insertion point for lap location and toe of Girt
				(if (or (= "U" MODE) (= "UP" MODE))
					(progn
						(~Change_Panel_Length (entlast) (+ 3 (- (cadr (trans POS 0 1)) (cadr BPOINT))))
						(~Change_Panel_Length PANEL (1+ (- (getpropertyvalue PANEL "AcDbDynBlockPropertyPanel Length") (- (cadr (trans POS 0 1)) (cadr BPOINT)))))
						(vlax-put-property (VLAX-ENAME->VLA-OBJECT PANEL) 'InsertionPoint (vlax-3d-point (trans (list (car BPOINT) (1- (cadr (trans POS 0 1))) (caddr BPOINT)) 1 0)))
					)
					(progn
						(~Change_Panel_Length (entlast) (1+ (- (cadr (trans POS 0 1)) (cadr BPOINT))))
						(~Change_Panel_Length PANEL (+ 3 (- (getpropertyvalue PANEL "AcDbDynBlockPropertyPanel Length") (- (cadr (trans POS 0 1)) (cadr BPOINT)))))
						(vlax-put-property (VLAX-ENAME->VLA-OBJECT PANEL) 'InsertionPoint (vlax-3d-point (trans (list (car BPOINT) (- (cadr (trans POS 0 1)) 3) (caddr BPOINT)) 1 0)))
					)
				)
				;;set properties of new panel to match propertis of existing
				(~Change_Panel_Prop (entlast) PANEL)
				(setq acadObj (vlax-get-acad-object))
				(setq FirstColour (vlax-create-object (strcat "AutoCAD.AcCmColor." (substr (getvar "ACADVER")1 2))))
				(vla-SetColorBookColor FirstColour "Steelway Cladding Colourbook" (getpropertyvalue PANEL "Color"))
				(vla-put-TrueColor (VLAX-ENAME->VLA-OBJECT (entlast)) FirstColour)
				(vlax-release-object FirstColour)
			)
		)
		(setq INDEX (1+ INDEX))
	)
	;;close loop
	;;reset zoom and ucs stat
	(command-s "zoom" "p")
	(command-s "ucs" "na" "r" "pstemp")
	(command-s "ucs" "na" "d" "pstemp")
	;;reset variables
	(~CWL-SVVCF OLDVAR)
	;;close undo point
	(vla-EndUndoMark 
		(vla-get-ActiveDocument 
			(vlax-get-acad-object)
		)
	)
	(princ)
)

		;;P1x (getpropertyvalue (ssname PANEL-SS INDEX) "Position/X")
		;;P2x (getpropertyvalue (ssname PANEL-SS INDEX) "Position/X")
	;;)
	;;(while (/= NIL(setq PANEL (ssname PANEL-SS INDEX)))
	;;	(cond
	;;		(( < P2x (getpropertyvalue PANEL "Position/X")) 
	;;			(setq P2x (getpropertyvalue PANEL "Position/X"))
	;;		)
	;;		(( > P1x (getpropertyvalue PANEL "Position/X")) 
	;;			(setq P1x (getpropertyvalue PANEL "Position/X"))
	;;		)
	;;		(t)
	;;	)
	;;	(setq INDEX (1+ INDEX))
	;;)
	;;(setq
	;;	P2x (+ P2x (/ (- P2x P1x) (1- (sslength PANEL-SS))))
	;;	INDEX 0
	;;)
	;;(while (member (car (setq pos (grread t 5 0))) '(5))
	;;	(redraw)
	;;	(grdraw
	;;		(list P1x (cadr (cadr pos)) (getpropertyvalue (ssname PANEL-SS INDEX) "Position/Z"))
	;;		(list P2x (cadr (cadr pos)) (getpropertyvalue (ssname PANEL-SS INDEX) "Position/Z")) 5)
	;;)
	;;(setq pos (cadr (cadr pos)))
