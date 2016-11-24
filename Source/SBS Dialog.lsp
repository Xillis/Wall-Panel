;;Dialog Pass through Function
(defun SBS-PANEL-DIA ( DIA-Name / DIA-ID DIA_State)
	(setq DIA_ID (load_dialog "SBS-Dialog.dcl"))
	(IF (NOT (new_dialog DIA-Name DIA_ID))
		(EXIT)
		(eval (list(read DIA-name)))
	)
	(setq DIA_State (start_dialog))
	(unload_dialog DIA_ID)
)

;; Test
(defun SBS_WallProperties ()
	(setq
		width (dimx_tile "Wimage")
		hight (dimy_tile "Wimage")
	)
	(start_image "Wimage")
		(fill_image 0 0 width hight 250)
		(vector_image 5 5 30 15 7)
		(vector_image 30 15 55 5 7)
		(vector_image 5 25 30 35 7)
		(vector_image 30 35 55 25 7)
	(end_image)
)

