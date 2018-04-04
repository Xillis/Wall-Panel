SBS_PANEL : dialog {
	label = "Panel Selection";
	key = "dlg_PSelect";
	:boxed_row {
		:column {
			:popup_list{
				label = "Panel type";
				key = "Panel_type";
				edit_width = 13;
			}
		}
	}
	ok_cancel;
}