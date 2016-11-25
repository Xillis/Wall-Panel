SBS_WallProperties : dialog {
	label = "Wall Information";
	key = "dlg_WProp";
	:boxed_row {
		:column {
			:image {
				label = "Wall Layout";
				key = "Wimage";
				width = 60;
				aspect_ratio = 0.6666;
			}
			:row {
				:button {
				label = "Pick Points";
				key = "Ppoints";
				}
				:button {
				label = "Select Polyline";
				key = "Spoly";
				}
			}
		}
	}
	ok_cancel; 
}
	
	
	
	