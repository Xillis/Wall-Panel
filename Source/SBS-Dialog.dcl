SBS_WallProperties : dialog {
	label = "Wall Information";
	key = "dlg_WProp";
	:row {
		:boxed_column {
			:image {
				label = "Wall Layout";
				key = "Wimage";
				width = 40;
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
		:boxed_column {
			:text {
				label = "Panel type";
				key = "Ptype";
				value = "123";
			}
			
	}	}
	ok_cancel; 
}