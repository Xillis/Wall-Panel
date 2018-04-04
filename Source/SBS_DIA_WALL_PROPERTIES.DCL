SBS_DIA_WALL_PROPERTIES : dialog {
	label = "Wall Information";
	key = "dlg_WProp";
	:row {
		:boxed_column {
			label = "Wall Shape";
			:image {
				label = "Wall Layout";
				key = "Wimage";
				width = 40;
				aspect_ratio = 0.6666;
			}
			:radio_row {
				label = "Start Point";
				key = "Startp";
				:radio_button {
					label = "Left";
					key = "l";
				}
				:radio_button {
					label = "Pick point";
					key = "p";
					alignment = centered;
				}
				:radio_button {
					label = "Right";
					key = "r";
					alignment = right;
				}
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
			label = "Panel Properties";
			:list_box {
				key = "panelinfo";
				is_enabled = false;
				fixed_width = true;
				width = 23;
			}
			:button {
				label = "Select Panel Type";
				key = "SPType";
			}
		}
	}
	:boxed_row {
			label = "Wall offsets";
		}
	ok_cancel; 
}