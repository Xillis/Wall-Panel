SBS_WallProperties : dialog {
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
			:text {
				key = "Pprop";
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

SBS_Panel_info : dialog {
	label = "Panel Properties";
	key = "dlg_PProp";
	:boxed_row {
		:column {
			:popup_list{
				label = "Panel type";
				key = "Panel_type";
				edit_width = 13;
			}
			:popup_list {
				label = "Panel Width";
				key = "Width";
				edit_width = 6;
			}
			:popup_list {
				label = "Profile";
				key = "Profile";
				edit_width = 6;
			}
			:popup_list {
				label = "Gauge";
				key = "Gauge";
				edit_width = 4;
			}
			:edit_box {
				label = "Panel Part ID";
				key = "PPID";
				width = 6;
				is_enabled = false;
			}
			:spacer {height = 2;}
		}
		:spacer {width = 3;}
		:column {
			:list_box {
				label = "Colour";
				key = "Colour";
				fixed_width = true;
				width = 23;
			}
			:toggle {
				label = "Other";
				key = "allColour";
			}
		}
		:spacer {width = 3;}
		:boxed_column {
			label = "Panel Features";
			:list_box {
				key = "Feature_list";
				fixed_width = true;
				width = 23;
			}
			:button {
				label = "Change";
				key = "Fset";
			}
		}
	}
	ok_cancel; 
}