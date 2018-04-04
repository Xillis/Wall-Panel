SBS_RTL_PANEL : dialog {
	label = "RTL Panel Properties";
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
				label = "Notch";
				key = "Notch";
				edit_width = 6;
			}
			:popup_list {
				label = "Holes";
				key = "Holes";
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
			:row {
				:toggle {
					label = "All";
					key = "allColour";
				}
			}
			:row {
				:toggle {
					label = "Other";
					key = "Othertog";
				}
				:edit_box {
					key = "OtherColour";
				}
			}
		}
		:spacer {width = 3;}
			:list_box {
				label = "Panel Features";
				key = "Feature_list";
				fixed_width = true;
				multiple_select = true;
				width = 25;
			}
	}
	:row {
		:text {
			key = "Alerttext";
			height = 4;
		}
	}
	ok_cancel; 
}