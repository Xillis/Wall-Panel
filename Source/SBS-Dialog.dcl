SBS_PanelProperties : dialog {
	label = "Wall Panel Information";
	key = "dlg_PProp";
	:boxed_row {
		label = "Panel Properties";
		:column {
		height = 5;
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
	:boxed_row {
		label = "Installation info";
		:column {
			:row {
				width = 8;
				:popup_list {
					label = "bundle";
					key = "BPV";
					fixed_width = true;
					width =6;
					edit_width = 6;
				}
				:edit_box {
					label = "-";
					key = "BVS";
					fixed_width = true;
					width = 3;
					edit_width = 3;
				}
			}
		}
		
		:edit_box {
			label = "Offset at panel base";
			key = "Base";
			width = 1;
		}
		:spacer {width = 20;}
	}
	ok_cancel; 
}

SBS_WallProperties : dialog {
	label = "Wall Information";
	key = "dlg_WProp";
	:image {
		label = "Wall Layout";
		key = "Wimage";
		width = 60;
		aspect_ratio = 0.6666;
		}
	ok_cancel; 
}
	
	
	
	