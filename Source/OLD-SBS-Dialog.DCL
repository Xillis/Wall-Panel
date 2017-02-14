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

SBS_Stormseal : dialog {
	label = "Stormseal Features";
	key = "dlg_StormsealF";
	:radio_column {
		key = "Surf";
		:radio_button {
			label = "Acoustic Perforated Pattern 01";
			key = "A01";
		}
		:radio_button {
			label = "Acoustic Perforated Pattern 02";
			key = "A02";
		}
		:radio_button {
			label = "Barrier Coat";
			key = "BC";
		}
	}
	:radio_column {
		key = "Coat";
		:radio_button {
			label = "Uniform Washcoat";
			key = "UW";
		}
		:radio_button {
			label = "Paint Face Underside";
			key = "PFU";
		}
	}
	ok_button;		
}

SBS_StrucSeal : dialog {
	label = "StrucSeal Features";
	key = "dlg_StrucSealF";
	:radio_button {
		label = "Fluted";
		key = "Flut";
	}
	:radio_column {
		key = "Surf";
		:radio_button {
			label = "Acoustic Perforated Pattern 01";
			key = "A01";
		}
		:radio_button {
			label = "Acoustic Perforated Pattern 02";
			key = "A02";
		}
		:radio_button {
			label = "Barrier Coat";
			key = "BC";
		}
	}
	:radio_column {
		key = "Coat";
		:radio_button {
			label = "Uniform Washcoat";
			key = "UW";
		}
		:radio_button {
			label = "Paint Face Underside";
			key = "PFU";
		}
	}
	ok_button;		
}

SBS_DVseal : dialog {
	label = "Diamondseal & VersaSeal Features";
	key = "dlg_DVsealF";
	:radio_column {
		key = "Coat";
		:radio_button {
			label = "Uniform Washcoat";
			key = "UW";
		}
		:radio_button {
			label = "Paint Face Underside";
			key = "PFU";
		}
	}
	ok_button;		
}

SBS_OPENING : dialog {
	label = "Enter Opening Details";
	key = "dlg_Openings";
	:row {
		:column {
			:boxed_column {
			label = "Opening Type";
				:popup_list {
					key = "TYPE";
					fixed_width = true;
					edit_width = 30;
				}
			}
			:boxed_column {
			label = "Opening size";
				:row {
					:edit_box {
						label = "Width";
						key = "WIDTH";
					}
					:edit_box {
						label = "Height";
						key = "HEIGHT";
					}
				}
				:row {
					:button {
						label = "Pick";
						key = "PICKP";
					}	
				}
			}
			:row {
				:popup_list {
					label = "Gauge";
					key = "Gauge";
				}
				:toggle {
					label = "Match Panel";
					key = "MPG";
				}
			}
			:spacer_1 {}
			:row {
				:toggle {
					label = "Use Assembly";
					key = "ASSEMBLY";
				}
				:toggle {
					label = "Remove Panels";
					key = "REMOVE";
				}
			}
		}
		:boxed_column {
			label = "Trim Colour";
			:toggle {
				label = "Match Panel";
				key = "MPC";
			}
			:list_box {
				key = "Colour";
				fixed_width = true;
				width = 23;
			}
		}
	}
	ok_cancel;
}


