*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: 		Build the crosswalk_states.dta
*first created: 03/14/2018
*last updated:  08/28/2020
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	


*import sheet crosswalk from industrial codes spreadsheet
import excel using "data/raw/spd_states_codes.xlsx", sheet("crosswalk") firstrow case(lower) clear

		label var state_var 	"CPS statefip variable used for the crosswalk"
		label var state_type 	"Ind. type used for the crosswalk: 1 ind 1962; 2 ind 1963-1967; 3 ind 1968-2016"
		label var state_group 	"Unified statefips code for 1962-2016"

		label define state_group_lbl  01  "California", add
		label define state_group_lbl  02  "Connecticut", add		
		label define state_group_lbl  03  "District of Columbia", add
		label define state_group_lbl  04  "Florida", add
		label define state_group_lbl  05  "Illinois", add
		label define state_group_lbl  06  "Indiana", add
		label define state_group_lbl  07  "New Jersey", add
		label define state_group_lbl  08  "New York", add				
		label define state_group_lbl  09  "Ohio", add		
		label define state_group_lbl  10  "Pennsylvania", add
		label define state_group_lbl  11  "Texas", add
		label define state_group_lbl  12  "Michigan-Wisconsin", add
		label define state_group_lbl  13  "Alabama-Mississippi", add
		label define state_group_lbl  14  "Maine-Massachussets-New Hampshire-Rhode Island-Vermont", add
		label define state_group_lbl  15  "North Carolina-South Carolina-Georgia", add
		label define state_group_lbl  16  "Kentucky-Tennessee", add
		label define state_group_lbl  17  "Arkansas-Louisiana-Oklahoma", add
		label define state_group_lbl  18  "Iowa-N Dakota-S Dakota-Nebraska-Kansas-Minnesota-Missouri", add
		label define state_group_lbl  19  "Washington-Oregon-Alaska-Hawaii", add
		label define state_group_lbl  20  "Montana-Wyoming-Colorado-New Mexico-Utah-Nevada-Arizona-Idaho", add
		label define state_group_lbl  21  "Delaware-Maryland-Virginia-West Virginia", add
		label define state_group_lbl  22  "State not identified (appears in CPS years 1962,1963, and 1971)", add
		
		label values state_group state_group_lbl		

saveold "data/output/crosswalk_states.dta", replace
