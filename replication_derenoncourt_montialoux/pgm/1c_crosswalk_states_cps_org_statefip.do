*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: Build the crosswalk_states_cps_org_statefip.dta
*first created: 05/08/2018
*last updated:  08/28/2020
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	

*import sheet crosswalk from industrial codes spreadsheet and save in stata format
import excel using "data/raw/spd_crosswalk_states_cps_cepr_org_statefips.xlsx",  firstrow case(lower) clear
saveold "data/output/crosswalk_states_cps_cepr_org_statefips.dta", replace
