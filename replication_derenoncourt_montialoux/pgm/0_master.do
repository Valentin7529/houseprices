*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: 		master program
*first created: 02/04/2019
*last updated:  08/29/2020
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	

clear all
set more off
set matsize 10000
set maxvar 10000

*ACTION REQUIRED 	--> change path to your replication folder
global path 		"[.....]/replication"
cd $path


*ACTION REQUIRED 	--> install ado files
					*ssc install winsor2
					*ssc install hotdeck	
					*ssc install estout
					*ssc install maptile
					*ssc install spmap
					*ssc install shp2dta
					*ssc install reghdfe
					*ssc install ftools
					*ssc install egenmore
		
					*maptile_install using "http://files.michaelstepner.com/geo_statehex.zip"
					*maptile_install using "http://files.michaelstepner.com/geo_state.zip"

					
*ACTION REQUIRED 	--> install .style files (customized color palette)
					*i. copy the .style files (located in 'color_palette' folder) at the top of your SITE or PERSONAL directory 
					*-->to get the path for either your SITE or PERSONAL directory, type in "adopath" in Stata. 
					*ii.type "discard" OR restart Stata to make sure STATA loads the new colors. 
					
					*--> to set your personal directory to the folder "color_palette"
					*sysdir set PERSONAL "color_palette"

*ACTION REQUIRED   --> load geography templates
					*maptile_install using "http://files.michaelstepner.com/geo_state.zip"
					*--> make sure they have been downloaded in C:/ado/personal/maptile_geographies 
					*maptile_install using "http://files.michaelstepner.com/geo_statehex.zip"		
											
**FOR A LIST OF ALL TABLES AND FIGURES PRODUCED BY EACH DO FILE>>>SEE "replication>DM2019_list_tables&figures.xls"
											
*1. Build databases
	 *crosswalks	
		do "pgm/1a_crosswalk_industry.do"
		do "pgm/1b_crosswalk_states.do"
		do "pgm/1c_crosswalk_states_cps_org_statefip.do" 
	 *current population survey (1/2)
		do "pgm/1d_build_march_cps_nomw.do"
	 *minimum wage database
		do "pgm/1e_weights_pop_by_state.do"
		do "pgm/1f_mw_database_monthly_state.do"
		do "pgm/1g_weights_workers_by_state_group.do"	
		do "pgm/1gBIS_weights_workers_by_state_group_for_Kaitz_Index.do"		
		do "pgm/1h_mw_database_annual_state_group.do"
		do "pgm/1hBIS_mw_database_annual_state_group_for_Kaitz_Index.do"
	 *current population survey (2/2)
		do "pgm/1i_build_march_cps_withmw.do"
	 *census 
		do "pgm/1j_build_census.do"
	 *bls industry wage reports	
		do "pgm/1k_build_bls.do"
	
*2. Descriptive statistics
		do "pgm/2a_cps_census_descriptives.do"
		do "pgm/2b_bls_descriptives.do"
	
*3. Analysis using CPS data
		do "pgm/3a_cps_wage.do"
		do "pgm/3b_cps_employment.do"
		do "pgm/3c_cps_racial_gaps.do"
	
*4. Analysis using BLS data
		do "pgm/4a_bls_wage.do"
		do "pgm/4b_bls_employment.do"
		
