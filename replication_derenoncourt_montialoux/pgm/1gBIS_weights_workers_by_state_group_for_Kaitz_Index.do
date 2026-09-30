*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: weights used to compute average mw by year*state_group*gender for Kaitz Index used for employment regressions  
*first created: 07/10/2019
*last updated:  08/28/2020
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	

clear all
set more off
set matsize 10000
set maxvar 10000


****************************************************************************************************
**WEIGHTS: NUMBER OF WORKERS (by industry, gender, industry, state and state_group)
****************************************************************************************************
*employment counts by year*state_group*industry*gender	
use year state_group weight flag_employed in_sample industry sex using "data/output/cps_master_individual_level_nomw.dta", clear		
			collapse (sum) 			emp_by_industry=flag_employed ///
									[pw=weight] if flag_employed==1 & in_sample==1 ///
									, by(year state_group industry sex)	
			gen emp_by_industry_rounded = round(emp_by_industry)	
			drop emp_by_industry
			rename emp_by_industry_rounded emp_by_industry
	
	*calculate employment shares by industry		
			bysort year state_group sex : egen emp = total(emp_by_industry)
			sort year state_group  sex industry
			order year state_group sex industry  emp_by_industry emp
			
			gen emp_share = . 
			replace emp_share = emp_by_industry / emp
			
			gen ind_category =. 
				replace ind_category = 1 if inlist(industry,2,3,5,6,7,8,9,12,13,23)
				replace ind_category = 2 if inlist(industry,1,10,15,16,17,18,19)
				replace ind_category = 3 if inlist(industry,14)
				replace ind_category = 4 if inlist(industry,20,21,22)	
				replace ind_category = 5 if inlist(industry,4,11)	
			label var ind_category "Industry category :1.1938,2.1966,3.priv. hh,4.public,5.1961"
			
			*check that sum of emp_by_industry within state_group and sex = 1
			bysort year state_group sex : egen check_emp_share = total(emp_share)
			tab check_emp_share, missing 

	preserve
	
		*weight2: nb of workers by year*state_group*gender
			drop ind_category
			collapse (mean) weight2=emp ///
							, by(year state_group sex)	
			
			tempfile weights_ySg
			save `weights_ySg'
		
		
		*weight3: nb of workers by year*state_group
			collapse (sum) weight3=weight2 ///
							, by(year state_group)	
			
			tempfile weights_yS
			save `weights_yS'
						
	restore		

		merge m:1 year state_group sex using `weights_ySg'
		drop _m
		merge m:1 year state_group using `weights_yS'
		drop _m
	
saveold "data/output/weights_by_year_state_group_industry_gender_for_Kaitz_Index.dta", replace
