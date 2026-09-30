*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: weights used to compute average mw by year*state_group*industry*gender  
*first created: 03/14/2018
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
			collapse (sum) 			emp_by_gender=flag_employed ///
									[pw=weight] if flag_employed==1 & in_sample==1 ///
									, by(year state_group industry sex)	
			gen emp_by_gender_rounded = round(emp_by_gender)	
			drop emp_by_gender
			rename emp_by_gender_rounded emp_by_gender
			
	
	*calculate employment shares by gender		
			bysort year state_group industry : egen emp = total(emp_by_gender)
			sort year state_group industry sex
			order year state_group industry sex emp_by_gender emp
			
			gen emp_share = . 
			replace emp_share = round(emp_by_gender / emp,.01)
			
			gen ind_category =. 
				replace ind_category = 1 if inlist(industry,2,3,5,6,7,8,9,12,13,23)
				replace ind_category = 2 if inlist(industry,1,10,15,16,17,18,19)
				replace ind_category = 3 if inlist(industry,14)
				replace ind_category = 4 if inlist(industry,20,21,22)	
				replace ind_category = 5 if inlist(industry,4,11)	
			label var ind_category "Industry category :1.1938,2.1966,3.priv. hh,4.public,5.1961"
			
			*check that sum of emp_by_gender within state_group and industry = 1
			*bysort year state_group industry : egen check_emp_share = total(emp_share)
			*tab check_emp_share, missing 

	preserve
	
		*weight2: nb of workers by year*state_group*industry
			drop ind_category
			collapse (mean) weight2=emp ///
							, by(year state_group industry)	
			
			tempfile weights_yjS
			save `weights_yjS'
		
		
		*weight3: nb of workers by year*industry
			collapse (sum) weight3=weight2 ///
							, by(year industry)	
			
			tempfile weights_yj
			save `weights_yj'
			
		*weight4: nb of workers by year*[treatement/control status]
			gen ind_category =. 
				replace ind_category = 1 if inlist(industry,2,3,5,6,7,8,9,12,13,23)
				replace ind_category = 2 if inlist(industry,1,10,15,16,17,18,19)
				replace ind_category = 3 if inlist(industry,14)
				replace ind_category = 4 if inlist(industry,20,21,22)	
				replace ind_category = 5 if inlist(industry,4,11)	
			label var ind_category "Industry category :1.1938,2.1966,3.priv. hh,4.public,5.1961"
			
			collapse (sum) weight4=weight3 ///
							, by(year ind_category)	
			format weight4 %12.0gc
			
			tempfile weights_yt
			save `weights_yt'
						
	restore		

		merge m:1 year industry state_group using `weights_yjS'
		drop _m
		merge m:1 year industry using `weights_yj'
		drop _m
		merge m:1 year ind_category using `weights_yt'
		drop _m
		
saveold "data/output/weights_by_year_state_group_industry_gender.dta", replace
