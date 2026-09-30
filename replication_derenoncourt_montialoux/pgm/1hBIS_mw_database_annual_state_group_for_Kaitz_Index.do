*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: 		create mw database: annual level, state_group*industry*gender for Kaitz Index
*first created: 07/10/2019
*last updated:  08/28/2020
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	

clear all
set more off
set matsize 10000
set maxvar 10000


*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*5. Create and save annual database at the state_group*industry*gender, 1961-2015 for selected industries with positive number of workers in the CPS
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
	use "data/output/mw_series_by_state_gender_industry_monthly_inter.dta", clear
	merge m:1 year state_group industry sex using "data/output/weights_by_year_state_group_industry_gender_for_Kaitz_Index.dta"
	keep if inrange(year,1961,2015)
	drop if inlist(industry,0,23)
	drop if state_group==22
	rename mw_nominal mw_nominal_ymjsg
		*30,492  obs (5.66% of sample) for which there are no workers in the corresponding industry in the CPS (ex: no workers in fisheries in CT in 1961). OK to drop. 
		*52 obs for which state is not identified. OK to drop. 
			drop if _m!=3
			drop _merge
			gen pop_by_state_gender = round(pop_by_state * emp_share)
				
			preserve 	
				
				collapse (sum) pop_by_state_group_gender=pop_by_state_gender ///
								, by(year month industry state_group sex)	
				
				
				tempfile weights_ymjSg
				save `weights_ymjSg'
				
			restore
			
				merge m:1 year month industry state_group sex using `weights_ymjSg'
				drop _m
				gen w1 = pop_by_state_gender/pop_by_state_group_gender 
				*rename mw_nominal mw_nominal_ymjsg
			
			preserve
			
				*step1: calculate mw at the year*month*state_group*industry*gender level 
					collapse (sum) mw_nominal_ymjSg = mw_nominal_ymjsg  /// 
							 (mean) w2=emp_share weight2 weight3 ind_category [pw=w1] ///
									, by(year month industry state_group sex)
					
					tempfile mw_nominal_ymjSg 
					save `mw_nominal_ymjSg'
			
				*step2: calculate mw at the year*month*state_group*sex level 
					*rename emp_share w2
					collapse (sum) mw_nominal_ymSg = mw_nominal_ymjSg  /// 
							 (mean) weight2 weight3  [pw=w2] ///
									, by(year month sex state_group)
					tempfile mw_nominal_ymSg 
					save `mw_nominal_ymSg'
			
				*step3: calculate mw at the year*month*state_group level 
					gen w3 = weight2/weight3	
					collapse (sum) mw_nominal_ymS = mw_nominal_ymSg  ///
							 (mean) weight3 [pw=w3] ///
									, by(year month state_group)
					tempfile mw_nominal_ymS 
					save `mw_nominal_ymS'
			restore	
				merge m:1 year month industry state_group sex using `mw_nominal_ymjSg'
					drop _m
				merge m:1 year month state_group sex 	  using `mw_nominal_ymSg'
					drop _m
				merge m:1 year month state_group		using `mw_nominal_ymS'
					drop _m
			
				label var mw_nominal_ymjsg	"mw at the year month industry state gender level (nominal)"
				label var mw_nominal_ymjSg 	"mw at the year month industry state-group gender level (nominal)"
				label var mw_nominal_ymSg 	"mw at the year month state-group sex level (nominal)"
				label var mw_nominal_ymS 	"mw at the year month state-group level (nominal)"
				
				order year month statefip state_abb state_name division region region_abb state_group south sex industry ind_category flsa_coverage mw*
				
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*5. Save monthly and annual mw and population databases
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%			

**state_group*gender	
	*use "data\output\mw_series_by_state_gender_industry_monthly.dta", clear
	*annual
	collapse (firstnm) state_name state_abb region_abb (mean)  mw_nominal_ySg=mw_nominal_ymSg mw_nominal_yS=mw_nominal_ymS ///
												    , by(year state_group industry sex) 
	drop state_abb state_name 

	saveold "data/output/mw_series_by_state_group_gender_industry_annually_for_Kaitz_Index.dta", replace	
	
	
	
	
	
