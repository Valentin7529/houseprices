*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: 		create mw database: annual level, state_group*industry*gender
*first created: 01/01/2018
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
	merge m:1 year state_group industry sex using "data/output/weights_by_year_state_group_industry_gender.dta"
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
							 (mean) w2=emp_share weight2 weight3 weight4 ind_category [pw=w1] ///
									, by(year month industry state_group sex)
					
					tempfile mw_nominal_ymjSg 
					save `mw_nominal_ymjSg'
			
				*step2: calculate mw at the year*month*state_group*industry level 
					*rename emp_share w2
					collapse (sum) mw_nominal_ymjS = mw_nominal_ymjSg  /// 
							 (mean) weight2 weight3 weight4 ind_category [pw=w2] ///
									, by(year month industry state_group)
					tempfile mw_nominal_ymjS 
					save `mw_nominal_ymjS'
			
				*step3: calculate mw at the year*month*industry level 
					gen w3 = weight2/weight3	
					collapse (sum) mw_nominal_ymj = mw_nominal_ymjS  ///
							 (mean) weight3 weight4 ind_category [pw=w3] ///
									, by(year month industry)
					tempfile mw_nominal_ymj 
					save `mw_nominal_ymj'
			
				*step4: calculate mw at the year*month*treatment status level 
					gen w4 = weight3/weight4	
					collapse (sum) mw_nominal_ymt = mw_nominal_ymj [pw=w4] /// 
									, by(year month ind_category)
					tempfile mw_nominal_ymt 
					save `mw_nominal_ymt'
			restore	
				merge m:1 year month industry state_group sex using `mw_nominal_ymjSg'
					drop _m
				merge m:1 year month industry state_group 	  using `mw_nominal_ymjS'
					drop _m
				merge m:1 year month industry				  using `mw_nominal_ymj'
					drop _m
				merge m:1 year month ind_category			  using `mw_nominal_ymt'
					drop _m
			
				label var mw_nominal_ymjsg	"mw at the year month industry state gender level (nominal)"
				label var mw_nominal_ymjSg 	"mw at the year month industry state-group gender level (nominal)"
				label var mw_nominal_ymjS 	"mw at the year month industry state-group level (nominal)"
				label var mw_nominal_ymj 	"mw at the year month industry level (nominal)"
				label var mw_nominal_ymt 	"mw at the year month industry category level (nominal)"
				
				order year month statefip state_abb state_name division region region_abb state_group south sex industry ind_category flsa_coverage mw*
				
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*5. Save monthly and annual mw and population databases
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%			
**state*industry*gender	
	*monthly	
	saveold "data/output/mw_series_by_state_gender_industry_monthly.dta", replace
	
	*annual
	collapse (firstnm) state_name state_abb region_abb (mean) region  division south ind_category mw_federal mw_by_state ///
													mw_nominal_yjSg=mw_nominal_ymjSg  mw_nominal_yjS=mw_nominal_ymjS mw_nominal_yj=mw_nominal_ymj mw_nominal_yt=mw_nominal_ymt ///
												    mw_1961_amendments mw_1966_amendments_farms mw_1966_amendments_except_farms pop_by_state pop_by_region, by(year industry statefip sex) 
	saveold "data/output/mw_series_by_state_gender_industry_annually.dta", replace
	
**state_group*industry*gender	
	use "data/output/mw_series_by_state_gender_industry_monthly.dta", clear
	*monthly	
	collapse (firstnm) state_name state_abb region_abb (mean) region  division south ind_category mw_federal mw_by_state mw_nominal_ymjSg  mw_nominal_ymjS mw_nominal_ymj mw_nominal_ymt ///
												    mw_1961_amendments mw_1966_amendments_farms mw_1966_amendments_except_farms pop_by_state pop_by_region, by(year month industry state_group sex) 
	saveold "data/output/mw_series_by_state_group_gender_industry_monthly.dta", replace
	
	*annual
	collapse (firstnm) state_name state_abb region_abb (mean) region  division south ind_category mw_federal mw_by_state ///
													mw_nominal_yjSg=mw_nominal_ymjSg  mw_nominal_yjS=mw_nominal_ymjS mw_nominal_yj=mw_nominal_ymj mw_nominal_yt=mw_nominal_ymt ///
												    mw_1961_amendments mw_1966_amendments_farms mw_1966_amendments_except_farms pop_by_state pop_by_region, by(year industry state_group sex) 
	
	saveold "data/output/mw_series_by_state_group_gender_industry_annually.dta", replace	
	
	
	
	
	
