*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*program: 		Estimate effects of 1966 amendements on wages, racial gaps and employment 
*first created: 01/19/2018
*last updated:  08/31/2020
*structure:    	Create interaction variables and list of covariates
*				Employment regressions		
*update:		changed covar_emp to make marst a categorical variable
*				changed time_emp
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	

*******************************************************************************************************************************************************************************************************************************
*COMPUTE STATE-LEVEL UNEMPLOYMENT RATES 	
*******************************************************************************************************************************************************************************************************************************

*1. Compute state-level unemployment rates and merge to cps individual data
	use "data/output/cps_master_individual_level.dta", clear	
	preserve
		collapse (mean)   year unemp=flag_unemployed  /// 
					if (flag_employed | flag_unemployed)  & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) [pw=weight], by(year_cps state_group)
	
	*drop if state_group==22
	*replace 1963 with mean of 1962 and 1964
	order year year_cps
	sort state_group year 
	by state_group : gen unemp_mean = (unemp[_n-1]+unemp[_n+1])/2
	replace unemp = unemp_mean if year==1962 
	drop unemp_mean
	
	*by state_group :  gen obs = _n
	*tab obs year

	cap drop ur_1962 ur_1966
	gen ur_1962 = .
	gen ur_1963 = .	
	gen ur_1964 = .	
	gen ur_1965 = .
	gen ur_1966 = . 
	
	
	forvalues y = 1962(1)1966 {
		by state_group : replace ur_`y' = unemp if year_cps==`y'
		
		qui bysort state_group (ur_`y'): replace ur_`y'=ur_`y'[1]
		sort state_group year	
	}
	
	
	gen ur_change_62_66 = (ur_1962/ur_1966)-1
	
	collapse (mean) ur_1962 ur_1963 ur_1964 ur_1965 ur_1966, by(state_group)
	gen ur_avg_62_66 = (ur_1962 + ur_1963 + ur_1964 + ur_1965 + ur_1966)/5
	
	
	tempfile ur_by_state
	save `ur_by_state'
		
	restore
	
	merge m:1 state_group using  `ur_by_state'
	drop _merge	
	
	merge m:1 state_group using "data/output/ssa_unemployment_1962_1966.dta"
	drop _merge
	
	gen ur_ssa_avg_62_66 = (ur_ssa_1962 + ur_ssa_1963 + ur_ssa_1964 +ur_ssa_1965 + ur_ssa_1966)/5

	*create dummies for above/below median of unemployment rate
	
	qui su ur_ssa_1966, det
	local median_ur_ssa_1966 = r(p50)
	gen bm_ssa_1966 = (ur_ssa_1966 < `median_ur_ssa_1966')
	
	
	*ceate new outcome variable (for L-L substitution calculations)
	gen flag_black = . 
	replace flag_black = (inlist(race,200))

	gen flag_white = . 
	replace flag_white = (inlist(race,100))
	
*******************************************************************************************************************************************************************************************************************************
****CREATE INTERACTION VARIABLES AND LIST OF COVARIATES
*******************************************************************************************************************************************************************************************************************************
*INDUSTRY DESIGN
	**long
	forval y=1961/1980 {
		gen inter_industry_long_`y' = (covered_1966 & year==`y')
	}
	
		*--> since CPS year 1963 (= year 1962 for earnings) is removed from sample: 
		replace inter_industry_long_1962 = 0
		*--> since CPS year 1966 (= year 1965 for earnings) is chosen as base year: 
		replace inter_industry_long_1965 = 0		
	
	**short
	gen inter_industry_short_1 = 0
	forval t=2/3 {
		gen inter_industry_short_`t' = (covered_1966 & time==`t')
	}
		
			
*STATE DESIGN
	**long
	forval y=1961/1980 {
		gen inter_state_long_`y' = (nomw_1966 & year==`y')
	}
	
		*--> since CPS year 1963 (= year 1962 for earnings) is removed from sample: 
		replace inter_state_long_1962 =  0
		*--> since CPS year 1966 (= year 1965 for earnings) is chosen as base year: 
		replace inter_state_long_1965 = 0		
	
	**short
	gen inter_state_short_1 = 0
	forval t=2/3 {
		gen inter_state_short_`t' = (nomw_1966 & time==`t')
	}
			
						
	global inter_industry_long 	"inter_industry_long_1961 inter_industry_long_1962 inter_industry_long_1963 inter_industry_long_1964 inter_industry_long_1965 inter_industry_long_1966 inter_industry_long_1967 inter_industry_long_1968 inter_industry_long_1969 inter_industry_long_1970 inter_industry_long_1971 inter_industry_long_1972 inter_industry_long_1973 inter_industry_long_1974 inter_industry_long_1975 inter_industry_long_1976 inter_industry_long_1977 inter_industry_long_1978 inter_industry_long_1979 inter_industry_long_1980"
	global inter_state_long 	"inter_state_long_1961 inter_state_long_1962 inter_state_long_1963 inter_state_long_1964 inter_state_long_1965 inter_state_long_1966 inter_state_long_1967 inter_state_long_1968 inter_state_long_1969 inter_state_long_1970 inter_state_long_1971 inter_state_long_1972 inter_state_long_1973 inter_state_long_1974 inter_state_long_1975 inter_state_long_1976 inter_state_long_1977 inter_state_long_1978 inter_state_long_1979 inter_state_long_1980"

	global inter_industry_short "inter_industry_short_1 inter_industry_short_2 inter_industry_short_3" 
	global inter_state_short 	"inter_state_short_1 inter_state_short_2 inter_state_short_3 " 

	*set of covariates used in the baseline model
	global covar 				"ib12.schooling exp exp_square exp_cubic ib1.fullpart ib6.wkswork2 ib40.ahrsworkt ib1.marst ib1.occupation" 
	global covar_nohnow 		"ib12.schooling exp exp_square exp_cubic ib1.marst ib1.occupation" 
	global covar_emp 			"age age_square schooling ib1.marst"	

		
	
	*tab flag_black year	
	
*******************************************************************************************************************************************************************************************************************************
****EMPLOYMENT REGRESSIONS
*******************************************************************************************************************************************************************************************************************************	   
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*TABLE 6 [PAPER]: MAIN EFFECTS OF 1967 REFORM ON PROBABILITY OF EMPLOYMENT (EXTENSIVE MARGIN) AND ROBUSTNESS CHECKS
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ***DESIGN #1: STRONGLY vs. WEAKLY TREATED
	**MAIN ESTIMATES
		**EMPLOYMENT	
			*all
			reg 	flag_employed_unemp nomw_1966##ib1.time_emp i.sex i.race $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_all_d1
			*black
			reg 	flag_employed_unemp nomw_1966##ib1.time_emp i.sex  $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,200)  & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_black_d1	
			*white
			reg 	flag_employed_unemp nomw_1966##ib1.time_emp i.sex  $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100) & race==100 & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_white_d1			
		**EARNINGS
			reg 	ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar  i.state_group [pw=weight]  ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  aw_all_d1
			*black
			reg		ln_annual_wage nomw_1966##ib1.time  i.sex  $covar  i.state_group [pw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  aw_black_d1
			*white
			reg		ln_annual_wage nomw_1966##ib1.time  i.sex  $covar  i.state_group [pw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  aw_white_d1					
			
	**EMPLOYMENT ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*all
			reg 	flag_employed_unemp nomw_1966##ib1.time_emp i.sex i.race $covar_emp  i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  emp_all_elast_d1
			*black
			reg 	flag_employed_unemp nomw_1966##ib1.time_emp i.sex  $covar_emp  i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,200)  & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  emp_black_elast_d1	
			*white
			reg 	flag_employed_unemp nomw_1966##ib1.time_emp i.sex  $covar_emp  i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,100) & race==100 & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  emp_white_elast_d1			
	
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			*all
			reg 	ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar  i.state_group [iw=weight]  ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  aw_all_elast_d1
			*black
			reg		ln_annual_wage nomw_1966##ib1.time  i.sex  $covar  i.state_group [iw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  aw_black_elast_d1
			*white
			reg		ln_annual_wage nomw_1966##ib1.time  i.sex  $covar  i.state_group [iw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  aw_white_elast_d1					
					
		**iii.EMPLOYMENT ELASTICITIES
			**mean employment rate in 1967-1972
			*all
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100,200) 
			eststo  epop_all
			*black
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,200) 
			eststo  epop_black
			*white
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100) 
			eststo  epop_white

			*all
			suest   epop_all emp_all_elast_d1 aw_all_elast_d1, vce(cluster state_group)
			nlcom  (emp_elast_all:(1/_b[epop_all_mean:_cons])*(_b[emp_all_elast_d1_mean:1.nomw_1966#2.time_emp]/_b[aw_all_elast_d1_mean:1.nomw_1966#2.time])), post
			mat 	elast_all_d1 = _b[emp_elast_all]\ _se[emp_elast_all]\_b[emp_elast_all]-1.96*_se[emp_elast_all]\_b[emp_elast_all]+1.96*_se[emp_elast_all]
			*ADD-ON:FOR TABLE 10 [SLIDES ONLY]
			mat elast_all_d1_slides = _b[emp_elast_all]\ _se[emp_elast_all]
		
			
			*black
			suest   epop_black emp_black_elast_d1 aw_black_elast_d1, vce(cluster state_group)
			nlcom 	(emp_elast_black:(1/_b[epop_black_mean:_cons])*(_b[emp_black_elast_d1_mean:1.nomw_1966#2.time_emp]/_b[aw_black_elast_d1_mean:1.nomw_1966#2.time])), post
			mat 	elast_black_d1 = _b[emp_elast_black]\ _se[emp_elast_black]\ _b[emp_elast_black]-1.96*_se[emp_elast_black]\_b[emp_elast_black]+1.96*_se[emp_elast_black]
			*ADD-ON:FOR TABLE 10 [SLIDES ONLY]
			mat elast_black_d1_slides = _b[emp_elast_black]\ _se[emp_elast_black]
			
			*white
			suest   epop_white emp_white_elast_d1 aw_white_elast_d1, vce(cluster state_group)
			nlcom 	(emp_elast_white:(1/_b[epop_white_mean:_cons])*(_b[emp_white_elast_d1_mean:1.nomw_1966#2.time_emp]/_b[aw_white_elast_d1_mean:1.nomw_1966#2.time])), post
			mat 	elast_white_d1 = _b[emp_elast_white]\ _se[emp_elast_white]\_b[emp_elast_white]-1.96*_se[emp_elast_white]\_b[emp_elast_white]+1.96*_se[emp_elast_white]
			*ADD-ON:FOR TABLE 10 [SLIDES ONLY]
			mat elast_white_d1_slides = _b[emp_elast_white]\ _se[emp_elast_white]
		

  ***DESIGN #2: KAITZ INDEX 1966
	*%%---ALL------------------------------------------------	
		**CALCULATE STANDARDIZED TREATMENT VARIABLE 
			egen KI_1966S_aw_std = std(KI_1966S)  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			egen KI_1966S_emp_std = std(KI_1966S) if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			
			su 	 KI_1966S if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			su 	 KI_1966S if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, det
			tabstat KI_1966S, statistics( iqr )
		**MAIN ESTIMATES
		**EMPLOYMENT	
			reg 	flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_all_d2			
		**EARNINGS
			reg 	ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar  i.state_group [pw=weight]  ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  aw_all_d2
		**EMPLOYMENT ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			reg 	flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp  i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  emp_all_elast_d2
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg 	ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar  i.state_group [iw=weight]  ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  aw_all_elast_d2					
		**iii.EMPLOYMENT ELASTICITIES
			**mean employment rate in 1967-1972
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100,200) 
			eststo  epop_all
			
			suest   epop_all emp_all_elast_d2 aw_all_elast_d2, vce(cluster state_group)
			nlcom  (emp_elast_all:(1/_b[epop_all_mean:_cons])*(_b[emp_all_elast_d2_mean:c.KI_1966S_emp_std#2.time_emp]/_b[aw_all_elast_d2_mean:c.KI_1966S_aw_std#2.time])), post
			mat 	elast_all_d2 = _b[emp_elast_all]\ _se[emp_elast_all]\_b[emp_elast_all]-1.96*_se[emp_elast_all]\_b[emp_elast_all]+1.96*_se[emp_elast_all]
		
	*%%---BLACK------------------------------------------------	
		**CALCULATE STANDARDIZED TREATMENT VARIABLE 
			drop KI_1966S_aw_std KI_1966S_emp_std
			egen KI_1966S_aw_std = std(KI_1966S) if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			egen KI_1966S_emp_std = std(KI_1966S) if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		
		**MAIN ESTIMATES
		**EMPLOYMENT	
			reg 	flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex  $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,200)  & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_black_d2		
		**EARNINGS
			reg		ln_annual_wage c.KI_1966S_aw_std##ib1.time  i.sex  $covar  i.state_group [pw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  aw_black_d2
		**EMPLOYMENT ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			reg 	flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex  $covar_emp  i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,200)  & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  emp_black_elast_d2	
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg		ln_annual_wage c.KI_1966S_aw_std##ib1.time  i.sex  $covar  i.state_group [iw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  aw_black_elast_d2				
		**iii.EMPLOYMENT ELASTICITIES
			**mean employment rate in 1967-1972
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,200) 
			eststo  epop_black
			
			suest   epop_black emp_black_elast_d2 aw_black_elast_d2, vce(cluster state_group)
			nlcom 	(emp_elast_black:(1/_b[epop_black_mean:_cons])*(_b[emp_black_elast_d2_mean:c.KI_1966S_emp_std#2.time_emp]/_b[aw_black_elast_d2_mean:c.KI_1966S_aw_std#2.time])), post
			mat 	elast_black_d2 = _b[emp_elast_black]\ _se[emp_elast_black]\ _b[emp_elast_black]-1.96*_se[emp_elast_black]\_b[emp_elast_black]+1.96*_se[emp_elast_black]
			
	*%%---WHITE------------------------------------------------	
		**CALCULATE STANDARDIZED TREATMENT VARIABLE 
			drop KI_1966S_aw_std KI_1966S_emp_std			
			egen KI_1966S_aw_std = std(KI_1966S) if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			egen KI_1966S_emp_std = std(KI_1966S) if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			
		**MAIN ESTIMATES
		**EMPLOYMENT	
			reg 	flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex  $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100) & race==100 & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_white_d2	
		**EARNINGS
			reg		ln_annual_wage c.KI_1966S_aw_std##ib1.time  i.sex  $covar  i.state_group [pw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  aw_white_d2	
		**EMPLOYMENT ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			reg 	flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex  $covar_emp  i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,100) & race==100 & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  emp_white_elast_d2		
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg		ln_annual_wage c.KI_1966S_aw_std##ib1.time  i.sex  $covar  i.state_group [iw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  aw_white_elast_d2				
		**iii.EMPLOYMENT ELASTICITIES
			**mean employment rate in 1967-1972
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100) 
			eststo  epop_white
			
			suest   epop_white emp_white_elast_d2 aw_white_elast_d2, vce(cluster state_group)
			nlcom 	(emp_elast_white:(1/_b[epop_white_mean:_cons])*(_b[emp_white_elast_d2_mean:c.KI_1966S_emp_std#2.time_emp]/_b[aw_white_elast_d2_mean:c.KI_1966S_aw_std#2.time])), post
			mat 	elast_white_d2 = _b[emp_elast_white]\ _se[emp_elast_white]\_b[emp_elast_white]-1.96*_se[emp_elast_white]\_b[emp_elast_white]+1.96*_se[emp_elast_white]
		
		
  ***DESIGN #3: FRACTION OF AFFECTED WORKERS BAILEY ET AL. 2018
	*%%---ALL------------------------------------------------	
		**CALCULATE STANDARDIZED TREATMENT VARIABLE 
			egen F_s1966_aw_std = std(F_s1966)  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			egen F_s1966_emp_std = std(F_s1966) if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			
			su 	 F_s1966 if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			su 	 F_s1966 if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22

		**MAIN ESTIMATES
		**EMPLOYMENT	
			reg 	flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_all_d3			
		**EARNINGS
			reg 	ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar  i.state_group [pw=weight]  ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  aw_all_d3
		**EMPLOYMENT ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			reg 	flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp  i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  emp_all_elast_d3
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg 	ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar  i.state_group [iw=weight]  ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  aw_all_elast_d3					
		**iii.EMPLOYMENT ELASTICITIES
			**mean employment rate in 1967-1972
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100,200) 
			eststo  epop_all
			
			suest   epop_all emp_all_elast_d3 aw_all_elast_d3, vce(cluster state_group)
			nlcom  (emp_elast_all:(1/_b[epop_all_mean:_cons])*(_b[emp_all_elast_d3_mean:c.F_s1966_emp_std#2.time_emp]/_b[aw_all_elast_d3_mean:c.F_s1966_aw_std#2.time])), post
			mat 	elast_all_d3 = _b[emp_elast_all]\ _se[emp_elast_all]\_b[emp_elast_all]-1.96*_se[emp_elast_all]\_b[emp_elast_all]+1.96*_se[emp_elast_all]
		
	*%%---BLACK------------------------------------------------	
		**CALCULATE STANDARDIZED TREATMENT VARIABLE 
			drop F_s1966_aw_std F_s1966_emp_std
			egen F_s1966_aw_std = std(F_s1966) if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			egen F_s1966_emp_std = std(F_s1966) if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		
		**MAIN ESTIMATES
		**EMPLOYMENT	
			reg 	flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex  $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,200)  & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_black_d3		
		**EARNINGS
			reg		ln_annual_wage c.F_s1966_aw_std##ib1.time  i.sex  $covar  i.state_group [pw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  aw_black_d3
		**EMPLOYMENT ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			reg 	flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex  $covar_emp  i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,200)  & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  emp_black_elast_d3	
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg		ln_annual_wage c.F_s1966_aw_std##ib1.time  i.sex  $covar  i.state_group [iw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  aw_black_elast_d3				
		**iii.EMPLOYMENT ELASTICITIES
			**mean employment rate in 1967-1972
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,200) 
			eststo  epop_black
			
			suest   epop_black emp_black_elast_d3 aw_black_elast_d3, vce(cluster state_group)
			nlcom 	(emp_elast_black:(1/_b[epop_black_mean:_cons])*(_b[emp_black_elast_d3_mean:c.F_s1966_emp_std#2.time_emp]/_b[aw_black_elast_d3_mean:c.F_s1966_aw_std#2.time])), post
			mat 	elast_black_d3 = _b[emp_elast_black]\ _se[emp_elast_black]\ _b[emp_elast_black]-1.96*_se[emp_elast_black]\_b[emp_elast_black]+1.96*_se[emp_elast_black]
			
	*%%---WHITE------------------------------------------------	
		**CALCULATE STANDARDIZED TREATMENT VARIABLE 
			drop F_s1966_aw_std F_s1966_emp_std			
			egen F_s1966_aw_std = std(F_s1966) if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			egen F_s1966_emp_std = std(F_s1966) if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			
		**MAIN ESTIMATES
		**EMPLOYMENT	
			reg 	flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex  $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100) & race==100 & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_white_d3	
		**EARNINGS
			reg		ln_annual_wage c.F_s1966_aw_std##ib1.time  i.sex  $covar  i.state_group [pw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  aw_white_d3	
		**EMPLOYMENT ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			reg 	flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex  $covar_emp  i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,100) & race==100 & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  emp_white_elast_d3		
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg		ln_annual_wage c.F_s1966_aw_std##ib1.time  i.sex  $covar  i.state_group [iw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  aw_white_elast_d3				
		**iii.EMPLOYMENT ELASTICITIES
			**mean employment rate in 1967-1972
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100) 
			eststo  epop_white
			
			suest   epop_white emp_white_elast_d3 aw_white_elast_d3, vce(cluster state_group)
			nlcom 	(emp_elast_white:(1/_b[epop_white_mean:_cons])*(_b[emp_white_elast_d3_mean:c.F_s1966_emp_std#2.time_emp]/_b[aw_white_elast_d3_mean:c.F_s1966_aw_std#2.time])), post
			mat 	elast_white_d3 = _b[emp_elast_white]\ _se[emp_elast_white]\_b[emp_elast_white]-1.96*_se[emp_elast_white]\_b[emp_elast_white]+1.96*_se[emp_elast_white]
		
	*%%-------------------------------------------------------	
		*iv. COMBINING RESULTS ACROSS CROSS-STATE DESIGNS	
			mat elast_d1 = (elast_all_d1, elast_black_d1, elast_white_d1)
			mat rownames elast_d1 = "\rule{0pt}{3ex}\textbf{Emp. elasticity}" "se" "lower bound" "upper bound"
			mat elast_d2 = (elast_all_d2, elast_black_d2, elast_white_d2)
			mat elast_d3 = (elast_all_d3, elast_black_d3, elast_white_d3)
			mat elast 	= (elast_d1,elast_d2,elast_d3)
						
		*v. OUTPUT TABLE
			esttab 	emp_all_d1 emp_black_d1 emp_white_d1 emp_all_d2 emp_black_d2 emp_white_d2 emp_all_d3 emp_black_d3 emp_white_d3 ///
					using "tables/table_emp_cps.tex", replace  label fragment ///
					nolines  posthead(\cmidrule{2-10}) booktabs ///
					nonumbers mtitle("All" "Black" "White" "All" "Black" "White" "All" "Black" "White") collabels(none)    ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					keep(1.nomw_1966#2.time_emp 2.time_emp#c.KI_1966S_emp_std 2.time_emp#c.F_s1966_emp_std)   ///				
					refcat(1.nomw_1966#2.time_emp "Treatment var. $\times$& & & \\ \hspace{0.5cm}{1967-1972}", nolabel) ///
					coeflabel(1.nomw_1966#2.time_emp "\rule{0pt}{3ex}\textbf{Employment}") ///
					stats(N, fmt(%11.0gc) label("\hspace{0.2cm}{}")) onecell 							
			esttab 	aw_all_d1 aw_black_d1 aw_white_d1 aw_all_d2 aw_black_d2 aw_white_d2 aw_all_d3 aw_black_d3 aw_white_d3 ///
					using "tables/table_emp_cps.tex", append  label fragment ///
					nolines nonumbers nomtitles collabels(none)  ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					keep(1.nomw_1966#2.time 2.time#c.KI_1966S_aw_std 2.time#c.F_s1966_aw_std)   ///				
					coeflabel(1.nomw_1966#2.time "\rule{0pt}{3ex}\textbf{Earnings}") ///
					stats(N, fmt(%11.0gc) label("\hspace{0.2cm}{}")) onecell 											
			esttab	matrix(elast,fmt(%3.2f)) ///
					using "tables/table_emp_cps.tex", append  label fragment ///
					nolines booktabs ///
					nonumbers nomtitles collabels(none) postfoot(\bottomrule) 

					
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*TABLE 6 [SLIDES ONLY]: EFFECT OF 1967 REFORM ON PROBABILITY OF BEING EMPLOYED VS. UNEMPLOYED  (SHORT VERSION OF THE ABOVE)
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
			*i. Adapt elasticity results to slides 
			mat elast_slides = (elast_all_d1_slides, elast_black_d1_slides, elast_white_d1_slides)
			mat rownames elast_slides = "\rule{0pt}{3ex}\textbf{Elast.}" "se"
		   			
			*ii. Output table 
			esttab 	emp_all_d1 emp_black_d1 emp_white_d1  ///
					using "tables/table_emp_cps_slides.tex", replace  label fragment ///
					nolines  posthead(\cmidrule{2-4}) booktabs ///
					nonumbers mtitle("All" "Black" "White" ) collabels(none)    ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					keep(1.nomw_1966#2.time_emp)   ///				
					refcat(1.nomw_1966#2.time_emp "Strongly treated states $\times$& & & \\ \hspace{0.2cm}{1967-1972}", nolabel) ///
					coeflabel(1.nomw_1966#2.time_emp "\textbf{Employment}") ///
					stats(N, fmt(%11.0gc) label("{}")) onecell 							
			esttab 	aw_all_d1 aw_black_d1 aw_white_d1 ///
					using "tables/table_emp_cps_slides.tex", append  label fragment ///
					nolines nonumbers nomtitles collabels(none)  ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					keep(1.nomw_1966#2.time)   ///				
					coeflabel(1.nomw_1966#2.time "\rule{0pt}{3ex}\textbf{Earnings}") ///
					stats(N, fmt(%11.0gc) label("{}")) onecell 											
			esttab	matrix(elast_slides,fmt(%3.2f)) ///
					using "tables/table_emp_cps_slides.tex", append  label fragment ///
					nolines booktabs ///
					nonumbers nomtitles collabels(none) postfoot(\bottomrule) 					

*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*TABLE E9 [APPENDIX]: MAIN EFFECTS OF 1967 REFORM ON RELATIVE BLACK VS. WHITE SHARES OF EMPLOYMENT AT THE STATE-LEVEL
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	***DESIGN #1: STRONGLY vs. WEAKLY TREATED
	**MAIN ESTIMATES
		**RELATIVE EMPLOYMENT	
			*outcome: relative shares of black and white persons among employed workers
			reg 	flag_white nomw_1966##ib1.time_emp i.sex $covar_emp  i.state_group [pw=weight]  ///
					if flag_employed & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_emp_all_d1
			
			reg 	flag_white nomw_1966##ib1.time_emp  $covar_emp  i.state_group [pw=weight]  ///
					if flag_employed & sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_emp_men_d1
			
			reg 	flag_white nomw_1966##ib1.time_emp  $covar_emp  i.state_group [pw=weight]  ///
					if flag_employed & sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_emp_women_d1
			
			*outcome: epop gap between whites and blacks
			reg flag_employed_nilf nomw_1966##ib1.time_emp##ib200.race i.sex  $covar_emp i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_epop_all_d1
			
			reg flag_employed_nilf nomw_1966##ib1.time_emp##ib200.race  $covar_emp i.state_group [iw=weight]  ///
					if inrange(age,25,55) & sex==1  & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_epop_men_d1
			
			reg flag_employed_nilf nomw_1966##ib1.time_emp##ib200.race   $covar_emp i.state_group [iw=weight]  ///
					if inrange(age,25,55) & sex==2  & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_epop_women_d1
			

		**RELATIVE EARNINGS
			reg 	ln_annual_wage nomw_1966##ib1.time##ib200.race i.sex $covar  i.state_group [pw=weight]  ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  rel_aw_all_d1

			reg 	ln_annual_wage nomw_1966##ib1.time##ib200.race $covar  i.state_group [pw=weight]  ///
					if flag_employed & sex==1 & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  rel_aw_men_d1
			
			reg 	ln_annual_wage nomw_1966##ib1.time##ib200.race  $covar  i.state_group [pw=weight]  ///
					if flag_employed & sex==2 & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  rel_aw_women_d1			
				
	**EMPLOYMENT ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*outcome: relative shares of black and white persons among employed workers			
			reg 	flag_white nomw_1966##ib1.time_emp i.sex $covar_emp  i.state_group [iw=weight]  ///
					if flag_employed & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 & flag_white!=.
			eststo  rel_emp_all_elast_d1
			
			reg 	flag_white nomw_1966##ib1.time_emp  $covar_emp  i.state_group [iw=weight]  ///
					if flag_employed & sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  rel_emp_men_elast_d1
			
			reg 	flag_white nomw_1966##ib1.time_emp  $covar_emp  i.state_group [iw=weight]  ///
					if flag_employed & sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
			eststo  rel_emp_women_elast_d1
			
			*outcome: epop gap between whites and blacks
			reg flag_employed_nilf nomw_1966##ib1.time_emp##ib200.race i.sex  $covar_emp i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  rel_epop_all_elast_d1
			
			reg flag_employed_nilf nomw_1966##ib1.time_emp##ib200.race  $covar_emp i.state_group [iw=weight]  ///
					if inrange(age,25,55) & sex==1  & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  rel_epop_men_elast_d1
			
			reg flag_employed_nilf nomw_1966##ib1.time_emp##ib200.race   $covar_emp i.state_group [iw=weight]  ///
					if inrange(age,25,55) & sex==2  & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  rel_epop_women_elast_d1
	
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg 	ln_annual_wage nomw_1966##ib1.time##ib200.race i.sex $covar  i.state_group [iw=weight]  ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  rel_aw_all_elast_d1

			reg 	ln_annual_wage nomw_1966##ib1.time##ib200.race $covar  i.state_group [iw=weight]  ///
					if flag_employed & sex==1 & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  rel_aw_men_elast_d1
			
			reg 	ln_annual_wage nomw_1966##ib1.time##ib200.race  $covar  i.state_group [iw=weight]  ///
					if flag_employed & sex==2 & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  rel_aw_women_elast_d1			
									
		**iii.LABOR-LABOR SUBSTITUTION ELASTICITIES
			**white and black shares in 1967-1972
			*all
			reg 	flag_white [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100,200) 
			eststo  share_white_all
			*men
			reg 	flag_white [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & sex==1
			eststo  share_white_men
			*women
			reg 	flag_white [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & sex==2 
			eststo  share_white_women	
			
			**mean epop ratio in 1967-1972
			**white
			*all
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100) 
			eststo  epop_white_all
			*men
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100) & sex==1
			eststo  epop_white_men
			*women
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100) & sex==2 
			eststo  epop_white_women
			
			**black
			*all
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,200) 
			eststo  epop_black_all
			*men
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,200) & sex==1
			eststo  epop_black_men
			*women
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,200) & sex==2 
			eststo  epop_black_women		
			
			
			*outcome: relative shares of black and white persons among employed workers			
			*all
			suest   share_white_all rel_emp_all_elast_d1 rel_aw_all_elast_d1, vce(cluster state_group)
			nlcom  (rel_emp_elast_all:-((1-_b[share_white_all_mean:_cons])/_b[share_white_all_mean:_cons])*(_b[rel_emp_all_elast_d1_mean:1.nomw_1966#2.time_emp]/_b[rel_aw_all_elast_d1_mean:1.nomw_1966#2.time#100.race])), post
			mat 	elast_rel_emp_all_d1 = _b[rel_emp_elast_all]\ _se[rel_emp_elast_all]\_b[rel_emp_elast_all]-1.96*_se[rel_emp_elast_all]\_b[rel_emp_elast_all]+1.96*_se[rel_emp_elast_all]

			*men
			suest   share_white_men rel_emp_men_elast_d1 rel_aw_men_elast_d1, vce(cluster state_group)
			nlcom  (rel_emp_elast_men:-((1-_b[share_white_men_mean:_cons])/_b[share_white_men_mean:_cons])*(_b[rel_emp_men_elast_d1_mean:1.nomw_1966#2.time_emp]/_b[rel_aw_men_elast_d1_mean:1.nomw_1966#2.time#100.race])), post
			mat 	elast_rel_emp_men_d1 = _b[rel_emp_elast_men]\ _se[rel_emp_elast_men]\_b[rel_emp_elast_men]-1.96*_se[rel_emp_elast_men]\_b[rel_emp_elast_men]+1.96*_se[rel_emp_elast_men]
			
			*women
			suest   share_white_women rel_emp_women_elast_d1 rel_aw_women_elast_d1, vce(cluster state_group)
			nlcom  (rel_emp_elast_women:-((1-_b[share_white_women_mean:_cons])/_b[share_white_women_mean:_cons])*(_b[rel_emp_women_elast_d1_mean:1.nomw_1966#2.time_emp]/_b[rel_aw_women_elast_d1_mean:1.nomw_1966#2.time#100.race])), post
			mat 	elast_rel_emp_women_d1 = _b[rel_emp_elast_women]\ _se[rel_emp_elast_women]\_b[rel_emp_elast_women]-1.96*_se[rel_emp_elast_women]\_b[rel_emp_elast_women]+1.96*_se[rel_emp_elast_women]
							
			*outcome: epop gap between whites and blacks
			*all
			suest   epop_black_all epop_white_all rel_epop_all_elast_d1 rel_aw_all_elast_d1, vce(cluster state_group)
			nlcom  (rel_epop_elast_all:-(_b[epop_black_all_mean:_cons]/_b[epop_white_all_mean:_cons])*(_b[rel_epop_all_elast_d1_mean:1.nomw_1966#2.time_emp]/_b[rel_aw_all_elast_d1_mean:1.nomw_1966#2.time#100.race])), post
			mat 	elast_rel_epop_all_d1 = _b[rel_epop_elast_all]\ _se[rel_epop_elast_all]\_b[rel_epop_elast_all]-1.96*_se[rel_epop_elast_all]\_b[rel_epop_elast_all]+1.96*_se[rel_epop_elast_all]

			*men
			suest   epop_black_men epop_white_men rel_epop_men_elast_d1 rel_aw_men_elast_d1, vce(cluster state_group)
			nlcom  (rel_epop_elast_men:-(_b[epop_black_men_mean:_cons]/_b[epop_white_men_mean:_cons])*(_b[rel_epop_men_elast_d1_mean:1.nomw_1966#2.time_emp]/_b[rel_aw_men_elast_d1_mean:1.nomw_1966#2.time#100.race])), post
			mat 	elast_rel_epop_men_d1 = _b[rel_epop_elast_men]\ _se[rel_epop_elast_men]\_b[rel_epop_elast_men]-1.96*_se[rel_epop_elast_men]\_b[rel_epop_elast_men]+1.96*_se[rel_epop_elast_men]
			
			*women
			suest   epop_black_women epop_white_women rel_epop_women_elast_d1 rel_aw_women_elast_d1, vce(cluster state_group)
			nlcom  (rel_epop_elast_women:-(_b[epop_black_women_mean:_cons]/_b[epop_white_women_mean:_cons])*(_b[rel_epop_women_elast_d1_mean:1.nomw_1966#2.time_emp]/_b[rel_aw_women_elast_d1_mean:1.nomw_1966#2.time#100.race])), post
			mat 	elast_rel_epop_women_d1 = _b[rel_epop_elast_women]\ _se[rel_epop_elast_women]\_b[rel_epop_elast_women]-1.96*_se[rel_epop_elast_women]\_b[rel_epop_elast_women]+1.96*_se[rel_epop_elast_women]
							

							
 ***DESIGN #2: KAITZ INDEX 1966
	*%%---ALL------------------------------------------------	
		**CALCULATE STANDARDIZED TREATMENT VARIABLE 
			cap drop KI_1966S_aw_std KI_1966S_emp_std 
			egen KI_1966S_aw_std = std(KI_1966S)  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			egen KI_1966S_emp_std = std(KI_1966S) if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			
			su 	 KI_1966S if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			su 	 KI_1966S if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, det
			*tabstat KI_1966S, statistics( iqr )
		
		**MAIN ESTIMATES
		**EMPLOYMENT	
			reg 	flag_white c.KI_1966S_emp_std##ib1.time_emp i.sex $covar_emp  i.state_group [pw=weight]  ///
					if flag_employed & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_emp_all_d2
		
			reg 	flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp##ib200.race i.sex  $covar_emp i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_epop_all_d2
			
		**EARNINGS
			reg 	ln_annual_wage c.KI_1966S_aw_std##ib1.time##ib200.race i.sex $covar  i.state_group [pw=weight]  ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  rel_aw_all_d2
			
			
		**EMP and EARNINGS FOR LABOR-LABOR ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			reg 	flag_white c.KI_1966S_emp_std##ib1.time_emp i.sex $covar_emp  i.state_group [iw=weight]  ///
					if flag_employed & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 & flag_white!=.
			eststo  rel_emp_all_elast_d2
			
			reg 	flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp##ib200.race i.sex  $covar_emp i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  rel_epop_all_elast_d2
			
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg 	ln_annual_wage c.KI_1966S_aw_std##ib1.time##ib200.race i.sex $covar  i.state_group [iw=weight]  ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  rel_aw_all_elast_d2
				
		**iii. LABOR-LABOR ELASTICITIES
			suest   share_white_all rel_emp_all_elast_d2 rel_aw_all_elast_d2, vce(cluster state_group)
			nlcom  (rel_emp_elast_all:-((1-_b[share_white_all_mean:_cons])/_b[share_white_all_mean:_cons])*(_b[rel_emp_all_elast_d2_mean:c.KI_1966S_emp_std#2.time_emp]/_b[rel_aw_all_elast_d2_mean:c.KI_1966S_aw_std#2.time#100.race])), post
			mat 	elast_rel_emp_all_d2 = _b[rel_emp_elast_all]\ _se[rel_emp_elast_all]\_b[rel_emp_elast_all]-1.96*_se[rel_emp_elast_all]\_b[rel_emp_elast_all]+1.96*_se[rel_emp_elast_all]

			suest   epop_black_all epop_white_all rel_epop_all_elast_d2 rel_aw_all_elast_d2, vce(cluster state_group)
			nlcom  (rel_epop_elast_all:-(_b[epop_black_all_mean:_cons]/_b[epop_white_all_mean:_cons])*(_b[rel_epop_all_elast_d2_mean:c.KI_1966S_emp_std#2.time_emp]/_b[rel_aw_all_elast_d2_mean:c.KI_1966S_aw_std#2.time#100.race])), post
			mat 	elast_rel_epop_all_d2 = _b[rel_epop_elast_all]\ _se[rel_epop_elast_all]\_b[rel_epop_elast_all]-1.96*_se[rel_epop_elast_all]\_b[rel_epop_elast_all]+1.96*_se[rel_epop_elast_all]


	*%%---MEN------------------------------------------------	
		**CALCULATE STANDARDIZED TREATMENT VARIABLE 
			drop KI_1966S_aw_std KI_1966S_emp_std
			egen KI_1966S_aw_std = std(KI_1966S) if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22 & sex==1
			egen KI_1966S_emp_std = std(KI_1966S) if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 & sex==1
		
		**MAIN ESTIMATES
		**EMPLOYMENT	
			reg 	flag_white c.KI_1966S_emp_std##ib1.time_emp  $covar_emp  i.state_group [pw=weight]  ///
					if flag_employed & sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_emp_men_d2
		
			reg 	flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp##ib200.race   $covar_emp i.state_group [pw=weight]  ///
					if inrange(age,25,55) & sex==1 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_epop_men_d2
			
		**EARNINGS
			reg 	ln_annual_wage c.KI_1966S_aw_std##ib1.time##ib200.race  $covar  i.state_group [pw=weight]  ///
					if flag_employed & sex==1 & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  rel_aw_men_d2
			
			
		**EMP and EARNINGS FOR LABOR-LABOR ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			reg 	flag_white c.KI_1966S_emp_std##ib1.time_emp  $covar_emp  i.state_group [iw=weight]  ///
					if flag_employed & sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 & flag_white!=.
			eststo  rel_emp_men_elast_d2
			
			reg 	flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp##ib200.race   $covar_emp i.state_group [iw=weight]  ///
					if inrange(age,25,55) & sex==1 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  rel_epop_men_elast_d2
			
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg 	ln_annual_wage c.KI_1966S_aw_std##ib1.time##ib200.race  $covar  i.state_group [iw=weight]  ///
					if flag_employed & sex==1 & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  rel_aw_men_elast_d2
				
		**iii. LABOR-LABOR ELASTICITIES
			suest   share_white_men rel_emp_men_elast_d2 rel_aw_men_elast_d2, vce(cluster state_group)
			nlcom  (rel_emp_elast_men:-((1-_b[share_white_men_mean:_cons])/_b[share_white_men_mean:_cons])*(_b[rel_emp_men_elast_d2_mean:c.KI_1966S_emp_std#2.time_emp]/_b[rel_aw_men_elast_d2_mean:c.KI_1966S_aw_std#2.time#100.race])), post
			mat 	elast_rel_emp_men_d2 = _b[rel_emp_elast_men]\ _se[rel_emp_elast_men]\_b[rel_emp_elast_men]-1.96*_se[rel_emp_elast_men]\_b[rel_emp_elast_men]+1.96*_se[rel_emp_elast_men]

			suest   epop_black_men epop_white_men rel_epop_men_elast_d2 rel_aw_men_elast_d2, vce(cluster state_group)
			nlcom  (rel_epop_elast_men:-(_b[epop_black_men_mean:_cons]/_b[epop_white_men_mean:_cons])*(_b[rel_epop_men_elast_d2_mean:c.KI_1966S_emp_std#2.time_emp]/_b[rel_aw_men_elast_d2_mean:c.KI_1966S_aw_std#2.time#100.race])), post
			mat 	elast_rel_epop_men_d2 = _b[rel_epop_elast_men]\ _se[rel_epop_elast_men]\_b[rel_epop_elast_men]-1.96*_se[rel_epop_elast_men]\_b[rel_epop_elast_men]+1.96*_se[rel_epop_elast_men]
	
	
	*%%---WOMEN------------------------------------------------	
		**CALCULATE STANDARDIZED TREATMENT VARIABLE 
			drop KI_1966S_aw_std KI_1966S_emp_std			
			egen KI_1966S_aw_std = std(KI_1966S) if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22 & sex==2
			egen KI_1966S_emp_std = std(KI_1966S) if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 & sex==2
			
		**MAIN ESTIMATES
		**EMPLOYMENT	
			reg 	flag_white c.KI_1966S_emp_std##ib1.time_emp  $covar_emp  i.state_group [pw=weight]  ///
					if flag_employed & sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_emp_women_d2
		
			reg 	flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp##ib200.race   $covar_emp i.state_group [pw=weight]  ///
					if inrange(age,25,55) & sex==2 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_epop_women_d2
			
		**EARNINGS
			reg 	ln_annual_wage c.KI_1966S_aw_std##ib1.time##ib200.race  $covar  i.state_group [pw=weight]  ///
					if flag_employed & sex==2 & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  rel_aw_women_d2
			
			
		**EMP and EARNINGS FOR LABOR-LABOR ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			reg 	flag_white c.KI_1966S_emp_std##ib1.time_emp  $covar_emp  i.state_group [iw=weight]  ///
					if flag_employed & sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 & flag_white!=.
			eststo  rel_emp_women_elast_d2
			
			reg 	flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp##ib200.race   $covar_emp i.state_group [iw=weight]  ///
					if inrange(age,25,55) & sex==2 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  rel_epop_women_elast_d2
			
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg 	ln_annual_wage c.KI_1966S_aw_std##ib1.time##ib200.race  $covar  i.state_group [iw=weight]  ///
					if flag_employed & sex==2 & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  rel_aw_women_elast_d2
				
		**iii. LABOR-LABOR ELASTICITIES
			suest   share_white_women rel_emp_women_elast_d2 rel_aw_women_elast_d2, vce(cluster state_group)
			nlcom  (rel_emp_elast_women:-((1-_b[share_white_women_mean:_cons])/_b[share_white_women_mean:_cons])*(_b[rel_emp_women_elast_d2_mean:c.KI_1966S_emp_std#2.time_emp]/_b[rel_aw_women_elast_d2_mean:c.KI_1966S_aw_std#2.time#100.race])), post
			mat 	elast_rel_emp_women_d2 = _b[rel_emp_elast_women]\ _se[rel_emp_elast_women]\_b[rel_emp_elast_women]-1.96*_se[rel_emp_elast_women]\_b[rel_emp_elast_women]+1.96*_se[rel_emp_elast_women]

			suest   epop_black_women epop_white_women rel_epop_women_elast_d2 rel_aw_women_elast_d2, vce(cluster state_group)
			nlcom  (rel_epop_elast_women:-(_b[epop_black_women_mean:_cons]/_b[epop_white_women_mean:_cons])*(_b[rel_epop_women_elast_d2_mean:c.KI_1966S_emp_std#2.time_emp]/_b[rel_aw_women_elast_d2_mean:c.KI_1966S_aw_std#2.time#100.race])), post
			mat 	elast_rel_epop_women_d2 = _b[rel_epop_elast_women]\ _se[rel_epop_elast_women]\_b[rel_epop_elast_women]-1.96*_se[rel_epop_elast_women]\_b[rel_epop_elast_women]+1.96*_se[rel_epop_elast_women]
							
							
  ***DESIGN #3: FRACTION OF AFFECTED WORKERS BAILEY ET AL. 2018
	*%%---ALL------------------------------------------------	
		**CALCULATE STANDARDIZED TREATMENT VARIABLE 
			cap drop F_s1966_aw_std F_s1966_emp_std
			egen F_s1966_aw_std = std(F_s1966)  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			egen F_s1966_emp_std = std(F_s1966) if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			
			su 	 F_s1966 if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			su 	 F_s1966 if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=2
				
		**MAIN ESTIMATES
		**EMPLOYMENT	
			reg 	flag_white c.F_s1966_emp_std##ib1.time_emp i.sex $covar_emp  i.state_group [pw=weight]  ///
					if flag_employed & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_emp_all_d3
		
			reg 	flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp##ib200.race i.sex  $covar_emp i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_epop_all_d3
			
		**EARNINGS
			reg 	ln_annual_wage c.F_s1966_aw_std##ib1.time##ib200.race i.sex $covar  i.state_group [pw=weight]  ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  rel_aw_all_d3
			
			
		**EMP and EARNINGS FOR LABOR-LABOR ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			reg 	flag_white c.F_s1966_emp_std##ib1.time_emp i.sex $covar_emp  i.state_group [iw=weight]  ///
					if flag_employed & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 & flag_white!=.
			eststo  rel_emp_all_elast_d3
			
			reg 	flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp##ib200.race i.sex  $covar_emp i.state_group [iw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  rel_epop_all_elast_d3
			
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg 	ln_annual_wage c.F_s1966_aw_std##ib1.time##ib200.race i.sex $covar  i.state_group [iw=weight]  ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  rel_aw_all_elast_d3
				
		**iii. LABOR-LABOR ELASTICITIES
			suest   share_white_all rel_emp_all_elast_d3 rel_aw_all_elast_d3, vce(cluster state_group)
			nlcom  (rel_emp_elast_all:-((1-_b[share_white_all_mean:_cons])/_b[share_white_all_mean:_cons])*(_b[rel_emp_all_elast_d3_mean:c.F_s1966_emp_std#2.time_emp]/_b[rel_aw_all_elast_d3_mean:c.F_s1966_aw_std#2.time#100.race])), post
			mat 	elast_rel_emp_all_d3 = _b[rel_emp_elast_all]\ _se[rel_emp_elast_all]\_b[rel_emp_elast_all]-1.96*_se[rel_emp_elast_all]\_b[rel_emp_elast_all]+1.96*_se[rel_emp_elast_all]

			suest   epop_black_all epop_white_all rel_epop_all_elast_d3 rel_aw_all_elast_d3, vce(cluster state_group)
			nlcom  (rel_epop_elast_all:-(_b[epop_black_all_mean:_cons]/_b[epop_white_all_mean:_cons])*(_b[rel_epop_all_elast_d3_mean:c.F_s1966_emp_std#2.time_emp]/_b[rel_aw_all_elast_d3_mean:c.F_s1966_aw_std#2.time#100.race])), post
			mat 	elast_rel_epop_all_d3 = _b[rel_epop_elast_all]\ _se[rel_epop_elast_all]\_b[rel_epop_elast_all]-1.96*_se[rel_epop_elast_all]\_b[rel_epop_elast_all]+1.96*_se[rel_epop_elast_all]


	*%%---MEN------------------------------------------------	
		**CALCULATE STANDARDIZED TREATMENT VARIABLE 
			drop F_s1966_aw_std F_s1966_emp_std
			egen F_s1966_aw_std = std(F_s1966)  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22 & sex==1
			egen F_s1966_emp_std = std(F_s1966) if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 & sex==1
			
		**MAIN ESTIMATES
		**EMPLOYMENT	
			reg 	flag_white c.F_s1966_emp_std##ib1.time_emp  $covar_emp  i.state_group [pw=weight]  ///
					if flag_employed & sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_emp_men_d3
		
			reg 	flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp##ib200.race   $covar_emp i.state_group [pw=weight]  ///
					if inrange(age,25,55) & sex==1 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_epop_men_d3
			
		**EARNINGS
			reg 	ln_annual_wage c.F_s1966_aw_std##ib1.time##ib200.race  $covar  i.state_group [pw=weight]  ///
					if flag_employed & sex==1 & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  rel_aw_men_d3
			
			
		**EMP and EARNINGS FOR LABOR-LABOR ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			reg 	flag_white c.F_s1966_emp_std##ib1.time_emp  $covar_emp  i.state_group [iw=weight]  ///
					if flag_employed & sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 & flag_white!=.
			eststo  rel_emp_men_elast_d3
			
			reg 	flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp##ib200.race   $covar_emp i.state_group [iw=weight]  ///
					if inrange(age,25,55) & sex==1 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  rel_epop_men_elast_d3
			
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg 	ln_annual_wage c.F_s1966_aw_std##ib1.time##ib200.race  $covar  i.state_group [iw=weight]  ///
					if flag_employed & sex==1 & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  rel_aw_men_elast_d3
				
		**iii. LABOR-LABOR ELASTICITIES
			suest   share_white_men rel_emp_men_elast_d3 rel_aw_men_elast_d3, vce(cluster state_group)
			nlcom  (rel_emp_elast_men:-((1-_b[share_white_men_mean:_cons])/_b[share_white_men_mean:_cons])*(_b[rel_emp_men_elast_d3_mean:c.F_s1966_emp_std#2.time_emp]/_b[rel_aw_men_elast_d3_mean:c.F_s1966_aw_std#2.time#100.race])), post
			mat 	elast_rel_emp_men_d3 = _b[rel_emp_elast_men]\ _se[rel_emp_elast_men]\_b[rel_emp_elast_men]-1.96*_se[rel_emp_elast_men]\_b[rel_emp_elast_men]+1.96*_se[rel_emp_elast_men]

			suest   epop_black_men epop_white_men rel_epop_men_elast_d3 rel_aw_men_elast_d3, vce(cluster state_group)
			nlcom  (rel_epop_elast_men:-(_b[epop_black_men_mean:_cons]/_b[epop_white_men_mean:_cons])*(_b[rel_epop_men_elast_d3_mean:c.F_s1966_emp_std#2.time_emp]/_b[rel_aw_men_elast_d3_mean:c.F_s1966_aw_std#2.time#100.race])), post
			mat 	elast_rel_epop_men_d3 = _b[rel_epop_elast_men]\ _se[rel_epop_elast_men]\_b[rel_epop_elast_men]-1.96*_se[rel_epop_elast_men]\_b[rel_epop_elast_men]+1.96*_se[rel_epop_elast_men]
	
	
	*%%---WOMEN------------------------------------------------	
		**CALCULATE STANDARDIZED TREATMENT VARIABLE 
			drop F_s1966_aw_std F_s1966_emp_std
			egen F_s1966_aw_std = std(F_s1966)  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22 & sex==2
			egen F_s1966_emp_std = std(F_s1966) if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 & sex==2
			
		**MAIN ESTIMATES
		**EMPLOYMENT	
			reg 	flag_white c.F_s1966_emp_std##ib1.time_emp  $covar_emp  i.state_group [pw=weight]  ///
					if flag_employed & sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_emp_women_d3
		
			reg 	flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp##ib200.race   $covar_emp i.state_group [pw=weight]  ///
					if inrange(age,25,55) & sex==2 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group) 
			eststo  rel_epop_women_d3
			
		**EARNINGS
			reg 	ln_annual_wage c.F_s1966_aw_std##ib1.time##ib200.race  $covar  i.state_group [pw=weight]  ///
					if flag_employed & sex==2 & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22, cluster(state_group)
			eststo  rel_aw_women_d3
			
			
		**EMP and EARNINGS FOR LABOR-LABOR ELASTICITIES
		**i.EMPLOYMENT [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			reg 	flag_white c.F_s1966_emp_std##ib1.time_emp  $covar_emp  i.state_group [iw=weight]  ///
					if flag_employed & sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 & flag_white!=.
			eststo  rel_emp_women_elast_d3
			
			reg 	flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp##ib200.race   $covar_emp i.state_group [iw=weight]  ///
					if inrange(age,25,55) & sex==2 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			eststo  rel_epop_women_elast_d3
			
		**ii.EARNINGS [USING IWEIGHTS AND NO CLUSTER SO THAT CAN BE USED IN SUEST]
			*iweights are not allowed with reghdfe, so using reg for earnings instead		
			reg 	ln_annual_wage c.F_s1966_aw_std##ib1.time##ib200.race  $covar  i.state_group [iw=weight]  ///
					if flag_employed & sex==2 & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
			eststo  rel_aw_women_elast_d3
				
		**iii. LABOR-LABOR ELASTICITIES
			suest   share_white_women rel_emp_women_elast_d3 rel_aw_women_elast_d3, vce(cluster state_group)
			nlcom  (rel_emp_elast_women:-((1-_b[share_white_women_mean:_cons])/_b[share_white_women_mean:_cons])*(_b[rel_emp_women_elast_d3_mean:c.F_s1966_emp_std#2.time_emp]/_b[rel_aw_women_elast_d3_mean:c.F_s1966_aw_std#2.time#100.race])), post
			mat 	elast_rel_emp_women_d3 = _b[rel_emp_elast_women]\ _se[rel_emp_elast_women]\_b[rel_emp_elast_women]-1.96*_se[rel_emp_elast_women]\_b[rel_emp_elast_women]+1.96*_se[rel_emp_elast_women]

			suest   epop_black_women epop_white_women rel_epop_women_elast_d3 rel_aw_women_elast_d3, vce(cluster state_group)
			nlcom  (rel_epop_elast_women:-(_b[epop_black_women_mean:_cons]/_b[epop_white_women_mean:_cons])*(_b[rel_epop_women_elast_d3_mean:c.F_s1966_emp_std#2.time_emp]/_b[rel_aw_women_elast_d3_mean:c.F_s1966_aw_std#2.time#100.race])), post
			mat 	elast_rel_epop_women_d3 = _b[rel_epop_elast_women]\ _se[rel_epop_elast_women]\_b[rel_epop_elast_women]-1.96*_se[rel_epop_elast_women]\_b[rel_epop_elast_women]+1.96*_se[rel_epop_elast_women]							
	
 *%-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------						
	*iv. COMBINING RESULTS ACROSS CROSS-STATE DESIGNS	
			mat elast_rel_emp = (elast_rel_emp_all_d1, elast_rel_emp_men_d1, elast_rel_emp_women_d1, elast_rel_emp_all_d2, elast_rel_emp_men_d2, elast_rel_emp_women_d2, elast_rel_emp_all_d3, elast_rel_emp_men_d3, elast_rel_emp_women_d3)
			mat rownames elast_rel_emp = "\rule{0pt}{3ex}\textbf{L-L elast. (emp. shares)}" "se" "lower bound" "upper bound"
			mat elast_rel_epop = (elast_rel_epop_all_d1, elast_rel_epop_men_d1, elast_rel_epop_women_d1, elast_rel_epop_all_d2, elast_rel_epop_men_d2, elast_rel_epop_women_d2, elast_rel_epop_all_d3, elast_rel_epop_men_d3, elast_rel_epop_women_d3)
			mat rownames elast_rel_epop = "\rule{0pt}{3ex}\textbf{L-L elast. (epop gap)}" "se" "lower bound" "upper bound"
			mat elast_rel 	= (elast_rel_emp\elast_rel_epop)	

			
		*v. OUTPUT TABLE
			esttab 	rel_emp_all_d1 rel_emp_men_d1 rel_emp_women_d1  rel_emp_all_d2 rel_emp_men_d2 rel_emp_women_d2 rel_emp_all_d3 rel_emp_men_d3 rel_emp_women_d3 ///
					using "tables/table_llsubstitution_cps.tex", replace  label fragment ///
					nolines  posthead(\cmidrule{2-10}) booktabs ///
					nonumbers mtitle("All" "Men" "Women" "All" "Men" "Women" "All" "Men" "Women" ) collabels(none)    ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					keep(1.nomw_1966#2.time_emp 2.time_emp#c.KI_1966S_emp_std 2.time_emp#c.F_s1966_emp_std)   ///				
					refcat(1.nomw_1966#2.time_emp "Treatment var. $\times$& & & \\ \hspace{0.5cm}{1967-1972}", nolabel) ///
					coeflabel(1.nomw_1966#2.time_emp "\rule{0pt}{3ex}\textbf{Relative W/B shares of workers}") ///
					stats(N, fmt(%11.0gc) label("\hspace{0.2cm}{}")) onecell 
			esttab 	rel_epop_all_d1 rel_epop_men_d1 rel_epop_women_d1  rel_epop_all_d2 rel_epop_men_d2 rel_epop_women_d2 rel_epop_all_d3 rel_epop_men_d3 rel_epop_women_d3  ///
					using "tables/table_llsubstitution_cps.tex", append  label fragment ///
					nolines nonumbers nomtitles collabels(none)    ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					keep(1.nomw_1966#2.time_emp#100.race 2.time_emp#100.race#c.KI_1966S_emp_std 2.time_emp#100.race#c.F_s1966_emp_std)   ///				
					coeflabel(1.nomw_1966#2.time_emp#100.race "\rule{0pt}{3ex}\textbf{Relative W/B epop gap}") ///
					stats(N, fmt(%11.0gc) label("\hspace{0.2cm}{}")) onecell 					
			esttab 	rel_aw_all_d1 rel_aw_men_d1 rel_aw_women_d1  rel_aw_all_d2 rel_aw_men_d2 rel_aw_women_d2 rel_aw_all_d3 rel_aw_men_d3 rel_aw_women_d3  ///
					using "tables/table_llsubstitution_cps.tex", append  label fragment ///
					nolines nonumbers nomtitles collabels(none)  ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					keep(1.nomw_1966#2.time#100.race 2.time#100.race#c.KI_1966S_aw_std 2.time#100.race#c.F_s1966_aw_std)   ///				
					coeflabel(1.nomw_1966#2.time#100.race "\rule{0pt}{3ex}\textbf{Relative W/B earnings}") ///
					stats(N, fmt(%11.0gc) label("\hspace{0.2cm}{}")) onecell 											
			esttab	matrix(elast_rel,fmt(%3.2f)) ///
					using "tables/table_llsubstitution_cps.tex", append  label fragment ///
					nolines booktabs ///
					nonumbers nomtitles collabels(none) postfoot(\bottomrule) 
							

*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*TABLES E7 & E8  MAIN EFFECTS OF 1967 REFORM ON EMPLOYMENT, CONDITIONAL ON UNEMPLOYMENT RATES AT THE STATE-LEVL 
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 	
 	foreach var of varlist   bm_ssa_1966 south {
 ***DESIGN #1: STRONGLY vs. WEAKLY TREATED
		**EMPLOYMENT	
			*all
			reg 	flag_employed_unemp nomw_1966##ib1.time_emp##i.`var' i.sex i.race $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_a_d1_`var'
			*black
			reg 	flag_employed_unemp nomw_1966##ib1.time_emp##i.`var' i.sex  $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,200)  & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_b_d1_`var'
			*white
			reg 	flag_employed_unemp nomw_1966##ib1.time_emp##i.`var' i.sex  $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100) & race==100 & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_w_d1_`var'
	

  ***DESIGN #2: KAITZ INDEX 1966
		*%%---ALL------------------------------------------------	
			cap drop KI_1966S_emp_std 
			egen KI_1966S_emp_std = std(KI_1966S) if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			**EMPLOYMENT	
			reg 	flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp##i.`var' i.sex i.race $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_a_d2_`var'
				
		*%%---BLACK------------------------------------------------	
			drop KI_1966S_emp_std
			egen KI_1966S_emp_std = std(KI_1966S) if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			**EMPLOYMENT	
			reg 	flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp##i.`var' i.sex i.race $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_b_d2_`var'	
			
		*%%---WHITE------------------------------------------------	
			drop KI_1966S_emp_std
			egen KI_1966S_emp_std = std(KI_1966S) if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22			
			**EMPLOYMENT	
			reg 	flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp##i.`var' i.sex i.race $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_w_d2_`var'	
					
	***DESIGN #3: FRACTION OF AFFECTED WORKERS BAILEY ET AL. 2018
		*%%---ALL------------------------------------------------	
			cap drop F_s1966_emp_std
			egen F_s1966_emp_std = std(F_s1966) if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			
			**EMPLOYMENT	
			reg 	flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp##i.`var'  i.sex i.race $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_a_d3_`var'
			
		*%%---BLACK------------------------------------------------	
			drop F_s1966_emp_std
			egen F_s1966_emp_std = std(F_s1966) if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			
			**EMPLOYMENT	
			reg 	flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp##i.`var'  i.sex i.race $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_b_d3_`var'
			
		*%%---WHITE------------------------------------------------	
			drop F_s1966_emp_std
			egen F_s1966_emp_std = std(F_s1966) if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22
			
			**EMPLOYMENT	
			reg 	flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp##i.`var'  i.sex i.race $covar_emp  i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22, cluster(state_group)
			eststo  emp_w_d3_`var'
			

			
		*OUTPUT TABLE
			esttab 	emp_a_d1_`var' emp_b_d1_`var' emp_w_d1_`var' emp_a_d2_`var' emp_b_d2_`var' emp_w_d2_`var'  emp_a_d3_`var' emp_b_d3_`var' emp_w_d3_`var' ///
					using "tables/table_emp_cps_conditional_`var'.tex", replace  label fragment ///
					nolines  posthead(\cmidrule{2-10}) booktabs ///
					nonumbers mtitle("All" "Black" "White" "All" "Black" "White" "All" "Black" "White") collabels(none)    ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					keep(1.nomw_1966#2.time_emp 2.time_emp#c.KI_1966S_emp_std 2.time_emp#c.F_s1966_emp_std)   ///				
					coeflabel(1.nomw_1966#2.time_emp "Treatment var. $\times$ 1967-1972")			
			esttab 	emp_a_d1_`var' emp_b_d1_`var' emp_w_d1_`var' emp_a_d2_`var' emp_b_d2_`var' emp_w_d2_`var'  emp_a_d3_`var' emp_b_d3_`var' emp_w_d3_`var' ///
					using "tables/table_emp_cps_conditional_`var'.tex", append  label fragment ///
					nolines nonumbers nomtitles collabels(none)  ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					keep(2.time_emp#1.`var' 2.time_emp#1.`var' 2.time_emp#1.`var')   ///				
					coeflabel(2.time_emp#1.`var' "\rule{0pt}{3ex}South $\times$ 1967-1972")					
			esttab 	emp_a_d1_`var' emp_b_d1_`var' emp_w_d1_`var' emp_a_d2_`var' emp_b_d2_`var' emp_w_d2_`var'  emp_a_d3_`var' emp_b_d3_`var' emp_w_d3_`var' ///
					using "tables/table_emp_cps_conditional_`var'.tex", append  label fragment ///
					nolines nonumbers nomtitles collabels(none)  ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) postfoot(\bottomrule) ///						
					keep(1.nomw_1966#2.time_emp#1.`var' 2.time_emp#1.`var'#c.KI_1966S_emp_std 2.time_emp#1.`var'#c.F_s1966_emp_std)   ///				
					coeflabel(1.nomw_1966#2.time_emp#1.`var' "\rule{0pt}{3ex}South $\times$ Treatment var. $\times$ 1967-72") ///	
					stats(N, fmt(%11.0gc) label("\hspace{0.2cm}{}")) onecell 													
					
		}
 
 
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*TABLE E4 [APPENDIX]: EFFECT OF 1967 REFORM USING STATE MINIMUM WAGE LEGISLATION
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*>>>>>>>ALL		
	**MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 	
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo aw_nomw_all
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
		if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo epop_nomw_all
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	
	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
	eststo emp_nomw_all
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**LN_HOURS
		reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_nomw_all
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
	**EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
	eststo aw_elast_nomw_all	
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
	eststo epop_elast_nomw_all

	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [iw=weight]  ///
		if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo emp_elast_nomw_all	
	
	**LN_HOURS
	reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo ahours_elast_nomw_all
  
  
	**ELASTICITIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  epop_all
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  emp_all
			
			*matrices elasticties		
			suest   epop_all epop_elast_nomw_all aw_elast_nomw_all, vce(cluster state_group)
			nlcom  (epop_elast_all:(1/_b[epop_all_mean:_cons])*(_b[epop_elast_nomw_all_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_all_mean:1.nomw#2.time])), post
			mat 	epop_elast_all = _b[epop_elast_all]\ _se[epop_elast_all]\_b[epop_elast_all]-1.96*_se[epop_elast_all]\_b[epop_elast_all]+1.96*_se[epop_elast_all]
			*ADD-ON:FOR TABLE A6 [SLIDES ONLY]
			mat epop_elast_all_slides = _b[epop_elast_all]\ _se[epop_elast_all]
		
			suest   emp_all emp_elast_nomw_all aw_elast_nomw_all, vce(cluster state_group)
			nlcom  (emp_elast_all:(1/_b[emp_all_mean:_cons])*(_b[emp_elast_nomw_all_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_all_mean:1.nomw#2.time])), post
			mat 	emp_elast_all = _b[emp_elast_all]\ _se[emp_elast_all]\_b[emp_elast_all]-1.96*_se[emp_elast_all]\_b[emp_elast_all]+1.96*_se[emp_elast_all]
			
			suest   ahours_elast_nomw_all aw_elast_nomw_all, vce(cluster state_group)
			nlcom  (ahours_elast_all:_b[ahours_elast_nomw_all_mean:1.nomw#2.time]/_b[aw_elast_nomw_all_mean:1.nomw#2.time]), post
			mat 	ahours_elast_all = _b[ahours_elast_all]\ _se[ahours_elast_all]\_b[ahours_elast_all]-1.96*_se[ahours_elast_all]\_b[ahours_elast_all]+1.96*_se[ahours_elast_all]

*>>>>>>>BLACK PERSONS		
	**MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 	
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo aw_nomw_black
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
		if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo epop_nomw_black
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	
	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
	eststo emp_nomw_black
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**LN_HOURS
		reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_nomw_black
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
	**EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
	eststo aw_elast_nomw_black	
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
	eststo epop_elast_nomw_black

	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [iw=weight]  ///
		if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo emp_elast_nomw_black	
	
	**LN_HOURS
	reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo ahours_elast_nomw_black
  
  
	**ELASTICITIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,200) & state_group!=22
			eststo  epop_black
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,200) & state_group!=22
			eststo  emp_black
			
			*matrices elasticties		
			suest   epop_black epop_elast_nomw_black aw_elast_nomw_black, vce(cluster state_group)
			nlcom  (epop_elast_black:(1/_b[epop_black_mean:_cons])*(_b[epop_elast_nomw_black_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_black_mean:1.nomw#2.time])), post
			mat 	epop_elast_black = _b[epop_elast_black]\ _se[epop_elast_black]\_b[epop_elast_black]-1.96*_se[epop_elast_black]\_b[epop_elast_black]+1.96*_se[epop_elast_black]
			*ADD-ON:FOR TABLE A6 [SLIDES ONLY]
			mat epop_elast_black_slides = _b[epop_elast_black]\ _se[epop_elast_black]
		
			suest   emp_black emp_elast_nomw_black aw_elast_nomw_black, vce(cluster state_group)
			nlcom  (emp_elast_black:(1/_b[emp_black_mean:_cons])*(_b[emp_elast_nomw_black_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_black_mean:1.nomw#2.time])), post
			mat 	emp_elast_black = _b[emp_elast_black]\ _se[emp_elast_black]\_b[emp_elast_black]-1.96*_se[emp_elast_black]\_b[emp_elast_black]+1.96*_se[emp_elast_black]
			
			suest   ahours_elast_nomw_black aw_elast_nomw_black, vce(cluster state_group)
			nlcom  (ahours_elast_black:_b[ahours_elast_nomw_black_mean:1.nomw#2.time]/_b[aw_elast_nomw_black_mean:1.nomw#2.time]), post
			mat 	ahours_elast_black = _b[ahours_elast_black]\ _se[ahours_elast_black]\_b[ahours_elast_black]-1.96*_se[ahours_elast_black]\_b[ahours_elast_black]+1.96*_se[ahours_elast_black]
			
*>>>>>>>WHITE PERSONS		
	**MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 	
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo aw_nomw_white
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
		if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo epop_nomw_white
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	
	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
	eststo emp_nomw_white
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**LN_HOURS
		reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_nomw_white
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
	**EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
	eststo aw_elast_nomw_white	
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
	eststo epop_elast_nomw_white

	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [iw=weight]  ///
		if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo emp_elast_nomw_white	
	
	**LN_HOURS
	reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo ahours_elast_nomw_white
  
  
	**ELASTICITIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100) & state_group!=22
			eststo  epop_white
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100) & state_group!=22
			eststo  emp_white
			
			*matrices elasticties		
			suest   epop_white epop_elast_nomw_white aw_elast_nomw_white, vce(cluster state_group)
			nlcom  (epop_elast_white:(1/_b[epop_white_mean:_cons])*(_b[epop_elast_nomw_white_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_white_mean:1.nomw#2.time])), post
			mat 	epop_elast_white = _b[epop_elast_white]\ _se[epop_elast_white]\_b[epop_elast_white]-1.96*_se[epop_elast_white]\_b[epop_elast_white]+1.96*_se[epop_elast_white]
			*ADD-ON:FOR TABLE A6 [SLIDES ONLY]
			mat epop_elast_white_slides = _b[epop_elast_white]\ _se[epop_elast_white]
		
			suest   emp_white emp_elast_nomw_white aw_elast_nomw_white, vce(cluster state_group)
			nlcom  (emp_elast_white:(1/_b[emp_white_mean:_cons])*(_b[emp_elast_nomw_white_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_white_mean:1.nomw#2.time])), post
			mat 	emp_elast_white = _b[emp_elast_white]\ _se[emp_elast_white]\_b[emp_elast_white]-1.96*_se[emp_elast_white]\_b[emp_elast_white]+1.96*_se[emp_elast_white]
			
			suest   ahours_elast_nomw_white aw_elast_nomw_white, vce(cluster state_group)
			nlcom  (ahours_elast_white:_b[ahours_elast_nomw_white_mean:1.nomw#2.time]/_b[aw_elast_nomw_white_mean:1.nomw#2.time]), post
			mat 	ahours_elast_white = _b[ahours_elast_white]\ _se[ahours_elast_white]\_b[ahours_elast_white]-1.96*_se[ahours_elast_white]\_b[ahours_elast_white]+1.96*_se[ahours_elast_white]
						
*>>>>>>>MEN	
	**MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 	
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) & sex==1 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo aw_nomw_men
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
		if inrange(age,25,55) & sex==1 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo epop_nomw_men
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	
	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & sex==1 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
	eststo emp_nomw_men
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**LN_HOURS
		reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & sex==1 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_nomw_men
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
	**EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) & sex==1 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
	eststo aw_elast_nomw_men	
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & sex==1 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
	eststo epop_elast_nomw_men

	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [iw=weight]  ///
		if inrange(age,25,55) & sex==1 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo emp_elast_nomw_men	
	
	**LN_HOURS
	reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & sex==1 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo ahours_elast_nomw_men
  
  
	**ELASTICITIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & sex==1 & inlist(race,100,200) & state_group!=22
			eststo  epop_men
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & sex==1 & inlist(race,100,200) & state_group!=22
			eststo  emp_men
			
			*matrices elasticties		
			suest   epop_men epop_elast_nomw_men aw_elast_nomw_men, vce(cluster state_group)
			nlcom  (epop_elast_men:(1/_b[epop_men_mean:_cons])*(_b[epop_elast_nomw_men_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_men_mean:1.nomw#2.time])), post
			mat 	epop_elast_men = _b[epop_elast_men]\ _se[epop_elast_men]\_b[epop_elast_men]-1.96*_se[epop_elast_men]\_b[epop_elast_men]+1.96*_se[epop_elast_men]

			suest   emp_men emp_elast_nomw_men aw_elast_nomw_men, vce(cluster state_group)
			nlcom  (emp_elast_men:(1/_b[emp_men_mean:_cons])*(_b[emp_elast_nomw_men_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_men_mean:1.nomw#2.time])), post
			mat 	emp_elast_men = _b[emp_elast_men]\ _se[emp_elast_men]\_b[emp_elast_men]-1.96*_se[emp_elast_men]\_b[emp_elast_men]+1.96*_se[emp_elast_men]
			
			suest   ahours_elast_nomw_men aw_elast_nomw_men, vce(cluster state_group)
			nlcom  (ahours_elast_men:_b[ahours_elast_nomw_men_mean:1.nomw#2.time]/_b[aw_elast_nomw_men_mean:1.nomw#2.time]), post
			mat 	ahours_elast_men = _b[ahours_elast_men]\ _se[ahours_elast_men]\_b[ahours_elast_men]-1.96*_se[ahours_elast_men]\_b[ahours_elast_men]+1.96*_se[ahours_elast_men]

			
*>>>>>>>WOMEN	
	**MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 	
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) & sex==2 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo aw_nomw_women
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
		if inrange(age,25,55) & sex==2 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo epop_nomw_women
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	
	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & sex==2 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
	eststo emp_nomw_women
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**LN_HOURS
		reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & sex==2 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_nomw_women
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
	**EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) & sex==2 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
	eststo aw_elast_nomw_women	
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & sex==2 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
	eststo epop_elast_nomw_women

	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [iw=weight]  ///
		if inrange(age,25,55) & sex==2 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo emp_elast_nomw_women	
	
	**LN_HOURS
	reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & sex==2 & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo ahours_elast_nomw_women
  
  
	**ELASTICITIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & sex==2 & inlist(race,100,200) & state_group!=22
			eststo  epop_women
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & sex==2 & inlist(race,100,200) & state_group!=22
			eststo  emp_women
			
			*matrices elasticties		
			suest   epop_women epop_elast_nomw_women aw_elast_nomw_women, vce(cluster state_group)
			nlcom  (epop_elast_women:(1/_b[epop_women_mean:_cons])*(_b[epop_elast_nomw_women_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_women_mean:1.nomw#2.time])), post
			mat 	epop_elast_women = _b[epop_elast_women]\ _se[epop_elast_women]\_b[epop_elast_women]-1.96*_se[epop_elast_women]\_b[epop_elast_women]+1.96*_se[epop_elast_women]

			suest   emp_women emp_elast_nomw_women aw_elast_nomw_women, vce(cluster state_group)
			nlcom  (emp_elast_women:(1/_b[emp_women_mean:_cons])*(_b[emp_elast_nomw_women_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_women_mean:1.nomw#2.time])), post
			mat 	emp_elast_women = _b[emp_elast_women]\ _se[emp_elast_women]\_b[emp_elast_women]-1.96*_se[emp_elast_women]\_b[emp_elast_women]+1.96*_se[emp_elast_women]
			
			suest   ahours_elast_nomw_women aw_elast_nomw_women, vce(cluster state_group)
			nlcom  (ahours_elast_women:_b[ahours_elast_nomw_women_mean:1.nomw#2.time]/_b[aw_elast_nomw_women_mean:1.nomw#2.time]), post
			mat 	ahours_elast_women = _b[ahours_elast_women]\ _se[ahours_elast_women]\_b[ahours_elast_women]-1.96*_se[ahours_elast_women]\_b[ahours_elast_women]+1.96*_se[ahours_elast_women]
				

*>>>>>>>LOW-EDUCATION inrange(educ,010,071) & !inlist(educ,000,999))				
	**MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 	
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) & inrange(educ,010,071) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo aw_nomw_ls
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
		if inrange(age,25,55) &  inrange(educ,010,071) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo epop_nomw_ls
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	
	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) &  inrange(educ,010,071) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
	eststo emp_nomw_ls
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**LN_HOURS
		reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) &  inrange(educ,010,071) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_nomw_ls
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
	**EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) &  inrange(educ,010,071) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
	eststo aw_elast_nomw_ls	
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) &  inrange(educ,010,071) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
	eststo epop_elast_nomw_ls

	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [iw=weight]  ///
		if inrange(age,25,55) &  inrange(educ,010,071) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo emp_elast_nomw_ls	
	
	**LN_HOURS
	reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) &  inrange(educ,010,071) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo ahours_elast_nomw_ls
  
  
	**ELASTICITIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) &  inrange(educ,010,071) & !inlist(educ,000,999) & inlist(race,100,200) & state_group!=22
			eststo  epop_ls
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) &  inrange(educ,010,071) & !inlist(educ,000,999) & inlist(race,100,200) & state_group!=22
			eststo  emp_ls
			
			*matrices elasticties		
			suest   epop_ls epop_elast_nomw_ls aw_elast_nomw_ls, vce(cluster state_group)
			nlcom  (epop_elast_ls:(1/_b[epop_ls_mean:_cons])*(_b[epop_elast_nomw_ls_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_ls_mean:1.nomw#2.time])), post
			mat 	epop_elast_ls = _b[epop_elast_ls]\ _se[epop_elast_ls]\_b[epop_elast_ls]-1.96*_se[epop_elast_ls]\_b[epop_elast_ls]+1.96*_se[epop_elast_ls]

			suest   emp_ls emp_elast_nomw_ls aw_elast_nomw_ls, vce(cluster state_group)
			nlcom  (emp_elast_ls:(1/_b[emp_ls_mean:_cons])*(_b[emp_elast_nomw_ls_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_ls_mean:1.nomw#2.time])), post
			mat 	emp_elast_ls = _b[emp_elast_ls]\ _se[emp_elast_ls]\_b[emp_elast_ls]-1.96*_se[emp_elast_ls]\_b[emp_elast_ls]+1.96*_se[emp_elast_ls]
			
			suest   ahours_elast_nomw_ls aw_elast_nomw_ls, vce(cluster state_group)
			nlcom  (ahours_elast_ls:_b[ahours_elast_nomw_ls_mean:1.nomw#2.time]/_b[aw_elast_nomw_ls_mean:1.nomw#2.time]), post
			mat 	ahours_elast_ls = _b[ahours_elast_ls]\ _se[ahours_elast_ls]\_b[ahours_elast_ls]-1.96*_se[ahours_elast_ls]\_b[ahours_elast_ls]+1.96*_se[ahours_elast_ls]
				
*>>>>>>>HIGH-EDUCATION inrange(educ,072,125) & !inlist(educ,000,999))				
	**MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 	
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) & inrange(educ,072,125) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo aw_nomw_hs
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
		if inrange(age,25,55) &  inrange(educ,072,125) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo epop_nomw_hs
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	
	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) &  inrange(educ,072,125) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
	eststo emp_nomw_hs
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**LN_HOURS
		reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(educ,072,125) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_nomw_hs
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
	**EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
		if flag_employed & in_sample & inrange(age,25,55) &  inrange(educ,072,125) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
	eststo aw_elast_nomw_hs	
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inrange(educ,072,125) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
	eststo epop_elast_nomw_hs

	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [iw=weight]  ///
		if inrange(age,25,55) &  inrange(educ,072,125) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo emp_elast_nomw_hs	
	
	**LN_HOURS
	reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) &  inrange(educ,072,125) & !inlist(educ,000,999) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo ahours_elast_nomw_hs
  
  
	**ELASTICITIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) &  inrange(educ,072,125) & !inlist(educ,000,999) & inlist(race,100,200) & state_group!=22
			eststo  epop_hs
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) &  inrange(educ,072,125) & !inlist(educ,000,999) & inlist(race,100,200) & state_group!=22
			eststo  emp_hs
			
			*matrices elasticties		
			suest   epop_hs epop_elast_nomw_hs aw_elast_nomw_hs, vce(cluster state_group)
			nlcom  (epop_elast_hs:(1/_b[epop_hs_mean:_cons])*(_b[epop_elast_nomw_hs_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_hs_mean:1.nomw#2.time])), post
			mat 	epop_elast_hs = _b[epop_elast_hs]\ _se[epop_elast_hs]\_b[epop_elast_hs]-1.96*_se[epop_elast_hs]\_b[epop_elast_hs]+1.96*_se[epop_elast_hs]

			suest   emp_hs emp_elast_nomw_hs aw_elast_nomw_hs, vce(cluster state_group)
			nlcom  (emp_elast_hs:(1/_b[emp_hs_mean:_cons])*(_b[emp_elast_nomw_hs_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_hs_mean:1.nomw#2.time])), post
			mat 	emp_elast_hs = _b[emp_elast_hs]\ _se[emp_elast_hs]\_b[emp_elast_hs]-1.96*_se[emp_elast_hs]\_b[emp_elast_hs]+1.96*_se[emp_elast_hs]
			
			suest   ahours_elast_nomw_hs aw_elast_nomw_hs, vce(cluster state_group)
			nlcom  (ahours_elast_hs:_b[ahours_elast_nomw_hs_mean:1.nomw#2.time]/_b[aw_elast_nomw_hs_mean:1.nomw#2.time]), post
			mat 	ahours_elast_hs = _b[ahours_elast_hs]\ _se[ahours_elast_hs]\_b[ahours_elast_hs]-1.96*_se[ahours_elast_hs]\_b[ahours_elast_hs]+1.96*_se[ahours_elast_hs]
							
*>>>>>>>BY COHORT-- 16-30 YEARS OLD					
	**MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 	
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
		if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo aw_nomw_1630
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
		if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo epop_nomw_1630
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	
	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
	eststo emp_nomw_1630
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**LN_HOURS
		reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_nomw_1630
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
	**EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
		if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
	eststo aw_elast_nomw_1630	
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
	eststo epop_elast_nomw_1630

	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [iw=weight]  ///
		if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo emp_elast_nomw_1630	
	
	**LN_HOURS
	reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo ahours_elast_nomw_1630
  
  
	**ELASTICITIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,16,30) & inlist(race,100,200) & state_group!=22
			eststo  epop_1630
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,16,30) & inlist(race,100,200) & state_group!=22
			eststo  emp_1630
			
			*matrices elasticties		
			suest   epop_1630 epop_elast_nomw_1630 aw_elast_nomw_1630, vce(cluster state_group)
			nlcom  (epop_elast_1630:(1/_b[epop_1630_mean:_cons])*(_b[epop_elast_nomw_1630_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_1630_mean:1.nomw#2.time])), post
			mat 	epop_elast_1630 = _b[epop_elast_1630]\ _se[epop_elast_1630]\_b[epop_elast_1630]-1.96*_se[epop_elast_1630]\_b[epop_elast_1630]+1.96*_se[epop_elast_1630]

			suest   emp_1630 emp_elast_nomw_1630 aw_elast_nomw_1630, vce(cluster state_group)
			nlcom  (emp_elast_1630:(1/_b[emp_1630_mean:_cons])*(_b[emp_elast_nomw_1630_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_1630_mean:1.nomw#2.time])), post
			mat 	emp_elast_1630 = _b[emp_elast_1630]\ _se[emp_elast_1630]\_b[emp_elast_1630]-1.96*_se[emp_elast_1630]\_b[emp_elast_1630]+1.96*_se[emp_elast_1630]
			
			suest   ahours_elast_nomw_1630 aw_elast_nomw_1630, vce(cluster state_group)
			nlcom  (ahours_elast_1630:_b[ahours_elast_nomw_1630_mean:1.nomw#2.time]/_b[aw_elast_nomw_1630_mean:1.nomw#2.time]), post
			mat 	ahours_elast_1630 = _b[ahours_elast_1630]\ _se[ahours_elast_1630]\_b[ahours_elast_1630]-1.96*_se[ahours_elast_1630]\_b[ahours_elast_1630]+1.96*_se[ahours_elast_1630]
				
*>>>>>>>BY COHORT-- 16-64 YEARS OLD					
	**MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 	
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
		if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo aw_nomw_1664
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
		if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo epop_nomw_1664
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	
	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
	eststo emp_nomw_1664
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**LN_HOURS
		reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_nomw_1664
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
	**EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
		if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
	eststo aw_elast_nomw_1664	
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
	eststo epop_elast_nomw_1664

	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [iw=weight]  ///
		if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo emp_elast_nomw_1664	
	
	**LN_HOURS
	reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo ahours_elast_nomw_1664
  
  
	**ELASTICITIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,50,64) & inlist(race,100,200) & state_group!=22
			eststo  epop_1664
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,50,64) & inlist(race,100,200) & state_group!=22
			eststo  emp_1664
			
			*matrices elasticties		
			suest   epop_1664 epop_elast_nomw_1664 aw_elast_nomw_1664, vce(cluster state_group)
			nlcom  (epop_elast_1664:(1/_b[epop_1664_mean:_cons])*(_b[epop_elast_nomw_1664_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_1664_mean:1.nomw#2.time])), post
			mat 	epop_elast_1664 = _b[epop_elast_1664]\ _se[epop_elast_1664]\_b[epop_elast_1664]-1.96*_se[epop_elast_1664]\_b[epop_elast_1664]+1.96*_se[epop_elast_1664]

			suest   emp_1664 emp_elast_nomw_1664 aw_elast_nomw_1664, vce(cluster state_group)
			nlcom  (emp_elast_1664:(1/_b[emp_1664_mean:_cons])*(_b[emp_elast_nomw_1664_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_1664_mean:1.nomw#2.time])), post
			mat 	emp_elast_1664 = _b[emp_elast_1664]\ _se[emp_elast_1664]\_b[emp_elast_1664]-1.96*_se[emp_elast_1664]\_b[emp_elast_1664]+1.96*_se[emp_elast_1664]
			
			suest   ahours_elast_nomw_1664 aw_elast_nomw_1664, vce(cluster state_group)
			nlcom  (ahours_elast_1664:_b[ahours_elast_nomw_1664_mean:1.nomw#2.time]/_b[aw_elast_nomw_1664_mean:1.nomw#2.time]), post
			mat 	ahours_elast_1664 = _b[ahours_elast_1664]\ _se[ahours_elast_1664]\_b[ahours_elast_1664]-1.96*_se[ahours_elast_1664]\_b[ahours_elast_1664]+1.96*_se[ahours_elast_1664]

*>>>>>>>BY COHORT-- 50-64 YEARS OLD					
	**MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 	
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
		if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo aw_nomw_5064
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
		if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
		, robust cluster(state_group) 
	eststo epop_nomw_5064
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	
	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
	eststo emp_nomw_5064
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasstatefe 	"Y"			
	
	**LN_HOURS
		reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_nomw_5064
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
	**EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
	reg ln_annual_wage nomw_1966##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
		if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
	eststo aw_elast_nomw_5064	
	
	**EMPLOYMENT (EPOP)
	reg flag_employed_nilf nomw_1966##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
	eststo epop_elast_nomw_5064

	**EMPLOYMENT (1-UNEMP RATE)
	reg flag_employed_unemp nomw_1966##ib1.time_emp  i.sex i.race $covar_emp i.state_group [iw=weight]  ///
		if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo emp_elast_nomw_5064	
	
	**LN_HOURS
	reg ln_ahours nomw_1966##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	eststo ahours_elast_nomw_5064
  
  
	**ELASTICITIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,50,64) & inlist(race,100,200) & state_group!=22
			eststo  epop_5064
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,50,64) & inlist(race,100,200) & state_group!=22
			eststo  emp_5064
			
			*matrices elasticties		
			suest   epop_5064 epop_elast_nomw_5064 aw_elast_nomw_5064, vce(cluster state_group)
			nlcom  (epop_elast_5064:(1/_b[epop_5064_mean:_cons])*(_b[epop_elast_nomw_5064_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_5064_mean:1.nomw#2.time])), post
			mat 	epop_elast_5064 = _b[epop_elast_5064]\ _se[epop_elast_5064]\_b[epop_elast_5064]-1.96*_se[epop_elast_5064]\_b[epop_elast_5064]+1.96*_se[epop_elast_5064]

			suest   emp_5064 emp_elast_nomw_5064 aw_elast_nomw_5064, vce(cluster state_group)
			nlcom  (emp_elast_5064:(1/_b[emp_5064_mean:_cons])*(_b[emp_elast_nomw_5064_mean:1.nomw#2.time_emp]/_b[aw_elast_nomw_5064_mean:1.nomw#2.time])), post
			mat 	emp_elast_5064 = _b[emp_elast_5064]\ _se[emp_elast_5064]\_b[emp_elast_5064]-1.96*_se[emp_elast_5064]\_b[emp_elast_5064]+1.96*_se[emp_elast_5064]
			
			suest   ahours_elast_nomw_5064 aw_elast_nomw_5064, vce(cluster state_group)
			nlcom  (ahours_elast_5064:_b[ahours_elast_nomw_5064_mean:1.nomw#2.time]/_b[aw_elast_nomw_5064_mean:1.nomw#2.time]), post
			mat 	ahours_elast_5064 = _b[ahours_elast_5064]\ _se[ahours_elast_5064]\_b[ahours_elast_5064]-1.96*_se[ahours_elast_5064]\_b[ahours_elast_5064]+1.96*_se[ahours_elast_5064]
				
  **MATRICES ELASTICTIES 
			mat 		 epop_elast_nomw = (epop_elast_all,epop_elast_black,epop_elast_white,epop_elast_men,epop_elast_women,epop_elast_ls,epop_elast_hs)
			mat rownames epop_elast_nomw ="\rule{0pt}{3ex}\textbf{Emp. (vs.unemp/nilf) elast.}" "se" "lower bound" "upper bound"
		   
		   mat 			emp_elast_nomw = (emp_elast_all,emp_elast_black,emp_elast_white,emp_elast_men,emp_elast_women,emp_elast_ls,emp_elast_hs)
		   mat rownames emp_elast_nomw ="\rule{0pt}{3ex}\textbf{Emp. (vs. unemp.) elasticity}" "se" "lower bound" "upper bound"
	
		   mat 			ahours_elast_nomw = (ahours_elast_all,ahours_elast_black,ahours_elast_white,ahours_elast_men,ahours_elast_women,ahours_elast_ls,ahours_elast_hs)
		   mat rownames ahours_elast_nomw ="\rule{0pt}{3ex}\textbf{Annual Hours elast.}" "se" "lower bound" "upper bound"
				
	
  **OUTPUT TABLES 	
		esttab 	aw_nomw_all aw_nomw_black aw_nomw_white aw_nomw_men aw_nomw_women aw_nomw_ls aw_nomw_hs ///
				using "tables/table_emp_nomw.tex", replace  label fragment ///
				nolines  posthead(\cmidrule{2-8}) booktabs ///
				nonumbers mtitle("All" "Black" "White" "Men" "Women" "Low-educ." "High-educ") collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(1.nomw_1966#2.time)   ///				
				refcat(1.nomw_1966#2.time "Strongly treated states $\times$& & & \\ \hspace{0.5cm}{1967-1972}", nolabel) ///
				coeflabel(1.nomw_1966#2.time "\rule{0pt}{3ex}{Earnings}") noobs onecell 
		esttab 	emp_nomw_all emp_nomw_black emp_nomw_white emp_nomw_men emp_nomw_women emp_nomw_ls emp_nomw_hs ///
				using "tables/table_emp_nomw.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(1.nomw_1966#2.time_emp)   ///				
				coeflabel(1.nomw_1966#2.time_emp "\rule{0pt}{3ex}{Emp. (vs. unemp.)}") noobs onecell		
		esttab 	epop_nomw_all epop_nomw_black epop_nomw_white epop_nomw_men epop_nomw_women epop_nomw_ls epop_nomw_hs ///
				using "tables/table_emp_nomw.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(1.nomw_1966#2.time_emp)   ///				
				coeflabel(1.nomw_1966#2.time_emp "\rule{0pt}{3ex}{Emp. (vs. unemp/nilf)}") noobs onecell		
		esttab 	ahours_nomw_all ahours_nomw_black ahours_nomw_white ahours_nomw_men ahours_nomw_women ahours_nomw_ls ahours_nomw_hs ///
				using "tables/table_emp_nomw.tex", append  label fragment ///
				nolines  prefoot(\midrule) postfoot(\bottomrule) booktabs ///
				nonumbers nomtitles collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(1.nomw_1966#2.time)   ///				
				coeflabel(1.nomw_1966#2.time "\rule{0pt}{3ex}{Annual Hours}") ///
				stats(N hascontrols hastimefe hasstatefe, ///
				fmt(%11.0gc) label("Observations" "Controls" "Time FE" "State FE")) onecell 		
		esttab  matrix(emp_elast_nomw,fmt(%3.2f)) ///
				using "tables/table_emp_nomw.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none) 	
		esttab  matrix(epop_elast_nomw,fmt(%3.2f)) ///
				using "tables/table_emp_nomw.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none) 
		esttab  matrix(ahours_elast_nomw,fmt(%3.2f)) ///
				using "tables/table_emp_nomw.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none) postfoot(\bottomrule \bottomrule) 

*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*TABLE E4 [SLIDES ONLY]: EFFECT OF 1967 REFORM ON PROBABILITY OF BEING EMPLOYED VS. UNEMPLOYED OR NILF  (SHORT VERSION OF THE ABOVE)
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
			*i. Adapt elasticity results to slides 
			mat elast_nilf_slides = (epop_elast_all_slides, epop_elast_black_slides, epop_elast_white_slides)
			mat rownames elast_nilf_slides = "\rule{0pt}{3ex}\textbf{Elast.}" "se"
		   			
			*ii. Output table 
			esttab 	epop_nomw_all epop_nomw_black epop_nomw_white  ///
					using "tables/table_emp_nomw_slides.tex", replace  label fragment ///
					nolines  posthead(\cmidrule{2-4}) booktabs ///
					nonumbers mtitle("All" "Black" "White" ) collabels(none)    ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					keep(1.nomw_1966#2.time_emp)   ///				
					refcat(1.nomw_1966#2.time_emp "Strongly treated states $\times$& & & \\ \hspace{0.2cm}{1967-1972}", nolabel) ///
					coeflabel(1.nomw_1966#2.time_emp "\textbf{Employment}") ///
					stats(N, fmt(%11.0gc) label("{}")) onecell 							
			esttab 	aw_nomw_all aw_nomw_black aw_nomw_white ///
					using "tables/table_emp_nomw_slides.tex", append  label fragment ///
					nolines nonumbers nomtitles collabels(none)  ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					keep(1.nomw_1966#2.time)   ///				
					coeflabel(1.nomw_1966#2.time "\rule{0pt}{3ex}\textbf{Earnings}") ///
					stats(N, fmt(%11.0gc) label("{}")) onecell 											
			esttab	matrix(elast_nilf_slides,fmt(%3.2f)) ///
					using "tables/table_emp_nomw_slides.tex", append  label fragment ///
					nolines booktabs ///
					nonumbers nomtitles collabels(none) postfoot(\bottomrule) 					
	
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*TABLE E5 [APPENDIX]: EFFECT OF 1967 REFORM USING 1966 KAITZ INDEX
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*>>>>>>>ALL					
	*USE STANDARDIZED MEASURE OF THE TREATMENT VARIABLE
	cap drop KI_1966S_aw_std KI_1966S_emp_std
	egen KI_1966S_aw_std = std(KI_1966S) if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen KI_1966S_emp_std = std(KI_1966S) if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	**MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_KI_1966S_all
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_KI_1966S_all
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_KI_1966S_all
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_KI_1966S_all
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_KI_1966S_all		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_KI_1966S_all

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_KI_1966S_all

	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_KI_1966S_all
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  epop_all
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  emp_all
			
			*matrices elasticties		
			suest   epop_all epop_elast_KI_1966S_all aw_elast_KI_1966S_all, vce(cluster state_group)
			nlcom  (epop_elast_all:(1/_b[epop_all_mean:_cons])*(_b[epop_elast_KI_1966S_all_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_all_mean:2.time#KI_1966S_aw_std])), post
			mat 	epop_elast_all = _b[epop_elast_all]\ _se[epop_elast_all]\_b[epop_elast_all]-1.96*_se[epop_elast_all]\_b[epop_elast_all]+1.96*_se[epop_elast_all]

			suest   emp_all emp_elast_KI_1966S_all aw_elast_KI_1966S_all, vce(cluster state_group)
			nlcom  (emp_elast_all:(1/_b[emp_all_mean:_cons])*(_b[emp_elast_KI_1966S_all_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_all_mean:2.time#KI_1966S_aw_std])), post
			mat 	emp_elast_all = _b[emp_elast_all]\ _se[emp_elast_all]\_b[emp_elast_all]-1.96*_se[emp_elast_all]\_b[emp_elast_all]+1.96*_se[emp_elast_all]
			
			suest   ahours_elast_KI_1966S_all aw_elast_KI_1966S_all, vce(cluster state_group)
			nlcom  (ahours_elast_all:_b[ahours_elast_KI_1966S_all_mean:2.time#KI_1966S_aw_std]/_b[aw_elast_KI_1966S_all_mean:2.time#KI_1966S_aw_std]), post
			mat 	ahours_elast_all = _b[ahours_elast_all]\ _se[ahours_elast_all]\_b[ahours_elast_all]-1.96*_se[ahours_elast_all]\_b[ahours_elast_all]+1.96*_se[ahours_elast_all]
				
*>>>>>>>BLACK PERSONS				
	*USE STANDARDIZED MEASURE OF THE TREATMENT VARIABLE
	drop KI_1966S_aw_std KI_1966S_emp_std 
	egen KI_1966S_aw_std = std(KI_1966S) if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen KI_1966S_emp_std = std(KI_1966S) if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex $covar i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_KI_1966S_black
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex  $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_KI_1966S_black
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex  $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_KI_1966S_black
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_KI_1966S_black
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex  $covar i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_KI_1966S_black		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_KI_1966S_black

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex  $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_KI_1966S_black

	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex  $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_KI_1966S_black
	
  **ELASTICITIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,200) & state_group!=22
			eststo  epop_black
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,200) & state_group!=22
			eststo  emp_black
			
			*matrices elasticties		
			suest   epop_black epop_elast_KI_1966S_black aw_elast_KI_1966S_black, vce(cluster state_group)
			nlcom  (epop_elast_black:(1/_b[epop_black_mean:_cons])*(_b[epop_elast_KI_1966S_black_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_black_mean:2.time#KI_1966S_aw_std])), post
			mat 	epop_elast_black = _b[epop_elast_black]\ _se[epop_elast_black]\_b[epop_elast_black]-1.96*_se[epop_elast_black]\_b[epop_elast_black]+1.96*_se[epop_elast_black]

			suest   emp_black emp_elast_KI_1966S_black aw_elast_KI_1966S_black, vce(cluster state_group)
			nlcom  (emp_elast_black:(1/_b[emp_black_mean:_cons])*(_b[emp_elast_KI_1966S_black_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_black_mean:2.time#KI_1966S_aw_std])), post
			mat 	emp_elast_black = _b[emp_elast_black]\ _se[emp_elast_black]\_b[emp_elast_black]-1.96*_se[emp_elast_black]\_b[emp_elast_black]+1.96*_se[emp_elast_black]
			
			suest   ahours_elast_KI_1966S_black aw_elast_KI_1966S_black, vce(cluster state_group)
			nlcom  (ahours_elast_black:_b[ahours_elast_KI_1966S_black_mean:2.time#KI_1966S_aw_std]/_b[aw_elast_KI_1966S_black_mean:2.time#KI_1966S_aw_std]), post
			mat 	ahours_elast_black = _b[ahours_elast_black]\ _se[ahours_elast_black]\_b[ahours_elast_black]-1.96*_se[ahours_elast_black]\_b[ahours_elast_black]+1.96*_se[ahours_elast_black]

*>>>>>>>WHITE PERSONS				
  *USE STANDARDIZED MEASURE OF THE TREATMENT VARIABLE
	drop KI_1966S_aw_std KI_1966S_emp_std 
	egen KI_1966S_aw_std 	= std(KI_1966S) if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen KI_1966S_emp_std  	= std(KI_1966S) if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex $covar i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_KI_1966S_white
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex  $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_KI_1966S_white
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex  $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_KI_1966S_white
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_KI_1966S_white
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex  $covar i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_KI_1966S_white		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_KI_1966S_white

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex  $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_KI_1966S_white

	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex  $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_KI_1966S_white
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100) & state_group!=22
			eststo  epop_white
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100) & state_group!=22
			eststo  emp_white
			
			*matrices elasticties		
			suest   epop_white epop_elast_KI_1966S_white aw_elast_KI_1966S_white, vce(cluster state_group)
			nlcom  (epop_elast_white:(1/_b[epop_white_mean:_cons])*(_b[epop_elast_KI_1966S_white_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_white_mean:2.time#KI_1966S_aw_std])), post
			mat 	epop_elast_white = _b[epop_elast_white]\ _se[epop_elast_white]\_b[epop_elast_white]-1.96*_se[epop_elast_white]\_b[epop_elast_white]+1.96*_se[epop_elast_white]

			suest   emp_white emp_elast_KI_1966S_white aw_elast_KI_1966S_white, vce(cluster state_group)
			nlcom  (emp_elast_white:(1/_b[emp_white_mean:_cons])*(_b[emp_elast_KI_1966S_white_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_white_mean:2.time#KI_1966S_aw_std])), post
			mat 	emp_elast_white = _b[emp_elast_white]\ _se[emp_elast_white]\_b[emp_elast_white]-1.96*_se[emp_elast_white]\_b[emp_elast_white]+1.96*_se[emp_elast_white]
			
			suest   ahours_elast_KI_1966S_white aw_elast_KI_1966S_white, vce(cluster state_group)
			nlcom  (ahours_elast_white:_b[ahours_elast_KI_1966S_white_mean:2.time#KI_1966S_aw_std]/_b[aw_elast_KI_1966S_white_mean:2.time#KI_1966S_aw_std]), post
			mat 	ahours_elast_white = _b[ahours_elast_white]\ _se[ahours_elast_white]\_b[ahours_elast_white]-1.96*_se[ahours_elast_white]\_b[ahours_elast_white]+1.96*_se[ahours_elast_white]

*>>>>>>>MEN			
	drop KI_1966S_aw_std KI_1966S_emp_std 
	egen KI_1966S_aw_std 	= std(KI_1966S) if sex==1 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen KI_1966S_emp_std  	= std(KI_1966S) if sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.race $covar i.state_group [pw=weight] ///
			if sex==1 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_KI_1966S_men
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [pw=weight]  ///
			if sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_KI_1966S_men
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [pw=weight]  ///
			if sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_KI_1966S_men
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time  i.race $covar_nohnow i.state_group [pw=weight] ///
			if sex==1 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_KI_1966S_men
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time  i.race $covar i.state_group [iw=weight] ///
			if sex==1 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_KI_1966S_men		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [iw=weight]  ///
			if sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_KI_1966S_men

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [iw=weight]  ///
			if sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_KI_1966S_men

	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.race $covar_nohnow i.state_group [iw=weight] ///
			if sex==1 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_KI_1966S_men
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & sex==1 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  epop_men
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & sex==1  & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  emp_men
			
			*matrices elasticties		
			suest   epop_men epop_elast_KI_1966S_men aw_elast_KI_1966S_men, vce(cluster state_group)
			nlcom  (epop_elast_men:(1/_b[epop_men_mean:_cons])*(_b[epop_elast_KI_1966S_men_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_men_mean:2.time#KI_1966S_aw_std])), post
			mat 	epop_elast_men = _b[epop_elast_men]\ _se[epop_elast_men]\_b[epop_elast_men]-1.96*_se[epop_elast_men]\_b[epop_elast_men]+1.96*_se[epop_elast_men]

			suest   emp_men emp_elast_KI_1966S_men aw_elast_KI_1966S_men, vce(cluster state_group)
			nlcom  (emp_elast_men:(1/_b[emp_men_mean:_cons])*(_b[emp_elast_KI_1966S_men_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_men_mean:2.time#KI_1966S_aw_std])), post
			mat 	emp_elast_men = _b[emp_elast_men]\ _se[emp_elast_men]\_b[emp_elast_men]-1.96*_se[emp_elast_men]\_b[emp_elast_men]+1.96*_se[emp_elast_men]
			
			suest   ahours_elast_KI_1966S_men aw_elast_KI_1966S_men, vce(cluster state_group)
			nlcom  (ahours_elast_men:_b[ahours_elast_KI_1966S_men_mean:2.time#KI_1966S_aw_std]/_b[aw_elast_KI_1966S_men_mean:2.time#KI_1966S_aw_std]), post
			mat 	ahours_elast_men = _b[ahours_elast_men]\ _se[ahours_elast_men]\_b[ahours_elast_men]-1.96*_se[ahours_elast_men]\_b[ahours_elast_men]+1.96*_se[ahours_elast_men]
												
*>>>>>>>WOMEN			
	drop KI_1966S_aw_std KI_1966S_emp_std 
	egen KI_1966S_aw_std 	= std(KI_1966S) if sex==2 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen KI_1966S_emp_std  	= std(KI_1966S) if sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.race $covar i.state_group [pw=weight] ///
			if sex==2 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_KI_1966S_women
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [pw=weight]  ///
			if sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_KI_1966S_women
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [pw=weight]  ///
			if sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_KI_1966S_women
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time  i.race $covar_nohnow i.state_group [pw=weight] ///
			if sex==2 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_KI_1966S_women
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time  i.race $covar i.state_group [iw=weight] ///
			if sex==2 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_KI_1966S_women		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [iw=weight]  ///
			if sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_KI_1966S_women

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [iw=weight]  ///
			if sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_KI_1966S_women

	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.race $covar_nohnow i.state_group [iw=weight] ///
			if sex==2 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_KI_1966S_women
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & sex==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  epop_women
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & sex==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  emp_women
			
			*matrices elasticties		
			suest   epop_women epop_elast_KI_1966S_women aw_elast_KI_1966S_women, vce(cluster state_group)
			nlcom  (epop_elast_women:(1/_b[epop_women_mean:_cons])*(_b[epop_elast_KI_1966S_women_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_women_mean:2.time#KI_1966S_aw_std])), post
			mat 	epop_elast_women = _b[epop_elast_women]\ _se[epop_elast_women]\_b[epop_elast_women]-1.96*_se[epop_elast_women]\_b[epop_elast_women]+1.96*_se[epop_elast_women]

			suest   emp_women emp_elast_KI_1966S_women aw_elast_KI_1966S_women, vce(cluster state_group)
			nlcom  (emp_elast_women:(1/_b[emp_women_mean:_cons])*(_b[emp_elast_KI_1966S_women_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_women_mean:2.time#KI_1966S_aw_std])), post
			mat 	emp_elast_women = _b[emp_elast_women]\ _se[emp_elast_women]\_b[emp_elast_women]-1.96*_se[emp_elast_women]\_b[emp_elast_women]+1.96*_se[emp_elast_women]
			
			suest   ahours_elast_KI_1966S_women aw_elast_KI_1966S_women, vce(cluster state_group)
			nlcom  (ahours_elast_women:_b[ahours_elast_KI_1966S_women_mean:2.time#KI_1966S_aw_std]/_b[aw_elast_KI_1966S_women_mean:2.time#KI_1966S_aw_std]), post
			mat 	ahours_elast_women = _b[ahours_elast_women]\ _se[ahours_elast_women]\_b[ahours_elast_women]-1.96*_se[ahours_elast_women]\_b[ahours_elast_women]+1.96*_se[ahours_elast_women]

*>>>>>>>LOW-EDUCATION (inrange(educ,010,071) & !inlist(educ,000,999))				
	drop KI_1966S_aw_std KI_1966S_emp_std 
	egen KI_1966S_aw_std 	= std(KI_1966S) if inrange(educ,010,071) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen KI_1966S_emp_std  	= std(KI_1966S) if inrange(educ,010,071) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_KI_1966S_ls
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_KI_1966S_ls
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_KI_1966S_ls
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_KI_1966S_ls
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_KI_1966S_ls		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_KI_1966S_ls

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_KI_1966S_ls

	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_KI_1966S_ls
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if inrange(educ,010,071) & !inlist(educ,000,999) & time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  epop_ls
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if inrange(educ,010,071) & !inlist(educ,000,999) & time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  emp_ls
			
			*matrices elasticties		
			suest   epop_ls epop_elast_KI_1966S_ls aw_elast_KI_1966S_ls, vce(cluster state_group)
			nlcom  (epop_elast_ls:(1/_b[epop_ls_mean:_cons])*(_b[epop_elast_KI_1966S_ls_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_ls_mean:2.time#KI_1966S_aw_std])), post
			mat 	epop_elast_ls = _b[epop_elast_ls]\ _se[epop_elast_ls]\_b[epop_elast_ls]-1.96*_se[epop_elast_ls]\_b[epop_elast_ls]+1.96*_se[epop_elast_ls]

			suest   emp_ls emp_elast_KI_1966S_ls aw_elast_KI_1966S_ls, vce(cluster state_group)
			nlcom  (emp_elast_ls:(1/_b[emp_ls_mean:_cons])*(_b[emp_elast_KI_1966S_ls_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_ls_mean:2.time#KI_1966S_aw_std])), post
			mat 	emp_elast_ls = _b[emp_elast_ls]\ _se[emp_elast_ls]\_b[emp_elast_ls]-1.96*_se[emp_elast_ls]\_b[emp_elast_ls]+1.96*_se[emp_elast_ls]
			
			suest   ahours_elast_KI_1966S_ls aw_elast_KI_1966S_ls, vce(cluster state_group)
			nlcom  (ahours_elast_ls:_b[ahours_elast_KI_1966S_ls_mean:2.time#KI_1966S_aw_std]/_b[aw_elast_KI_1966S_ls_mean:2.time#KI_1966S_aw_std]), post
			mat 	ahours_elast_ls = _b[ahours_elast_ls]\ _se[ahours_elast_ls]\_b[ahours_elast_ls]-1.96*_se[ahours_elast_ls]\_b[ahours_elast_ls]+1.96*_se[ahours_elast_ls]
							
*>>>>>>>HIGH-EDUCATION (inrange(educ,072,125) & !inlist(educ,000,999))				
 	drop KI_1966S_aw_std KI_1966S_emp_std 
	egen KI_1966S_aw_std 	= std(KI_1966S) if inrange(educ,072,125) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen KI_1966S_emp_std  	= std(KI_1966S) if inrange(educ,072,125) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
 **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_KI_1966S_hs
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_KI_1966S_hs
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_KI_1966S_hs
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_KI_1966S_hs
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_KI_1966S_hs		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_KI_1966S_hs

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_KI_1966S_hs

	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_KI_1966S_hs
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if inrange(educ,072,125) & !inlist(educ,000,999) & time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  epop_hs
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if inrange(educ,072,125) & !inlist(educ,000,999) & time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  emp_hs
			
			*matrices elasticties		
			suest   epop_hs epop_elast_KI_1966S_hs aw_elast_KI_1966S_hs, vce(cluster state_group)
			nlcom  (epop_elast_hs:(1/_b[epop_hs_mean:_cons])*(_b[epop_elast_KI_1966S_hs_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_hs_mean:2.time#KI_1966S_aw_std])), post
			mat 	epop_elast_hs = _b[epop_elast_hs]\ _se[epop_elast_hs]\_b[epop_elast_hs]-1.96*_se[epop_elast_hs]\_b[epop_elast_hs]+1.96*_se[epop_elast_hs]

			suest   emp_hs emp_elast_KI_1966S_hs aw_elast_KI_1966S_hs, vce(cluster state_group)
			nlcom  (emp_elast_hs:(1/_b[emp_hs_mean:_cons])*(_b[emp_elast_KI_1966S_hs_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_hs_mean:2.time#KI_1966S_aw_std])), post
			mat 	emp_elast_hs = _b[emp_elast_hs]\ _se[emp_elast_hs]\_b[emp_elast_hs]-1.96*_se[emp_elast_hs]\_b[emp_elast_hs]+1.96*_se[emp_elast_hs]
			
			suest   ahours_elast_KI_1966S_hs aw_elast_KI_1966S_hs, vce(cluster state_group)
			nlcom  (ahours_elast_hs:_b[ahours_elast_KI_1966S_hs_mean:2.time#KI_1966S_aw_std]/_b[aw_elast_KI_1966S_hs_mean:2.time#KI_1966S_aw_std]), post
			mat 	ahours_elast_hs = _b[ahours_elast_hs]\ _se[ahours_elast_hs]\_b[ahours_elast_hs]-1.96*_se[ahours_elast_hs]\_b[ahours_elast_hs]+1.96*_se[ahours_elast_hs]

			
*>>>>>>>BY COHORT-- 16-30 YEARS OLD					
 	drop KI_1966S_aw_std KI_1966S_emp_std 
	egen KI_1966S_aw_std 	= std(KI_1966S) if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen KI_1966S_emp_std  	= std(KI_1966S) if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_KI_1966S_1630
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_KI_1966S_1630
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_KI_1966S_1630
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_KI_1966S_1630
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_KI_1966S_1630		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_KI_1966S_1630

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_KI_1966S_1630

	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_KI_1966S_1630
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,16,30) & inlist(race,100,200) & state_group!=22
			eststo  epop_1630
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,16,30) & inlist(race,100,200) & state_group!=22
			eststo  emp_1630
			
			*matrices elasticties		
			suest   epop_1630 epop_elast_KI_1966S_1630 aw_elast_KI_1966S_1630, vce(cluster state_group)
			nlcom  (epop_elast_1630:(1/_b[epop_1630_mean:_cons])*(_b[epop_elast_KI_1966S_1630_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_1630_mean:2.time#KI_1966S_aw_std])), post
			mat 	epop_elast_1630 = _b[epop_elast_1630]\ _se[epop_elast_1630]\_b[epop_elast_1630]-1.96*_se[epop_elast_1630]\_b[epop_elast_1630]+1.96*_se[epop_elast_1630]

			suest   emp_1630 emp_elast_KI_1966S_1630 aw_elast_KI_1966S_1630, vce(cluster state_group)
			nlcom  (emp_elast_1630:(1/_b[emp_1630_mean:_cons])*(_b[emp_elast_KI_1966S_1630_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_1630_mean:2.time#KI_1966S_aw_std])), post
			mat 	emp_elast_1630 = _b[emp_elast_1630]\ _se[emp_elast_1630]\_b[emp_elast_1630]-1.96*_se[emp_elast_1630]\_b[emp_elast_1630]+1.96*_se[emp_elast_1630]
			
			suest   ahours_elast_KI_1966S_1630 aw_elast_KI_1966S_1630, vce(cluster state_group)
			nlcom  (ahours_elast_1630:_b[ahours_elast_KI_1966S_1630_mean:2.time#KI_1966S_aw_std]/_b[aw_elast_KI_1966S_1630_mean:2.time#KI_1966S_aw_std]), post
			mat 	ahours_elast_1630 = _b[ahours_elast_1630]\ _se[ahours_elast_1630]\_b[ahours_elast_1630]-1.96*_se[ahours_elast_1630]\_b[ahours_elast_1630]+1.96*_se[ahours_elast_1630]
				
*>>>>>>>BY COHORT-- 16-64 YEARS OLD					
 	drop KI_1966S_aw_std KI_1966S_emp_std 
	egen KI_1966S_aw_std 	= std(KI_1966S) if flag_employed & in_sample & inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen KI_1966S_emp_std  	= std(KI_1966S) if inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_KI_1966S_1664
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_KI_1966S_1664
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_KI_1966S_1664
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_KI_1966S_1664
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_KI_1966S_1664		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_KI_1966S_1664

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_KI_1966S_1664

	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_KI_1966S_1664
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,16,64) & inlist(race,100,200) & state_group!=22
			eststo  epop_1664
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,16,64) & inlist(race,100,200) & state_group!=22
			eststo  emp_1664
			
			*matrices elasticties		
			suest   epop_1664 epop_elast_KI_1966S_1664 aw_elast_KI_1966S_1664, vce(cluster state_group)
			nlcom  (epop_elast_1664:(1/_b[epop_1664_mean:_cons])*(_b[epop_elast_KI_1966S_1664_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_1664_mean:2.time#KI_1966S_aw_std])), post
			mat 	epop_elast_1664 = _b[epop_elast_1664]\ _se[epop_elast_1664]\_b[epop_elast_1664]-1.96*_se[epop_elast_1664]\_b[epop_elast_1664]+1.96*_se[epop_elast_1664]

			suest   emp_1664 emp_elast_KI_1966S_1664 aw_elast_KI_1966S_1664, vce(cluster state_group)
			nlcom  (emp_elast_1664:(1/_b[emp_1664_mean:_cons])*(_b[emp_elast_KI_1966S_1664_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_1664_mean:2.time#KI_1966S_aw_std])), post
			mat 	emp_elast_1664 = _b[emp_elast_1664]\ _se[emp_elast_1664]\_b[emp_elast_1664]-1.96*_se[emp_elast_1664]\_b[emp_elast_1664]+1.96*_se[emp_elast_1664]
			
			suest   ahours_elast_KI_1966S_1664 aw_elast_KI_1966S_1664, vce(cluster state_group)
			nlcom  (ahours_elast_1664:_b[ahours_elast_KI_1966S_1664_mean:2.time#KI_1966S_aw_std]/_b[aw_elast_KI_1966S_1664_mean:2.time#KI_1966S_aw_std]), post
			mat 	ahours_elast_1664 = _b[ahours_elast_1664]\ _se[ahours_elast_1664]\_b[ahours_elast_1664]-1.96*_se[ahours_elast_1664]\_b[ahours_elast_1664]+1.96*_se[ahours_elast_1664]
															
*>>>>>>>BY COHORT-- 50-64 YEARS OLD																
	drop KI_1966S_aw_std KI_1966S_emp_std 
	egen KI_1966S_aw_std 	= std(KI_1966S) if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen KI_1966S_emp_std  	= std(KI_1966S) if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
 **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_KI_1966S_5064
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_KI_1966S_5064
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_KI_1966S_5064
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_KI_1966S_5064
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.KI_1966S_aw_std##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_KI_1966S_5064		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_KI_1966S_5064

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.KI_1966S_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_KI_1966S_5064

	**LN_HOURS
		reg ln_ahours c.KI_1966S_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_KI_1966S_5064
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,50,64) & inlist(race,100,200) & state_group!=22
			eststo  epop_5064
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,50,64) & inlist(race,100,200) & state_group!=22
			eststo  emp_5064
			
			*matrices elasticties		
			suest   epop_5064 epop_elast_KI_1966S_5064 aw_elast_KI_1966S_5064, vce(cluster state_group)
			nlcom  (epop_elast_5064:(1/_b[epop_5064_mean:_cons])*(_b[epop_elast_KI_1966S_5064_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_5064_mean:2.time#KI_1966S_aw_std])), post
			mat 	epop_elast_5064 = _b[epop_elast_5064]\ _se[epop_elast_5064]\_b[epop_elast_5064]-1.96*_se[epop_elast_5064]\_b[epop_elast_5064]+1.96*_se[epop_elast_5064]

			suest   emp_5064 emp_elast_KI_1966S_5064 aw_elast_KI_1966S_5064, vce(cluster state_group)
			nlcom  (emp_elast_5064:(1/_b[emp_5064_mean:_cons])*(_b[emp_elast_KI_1966S_5064_mean:2.time_emp#KI_1966S_emp_std]/_b[aw_elast_KI_1966S_5064_mean:2.time#KI_1966S_aw_std])), post
			mat 	emp_elast_5064 = _b[emp_elast_5064]\ _se[emp_elast_5064]\_b[emp_elast_5064]-1.96*_se[emp_elast_5064]\_b[emp_elast_5064]+1.96*_se[emp_elast_5064]
			
			suest   ahours_elast_KI_1966S_5064 aw_elast_KI_1966S_5064, vce(cluster state_group)
			nlcom  (ahours_elast_5064:_b[ahours_elast_KI_1966S_5064_mean:2.time#KI_1966S_aw_std]/_b[aw_elast_KI_1966S_5064_mean:2.time#KI_1966S_aw_std]), post
			mat 	ahours_elast_5064 = _b[ahours_elast_5064]\ _se[ahours_elast_5064]\_b[ahours_elast_5064]-1.96*_se[ahours_elast_5064]\_b[ahours_elast_5064]+1.96*_se[ahours_elast_5064]
																
							
  **MATRICES ELASTICTIES 
			mat 		 epop_elast_KI_1966S = (epop_elast_all,epop_elast_black,epop_elast_white,epop_elast_men,epop_elast_women,epop_elast_ls,epop_elast_hs)
			mat rownames epop_elast_KI_1966S ="\rule{0pt}{3ex}\textbf{Emp. (vs. unemp/nilf) elast.}" "se" "lower bound" "upper bound"
		   
		   mat 			emp_elast_KI_1966S = (emp_elast_all,emp_elast_black,emp_elast_white,emp_elast_men,emp_elast_women,emp_elast_ls,emp_elast_hs)
		   mat rownames emp_elast_KI_1966S ="\rule{0pt}{3ex}\textbf{Emp. (vs. unemp) elast.}" "se" "lower bound" "upper bound"
	
		   mat 			ahours_elast_KI_1966S = (ahours_elast_all,ahours_elast_black,ahours_elast_white,ahours_elast_men,ahours_elast_women,ahours_elast_ls,ahours_elast_hs)
		   mat rownames ahours_elast_KI_1966S ="\rule{0pt}{3ex}\textbf{Annual Hours elast.}" "se" "lower bound" "upper bound"
				
	
  **OUTPUT TABLES 	
		esttab 	aw_KI_1966S_all aw_KI_1966S_black aw_KI_1966S_white aw_KI_1966S_men aw_KI_1966S_women aw_KI_1966S_ls aw_KI_1966S_hs ///
				using "tables/table_emp_KI_1966S.tex", replace  label fragment ///
				nolines  posthead(\cmidrule{2-8}) booktabs ///
				nonumbers mtitle("All" "Black" "White" "Men" "Women" "Low-educ." "High-educ") collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(2.time#c.KI_1966S_aw_std)   ///				
				refcat(2.time#c.KI_1966S_aw_std "1966 Kaitz Index $\times$& & & \\ \hspace{0.5cm}{1967-1972}", nolabel) ///
				coeflabel(2.time#c.KI_1966S_aw_std "\rule{0pt}{3ex}{Earnings}") noobs onecell 	
		esttab 	emp_KI_1966S_all emp_KI_1966S_black emp_KI_1966S_white emp_KI_1966S_men emp_KI_1966S_women emp_KI_1966S_ls emp_KI_1966S_hs ///
				using "tables/table_emp_KI_1966S.tex", append  label fragment ///
				nolines  booktabs ///
				nonumbers nomtitles collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(2.time_emp#c.KI_1966S_emp_std)   ///				
				coeflabel(2.time_emp#c.KI_1966S_emp_std "\rule{0pt}{3ex}{Emp. (vs. unemp.)}") noobs onecell		
		esttab 	epop_KI_1966S_all epop_KI_1966S_black epop_KI_1966S_white epop_KI_1966S_men epop_KI_1966S_women epop_KI_1966S_ls epop_KI_1966S_hs ///
				using "tables/table_emp_KI_1966S.tex", append  label fragment ///
				nolines  booktabs ///
				nonumbers nomtitles collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(2.time_emp#c.KI_1966S_emp_std)   ///				
				coeflabel(2.time_emp#c.KI_1966S_emp_std "\rule{0pt}{3ex}{Emp. (vs. unemp/nilf)}") noobs onecell	
		esttab 	ahours_KI_1966S_all ahours_KI_1966S_black ahours_KI_1966S_white ahours_KI_1966S_men ahours_KI_1966S_women ahours_KI_1966S_ls ahours_KI_1966S_hs ///
				using "tables/table_emp_KI_1966S.tex", append  label fragment ///
				nolines  prefoot(\midrule) postfoot(\bottomrule ) booktabs ///
				nonumbers nomtitles collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(2.time#c.KI_1966S_aw_std)   ///				
				coeflabel(2.time#c.KI_1966S_aw_std "\rule{0pt}{3ex}{Annual Hours}") ///
				stats(N hascontrols hastimefe hasstatefe, ///
				fmt(%11.0gc) label("Observations" "Controls" "Time FE" "State FE")) onecell 	
		esttab  matrix(emp_elast_KI_1966S,fmt(%3.2f)) ///
				using "tables/table_emp_KI_1966S.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none) 
		esttab  matrix(epop_elast_KI_1966S,fmt(%3.2f)) ///
				using "tables/table_emp_KI_1966S.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none) 	
		esttab  matrix(ahours_elast_KI_1966S,fmt(%3.2f)) ///
				using "tables/table_emp_KI_1966S.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none) postfoot(\bottomrule \bottomrule) 

*%------------------------------------------------------------------------------------------
*Stata error: system limit exceeded you need to drop one or more models
estimates drop _all
*to keep" _est_aw_F_s1966_all _est_epop_F_s1966_all _est_emp_F_s1966_all
*%------------------------------------------------------------------------------------------
				
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*TABLE E6 [APPENDIX]: EFFECT OF 1967 REFORM USING SHARE OF WORKERS BELOW $1.60 in 1966
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*>>>>>>>ALL					
	*USE STANDARDIZED MEASURE OF THE TREATMENT VARIABLE
	cap drop F_s1966_aw_std F_s1966_emp_std
	egen F_s1966_aw_std = std(F_s1966) if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen F_s1966_emp_std = std(F_s1966) if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
	**MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_F_s1966_all
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_F_s1966_all
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_F_s1966_all
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_F_s1966_all
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_F_s1966_all		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_F_s1966_all

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_F_s1966_all

	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_F_s1966_all
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  epop_all
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  emp_all
			
			*matrices elasticties		
			suest   epop_all epop_elast_F_s1966_all aw_elast_F_s1966_all, vce(cluster state_group)
			nlcom  (epop_elast_all:(1/_b[epop_all_mean:_cons])*(_b[epop_elast_F_s1966_all_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_all_mean:2.time#F_s1966_aw_std])), post
			mat 	epop_elast_all = _b[epop_elast_all]\ _se[epop_elast_all]\_b[epop_elast_all]-1.96*_se[epop_elast_all]\_b[epop_elast_all]+1.96*_se[epop_elast_all]

			suest   emp_all emp_elast_F_s1966_all aw_elast_F_s1966_all, vce(cluster state_group)
			nlcom  (emp_elast_all:(1/_b[emp_all_mean:_cons])*(_b[emp_elast_F_s1966_all_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_all_mean:2.time#F_s1966_aw_std])), post
			mat 	emp_elast_all = _b[emp_elast_all]\ _se[emp_elast_all]\_b[emp_elast_all]-1.96*_se[emp_elast_all]\_b[emp_elast_all]+1.96*_se[emp_elast_all]
			
			suest   ahours_elast_F_s1966_all aw_elast_F_s1966_all, vce(cluster state_group)
			nlcom  (ahours_elast_all:_b[ahours_elast_F_s1966_all_mean:2.time#F_s1966_aw_std]/_b[aw_elast_F_s1966_all_mean:2.time#F_s1966_aw_std]), post
			mat 	ahours_elast_all = _b[ahours_elast_all]\ _se[ahours_elast_all]\_b[ahours_elast_all]-1.96*_se[ahours_elast_all]\_b[ahours_elast_all]+1.96*_se[ahours_elast_all]
				
*>>>>>>>BLACK PERSONS				
	*USE STANDARDIZED MEASURE OF THE TREATMENT VARIABLE
	drop F_s1966_aw_std F_s1966_emp_std 
	egen F_s1966_aw_std = std(F_s1966) if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen F_s1966_emp_std = std(F_s1966) if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex $covar i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_F_s1966_black
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex  $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_F_s1966_black
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex  $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_F_s1966_black
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_F_s1966_black
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex  $covar i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_F_s1966_black		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_F_s1966_black

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex  $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_F_s1966_black

	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex  $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_F_s1966_black
	
  **ELASTICITIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,200) & state_group!=22
			eststo  epop_black
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,200) & state_group!=22
			eststo  emp_black
			
			*matrices elasticties		
			suest   epop_black epop_elast_F_s1966_black aw_elast_F_s1966_black, vce(cluster state_group)
			nlcom  (epop_elast_black:(1/_b[epop_black_mean:_cons])*(_b[epop_elast_F_s1966_black_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_black_mean:2.time#F_s1966_aw_std])), post
			mat 	epop_elast_black = _b[epop_elast_black]\ _se[epop_elast_black]\_b[epop_elast_black]-1.96*_se[epop_elast_black]\_b[epop_elast_black]+1.96*_se[epop_elast_black]

			suest   emp_black emp_elast_F_s1966_black aw_elast_F_s1966_black, vce(cluster state_group)
			nlcom  (emp_elast_black:(1/_b[emp_black_mean:_cons])*(_b[emp_elast_F_s1966_black_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_black_mean:2.time#F_s1966_aw_std])), post
			mat 	emp_elast_black = _b[emp_elast_black]\ _se[emp_elast_black]\_b[emp_elast_black]-1.96*_se[emp_elast_black]\_b[emp_elast_black]+1.96*_se[emp_elast_black]
			
			suest   ahours_elast_F_s1966_black aw_elast_F_s1966_black, vce(cluster state_group)
			nlcom  (ahours_elast_black:_b[ahours_elast_F_s1966_black_mean:2.time#F_s1966_aw_std]/_b[aw_elast_F_s1966_black_mean:2.time#F_s1966_aw_std]), post
			mat 	ahours_elast_black = _b[ahours_elast_black]\ _se[ahours_elast_black]\_b[ahours_elast_black]-1.96*_se[ahours_elast_black]\_b[ahours_elast_black]+1.96*_se[ahours_elast_black]

*>>>>>>>WHITE PERSONS				
  *USE STANDARDIZED MEASURE OF THE TREATMENT VARIABLE
	drop F_s1966_aw_std F_s1966_emp_std 
	egen F_s1966_aw_std 	= std(F_s1966) if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen F_s1966_emp_std  	= std(F_s1966) if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex $covar i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_F_s1966_white
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex  $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_F_s1966_white
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex  $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_F_s1966_white
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_F_s1966_white
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex  $covar i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_F_s1966_white		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_F_s1966_white

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex  $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_F_s1966_white

	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex  $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_F_s1966_white
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100) & state_group!=22
			eststo  epop_white
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,25,55) & inlist(race,100) & state_group!=22
			eststo  emp_white
			
			*matrices elasticties		
			suest   epop_white epop_elast_F_s1966_white aw_elast_F_s1966_white, vce(cluster state_group)
			nlcom  (epop_elast_white:(1/_b[epop_white_mean:_cons])*(_b[epop_elast_F_s1966_white_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_white_mean:2.time#F_s1966_aw_std])), post
			mat 	epop_elast_white = _b[epop_elast_white]\ _se[epop_elast_white]\_b[epop_elast_white]-1.96*_se[epop_elast_white]\_b[epop_elast_white]+1.96*_se[epop_elast_white]

			suest   emp_white emp_elast_F_s1966_white aw_elast_F_s1966_white, vce(cluster state_group)
			nlcom  (emp_elast_white:(1/_b[emp_white_mean:_cons])*(_b[emp_elast_F_s1966_white_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_white_mean:2.time#F_s1966_aw_std])), post
			mat 	emp_elast_white = _b[emp_elast_white]\ _se[emp_elast_white]\_b[emp_elast_white]-1.96*_se[emp_elast_white]\_b[emp_elast_white]+1.96*_se[emp_elast_white]
			
			suest   ahours_elast_F_s1966_white aw_elast_F_s1966_white, vce(cluster state_group)
			nlcom  (ahours_elast_white:_b[ahours_elast_F_s1966_white_mean:2.time#F_s1966_aw_std]/_b[aw_elast_F_s1966_white_mean:2.time#F_s1966_aw_std]), post
			mat 	ahours_elast_white = _b[ahours_elast_white]\ _se[ahours_elast_white]\_b[ahours_elast_white]-1.96*_se[ahours_elast_white]\_b[ahours_elast_white]+1.96*_se[ahours_elast_white]

*>>>>>>>MEN			
	drop F_s1966_aw_std F_s1966_emp_std 
	egen F_s1966_aw_std 	= std(F_s1966) if sex==1 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen F_s1966_emp_std  	= std(F_s1966) if sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.race $covar i.state_group [pw=weight] ///
			if sex==1 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_F_s1966_men
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [pw=weight]  ///
			if sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_F_s1966_men
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [pw=weight]  ///
			if sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_F_s1966_men
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time  i.race $covar_nohnow i.state_group [pw=weight] ///
			if sex==1 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_F_s1966_men
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time  i.race $covar i.state_group [iw=weight] ///
			if sex==1 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_F_s1966_men		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [iw=weight]  ///
			if sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_F_s1966_men

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [iw=weight]  ///
			if sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_F_s1966_men

	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.race $covar_nohnow i.state_group [iw=weight] ///
			if sex==1 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_F_s1966_men
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & sex==1 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  epop_men
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & sex==1  & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  emp_men
			
			*matrices elasticties		
			suest   epop_men epop_elast_F_s1966_men aw_elast_F_s1966_men, vce(cluster state_group)
			nlcom  (epop_elast_men:(1/_b[epop_men_mean:_cons])*(_b[epop_elast_F_s1966_men_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_men_mean:2.time#F_s1966_aw_std])), post
			mat 	epop_elast_men = _b[epop_elast_men]\ _se[epop_elast_men]\_b[epop_elast_men]-1.96*_se[epop_elast_men]\_b[epop_elast_men]+1.96*_se[epop_elast_men]

			suest   emp_men emp_elast_F_s1966_men aw_elast_F_s1966_men, vce(cluster state_group)
			nlcom  (emp_elast_men:(1/_b[emp_men_mean:_cons])*(_b[emp_elast_F_s1966_men_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_men_mean:2.time#F_s1966_aw_std])), post
			mat 	emp_elast_men = _b[emp_elast_men]\ _se[emp_elast_men]\_b[emp_elast_men]-1.96*_se[emp_elast_men]\_b[emp_elast_men]+1.96*_se[emp_elast_men]
			
			suest   ahours_elast_F_s1966_men aw_elast_F_s1966_men, vce(cluster state_group)
			nlcom  (ahours_elast_men:_b[ahours_elast_F_s1966_men_mean:2.time#F_s1966_aw_std]/_b[aw_elast_F_s1966_men_mean:2.time#F_s1966_aw_std]), post
			mat 	ahours_elast_men = _b[ahours_elast_men]\ _se[ahours_elast_men]\_b[ahours_elast_men]-1.96*_se[ahours_elast_men]\_b[ahours_elast_men]+1.96*_se[ahours_elast_men]
												
*>>>>>>>WOMEN			
	drop F_s1966_aw_std F_s1966_emp_std 
	egen F_s1966_aw_std 	= std(F_s1966) if sex==2 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen F_s1966_emp_std  	= std(F_s1966) if sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.race $covar i.state_group [pw=weight] ///
			if sex==2 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_F_s1966_women
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [pw=weight]  ///
			if sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_F_s1966_women
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [pw=weight]  ///
			if sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_F_s1966_women
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time  i.race $covar_nohnow i.state_group [pw=weight] ///
			if sex==2 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_F_s1966_women
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time  i.race $covar i.state_group [iw=weight] ///
			if sex==2 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_F_s1966_women		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [iw=weight]  ///
			if sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_F_s1966_women

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp  i.race $covar_emp i.state_group [iw=weight]  ///
			if sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_F_s1966_women

	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.race $covar_nohnow i.state_group [iw=weight] ///
			if sex==2 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_F_s1966_women
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & sex==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  epop_women
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & sex==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  emp_women
			
			*matrices elasticties		
			suest   epop_women epop_elast_F_s1966_women aw_elast_F_s1966_women, vce(cluster state_group)
			nlcom  (epop_elast_women:(1/_b[epop_women_mean:_cons])*(_b[epop_elast_F_s1966_women_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_women_mean:2.time#F_s1966_aw_std])), post
			mat 	epop_elast_women = _b[epop_elast_women]\ _se[epop_elast_women]\_b[epop_elast_women]-1.96*_se[epop_elast_women]\_b[epop_elast_women]+1.96*_se[epop_elast_women]

			suest   emp_women emp_elast_F_s1966_women aw_elast_F_s1966_women, vce(cluster state_group)
			nlcom  (emp_elast_women:(1/_b[emp_women_mean:_cons])*(_b[emp_elast_F_s1966_women_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_women_mean:2.time#F_s1966_aw_std])), post
			mat 	emp_elast_women = _b[emp_elast_women]\ _se[emp_elast_women]\_b[emp_elast_women]-1.96*_se[emp_elast_women]\_b[emp_elast_women]+1.96*_se[emp_elast_women]
			
			suest   ahours_elast_F_s1966_women aw_elast_F_s1966_women, vce(cluster state_group)
			nlcom  (ahours_elast_women:_b[ahours_elast_F_s1966_women_mean:2.time#F_s1966_aw_std]/_b[aw_elast_F_s1966_women_mean:2.time#F_s1966_aw_std]), post
			mat 	ahours_elast_women = _b[ahours_elast_women]\ _se[ahours_elast_women]\_b[ahours_elast_women]-1.96*_se[ahours_elast_women]\_b[ahours_elast_women]+1.96*_se[ahours_elast_women]

*>>>>>>>LOW-EDUCATION (inrange(educ,010,071) & !inlist(educ,000,999))				
	drop F_s1966_aw_std F_s1966_emp_std 
	egen F_s1966_aw_std 	= std(F_s1966) if inrange(educ,010,071) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen F_s1966_emp_std  	= std(F_s1966) if inrange(educ,010,071) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_F_s1966_ls
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_F_s1966_ls
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_F_s1966_ls
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_F_s1966_ls
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_F_s1966_ls		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_F_s1966_ls

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_F_s1966_ls

	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if inrange(educ,010,071) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_F_s1966_ls
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if inrange(educ,010,071) & !inlist(educ,000,999) & time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  epop_ls
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if inrange(educ,010,071) & !inlist(educ,000,999) & time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  emp_ls
			
			*matrices elasticties		
			suest   epop_ls epop_elast_F_s1966_ls aw_elast_F_s1966_ls, vce(cluster state_group)
			nlcom  (epop_elast_ls:(1/_b[epop_ls_mean:_cons])*(_b[epop_elast_F_s1966_ls_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_ls_mean:2.time#F_s1966_aw_std])), post
			mat 	epop_elast_ls = _b[epop_elast_ls]\ _se[epop_elast_ls]\_b[epop_elast_ls]-1.96*_se[epop_elast_ls]\_b[epop_elast_ls]+1.96*_se[epop_elast_ls]

			suest   emp_ls emp_elast_F_s1966_ls aw_elast_F_s1966_ls, vce(cluster state_group)
			nlcom  (emp_elast_ls:(1/_b[emp_ls_mean:_cons])*(_b[emp_elast_F_s1966_ls_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_ls_mean:2.time#F_s1966_aw_std])), post
			mat 	emp_elast_ls = _b[emp_elast_ls]\ _se[emp_elast_ls]\_b[emp_elast_ls]-1.96*_se[emp_elast_ls]\_b[emp_elast_ls]+1.96*_se[emp_elast_ls]
			
			suest   ahours_elast_F_s1966_ls aw_elast_F_s1966_ls, vce(cluster state_group)
			nlcom  (ahours_elast_ls:_b[ahours_elast_F_s1966_ls_mean:2.time#F_s1966_aw_std]/_b[aw_elast_F_s1966_ls_mean:2.time#F_s1966_aw_std]), post
			mat 	ahours_elast_ls = _b[ahours_elast_ls]\ _se[ahours_elast_ls]\_b[ahours_elast_ls]-1.96*_se[ahours_elast_ls]\_b[ahours_elast_ls]+1.96*_se[ahours_elast_ls]
							
*>>>>>>>HIGH-EDUCATION (inrange(educ,010,071) & !inlist(educ,000,999))				
 	drop F_s1966_aw_std F_s1966_emp_std 
	egen F_s1966_aw_std 	= std(F_s1966) if inrange(educ,072,125) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen F_s1966_emp_std  	= std(F_s1966) if inrange(educ,072,125) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
 **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_F_s1966_hs
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_F_s1966_hs
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_F_s1966_hs
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_F_s1966_hs
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_F_s1966_hs		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_F_s1966_hs

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_F_s1966_hs

	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if inrange(educ,072,125) & !inlist(educ,000,999) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_F_s1966_hs
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if inrange(educ,072,125) & !inlist(educ,000,999) & time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  epop_hs
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if inrange(educ,072,125) & !inlist(educ,000,999) & time_emp==2 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22
			eststo  emp_hs
			
			*matrices elasticties		
			suest   epop_hs epop_elast_F_s1966_hs aw_elast_F_s1966_hs, vce(cluster state_group)
			nlcom  (epop_elast_hs:(1/_b[epop_hs_mean:_cons])*(_b[epop_elast_F_s1966_hs_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_hs_mean:2.time#F_s1966_aw_std])), post
			mat 	epop_elast_hs = _b[epop_elast_hs]\ _se[epop_elast_hs]\_b[epop_elast_hs]-1.96*_se[epop_elast_hs]\_b[epop_elast_hs]+1.96*_se[epop_elast_hs]

			suest   emp_hs emp_elast_F_s1966_hs aw_elast_F_s1966_hs, vce(cluster state_group)
			nlcom  (emp_elast_hs:(1/_b[emp_hs_mean:_cons])*(_b[emp_elast_F_s1966_hs_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_hs_mean:2.time#F_s1966_aw_std])), post
			mat 	emp_elast_hs = _b[emp_elast_hs]\ _se[emp_elast_hs]\_b[emp_elast_hs]-1.96*_se[emp_elast_hs]\_b[emp_elast_hs]+1.96*_se[emp_elast_hs]
			
			suest   ahours_elast_F_s1966_hs aw_elast_F_s1966_hs, vce(cluster state_group)
			nlcom  (ahours_elast_hs:_b[ahours_elast_F_s1966_hs_mean:2.time#F_s1966_aw_std]/_b[aw_elast_F_s1966_hs_mean:2.time#F_s1966_aw_std]), post
			mat 	ahours_elast_hs = _b[ahours_elast_hs]\ _se[ahours_elast_hs]\_b[ahours_elast_hs]-1.96*_se[ahours_elast_hs]\_b[ahours_elast_hs]+1.96*_se[ahours_elast_hs]

			
*>>>>>>>BY COHORT-- 16-30 YEARS OLD					
 	drop F_s1966_aw_std F_s1966_emp_std 
	egen F_s1966_aw_std 	= std(F_s1966) if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen F_s1966_emp_std  	= std(F_s1966) if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_F_s1966_1630
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_F_s1966_1630
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_F_s1966_1630
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_F_s1966_1630
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_F_s1966_1630		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_F_s1966_1630

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_F_s1966_1630

	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,16,30) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_F_s1966_1630
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time==2 & inrange(age,16,30) & inlist(race,100,200) & state_group!=22
			eststo  epop_1630
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time==2 & inrange(age,16,30) & inlist(race,100,200) & state_group!=22
			eststo  emp_1630
			
			*matrices elasticties		
			suest   epop_1630 epop_elast_F_s1966_1630 aw_elast_F_s1966_1630, vce(cluster state_group)
			nlcom  (epop_elast_1630:(1/_b[epop_1630_mean:_cons])*(_b[epop_elast_F_s1966_1630_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_1630_mean:2.time#F_s1966_aw_std])), post
			mat 	epop_elast_1630 = _b[epop_elast_1630]\ _se[epop_elast_1630]\_b[epop_elast_1630]-1.96*_se[epop_elast_1630]\_b[epop_elast_1630]+1.96*_se[epop_elast_1630]

			suest   emp_1630 emp_elast_F_s1966_1630 aw_elast_F_s1966_1630, vce(cluster state_group)
			nlcom  (emp_elast_1630:(1/_b[emp_1630_mean:_cons])*(_b[emp_elast_F_s1966_1630_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_1630_mean:2.time#F_s1966_aw_std])), post
			mat 	emp_elast_1630 = _b[emp_elast_1630]\ _se[emp_elast_1630]\_b[emp_elast_1630]-1.96*_se[emp_elast_1630]\_b[emp_elast_1630]+1.96*_se[emp_elast_1630]
			
			suest   ahours_elast_F_s1966_1630 aw_elast_F_s1966_1630, vce(cluster state_group)
			nlcom  (ahours_elast_1630:_b[ahours_elast_F_s1966_1630_mean:2.time#F_s1966_aw_std]/_b[aw_elast_F_s1966_1630_mean:2.time#F_s1966_aw_std]), post
			mat 	ahours_elast_1630 = _b[ahours_elast_1630]\ _se[ahours_elast_1630]\_b[ahours_elast_1630]-1.96*_se[ahours_elast_1630]\_b[ahours_elast_1630]+1.96*_se[ahours_elast_1630]
				
*>>>>>>>BY COHORT-- 16-64 YEARS OLD					
 	drop F_s1966_aw_std F_s1966_emp_std 
	egen F_s1966_aw_std 	= std(F_s1966) if flag_employed & in_sample & inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen F_s1966_emp_std  	= std(F_s1966) if inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
  **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_F_s1966_1664
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_F_s1966_1664
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_F_s1966_1664
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_F_s1966_1664
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_F_s1966_1664		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_F_s1966_1664

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_F_s1966_1664

	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,16,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_F_s1966_1664
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,16,64) & inlist(race,100,200) & state_group!=22
			eststo  epop_1664
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,16,64) & inlist(race,100,200) & state_group!=22
			eststo  emp_1664
			
			*matrices elasticties		
			suest   epop_1664 epop_elast_F_s1966_1664 aw_elast_F_s1966_1664, vce(cluster state_group)
			nlcom  (epop_elast_1664:(1/_b[epop_1664_mean:_cons])*(_b[epop_elast_F_s1966_1664_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_1664_mean:2.time#F_s1966_aw_std])), post
			mat 	epop_elast_1664 = _b[epop_elast_1664]\ _se[epop_elast_1664]\_b[epop_elast_1664]-1.96*_se[epop_elast_1664]\_b[epop_elast_1664]+1.96*_se[epop_elast_1664]

			suest   emp_1664 emp_elast_F_s1966_1664 aw_elast_F_s1966_1664, vce(cluster state_group)
			nlcom  (emp_elast_1664:(1/_b[emp_1664_mean:_cons])*(_b[emp_elast_F_s1966_1664_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_1664_mean:2.time#F_s1966_aw_std])), post
			mat 	emp_elast_1664 = _b[emp_elast_1664]\ _se[emp_elast_1664]\_b[emp_elast_1664]-1.96*_se[emp_elast_1664]\_b[emp_elast_1664]+1.96*_se[emp_elast_1664]
			
			suest   ahours_elast_F_s1966_1664 aw_elast_F_s1966_1664, vce(cluster state_group)
			nlcom  (ahours_elast_1664:_b[ahours_elast_F_s1966_1664_mean:2.time#F_s1966_aw_std]/_b[aw_elast_F_s1966_1664_mean:2.time#F_s1966_aw_std]), post
			mat 	ahours_elast_1664 = _b[ahours_elast_1664]\ _se[ahours_elast_1664]\_b[ahours_elast_1664]-1.96*_se[ahours_elast_1664]\_b[ahours_elast_1664]+1.96*_se[ahours_elast_1664]
															
*>>>>>>>BY COHORT-- 50-64 YEARS OLD																
	drop F_s1966_aw_std F_s1966_emp_std 
	egen F_s1966_aw_std 	= std(F_s1966) if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22
	egen F_s1966_emp_std  	= std(F_s1966) if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
 **MAIN EFFECTS (WITH CLUSTER)
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo aw_F_s1966_5064
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo epop_F_s1966_5064
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
	
	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [pw=weight]  ///
			if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo emp_F_s1966_5064
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"		
	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [pw=weight] ///
			if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 ///
			, robust cluster(state_group) 
		eststo ahours_F_s1966_5064
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasstatefe 	"Y"			
		
  **EFFECTS FOR ELASTICTIES (NO CLUSTER) -- SO CAN BE USED IN SUEST
	**EARNINGS 
		reg ln_annual_wage c.F_s1966_aw_std##ib1.time i.sex i.race $covar i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22		
		eststo aw_elast_F_s1966_5064		
	
	**EMPLOYMENT (EPOP)
		reg flag_employed_nilf c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22 
		eststo epop_elast_F_s1966_5064

	**EMPLOYMENT (1-UNEMP RATE)
		reg flag_employed_unemp c.F_s1966_emp_std##ib1.time_emp i.sex i.race $covar_emp i.state_group [iw=weight]  ///
			if inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo emp_elast_F_s1966_5064

	**LN_HOURS
		reg ln_ahours c.F_s1966_aw_std##ib1.time i.sex i.race $covar_nohnow i.state_group [iw=weight] ///
			if flag_employed & in_sample & inrange(age,50,64) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22
		eststo ahours_elast_F_s1966_5064
	
  **ELASTICTIES 
			*mean epop
			reg 	flag_employed_nilf [iw=weight] ///
					if time_emp==2 & inrange(age,50,64) & inlist(race,100,200) & state_group!=22
			eststo  epop_5064
			*mean 1-unemp rate 
			reg 	flag_employed_unemp [iw=weight] ///
					if time_emp==2 & inrange(age,50,64) & inlist(race,100,200) & state_group!=22
			eststo  emp_5064
			
			*matrices elasticties		
			suest   epop_5064 epop_elast_F_s1966_5064 aw_elast_F_s1966_5064, vce(cluster state_group)
			nlcom  (epop_elast_5064:(1/_b[epop_5064_mean:_cons])*(_b[epop_elast_F_s1966_5064_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_5064_mean:2.time#F_s1966_aw_std])), post
			mat 	epop_elast_5064 = _b[epop_elast_5064]\ _se[epop_elast_5064]\_b[epop_elast_5064]-1.96*_se[epop_elast_5064]\_b[epop_elast_5064]+1.96*_se[epop_elast_5064]

			suest   emp_5064 emp_elast_F_s1966_5064 aw_elast_F_s1966_5064, vce(cluster state_group)
			nlcom  (emp_elast_5064:(1/_b[emp_5064_mean:_cons])*(_b[emp_elast_F_s1966_5064_mean:2.time_emp#F_s1966_emp_std]/_b[aw_elast_F_s1966_5064_mean:2.time#F_s1966_aw_std])), post
			mat 	emp_elast_5064 = _b[emp_elast_5064]\ _se[emp_elast_5064]\_b[emp_elast_5064]-1.96*_se[emp_elast_5064]\_b[emp_elast_5064]+1.96*_se[emp_elast_5064]
			
			suest   ahours_elast_F_s1966_5064 aw_elast_F_s1966_5064, vce(cluster state_group)
			nlcom  (ahours_elast_5064:_b[ahours_elast_F_s1966_5064_mean:2.time#F_s1966_aw_std]/_b[aw_elast_F_s1966_5064_mean:2.time#F_s1966_aw_std]), post
			mat 	ahours_elast_5064 = _b[ahours_elast_5064]\ _se[ahours_elast_5064]\_b[ahours_elast_5064]-1.96*_se[ahours_elast_5064]\_b[ahours_elast_5064]+1.96*_se[ahours_elast_5064]
																
							
  **MATRICES ELASTICTIES 
			mat 		 epop_elast_F_s1966 = (epop_elast_all,epop_elast_black,epop_elast_white,epop_elast_men,epop_elast_women,epop_elast_ls,epop_elast_hs)
			mat rownames epop_elast_F_s1966 ="\rule{0pt}{3ex}\textbf{Emp. (vs. unemp/nilf) elast.}" "se" "lower bound" "upper bound"
		   
		   mat 			emp_elast_F_s1966 = (emp_elast_all,emp_elast_black,emp_elast_white,emp_elast_men,emp_elast_women,emp_elast_ls,emp_elast_hs)
		   mat rownames emp_elast_F_s1966 ="\rule{0pt}{3ex}\textbf{Emp. (vs. unemp) elasticity}" "se" "lower bound" "upper bound"
	
		   mat 			ahours_elast_F_s1966 = (ahours_elast_all,ahours_elast_black,ahours_elast_white,ahours_elast_men,ahours_elast_women,ahours_elast_ls,ahours_elast_hs)
		   mat rownames ahours_elast_F_s1966 ="\rule{0pt}{3ex}\textbf{Annual Hours elast.}" "se" "lower bound" "upper bound"
				
	
  **OUTPUT TABLES 	
		esttab 	aw_F_s1966_all aw_F_s1966_black aw_F_s1966_white aw_F_s1966_men aw_F_s1966_women aw_F_s1966_ls aw_F_s1966_hs ///
				using "tables/table_emp_F_s1966.tex", replace  label fragment ///
				nolines  posthead(\cmidrule{2-8}) booktabs ///
				nonumbers mtitle("All" "Black" "White" "Men" "Women" "Low-educ." "High-educ") collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(2.time#c.F_s1966_aw_std)   ///				
				refcat(2.time#c.F_s1966_aw_std "Share wages below \\$1.60  $\times$& & & \\ \hspace{0.5cm}{1967-1972}", nolabel) ///
				coeflabel(2.time#c.F_s1966_aw_std "\rule{0pt}{3ex}{Earnings}") noobs onecell 
		esttab 	emp_F_s1966_all emp_F_s1966_black emp_F_s1966_white emp_F_s1966_men emp_F_s1966_women emp_F_s1966_ls emp_F_s1966_hs ///
				using "tables/table_emp_F_s1966.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(2.time_emp#c.F_s1966_emp_std)   ///				
				coeflabel(2.time_emp#c.F_s1966_emp_std "\rule{0pt}{3ex}{Emp. (vs. unemp.)}") noobs onecell	
		esttab 	epop_F_s1966_all epop_F_s1966_black epop_F_s1966_white epop_F_s1966_men epop_F_s1966_women epop_F_s1966_ls epop_F_s1966_hs ///
				using "tables/table_emp_F_s1966.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(2.time_emp#c.F_s1966_emp_std)   ///				
				coeflabel(2.time_emp#c.F_s1966_emp_std "\rule{0pt}{3ex}{Emp. (vs. unemp/nilf)}") noobs onecell			
		esttab 	ahours_F_s1966_all ahours_F_s1966_black ahours_F_s1966_white ahours_F_s1966_men ahours_F_s1966_women ahours_F_s1966_ls ahours_F_s1966_hs ///
				using "tables/table_emp_F_s1966.tex", append  label fragment ///
				nolines  prefoot(\midrule) postfoot(\bottomrule) booktabs ///
				nonumbers nomtitles collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(2.time#c.F_s1966_aw_std)   ///				
				coeflabel(2.time#c.F_s1966_aw_std "\rule{0pt}{3ex}{Annual Hours}") ///
				stats(N hascontrols hastimefe hasstatefe, ///
				fmt(%11.0gc) label("Observations" "Controls" "Time FE" "State FE")) onecell 	
		esttab  matrix(emp_elast_F_s1966,fmt(%3.2f)) ///
				using "tables/table_emp_F_s1966.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none)
		esttab  matrix(epop_elast_F_s1966,fmt(%3.2f)) ///
				using "tables/table_emp_F_s1966.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none) 	
		esttab  matrix(ahours_elast_F_s1966,fmt(%3.2f)) ///
				using "tables/table_emp_F_s1966.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none) postfoot(\bottomrule \bottomrule) 
	
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*FIGURE 7 [PAPER & SLIDES]: IMPACT OF 1966 FLSA ON EMPLOYMENT (INTENSIVE AND EXTENSIVE MARGINS)
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------			
	*FIGURE 7a: IMPACT ON INTENSIVE MARGIN
		reghdfe ln_ahours covered_1966 ib1965.year $inter_industry_long ib1.sex ib100.race $covar_nohnow [pw=weight] ///
				if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, absorb(i.industry) cluster(industry) 
		eststo ahours_long	
				
		coefplot(ahours_long, baselevels omitted keep(inter_*) ) ///
				 , vertical levels(95) pstyle(matrix) ciopts(recast(rcap) lcolor(gs12) lwidth(medthin)) /*noci*/ ///
				 yline(0, lstyle(major_grid)) connect(direct) lcolor(mydeepblue) lw(medthick) msize(medlarge) mfcolor(white) mlcolor(mydeepblue) mlw(medthick) ytitle("Estimated Effect on Log Annnual Hours", color(gs4)) ///
				 ylabel("-.05(.05).10",labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
				 xline(6.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1961" 5 "1965" 10 "1970" 15 "1975" 20 "1980" ///
				 ,labsize(medsmall) labcolor(gs4))
		gr export "figures/ahours.pdf", replace
	
	*FIGURE 7b: IMPACT ON EXTENSIVE MARGIN
		reg flag_employed_unemp nomw_1966 ib1965.year $inter_state_long ib1.sex ib100.race $covar_emp i.state_group [pw=weight] ///
			if  inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, robust cluster(state_group)		
		eststo emp_long_all				
		
		coefplot(emp_long_all, baselevels omitted keep(inter_*)) ///
				, vertical levels(95) pstyle(matrix) ciopts(recast(rcap)lcolor(gs10) lwidth(medthin)) ///
				yline(0, lstyle(major_grid)) connect(direct) lcolor(mydeepblue) lw(medium) msize(medium) mcolor(mydeepblue) ytitle("Probability of being employed (vs. unemployed)", color(gs4))  ///
				ylabel("-.1(0.05).10", labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
				xline(5.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1962"  4 "1965" 9 "1970" 14 "1975" 19 "1980",labsize(small)) 
		gr export "figures/emp_all.pdf", replace
						

	
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*FIGURE E2 [APPENDIX]: IMPACT OF THE 1966 FLSA ON PROBABILITY OF BEING EMPLOYED (VS. UNEMPLOYED) (1/2)
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------			
	*FIGURE E2a: BLACK vs. WHITE WORKERS		
		*white
		reg flag_employed_unemp nomw_1966 ib1965.year $inter_state_long  ib1.sex  $covar_emp i.state_group [pw=weight]  ///
			if  race==100 & inrange(age,25,55) & inrange(year,1961,1980) & year!=1962 & inlist(race,100,200) & state_group!=22, robust cluster(state_group)
		eststo emp_long_white		
		
		*black			
		reg flag_employed_unemp nomw_1966 ib1965.year $inter_state_long  ib1.sex   $covar_emp i.state_group [pw=weight] ///
			if race==200 & inrange(age,25,55)  & inrange(year,1961,1980) & year!=1962  & inlist(race,100,200) & state_group!=22, robust cluster(state_group)
		eststo emp_long_black	
			
		coefplot(emp_long_white, baselevels omitted keep(inter*) label("White") connect(direct) lcolor(mydeepblue) lw(medium) msize(medlarge) mcolor(mydeepblue) mlcolor(mydeepblue)) ///
				(emp_long_black, baselevels omitted keep(inter*) label("Black")   connect(direct) lcolor(mydeepblue) lw(medthick) msymbol(triangle_hollow) msize(large) mcolor(mydeepblue) mlcolor(mydeepblue) ciopts(recast(rcap) lcolor(gs14)) ) ///
				  , vertical levels(95) pstyle(matrix) ciopts(recast(rcap) lcolor(gs10) lwidth(medthin)) ///
				  yline(0, lstyle(major_grid))  ytitle("Estimated Effect on probability of employment", color(gs4)) ///
				  ylabel(-.1(0.05).1,labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
				  xline(5.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1962" 4 "1965" 9 "1970" 14 "1975" 19 "1980",labsize(medsmall) labcolor(gs4)) ///
				  legend(order(2 "White" 4 "Black") ring(0) position(4) bmargin(large) color(gs1) c(1) region(col(white)))
		gr export "figures/emp_black_white.pdf", replace			
		
		
	*FIGURE E2b: LOW-EDUCATION vs. HIGH-EDUCATION
		*low-education 
			reg 	flag_employed_unemp  nomw_1966 ib1965.year $inter_state_long  ib1.sex $covar_emp i.state_group [pw=weight] ///
					if inrange(age,25,55)  & inrange(year,1961,1980) & year!=1962 & inrange(educ,010,071) & !inlist(educ,000,999) & state_group!=22, robust cluster(state_group)
			eststo 	emp_long_ls		
		*high_educated 
			reg 	flag_employed_unemp nomw_1966 ib1965.year $inter_state_long  ib1.sex  $covar_emp i.state_group [pw=weight]  ///
					if inrange(age,25,55) & inrange(year,1961,1980) & year!=1962 & inrange(educ,072,125) & !inlist(educ,000,999) & state_group!=22, robust cluster(state_group)
			eststo 	emp_long_hs		
				
			coefplot(emp_long_ls, baselevels omitted keep(inter*) label("Low-education") connect(direct) lcolor(mybeige) lw(medium) msize(medlarge) mcolor(mybeige) mlcolor(mybeige)) ///
					(emp_long_hs, baselevels omitted keep(inter*) label("High-education")   connect(direct) lcolor(myred) lw(medthick) msize(medlarge) mcolor(myred) mlcolor(myred) ciopts(recast(rcap) lcolor(gs14)) ) ///
					  , vertical levels(95) pstyle(matrix) ciopts(recast(rcap) lcolor(gs10) lwidth(medthin)) ///
					  yline(0, lstyle(major_grid))  ytitle("Estimated Effect on probability of employment", color(gs4)) ///
					  ylabel(-.1(0.05).1,labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
					  xline(5.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1962" 4 "1965" 9 "1970" 14 "1975" 19 "1980",labsize(medsmall) labcolor(gs4)) ///
					  legend(order(2 "Low-education" 4 "High-education") ring(0) position(4) bmargin(large) color(gs1) c(1) region(col(white)))
			gr export "figures/emp_ls_hs.pdf", replace	
		
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*FIGURE E3 [APPENDIX]: IMPACT OF THE 1966 FLSA ON PROBABILITY OF BEING EMPLOYED (VS. UNEMPLOYED) (2/2)
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------			
	*FIGURE E3a: BY GENDER
		**women
		reg 	flag_employed_unemp nomw_1966 ib1965.year $inter_state_long  i.race $covar_emp i.state_group [pw=weight] ///
				if sex==2 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, robust cluster(state_group)				
		eststo 	emp_long_women		
		***men 
		reg 	flag_employed_unemp nomw_1966 ib1965.year $inter_state_long  i.race $covar_emp i.state_group [pw=weight] ///
				if sex==1 & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, robust cluster(state_group)				
		eststo 	emp_long_men	

		
		coefplot(emp_long_women, baselevels omitted keep(inter*) label("Women") connect(direct) lcolor(myarticblue) lw(medium) msymbol(circle_hollow) msize(medlarge) mcolor(myarticblue) mlcolor(myarticblue)) ///
				(emp_long_men, baselevels omitted keep(inter*) label("Men")   connect(direct) lcolor(myarticblue) lw(medthick)  msize(large) mcolor(myarticblue) mlcolor(myarticblue) ciopts(recast(rcap) lcolor(gs14)) ) ///
				  , vertical levels(95) pstyle(matrix) ciopts(recast(rcap) lcolor(gs10) lwidth(medthin)) ///
				  yline(0, lstyle(major_grid))  ytitle("Estimated Effect on probability of employment", color(gs4)) ///
				  ylabel(-.1(0.05).1,labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
				  xline(5.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1962"  4 "1965" 9 "1970" 14 "1975" 19 "1980",labsize(small)) ///
				  legend(order(2 "Women" 4 "Men") ring(0) position(4) bmargin(large) color(gs1) c(1) region(col(white)))	   
		gr export "figures/emp_men_women.pdf", replace						
	
	*FIGURE E3b: BY COHORT
		*16-30 years-old 
		reg 	flag_employed_nilf ib1965.year $inter_state_long $covar_emp i.state_group [pw=weight] ///
				if inrange(age,16,30) & inrange(year,1961,1980) & year!=1962 & inlist(race,100,200) & state_group!=22, robust cluster(state_group)
		eststo 	emp_long_all_16_30
		*50-64 years-old 
		reg flag_employed_nilf ib1965.year $inter_state_long $covar_emp i.state_group [pw=weight]  ///
				if inrange(age,50,64) & inrange(year,1961,1980) & year!=1962 & inlist(race,100,200) & state_group!=22, robust cluster(state_group)
		eststo 	emp_long_all_50_64
		*16-64 years-old 
		reg flag_employed_nilf ib1965.year $inter_state_long $covar_emp i.state_group [pw=weight]  ///
				if inrange(age,16,64) & inrange(year,1961,1980) & year!=1962 & inlist(race,100,200) & state_group!=22, robust cluster(state_group)
		eststo 	emp_long_all_16_64		

		coefplot(emp_long_all_16_30, baselevels omitted keep(inter*) connect(direct) lcolor(myarticblue) lw(medthick) msize(medlarge) mcolor(white) mlcolor(myarticblue) ciopts(recast(rcap) lcolor(gs14)) ) ///
				(emp_long_all_50_64, baselevels omitted keep(inter*)   connect(direct) lcolor(mydeepblue) lw(medthick) msize(medlarge) mcolor(mydeepblue) mlcolor(mydeepblue) ciopts(recast(rcap) lcolor(gs14)) ) ///
				(emp_long_all_16_64, baselevels omitted keep(inter*)   connect(direct) lcolor(mydeepblue) lw(vthin) msize(medlarge) mcolor(white) mlcolor(myred) ciopts(recast(rcap) lcolor(gs14)) ) ///
				, vertical levels(95) pstyle(matrix) ciopts(recast(rcap)lcolor(gs10) lwidth(medthin)) ///
				yline(0, lstyle(major_grid)) connect(direct) lcolor(mydeepblue) lw(medium) msize(medium) mcolor(mydeepblue) ytitle("Probability of being employed", color(gs4))  ///
				ylabel("-.2(0.1).20", labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
				xline(5.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1962"  4 "1965" 9 "1970" 12 "1975" 17 "1980",labsize(small)) ///
				legend(order(2 "16-30 years-old" 4 "50-64 years-old" 6 "16-64 years-old") ring(0) position(4) bmargin(large) color(gs1) c(1) region(col(white)))			
		gr export "figures/emp_cohorts.pdf", replace		
	

	
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*
*FIGURE E5: EMPLOYMENT ELASTICITIES WRT WAGE IN THE LITERATURE AND IN THIS PAPER 
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*
	use "data/output/Figure12.dta", clear	
	
	expand 2, gen(aux)
	gen ci=coeff-1.96*se if aux==0
	replace ci=coeff+1.96*se if aux==1

	bysort aux: gen id=_n
	gen ygraphval=.
	 
	 
	replace ygraphval=1 if id==1   // "Allegretto et al. 2011"
	replace ygraphval=2 if id==2   // "Bell 1997"
	replace ygraphval=3 if id==3   // "Burkhauser et al. 2000"
	replace ygraphval=4 if id==4   // "Card 1992b"
	replace ygraphval=5 if id==5   // "Card 1992a"
	replace ygraphval=6 if id==6   // "Card et al 1994"
	replace ygraphval=7 if id==7   // "Currie and Fallick 1996"
	replace ygraphval=8 if id==8   // "Dube et al. 2010"
	replace ygraphval=9 if id==9   // "Dube et al. 2007"
	replace ygraphval=10 if id==10   // "Dube et al. 2010"
	replace ygraphval=11 if id==11   // "Fang and Lin 2015"
	replace ygraphval=12 if id==12   // "Giuliano 2013"
	replace ygraphval=13 if id==13   // "Kim and Taylor 1995"
	replace ygraphval=14 if id==14   // "Machin et al. 2003"
	replace ygraphval=15 if id==15   // "Neumark & Nizalove 2007"
	replace ygraphval=16 if id==16   // "Pereira 2003"
	replace ygraphval=17 if id==17   // "Dustmann et al. 2020"
	replace ygraphval=18 if id==18   // "Cengiz et al. 2019"
	replace ygraphval=19 if id==19   // "Harasztosi & Lindner 2019"
	replace ygraphval=20 if id==20   // "Bailey et al. 2020"
	replace ygraphval=21 if id==21   //  
	replace ygraphval=22 if id==22   // "Derenoncourt & Montialoux 2020"

	label define ygraphvallabels    ///	
	1 "Allegretto et al. 2011" ///
	2 "Bell 1997" ///
	3 "Burkhauser et al. 2000" ///
	4 "Card 1992b" ///
	5 "Card 1992a" ///
	6 "Card et al. 1994" ///
	7 "Currie and Fallick 1996" ///
	8 "Dube et al. 2010" ///
	9 "Dube et al. 2007" ///
	10 "Fang and Lin 2015" ///
	11 "Giuliano 2013" ///
	12 "Kim and Taylor 1995" ///
	13 "Machin et al. 2003" ///
	14 "Neumark & Nizalova 2007" ///
	15 "Perira 2003" ///
	16 "Dustmann et al. 2020" ///
	17 "Cengiz et al. 2019" ///
	18 "Harasztosi & Lindner 2019" ///
	19 "Bailey et al. 2020" ///
	20 " " ///
	21 "Derenoncourt & Montialoux 2020" ///

	label values ygraphval ygraphvallabels

	twoway scatter ygraphval coeff if id<21, mc(mydeepblue) m(s) /* mcolor(myarticblue)*/ ///
		|| scatter ygraphval coeff if id>=21, mc(myarticblue) m(s)  /*mcolor(myarticblue)*/ ///
		|| scatter ygraphval ci if ygraphval==1, c(l) lc(mydeepblue) m(i) lp(solid)  ///
		|| scatter ygraphval ci if ygraphval==2, c(l) lc(mydeepblue) m(i)lp(solid)  ///
		|| scatter ygraphval ci if ygraphval==3, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==4, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==5, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==6, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==7, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==8, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==9, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==10, c(l) lc(mydeepblue) m(i) lp(solid)  ///
		|| scatter ygraphval ci if ygraphval==11, c(l) lc(mydeepblue) m(i) lp(solid)  ///
		|| scatter ygraphval ci if ygraphval==12, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==13, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==14, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==15, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==16, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==17, c(l) lc(mydeepblue) m(i) lp(solid) ///				
		|| scatter ygraphval ci if ygraphval==18, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==19, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==20, c(l) lc(mydeepblue) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==21, c(l) lc(myarticblue) m(i) lp(solid) ///
		yline(1/19 21,lc(gs12) lwidth(thin) lp(dot)) ///
		xline(-.16, lp(dash)lc(myarticblue)) ///
		xline(0, lp(solid) lc(mydeepblue) lw(vthin)) ///
		graphregion(color(white)) ///
		ylab(1/21, labsize(small) valuelabel angle(0) nogrid tlength(0)) ytitle("") ///
		xlab(-2(.5)2, labsize(small)) xtitle("Estimated Employment Elasticity wrt Wage", size(small)) legend(off) ///
		scheme(plotplain)
		

	gr export "figures/emp_elasticities.pdf", replace

*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*FIGURE E4 [APPENDIX]: EMPLOYMENT ELASTICITIES WRT WAGE IN THE LITERATURE AND IN BAILEY ET AL.
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------			
	use "data/output/App_FigureF1.dta", clear	
	
	expand 2, gen(aux)
	gen ci=coeff-1.96*se if aux==0
	replace ci=coeff+1.96*se if aux==1

	bysort aux: gen id=_n
	gen ygraphval=.
	 
	
	replace ygraphval=1 if id==1     // "Card 1992b"
	replace ygraphval=2 if id==2  	 // "Card et al 1994"
	replace ygraphval=3 if id==3   	 // "Neumark & Nizalove 2007"
	replace ygraphval=4 if id==4   // "Dustmann et al. 2020"
	replace ygraphval=5 if id==5   // "Cengiz et al. 2019"
	replace ygraphval=6 if id==6   //  
	replace ygraphval=7 if id==7   //  Bailey et al. 2020 (black men, employed during year)
	replace ygraphval=8 if id==8   //  Bailey et al. 2020 (black men, employed in reference week)
	replace ygraphval=9 if id==9   //  Derenoncourt & Montialoux 2020 (blacks, employed in reference week)
	replace ygraphval=10 if id==10   //  
	replace ygraphval=11 if id==11   //  Bailey et al. 2020 (all, employed during year)
	replace ygraphval=12 if id==12   //  Bailey et al. 2020 (all, employed in reference week)
	replace ygraphval=13 if id==13   // "Derenoncourt & Montialoux 2020 (all, employed in reference week)"
	
	
	label define ygraphvallabels    ///	
	1 "Card 1992b" ///
	2 "Card et al 1994" ///
	3 "Neumark & Nizalove 2007" ///
	4 "Dustmann et al. 2020" ///
	5 "Cengiz et al. 2019" ///
	6 "  " ///
	7 "Bailey et al. 2020 (black men, emp. during year)" ///
	8 "Bailey et al. 2020 (black men, emp. in ref. week)" ///
	9 "DM 2020 (black men & women, emp. in reference week)" ///
	10 " " ///
	11 "Bailey et al. 2020 (all, emp. during year)" ///
	12 "Bailey et al. 2020 (all, emp. in ref. week)" ///
	13 "DM 2020 (all, emp. in reference week)" ///
	

	label values ygraphval ygraphvallabels


	
	twoway scatter ygraphval coeff if inrange(id,1,6), mc(gs4) m(s)  ///
		|| scatter ygraphval coeff if inrange(id,7,8), mc(mydeepblue) mfc(white) m(Th)  mlw(thin) ///
		|| scatter ygraphval coeff if inrange(id,9,10), mc(myarticblue) mfc(white) m(Th) mlw(thin)  ///
		|| scatter ygraphval coeff if inrange(id,11,12), mc(mydeepblue) /*mfc(white)*/ m(o)  ///		
		|| scatter ygraphval coeff if id==13, mc(myarticblue) /*mfc(white)*/ m(o)  ///		
		|| scatter ygraphval ci if ygraphval==1, c(l) lc(gs4) lw(thin) m(i) lp(solid)  ///
		|| scatter ygraphval ci if ygraphval==2, c(l) lc(gs4) lw(thin) m(i)lp(solid)  ///
		|| scatter ygraphval ci if ygraphval==3, c(l) lc(gs4) lw(thin) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==4, c(l) lc(gs4) lw(thin) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==5, c(l) lc(gs4) lw(thin) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==6, c(l) lc(gs4) lw(thin) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==7, c(l) lc(mydeepblue) lw(thin) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==8, c(l) lc(mydeepblue) lw(thin) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==9, c(l) lc(myarticblue) lw(thin) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==10, c(l) lc(myarticblue) lw(thin) m(i) lp(solid)  ///
		|| scatter ygraphval ci if ygraphval==11, c(l) lc(mydeepblue) lw(thin) m(i) lp(solid)  ///
		|| scatter ygraphval ci if ygraphval==12, c(l) lc(mydeepblue) lw(thin) m(i) lp(solid) ///
		|| scatter ygraphval ci if ygraphval==13, c(l) lc(myarticblue) lw(thin) m(i) lp(solid) ///
		yline(1/13,lc(gs12) lwidth(thin) lp(dot)) ///
		xline(-.16, lp(dash) lc(myarticblue ) ) ///
		xline(0, lp(solid) lc(mydeepblue) lw(vthin)) ///
		graphregion(color(white)) ///
		ylab(1/13, labsize(small) valuelabel angle(0) nogrid /*tlength(0)*/) ytitle("") ///
		xlab(-2(.5)2, labsize(small)) xtitle("Emp. Elasticity wrt Wage", size(small)) legend(off) ///
		scheme(plotplain)
		

	gr export "figures/appendix_figureF1.pdf", replace
	
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*TABLE E1 [APPENDIX] :  EFFECT OF THE 1967 REFORM ON EMPLOYMENT, USING CROSS-INDUSTRY DESIGN, AGGREGATE LEVEL
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------			
		use "data/output/cps_master_individual_level.dta", clear
		keep if inrange(year,1961,1980)
			gen white = (race==100)
			gen male = (sex==1)
			gen low_education = .
			replace low_education = 1 if  inrange(educ,010,071) & !inlist(educ,000,999)
			replace low_education = 0 if  inrange(educ,072,125) & !inlist(educ,000,999) /*i.e. high-education workers*/
			gen married = (inlist(marst,1,2))	
			*gen flag_employed 	= inlist(empstat,10,12) & labforce==2
			*gen in_sample = (inlist(empstat,10,12) & inrange(age,16,64) & !inlist(classwkr,10,13,14,29) & gq!=2 & !inlist(wkswork2,0,.,1,9) & inrange(ahrsworkt,4,150)
				*& !inlist(industry,00,.) & !inlist(occupation,.,12) & incwage > 0 & incwage!=99998 & incwage!=9999998)
			
			collapse (mean) flag_employed flag_unemployed flag_nilf flag_universe covered_1966 covered_all ///
						age schooling low_education white_share = white married male_share = male time time_emp  [pw=weight] ///
						if inrange(age,25,55) & inlist(race,100,200) & inlist(sex,1,2) & year!=1962 & state_group!=22  ///
						, by(year industry state_group)
		
		*create employment outcome variables
		gen ln_emp 		= .
		replace ln_emp 	= ln(flag_employed) if !inlist(flag_employed,.,0)	
		
		label var ln_emp 	 "log(employment)"
			
		tempfile emp_all_j
		save `emp_all_j'
		
		*Construct log annual earnings variable at the industry level
		use "data/output/cps_master_individual_level.dta", clear
			keep if inrange(year,1961,1980)
			gen full_time = (fullpart==1)
			gen full_year = (wkswork2==6)				
			gen white = (race==100)	
			gen married = (inlist(marst,1,2))	
			
			gen aw_low_education = .
			replace aw_low_education = 1 if  inrange(educ,010,071) & !inlist(educ,000,999)
			replace aw_low_education = 0 if  inrange(educ,072,125) & !inlist(educ,000,999) /*i.e. high-education workers*/
			
			collapse (mean) annual_wage ahours aw_full_time_share = full_time aw_full_year_share = full_year  covered_1966 ///
							aw_low_education aw_age = age aw_schooling = schooling aw_white_share = white aw_married= married [pw=weight] ///
							if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  & state_group!=22 & covered_all ///
							, by(year industry state_group)
		
			*create outcome variables
			gen ln_annual_wage 	= .
			replace ln_annual_wage 	= log(annual_wage) if !inlist(annual_wage,.,0)	
	
			gen ln_ahours 		= . 
			replace ln_ahours 	= log(ahours) if !inlist(ahours,.,0)	
			label var ln_ahours  	 "annual number of hours (log) per year*state_group"	
			label var ln_annual_wage "annual wage (log) per year*state_group"	
	
		merge 1:1 year industry state_group using `emp_all_j'
		drop if covered_all==0
		drop if annual_wage==.
		drop _merge
		egen fe_state_year = group(state_group year)

		*i) annual wages, log(employment) WITH cluster-options
		** without state FE
			reg ln_annual_wage ib0.covered_1966##ib1.time aw_age  aw_schooling aw_married aw_full_time_share aw_full_year_share  aw_white_share male_share i.industry , cluster(industry)
				eststo aw_agg_1
			reg ln_emp i.covered_1966##i.time_emp age male_share white_share low_education married  i.industry , cluster(industry)
				eststo ln_emp_agg_1	
				estadd local hascontrols 		"Y"
				estadd local hastimefe 			"Y"
				estadd local hasindustryfe 		"Y"		
				estadd local hasstatefe 		"N"			
				estadd local hasstatebyyearfe 	"N"	
				
		** with state FE
			reg ln_annual_wage ib0.covered_1966##ib1.time aw_age  aw_schooling aw_married aw_full_time_share aw_full_year_share  aw_white_share male_share i.industry i.state_group , cluster(industry)
				eststo aw_agg_2
			reg ln_emp i.covered_1966##i.time_emp age male_share white_share low_education married  i.industry i.state_group, cluster(industry)
				eststo ln_emp_agg_2	
				estadd local hascontrols 		"Y"
				estadd local hastimefe 			"Y"
				estadd local hasindustryfe 		"Y"		
				estadd local hasstatefe 		"Y"		
				estadd local hasstatebyyearfe 	"N"	
					
		** with state by year FE
			reg ln_annual_wage ib0.covered_1966##ib1.time aw_age  aw_schooling aw_married aw_full_time_share aw_full_year_share  aw_white_share male_share i.industry i.fe_state_year, cluster(industry)
				eststo aw_agg_3
			reg ln_emp i.covered_1966##i.time_emp age male_share white_share low_education married  i.industry i.state_group, cluster(industry)
				eststo ln_emp_agg_3	
				estadd local hascontrols 		"Y"
				estadd local hastimefe 			"Y"
				estadd local hasindustryfe 		"Y"		
				estadd local hasstatefe 		"N"	
				estadd local hasstatebyyearfe 	"Y"	
				
		*ii) annual wages, log(employment) WITHOUT cluster-options so that it can be used in SUEST
		** without state FE		
			reg ln_annual_wage ib0.covered_1966##ib1.time aw_age  aw_schooling aw_married aw_full_time_share aw_full_year_share  aw_white_share male_share i.industry 
				eststo aw_agg_elast_1
			reg ln_emp i.covered_1966##i.time_emp age male_share white_share low_education married  i.industry 
				eststo ln_emp_agg_elast_1	
				
		** with state FE
			reg ln_annual_wage ib0.covered_1966##ib1.time aw_age  aw_schooling aw_married aw_full_time_share aw_full_year_share  aw_white_share male_share i.industry i.state_group 
				eststo aw_agg_elast_2
			reg ln_emp i.covered_1966##i.time_emp age male_share white_share low_education married  i.industry i.state_group
				eststo ln_emp_agg_elast_2	
				
		** with state by year FE
			reg ln_annual_wage ib0.covered_1966##ib1.time aw_age  aw_schooling aw_married aw_full_time_share aw_full_year_share  aw_white_share male_share i.industry i.fe_state_year
				eststo aw_agg_elast_3
			reg ln_emp i.covered_1966##i.time_emp age male_share white_share low_education married  i.industry i.state_group
				eststo ln_emp_agg_elast_3	
			
		*>>elasticities	
		** without state FE		
			suest  	ln_emp_agg_elast_1 aw_agg_elast_1, vce(cluster industry)
			nlcom  (lnemp_elast_1:(_b[ln_emp_agg_elast_1_mean:1.covered_1966#2.time_emp]/_b[aw_agg_elast_1_mean:2.time#1.covered_1966])), post
			mat 	lnemp_elast_1 = _b[lnemp_elast_1]\ _se[lnemp_elast_1]\_b[lnemp_elast_1]-1.96*_se[lnemp_elast_1]\_b[lnemp_elast_1]+1.96*_se[lnemp_elast_1]
		
		** with state FE
			suest  	ln_emp_agg_elast_2 aw_agg_elast_2, vce(cluster industry)
			nlcom  (lnemp_elast_2:(_b[ln_emp_agg_elast_2_mean:1.covered_1966#2.time_emp]/_b[aw_agg_elast_2_mean:2.time#1.covered_1966])), post
			mat 	lnemp_elast_2 = _b[lnemp_elast_2]\ _se[lnemp_elast_2]\_b[lnemp_elast_2]-1.96*_se[lnemp_elast_2]\_b[lnemp_elast_2]+1.96*_se[lnemp_elast_2]
		
		** with state by year FE
			suest  	ln_emp_agg_elast_3 aw_agg_elast_3, vce(cluster industry)
			nlcom  (lnemp_elast_3:(_b[ln_emp_agg_elast_3_mean:1.covered_1966#2.time_emp]/_b[aw_agg_elast_3_mean:2.time#1.covered_1966])), post
			mat 	lnemp_elast_3 = _b[lnemp_elast_3]\ _se[lnemp_elast_3]\_b[lnemp_elast_3]-1.96*_se[lnemp_elast_3]\_b[lnemp_elast_3]+1.96*_se[lnemp_elast_3]
								
	***CREATE MATRICES OF ELASTICTIES
		   mat lnemp_elast = (lnemp_elast_1,lnemp_elast_2,lnemp_elast_3)
		   mat rownames lnemp_elast = "\rule{0pt}{3ex}{\textbf{Emp. elasticity}}" "se" "lower bound" "upper bound"

	
	***EMPLOYEMENT TABLE CROSS-INDUSTRY DESIGN, AGGREGATE LEVEL
		esttab 	aw_agg_1  aw_agg_2 aw_agg_3 ///
				using "tables/table_cps_emp_ind_agg.tex", replace  label fragment ///
				nolines  posthead(\cmidrule(lr){2-2} \cmidrule(lr){3-3} \cmidrule(lr){4-4}) booktabs ///
				nonumbers mtitle("(1)" "(2)" "(3)" ) collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(1.covered_1966#2.time)   ///				
				refcat(1.covered_1966#2.time "Covered in 1967 $\times$& &  \\ \hspace{0.5cm}{1967-1972}", nolabel) ///
				coeflabel(1.covered_1966#2.time "\rule{0pt}{3ex}{\textbf{Earnings}}") noobs onecell 			
		esttab 	ln_emp_agg_1  ln_emp_agg_2 ln_emp_agg_3   ///
				using "tables/table_cps_emp_ind_agg.tex", append   label fragment ///
				nolines   booktabs ///
				nonumbers nomtitles collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				keep(1.covered_1966#2.time_emp)   ///				
				coeflabel(1.covered_1966#2.time_emp "\rule{0pt}{3ex}{\textbf{Employment}}")   	///	
				stats(N hascontrols hastimefe hasindustryfe hasstatefe hasstatebyyearfe, ///
				fmt(%11.0gc) label("Industry-by-State-Year Obs" "Has Controls" "Has Time FE" "Has Industry FE" "Has State FE" "Has State-by-year FE")) onecell 				
		esttab  matrix(lnemp_elast,fmt(%3.2f)) ///
				using "tables/table_cps_emp_ind_agg.tex", append  label fragment ///
				nolines booktabs ///
				nonumbers nomtitles collabels(none) postfoot(\bottomrule \bottomrule)  	
					
			
