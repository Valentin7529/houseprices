*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*program: 		Estimate effects of 1966 amendements on wages, racial gaps and employment 
*first created: 01/19/2018
*last updated:  08/29/2020
*structure:    	Create interaction variables and list of covariates
*				Wage regressions		
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
	
clear all
set more off
set matsize 10000
set maxvar 10000

use "data/output/cps_master_individual_level.dta", clear	
*---> TO DO move creation of this variable to "1d_build_march_cps_nomw.do"
gen ftfy = (ahrsworkt==40 &  wkswork2==6) if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980)
*drop if annual_wage > 500000 & flag_employed & in_sample  & inrange(age,25,55) & inlist(race,100,200) & year==1961

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
	global covar_emp 			"age age_square schooling marst"	

	*winsorized covariates
	global covar_w199 			"schooling_w199 exp_w199 exp_square_w199 exp_cubic_w199  ib1.fullpart ib6.wkswork2_w199 ahrsworkt_w199 i.marst i.occupation" 
	global covar_w595 			"schooling_w595 exp_w595 exp_square_w595 exp_cubic_w595  ib1.fullpart ib6.wkswork2_w595 ahrsworkt_w595 i.marst i.occupation" 
	global covar_w595_nohnow 	"schooling_w595 exp_w595 exp_square_w595 exp_cubic_w595  i.marst i.occupation" 

	*create state*year fixed effects 
	egen fe_state_year = group(state_group year)

	*create state linear trends
	tab state_group, gen(state_linear)
		foreach var of varlist state_linear* {
		gen `var'_trend =`var'*year
		}

	*create industry linear trends
	tab industry, gen(industry_linear)
		foreach var of varlist industry_linear* {
		gen `var'_trend =`var'*year
		}
		
*******************************************************************************************************************************************************************************************************************************
****WAGE REGRESSIONS
*******************************************************************************************************************************************************************************************************************************	   
*INDUSTRY DESIGN
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
*FIGURE 5 [PAPER & SLIDES]: IMPACT OF THE 1966 FLSA ON ANNUAL WAGES
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long ib1.sex ib100.race $covar [pw=weight] ///
			if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry) 
	eststo aw_long	
	*add-on TABLE 4
	mat tab_all = _b[inter_industry_long_1967], _b[inter_industry_long_1967] - 1.96* _se[inter_industry_long_1967] , _b[inter_industry_long_1967] + 1.96* _se[inter_industry_long_1967]  
		
	coefplot(aw_long, baselevels omitted keep(inter_*) ) ///
						  , vertical levels(95) pstyle(matrix) ciopts(recast(rcap) lcolor(gs12) lwidth(medthin))  ///
						  yline(0, lstyle(major_grid)) connect(direct) lcolor(mydeepblue) lw(medthick) msize(medlarge) mfcolor(white) mlcolor(mydeepblue) mlw(medthick) ytitle("Estimated Effect on Log Annual Earnings", color(gs4)) ///
						  ylabel("-.05(.05).10",labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
						  xline(6.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1961" 5 "1965" 10 "1970" 15 "1975" 20 "1980" ///
						  ,labsize(medsmall) labcolor(gs4)) ///
						  subtitle("Industries covered in 1967 vs. in 1938", color(gs6)) 				  
	gr export "figures/aw_industry_design.pdf", replace
	

	*FIGURE [SLIDES ONLY]: IMPACT OF THE 1966 FLSA ON ANNUAL WAGES WITHOUT AND WITH INDIVIDUAL-LEVEL CONTROLS
	*no controls
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long  [pw=weight] ///
			if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & year!=1961, absorb(i.industry) vce(cluster industry) 
	eststo aw_long_no_controls
	
	coefplot(aw_long, baselevels omitted keep(inter*) label("With controls") connect(direct) lcolor(mydeepblue) lw(vthin) msize(medlarge) mfcolor(white) mlcolor(mydeepblue) mlw(medthick) /*lpattern(dash)*/ ) ///
			(aw_long_no_controls, baselevels omitted keep(inter*) label("With no controls")  connect(direct) lcolor(mydeepblue) lw(medthick) msymbol(square_hollow) msize(medlarge) mcolor(mydeepblue) mlcolor(mydeepblue) noci  ) ///
						  , vertical levels(95) pstyle(matrix) ciopts(recast(rcap) lcolor(gs15) lwidth(medthin)) /*noci*/ ///
						  yline(0, lstyle(major_grid)) connect(direct) lcolor(mydeepblue) lw(medthick) msize(medlarge) mfcolor(white) mlcolor(mydeepblue) mlw(medthick) ytitle("Estimated Effect on Log Annual Earnings", color(gs4)) ///
						  ylabel("-.05(.05).10",labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
						  xline(6.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1961" 5 "1965" 10 "1970" 15 "1975" 20 "1980" ///
						  ,labsize(medsmall) labcolor(gs4)) ///
						  subtitle("Industries covered in 1967 vs. in 1938", color(gs6)) ///
						  legend(order(2 "With controls" 3 "Without controls") ring(0) position(5) bmargin(large) color(gs1) c(1) region(col(white))) 
						  
	gr export "figures/aw_industry_design_no_and_with_controls.pdf", replace

*%--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*FIGURE D1 [SLIDES ONLY]: IMPACT OF THE 1966 FLSA ON ANNUAL WAGES WITHOUT AND WITH INDIVIDUAL-LEVEL CONTROLS, AND WITH NO WEEKS AND NO HOURS WORKED					
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	*no weeks worked and no hours worked
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long ib1.sex ib100.race ib12.schooling exp exp_square exp_cubic ib1.marst ib1.occupation [pw=weight] ///
		if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry) 
	eststo aw_long_no_weeks_hours
	
	coefplot(aw_long, baselevels omitted keep(inter*) label("With controls") connect(direct) lcolor(mydeepblue) lw(vthin) msize(medlarge) mfcolor(white) mlcolor(mydeepblue) mlw(medthick) /*lpattern(dash)*/ ) ///
			(aw_long_no_controls, baselevels omitted keep(inter*) label("With no controls")  connect(direct) lcolor(mydeepblue) lw(vthin) msymbol(square_hollow) msize(medlarge) mcolor(mydeepblue) mlcolor(mydeepblue) noci ) ///
			(aw_long_no_weeks_hours, baselevels omitted keep(inter*) label("W/o weeks and hours worked")  connect(direct) lcolor(mydeepblue) lw(medthick) msymbol(triangle_hollow) msize(medlarge) mcolor(mydeepblue) mlcolor(mydeepblue) noci) ///
						  , vertical levels(95) pstyle(matrix) ciopts(recast(rcap) lcolor(gs15) lwidth(medthin)) /*noci*/ ///
						  yline(0, lstyle(major_grid)) connect(direct) lcolor(mydeepblue) lw(medthick) msize(medlarge) mfcolor(white) mlcolor(mydeepblue) mlw(medthick) ytitle("Estimated Effect on Log Annual Earnings", color(gs4)) ///
						  ylabel("-.05(.05).10",labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
						  xline(6.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1961" 5 "1965" 10 "1970" 15 "1975" 20 "1980" ///
						  ,labsize(medsmall) labcolor(gs4)) ///
						  subtitle("Industries covered in 1967 vs. in 1938", color(gs6)) ///
						  legend(order(2 "With controls" 3 "W/o controls" 4 "W/o weeks and hours worked") ring(0) position(5) bmargin(large) color(gs1) c(1) region(col(white))) 
	gr export "figures/aw_industry_design_with_and_without_controls_wohours.pdf", replace

	
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
*FIGURE D2: IMPACT OF THE 1966 FLSA ON ANNUAL WAGES SHOWING TIME PATH FOR WAGES IN TREATED AND CONTROL INDUSTRIES
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
		capture program drop aw_c_level
		program define aw_c_level
		args  a 			
		nlcom 	  (aw1961: (_b[_cons]+_b[1961.year])) ///
				  (aw1962: ((_b[_cons]+_b[1961.year]+_b[_cons]+_b[1963.year])/2)) ///
				  (aw1963: (_b[_cons]+_b[1963.year])) ///
				  (aw1964: (_b[_cons]+_b[1964.year])) ///
				  (aw1965: (_b[_cons]+_b[1965o.year])) ///
				  (aw1966: (_b[_cons]+_b[1966.year])) ///
				  (aw1967: (_b[_cons]+_b[1967.year])) ///
				  (aw1968: (_b[_cons]+_b[1968.year])) ///
				  (aw1969: (_b[_cons]+_b[1969.year])) ///
				  (aw1970: (_b[_cons]+_b[1970.year])) ///
				  (aw1971: (_b[_cons]+_b[1971.year])) ///
				  (aw1972: (_b[_cons]+_b[1972.year])) ///
				  (aw1973: (_b[_cons]+_b[1973.year])) ///
				  (aw1974: (_b[_cons]+_b[1974.year])) ///
				  (aw1975: (_b[_cons]+_b[1975.year])) ///
				  (aw1976: (_b[_cons]+_b[1976.year])) ///
				  (aw1977: (_b[_cons]+_b[1977.year])) ///
				  (aw1978: (_b[_cons]+_b[1978.year])) ///
				  (aw1979: (_b[_cons]+_b[1979.year])) ///
				  (aw1980: (_b[_cons]+_b[1980.year])) ///			  
				  , post
		end
					su ln_annual_wage [aw=weight] if inlist(year,1965) & covered_1966==0 & covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) 

						  
			su ln_annual_wage [aw=weight] if year==1965 & covered_1966==0 & covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) 
			global ln_aw_c_mean_1965 `r(mean)'
			
			su ln_annual_wage [aw=weight] if year==1965 & covered_1966==1 & covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200)
			global ln_aw_t_mean_1965 `r(mean)'
			

			gen ln_aw_normalized = . 
			replace ln_aw_normalized = ln_annual_wage- $ln_aw_c_mean_1965 if  covered_1966==0 & covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980)
			replace ln_aw_normalized = ln_annual_wage- $ln_aw_t_mean_1965 if  covered_1966==1 & covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980)
			
			reghdfe ln_aw_normalized ib1965.year   [pw=weight] ///
			if covered_1966==0 & covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962,  noabsorb vce(cluster industry) 
			aw_c_level 
			eststo aw_c_level_norm	
			
			reghdfe ln_aw_normalized ib1965.year   [pw=weight] ///
			if covered_1966==1 & covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962,  noabsorb  vce(cluster industry) 
			aw_c_level 
			eststo aw_t_level_norm				
			
		coefplot(aw_c_level_norm, label("Covered in 1938")  connect(direct) lcolor(mydeepblue) lw(medthick) msize(medlarge) mfcolor(mydeepblue) mlcolor(mydeepblue) mlw(medthick)) ///
				(aw_t_level_norm, label("Covered in 1967")  connect(direct) lcolor(myarticblue) lw(medthick) msize(medlarge) mfcolor(myarticblue) mlcolor(myarticblue) mlw(medthick)) ///	
				  , vertical levels(95) pstyle(matrix) noci ///
				  connect(direct) lcolor(mydeepblue) lw(medthick) msize(large) mcolor(mydeepblue) ytitle("Log Annual Earnings, normalized to 0 in 1965", color(gs4)) ///
				  ylabel(,labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
				  xline(6.5, lcolor(myred)) xlabel(1 "1961"  5 "1965" 10 "1970" 15 "1975" 20 "1980",labsize(medsmall) labcolor(gs4)) ///
				  legend(order(1 "Industries covered in 1938" 2 "Industries covered in 1967") ring(0) position(4) bmargin(large) color(gs1) c(1) region(col(white)))		
		gr export "figures/aw_levels_no_controls_normalized.pdf", replace			
		
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------			 
*TABLE 2 [PAPER & SLIDES]: WAGE EFFECT: MAIN RESULTS AND ROBUSTNESS CHECKS
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	*(1) Baseline
	reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex ib100.race $covar [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry) 
	eststo aw_short_1	
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatelinear "N"
			estadd local hasstatebyyear "N"				
			estadd local agrionly	 	"N"				
			estadd local ftonly 		"N"
			estadd local ind1961 		"N"
			estadd local winsorized 	"N"
			estadd local twcluster	 	"N"
	
	*(2) (1)+ state linear trends
	 reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex ib100.race $covar state_linear*_trend  [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry) 
	 eststo aw_short_2
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatelinear "Y"
			estadd local hasstatebyyear "N"				
			estadd local agrionly	 	"N"				
			estadd local ftonly 		"N"
			estadd local ind1961 		"N"
			estadd local winsorized 	"N"
			estadd local twcluster	 	"N"
			
			
	*(3) (1)+ state-by-year FE
	 reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex ib100.race $covar   i.state_group [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry) 
	 eststo aw_short_3
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatelinear "N"
			estadd local hasstatebyyear "Y"				
			estadd local agrionly	 	"N"				
			estadd local ftonly 		"N"
			estadd local ind1961 		"N"
			estadd local winsorized 	"N"
			estadd local twcluster	 	"N"
	
	*(3) (1)+ state linear trend

		
	*(4): baseline (1) without agriculture
	 reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex ib100.race $covar  [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & industry!=1, absorb(i.industry) vce(cluster industry) 
	 eststo aw_short_4				
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatelinear "N"
			estadd local hasstatebyyear "N"				
			estadd local agrionly	 	"Y"				
			estadd local ftonly 		"N"
			estadd local ind1961 		"N"
			estadd local winsorized 	"N"
			estadd local twcluster	 	"N"
	
	*(5): baseline (1) on full-time only
	 reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex ib100.race schooling exp exp_square exp_cubic ib6.wkswork2 ahrsworkt i.occupation i.marst [pw=weight] if covered_all & flag_employed & inrange(age,25,55) & in_sample & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & fullpart==1, absorb(i.industry) vce(cluster industry) 
	 eststo aw_short_5				
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatelinear "N"
			estadd local hasstatebyyear "N"				
			estadd local agrionly	 	"N"				
			estadd local ftonly 		"Y"
			estadd local ind1961 		"N"
			estadd local winsorized 	"N"
			estadd local twcluster	 	"N"
			
	*(6) add 1961 industries in the control group 
		reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex ib100.race $covar [pw=weight] if (covered_all | inlist(industry,4,11)) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry) 
		eststo aw_short_6	
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatelinear "N"
			estadd local hasstatebyyear "N"				
			estadd local agrionly	 	"N"				
			estadd local ftonly 		"N"
			estadd local ind1961 		"Y"
			estadd local winsorized 	"N"
			estadd local twcluster	 	"N"
	
	
	*(7): baseline (1) with winsorized outcome and control variables at the 5% level
	 reghdfe ln_annual_wage_w595 covered_1966 ib1.time $inter_industry_short ib1.sex ib100.race $covar_w595  [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry) 
	 eststo aw_short_7				
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatelinear "N"
			estadd local hasstatebyyear "N"				
			estadd local agrionly	 	"N"				
			estadd local ftonly 		"N"
			estadd local ind1961 		"N"
			estadd local winsorized 	"Y"
			estadd local twcluster	 	"N"				
	
	*(8): baseline (1) two-way cluster
	reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex ib100.race $covar [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) cluster(state_group industry) 
	eststo aw_short_8	
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatelinear "N"
			estadd local hasstatebyyear "N"				
			estadd local agrionly	 	"N"				
			estadd local ftonly 		"N"
			estadd local ind1961 		"N"
			estadd local winsorized 	"N"
			estadd local twcluster	 	"Y"		
	
	 

	*output table
	esttab 	aw_short_1 aw_short_2 aw_short_3 aw_short_4 aw_short_5  aw_short_6 aw_short_7 aw_short_8 ///
			using "tables/table_aw_industry_design.tex",  replace label fragment ///
			nolines  posthead(\cmidrule{2-9}) prefoot(\midrule) postfoot(\bottomrule \bottomrule) booktabs ///
			nonumbers mtitle("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)") collabels(none)    ///
			cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
			refcat(inter_industry_short_2 "Covered in 1967 $\times$", nolabel) ///
			keep(inter_industry_short_2 )   ///
			coeflabel(inter_industry_short_2 "\hspace{0.5cm}{1967-1972}") ///
			stats(N hascontrols hastimefe hasindustryfe  hasstatelinear hasstatebyyear agrionly ftonly ind1961 winsorized twcluster, ///
			fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Industry FE"  "State linear trends" "State-by-year FE"  "W/o agriculture" "Full-Time only" "1961 ind. in control grp" "Winsorized data" "2-way clusters")) onecell 

			
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
*TABLE D1 [APPENDIX]: IMPACT OF THE 1966 FLSA ON DIFFERENT QUANTILES OF ANNUAL WAGES
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
			
				*25th percentile	
					gen p25_all 	= .
					gen bp25_all 	= . 
					gen p25_black 	= .
					gen bp25_black 	= . 
					gen p25_white 	= .
					gen bp25_white 	= . 
				
				*50th percentile	
					gen p50_all 	= .
					gen bp50_all 	= . 
					gen p50_black 	= .
					gen bp50_black 	= . 
					gen p50_white 	= .
					gen bp50_whitey = . 
				
				*75th percentile	
					gen p75_all 	= .
					gen bp75_all 	= . 
					gen p75_black 	= .
					gen bp75_black 	= . 
					gen p75_white 	= .
					gen bp75_white 	= . 
							
			
					forval pct = 25(25)75 {	
					forval 	y=1961/1980 {
							qui su ln_annual_wage [aw=weight] if year==1966 & covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) , det 
							replace p`pct'_all = `r(p`pct')' if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200)  
							replace bp`pct'_all = (ln_annual_wage <`r(p`pct')') if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200)  
							 
							}
							}
	
					gen 	Qtile = . 
					replace Qtile = 1 if ln_annual_wage <= p25_all 								& covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200)  
					replace Qtile = 2 if ln_annual_wage <= p50_all & ln_annual_wage > p25_all 	& covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200)  
					replace Qtile = 3 if ln_annual_wage <= p75_all & ln_annual_wage > p50_all 	& covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200)  
					replace Qtile = 4 if ln_annual_wage > p75_all 								& covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200)  
					
	
				**(i) All
						*baseline cross-industry design 
						reghdfe ln_annual_wage ib0.covered_1966##ib1.time##ib4.Qtile ib1.sex ib100.race $covar [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 , absorb(i.industry) vce(cluster industry) 	
						eststo 	aw1_all
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#1.Qtile]
							mat did1_bp25_all_t2 = r(estimate) \ r(se)	   
							mat did1_bp25_all_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#2.Qtile]
							mat did1_bp50_all_t2 = r(estimate) \ r(se)	   
							mat did1_bp50_all_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#3.Qtile]
							mat did1_bp75_all_t2 = r(estimate) \ r(se)	   
							mat did1_bp75_all_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#1.Qtile]
							mat did1_bp25_all_t3 = r(estimate) \ r(se)	   
							mat did1_bp25_all_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#2.Qtile]
							mat did1_bp50_all_t3 = r(estimate) \ r(se)	   
							mat did1_bp50_all_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#3.Qtile]
							mat did1_bp75_all_t3 = r(estimate) \ r(se)	   
							mat did1_bp75_all_p_t3 = r(p) \ r(level)							
						estadd local hascontrols 	"Y"
						estadd local hastimefe 		"Y"
						estadd local hasindustryfe 	"Y"
						estadd local hasstatefe 	"N"
						estadd local hasstateyearfe "N"
						
						*Alternative cross-industry design (w. state FE)
						reghdfe ln_annual_wage ib0.covered_1966##ib1.time##ib4.Qtile ib1.sex ib100.race $covar i.state_group [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 , absorb(i.industry) vce(cluster industry) 						
						eststo 	aw2_all
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#1.Qtile]
							mat did2_bp25_all_t2 = r(estimate) \ r(se)	   
							mat did2_bp25_all_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#2.Qtile]
							mat did2_bp50_all_t2 = r(estimate) \ r(se)	   
							mat did2_bp50_all_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#3.Qtile]
							mat did2_bp75_all_t2 = r(estimate) \ r(se)	   
							mat did2_bp75_all_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#1.Qtile]
							mat did2_bp25_all_t3 = r(estimate) \ r(se)	   
							mat did2_bp25_all_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#2.Qtile]
							mat did2_bp50_all_t3 = r(estimate) \ r(se)	   
							mat did2_bp50_all_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#3.Qtile]
							mat did2_bp75_all_t3 = r(estimate) \ r(se)	   
							mat did2_bp75_all_p_t3 = r(p) \ r(level)							
						estadd local hascontrols 	"Y"
						estadd local hastimefe 		"Y"
						estadd local hasindustryfe 	"Y"
						estadd local hasstatefe 	"Y"
						estadd local hasstateyearfe "N"				
					
					*Alternative cross-industry design (w. state-by-year FE)
						reghdfe ln_annual_wage ib0.covered_1966##ib1.time##ib4.Qtile ib1.sex ib100.race $covar i.state_group i.fe_state_year [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 , absorb(i.industry) vce(cluster industry) 						
						eststo 	aw3_all
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#1.Qtile]
							mat did3_bp25_all_t2 = r(estimate) \ r(se)	   
							mat did3_bp25_all_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#2.Qtile]
							mat did3_bp50_all_t2 = r(estimate) \ r(se)	   
							mat did3_bp50_all_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#3.Qtile]
							mat did3_bp75_all_t2 = r(estimate) \ r(se)	   
							mat did3_bp75_all_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#1.Qtile]
							mat did3_bp25_all_t3 = r(estimate) \ r(se)	   
							mat did3_bp25_all_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#2.Qtile]
							mat did3_bp50_all_t3 = r(estimate) \ r(se)	   
							mat did3_bp50_all_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#3.Qtile]
							mat did3_bp75_all_t3 = r(estimate) \ r(se)	   
							mat did3_bp75_all_p_t3 = r(p) \ r(level)							
						estadd local hascontrols 	"Y"
						estadd local hastimefe 		"Y"
						estadd local hasindustryfe 	"Y"
						estadd local hasstatefe 	"N"
						estadd local hasstateyearfe "Y"				


				**(ii) Black
						*baseline cross-industry design 
						reghdfe ln_annual_wage ib0.covered_1966##ib1.time##ib4.Qtile ib1.sex  $covar [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 , absorb(i.industry) vce(cluster industry) 				
						eststo 	aw1_black
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#1.Qtile]
							mat did1_bp25_black_t2 = r(estimate) \ r(se)	   
							mat did1_bp25_black_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#2.Qtile]
							mat did1_bp50_black_t2 = r(estimate) \ r(se)	   
							mat did1_bp50_black_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#3.Qtile]
							mat did1_bp75_black_t2 = r(estimate) \ r(se)	   
							mat did1_bp75_black_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#1.Qtile]
							mat did1_bp25_black_t3 = r(estimate) \ r(se)	   
							mat did1_bp25_black_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#2.Qtile]
							mat did1_bp50_black_t3 = r(estimate) \ r(se)	   
							mat did1_bp50_black_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#3.Qtile]
							mat did1_bp75_black_t3 = r(estimate) \ r(se)	   
							mat did1_bp75_black_p_t3 = r(p) \ r(level)							
						estadd local hascontrols 	"Y"
						estadd local hastimefe 		"Y"
						estadd local hasindustryfe 	"Y"
						estadd local hasstatefe 	"N"
						estadd local hasstateyearfe "N"
						
						*Alternative cross-industry design (w. state FE)
						reghdfe ln_annual_wage ib0.covered_1966##ib1.time##ib4.Qtile ib1.sex  $covar i.state_group [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 , absorb(i.industry) vce(cluster industry) 						
						eststo 	aw2_black
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#1.Qtile]
							mat did2_bp25_black_t2 = r(estimate) \ r(se)	   
							mat did2_bp25_black_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#2.Qtile]
							mat did2_bp50_black_t2 = r(estimate) \ r(se)	   
							mat did2_bp50_black_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#3.Qtile]
							mat did2_bp75_black_t2 = r(estimate) \ r(se)	   
							mat did2_bp75_black_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#1.Qtile]
							mat did2_bp25_black_t3 = r(estimate) \ r(se)	   
							mat did2_bp25_black_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#2.Qtile]
							mat did2_bp50_black_t3 = r(estimate) \ r(se)	   
							mat did2_bp50_black_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#3.Qtile]
							mat did2_bp75_black_t3 = r(estimate) \ r(se)	   
							mat did2_bp75_black_p_t3 = r(p) \ r(level)							
						estadd local hascontrols 	"Y"
						estadd local hastimefe 		"Y"
						estadd local hasindustryfe 	"Y"
						estadd local hasstatefe 	"Y"
						estadd local hasstateyearfe "N"				
					
					*Alternative cross-industry design (w. state-by-year FE)
						reghdfe ln_annual_wage ib0.covered_1966##ib1.time##ib4.Qtile ib1.sex  $covar i.state_group i.fe_state_year [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 , absorb(i.industry) vce(cluster industry) 						
						eststo 	aw3_black
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#1.Qtile]
							mat did3_bp25_black_t2 = r(estimate) \ r(se)	   
							mat did3_bp25_black_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#2.Qtile]
							mat did3_bp50_black_t2 = r(estimate) \ r(se)	   
							mat did3_bp50_black_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#3.Qtile]
							mat did3_bp75_black_t2 = r(estimate) \ r(se)	   
							mat did3_bp75_black_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#1.Qtile]
							mat did3_bp25_black_t3 = r(estimate) \ r(se)	   
							mat did3_bp25_black_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#2.Qtile]
							mat did3_bp50_black_t3 = r(estimate) \ r(se)	   
							mat did3_bp50_black_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#3.Qtile]
							mat did3_bp75_black_t3 = r(estimate) \ r(se)	   
							mat did3_bp75_black_p_t3 = r(p) \ r(level)							
						estadd local hascontrols 	"Y"
						estadd local hastimefe 		"Y"
						estadd local hasindustryfe 	"Y"
						estadd local hasstatefe 	"N"
						estadd local hasstateyearfe "Y"								
				
				**(iii) White
						*baseline cross-industry design 
						reghdfe ln_annual_wage ib0.covered_1966##ib1.time##ib4.Qtile ib1.sex  $covar [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 , absorb(i.industry) vce(cluster industry) 				
						eststo 	aw1_white
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#1.Qtile]
							mat did1_bp25_white_t2 = r(estimate) \ r(se)	   
							mat did1_bp25_white_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#2.Qtile]
							mat did1_bp50_white_t2 = r(estimate) \ r(se)	   
							mat did1_bp50_white_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#3.Qtile]
							mat did1_bp75_white_t2 = r(estimate) \ r(se)	   
							mat did1_bp75_white_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#1.Qtile]
							mat did1_bp25_white_t3 = r(estimate) \ r(se)	   
							mat did1_bp25_white_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#2.Qtile]
							mat did1_bp50_white_t3 = r(estimate) \ r(se)	   
							mat did1_bp50_white_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#3.Qtile]
							mat did1_bp75_white_t3 = r(estimate) \ r(se)	   
							mat did1_bp75_white_p_t3 = r(p) \ r(level)							
						estadd local hascontrols 	"Y"
						estadd local hastimefe 		"Y"
						estadd local hasindustryfe 	"Y"
						estadd local hasstatefe 	"N"
						estadd local hasstateyearfe "N"
						
						*Alternative cross-industry design (w. state FE)
						reghdfe ln_annual_wage ib0.covered_1966##ib1.time##ib4.Qtile ib1.sex  $covar i.state_group [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 , absorb(i.industry) vce(cluster industry) 						
						eststo 	aw2_white
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#1.Qtile]
							mat did2_bp25_white_t2 = r(estimate) \ r(se)	   
							mat did2_bp25_white_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#2.Qtile]
							mat did2_bp50_white_t2 = r(estimate) \ r(se)	   
							mat did2_bp50_white_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#3.Qtile]
							mat did2_bp75_white_t2 = r(estimate) \ r(se)	   
							mat did2_bp75_white_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#1.Qtile]
							mat did2_bp25_white_t3 = r(estimate) \ r(se)	   
							mat did2_bp25_white_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#2.Qtile]
							mat did2_bp50_white_t3 = r(estimate) \ r(se)	   
							mat did2_bp50_white_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#3.Qtile]
							mat did2_bp75_white_t3 = r(estimate) \ r(se)	   
							mat did2_bp75_white_p_t3 = r(p) \ r(level)							
						estadd local hascontrols 	"Y"
						estadd local hastimefe 		"Y"
						estadd local hasindustryfe 	"Y"
						estadd local hasstatefe 	"Y"
						estadd local hasstateyearfe "N"				
					
					*Alternative cross-industry design (w. state-by-year FE)
						reghdfe ln_annual_wage ib0.covered_1966##ib1.time##ib4.Qtile ib1.sex  $covar i.state_group i.fe_state_year [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 , absorb(i.industry) vce(cluster industry) 						
						eststo 	aw3_white
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#1.Qtile]
							mat did3_bp25_white_t2 = r(estimate) \ r(se)	   
							mat did3_bp25_white_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#2.Qtile]
							mat did3_bp50_white_t2 = r(estimate) \ r(se)	   
							mat did3_bp50_white_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#2.time]+_b[1.covered_1966#2.time#3.Qtile]
							mat did3_bp75_white_t2 = r(estimate) \ r(se)	   
							mat did3_bp75_white_p_t2 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#1.Qtile]
							mat did3_bp25_white_t3 = r(estimate) \ r(se)	   
							mat did3_bp25_white_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#2.Qtile]
							mat did3_bp50_white_t3 = r(estimate) \ r(se)	   
							mat did3_bp50_white_p_t3 = r(p) \ r(level)
						lincom _b[1.covered_1966#3.time]+_b[1.covered_1966#3.time#3.Qtile]
							mat did3_bp75_white_t3 = r(estimate) \ r(se)	   
							mat did3_bp75_white_p_t3 = r(p) \ r(level)							
						estadd local hascontrols 	"Y"
						estadd local hastimefe 		"Y"
						estadd local hasindustryfe 	"Y"
						estadd local hasstatefe 	"N"
						estadd local hasstateyearfe "Y"	

				*combine matrices of DinD estimates for workers above and below pctile
						mat did_all_t2 	= (did1_bp25_all_t2,did2_bp25_all_t2, did3_bp25_all_t2, did1_bp25_black_t2,did2_bp25_black_t2, did3_bp25_black_t2, did1_bp25_white_t2,did2_bp25_white_t2, did3_bp25_white_t2 ///
											\ did1_bp50_all_t2,did2_bp50_all_t2, did3_bp50_all_t2, did1_bp50_black_t2,did2_bp50_black_t2, did3_bp50_black_t2, did1_bp50_white_t2,did2_bp50_white_t2, did3_bp50_white_t2 ///
											\  did1_bp75_all_t2,did2_bp75_all_t2, did3_bp75_all_t2, did1_bp75_black_t2,did2_bp75_black_t2, did3_bp75_black_t2, did1_bp75_white_t2,did2_bp75_white_t2, did3_bp75_white_t2) 
						mat did_all_p_t2 	= (did1_bp25_all_p_t2,did2_bp25_all_p_t2, did3_bp25_all_p_t2, did1_bp25_black_p_t2,did2_bp25_black_p_t2, did3_bp25_black_p_t2, did1_bp25_white_p_t2,did2_bp25_white_p_t2, did3_bp25_white_p_t2 ///
											\ did1_bp50_all_p_t2,did2_bp50_all_p_t2, did3_bp50_all_p_t2, did1_bp50_black_p_t2,did2_bp50_black_p_t2, did3_bp50_black_p_t2, did1_bp50_white_p_t2,did2_bp50_white_p_t2, did3_bp50_white_p_t2 ///
											\  did1_bp75_all_p_t2,did2_bp75_all_p_t2, did3_bp75_all_p_t2, did1_bp75_black_p_t2,did2_bp75_black_p_t2, did3_bp75_black_p_t2, did1_bp75_white_p_t2,did2_bp75_white_p_t2, did3_bp75_white_p_t2) 
						mat did_all_t3 	= (did1_bp25_all_t3,did2_bp25_all_t3, did3_bp25_all_t3, did1_bp25_black_t3,did2_bp25_black_t3, did3_bp25_black_t3, did1_bp25_white_t3,did2_bp25_white_t3, did3_bp25_white_t3 ///
											\ did1_bp50_all_t3,did2_bp50_all_t3, did3_bp50_all_t3, did1_bp50_black_t3,did2_bp50_black_t3, did3_bp50_black_t3, did1_bp50_white_t3,did2_bp50_white_t3, did3_bp50_white_t3 ///
											\  did1_bp75_all_t3,did2_bp75_all_t3, did3_bp75_all_t3, did1_bp75_black_t3,did2_bp75_black_t3, did3_bp75_black_t3, did1_bp75_white_t3,did2_bp75_white_t3, did3_bp75_white_t3) 
						mat did_all_p_t3 	= (did1_bp25_all_p_t3,did2_bp25_all_p_t3, did3_bp25_all_p_t3, did1_bp25_black_p_t3,did2_bp25_black_p_t3, did3_bp25_black_p_t3, did1_bp25_white_p_t3,did2_bp25_white_p_t3, did3_bp25_white_p_t3 ///
											\ did1_bp50_all_p_t3,did2_bp50_all_p_t3, did3_bp50_all_p_t3, did1_bp50_black_p_t3,did2_bp50_black_p_t3, did3_bp50_black_p_t3, did1_bp50_white_p_t3,did2_bp50_white_p_t3, did3_bp50_white_p_t3 ///
											\  did1_bp75_all_p_t3,did2_bp75_all_p_t3, did3_bp75_all_p_t3, did1_bp75_black_p_t3,did2_bp75_black_p_t3, did3_bp75_black_p_t3, did1_bp75_white_p_t3,did2_bp75_white_p_t3, did3_bp75_white_p_t3) 
						
				esttab	matrix(did_all_t2,fmt(%9.3f)) ///
						using "tables/table_aw_qtile.tex", replace  label fragment ///
						nolines booktabs ///
						nonumbers nomtitles collabels(none) noobs   				
				esttab  aw1_all aw2_all aw3_all aw1_black aw2_black aw3_black aw1_white aw2_white aw3_white ///
						using "tables/table_aw_qtile.tex", append  label fragment ///
						nolines booktabs ///
						nonumbers nomtitles collabels(none) noobs   ///
						cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
						keep(1.covered_1966#2.time)   ///				
						coeflabel(1.covered_1966#2.time "\hspace{1cm}{\textit{4th Quartile}}")	
				esttab	matrix(did_all_t3,fmt(%9.3f)) ///
						using "tables/table_aw_qtile.tex", append  label fragment ///
						nolines booktabs ///
						nonumbers nomtitles collabels(none)		
				esttab  aw1_all aw2_all aw3_all aw1_black aw2_black aw3_black aw1_white aw2_white aw3_white ///
						using "tables/table_aw_qtile.tex", append  label fragment ///
						nolines nonumbers nomtitles collabels(none) noobs ///
						cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
						keep(1.covered_1966#3.time)   ///				
						coeflabel(1.covered_1966#3.time "\hspace{1cm}{\textit{4th Quartile}}")	///
						stats(N hascontrols hastimefe hasindustryfe hasstatefe hasstateyearfe, fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Industry FE" "State FE" "State-by-year FE")) onecell 		

*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
*FIGURE 6 [PAPER & SLIDES]: HETEROGENEITY OF THE WAGE EFFECT OF THE 1966 FLSA
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	*FIGURE 6a: BY LEVEL OF EDUCATION 
	*low-education
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long ib1.sex ib100.race $covar [pw=weight] ///
			if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 &  inrange(educ,010,071) & !inlist(educ,000,999)  ///
			, absorb(i.industry) vce(cluster industry) 
	eststo aw_long_ls
	*add-on TABLE 4
	mat tab_ls = _b[inter_industry_long_1967], _b[inter_industry_long_1967] - 1.96* _se[inter_industry_long_1967] , _b[inter_industry_long_1967] + 1.96* _se[inter_industry_long_1967]  
	

	*high-education
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long ib1.sex ib100.race $covar [pw=weight] ///
			if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 &  inrange(educ,072,125) & !inlist(educ,000,999) ///
			, absorb(i.industry) vce(cluster industry) 
	eststo aw_long_hs	
	*add-on TABLE 4			
	mat tab_hs = _b[inter_industry_long_1967], _b[inter_industry_long_1967] - 1.96* _se[inter_industry_long_1967] , _b[inter_industry_long_1967] + 1.96* _se[inter_industry_long_1967]  
					
	*output graph
	coefplot(aw_long_ls, baselevels omitted keep(inter*) label("Low-education") connect(direct) lcolor(mydeepblue) lw(medthick) msymbol(square) msize(medlarge) mcolor(mydeepblue) mlcolor(mydeepblue)) ///
			(aw_long_hs, baselevels omitted keep(inter*) label("High-education")  connect(direct) lcolor(mydeepblue) lw(medthick) msymbol(square_hollow) msize(large) mcolor(mydeepblue) mlcolor(mydeepblue)  ) ///
			  , vertical levels(95) pstyle(matrix) noci  ///
			  yline(0, lstyle(major_grid))  ytitle("Estimated Effect on Log Annual Earnings", color(gs4)) ///
			  ylabel(-0.1(.1).2,labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
			  xline(6.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1961" 5 "1965" 10 "1970" 15 "1975" 20 "1980") ///
			  legend(order(1 "Low-education" 2 "High-education") ring(0) position(4) bmargin(large) color(gs1) c(1) region(col(white)))
	gr export "figures/aw_lshs.pdf", replace	
	
	*FIGURE 6b: BY RACE 
	*black
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long ib1.sex $covar  [pw=weight] if covered_all  & race==200 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry)
	eststo aw_long_black
	*add-on TABLE 4
	mat tab_black = _b[inter_industry_long_1967], _b[inter_industry_long_1967] - 1.96* _se[inter_industry_long_1967] , _b[inter_industry_long_1967] + 1.96* _se[inter_industry_long_1967]  

	*white
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long ib1.sex $covar  [pw=weight] if covered_all  & race==100  & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry)
	eststo aw_long_white				
	*add-on TABLE 4
	mat tab_white = _b[inter_industry_long_1967], _b[inter_industry_long_1967] - 1.96* _se[inter_industry_long_1967] , _b[inter_industry_long_1967] + 1.96* _se[inter_industry_long_1967]  
		
	*output graph	
	coefplot(aw_long_white, baselevels omitted keep(inter*) label("White") connect(direct) lcolor(mydeepblue) lw(medthick) msize(medlarge) mcolor(mydeepblue) mlcolor(mydeepblue)) ///
			(aw_long_black, baselevels omitted keep(inter*) label("Black")  connect(direct) lcolor(mydeepblue) lw(medthick) msymbol(triangle_hollow) msize(large) mcolor(mydeepblue) mlcolor(mydeepblue)  ) ///
			  , vertical levels(95) pstyle(matrix) noci  ///
			  yline(0, lstyle(major_grid))  ytitle("Estimated Effect on Log Annual Earnings", color(gs4)) ///
			  ylabel(-0.1(.1).2,labsize(medsmall) labcolor(gs4))  graphregion(color(white)) ///
			  xline(6.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1961" 5 "1965" 10 "1970" 15 "1975" 20 "1980") ///
			  legend(order(1 "White" 2 "Black") ring(0) position(4) bmargin(large) color(gs1) c(1) region(col(white))) 
	gr export "figures/aw_black_white.pdf", replace	
	
	
	
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
*FIGURE D4 [APPENDIX]: HETEROGENEITY OF THE WAGE EFFECT OF THE 1966 FLSA BY LEVEL OF EDUCATION AMOMG BLACK WORKERS	
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	*FIGURE D4a: AMONG BLACKS	
	*low-education
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long ib1.sex $covar [pw=weight] ///
			if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 &  inrange(educ,010,071) & !inlist(educ,000,999) ///
			, absorb(i.industry) vce(cluster industry) 
	eststo aw_long_ls_black
	

	**high-education
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long ib1.sex $covar [pw=weight] ///
			if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 &  inrange(educ,072,125) & !inlist(educ,000,999) ///
			, absorb(i.industry) vce(cluster industry) 
	eststo aw_long_hs_black	
		
	*output graph	
	coefplot(aw_long_ls_black, baselevels omitted keep(inter*) label("Low-education") connect(direct) lcolor(mydeepblue) lw(medthick) msymbol(triangle) msize(medlarge) mcolor(mydeepblue) mlcolor(mydeepblue)) ///
			(aw_long_hs_black, baselevels omitted keep(inter*) label("High-education")  connect(direct) lcolor(mydeepblue) lw(medthick) msymbol(triangle_hollow) msize(large) mcolor(mydeepblue) mlcolor(mydeepblue)  ) ///
			  , vertical levels(95) pstyle(matrix) noci  ///
			  yline(0, lstyle(major_grid))  ytitle("Estimated Effect on Log Annual Earnings", color(gs4)) ///
			  ylabel(-0.1(.1).2,labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
			  xline(6.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1961" 5 "1965" 10 "1970" 15 "1975" 20 "1980") ///
			  legend(order(1 "Low-education" 2 "High-education") ring(0) position(4) bmargin(large) color(gs1) c(1) region(col(white)))
	gr export "figures/aw_lshs_black.pdf", replace

	*FIGURE D4a: AMONG WHITES	
	*low-education
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long ib1.sex $covar [pw=weight] ///
			if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 &  inrange(educ,010,071) & !inlist(educ,000,999) ///
			, absorb(i.industry) vce(cluster industry) 
	eststo aw_long_ls_white
	
	*high-education
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long ib1.sex $covar [pw=weight] ///
			if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 &  inrange(educ,072,125) & !inlist(educ,000,999) ///
			, absorb(i.industry) vce(cluster industry) 
	eststo aw_long_hs_white	
	
	*output graph	
	coefplot(aw_long_ls_white, baselevels omitted keep(inter*) label("Low-education") connect(direct) lcolor(mydeepblue) lw(medthick) msymbol(circle) msize(medlarge) mcolor(mydeepblue) mlcolor(mydeepblue)) ///
			(aw_long_hs_white, baselevels omitted keep(inter*) label("High-education")  connect(direct) lcolor(mydeepblue) lw(medthick) msymbol(circle_hollow) msize(large) mcolor(mydeepblue) mlcolor(mydeepblue)  ) ///
			  , vertical levels(95) pstyle(matrix) noci  ///
			  yline(0, lstyle(major_grid))  ytitle("Estimated Effect on Log Annual Earnings", color(gs4)) ///
			  ylabel(-0.1(.1).2,labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
			  xline(6.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1961" 5 "1965" 10 "1970" 15 "1975" 20 "1980") ///
			  legend(order(1 "Low-education" 2 "High-education") ring(0) position(4) bmargin(large) color(gs1) c(1) region(col(white)))
	gr export "figures/aw_lshs_white.pdf", replace
		

*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
*TABLE 5 [PAPER & SLIDES]: WAGE EFFECT BY RACE
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	**BASELINE
	*black -- baseline
	reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex $covar [pw=weight] if covered_all  & race==200  & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry)
	eststo  aw_short_black
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatefe 	"N"
			estadd local hasstateyearfe "N"

	*white	-- baseline	
	reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex $covar  [pw=weight] if covered_all  & race==100 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry)
	eststo  aw_short_white
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatefe 	"N"
			estadd local hasstateyearfe "N"		

	**WITH STATE AND INDUSTRY FE
	*black -- baseline
	reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex $covar  i.state_group [pw=weight] if covered_all  & race==200  & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry)
	eststo  aw_short_black_sfe
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatefe 	"Y"
			estadd local hasstateyearfe "N"

	*white	-- baseline	
	reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex $covar   i.state_group [pw=weight] if covered_all  & race==100 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry)
	eststo  aw_short_white_sfe
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatefe 	"Y"
			estadd local hasstateyearfe "N"		
	
	**WITH INDUSTRY AND STATE*YEAR FE
	*black -- baseline with state FE and state*year FE (note: removed years 1961 and 1962)
	reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex $covar  i.fe_state_year [pw=weight] if covered_all  & race==200  & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & year!=1961, absorb(i.industry) vce(cluster industry)
	eststo  aw_short_black_syfe
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"
			estadd local hasstatefe 	"N"
			estadd local hasstateyearfe "Y"

	*white -- baseline with state FE and state*year FE
	reghdfe ln_annual_wage covered_1966 ib1.time $inter_industry_short ib1.sex $covar   i.fe_state_year [pw=weight] if covered_all  & race==100 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry)
	eststo  aw_short_white_syfe
			estadd local hascontrols 	"Y"
			estadd local hastimefe 		"Y"
			estadd local hasindustryfe 	"Y"		
			estadd local hasstatefe 	"N"
			estadd local hasstateyearfe "Y"					
						
	*output table
	esttab  aw_short_black aw_short_white aw_short_black_sfe aw_short_white_sfe aw_short_black_syfe aw_short_white_syfe   ///
			using "tables/table_aw_black_white_wwosfesyfe.tex",  replace label fragment ///
			nolines  posthead(\cmidrule(lr){2-2} \cmidrule(lr){3-3} \cmidrule(lr){4-4} \cmidrule(lr){5-5} \cmidrule(lr){6-6} \cmidrule(lr){7-7}) prefoot(\midrule) postfoot(\bottomrule \bottomrule) booktabs	///							
			nonumbers mtitle("Black" "White" "Black" "White" "Black" "White") collabels(none)  ///
			cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
			refcat(inter_industry_short_2 "Covered in 1967 $\times$", nolabel) ///
			keep(inter_industry_short_2 inter_industry_short_3)   ///
			coeflabel(inter_industry_short_2 "\hspace{0.5cm}{1967-1972}" inter_industry_short_3 "\hspace{0.5cm}{1973-1980}") ///
			stats(N hascontrols hastimefe hasindustryfe hasstatefe hasstateyearfe, ///
			fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Industry FE" "State FE" "State-by-year FE")) onecell 

*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
*FIGURE D3 [APPENDIX]: IMPACT OF THE 1966 FLSA ON ANNUAL EARNINGS BY RACE
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
	*Blacks in treated industries vs. Blacks + Whites in control industries
	*Whites in treated industries vs. Blacks + Whites in control industries
	
	gen white_control = (race==100 & covered_1966==0) //  whites in the control group only
	gen black_control = (race==200 & covered_1966==0) //  blacks in the control group only		
	
	*black
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long ib1.sex $covar  [pw=weight] ///
			if covered_all  & (white_control | race==200) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry)
			eststo aw_long_black_nonsep

	*white
	reghdfe ln_annual_wage covered_1966 ib1965.year $inter_industry_long ib1.sex $covar  [pw=weight] ///
			if covered_all  & (black_control | race==100)  & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry)
			eststo aw_long_white_nonsep				

	coefplot(aw_long_white_nonsep, baselevels omitted keep(inter*) label("White") connect(direct) lcolor(mydeepblue) lw(medthick) msize(medlarge) mcolor(mydeepblue) mlcolor(mydeepblue)) ///
			(aw_long_black_nonsep, baselevels omitted keep(inter*) label("Black")  connect(direct) lcolor(mydeepblue) lw(medthick) msymbol(triangle_hollow) msize(large) mcolor(mydeepblue) mlcolor(mydeepblue)  ) ///
			  , vertical levels(95) pstyle(matrix)  noci  ///
			  yline(0, lstyle(major_grid))  ytitle("Estimated Effect on Log Annual Earnings", color(gs4)) ///
			  ylabel(-0.1(.1).2,labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
			  subtitle("Industries covered in 1967 vs. in 1938", color(gs6)) ///
			  xline(6.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1961" 5 "1965" 10 "1970" 15 "1975" 20 "1980") ///
			  legend(order(1 "White" 2 "Black") ring(0) position(4) bmargin(large) color(gs1) c(1) region(col(white)))
	gr export "figures/aw_black_white_nonsep.pdf", replace	
			
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
*TABLE 3 [PAPER & SLIDES]: PREDICTED WAGE EFFECT
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
	*%-----TAKING INTO SPILLOVER EFFECTS UP TO 1.15*NEW MW IN CPS AND WINSORIZE BELOW $0.10 IN THE MW
	su hmw_1966FLSA if year==1967 & covered_1966 & industry!=1
	global federal_mw_1967 `r(mean)'
	cap drop share_atb_mw_1967
	gen share_atb_mw_1967 = (flag_employed & in_sample & inlist(race,100,200) & covered_1966 & inlist(year,1966) & hourly_wage <=1.15*$federal_mw_1967 & hourly_wage>0)
	
	replace hourly_wage=0.64171943 if hourly_wage<0.64171943  & hourly_wage!=0 // 0.64171943 is equivalent to $0.10
	
	*all 
		su share_atb_mw_1967  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,100,200) &  inlist(year,1966) [w=weight], meanonly
		local share_mw `r(mean)'
		su hourly_wage  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,100,200) &  inlist(year,1966) & hourly_wage <=1.15*$federal_mw_1967 & hourly_wage>0 [w=weight], meanonly
		local hourly_wage `r(mean)'
		mat res_all = `share_mw',(($federal_mw_1967 - `hourly_wage')/`hourly_wage'), `share_mw'*(($federal_mw_1967 - `hourly_wage')/`hourly_wage')
		mat rownames res_all="All" 
	
	*low-education 
		su share_atb_mw_1967  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,100,200) &  year==1966 & inrange(educ,010,071) & !inlist(educ,000,999) [w=weight], meanonly
		local share_mw `r(mean)'
		su hourly_wage  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,100,200) &  year==1966 & hourly_wage <=1.15*$federal_mw_1967 & hourly_wage>0 & inrange(educ,010,071) & !inlist(educ,000,999) [w=weight], meanonly
		local hourly_wage `r(mean)'
		mat res_ls = `share_mw',(($federal_mw_1967 - `hourly_wage')/`hourly_wage'), `share_mw'*(($federal_mw_1967 - `hourly_wage')/`hourly_wage')
		mat rownames res_ls="\hspace{3mm}{Low-education}"
				
	*high-education 
		su share_atb_mw_1967  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,100,200) &  year==1966 & inrange(educ,072,125) & !inlist(educ,000,999) [w=weight], meanonly
		local share_mw `r(mean)'
		su hourly_wage  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,100,200) &  year==1966 & hourly_wage <=1.15*$federal_mw_1967 & hourly_wage>0 & inrange(educ,072,125) & !inlist(educ,000,999) [w=weight], meanonly
		local hourly_wage `r(mean)'
		mat res_hs = `share_mw',(($federal_mw_1967 - `hourly_wage')/`hourly_wage'), `share_mw'*(($federal_mw_1967 - `hourly_wage')/`hourly_wage')
		mat rownames res_hs="\hspace{3mm}{High-education}"									

	*black 
		su share_atb_mw_1967  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,200) &  year==1966 [w=weight], meanonly
		local share_mw `r(mean)'
		su hourly_wage  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,200) &  year==1966 & hourly_wage <=1.15*$federal_mw_1967 & hourly_wage>0 [w=weight], meanonly
		local hourly_wage `r(mean)'
		mat res_black = `share_mw',(($federal_mw_1967 - `hourly_wage')/`hourly_wage'), `share_mw'*(($federal_mw_1967 - `hourly_wage')/`hourly_wage')
		mat rownames res_black="\hspace{3mm}{Black}"	
	
	*white 
		su share_atb_mw_1967  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,100) &  year==1966 [w=weight], meanonly
		local share_mw `r(mean)'
		su hourly_wage  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,100) &  year==1966 & hourly_wage <=1.15*$federal_mw_1967 & hourly_wage>0 [w=weight], meanonly
		local hourly_wage `r(mean)'
		mat res_white = `share_mw',(($federal_mw_1967 - `hourly_wage')/`hourly_wage'), `share_mw'*(($federal_mw_1967 - `hourly_wage')/`hourly_wage')
		mat rownames res_white="\hspace{3mm}{White}" 				
	
	*stack all matrices of results together
		mat res = res_all\res_ls\res_hs\res_black\res_white
		mat res = res*100
		mat li res

		mat est = tab_all\tab_ls\tab_hs\tab_black\tab_white
		*select point estimate (first column)
		mat est_point = est[1..5,1..1]
		*transform first column in log points into percentage points
		mata: st_matrix("est_point", exp(st_matrix("est_point")))
		mat one = J(5,1,1)	
		mat est_point = (est_point - one)
		mat est_point = est_point*100 
		mat li est_point
	
		mat res = (res,est_point)
		
	*output table to csv file 
		esttab 	matrix(res,fmt(%3.1f)) ///
				using "tables/table_aw_predictions_demog", replace label fragment ///
				refcat(\hspace{3mm}{Low-education} "\rule{0pt}{3ex}\emph{By education}" \hspace{3mm}{Black} "\rule{0pt}{3ex}\emph{By race}", nolabel) ///
				nolines posthead( "&\multicolumn{1}{c}{(1)} &\multicolumn{1}{c}{(2)} &\multicolumn{1}{c}{(3) = (1) $\times$ (2)} &\multicolumn{1}{c}{(4)} \\" ///
				\cmidrule{2-5} ///
				"& Share of workers & Avg increase & Predicted & Estimated \\" ///
				"& at or below  & in earnings for & increase in & increase in \\" ///
				"& the MW (\%)  & MW workers (\%) & earnings (\%) &  earnings (\%) \\" ///
				\cmidrule{2-5} \vspace{0.1cm}) ///
				postfoot(\bottomrule \bottomrule) booktabs	///							
				nonumbers nomtitles collabels(none) 													

 				
				
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
*TABLE E2 [APPENDIX]: WAGE EFFECT IN TREATED AND CONTROL INDUSTRIES, BY RACE AND EDUCATION LEVEL, USING THE CROSS-STATE DESIGN
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
			*all
			reghdfe ln_annual_wage nomw_1966 ib1.time $inter_state_short i.sex i.race $covar [pw=weight] ///
					if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, absorb(i.state_group) vce(cluster state_group)
			eststo  aw_state_short_all
			estadd 	local hascontrols "Y"
			estadd 	local hastimefe   "Y"
			estadd 	local hasstatefe  "Y"	
			
			*treatment
			reghdfe ln_annual_wage nomw_1966 ib1.time $inter_state_short ib1.sex $covar [pw=weight] ///
					if covered_1966  & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, absorb(i.state_group) vce(cluster state_group)
			eststo  aw_state_short_t
					estadd local hascontrols 	"Y"
					estadd local hastimefe 		"Y"
					estadd local hasstatefe 	"Y"
				
			*control group
			reghdfe ln_annual_wage nomw_1966 ib1.time $inter_state_short ib1.sex $covar [pw=weight] ///
					if covered_1938  & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, absorb(i.state_group) vce(cluster state_group)
			eststo  aw_state_short_c
					estadd local hascontrols 	"Y"
					estadd local hastimefe 		"Y"
					estadd local hasstatefe 	"Y"
					
			*black
			reghdfe ln_annual_wage nomw_1966 ib1.time $inter_state_short i.sex  $covar [pw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) & year!=1962 & state_group!=22, absorb(i.state_group) vce(cluster state_group)
			eststo  aw_state_short_black
					estadd local hascontrols 	"Y"
					estadd local hastimefe 		"Y"
					estadd local hasstatefe 	"Y"
					
			*white
			reghdfe ln_annual_wage nomw_1966 ib1.time $inter_state_short i.sex  $covar [pw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) & year!=1962 & state_group!=22, absorb(i.state_group) vce(cluster state_group)
			eststo  aw_state_short_white
					estadd local hascontrols 	"Y"
					estadd local hastimefe 		"Y"
					estadd local hasstatefe 	"Y"
					
			*low-education 
			reghdfe ln_annual_wage nomw_1966 ib1.time $inter_state_short i.sex i.race $covar [pw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,200) & inrange(year,1961,1980) ///
					&  inrange(educ,010,071) & !inlist(educ,000,999) & year!=1962 & state_group!=22, absorb(i.state_group) vce(cluster state_group)
			eststo  aw_state_short_ls
					estadd local hascontrols 	"Y"
					estadd local hastimefe 		"Y"
					estadd local hasstatefe 	"Y"			
			*high-education
			reghdfe ln_annual_wage nomw_1966 ib1.time $inter_state_short i.sex i.race $covar [pw=weight] ///
					if flag_employed & in_sample  & inrange(age,25,55) & inlist(race,100) & inrange(year,1961,1980) ///
					&  inrange(educ,072,125) & !inlist(educ,000,999) & year!=1962 & state_group!=22, absorb(i.state_group) vce(cluster state_group)
			eststo  aw_state_short_hs
					estadd local hascontrols 	"Y"
					estadd local hastimefe 		"Y"
					estadd local hasstatefe 	"Y"			
			
							
			esttab  aw_state_short_all aw_state_short_t aw_state_short_c aw_state_short_black aw_state_short_white aw_state_short_ls aw_state_short_hs  ///
					using "tables/table_aw_state_tc.tex",  replace label fragment ///
					nolines  posthead(\cmidrule{2-8}) prefoot(\midrule) postfoot(\bottomrule \bottomrule) booktabs ///
					nonumbers mtitle("All" "Treated" "Control" "Black" "White" "Low-educ." "High-educ") collabels(none)    ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					refcat(inter_state_short_2 "Strongly treated states $\times$", nolabel) ///
					keep(inter_state_short_2)   ///
					coeflabel(inter_state_short_2 "\hspace{0.5cm}{1967-1972}" ) ///
					stats(N hascontrols hastimefe hasstatefe , ///
					fmt(%11.0gc) label("Observations" "Controls" "Time FE" "State FE")) onecell 	
					
					
			**IN APPENDIX E.2 CROSS STATE DESIGN
			*share of mw workers in strongly vs. weakly treated states 
			su hmw_1966FLSA if year==1967 & covered_1966 & industry!=1
			global federal_mw_1967 `r(mean)'		
			cap drop share_atb_mw_1967
			gen share_atb_mw_1967 = (flag_employed & in_sample & inlist(race,100,200)  & inlist(year,1966) & hourly_wage <=1.15*$federal_mw_1967 & hourly_wage>0)
			
			*strongly treated states  
				su share_atb_mw_1967  if flag_employed & in_sample & inrange(age,25,55) & nomw_1966==1 & inlist(race,100,200) &  inlist(year,1966) [w=weight], meanonly
				local share_mw `r(mean)'
				di `share_mw'
			*weakly treated states  
				su share_atb_mw_1967  if flag_employed & in_sample & inrange(age,25,55) & nomw_1966==0 & inlist(race,100,200) &  inlist(year,1966) [w=weight], meanonly
				local share_mw `r(mean)'		
				di `share_mw'
					
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
*TABLE E2 [SLIDES ONLY]: WAGE EFFECT IN TREATED AND CONTROL INDUSTRIES
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
		esttab  aw_state_short_all aw_state_short_t aw_state_short_c   ///
					using "tables/table_aw_state_tc_slides.tex",  replace label fragment ///
					nolines  posthead(\cmidrule{2-4}) prefoot(\midrule) postfoot(\bottomrule \bottomrule) booktabs ///
					nonumbers mtitle("All" "Treated" "Control") collabels(none)    ///
					cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
					refcat(inter_state_short_2 "Strongly treated states $\times$", nolabel) ///
					keep(inter_state_short_2)   ///
					coeflabel(inter_state_short_2 "\hspace{0.5cm}{1967-1972}" ) ///
					stats(N hascontrols hastimefe hasstatefe , ///
					fmt(%11.0gc) label("Observations" "Controls" "Time FE" "State FE")) onecell 							
					

*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
*APPENDIX TABLE D2: IMPACT ON EARNINGS (CONDITIONAL OR UNCONDITIONAL ON WORKING), 1961-1980
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------				
	use "data/output/cps_master_individual_level.dta", clear	
	
	*All workers
	gen annual_earnings = . 
	replace annual_earnings = annual_wage if flag_employed
	replace annual_earnings = incwage*cpi_u_rs if flag_unemployed
	replace annual_earnings = incwage*cpi_u_rs if flag_nilf
	
	replace annual_earnings = 0 if (flag_unemployed | flag_nilf)
	
	
	*All workers 	
		keep if inrange(year,1961,1980)
			gen white = (race==100)
			gen male = (sex==1)
			
			gen full_time = (fullpart==1)
			gen full_year = (wkswork2==6)	
			
			gen low_education = .
			replace low_education = 1 if  inrange(educ,010,071) & !inlist(educ,000,999)
			replace low_education = 0 if  inrange(educ,072,125) & !inlist(educ,000,999) /*i.e. high-education workers*/
			gen married = (inlist(marst,1,2))	
			
			collapse (mean) annual_earnings  full_time_share = full_time full_year_share = full_year nomw_1966 ///
						age schooling low_education white_share = white married male_share = male time time_emp  [pw=weight] ///
						if inrange(age,25,55) & inlist(race,100,200) & inlist(sex,1,2) & year!=1962 & state_group!=22 &  (flag_employed & in_sample)  ///
						, by(year state_group)
			
			gen ln_annual_earnings =. 
			replace ln_annual_earnings = log(annual_earnings)
			
	
			*all controls
			reg ln_annual_earnings nomw_1966##ib1.time age  schooling married full_time_share full_year_share  white_share male_share i.state_group , cluster(state_group)
				eststo ae_workers_1	
				estadd local hasallcontrols 	"Y"
				estadd local hassubsetcontrols 	"N"
				estadd local hastimefe 			"Y"
				estadd local hasstatefe 		"Y"			
			
			*subset of controls
			reg ln_annual_earnings nomw_1966##ib1.time age schooling married white_share male_share i.state_group, cluster(state_group)
				eststo ae_workers_2		
				estadd local hasallcontrols 	"N"
				estadd local hassubsetcontrols 	"Y"
				estadd local hastimefe 			"Y"
				estadd local hasstatefe 		"Y"		
				
	*All workers, unemployed or nilf
		use "data/output/cps_master_individual_level.dta", clear	
	
		gen annual_earnings = . 
		replace annual_earnings = annual_wage if flag_employed
		replace annual_earnings = incwage*cpi_u_rs if flag_unemployed
		replace annual_earnings = incwage*cpi_u_rs if flag_nilf
	
		replace annual_earnings = 0 if (flag_unemployed | flag_nilf)	
		
		keep if inrange(year,1961,1980)
			gen white = (race==100)
			gen male = (sex==1)
			
			gen low_education = .
			replace low_education = 1 if  inrange(educ,010,071) & !inlist(educ,000,999)
			replace low_education = 0 if  inrange(educ,072,125) & !inlist(educ,000,999) /*i.e. high-education workers*/
			gen married = (inlist(marst,1,2))	
			
			collapse (mean) annual_earnings  nomw_1966 ///
						age schooling low_education white_share = white married male_share = male time time_emp  [pw=weight] ///
						if inrange(age,25,55) & inlist(race,100,200) & inlist(sex,1,2) & year!=1962 & state_group!=22 &   ((flag_employed & in_sample) | flag_unemployed | flag_nilf)  ///
						, by(year state_group)
			
			gen ln_annual_earnings =. 
			replace ln_annual_earnings = log(annual_earnings)
		
			reg ln_annual_earnings nomw_1966##ib1.time age  schooling married white_share male_share i.state_group , cluster(state_group)
				eststo ae_all_2
				estadd local hasallcontrols 	"N"
				estadd local hassubsetcontrols 	"Y"
				estadd local hastimefe 			"Y"
				estadd local hasstatefe 		"Y"						
				
			
		esttab 	ae_workers_1 ae_workers_2 ae_all_2 /// 
				using "tables/table_earnings_unconditional_state_level.tex",  replace label fragment ///
				nolines  prefoot(\midrule) postfoot(\bottomrule \bottomrule) booktabs ///
				nonumbers nomtitles collabels(none)    ///
				refcat(1.nomw_1966#2.time "Strongly treated $\times$", nolabel) ///
				keep(1.nomw_1966#2.time)   ///
				coeflabel(1.nomw_1966#2.time "\hspace{0.5cm}{1967-1972}" ) ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///
				stats(N hasallcontrols hassubsetcontrols hastimefe hasstatefe , ///
				fmt(%11.0gc) label("Observations" "Controls (all)" "Controls (subset)" "Time FE" "State FE")) onecell 			

	
	
	
