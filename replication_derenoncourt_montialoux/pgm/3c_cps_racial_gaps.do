*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*program: 		Estimate effects of 1966 amendements on wages, racial gaps and employment 
*first created: 01/19/2018
*last updated:  08/31/2020
*structure:    	Create interaction variables and list of covariates
*				Racial gaps
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
	
clear all
set more off
set matsize 10000
set maxvar 10000

use "data/output/cps_master_individual_level.dta", clear	

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
***RACIAL GAPS
*******************************************************************************************************************************************************************************************************************************	   
	 use "data/output/cps_master_individual_level.dta", clear
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*
*FIGURE 10  [PAPER & SLIDES]: ADJUSTED RACIAL WAGE GAPS
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*
		*FIGURE 10a [PAPER & SLIDES]: WAGE EFFECTS IN LEVELS BY RACE AND TREATMENT STATUS
		**PROGRAMS TO CALCULATE WAGE LEVELS
		*control group
		capture program drop levels_c
		program define levels_c
		nlcom (b1961: (_b[_cons]+_b[1961.year])) ///
			  (b1962: (_b[_cons]+0.5*_b[1961.year]+0.5*_b[1963.year])) ///
			  (b1963: (_b[_cons]+_b[1963.year])) ///
			  (b1964: (_b[_cons]+_b[1964.year])) ///
			  (b1965: (_b[_cons]+_b[1965.year])) ///
			  (b1966: (_b[_cons]+_b[1966.year])) ///
			  (b1967: (_b[_cons]+_b[1967.year])) ///
			  (b1968: (_b[_cons]+_b[1968.year])) ///
			  (b1969: (_b[_cons]+_b[1969.year])) ///
			  (b1970: (_b[_cons]+_b[1970.year])) ///
			  (b1971: (_b[_cons]+_b[1971.year])) ///
			  (b1972: (_b[_cons]+_b[1972.year])) ///
			  (b1973: (_b[_cons]+_b[1973.year])) ///
			  (b1974: (_b[_cons]+_b[1974.year])) ///
			  (b1975: (_b[_cons]+_b[1975.year])) ///
			  (b1976: (_b[_cons]+_b[1976.year])) ///
			  (b1977: (_b[_cons]+_b[1977.year])) ///
			  (b1978: (_b[_cons]+_b[1978.year])) ///
			  (b1979: (_b[_cons]+_b[1979.year])) ///
			  (b1980: (_b[_cons]+_b[1980.year])) ///			  
		  , post
		end
	   
		*treatment group
		capture program drop levels_t
		program define levels_t
		args  a 			
		nlcom (b1961: (_b[_cons]+_b[1961.year]+_b[`a']+_b[`a'#1961.year])) ///
			  (b1962: (_b[_cons]+0.5*(_b[1961.year]+_b[`a']+_b[`a'#1961.year])+0.5*(_b[1963.year]+_b[`a']+_b[`a'#1963.year]))) ///
			  (b1963: (_b[_cons]+_b[1963.year]+_b[`a']+_b[`a'#1963.year])) ///
			  (b1964: (_b[_cons]+_b[1964.year]+_b[`a']+_b[`a'#1964.year])) ///
			  (b1965: (_b[_cons]+_b[1965.year]+_b[`a']+_b[`a'#1965.year])) ///
			  (b1966: (_b[_cons]+_b[1966.year]+_b[`a']+_b[`a'#1966.year])) ///
			  (b1967: (_b[_cons]+_b[1967.year]+_b[`a']+_b[`a'#1967.year])) ///
			  (b1968: (_b[_cons]+_b[1968.year]+_b[`a']+_b[`a'#1968.year])) ///
			  (b1969: (_b[_cons]+_b[1969.year]+_b[`a']+_b[`a'#1969.year])) ///
			  (b1970: (_b[_cons]+_b[1970.year]+_b[`a']+_b[`a'#1970.year])) ///
			  (b1971: (_b[_cons]+_b[1971.year]+_b[`a']+_b[`a'#1971.year])) ///
			  (b1972: (_b[_cons]+_b[1972.year]+_b[`a']+_b[`a'#1972.year])) ///
			  (b1973: (_b[_cons]+_b[1973.year]+_b[`a']+_b[`a'#1973.year])) ///
			  (b1974: (_b[_cons]+_b[1974.year]+_b[`a']+_b[`a'#1974.year])) ///
			  (b1975: (_b[_cons]+_b[1975.year]+_b[`a']+_b[`a'#1975.year])) ///
			  (b1976: (_b[_cons]+_b[1976.year]+_b[`a']+_b[`a'#1976.year])) ///
			  (b1977: (_b[_cons]+_b[1977.year]+_b[`a']+_b[`a'#1977.year])) ///
			  (b1978: (_b[_cons]+_b[1978.year]+_b[`a']+_b[`a'#1978.year])) ///
			  (b1979: (_b[_cons]+_b[1979.year]+_b[`a']+_b[`a'#1979.year])) ///
			  (b1980: (_b[_cons]+_b[1980.year]+_b[`a']+_b[`a'#1980.year])) ///			  
		  , post
			end		  		
		 
		*whites
			**control
			xi:reg ln_annual_wage covered_1966##ib1961.year ib1.sex ib12.schooling ib5.exp ib1.fullpart ib6.wkswork2 ib40.ahrsworkt ib1.marst ib1.occupation ib13.industry  [pw=weight] if covered_all & race==100  & flag_employed & in_sample & inrange(age,25,55) & inrange(year,1961,1980) & year!=1962, robust cluster(industry)
			levels_c					
			eststo 	aw_levels_white_c

			**treatment
			xi:reg ln_annual_wage covered_1966##ib1961.year ib1.sex ib12.schooling ib5.exp ib1.fullpart ib6.wkswork2 ib40.ahrsworkt ib1.marst ib1.occupation ib13.industry  [pw=weight] if covered_all & race==100  & flag_employed & in_sample & inrange(age,25,55)  & inrange(year,1961,1980) & year!=1962, robust cluster(industry)		
			levels_t 1.covered_1966 					
			eststo 	aw_levels_white_t

		*blacks
			**control
			xi:reg ln_annual_wage covered_1966##ib1961.year ib1.sex ib12.schooling ib5.exp ib1.fullpart ib6.wkswork2 ib40.ahrsworkt ib1.marst ib1.occupation ib13.industry  [pw=weight] if covered_all & race==200 & flag_employed & in_sample & inrange(age,25,55)  & inrange(year,1961,1980) & year!=1962, robust cluster(state_group)
			levels_c					
			eststo 	aw_levels_black_c

			**treatment	
			xi:reg ln_annual_wage covered_1966##ib1961.year ib1.sex ib12.schooling ib5.exp ib1.fullpart ib6.wkswork2 ib40.ahrsworkt ib1.marst ib1.occupation ib13.industry  [pw=weight] if covered_all & race==200  & flag_employed & in_sample & inrange(age,25,55)  & inrange(year,1961,1980) & year!=1962, robust cluster(state_group)
			levels_t 1.covered_1966 					
			eststo 	aw_levels_black_t	

			coefplot(aw_levels_white_c, label("Covered in 1938")  connect(direct) lcolor(mydeepblue) lw(medthick) msize(medlarge) mfcolor(mydeepblue) mlcolor(mydeepblue) mlw(medthick)) ///
					(aw_levels_white_t, label("Covered in 1967")  connect(direct) lcolor(myarticblue) lw(medthick) msize(medlarge) mfcolor(myarticblue) mlcolor(myarticblue) mlw(medthick)) ///
					(aw_levels_black_c, label("Covered in 1938")  connect(direct) lcolor(mydeepblue) lw(medthick) msymbol(triangle_hollow) msize(large) mcolor(mydeepblue) mlcolor(mydeepblue)) ///
					(aw_levels_black_t, label("Covered in 1967")  connect(direct) lcolor(myarticblue) lw(medthick) msymbol(triangle_hollow) msize(large) mcolor(myarticblue) mlcolor(myarticblue)) ///
					 , vertical levels(95) pstyle(matrix) noci ///
					 connect(direct) lcolor(mydeepblue) lw(medthick) msize(large) mcolor(mydeepblue) ytitle("Average Log Annual Wages", color(gs4)) ///
					 ylabel(,labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
					 xline(6.5, lcolor(myred))  xlabel(1 "1961" 5 "1965" 10 "1970" 15 "1975" 20 "1980",labsize(medsmall) labcolor(gs4)) ///
					 legend(order(1 "White - covered in 1938" 2 "White - Covered in 1967" 3 "Black - covered in 1938" 4 "Black - Covered in 1967") ring(0) position(4) bmargin(large) color(gs1) c(1) region(col(white)))
			gr export "figures/aw_levels_black_white_tc.pdf", replace	
		

		*FIGURE 10b [PAPER & SLIDES]: ADJUSTED RACIAL WAGE GAPS, BY TREATEMENT STATUS
		**1961-1980
		capture program drop gaps_adj_1980
		program define gaps_adj_1980
		args  a 			
		nlcom 	  (bw1961: (-_b[`a']-_b[`a'#1961.year])) ///
				  (bw1962: (-_b[`a']-(_b[`a'#1961.year]+_b[`a'#1963.year])/2)) ///
				  (bw1963: (-_b[`a']-_b[`a'#1963.year])) ///
				  (bw1964: (-_b[`a']-_b[`a'#1964.year])) ///
				  (bw1965: (-_b[`a']-_b[`a'#1965.year])) ///
				  (bw1966: (-_b[`a']-_b[`a'#1966.year])) ///
				  (bw1967: (-_b[`a']-_b[`a'#1967.year])) ///
				  (bw1968: (-_b[`a']-_b[`a'#1968.year])) ///
				  (bw1969: (-_b[`a']-_b[`a'#1969.year])) ///
				  (bw1970: (-_b[`a']-_b[`a'#1970.year])) ///
				  (bw1971: (-_b[`a']-_b[`a'#1971.year])) ///
				  (bw1972: (-_b[`a']-_b[`a'#1972.year])) ///
				  (bw1973: (-_b[`a']-_b[`a'#1973.year])) ///
				  (bw1974: (-_b[`a']-_b[`a'#1974.year])) ///
				  (bw1975: (-_b[`a']-_b[`a'#1975.year])) ///
				  (bw1976: (-_b[`a']-_b[`a'#1976.year])) ///
				  (bw1977: (-_b[`a']-_b[`a'#1977.year])) ///
				  (bw1978: (-_b[`a']-_b[`a'#1978.year])) ///
				  (bw1979: (-_b[`a']-_b[`a'#1979.year])) ///
				  (bw1980: (-_b[`a']-_b[`a'#1980.year])) ///			  
				  , post
		end
		
		*Covered in 1938	
		qui reg ln_annual_wage ib100.race##ib1965.year ib1.sex $covar i.industry  [pw=weight] ///
				if covered_1938 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962  
		gaps_adj_1980 200.race
		eststo adj_rg_c_1961_1980
		
		*Covered in 1967	
		qui reg ln_annual_wage ib100.race##ib1965.year ib1.sex $covar i.industry [pw=weight] ///
				if covered_1966 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962 
		gaps_adj_1980 200.race
		eststo adj_rg_t_1961_1980

		
		coefplot(adj_rg_c_1961_1980, label("Covered in 1938")  connect(direct) lcolor(mydeepblue) lw(medthick) msize(medlarge) mfcolor(mydeepblue) mlcolor(mydeepblue) mlw(medthick)) ///
				(adj_rg_t_1961_1980, label("Covered in 1967")  connect(direct) lcolor(myarticblue) lw(medthick) msize(medlarge) mfcolor(myarticblue) mlcolor(myarticblue) mlw(medthick)) ///
				  , vertical levels(95) pstyle(matrix) noci ///
				  connect(direct) lcolor(mydeepblue) lw(medthick) msize(large) mcolor(mydeepblue) ytitle("White-Black Mean Log Annual Earnings Gap", color(gs4)) ///
				  ylabel(,labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
				  xline(6.5, lcolor(myred)) xlabel(1 "1961"  5 "1965" 10 "1970" 15 "1975" 20 "1980",labsize(medsmall) labcolor(gs4)) ///
				  legend(order(1 "Industries covered in 1938" 2 "Industries covered in 1967") ring(0) position(2) bmargin(large) color(gs1) c(1) region(col(white)))
		gr export "figures/adj_rg_tc_1961_1980.pdf", replace			

*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*		
*FIGURE D5 [APPENDIX]: ADJUSTED RACIAL WAGE GAPS, BY LEVEL OF EDUCATION
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*
		*FIGURE D5a [PAPER & SLIDES]: WHITE-BLACK EARNINGS GAP (ADJUSTED) IN TREATED INDUSTRIES
		*low-education	(less than high-school)
		qui reg ln_annual_wage ib100.race##ib1965.year ib1.sex $covar  i.industry [pw=weight] if covered_1966 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) &   inrange(educ,000,072) & educ!=999 & inrange(year,1961,1980) & year!=1962
		gaps_adj_1980 200.race
		eststo adj_rg_t_ls_1961_1980
		
		*high-education					
		qui reg ln_annual_wage ib100.race##ib1965.year ib1.sex $covar  i.industry  [pw=weight] if covered_1966 & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) &   inrange(educ,073,125) & educ!=999 & inrange(year,1961,1980) & year!=1962
		gaps_adj_1980 200.race
		eststo adj_rg_t_hs_1961_1980
			
		coefplot(adj_rg_t_ls_1961_1980,  label("Low-education")  connect(direct) lcolor(myarticblue) lw(medthick) msize(medlarge) mfcolor(myarticblue) mlcolor(myarticblue) mlw(medthick)) ///
				(adj_rg_t_hs_1961_1980,  label("High-education") connect(direct) lcolor(myarticblue) lp(dash) lw(medthick) msize(medlarge) mfcolor(myarticblue) mlcolor(myarticblue) mlw(medthick) ) ///					
				 , vertical levels(95) pstyle(matrix) noci ///
				 connect(direct) lcolor(mydeepblue) lw(medthick) msize(large) mcolor(mydeepblue) ytitle("White-Black Mean Log Annual Earnings Gap", color(gs4)) ///
				 ylabel(0(0.1)0.3,labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
				 xline(6.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1961"  5 "1965" 10 "1970" 15 "1975" 20 "1980",labsize(medsmall) labcolor(gs4)) ///
				 legend(order(1 "Low-education" 2 "High-education") ring(0) position(2) bmargin(large) color(gs1) c(1) region(col(white)))
		gr export "figures/adj_rg_t_skill_highs_1961_1980.pdf", replace	


		*FIGURE D5b [PAPER & SLIDES]: WHITE-BLACK EARNINGS GAP (ADJUSTED) IN CONTROL INDUSTRIES
		*low-education	
		qui reg ln_annual_wage ib100.race##ib1965.year ib1.sex $covar  i.industry [pw=weight] if covered_1938 & !inlist(industry,8) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) &   inrange(educ,000,072) & educ!=999 & inrange(year,1961,1980) & year!=1962
		gaps_adj_1980 200.race
		eststo adj_rg_c_ls_1961_1980
			
		*high-education	
		qui reg ln_annual_wage ib100.race##ib1965.year ib1.sex $covar  i.industry  [pw=weight] if covered_1938 & !inlist(industry,8) & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) &   inrange(educ,073,125) & educ!=999 & inrange(year,1961,1980) & year!=1962
		gaps_adj_1980 200.race
		eststo adj_rg_c_hs_1961_1980
			
		coefplot(adj_rg_c_ls_1961_1980,  label("Low-education")  connect(direct)  lcolor(mydeepblue)  lw(medthick) msize(medlarge) mfcolor(mydeepblue) mlcolor(mydeepblue) mlw(medthick)) ///
				(adj_rg_c_hs_1961_1980,  label("High-education") connect(direct) lp(dash) lcolor(mydeepblue) lw(medthick) msize(medlarge) mfcolor(mydeepblue) mlcolor(mydeepblue) mlw(medthick) ) ///					
				, vertical levels(95) pstyle(matrix) noci ///
				connect(direct) lcolor(mydeepblue) lw(medthick) msize(large) mcolor(mydeepblue) ytitle("White-Black Mean Log Annual Earnings Gap", color(gs4)) ///
				ylabel(0(0.1)0.3,labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
				yline(0,lcolor(gs7) lw(medthin)) xlabel(1 "1961"  5 "1965" 10 "1970" 15 "1975" 20 "1980",labsize(medsmall) labcolor(gs4)) ///
				legend(order(1 "Low-education" 2 "High-education") ring(0) position(2) bmargin(large) color(gs1) c(1) region(col(white)))
		gr export "figures/adj_rg_c_skill_highs_1961_1980.pdf", replace				
			
			
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*
*TABLE [SLIDES ONLY -- APPENDIX]: EFFECT ON RACIAL GAP DRIVEN BY REDUCED GAP AMONG MEN
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*
		*all
		reghdfe ln_annual_wage ib200.race##i.covered_1966##ib1.time ib1.sex $covar [pw=weight] ///
				if covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry) 
		eststo 	rg_adj_short_all
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasindustryfe 	"Y"

		*women		
		reghdfe ln_annual_wage ib200.race##i.covered_1966##ib1.time $covar  [pw=weight] ///
				if sex==2 & covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry) 
		eststo 	rg_adj_short_women
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasindustryfe 	"Y"
		*men		
		reghdfe ln_annual_wage ib200.race##i.covered_1966##ib1.time $covar  [pw=weight] ///
				if sex==1 & covered_all & flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) & year!=1962, absorb(i.industry) vce(cluster industry) 
		eststo 	rg_adj_short_men
				estadd local hascontrols 	"Y"
				estadd local hastimefe 		"Y"
				estadd local hasindustryfe 	"Y"		
		
		*output table
		esttab  rg_adj_short_all rg_adj_short_women rg_adj_short_men  ///
				using "tables/table_rg_adj.tex",  replace label fragment ///
				nolines  posthead(\cmidrule{2-4}) prefoot(\midrule) postfoot(\bottomrule \bottomrule) booktabs ///
				nonumbers mtitle("All" "Women" "Men") collabels(none)    ///
				cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
				refcat(100.race#1.covered_1966#2.time "Covered in 1967 $\times$", nolabel) ///
				keep(100.race#1.covered_1966#2.time)   ///
				coeflabel(100.race#1.covered_1966#2.time "\hspace{0.5cm}{1967-1972}") ///
				stats(N hascontrols hastimefe hasindustryfe , ///
				fmt(%11.0gc) label("Observations" "Controls" "Time FE" "Industry FE" )) onecell 
		
				