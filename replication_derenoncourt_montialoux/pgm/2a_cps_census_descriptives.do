*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: 		descriptive statistics 
*first created: 01/01/2018
*last updated:  08/30/2020
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	

clear all
set more off
set matsize 10000
set maxvar 10000



*---------------------------------------------------------------------------------------------
*TABLE B3 [APPENDIX]: EMPLOYMENT AND EARNINGS BY RACE, 1967 
*---------------------------------------------------------------------------------------------	
use "data/output/cps_master_individual_level.dta", clear
	gen emp_white 	= (race==100)
	gen emp_black 	= (race==200)
	gen emp_men		= (sex==1)
	gen emp_women 	= (sex==2)	
	
	gen earnings_white 		= . 
	replace earnings_white 	= annual_wage if race==100
	
	gen earnings_black 		= .
	replace earnings_black 	= annual_wage if race==200	
	
	gen earnings_men 	 	= . 
	replace earnings_men 	= annual_wage if sex==1

	gen earnings_women   	= .
	replace earnings_women 	= annual_wage if sex==2	
		
	
	*all industries
	 preserve 
		collapse (count)  nb_emp=flag_employed /// 
				 (mean)  emp_white emp_black earnings_white earnings_black /// 
				  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) &  year==1966 [pw=weight] 
		
		su nb_emp
		global emp_all `r(mean)'
		cap drop share_emp
		gen share_emp = . 
		replace share_emp = nb_emp/$emp_all 				  		  
		mkmat 	nb_emp share_emp emp_white emp_black earnings_white earnings_black, matrix(row1)
	restore
	
	
	**create program for the rest of the industries 
	capture program drop sum_stats 
	program define sum_stats
	args cond mat
	 preserve 
		
		collapse (count)  nb_emp=flag_employed /// 
				 (mean)   emp_white emp_black earnings_white earnings_black /// 
				  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) &  year==1966 `cond' [pw=weight] 
		cap drop share_emp
		gen share_emp = . 
		replace share_emp = nb_emp/$emp_all 				  		  
		mkmat 	nb_emp share_emp emp_white emp_black earnings_white earnings_black, matrix(`mat')
	restore
	end
	
	
	*1938 industries
	sum_stats "& covered_1938" "row2"
		*manufacturing
		sum_stats "& inlist(industry,5,6,7)" "row3"
		*transportation, communication, and other utilities
		sum_stats "& inlist(industry,8)" "row4"
		*finance, insurance, and real estate
		sum_stats "& inlist(industry,12)" "row5"
		*wholesale trade
		sum_stats "& inlist(industry,9)" "row6"
		*business and repair services
		sum_stats "& inlist(industry,13)" "row7"
		*mining
		sum_stats "& inlist(industry,3)" "row8"
		*forestry and fishing 
		sum_stats "& inlist(industry,2)" "row9"
	*1961 industries 
	sum_stats "& inlist(industry,4,11)" "row10"
		*retail trade 
		sum_stats "& inlist(industry,11)" "row11"
		*construction 
		sum_stats "& inlist(industry,4)" "row12"	
	*1966 industries 
	sum_stats "& covered_1966" "row13"
		*schools
		sum_stats "& inlist(industry,19)" "row14"
		*nursing homes and other professional services 
		sum_stats "& inlist(industry,17)" "row15"	
		*hospitals
		sum_stats "& inlist(industry,18)" "row16"
		*Hotels and laundries 
		sum_stats "& inlist(industry,15)" "row17"	
		*restaurants
		sum_stats "& inlist(industry,10)" "row18"
		*agriculture 
		sum_stats "& inlist(industry,1)" "row19"			
		*entertainment and recreation services
		sum_stats "& inlist(industry,16)" "row20"
	*Public Administration
		sum_stats "& inlist(industry,20,21,22)" "row21"
	*domestic service
		sum_stats "& inlist(industry,14)" "row22"

	*stack all matrices of results together
		mat mat_stats = row1 
		forval j = 2/22 {
			mat mat_stats = mat_stats\ row`j'
			}
		
		
		matrix colnames mat_stats = "Number" "Percent" "White" "Black"  "White" "Black" 	
		matrix rownames mat_stats = "All industries" ///
									 "Industries covered by 1938 FLSA" ///
										"\hspace{3mm}{Manufacturing}"  "\hspace{3mm}{Transportation}" ///
										"\hspace{3mm}{Finance, Insurance}"  "\hspace{3mm}{Wholesale Trade}" ///									 
										"\hspace{3mm}{Business, Repair}"  "\hspace{3mm}{Mining}" "\hspace{3mm}{Forestry, fishing}" ///									
									 "Industries covered by 1961 FLSA"  "\hspace{3mm}{Retail trade}"  "\hspace{3mm}{Construction}" ///
									 "Industries covered by 1966 FLSA"  "\hspace{3mm}{Schools}"  "\hspace{3mm}{Nursing homes}" ///
										"\hspace{3mm}{Hospitals}"  "\hspace{3mm}{Hotels, laundries}"  "\hspace{3mm}{Restaurants}" ///
										"\hspace{3mm}{Agriculture}"  "\hspace{3mm}{Entertainment}" ///
									 "Public Administration"  ///
									 "Domestic service"						 
	
  *output table 
	esttab 	matrix(mat_stats,fmt(%12.0fc %3.2f %3.2f %3.2f %12.0fc %12.0fc)) ///
			using "tables/table_emp_earnings_by_race.tex", replace label fragment ///
			nolines posthead("& \multicolumn{2}{c}{Employment} & \multicolumn{2}{c}{Employment shares} & \multicolumn{2}{c}{Earnings (\\$2017)} \\" ///
			"\cmidrule(lr){2-3} \cmidrule(lr){4-5}  \cmidrule(lr){6-7}    & \multicolumn{1}{c}{Number} & \multicolumn{1}{c}{Percent} &   \multicolumn{1}{c}{White}  & \multicolumn{1}{c}{Black} &  \multicolumn{1}{c}{White} & \multicolumn{1}{c}{Black} \\" ///
			\cmidrule(lr){2-3}\cmidrule(lr){4-5}\cmidrule(lr){6-7}) ///
			postfoot(\bottomrule \bottomrule) booktabs	///							
			nonumbers nomtitles collabels(none) 			
				
*---------------------------------------------------------------------------------------------
*TABLE 1 [PAPER]: WORKERS CHARACTERISTICS, 1965-66 (i.e. FOR CPS YEARS 1966-67)
*---------------------------------------------------------------------------------------------
  *select characteristics 	
	*characteristics 
	gen male = (sex==1)
	gen female  = (sex==2)
	
	*age
		
	*education attainment 
	gen less_educ = inrange(educ,010,071) /*11 years of schooling or less*/
	gen more_educ= inrange(educ,72,125)	  /*More than 11 years of schooling*/
	
	*marital status 
	gen married = inlist(marst,1,2)		  /*married, spouse present or absent*/	
	gen single = inrange(marst,4,7) 	  /*single, separated, divorced or widowed*/

	*Region
	gen nc=(region_abb=="NC")			  /*North central*/
	gen ne=(region_abb=="NE")			  /*North east*/
	gen s=(region_abb=="S")				  /*South*/	
	gen w=(region_abb=="W")				  /*West*/
	
	*occupation 
	gen occ_7  = (occupation==7)  		  /*Operatives*/
	gen occ_6  = (occupation==6)  		  /*Craftsmen*/
	gen occ_4  = (occupation==4)  		  /*Clerical and kindred*/
	gen occ_3  = (occupation==3)  		  /*Managers, officials and proprietors*/
	gen occ_1  = (occupation==1)  		  /*Professional and technical*/
	gen occ_5  = (occupation==5)    	  /*Sales worker*/
	gen occ_9  = (occupation==9)    	  /*Service worker*/
	gen occ_other = (inlist(occupation,2,10,11)) /*Farmer, or laborer (farm laborer or not)*/
	
	*FT/PT status
	gen ftfy = (fullpart==1 & inlist(wkswork2,5,6))  if  flag_employed  & in_sample 
	gen pt = (ftfy==0)  if  flag_employed  & in_sample 
	
  *include those characteristics in the table 
	global dem annual_wage age male female less_educ more_educ married single nc ne s w ///
			   occ_7 occ_6 occ_4 occ_3 occ_1 occ_5 occ_9  occ_other ftfy pt  
	*control group
	**white
	estpost su $dem  if flag_employed & in_sample & inrange(age,25,55) & covered_1938 & inlist(race,100) &  inlist(year,1965,1966) [w=weight]
	est store des_control_white
	**black
	estpost su $dem  if flag_employed & in_sample & inrange(age,25,55) & covered_1938 & inlist(race,200) &  inlist(year,1965,1966)  [w=weight]
	est store des_control_black

	*treatment group
	**white
	estpost su $dem  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,100) &  inlist(year,1965,1966)  [w=weight]
	est store des_treatment_white
	**black
	estpost su $dem  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,200) &  inlist(year,1965,1966)  [w=weight]
	est store des_treatment_black			
	
  *output table to csv file 
	esttab  des_control_white des_control_black   des_treatment_white  des_treatment_black ///
			using "tables/table_sum_stats.tex", replace  fragment ///
			refcat(male "\rule{0pt}{4ex}\emph{Gender}" less_educ "\rule{0pt}{4ex}\emph{Education}" married "\rule{0pt}{4ex}\emph{Marital status}"  nc "\rule{0pt}{4ex}\emph{Region}" occ_7 "\rule{0pt}{4ex}\emph{Occupation}" ///
				  ftfy  "\rule{0pt}{4ex}\emph{Full-time/part-time status}", nolabel) ///
			coeflabel(annual_wage "\rule{0pt}{4ex}Annual earnings (in \\$2017)" age "\rule{0pt}{4ex}Age" male "\hspace{0.5cm}{Male}" female "\hspace{0.5cm}{Female}"   ///
					  less_educ "\hspace{0.5cm}{11 years of schooling or less}"  more_educ "\hspace{0.5cm}{More than 11 years of schooling}"  ///
					  single "\hspace{0.5cm}{Single}" married "\hspace{0.5cm}{Married}" separated "\hspace{0.5cm}{Separated}" divorced_widowed "\hspace{0.5cm}{Divorced or widowed}" ///
					  nc "\hspace{0.5cm}{North Central}" ne "\hspace{0.5cm}{North East}" s "\hspace{0.5cm}{South}" w "\hspace{0.5cm}{West}" ///
					  occ_7 "\hspace{0.5cm}{Operatives}" occ_6 "\hspace{0.5cm}{Craftsmen}" occ_4 "\hspace{0.5cm}{Clerical and kindred}" occ_3 "\hspace{0.5cm}{Managers, Officials and proprietors}" ///
					  occ_1 "\hspace{0.5cm}{Professional and technical}" occ_5 "\hspace{0.5cm}{Sales worker}" occ_9 "\hspace{0.5cm}{Service worker}"  occ_other "\hspace{0.5cm}{Other}"   ///
					  ftfy "\hspace{0.5cm}{Full-time, full-year}" pt "\hspace{0.5cm}{Part-time}" annual_wage "\rule{0pt}{4ex}Annual wage") ///
			mtitle("White" "Black" "White" "Black") ///
			nolines  posthead(\cmidrule{2-5}) postfoot(\bottomrule \bottomrule) noobs	///
			cells(mean(fmt(%8.0gc 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ))) label booktabs nonum collabels(none) 
			
*---------------------------------------------------------------------------------------------
*TABLE 1 [SLIDES ONLY]: WORKERS CHARACTERISTICS, 1965-66 (i.e. FOR CPS YEARS 1966-67)
*---------------------------------------------------------------------------------------------

  *select characteristics 	
	*region
	gen ns=(region_abb=="W" | region_abb=="NE" | region_abb=="NC")	/*non-south*/
		
  *include those characteristics in the table 
	*Statistics to include in the table 
	global dem_slides annual_wage age male female less_educ more_educ s ns ftfy pt 
		  
	**control group
	**white
	estpost su $dem_slides  if flag_employed & in_sample & inrange(age,25,55) & covered_1938 & inlist(race,100) &  inlist(year,1965,1966) [w=weight]
	est store des_control_white
	**black
	estpost su $dem_slides  if flag_employed & in_sample & inrange(age,25,55) & covered_1938 & inlist(race,200) &  inlist(year,1965,1966) [w=weight]
	est store des_control_black

	**treatment group
	**white
	estpost su $dem_slides  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,100) &  inlist(year,1965,1966) [w=weight]
	est store des_treatment_white
	**black
	estpost su $dem_slides  if flag_employed & in_sample & inrange(age,25,55) & covered_1966 & inlist(race,200) &  inlist(year,1965,1966) [w=weight]
	est store des_treatment_black			
	
  *output table to csv file 
	esttab  des_control_white des_control_black des_treatment_white des_treatment_black ///
			using "tables/table_sum_stats_slides.tex", replace  fragment ///
			refcat(male "\rule{0pt}{4ex}\emph{Gender}" less_educ "\rule{0pt}{4ex}\emph{Education}" s "\rule{0pt}{4ex}\emph{Region}"  ///
				  ftfy  "\rule{0pt}{4ex}\emph{Full-time/part-time status}", nolabel) ///
			coeflabel(annual_wage "\rule{0pt}{4ex}Annual earnings (in \\$2017)" age "\rule{0pt}{4ex}Age" male "\hspace{0.5cm}{Male}" female "\hspace{0.5cm}{Female}"   ///
					  less_educ "\hspace{0.5cm}{11 yrs of schooling or less}"  more_educ "\hspace{0.5cm}{More than 11 yrs of schooling}"   ///
					  s "\hspace{0.5cm}{South}"  ns "\hspace{0.5cm}{Non-South}"  ///
					  ftfy "\hspace{0.5cm}{Full-time, full-year}" pt "\hspace{0.5cm}{Part-time}" ) ///
			mtitle("White" "Black" "White" "Black") ///
			nolines  posthead(\cmidrule{2-5}) prefoot(\midrule) postfoot(\bottomrule \bottomrule) ///
			stats(N, fmt(%8.0gc) labels("Observations")) ///
			cells(mean(fmt(%8.0gc 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ))) label booktabs nonum collabels(none) 	  


*---------------------------------------------------------------------------------------------			
*TABLE B2 [APPENDIX]: OBSERVATIONS, EMPLOYMENT, AND WAGES IN THE MARCH CPS AND IN THE CENSUS
*---------------------------------------------------------------------------------------------
	
   **CPS section
	*nb of obs 
	 preserve 
		collapse (count)  obs_emp = flag_employed ///
						  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980), by (year) 	 
		mkmat obs_emp, matrix(cps_obs)
	restore 	
	 
	 *employment, employment shares earnings
	 preserve 
		collapse (count)  nb_emp=flag_employed /// 
				 (mean)   emp_white emp_black emp_men emp_women earnings_white earnings_black earnings_men earnings_women /// 
				  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & inrange(year,1961,1980) [pw=weight], by(year)
		mkmat nb_emp emp_white emp_black emp_men emp_women earnings_white earnings_black earnings_men earnings_women, matrix(cps_emp_earnings)	
	restore
	
   **CENSUS section
	*nb of obs
	preserve
	use "data/output/census_clean_1940_2017.dta", clear
		collapse (count)  obs_emp = flag_employed ///
						  if flag_employed  & inrange(age,25,55) & inlist(race,1,2) & inlist(year,1959,1969,1979), by (year) 	 
		mkmat obs_emp, matrix(census_obs)	
	restore
	
	preserve
	*employment, employment shares earnings
	use "data/output/census_clean_1940_2017.dta", clear
	
	gen emp_white 	= (race==1)
	gen emp_black 	= (race==2)
	gen emp_men		= (sex==1)
	gen emp_women 	= (sex==2)	
	
	gen 	earnings_white 	= . 
	replace earnings_white 	= annual_wage if race==1
	
	gen 	earnings_black 	= .
	replace earnings_black 	= annual_wage if race==2	
	
	gen 	earnings_men 	= . 
	replace earnings_men 	= annual_wage if sex==1

	gen 	earnings_women  = .
	replace earnings_women 	= annual_wage if sex==2	
	
		collapse (count)  nb_emp=flag_employed /// 
				 (mean)   emp_white emp_black emp_men emp_women earnings_white earnings_black earnings_men earnings_women /// 
						  if flag_employed & inrange(age,25,55) & inlist(race,1,2) & inlist(year,1959,1969,1979) [pw=perwt], by(year)
		mkmat nb_emp emp_white emp_black emp_men emp_women earnings_white earnings_black earnings_men earnings_women, matrix(census_emp_earnings)	
	restore
		
	
	
	*create matrices for CPS and CENSUSES results
	mat cps 	= (cps_obs,cps_emp_earnings)
	mat census  = (census_obs,census_emp_earnings)
	
	matrix colnames cps 	= " " " " "White" "Black" "Men" "Women" "White" "Black" "Men" "Women" 	
	matrix rownames cps 	= "\hspace{3mm}{1962}" "\hspace{3mm}{1963}" "\hspace{3mm}{1964}" "\hspace{3mm}{1965}" "\hspace{3mm}{1966}" ///
							  "\hspace{3mm}{1967}" "\hspace{3mm}{1968}" "\hspace{3mm}{1969}" "\hspace{3mm}{1970}" "\hspace{3mm}{1971}" ///
							  "\hspace{3mm}{1972}" "\hspace{3mm}{1973}" "\hspace{3mm}{1974}" "\hspace{3mm}{1975}" "\hspace{3mm}{1976}" ///
							  "\hspace{3mm}{1977}" "\hspace{3mm}{1978}" "\hspace{3mm}{1979}" "\hspace{3mm}{1980}" "\hspace{3mm}{1981}" 
	matrix rownames census =  "\hspace{3mm}{1960}" "\hspace{3mm}{1970}" "\hspace{3mm}{1980}" 
	
  *output table 
	esttab 	matrix(cps,fmt(%12.0fc %12.0fc %3.2f %3.2f %3.2f %3.2f %12.0fc %12.0fc %12.0fc %12.0fc)) ///
			using "tables/table_obs_emp_earnings_cps_census.tex", replace label fragment ///
			nolines posthead("& \multicolumn{1}{c}{Observations} &  \multicolumn{1}{c}{Employment} & \multicolumn{4}{c}{Employment shares} & \multicolumn{4}{c}{Earnings (\\$2017)} \\" ///
			"\cmidrule(lr){4-7} \cmidrule(lr){8-11}   & \multicolumn{1}{c}{ } & \multicolumn{1}{c}{} &   \multicolumn{1}{c}{White}  & \multicolumn{1}{c}{Black} &   \multicolumn{1}{c}{Men}  & \multicolumn{1}{c}{Women} &  \multicolumn{1}{c}{White} & \multicolumn{1}{c}{Black}  &  \multicolumn{1}{c}{Men} & \multicolumn{1}{c}{Women} \\" ///
			 \cmidrule(lr){4-7} \cmidrule(lr){8-11} \multicolumn{1}{c}{\textbf{March CPS}} & & & & & & & & & & \\) ///
			nonumbers nomtitles collabels(none) 				
	esttab 	matrix(census,fmt(%12.0fc %12.0fc %3.2f %3.2f %3.2f %3.2f %12.0fc %12.0fc %12.0fc %12.0fc)) ///
			using "tables/table_obs_emp_earnings_cps_census.tex", append  label fragment ///
			nolines  postfoot(\bottomrule \bottomrule) booktabs ///
			nonumbers nomtitles collabels(none)   ///
			refcat(\hspace{3mm}{1960} "\rule{0pt}{4ex}\textbf{US Census}", nolabel) 

			
*---------------------------------------------------------------------------------------------
*FIGURE 1 [PAPER & SLIDES]: ECONOMY-WIDE WHITE-BLACK UNADJUSTED WAGE GAP IN THE LONG-RUN 
*---------------------------------------------------------------------------------------------
	use "data/output/cps_master_individual_level.dta", clear	
	preserve
	**IN CENSUS	
	use "data/output/census_clean_1940_2017.dta", clear		
	
	**all industries
		foreach y in  "1949" "1959" "1969" "1979"  "1989"  "1999" "2010" {
		qui reg ln_annual_wage ib1.race  [pw=perwt] if covered_all & flag_employed & inrange(age,25,65) & inlist(race,1,2) & year==`y'
		mat tab_census_unadj_all_`y' =`y' ,-_b[2.race], _b[_cons]
		}
		
		*Compile matrices of results		
		mat tab_census_unadj_all = tab_census_unadj_all_1949
		foreach y in "1959" "1969" "1979"  "1989"  "1999" "2010" {
		mat tab_census_unadj_all = tab_census_unadj_all \ tab_census_unadj_all_`y'
		}

	**1938 industries
		foreach y in /*"1939"*/ "1949" "1959" "1969" "1979"  "1989"  "1999" "2010" {
		qui reg ln_annual_wage ib1.race  [pw=perwt] if covered_1938 & flag_employed & inrange(age,25,65) & inlist(race,1,2) & year==`y'
		mat tab_census_unadj_1938_`y' = -_b[2.race], _b[_cons]
		}
		
		*Compile matrices of results		
		mat tab_census_unadj_1938 = tab_census_unadj_1938_1949
		foreach y in "1959" "1969" "1979"  "1989"  "1999" "2010" {
		mat tab_census_unadj_1938 = tab_census_unadj_1938 \ tab_census_unadj_1938_`y'
		}				

	**1967 industries
		foreach y in /*"1939"*/ "1949" "1959" "1969" "1979"  "1989"  "1999" "2010" {
		qui reg ln_annual_wage ib1.race  [pw=perwt] if covered_1966 & flag_employed & inrange(age,25,65) & inlist(race,1,2) & year==`y'
		mat tab_census_unadj_1966_`y' = -_b[2.race], _b[_cons]
		}
		
		*Compile matrices of results		
		mat tab_census_unadj_1966 = tab_census_unadj_1966_1949
		foreach y in "1959" "1969" "1979"  "1989"  "1999" "2010" {
		mat tab_census_unadj_1966 = tab_census_unadj_1966 \ tab_census_unadj_1966_`y'
		}				
		
		*Compile matrices of for all, tc, 1938 and 1966
		mat rg_census_unadj = (tab_census_unadj_all,tab_census_unadj_1938,tab_census_unadj_1966)	

	restore
	
	**IN CPS	
	**treatment and control industries together
		forval y =1961/2015 {
		qui reg ln_annual_wage ib100.race  [pw=weight] if covered_all & flag_employed & in_sample & inrange(age,25,65) & inlist(race,100,200) & year==`y'
		mat tab_unadj_tc_`y' = `y',-_b[200.race], _b[_cons]
		}
		
		*Compile matrices of results		
		mat tab_unadj_tc = tab_unadj_tc_1961
		forvalues y = 1962/2015 {
		mat tab_unadj_tc = tab_unadj_tc \ tab_unadj_tc_`y'
		}			
		
	**1938 industries
		forval y =1961/2015 {
		qui reg ln_annual_wage ib100.race  [pw=weight] if covered_1938 & flag_employed & in_sample & inrange(age,25,65) & inlist(race,100,200) & year==`y'
		mat tab_unadj_1938_`y' = -_b[200.race], _b[_cons]
		}
		
		*Compile matrices of results		
		mat tab_unadj_1938 = tab_unadj_1938_1961
		forvalues y = 1962/2015 {
		mat tab_unadj_1938 = tab_unadj_1938 \ tab_unadj_1938_`y'
		}				

	**1967 industries
		forval y =1961/2015 {
		qui reg ln_annual_wage ib100.race  [pw=weight] if covered_1966 & flag_employed & in_sample & inrange(age,25,65) & inlist(race,100,200) & year==`y'
		mat tab_unadj_1966_`y' = -_b[200.race], _b[_cons]
		}
		
		*Compile matrices of results		
		mat tab_unadj_1966 = tab_unadj_1966_1961
		forvalues y = 1962/2015 {
		mat tab_unadj_1966 = tab_unadj_1966 \ tab_unadj_1966_`y'
		}				
		
	**Compile matrices of for all, tc, 1938 and 1966
		mat rg_unadj = (tab_unadj_tc,tab_unadj_1938,tab_unadj_1966)	
		svmat rg_unadj 
		
		rename rg_unadj1 year_graph
		rename rg_unadj2 unadj_tc
		rename rg_unadj3 unadj_cons_tc
		rename rg_unadj4 unadj_1938
		rename rg_unadj5 unadj_cons_1938			
		rename rg_unadj6 unadj_1966
		rename rg_unadj7 unadj_cons_1966						
	
	**Include matrices for census data
		svmat rg_census_unadj 
		
		rename rg_census_unadj1 year_census_graph
		rename rg_census_unadj2 unadj_census_tc
		rename rg_census_unadj3 unadj_census_cons_tc
		rename rg_census_unadj4 unadj_census_1938
		rename rg_census_unadj5 unadj_census_cons_1938			
		rename rg_census_unadj6 unadj_census_1966
		rename rg_census_unadj7 unadj_census_cons_1966
	
		*replace the year 1962 (omitted) by the 1961-1963 average 
		replace unadj_tc = 0.56957685 if year_graph==1962
		replace unadj_1938 = 0.47956715 if year_graph==1962
		replace unadj_1966 = 0.50496575 if year_graph==1962
		
	*plot figure [PAPER]
	twoway 		(connected  unadj_tc  year_graph if inrange(year_graph,1961,2015), lcolor(mybeige) mcolor(mybeige) msymbol(circle) msize(small)) ///
				(scatter unadj_census_tc year_census_graph if inrange(year_census_graph,1949,2017), connect(l) clwidth(vthick) clcolor(red) clpattern(dot) lcolor(myred) mcolor(myred) mfcolor(none) msymbol(diamond) msize(medlarge)), ///
				xtitle("") xlabel(1950(10)2020) ytitle("White-Black Mean Log Annual Earnings Gap", color(gs4)) ///
				legend(label(1 "Current Population Survey") label(2 "Census")   ///
				ring(0) position(2) bmargin(large) color(gs1) c(1) region(col(white)) ) ///
				graphregion(color(white)) ylab(0(0.2)0.7) 	
				gr export "figures/unadj_rg_all_1949_2017.pdf", replace
	
	*plot figure [DATA & PROGRAMS WEBPAGE]				
	twoway 		(connected  unadj_tc  year_graph if inrange(year_graph,1961,2015), lcolor(mybeige) mcolor(mybeige) msymbol(circle) msize(small)) ///
				(scatter unadj_census_tc year_census_graph if inrange(year_census_graph,1949,2017), connect(l) clwidth(vthick) clcolor(red) clpattern(dot) lcolor(myred) mcolor(myred) mfcolor(none) msymbol(diamond) msize(medlarge)), ///
				xtitle("") xlabel(1950(10)2020,labsize(medlarge)) ytitle("White-Black Mean Log Annual Earnings Gap", color(gs4) size(medlarge)) ///
				legend(label(1 "Current Population Survey") label(2 "Census")   ///
				ring(0) position(2) bmargin(large) color(gs1) c(1) region(col(white)) size(medlarge)) ///
				graphregion(color(white)) ylab(0(0.2)0.7,labsize(medlarge)) 				
				gr export "figures/unadj_rg_all_1949_2017.png", replace
		
*---------------------------------------------------------------------------------------------
*FIGURE G1 [APPENDIX]: WHITE-BLACK UNADJUSTED WAGE GAP IN THE LONG-RUN 
*---------------------------------------------------------------------------------------------
	*FIGURE G1a: WHITE-BLACK UNADJUSTED WAGE GAP IN THE LONG-RUN -- ECONOMY-WIDE
		twoway 	(connected  unadj_tc  year_graph if inrange(year_graph,1961,2015), lcolor(mybeige) mcolor(mybeige) msymbol(circle) msize(small)), ///
				xline(1966.5, lcolor(myred) lw(medthick)) xtitle("")  xlabel(1960(5)2020, labsize(small)) ytitle("White-Black Mean Log Annual Earnings Gap", color(gs4)) ///
				legend(label(1 "All industries")   ///
				ring(0) position(2) bmargin(large) color(gs1) c(1) region(col(white)) ) ///
				graphregion(color(white)) ylab(0(0.2)0.7) 	
		gr export "figures/unadj_rg_all_1961_2015.pdf", replace
		
	*FIGURE G1b: BY TYPE OF INDUSTRY
		twoway  (connected  unadj_1938 unadj_1966 year_graph if inrange(year_graph,1961,2015), lcolor(mydeepblue myarticblue) mcolor(mydeepblue myarticblue) msymbol(circle circle) msize(small small)), /// 	
				xline(1966.5, lcolor(myred) lw(medthick)) xtitle("") xlabel(1960(5)2020, labsize(small))  ytitle("White-Black Mean Log Annual Earnings Gap", color(gs4)) ///
				legend(label(1 "Industries covered in 1938") label(2 "Industries covered in 1967")  ///
				ring(0) position(2) bmargin(large) color(gs1) c(1) region(col(white)) ) ///
				graphregion(color(white)) ylab(0(0.2)0.7)
		gr export "figures/unadj_rg_tc_1961_2015.pdf", replace	
		
		
	/*FIGURE A28b BIS: BY TYPE OF INDUSTRY -- FOR SOLE 2020 CONFERENCE SLIDES
	twoway  (connected  unadj_1938 unadj_1966 year_graph if inrange(year_graph,1961,1980), lcolor(mydeepblue myarticblue) mcolor(mydeepblue myarticblue) msymbol(circle circle) msize(medlarge medlarge)), /// 	
				xline(1966.5, lcolor(myred) lw(medthick)) xtitle("") xlabel(1960(5)1980, labsize(small))  ytitle("White-Black Mean Log Annual Earnings Gap", color(gs4)) ///
				legend(label(1 "Industries covered in 1938") label(2 "Industries covered in 1967")  ///
				ring(0) position(2) bmargin(large) color(gs1) c(1) region(col(white)) ) ///
				graphregion(color(white)) ylab(0.1(0.1)0.6) ///
				text(0.228 1966.7 "Laundries" "Restaurants" "Hotels, Schools" "Nursing homes" "Agriculture, etc." , ///
				place(se) box c(myarticblue) bc(myarticblue) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(25.0)) ///
				text(0.48 1971.5 "Manufacturing, Transportation" "Communication, Wholesale" "Finance, Ins., Real Estate" "Mining, Forestry, Fishing, etc.", ///
				place(se) box c(mydeepblue) bc(mydeepblue) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(47.0)) 
		gr export "figures/unadj_rg_tc_1961_1980_wbox.pdf", replace	
		*/
	
*---------------------------------------------------------------------------------------------
*FIGURE 2 [PAPER & SLIDES]: EXPANSIONS IN MINIMUM WAGE COVERAGE
*---------------------------------------------------------------------------------------------			
		*>>> See graph 'G14c' in replication>figures>spd_mwdescriptives.xls	
							
*---------------------------------------------------------------------------------------------
*FIGURE A1 [APPENDIX]: MINIMUM WAGE TO MEDIAN RATIO, USING FEDERAL LAW ONLY
*---------------------------------------------------------------------------------------------
		*a. Calculate the Kaitz index at the year (y) * industry (j) level (to take into account how mw legislation vary by industry)
		*KI_y = N_yj/N_y * MinWage_yj/MedianWage_y
		use "data/output/cps_master_individual_level.dta", clear						
				collapse (sum)  	N_y = count ///
						 (median) 	median_wage_economy=annual_wage [pw=weight]  ///
									if flag_employed==1 & in_sample & fullpart==1 & wkswork2==6 & covered_all ///
									, by (year)
				tempfile N_y
				save 	`N_y'
			
				
		use "data/output/cps_master_individual_level.dta", clear						
				*gen refined industry category to disentengle agriculture from the rest of the 1966 industries			
				gen ind_category_refined =. 
				replace ind_category_refined = 1 if inlist(industry,2,3,5,6,7,8,9,12,13,23)
				replace ind_category_refined = 2 if inlist(industry,10,15,16,17,18,19)
				replace ind_category_refined = 3 if inlist(industry,14)
				replace ind_category_refined = 4 if inlist(industry,20,21,22)	
				replace ind_category_refined = 5 if inlist(industry,4,11)	
				replace ind_category_refined = 6 if inlist(industry,1)	
				label var ind_category_refined "Industry category :1.1938,2.1966 exept agri,3.priv. hh,4.public,5.1961,6.1966 agri"				
			
				collapse (mean)	 	hmw_federal hmw_1966FLSA_agri hmw_1966FLSA  ///
						 (sum)	    N_yj = count [pw=weight] ///
									if flag_employed==1 & in_sample & fullpart==1 & wkswork2==6 & covered_all  ///
														, by (year ind_category_refined)
			
			merge m:1 year using `N_y'	
				  tab _merge
				  drop _merge
					
				*Annual min wage
				gen amw_federal  		= hmw_federal*40*52   /*40 hours of work a week, 52 weeks a year*/
				gen amw_1966FLSA  		= hmw_1966FLSA*40*52   /*40 hours of work a week, 52 weeks a year*/
				gen amw_1966FLSA_agri 	= hmw_1966FLSA_agri*40*52   /*40 hours of work a week, 52 weeks a year*/
				
				*Kaitz Index_yj
				gen 	KI_yj 	= . 
				replace KI_yj 	= N_yj/N_y * amw_federal/median_wage_economy	if 	ind_category_refined==1
				replace KI_yj 	= N_yj/N_y * amw_1966FLSA/median_wage_economy	if 	ind_category_refined==2
				replace KI_yj 	= N_yj/N_y * amw_1966FLSA_agri/median_wage_economy	if 	ind_category_refined==6
							
		*b. Calculate the Kaitz index at the year (y) level (i.e. collapse KI_yj at the year level)
					collapse (sum) KI_y = KI_yj ///
								   , by(year)				
					gen KI_y_percent = KI_y*100 
					
					tempfile KI_y
					save 	`KI_y'
	
		*c.Add the lines on min wage/median ratio for control and treated industries
				use "data/output/cps_master_individual_level.dta", clear						
				collapse (mean) hmw_federal hmw_1961FLSA hmw_1966FLSA_agri hmw_1966FLSA, by (year)
				tempfile mw
				save `mw' 						
			*compute median wages for employed adults (full-time full-year)	
				*industries covered in 1938
				use "data/output/cps_master_individual_level.dta", clear				
				
				collapse (median) median_wage_1938_ind = annual_wage [pw=weight]  ///
								  if flag_employed==1 & in_sample & covered_1938 & fullpart==1 & wkswork2==6  ///
								  , by(year)
				tempfile median_1938_ind
				save `median_1938_ind' 
					
				*industries covered in 1966 (excluding agriculture)
				use "data/output/cps_master_individual_level.dta", clear
				
				collapse (median) median_wage_1966_ind=annual_wage [pw=weight]  ///
								  if flag_employed==1 & in_sample & covered_1966 & industry !=1 & fullpart==1 &  wkswork2==6  ///
								  , by(year)
				tempfile median_1966_ind
				save `median_1966_ind' 
									  				
			*merge all datasets, and compute mw to median ratios for 1- industries covered in 1938, 2- industries covered in 1966 (except farms) 	 	 
				merge 1:1 year using `median_1938_ind' 	
					rename _merge m_median_1938_ind
				merge 1:1 year using `median_1966_ind' 	
					rename _merge m_median_1966_ind			
				merge 1:1 year using `mw' 	
					rename _merge m_median_mw		
				merge 1:1 year using `KI_y' 	
				drop _merge m_median_1938_ind m_median_1966_ind m_median_mw
				
				*compute annual mw FTFY
				gen amw_federal  = hmw_federal*40*52   /*40 hours of work a week, 52 weeks a year*/
				gen amw_1966FLSA = hmw_1966FLSA*40*52  /*40 hours of work a week, 52 weeks a year*/

				*mw to median ratios
				gen mw_to_median_1938_ind =.
				replace mw_to_median_1938_ind = amw_federal *100 / median_wage_1938_ind 
				
				gen mw_to_median_1966_ind =.
				replace mw_to_median_1966_ind = amw_1966FLSA *100 / median_wage_1966_ind		
		
		*Plot Figure 5.1	with federal law only		
			keep if inrange(year,1961,1980)
			replace KI_y_percent =. if KI_y_percent ==0 & year==1962
			twoway connected mw_to_median_1938_ind mw_to_median_1966_ind KI_y_percent year, xline(1966.5, lcolor(myred) lw(medthick)) ///
							 xtitle("") ytitle("MW to median ratio (%)", color(gs4))  graphregion(color(white)) ///
							 legend(label (1 "Industries covered in 1938") label(2 "Industries covered in 1967") ///
							 label(3 "Kaitz Index") region(lwidth(none))) ///
							 lcolor(mydeepblue myarticblue mybeige) mcolor(mydeepblue myarticblue mybeige) msize(medlarge medlarge medlarge) mlw(medium medium medthick) ///
							 msymbol(circle circle circle_hollow)
			gr export "figures/mw_to_median_ratio_DC_federal.pdf", replace

*---------------------------------------------------------------------------------------------
*FIGURE E1 [APPENDIX]: MINIMUM WAGE TO MEDIAN RATIO, USING STATE MINIMUM WAGE LAWS
*---------------------------------------------------------------------------------------------
		*a. Calculate the Kaitz index at the year (y) * industry (j) level (to take into account how mw legislation vary by industry)
		*KI_y = N_yj/N_y * MinWage_yj/MedianWage_y
		use "data/output/cps_master_individual_level.dta", clear						
				collapse (sum)  	N_y = count ///
						 (median) 	median_wage_economy=annual_wage [pw=weight]  ///
									if flag_employed==1 & in_sample & fullpart==1 & wkswork2==6 & covered_all ///
									, by (year)
				tempfile N_y
				save 	`N_y'
				
		use "data/output/cps_master_individual_level.dta", clear						
					collapse (mean)	 	hmw_federal hmw_yj ///
							 (sum)  	N_yj2 = count [pw=weight] ///
										if flag_employed==1 & in_sample & fullpart==1 & wkswork2==6 & covered_all ///
										, by (year industry)
				
				merge m:1 year using `N_y'
					  tab _merge
					  drop _merge
					    
				*Annual min wage_yj
				gen amw_yj  	= hmw_yj*40*52   /*40 hours of work a week, 52 weeks a year*/
				*Kaitz Index_yj
				gen KI_yj2 	=  N_yj2/N_y * amw_yj/median_wage_economy				
				
		*b. Calculate the Kaitz index at the year (y) level (i.e. collapse KI_yj at the year level)
				collapse (sum) KI_y2 = KI_yj2 ///
							   , by(year)				
				gen KI_y2_percent = KI_y2*100 
				
				tempfile KI_y2
				save 	`KI_y2'
	
		*c. Add the lines on min wage/median ratio for control and treated industries
				use "data/output/cps_master_individual_level.dta", clear						
				*gen refined industry category to disentengle agriculture from the rest of the 1966 industries			
					gen ind_category_refined =. 
					replace ind_category_refined = 1 if inlist(industry,2,3,5,6,7,8,9,12,13,23)
					replace ind_category_refined = 2 if inlist(industry,10,15,16,17,18,19)
					replace ind_category_refined = 3 if inlist(industry,14)
					replace ind_category_refined = 4 if inlist(industry,20,21,22)	
					replace ind_category_refined = 5 if inlist(industry,4,11)	
					replace ind_category_refined = 6 if inlist(industry,1)	
					label var ind_category_refined "Industry category :1.1938,2.1966 exept agri,3.priv. hh,4.public,5.1961,6.1966 agri"	
				
				collapse (mean) hmw_federal hmw_yj  ///
						 (sum)  N_yj = count [pw=weight] ///			
								if flag_employed==1 & in_sample & fullpart==1 & wkswork2==6 & covered_all ///
								, by (year ind_category_refined)
					
					keep if inlist(ind_category_refined,1,2)
					reshape wide N_yj hmw_yj, i(year hmw_federal) j(ind_category_refined)
					
					gen 	hmw_1938FLSA_state = . 
					replace hmw_1938FLSA_state = max(hmw_federal,hmw_yj1)
					
					rename 	hmw_yj2 hmw_1966FLSA_state
					drop 	hmw_federal hmw_yj1 N_yj1 N_yj2
					
					tempfile mw
					save `mw' 						
			
			*compute median wages for employed adults (full-time full-year)	
				*industries covered in 1938
				use "data/output/cps_master_individual_level.dta", clear				
				
				collapse (median) median_wage_1938_ind = annual_wage [pw=weight]  ///
								  if flag_employed==1 & in_sample & covered_1938 & fullpart==1 & wkswork2==6  ///
								  , by(year)
				tempfile median_1938_ind
				save `median_1938_ind' 
					
				*industries covered in 1966 (excluding agriculture)
				use "data/output/cps_master_individual_level.dta", clear
				
				collapse (median) median_wage_1966_ind=annual_wage [pw=weight]  ///
								  if flag_employed==1 & in_sample & covered_1966 & industry !=1 & fullpart==1 &  wkswork2==6  ///
								  , by(year)
				tempfile median_1966_ind
				save `median_1966_ind' 
									  				
			*merge all datasets, and compute mw to median ratios for 1- industries covered in 1938, 2- industries covered in 1966 (except farms) 	 	 
				merge 1:1 year using `median_1938_ind' 	
					rename _merge m_median_1938_ind
				merge 1:1 year using `median_1966_ind' 	
					rename _merge m_median_1966_ind			
				merge 1:1 year using `mw' 	
					rename _merge m_median_mw		
				merge 1:1 year using `KI_y2' 	
				drop _merge m_median_1938_ind m_median_1966_ind m_median_mw
				
				*compute annual mw FTFY
				gen amw_1938FLSA_state  = hmw_1938FLSA_state*40*52   /*40 hours of work a week, 52 weeks a year*/
				gen amw_1966FLSA_state  = hmw_1966FLSA_state*40*52  /*40 hours of work a week, 52 weeks a year*/

				*mw to median ratios
				gen mw_to_median_1938_ind =.
				replace mw_to_median_1938_ind = amw_1938FLSA_state *100 / median_wage_1938_ind 
				
				gen mw_to_median_1966_ind =.
				replace mw_to_median_1966_ind = amw_1966FLSA_state *100 / median_wage_1966_ind		
		
		*d. Plot Figure 5.1	with state mw laws	
			keep if inrange(year,1961,1980)
			replace KI_y2_percent =. if KI_y2_percent ==0 & year==1962
			twoway connected mw_to_median_1938_ind mw_to_median_1966_ind KI_y2_percent year, xline(1966.5, lcolor(myred) lw(medthick)) ///
							 xtitle("") ytitle("MW to median ratio (%)", color(gs4))  ylabel("0(10)50",labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
							 legend(label (1 "Industries covered in 1938") label(2 "Industries covered in 1967") ///
							 label(3 "Kaitz Index") region(lwidth(none))) ///
							 lcolor(mydeepblue myarticblue mybeige) mcolor(mydeepblue myarticblue mybeige) msize(medlarge medlarge medlarge) mlw(medium medium medthick) ///
							 msymbol(circle circle circle_hollow)
			gr export "figures/mw_to_median_ratio_DC_state.pdf", replace	
	
*---------------------------------------------------------------------------------------------
*FIGURE 6 [PAPER & SLIDES]: STATES WITH NO MINIMUM WAGE LAWS AS OF JANUARY 1966
*---------------------------------------------------------------------------------------------		
	use "data/output/mw_series_by_state_gender_industry_monthly.dta", clear
		drop pop_by_state-w2
		drop mw_nominal*
		
		reshape wide mw_by_state, i(year month statefip industry  mw_federal mw_1961_amendments mw_1966_amendments_except_farms mw_1966_amendments_farms) j(sex)
		rename  mw_by_state1 mw_by_state_men
		rename  mw_by_state2 mw_by_state_women
		
		*replace missing values (i.e. in states where there is no obs in the CPS for that industry and that gender) by the preceding value
		replace mw_by_state_men 	= mw_by_state_men[_n-1] 	if missing(mw_by_state_men) 		
		replace mw_by_state_women 	= mw_by_state_women[_n-1] 	if missing(mw_by_state_women) 		
		
		*create a flag variable for states that did not have a mw law in 1965
		gen  state_nomw_1966_women 	= (mw_by_state_women == 0) 	if  year==1966 & month==1 & flsa_coverage==1 
		gen  state_nomw_1966_men 	= (mw_by_state_men == 0) 	if  year==1966 & month==1 & flsa_coverage==1 

		collapse (mean)  region division south state_group ///
						 state_nomw_1966_women state_nomw_1966_men, by(state_name state_abb statefip) 
		
		gen   nomw_1966_women = ( state_nomw_1966_women==0)
		gen   nomw_1966_men   = (state_nomw_1966_men==0)
		gen   nomw_1966 	  = !inlist(state_group,4,5,11,13,15,16,18,21)
		label var nomw_1966 "0/1: state group with states that have no mw law in Jan 1966, covering at least 50% of the population of the state group"

		rename statefip 	statefips
		rename state_name	state_name 
		rename state_abb	state 
		
		maptile nomw_1966, geo(state) fcolor(gs13) twopt(legend(off)) savegraph("figures/map_strongly_weakly_treated_states.pdf") replace
		
*---------------------------------------------------------------------------------------------
*FIGURE B2 [APPENDIX]: STATE GROUPS USED IN MARCH CPS (1962-1980)
*---------------------------------------------------------------------------------------------		
		maptile state_group, geo(statehex) cutvalues(1(0.5)21) fcolor(Rainbow) twopt(legend(off)) savegraph("figures/map_state_groups_cps.pdf") replace  
		
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*FIGURE B3 [APPENDIX]: EVOLUTION OF BLACK AND WHITE EMPLOYMENT IN TREATED AND CONTROL INDUSTRIES			
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
	**FIGURE B3a [APPENDIX]: EMPLOYMENT SHARES IN CONTROL VS. TREATED INDUSTRIES	
		use "data/output/cps_master_individual_level.dta", clear	
		collapse (mean) share_1967=covered_1966 share_1938=covered_1938    /// 
				  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200)  [w=weight], by(year)		
			
		twoway 	(connected share_1938  year if inrange(year,1961,1980), lcolor(mydeepblue) mcolor(mydeepblue) mlcolor(mydeepblue) msize(medlarge) sort) ///
				(connected share_1967 year if inrange(year,1961,1980), lcolor(myarticblue) mcolor(myarticblue) mlcolor(myarticblue) msize(medlarge) msymbol(circle) sort), ///
			     ytitle("Fraction in total employment (%)") xtitle("") ///
				 xline(1966.5, lcolor(myred)) yline(0,lcolor(gs7) lw(medthin))  ///
				 legend(order(1 "Industries covered in 1938" 2 "Industries covered in 1967") ring(0) position(3) bmargin(large) color(gs1) c(1) region(style(none))  ) graphregion(color(white))			
		gr export "figures/emp_share_tc.pdf", replace	
		
	**FIGURE B3b [APPENDIX]: BLACK EMPLOYMENT SHARES WITHIN 1938 and 1967 industries	
		use "data/output/cps_master_individual_level.dta", clear	
		gen black = (race==200)
		preserve
		collapse (mean) black_share_1938=black   /// 
				  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & covered_1938 [w=weight], by(year)		
		tempfile 1938
		save 	`1938'	
		restore
		
		collapse (mean) black_share_1967=black   /// 
				  if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & covered_1966 [w=weight], by(year)		
		merge 1:1 year using `1938'
		tab _merge 
		drop _merge
		
		twoway 	(connected black_share_1938 year if inrange(year,1961,1980), lcolor(mydeepblue) lw(medthick) msymbol(triangle_hollow) msize(medlarge) mcolor(mydeepblue) mlcolor(mydeepblue)) ///
				(connected black_share_1967 year if inrange(year,1961,1980), lcolor(myarticblue) lw(medthick) msymbol(triangle_hollow) msize(medlarge) mcolor(myarticblue) mlcolor(myarticblue)), ///
			     ytitle("Share of black workers (vs. white) in total employment (%)") xtitle("") ///
				 ylabel("0(.1).2",labsize(medsmall) labcolor(gs4)) graphregion(color(white)) ///
				 xline(1966.5, lcolor(myred))  ///
				 legend(order(1 "Industries covered in 1938" 2 "Industries covered in 1967") ring(0) position(1) bmargin(large) color(gs1) c(1) region(style(none))  ) graphregion(color(white))			
		gr export "figures/emp_black_share_tc.pdf", replace			

		
		
*---------------------------------------------------------------------------------------------
*TABLE E11 [APPENDIX]: OCCUPATION BY RACE AND TREATMENT STATUS, 1960-1980
*---------------------------------------------------------------------------------------------	
	use "data/output/census_clean_1940_2017.dta", clear

	*create more detailed occupations in Census
	*n.e.c. stands for "not elsewhere classified"
		gen occ_detailed = . 
		*replace occ_detailed = 1 		if 	inrange(occ1950,000,099) 
			replace occ_detailed = 110 	if 	inlist(occ1950,093) | inrange(occ1950,012,029)
			replace occ_detailed = 120 	if 	inlist(occ1950,058,059) 
			replace occ_detailed = 190	if 	inrange(occ1950,000,099) & !inlist(occ1950,093,058,059) & !inrange(occ1950,012,029)
		replace occ_detailed = 2 		if 	inrange(occ1950,100,123) | inrange(occ1950,810,970)
		replace occ_detailed = 3 		if 	inrange(occ1950,200,290)
		replace occ_detailed = 4 		if 	inrange(occ1950,300,390)
		replace occ_detailed = 5 		if 	inrange(occ1950,400,490)
		replace occ_detailed = 6 		if 	inrange(occ1950,500,595)
		*replace occ_detailed = 7 		if 	inrange(occ1950,600,690)
			replace occ_detailed = 710 	if 	inlist(occ1950,690)		
			replace occ_detailed = 720 	if 	inlist(occ1950,621,624,625,632,682,683)	/*621: attendants, auto service and parking; 624: Brakemen, railroad; 625:Bus drivers; 632:Deliverymen and routemen; 682:Taxicab drivers and chauffers; 683:Truck and tractor drivers*/	
			replace occ_detailed = 730 	if 	inlist(occ1950,643)						/*laundry and dry cleaning operatives */
			replace occ_detailed = 790 	if  inrange(occ1950,600,690) & !inlist(occ1950,690,621,624,625,632,682,683, 643)
		*replace occ_detailed = 8 		if 	inlist(occ1950,700,790)	
			replace occ_detailed = 810 	if 	inlist(occ1950,781,730)	
			replace occ_detailed = 820 	if 	inlist(occ1950,784)	
			replace occ_detailed = 830 	if 	inlist(occ1950,754)	
			replace occ_detailed = 840 	if 	inlist(occ1950,770,780,764,753,751)	
			replace occ_detailed = 890 	if 	inrange(occ1950,700,790) & !inlist(occ1950,781,730,784,754,770,780,764,753,751)
		replace occ_detailed = 99		if 	inrange(occ1950,979,995)
		
		label var occ_detailed 			  "Detailed occupations, Census Bureau groupings of OCC1950"
		*label define occ_detailed_lbl 	1 "Professional, Technical"
			label define occ_detailed_lbl  110  "Teachers, professors and instructors", add
			label define occ_detailed_lbl  120  "Nurses", add		
			label define occ_detailed_lbl  190  "Other professional and technical", add
		label define occ_detailed_lbl  2  	  "Laborers and farmers", add
		label define occ_detailed_lbl  3  	  "Managers, officials and proprietors", add
		label define occ_detailed_lbl  4      "Clerical and kindred", add
		label define occ_detailed_lbl  5      "Sales workers", add				
		label define occ_detailed_lbl  6      "Craftsmen", add		
		*label define occ_detailed_lbl  7      "Operatives", add		
			label define occ_detailed_lbl  710 "Operative and kindred workers (n.e.c.)", add
			label define occ_detailed_lbl  720 "Drivers & deliverymen", add
			label define occ_detailed_lbl  730 "Laundry and dry cleaning operatives ", add
			label define occ_detailed_lbl  790 "Other Operatives", add
		*label define occ_detailed_lbl  8     "Service workers", add	
			label define occ_detailed_lbl  810  "Practical nurses and hospital attendants", add
			label define occ_detailed_lbl  820  "Waiters and waitresses", add
			label define occ_detailed_lbl  830  "Cooks, except private household", add
			label define occ_detailed_lbl  840  "Janitors, porters, and cleaners", add
			label define occ_detailed_lbl  890  "Other Service workers", add		
		label define occ_detailed_lbl  99     "Not classified", add		
		
		label val occ_detailed occ_detailed_lbl
	
		
		*create more detailed occupations in Census
		gen occ_agg = . 
		replace  occ_agg = 1 		if 	inrange(occ1950,000,099)
		replace occ_agg = 2 		if 	inrange(occ1950,100,123) | inrange(occ1950,810,970)
		replace occ_agg = 3 		if 	inrange(occ1950,200,290)
		replace occ_agg = 4 		if 	inrange(occ1950,300,390)
		replace occ_agg = 5 		if 	inrange(occ1950,400,490)
		replace occ_agg = 6 		if 	inrange(occ1950,500,595)
		replace occ_agg = 7 		if 	inrange(occ1950,600,690)
		replace occ_agg = 8 		if 	inlist(occ1950,700,790)	

		
		label var occ_agg 			  	"agg occupations, Census Bureau groupings of OCC1950"
		label define occ_agg_lbl  1 	"Professional, Technical"
		label define occ_agg_lbl  2 	"Laborers and farmers", add
		label define occ_agg_lbl  3  	"Managers, officials and proprietors", add
		label define occ_agg_lbl  4     "Clerical and kindred", add
		label define occ_agg_lbl  5     "Sales workers", add				
		label define occ_agg_lbl  6     "Craftsmen", add		
		label define occ_agg_lbl  7     "Operatives", add		
		label define occ_agg_lbl  8     "Service workers", add		
		label val occ_agg occ_agg_lbl
		
		
		*create dummies for each detailed occupation
		foreach i in 110 120 190 2 3 4 5 6  710 720 730 790 810 820 830 840 850 890 99 {
			gen occ_det_`i'  = (occ_detailed==`i')
		}
			gen occ_det_1 = (inrange(occ1950,000,099))
			gen occ_det_7 = (inrange(occ1950,600,690))
			gen occ_det_8 = (inrange(occ1950,700,790))
		
		 *include those characteristics in the table 
		global det_occ 	occ_det_7 occ_det_710 occ_det_720 occ_det_730 occ_det_790 ///
						occ_det_6 ///
						occ_det_4 ///
						occ_det_3 ///
						occ_det_1  occ_det_110 occ_det_120 occ_det_190 ///
						occ_det_5 ///
						occ_det_8 occ_det_810 occ_det_820 occ_det_830 occ_det_840  occ_det_890 ///
						occ_det_2 
		
		forvalues y = 1959(10)1979 {
		*control group
		**white
		estpost su $det_occ  if flag_employed  & inrange(age,25,55) & covered_1938 & inlist(race,1) &  inlist(year,`y') [fw=perwt]
		est store occ_det_control_w`y'
		**black
		estpost su $det_occ  if flag_employed  & inrange(age,25,55) & covered_1938 & inlist(race,2) &  inlist(year,`y')  [fw=perwt]
		est store occ_det_control_b`y'

		*treatment group
		**white
		estpost su $det_occ  if flag_employed  & inrange(age,25,55) & covered_1966 & inlist(race,1) &  inlist(year,`y')  [fw=perwt]
		est store occ_det_treatment_w`y'
		**black
		estpost su $det_occ  if flag_employed & inrange(age,25,55) & covered_1966 & inlist(race,2) &  inlist(year,`y')  [fw=perwt]
		est store occ_det_treatment_b`y'			
		}
	  *output table to csv file 
		esttab  occ_det_control_w1959 occ_det_control_b1959   occ_det_treatment_w1959  occ_det_treatment_b1959 ///
				occ_det_control_w1969 occ_det_control_b1969   occ_det_treatment_w1969  occ_det_treatment_b1969 ///
				occ_det_control_w1979 occ_det_control_b1979   occ_det_treatment_w1979  occ_det_treatment_b1979 ///
				using "tables/table_detailed_occ_stats.tex", replace  fragment ///
				coeflabel(	occ_det_1 "\rule{0pt}{3ex}\textit{Professional, Technical}" ///
								occ_det_110 "\hspace{0.5cm}Teachers, professors and instructors" ///
								occ_det_120 "\hspace{0.5cm}Nurses" ///
								occ_det_190 "\hspace{0.5cm}Other professional and technical" ///
							occ_det_2 "\rule{0pt}{3ex}\textit{Laborers and farmers}" ///			
							occ_det_3 "\rule{0pt}{3ex}\textit{Managers, officials and proprietors}" ///			
							occ_det_4 "\rule{0pt}{3ex}\textit{Clerical and kindred}" ///			
							occ_det_5 "\rule{0pt}{3ex}\textit{Sales workers}" ///			
							occ_det_6 "\rule{0pt}{3ex}\textit{Craftsmen}" ///		
							occ_det_7 "\rule{0pt}{3ex}\textit{Operatives}" ///							
								occ_det_710 "\hspace{0.5cm}Operative and kindred workers (n.e.c.)" ///			
								occ_det_720 "\hspace{0.5cm}Drivers \& deliverymen" ///			
								occ_det_730 "\hspace{0.5cm}Laundry and dry cleaning operatives" ///			
								occ_det_790 "\hspace{0.5cm}Other Operatives" ///			
							occ_det_8 "\rule{0pt}{3ex}\textit{Service workers}" ///			
								occ_det_810 "\hspace{0.5cm}Practical nurses and hospital attendants" ///			
								occ_det_820 "\hspace{0.5cm}Waiters and waitresses" ///			
								occ_det_830 "\hspace{0.5cm}Cooks, except private household" ///			
								occ_det_840 "\hspace{0.5cm}Janitors, porters, and cleaners" ///			
								occ_det_890 "\hspace{0.5cm}Other Service workers") ///			
				mtitle("White" "Black" "White" "Black" "White" "Black" "White" "Black" "White" "Black" "White" "Black") ///
				nolines  posthead(\cmidrule{2-13}) postfoot(\bottomrule \bottomrule) noobs	///
				cells(mean(fmt(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ))) label booktabs nonum collabels(none) 

				
*---------------------------------------------------------------------------------------------
*TABLE E10 [APPENDIX]: OCCUPATIONAL SEGREGAGTION, 1960-1980  
*---------------------------------------------------------------------------------------------				
	gen white_share = (race==1)
	gen black_share = (race==2)
	
	forval	year = 1959(10)1979 {
		estpost tabstat white_share [fw=perwt] if flag_employed  & inrange(age,25,55) & covered_1938 & inlist(race,1,2) &  inlist(year,`year'), by(occ_agg) statistics(mean) columns(statistics) listwise
		est store white_c_`year'_agg
		estpost tabstat black_share [fw=perwt] if flag_employed  & inrange(age,25,55) & covered_1938 & inlist(race,1,2) &  inlist(year,`year'), by(occ_agg) statistics(mean) columns(statistics) listwise
		est store black_c_`year'_agg
		estpost tabstat white_share [fw=perwt] if flag_employed  & inrange(age,25,55) & covered_1966 & inlist(race,1,2) &  inlist(year,`year'), by(occ_agg) statistics(mean) columns(statistics) listwise
		est store white_t_`year'_agg
		estpost tabstat black_share [fw=perwt] if flag_employed  & inrange(age,25,55) & covered_1966 & inlist(race,1,2) &  inlist(year,`year'), by(occ_agg) statistics(mean) columns(statistics) listwise
		est store black_t_`year'_agg
		
		estpost tabstat white_share [fw=perwt] if flag_employed  & inrange(age,25,55) & covered_1938 & inlist(race,1,2) &  inlist(year,`year'), by(occ_detailed) statistics(mean) columns(statistics) listwise
		est store white_c_`year'_det
		estpost tabstat black_share [fw=perwt] if flag_employed  & inrange(age,25,55) & covered_1938 & inlist(race,1,2) &  inlist(year,`year'), by(occ_detailed) statistics(mean) columns(statistics) listwise
		est store black_c_`year'_det
		estpost tabstat white_share [fw=perwt] if flag_employed  & inrange(age,25,55) & covered_1966 & inlist(race,1,2) &  inlist(year,`year'), by(occ_detailed) statistics(mean) columns(statistics) listwise
		est store white_t_`year'_det
		estpost tabstat black_share [fw=perwt] if flag_employed  & inrange(age,25,55) & covered_1966 & inlist(race,1,2) &  inlist(year,`year'), by(occ_detailed) statistics(mean) columns(statistics) listwise
		est store black_t_`year'_det

	}
	
		esttab white_c_1959_agg black_c_1959_agg white_t_1959_agg black_t_1959_agg white_c_1969_agg black_c_1969_agg white_t_1969_agg black_t_1969_agg white_c_1979_agg black_c_1979_agg white_t_1979_agg black_t_1979_agg ///
		using "tables/table_segregation_occ_stats.tex", replace  fragment ///
		mtitle("White" "Black" "White" "Black" "White" "Black" "White" "Black" "White" "Black" "White" "Black") ///
		nolines  posthead(\cmidrule{2-13}) noobs	///
		cells(mean(fmt(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ))) label booktabs nonum collabels(none) ///
		varlabels(`e(labels)') varwidth(20)	
		esttab white_c_1959_det black_c_1959_det white_t_1959_det black_t_1959_det white_c_1969_det black_c_1969_det white_t_1969_det black_t_1969_det white_c_1979_det black_c_1979_det white_t_1979_det black_t_1979_det ///
		using "tables/table_segregation_occ_stats.tex", append  fragment ///
		nomtitles nolines  postfoot(\bottomrule \bottomrule) noobs	///
		cells(mean(fmt(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ))) label booktabs nonum collabels(none) ///
		varlabels(`e(labels)') varwidth(20)			
		
		
		
		
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*FIGURE B1 [APPENDIX]: ANALYSIS SAMPLE BEFORE THE REFORM (1966)
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
		use "data/output/cps_master_individual_level.dta", clear	

		gen age_cat = . 
		replace age_cat = 1 if age<21
		replace age_cat = 2 if inrange(age,21,24)
		replace age_cat = 3 if inrange(age,25,55)
		replace age_cat = 4 if inrange(age,56,64)
		replace age_cat = 5 if age > 64
		
		gen self_emp = inlist(classwkr,10,13,14)
		
		collapse (sum) 			emp=flag_employed unemp =flag_unemployed nilf = flag_nilf niu=flag_niu iu=flag_universe ///
								[pw=weight] ///
								, by(year age_cat self_emp)	
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("sample") firstrow(variables) sheetmodify
		
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*FIGURES [APPENDIX] B4 and B5 cf. EXCEL WORKSHEET: cps_agg_stat_des_analysis_sample.xls
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
use "data/output/cps_master_individual_level.dta", clear	
	*number of obs in the CPS every year
	*unemployment 
	*not in the labor force
	*employment, nb and percent
	*employment shares (men, women, black, white)
	*average wages (all, men, women, black, white)
		
	**EMPLOYMENT NUMBERS REFLECT EMPLOYMENT NUMBERS USED IN THE EMPLOYMENT REGRESSIONS (STATE-GROUP DESIGN)	
	**WAGES REFLECT WAGES FOR PEOPLE EMPLOYED IN OUR ANALYSIS SAMPLE 
		
	capture program drop  create_var
	program define create_var
		gen ind_category =. 
	replace ind_category = 1 if inlist(industry,2,3,5,6,7,8,9,12,13,23)
	replace ind_category = 2 if inlist(industry,1,10,15,16,17,18,19)
	replace ind_category = 3 if inlist(industry,14)
	replace ind_category = 4 if inlist(industry,20,21,22)	
	replace ind_category = 5 if inlist(industry,4,11)	
	label var ind_category "Industry category :covered by 1938 FLSA (1), 1966 FLSA (2), private hh (3), public admin (4), 1961 FLSA (5)"			
	end
	
	gen flag=1
	
	*%NUMBER OF OBSERVATIONS
	**number of obs all
	preserve	
		collapse (count) 		obs_cps=flag ///
								, by(year industry sex race)	
		create_var
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("obs_cps") firstrow(variables) sheetmodify
	restore	
		
	**number of obs (for employment)	
	preserve	
		collapse (count) obs_emp=flag_employed ///
						 if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200)   ///
						 , by(year industry sex race)	
		create_var
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("obs_emp") firstrow(variables) sheetmodify
	restore	
	
	**number of obs (for unemployment)	
	preserve	
		collapse (count) obs_unemp=flag_unemployed ///
						 if flag_unemployed & inrange(age,25,55) & inlist(race,100,200)   ///
						 , by(year sex race)	
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("obs_unemp") firstrow(variables) sheetmodify
	restore	
	
	**number of obs (for not in the labor force)	
		preserve	
		collapse (count) obs_nilf=flag_nilf ///
						 if flag_nilf & inrange(age,25,55) & inlist(race,100,200)   ///
						 , by(year sex race)	
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("obs_nilf") firstrow(variables) sheetmodify
	restore	
		
	*%WEIGHTED OBERSVATIONS
	**ALL
	preserve	
		collapse (count) cps=flag [pw=weight] ///
						 , by(year industry sex race)	
		create_var
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("universe") firstrow(variables) sheetmodify
	restore	
		
	**Employment
	preserve	
		collapse (count) emp=flag_employed [pw=weight] ///
						 if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200)   ///
						 , by(year industry sex race)	
		create_var
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("emp") firstrow(variables) sheetmodify
	restore	
		
	**Unemployment
	preserve	
		collapse (count) unemp=flag_unemployed [pw=weight]  ///
						 if flag_unemployed & inrange(age,25,55) & inlist(race,100,200)   ///
						, by(year  sex race)	
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("unemp") firstrow(variables) sheetmodify
	restore	
		
	
	**Not in the labor force
	preserve	
		collapse (count) nilf=flag_nilf [pw=weight]  ///
						 if flag_nilf & inrange(age,25,55) & inlist(race,100,200)   ///
						 , by(year  sex race)	
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("nilf") firstrow(variables) sheetmodify
	restore	

	*%WAGES
	*all	
	preserve	
		collapse  (mean)  	annual_wage weekly_wage hourly_wage ///
				  (sum)		emp=flag_employed [pw=weight] if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) /// 			
								, by(year industry year)		
		
		create_var
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("wages_all") firstrow(variables) sheetmodify
	restore
		
	*men	
	preserve	
		collapse (mean)  	annual_wage weekly_wage hourly_wage ///
				  (sum)		emp=flag_employed [pw=weight] if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & sex==1 /// 			
								, by(year industry year)		
		
		create_var
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("wages_men") firstrow(variables) sheetmodify		
	restore
		
	*women	
	preserve	
		collapse (mean)  	annual_wage weekly_wage hourly_wage /// 			
				 (sum)		emp=flag_employed [pw=weight] if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & sex==2 /// 			
								, by(year industry year)		
		
		create_var
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("wages_women") firstrow(variables) sheetmodify		
	restore	
		
	*black	
	preserve	
		collapse (mean)  	annual_wage weekly_wage hourly_wage ///
				 (sum)		emp=flag_employed [pw=weight] if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & race==200 /// 			
								, by(year industry year)		
		
		create_var
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("wages_black") firstrow(variables) sheetmodify		
	restore
		
		
	*white	
	preserve	
		collapse (mean)  	annual_wage weekly_wage hourly_wage  /// 			
				 (sum)		emp=flag_employed [pw=weight] if flag_employed & in_sample & inrange(age,25,55) & inlist(race,100,200) & race==100 /// 			
								, by(year industry year)		
		
		create_var
		export excel using "figures/cps_agg_stat_des_analysis_sample.xls", sheet("wages_white") firstrow(variables) sheetmodify	
	restore	
	

