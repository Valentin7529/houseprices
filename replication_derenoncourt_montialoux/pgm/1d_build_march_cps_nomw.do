*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: Build the dataset that contains measures of annual, weekly and hourly average and median earnings at the individual and aggregated by year 
	*1. create state-group, industry and occupation harmonized codes for March CPS 1962-2016 
	*2. create treatment status variable, industry categories, and measure of intenisty of treatment (strongly treated states vs. weakly treated states)
	*3. merge the CPS with CPI-U-RS series
	*4. merge the CPS at the individual level with MW series 
	*5.create sample selection and annual, weekly and hourly wages
	*6.create a harmonized code for number of years of schooling, and potential experience
*first created: 11/28/2017
*last updated:  08/28/2020
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	


	local varlist "year ind ind1950 ind50ly ind1990 ind90ly cpi99 incwage age  sex empstat wkswork2 ahrsworkt gq classwkr fullpart labforce classwly asecwt reportyr region statefip metro metarea metfips race  educ  occ occly qocc  occ1950 occ50ly occ1990 occ90ly marst inctot incbus incfarm incunern"
	*varibles not found: incss incwelfr incgov incidr incaloth incretir inccssi incdrt incint
	use `varlist' using "data/raw/march_cps_1962_2017.dta", clear	

*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*1. create state-group, industry and occupation harmonized codes for March CPS 1962-2016 
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
	*0. preliminary cleaning
		*create year variable for the year income was earned, as opposed to survey year
			gen year_wage = year-1
			rename year year_cps
			rename year_wage year
			label var year "year earnings were earned"
			order year year_cps
			drop if !inrange(year,1961,2015)
			
		*there are some individuals with negative weights on asecwt in some years (1962,1968,1969,1970,1971,1972,1973,1974,1975) 
			gen weight = . 
			replace weight = max(0,asecwt)

		*479 observations are deleted	
			
		*create non-integer weights and a count variable (useful for future collapse)
			gen weight_int = int(weight)
			label var weight_int "integer weights (weight)"		
			gen count=1		
			
		
	*a.create state-group variable 
		*see the crosswalk in documentation>"spd_nb_of_adults_geography.xls"		
		gen 	state_var = . 
		replace state_var = statefip
		
		gen 	state_type = . 
		replace state_type = 1 if year_cps==1962		
		replace state_type = 2 if inrange(year_cps,1963,1967)
		replace state_type = 3 if inrange(year_cps,1968,1971)
		replace state_type = 4 if year_cps==1972		
		replace state_type = 5 if inrange(year_cps,1973,1976)
		replace state_type = 6 if year_cps>1976
		
		label var state_var "CPS statefip variable used for the crosswalk"
		label var state_type "State type used for the crosswalk"
	
		merge m:1 state_var state_type using "data/output/crosswalk_states.dta"
		drop _merge state_var state_type
		
		*drop obs with no state identified
		*drop if state_group ==22 | state_group==. | statefip==.
		
	*b.create industry variable 
		*see the crosswalk documentation in documentation>"spd_industrial_codes.xls"
		gen 	ind_var = . 
		replace ind_var = ind if inrange(year_cps,1962,1967)
		replace ind_var = ind1950 if year_cps>1967
		
		gen 	ind_type = . 
		replace ind_type = 1 if year_cps == 1962
		replace ind_type = 2 if inrange(year_cps,1963,1967)
		replace ind_type = 3 if year_cps > 1967
		
		label var ind_var "CPS industry variable used for the crosswalk"
		label var ind_type "Ind. type used for the crosswalk: 1 in 1962; 2 in 1963-1967; 3 in 1968-2016"
	
		merge m:1 ind_var ind_type using "data/output/crosswalk_industry.dta"
		drop if _m==2
		*1 observation dropped: there is nobody in the March CPS with code 998 (i.e. "industry not reported") in ind1950
		drop _merge ind_var ind_type
			
		
	*c.create occupation variable (aggregate the 1950 occupation harmonized code (IPUMS) with the Census Bureau occupational groupings) 
		gen occupation = . 
		replace occupation = 1 if 	inrange(occ1950,000,099)
		replace occupation = 2 if 	inrange(occ1950,100,123)
		replace occupation = 3 if 	inrange(occ1950,200,290)
		replace occupation = 4 if 	inrange(occ1950,300,390)
		replace occupation = 5 if 	inrange(occ1950,400,490)
		replace occupation = 6 if 	inrange(occ1950,500,595)
		replace occupation = 7 if 	inrange(occ1950,600,690)
		replace occupation = 8 if 	inrange(occ1950,700,720)
		replace occupation = 9 if 	inrange(occ1950,730,790)
		replace occupation = 10 if 	inrange(occ1950,810,840)		
		replace occupation = 11 if 	inrange(occ1950,910,979)		
		replace occupation = 12 if 	inrange(occ1950,980,995)	
		
		*there are many missing values for occupation (i.e. for occ1950) in 1961-1966, but we have information on occupation via the variable occ instead
		*tab industry if occupation==.
		*tab year industry if occupation==.
		*tab occ if occupation==.
		
		replace occupation =1  if occupation==.  & inrange(occ,1,6)   & inrange(year,1961,1966) 
		replace occupation =2  if occupation==.  & occ==7 & inrange(year,1961,1966) 
		replace occupation =3  if occupation==.  & inrange(occ,8,10)  & inrange(year,1961,1966) 
		replace occupation =4  if occupation==.  & inrange(occ,11,12) & inrange(year,1961,1966) 
		replace occupation =5  if occupation==.  & inrange(occ,13,14) & inrange(year,1961,1966) 
		replace occupation =6  if occupation==.  & inrange(occ,15,22) & inrange(year,1961,1966) 
		replace occupation =7  if occupation==.  & inrange(occ,23,28) & inrange(year,1961,1966) 
		replace occupation =8  if occupation==.  & occ==29 & inrange(year,1961,1966) 
		replace occupation =9  if occupation==.  & inrange(occ,30,33) & inrange(year,1961,1966) 
		replace occupation =10 if occupation==.  & occ==34 & inrange(year,1961,1966) 
		replace occupation =11 if occupation==.  & inrange(occ,35,37) & inrange(year,1961,1966) 		
		
	
		label var occupation 			"Occupation, Census Bureau groupings of OCC1950"
		label define occupation_lbl  1  "Professional and technical", add
		label define occupation_lbl  2  "Farmers", add		
		label define occupation_lbl  3  "Managers, Occificals and proprietors", add
		label define occupation_lbl  4  "Clerical and kindred", add
		label define occupation_lbl  5  "Sales workers", add
		label define occupation_lbl  6  "Craftsmen", add
		label define occupation_lbl  7  "Operatives", add
		label define occupation_lbl  8  "Service workers (private households)", add				
		label define occupation_lbl  9  "Service workers (not private households)", add		
		label define occupation_lbl  10 "Farm laborers", add
		label define occupation_lbl  11 "Laborers (other than Farm)", add
		label define occupation_lbl  12 "nilf, unemployed", add
		
		label values occupation occupation_lbl		
		
		
		*indicator for clerical worker or not
		gen flag_clerical = . 
		replace flag_clerical = 1 if occupation == 4 
		replace flag_clerical = 2 if occupation !=4
		
		label var flag_clerical 			"Occupation information: whether occ is clerical worker (1) or not (2)"
		label define flag_clerical_lbl  1  	"Clerical worker", add
		label define flag_clerical_lbl  2  	"Not a clerical worker", add
		label values flag_clerical flag_clerical_lbl			

*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*2. create treatment status variable, industry categories, and measure of intenisty of treatment (strongly treated states vs. weakly treated states)
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
	*a. control/treatment groups		
		gen covered_1966 = (inlist(industry,1,10,15,16,17,18,19))
		label var covered_1966 "0/1: industry covered in 1966, i.e. treatment group"
		
		gen covered_1938 = (inlist(industry,2,3,5,6,7,8,9,12,13,23))
		label var covered_1938 "0/1: industry covered in 1938, i.e. control group"	
		
		gen covered_all = (!inlist(industry,4,11,14,20,21,22,00))
		label var covered_all "0/1: industry covered in 1938 or in 1966, i.e. either T or C group"

	*b. public/private	sectors
		gen flag_private 	= (!inlist(industry,20,21,22,00))
		gen flag_public 	= (inlist(industry,20,21,22))	
		label var flag_private "0/1: 1 if private sector"
		label var flag_public "0/1: 1 if public sector"
	
	*c. industry categories (control, treatment, treated later, etc.)				
		gen ind_category =. 
		replace ind_category = 1 if inlist(industry,2,3,5,6,7,8,9,12,13,23)
		replace ind_category = 2 if inlist(industry,1,10,15,16,17,18,19)
		replace ind_category = 3 if inlist(industry,14)
		replace ind_category = 4 if inlist(industry,20,21,22)	
		replace ind_category = 5 if inlist(industry,4,11)	
		label var ind_category "Industry category :1.1938,2.1966,3.priv. hh,4.public,5.1961"
				
	*d. states with no mw law							
		*gen nomw_1967 = inlist(state_group,18,5,11,13,4,15,16)
		*label var nomw_1967 "0/1: state group with states that have no mw law in Jan 1967, covering at least 50% of the population of the state group"
		*note: Arkansas-Louisiana-Oklahoma (17), Delaware-Maryland-Virginia-West Virginia (21) and Montana-Wyoming-Colorado-New Mexico-Utah-Nevada-Arizona-Idaho (20) dropped as they contain only one minor state with no law
		
		*gen nomw_1966 = inlist(state_group,4,5,10,11,13,15,16,17,18,21)
		gen nomw_1966 = inlist(state_group,4,5,11,13,15,16,18,21)

		label var nomw_1966 "0/1: state group with states that have no mw law in Jan 1966, covering at least 50% of the population of the state group"
		*the list of strongly treated state groups is: Florida, Illinois, Alabama-Mississiippi, North Carolina-South Carolina-Georgia
			*Kentucky-Tennessee, Iowa-North Dakota-etc,Delaware-Maryland, Virginia, West Virginia
		
		*gen nomw_1965 = inlist(state_group,18,5,11,13,4,15,16,10,40)
		*label var nomw_1965 "0/1: state group with states that have no mw law in Jan 1965, covering at least 50% of the population of the state group"	
		*create a 0/1 treatment variable with states with a state minimum wage and states with no MW legislation at the time of the 1966 amendments

*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*3. merge the CPS with CPI-U-RS series
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
		merge m:1 year using "data/raw/spd_cpi_u_rs_annual.dta"
		drop if !inrange(year,1961,2015)
		drop _merge
		label var cpi_u_rs "Annual average CPI-U-RS Using Current Methods All items (2017=100)"

*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*4.create sample selection and annual, weekly and hourly wages
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	 
  *a.create universe of labor market	
		*employed
			*employed (armed forces (empstat==1), at work or not last week (empstat==10,12))
			
		*employed 
		gen flag_employed 	= inlist(empstat,10,12) & labforce==2
		
		*unemployed
		gen flag_unemployed = (labforce==2 & inlist(empstat,20,21,22))

		*not in the labor force
		gen flag_nilf = (labforce==1 & inrange(empstat,30,36)) 
		
		*universe = in the labor force (employed+unemployed) + not in the labor force
		gen flag_universe = (flag_employed==1 | flag_unemployed==1 | flag_nilf==1)
		
		*not in universe 
		gen flag_niu = (labforce==0 &  inlist(empstat,0,1))

		*1 if employed, 0 if unemployed OR NILF
		gen 	flag_employed_nilf = . 
		replace flag_employed_nilf =1 if flag_employed==1
		replace flag_employed_nilf =0 if (flag_unemployed==1 | flag_nilf==1)
		tab 	flag_employed_nilf if flag_universe==1, missing

		*1 if employed, 0 if unemployed
		gen 	flag_employed_unemp = . 
		replace flag_employed_unemp =1 if flag_employed==1
		replace flag_employed_unemp =0 if flag_unemployed==1 
		tab 	flag_employed_unemp if flag_universe==1, missing
		
		label var flag_employed "0/1: 1 if employed"
		label var flag_unemployed "0/1: 1 if unemployed"
		label var flag_nilf "0/1: 1 if nilf"
		label var flag_universe "0/1: 1 if in universe (adults 21-64 either emp, unemp or nilf)"
		label var flag_employed_nilf "0/1: 1 if employed, 0 if unemp or nilf"
		label var flag_employed_unemp "0/1: 1 if employed, 0 if unemp"
	
		*sample restrictions 
			*(ii) 	employed at work or employed but not at work last week (inlist(empstat,10,12)), ie does NOT exlcudes employed, not at work last week. But de facto they are excluded with the condition "& inrange(ahrsworkt,4,150)" 
			*(iii) 	aged 16-64 (inrange(age,16,64))
			*(iv) 	not self-employed !inlist(classwkr,10,13,14) or unpaid family worker(!inlist(classwkr,29))
			*(v)	not in grouped quarters (gq!=2)
			*(vi)	has positive, non missing variable incwage (incwage > 0 & incwage!=99998 & incwage!=9999998), and works more than 13 weeks a year, and more than 3 hours a week (!inlist(wkswork2,0,.,1,9) & inrange(ahrsworkt,4,100))
			*(vii)	has industry and occupation code (!inlist(industry,00,.) & !inlist(occupation,.,12)) 
		
		gen in_sample = (inlist(empstat,10,12) & inrange(age,16,64) & !inlist(classwkr,10,13,14,29) & gq!=2 & !inlist(wkswork2,0,.,1,9) & inrange(ahrsworkt,4,150) ///
							& !inlist(industry,00,.) & !inlist(occupation,.,12) & incwage > 0 & incwage!=99998 & incwage!=9999998)
		
		label var in_sample "0/1: 1 if in sample of analysis (except for condition on age)"
	
		
	*b. annual wages	
		*deal with topcoded wage in 1965
		* see https://www.stata.com/statalist/archive/2013-04/msg00446.html
		gen flag_topcoded 	= (incwage==99999 | incwage==9999999) | (incwage==99900 & inrange(year,1961,1966)) |  (incwage==50000 & inrange(year,1967,1980)) | (incwage==75000 & inrange(year,1981,1983)) ///
								| (incwage==99999 & inrange(year,1984,1986))| (incwage==199998 & inrange(year,1987,1994))
		label var flag_topcoded "0/1: 1 if obs with wage topcoded and needs replacement value"
			*evaluate importance of share of topcoded data among employed people, by year
			*tab year if flag_topcoded & flag_employed
		egen rank_hi = rank(-incwage) if flag_employed==1 & flag_topcoded==0, by(year ind_category) unique
		egen hi1 = max(incwage) if flag_employed==1 & flag_topcoded==0, by(year ind_category)		
			*print the value of the highest income that is not top-coded, by year and industry category (covered in 1938, 1966, domestic service, public service, covered in 1961) 
			*tabdisp year ind_category, c(hi1)

			*construct the annual wage variable with no topcode, replacing topcode by 1.5 the value of the highest value		
			*create pre-tax salary/wage in 2017 dollars
		/*	
		*deal with low wages at the top in 1962
		*br year industry sex race state_group weight incwage if incwage>49000 & year<1965 & flag_employed & covered_all
		*br year industry sex race state_group weight incwage educ fullpart classwkr  ahrsworkt occupation if year==1962 & flag_employed & industry==12  & state_group==02 & sex==1 & race==100 & inrange(weight,2191,2192)
			*change inwage from 60000 to 90000 for one white male worker in Connecticut in Finance, sales worker
		replace incwage=90000 if year==1962 & industry==12  & state_group==02 & sex==1 & race==100 & occupation==5 & inrange(weight,2191,2192)
			*change inwage from 50000 to 60000 for  one white male worker in Iowa-N Dakota-S Dokota-Nebraska-Kansas-Minnesota-Missouri in Business & Repair Services, sales worker
		*br year industry sex race state_group weight incwage educ fullpart classwkr  ahrsworkt occupation if year==1962 & flag_employed & industry==13  & state_group==18 & sex==1 & race==100 & inrange(weight,2336,2337)
		replace incwage=60000  if year==1962 & flag_employed & industry==13  & state_group==18 & sex==1 & race==100 & inrange(weight,2336,2337) & ahrsworkt==66
		*/		
			
			*replace missing values in hi1 (i.e. for topcoded data) by value of hi1 in the specific group
			gen hi1_nomiss = hi1
			bysort year ind_category (hi1_nomiss) : replace hi1_nomiss = hi1_nomiss[_n-1] if missing(hi1_nomiss) 	
			*bysort year ind_category (hi1_nomiss) :assert (hi1_nomiss == hi1_nomiss[1]) | missing(hi1_nomiss) 
			
			gen 		annual_wage =. 
			replace 	annual_wage = incwage*cpi_u_rs   if flag_employed==1 & flag_topcoded==0 
			replace 	annual_wage = hi1_nomiss*1.5*cpi_u_rs   if flag_employed==1 & flag_topcoded 			
			
			label var 	annual_wage "pre-tax annual salary/wage of current year (2017 dollars), w treatment of topcoded data"		
			drop rank_hi hi1 hi1_nomiss
			
	*c. weekly wages
			*create the continuous weeks worked variable based on the midpoint of the cps variable ranges
			gen weeks_mid = . 
			replace weeks_mid = 7      	if wkswork2 == 1
			replace weeks_mid = 20.5   	if wkswork2 == 2
			replace weeks_mid = 33     	if wkswork2 == 3
			replace weeks_mid = 43.5   	if wkswork2 == 4
			replace weeks_mid = 48.5   	if wkswork2 == 5
			replace weeks_mid = 51   	if wkswork2 == 6
			replace weeks_mid = .   	if wkswork2 == 9
		
			*weekly wages
			gen weekly_wage_presmooth = annual_wage / weeks_mid
			set seed 653549
			gen rand = uniform()
			gen smth_weekly = 10 - (20*rand)
			gen weekly_wage = .
			replace weekly_wage = max(0,weekly_wage_presmooth + smth_weekly)
			
	*d. hourly wages
			gen hourly_wage_presmooth = annual_wage / (weeks_mid * ahrsworkt)
			gen smth_hourly = 0.25 - (0.5 * rand)
			*create smoothed wages
			gen hourly_wage = .
			replace hourly_wage = max(0,hourly_wage_presmooth + smth_hourly)
		
			drop weekly_wage_presmooth  hourly_wage_presmooth smth_weekly smth_hourly rand weeks_mid
			label var weekly_wage "Weekly pre-tax wage/salary of current year ($2017)"
			label var hourly_wage "Hourly pre-tax wage/salary of current year ($2017)"

	*e. log of wage variables
			gen ln_annual_wage=.
			replace ln_annual_wage=log(annual_wage) if flag_employed
			
			gen ln_weekly_wage=.
			replace ln_weekly_wage=log(weekly_wage) if flag_employed
			
			gen ln_hourly_wage=.			
			replace ln_hourly_wage=log(hourly_wage) if flag_employed
	
	*f. log hours
			gen ln_hours = . 
			replace ln_hours = log(ahrsworkt) if flag_employed
	
			*annual number of hours
			gen ahours = . 
			replace ahours = . if hourly_wage == .  | hourly_wage<0
			replace ahours = 0 if hourly_wage ==0
			replace ahours = annual_wage /hourly_wage if hourly_wage>0
			
			gen ln_ahours = . 
			replace ln_ahours = log(ahours) if ahours>0
					
			*winsorized annual wages
			winsor2 ln_annual_wage, suffix(_w199) cuts(1 99) by(year)
			winsor2 ln_annual_wage, suffix(_w595) cuts(5 95) by(year)
			
			label var ln_annual_wage 		"log(annual_wage)"
			label var ln_annual_wage_w199 	"log(annual_wage), cuts at 1 and 99%"
			label var ln_annual_wage_w595 	"log(annual_wage), cuts at 5 and 95%"			
			label var ln_weekly_wage 		"log(weekly_wage)"
			label var ln_hourly_wage 		"log(hourly_wage)"
	
	/*g. Share of workers below the federal minimum wage in 1967
			su hmw_1966FLSA if year==1967 & covered_1966 & industry!=1
			global federal_mw_1967 `r(mean)'
			gen share_atb_mw_1967 = (flag_employed & inlist(race,100,200) & covered_1966 & inrange(year,1961,1966) & hourly_wage <=$federal_mw_1967 & hourly_wage>0)
	*/
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*5.create a harmonized code for number of years of schooling, and potential experience
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	 		
	*a.create schooling variable 
		gen 	schooling = . 
		replace schooling = . 	if educ== 1
		replace schooling = 0 	if educ==002
		replace schooling = 1 	if educ==011
		replace schooling = 2 	if educ==012
		replace schooling = 3 	if educ==013
		replace schooling = 4 	if educ==014
		replace schooling = 5 	if educ==021	
		replace schooling = 6 	if educ==022
		replace schooling = 7 	if educ==031
		replace schooling = 8 	if educ==032
		replace schooling = 9 	if educ==040
		replace schooling = 10 	if educ==050
		replace schooling = 11 	if educ==060
		replace schooling = 12 	if educ==070
		replace schooling = 12 	if educ==071
		replace schooling = 12 	if educ==072
		replace schooling = 12 	if educ==073
		replace schooling = 13 	if educ==080
		replace schooling = 13 	if educ==081
		replace schooling = 14 	if educ==090
		replace schooling = 14 	if educ==091
		replace schooling = 14 	if educ==092
		replace schooling = 15 	if educ==100
		replace schooling = 16 	if educ==110
		replace schooling = 16 	if educ==111
		replace schooling = 17 	if educ==121
		replace schooling = 18 	if educ==122
		replace schooling = 18 	if educ==123
		replace schooling = 18 	if educ==124
		replace schooling = 21 	if educ==125
		replace schooling = .  	if educ==999
/*
	gen flag_intitial_dataset = 1	
	*use "$resultspath\output data\cps_master_individual_level.dta", clear
	preserve
	*b.impute shooling variable in 1962
		replace flag_intitial_dataset=0
		
		gen 	age_cat=.
		replace age_cat = 1 if inrange(age,14,24)
		replace age_cat = 2 if inrange(age,25,55)
		replace age_cat = 3 if age>55
		
		xtile 	wage_quartile = annual_wage if annual_wage>0 & inrange(year,1961,1964) [pw=weight], nq(4)
		replace wage_quartile = 0 if inlist(annual_wage,.,0) & inrange(year,1961,1964)
		
		hotdeck schooling if inrange(year,1961,1964), by(race sex state_group age_cat fullpart industry flag_nilf flag_unemployed flag_employed wage_quartile) keep(_all) store impute(1) seed(12345)		
		use "data/raw/imp1.dta", clear
			twoway 	(hist schooling if year==1961 & flag_employed [fw=weight_int],width(1) color(myred%10)) ///
					(hist schooling if year==1962 & flag_employed [fw=weight_int],width(1) color(myarticblue%30)) ///
					(hist schooling if year==1963 & flag_employed [fw=weight_int],width(1) fcolor(none) lcolor(mydeepblue)) ///
					,legend(order(1 "1961" 2 "1962" 3 "1963"))	
			keep if year==1962
			drop age_cat wage_quartile
		save "data/output/imp_schooling.dta", replace
	
	
	restore
	 append using "data/output/imp_schooling.dta"
	 drop if flag_intitial_dataset & year==1962
	 drop flag_intitial_dataset
	*/
	
	*b.create "potential experience" (see DiNardo, Fortin, Lemieux (1996) = age - schooling - 5). Alternatively, see (Mincer (1974) and Card (1999), Handbook of Labor Economics = age - schooling - 6)
		gen exp = . 
		replace exp = max(0,age - schooling - 5) 
		
		gen exp_square = . 
		replace exp_square = exp^2

		gen exp_cubic = . 
		replace exp_cubic = exp^3
		
		gen age_square = . 
		replace age_square = age^2
		
		gen age_cubic = . 
		replace age_cubic = age^3
		
		label var exp "potential experience"  
		label var exp_square "potential experience, square"  
		label var exp_cubic "potential experience, cubic"  	
		label var age_square "age, square"  
		label var age_cubic "age, cubic"  
		

	*c.create three time periods (before, after1 and after2)
			*for earnings and annual number of hours worked
			gen time = . 
			replace time=1 if inrange(year,1961,1966)
			replace time=2 if inrange(year,1967,1972)
			replace time=3 if inrange(year,1973,1980)
			
			*for employment outcomes
			gen time_emp = . 
			replace time_emp=1 if inrange(year,1961,1965)
			replace time_emp=2 if inrange(year,1966,1971)
			replace time_emp=3 if inrange(year,1972,1980)
		
	*d. winsorize all covariates
		*schooling
			winsor2 schooling, suffix(_w199) cuts(1 99) by(year)
			winsor2 schooling, suffix(_w595) cuts(5 95) by(year)
		
		*experience 
			winsor2 exp, suffix(_w199) cuts(1 99) by(year)
			winsor2 exp, suffix(_w595) cuts(5 95) by(year)
			
			winsor2 exp_square, suffix(_w199) cuts(1 99) by(year)
			winsor2 exp_square, suffix(_w595) cuts(5 95) by(year)			
	
			winsor2 exp_cubic, suffix(_w199) cuts(1 99) by(year)
			winsor2 exp_cubic, suffix(_w595) cuts(5 95) by(year)		
		
		*number of hours worked per week
			winsor2 ahrsworkt, suffix(_w199) cuts(1 99) by(year)
			winsor2 ahrsworkt, suffix(_w595) cuts(5 95) by(year)				
		
		*number of weeks worked per year
			winsor2 wkswork2, suffix(_w199) cuts(1 99) by(year)
			winsor2 wkswork2, suffix(_w595) cuts(5 95) by(year)
			
			label var schooling_w199 	"schooling, cuts at 1 and 99%"
			label var schooling_w595 	"schooling, cuts at 5 and 95%"
			label var exp_w199 			"exp, cuts at 1 and 99%"
			label var exp_w595 			"exp, cuts at 5 and 95%"	
			label var exp_square_w199 	"exp square, cuts at 1 and 99%"
			label var exp_square_w595 	"exp square, cuts at 5 and 95%"	
			label var exp_cubic_w199 	"exp cubic, cuts at 1 and 99%"
			label var exp_cubic_w595 	"exp cubic, cuts at 5 and 95%"	
			label var ahrsworkt_w199 	"hours of work per week, cuts at 1 and 99%"
			label var ahrsworkt_w595 	"hours of work per week, cuts at 5 and 95%"			
			label var wkswork2_w199 	"weeks worked laste year, cuts at 1 and 99%"
			label var wkswork2_w595 	"weeks worked laste year, cuts at 5 and 95%"	
			
			sort year statefip state_group		
			order	year year_cps  statefip state_group metro metarea metfips region age sex race schooling exp exp_square exp_cubic marst empstat labforce industry occupation  ///
					weight cpi_u_rs covered_1938 covered_1966 covered_all flag_employed flag_unemployed flag_nilf flag_universe flag_niu in_sample annual_wage ln_annual_wage ln_annual_wage_w199 ln_annual_wage_w595 ///
					weekly_wage ln_weekly_wage hourly_wage ln_hourly_wage 

	*drop year 1962
	*drop if year==1962
	
	
	saveold "data/output/cps_master_individual_level_nomw.dta", replace
