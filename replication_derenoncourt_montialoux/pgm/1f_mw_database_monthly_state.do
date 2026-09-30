*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: 		create mw database
*first created: 01/01/2018
*last updated:  08/28/2020
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	

clear all
set more off
set matsize 10000
set maxvar 10000


*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*0. Rearrange Vaghul-Zipperer MW database from 1980 and save (source of the state monthly file http://equitablegrowth.org/working-papers/historical-state-and-sub-state-minimum-wage-data/)
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
	use "data/raw/VZ_state_monthly.dta"
		keep statefips monthly_date min_*
		
		rename statefips statefip
		format monthly_date %10.0g
		
		*create year and month variables
		gen date = dofm(monthly_date)
		format date %d
			gen month = month(date)
			gen year = year(date)
			keep if inrange(year,1981,2015)
			
		drop date monthly_date
		rename min_fed_mw mw_federal
		rename min_mw mw_by_state_men
		
		gen mw_by_state_women = . 
		replace mw_by_state_women =mw_by_state_men
		
		
		gen mw_1961_amendments = . 
		replace mw_1961_amendments =mw_federal
		
		gen mw_1966_amendments_except_farms = . 
		replace mw_1966_amendments_except_farms =mw_federal
		
		gen mw_1966_amendments_farms = . 
		replace mw_1966_amendments_farms = mw_federal
		
		order statefip year month mw_federal mw_1961_amendments mw_1966_amendments_except_farms mw_1966_amendments_farms mw_by_state_men mw_by_state_women
	
	tempfile mw_state_monthly_1981_2015
	save `mw_state_monthly_1981_2015' 
	
	
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*1. Import monthly mw databases for men and women and reshape from wide to long
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
	*create a balanced panel state*month*year from 1950 to 2016
		clear
		set obs 56
		gen statefip = _n
		
		*remove statefip codes that do not exist
		drop if inlist(statefip,3,7,14,43,52)
		
		expand 66
		bys statefip: gen year = _n + 1949
		
		expand 12
		bys statefip year : gen month = _n 
		
		expand 24
		bys statefip year month: gen industry = _n-1
		order state year month industry
		
		tempfile panel_state_year_month_industry
		save `panel_state_year_month_industry' 
	
	
	*import monthly database men
	import excel "data/raw/spd_mw_series_men_monthly.xlsx", firstrow clear
		reshape long s, i(year month) j(statefip)
		rename s mw_by_state_men
		sort year month statefip

	append using `mw_state_monthly_1981_2015'
	drop mw_by_state_women
	
	merge 1:m year month statefip using  `panel_state_year_month_industry'
	drop _m
	sort year month  statefip industry 	
	order year month statefip industry 

	tempfile mw_men
	save `mw_men'
	
	*import monthly database women
	import excel "data/raw/spd_mw_series_women_monthly.xlsx", firstrow clear
		drop mw_federal mw_1961_amendments mw_1966_amendments_except_farms mw_1966_amendments_farms
		reshape long s, i(year month) j(statefip)
		rename s mw_by_state_women
		sort year month statefip
		
		append using `mw_state_monthly_1981_2015'
		drop mw_by_state_men  mw_federal mw_1961_amendments mw_1966_amendments_except_farms mw_1966_amendments_farms
	
	merge 1:m year month statefip using  `panel_state_year_month_industry'
	drop _m
	sort year month  statefip industry 
	order year month statefip industry 

	*merge the women and men databases
	merge 1:m  year month statefip industry  using `mw_men'
		drop _m
		order year month statefip industry  mw_federal mw_1961_amendments mw_1966_amendments_except_farms mw_1966_amendments_farms mw_by_state_men mw_by_state_women
		label var mw_federal 		"federal mw, nominal dollars"
		label var mw_by_state_men 	"state mw for men, nominal dollars"
		label var mw_by_state_women "state mw for women, nominal dollars"

		rename mw_by_state_men mw_by_state1
		rename mw_by_state_women mw_by_state2
		
		reshape long mw_by_state, i(year month statefip industry  mw_federal mw_1961_amendments mw_1966_amendments_except_farms mw_1966_amendments_farms) j(sex)
		order year month statefip industry  sex mw_federal mw_by_state

	*recode the statefip variable to get the state_group variable
		gen state_group = .

		replace state_group = 01 	if  statefip==6  
		replace state_group = 02 	if  statefip==9 
		replace state_group = 03 	if  statefip==11 
		replace state_group = 04 	if  statefip==12	
		replace state_group = 05 	if  statefip==17
		replace state_group = 06 	if  statefip==18
		replace state_group = 07 	if  statefip==34
		replace state_group = 08 	if  statefip==36			
		replace state_group = 09 	if  statefip==39			
		replace state_group = 10 	if  statefip==42		
		replace state_group = 11 	if  statefip==48
		replace state_group = 12 	if  inlist(statefip,26,55)
		replace state_group = 13 	if  inlist(statefip,1,28)		
		replace state_group = 14 	if  inlist(statefip,23,25,33,44,50)
		replace state_group = 15 	if  inlist(statefip,37,45,13)
		replace state_group = 16 	if  inlist(statefip,21,47)
		replace state_group = 17 	if  inlist(statefip,5,22,40)
		replace state_group = 18 	if  inlist(statefip,19,38,46,31,20,27,29)
		replace state_group = 19   if  	inlist(statefip,53,41,2,15) 
		replace state_group = 20   if  	inlist(statefip,30,56,8,35,49,32,4,16) 
		replace state_group = 21   if  	inlist(statefip,10,24,51,54)		
		replace state_group = 22   if  	statefip == 99	| statefip == .		

		label var state_group 	"Unified statefips code for 1962-2016"

		label define state_group_lbl  01  "California", add
		label define state_group_lbl  02  "Connecticut", add		
		label define state_group_lbl  03  "District of Columbia", add
		label define state_group_lbl  04  "Florida", add
		label define state_group_lbl  05  "Illinois", add
		label define state_group_lbl  06  "Indiana", add
		label define state_group_lbl  07  "New Jersey", add
		label define state_group_lbl  08  "New York", add				
		label define state_group_lbl  09  "Ohio", add		
		label define state_group_lbl  10  "Pennsylvania", add
		label define state_group_lbl  11  "Texas", add
		label define state_group_lbl  12  "Michigan-Wisconsin", add
		label define state_group_lbl  13  "Alabama-Mississippi", add
		label define state_group_lbl  14  "Maine-Massachussets-New Hampshire-Rhode Island-Vermont", add
		label define state_group_lbl  15  "North Carolina-South Carolina-Georgia", add
		label define state_group_lbl  16  "Kentucky-Tennessee", add
		label define state_group_lbl  17  "Arkansas-Louisiana-Oklahoma", add
		label define state_group_lbl  18  "Iowa-N Dakota-S Dakota-Nebraska-Kansas-Minnesota-Missouri", add
		label define state_group_lbl  19  "Washington-Oregon-Alaska-Hawaii", add
		label define state_group_lbl  20  "Montana-Wyoming-Colorado-New Mexico-Utah-Nevada-Arizona-Idaho", add
		label define state_group_lbl  21  "Delaware-Maryland-Virginia-West Virginia", add
		label define state_group_lbl  22  "State not identified (appears in CPS years 1962,1963, and 1971)", add
		
		label values state_group state_group_lbl		

		order year month sex statefip state_group

	
	*create the division and region variable
		gen division = . 
		replace division = 11 if inlist(statefip,09,23,25,33,44,50)
		replace division = 12 if inlist(statefip,34,36,42)
		replace division = 21 if inlist(statefip,17,18,26,39,55)
		replace division = 22 if inlist(statefip,19,20,27,29,31,38,46)
		replace division = 31 if inlist(statefip,10,11,12,13,24,37,45,51,54)
		replace division = 32 if inlist(statefip,01,21,28,47)
		replace division = 33 if inlist(statefip,05,22,40,48)
		replace division = 41 if inlist(statefip,04,08,16,30,32,35,49,56)
		replace division = 42 if inlist(statefip,02,06,15,41,53)
		
		gen region = . 
		replace region = 1 if inlist(division,11,12)
		replace region = 2 if inlist(division,21,22)
		replace region = 3 if inlist(division,31,32,33)
		replace region = 4 if inlist(division,41,42)
		replace region = 97 if statefip==. | division==.
		
		gen south = (region==3)
		
		label var division 			   "Division"
		label define division_lbl  11  "New England", add
		label define division_lbl  12  "Middle Atlantic", add
		label define division_lbl  21  "East North Central", add
		label define division_lbl  22  "West North Central", add
		label define division_lbl  31  "South Atlantic", add
		label define division_lbl  32  "East South Central", add
		label define division_lbl  33  "West South Central", add
		label define division_lbl  41  "Mountain", add
		label define division_lbl  42  "Pacific", add	
		label values division division_lbl	
		
		label var region 			"Region"
		label define region_lbl  1  "Northeast", add
		label define region_lbl  2  "Midwest", add
		label define region_lbl  3  "South", add
		label define region_lbl  4  "West", add
		label define region_lbl 97 	"N/A",add
		label values region region_lbl	
		
		label var south 		   "0/1; 1: South; 0: not South"
		label define south_lbl  0  "Non-South", add
		label define south_lbl  1  "South", add		
		label values south south_lbl	
	
		order year month sex region division statefip state_group south

	
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*2. Create a variable "mw_nominal" which tells you the mininum wage that applies by state*gender*industry for each worker
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
		*i.e. state mw levels for retail trade (industry =10), agriculture (industry =1), restaurants (industry =21), hotels 
		*create a variable that indicates when the industry is covered by FLSA (1938, SEP 1961, FEB 1967, MAY 1974 or after 1980)
		gen 	flsa_coverage = . 
		replace flsa_coverage = 1 if inlist(industry,2,3,5,6,7,8,9,12,13,23) /*industries covered by 1938 FLSA */
		replace flsa_coverage = 2 if inlist(industry,4,11) /*industries newly covered by 1961 amendments */
		replace flsa_coverage = 3 if inlist(industry,1,10,15,16,17,18,19) /*industries newly covered by 1966 amendments */
		replace flsa_coverage = 4 if inlist(industry,14,20,22) /*industries newly covered by 1974 amendments */
		replace flsa_coverage = 5 if industry ==21 /*industries covered in 1986*/
		*missing values of flsa_coverage correpond to observations for which we do not have an industry code.
		
		*in 1976, in National League of Cities v. Usery, the Supreme Court held that the minimum wage and overtime provisions of the FLSA could not constitutionally apply 
		**to State and local government employees engaged in traditional government functions.
		
		*introduce the minimum wage values for the industries newly covered in 1961, 1966, and 1974. 
		*see p.254 (PDF) https://fraser.stlouisfed.org/files/docs/publications/women/b0290_dolwb_1965.pdf: double check info on minimum wage contained from p.233 (chapter 7)
		
	*1.Value of the minimum wage the worker is subjected to. If in industry not covered in 1938 FLSA, assumes no coverage before amendments. Corrections for wage orders by state*industry in part 2.  	
		gen mw_nominal = . 
		*industries covered by FLSA 1938, 1950-1980
		replace mw_nominal = max(mw_by_state,mw_federal) if flsa_coverage == 1
		
		*industries covered by FLSA 1961 (starting in September 1961), 1950-1980
			*assumes the entire retail trade sector (except eating and drinking places) is covered starting in 1961 (although only retail establishments with more than $1m are covered)
			*assumes state mw law applies before the reform (when there are specific state mw ordinances, we assume they prevail, see section below) --> see pp.233-234 in "1965 Handbook on Women Workers"
			*assumes the max of 1961 federal legislation and state mw law applies after the reform
		replace mw_nominal = mw_by_state if (flsa_coverage == 2 & year<1961) | (flsa_coverage == 2 & year==1961 & month<10)
		replace mw_nominal = max(mw_1961_amendments,mw_by_state) if (flsa_coverage == 2 & year>1961) | (flsa_coverage == 2 & year==1961 & month>9)
				
		*industries covered by 1966 amendments (starting Feb 1967), non farm
			*assumes state mw law applies before the reform --> see pp.233-234 in "1965 Handbook on Women Workers" (when there are specific state mw ordinances, we assume they prevail, see section below) --> see pp.233-234 in "1965 Handbook on Women Workers"
			*assumes the max between 1966 federal legislation and state mw law applies after the reform
		replace mw_nominal = mw_by_state if (flsa_coverage == 3 & industry!=1 & year<1967) | (flsa_coverage == 3 & industry!=1 & year==1967 & month==1)
		replace mw_nominal = max(mw_1966_amendments_except_farms,mw_by_state) if (flsa_coverage == 3 & industry!=1 & year>1967) | (flsa_coverage == 3 & industry!=1 & year==1967 & month>1)
		
		*industries covered by 1966 amendments (starting Feb 1967), (big) farms
			*assumes state mw law excludes agriculture before the reform --> see pp.233-234 in "1965 Handbook on Women Workers" (when there are specific state mw ordinances, we assume they prevail, see section below) 			
			*assumes state mw law applies in agriculture for the following states: 
				**California (6), Colorado (8), Kansas (20), Michigan (26), North Dakota (38), Oregon (41), Utah (49), Washington (53), Wisconsin (55) see Footnote 1 p.234 of "1965 Handbook on Women Workers"
			*assumes the max 1966 federal legislation for agri applies after the reform for the states that did not include agriculture in their state mw law. 
			*assumes the max between 1966 federal legislation for agri and state mw law applies after the reform for the states that included agriculture in their state mw law. 
		*(1) for states with a mw law that exclude agriculture from mw coverage 
		replace mw_nominal = 0 	if (flsa_coverage == 3 & industry==1 & !inlist(statefip,6,8,20,26,38,41,49,53,55) & year<1967) | (flsa_coverage == 3 & industry==1 & !inlist(statefip,6,8,20,26,38,41,49,53,55) & year==1967 & month==1)			
		replace mw_nominal = mw_1966_amendments_farms if (flsa_coverage == 3 & industry==1 & !inlist(statefip,6,8,20,26,38,41,49,53,55) & year>1967) | (flsa_coverage == 3 & industry==1 & !inlist(statefip,6,8,20,26,38,41,49,53,55) & year==1967 & month>1)
		*(2) for states with a mw law that do not exclude agriculture from mw coverage 
		replace mw_nominal = mw_by_state if (flsa_coverage == 3 & industry==1 & inlist(statefip,6,8,20,26,38,41,49,53,55) & year<1967) | (flsa_coverage == 3 & industry==1 &  inlist(statefip,6,8,20,26,38,41,49,53,55) & year==1967 & month==1)
		replace mw_nominal = max(mw_1966_amendments_farms,mw_by_state) if (flsa_coverage == 3 & industry==1 & inlist(statefip,6,8,20,26,38,41,49,53,55) & year>1967) | (flsa_coverage == 3 & industry==1 & inlist(statefip,6,8,20,26,38,41,49,53,55) & year==1967 & month>1)

		*industries covered by 1974 amendments (starting May 1974)
			*assumes state mw law excludes domestic service before the reform --> see pp.233-234 in "1965 Handbook on Women Workers" (when there are specific state mw ordinances, we assume they prevail, see section below) 			
			*assumes state mw law applies in domestic service for the following states
				*California (6), Colorado (8), Kansas (20), Michigan (26), North Dakota (38), Oregon (41), Utah (49), Washington (53), Wisconsin (55) see Footnote 1 p.234 of "1965 Handbook on Women Workers"
				*Domestic service workers are: day workers, chauffeurs, housekeepers, full-time babysitters and cooks
				*(1a) Domestic service in states where the state mw does not apply to domestic service
				replace mw_nominal = 0 if (flsa_coverage == 4 & industry==14 & !inlist(statefip,6,8,20,26,38,41,49,53,55)  & year<1974)  | (flsa_coverage == 4 & industry==14 & !inlist(statefip,6,8,20,26,38,41,49,53,55) & year==1974 & inlist(month,1,2,3))
				replace mw_nominal = mw_federal if (flsa_coverage == 4 & industry==14 & !inlist(statefip,6,8,20,26,38,41,49,53,55) & year>1974) | (flsa_coverage == 4 & industry==14 & !inlist(statefip,6,8,20,26,38,41,49,53,55) & year==1974 & inrange(month,4,12)) 

				*(1b) Domestic service in states where the state mw does apply to domestic service
				replace mw_nominal = mw_by_state if (flsa_coverage == 4 & industry==14 & inlist(statefip,6,8,20,26,38,41,49,53,55)  & year<1974)  | (flsa_coverage == 4 & industry==14 & inlist(statefip,6,8,20,26,38,41,49,53,55) & year==1974 & inlist(month,1,2,3))
				replace mw_nominal = max(mw_federal,mw_by_state) if (flsa_coverage == 4 & industry==14 & inlist(statefip,6,8,20,26,38,41,49,53,55) & year>1974) | (flsa_coverage == 4 & industry==14 & inlist(statefip,6,8,20,26,38,41,49,53,55) & year==1974 & inrange(month,4,12)) 
				
				*(2) Federal workers, postal service 
					*assumes that federal workers and workers in postal service are not covered by state mw law 
					*assumes that federal workers and workers in postal service are covered by federal law in 1974 -- state mw does not apply for them 
				replace mw_nominal = 0 if (flsa_coverage == 4 & inlist(industry,20,22) & year<1974)  | (flsa_coverage == 4 & inlist(industry,20,22) & year==1974 & inrange(month,1,4))
				replace mw_nominal = mw_federal if (flsa_coverage == 4 & inlist(industry,20,22) & year==1974 & inrange(month,5,12))  | (flsa_coverage == 4 & inlist(industry,20,22) & year>1974)
				
		*industries covered  in 1986 (state and local government employees)
		replace mw_nominal = 0 if (flsa_coverage == 5 & year<1986)  | (flsa_coverage == 5 & year==1986 & inlist(month,1,2,3))
		replace mw_nominal = max(mw_federal,mw_by_state)  if  (flsa_coverage == 5 & year>1986)  | (flsa_coverage == 5 & year==1986 & !inlist(month,1,2,3))
		
		
	*2. Corrections in the state_mw for the states that had specific state mw by industries. 
			**Alaska: 	1950-1954: retail trade $16/wk(=$0.4/hr if 40h worked per week); $18.72/wk (=$0.468/hr). Ignored, bc. there is already a state mw for women at $0.325/hr (which is lower than apparently the mw in RT). 
								*Assumes the state_mw for women applies to all workers (men and women) in retail (11) and in dry cleaning (15) (=0.15=0.3525/2 as I take into account there are hotels in this industry code as well). 
				replace mw_nominal = 0.3525 if statefip==2 & industry==11 & inrange(year,1950,1954)
				replace mw_nominal = 0.15 if statefip==2 &  industry==15 & inrange(year,1950,1954)
						*1959: State MW law exempts agriculture, fishermen, government employees, domestic service (already assumed in my assumptions)
						*1955-1967 (Jan) assumes state mw law applies to all men and women (except agriculture, fishermen, govt employees and domestic service)
			
			**Arizona:   1954 (AUG)-1966: retail trade $0.55/hr for men (women already covered by state law)
				*retail trade for men
				replace mw_nominal = 0.5475 if (statefip ==4 & industry == 11 & sex==1 & year==1954 & inrange(month,8,12)) | (statefip ==4 & industry == 11 & sex==1 & inrange(year,1955,1966)) | (statefip ==4 & industry == 11 & sex==1 & year==1967 & month==1)
				replace mw_nominal = max(mw_1966_amendments_except_farms, mw_by_state) if (statefip ==4 & industry == 11 & year==1967 & inrange(month,2,12)) | (statefip ==4 & industry == 11 & year>1967) 
				*dry cleaning for men (=1/2 *0.5475 as there are hotels that I believe are not covered by the mw ordinances specific to industries in that period)
				replace mw_nominal = 0.25 if (statefip ==4 & industry == 15 & sex==1 & year==1954 & inrange(month,8,12)) | (statefip ==4 & industry == 15 & sex==1 & inrange(year,1955,1966)) | (statefip ==4 & industry == 15 & sex==1 & year==1967 & month==1)
				replace mw_nominal = max(mw_1966_amendments_except_farms, mw_by_state) if (statefip ==4 & industry == 15 & year==1967 & inrange(month,2,12)) | (statefip ==4 & industry == 15 & year>1967) 
		
			**Arkansas: mw law applies only to men. $1 per day, i.e. approx $0.125/hr
			
			**California: 1950-1957: $0.65 for women, men not covered until 1974. 
						*1950-1957: wage orders in manufacturing, personal services, canning, public housekeeping, laundry and cly cleaning, amusement and recreation. Wage orders are ignored here, as the state mw prevails by law from 11/1957.
							*Women get the state mw. 
						*1961-1980: agricultural wage orders for farms employing 10 or more women and minors $1/hr, then additional wage orders. This rate is for women only.
				replace mw_nominal = 1 if (statefip ==6 & industry==1 & sex==2 & inrange(year,1962,1964)) | (statefip==6 & industry==1 & sex==2 & year==1961 & month>8) | (statefip==6 & industry==1 & sex==2 & year==1965 & month<9)		
				replace mw_nominal = 1.30 if (statefip==6 & industry==1 & sex==2 & year==1966) | (statefip==6 & industry==1 & sex==2 & year==1965 & month>8) | (statefip==6 & industry==1 & sex==2 & year==1967 & month==1)		
				replace mw_nominal = max(1.30,mw_1966_amendments_farms) if (statefip==6 & industry==1 & sex==2 & year==1967 & month>1) | (statefip==6 & industry==1 & sex==2 & year==1968 & month==1)
				replace mw_nominal = max(1.65,mw_1966_amendments_farms) if (statefip==6 & industry==1 & sex==2 & year==1968 & month>2) | (statefip==6 & industry==1 & sex==2 & year>1968)

			**Colorado: 	
				*only women covered by four wage orders. Men only covered at the state level in 07/1977. 
					*retail trade and laundries	
				replace mw_nominal = 0.5 if (statefip ==8 & inlist(industry,11,15) & sex==2 & inrange(year,1952,1955)) | (statefip ==8 & inlist(industry,11,15) & sex==2 & year==1951 & month>2) | (statefip ==8 & inlist(industry,11,15) & sex==2 & year==1956 & month<5)		
				replace mw_nominal = 0.7 if (statefip ==8 & inlist(industry,11,15) & sex==2 & inrange(year,1957,1965)) | (statefip ==8 & inlist(industry,11,15) & sex==2 & year==1956 & month>4) | (statefip ==8 & inlist(industry,11,15) & sex==2 & year==1966 & month<5)		
				replace mw_nominal = 0.95 if (statefip ==8 & inlist(industry,11,15) & sex==2 & year==1966 & month>4) | (statefip ==8 & inlist(industry,11,15) & sex==2 & year==1967 & month==1) 
				replace mw_nominal = max(0.95,mw_1966_amendments_except_farms) if statefip ==8 & inlist(industry,11,15) & sex==2 & year==1967 & inlist(month,2,3,4)
				replace mw_nominal = max(1.05,mw_1966_amendments_except_farms) if (statefip ==8 & inlist(industry,11,15) & sex==2 & inrange(year,1968,1977)) | (statefip ==8 & inlist(industry,11,15) & sex==2 & year==1967 & month>4) | (statefip ==8 & inlist(industry,11,15) & sex==2 & year==1978 & month==1) 
				replace mw_nominal = max(1.90,mw_1966_amendments_except_farms) if (statefip ==8 & inlist(industry,11,15) & sex==2 & year>1978) | (statefip ==8 & inlist(industry,11,15) & sex==2 & year==1978 & month>1) 
				
				*men at the level of the federal law, until 1977
					*1938 industries
				replace mw_nominal = mw_federal if  (statefip ==8 & flsa_coverage == 1 & sex==1 & inrange(year,1950,1976)) | (statefip ==8 & flsa_coverage == 1 & sex==1 & year==1977 & inrange(month,1,6))
				replace mw_nominal = max(mw_federal,mw_by_state) if  (statefip ==8 & flsa_coverage == 1 & sex==1 & year==1977 & month>6) 
					*1961 industries
				replace mw_nominal = mw_1961_amendments if  (statefip ==8 & flsa_coverage == 2 & sex==1 & inrange(year,1950,1976)) | (statefip ==8 & flsa_coverage == 2 & sex==1 & year==1977 & inrange(month,1,6))
				replace mw_nominal = max(mw_by_state,mw_1961_amendments) if  (statefip ==8 & flsa_coverage == 2 & sex==1 & year==1977 & month>6) 
					*1966 industries, except agriculture
				replace mw_nominal = mw_1966_amendments_except_farms if  (statefip ==8 & flsa_coverage == 3 & industry!=1 & sex==1 & inrange(year,1950,1976)) | (statefip ==8 & flsa_coverage == 3 & industry!=1 & sex==1 & year==1977 & inrange(month,1,6))
				replace mw_nominal = max(mw_by_state,mw_1966_amendments_except_farms) if  (statefip ==8 & flsa_coverage == 3 & industry!=1 & sex==1 & year==1977 & month>6) 
					*agriculture
				replace mw_nominal = mw_1966_amendments_farms if  (statefip ==8 & flsa_coverage == 3 & industry==1 & sex==1 & inrange(year,1950,1976)) | (statefip ==8 & flsa_coverage == 3 & industry==1 & sex==1 & year==1977 & inrange(month,1,6))
				replace mw_nominal = max(mw_by_state,mw_1966_amendments_farms) if  (statefip ==8 & flsa_coverage == 3 & industry==1 & sex==1 & year==1977 & month>6) 
					*1974 industries
				replace mw_nominal = 0 if  (statefip ==8 & flsa_coverage == 4 & sex==1 & inrange(year,1950,1973)) | (statefip ==8 & flsa_coverage == 4 & sex==1 & year==1974 & inrange(month,1,5))
				replace mw_nominal = mw_federal if  (statefip ==8 & flsa_coverage == 4 & sex==1 & year==1974 & month>5) | (statefip ==8 & flsa_coverage == 4 & sex==1 & inrange(year,1975,1976)) | (statefip ==8 & flsa_coverage == 4 & sex==1 & year==1977 & month<7)
				replace mw_nominal = max(mw_by_state,mw_federal) if  (statefip ==8 & flsa_coverage == 4 & sex==1 & year==1977 & month>6) 
					*1986 industries
				replace mw_nominal = 0 if  (statefip ==8 & flsa_coverage == 5 & sex==1 & inrange(year,1950,1976)) | (statefip ==8 & flsa_coverage == 5 & sex==1 & year==1977 & inrange(month,1,6))
				replace mw_nominal = max(mw_by_state,mw_federal) if  (statefip ==8 & flsa_coverage == 5 & sex==1 & year==1977 & month>6) 

				
			**New York State
				*Amusement and recreation (industry==16). Special rates from 1951.
				replace mw_nominal = 0.68 if (statefip==36 & industry==16 & year==1951 & month>4) | (statefip==36 & industry==16 & inrange(year,1952,1958)) | (statefip==36 & industry==16 & year==1959 & inlist(month,1,2))
				replace mw_nominal = 1.00 if statefip==36 & industry==16 & year==1959 & inrange(month,3,10) 
				replace mw_nominal = 1.05 if (statefip==36 & industry==16 & year==1959 & inlist(month,11,12)) | (statefip==36 & industry==16 & inrange(year,1960,1961)) | (statefip==36 & industry==16 & year==1962 & inrange(month,1,9))
				replace mw_nominal = 1.15 if (statefip==36 & industry==16 & year==1962 & inlist(month,10,11,12)) | (statefip==36 & industry==16 & year==1963) | (statefip==36 & industry==16 & year==1964 & inrange(month,1,9))
				replace mw_nominal = 1.25 if (statefip==36 & industry==16 & year==1964 & inlist(month,10,11,12)) | (statefip==36 & industry==16 & inrange(year,1965,1966)) 
				replace mw_nominal = 1.50 if statefip==36 & industry==16 & year==1967 & month==1
				replace mw_nominal = max(1.60,mw_1966_amendments_except_farms) if (statefip==36 & industry==16 & year==1967 & month>1) | (statefip==36 & industry==16 & year>1967)
				
				*Hotels and cleaning and dyeing services, laundries, beauty services and building services (industry==15). Assumes a weighted average of mw in all those subsectors until 1966. 
					*assumes max(state_mw,fed_mw) 
				replace mw_nominal = 0.575 if (statefip==36 & industry==15 & inrange(year,1950,1952)) | (statefip==36 & industry==15 & year==1953 &  month==1)
				replace mw_nominal = 0.75  if (statefip==36 & industry==15 & year==1953 & inrange(month,2,12))  | (statefip==36 & industry==15 & inrange(year,1954,1956)) | (statefip==36 & industry==15 & year==1957 & inlist(month,1,2))
				replace mw_nominal = 0.85  if (statefip==36 & industry==15 & year==1957 & inrange(month,4,12))  | (statefip==36 & industry==15 & inrange(year,1958,1960)) | (statefip==36 & industry==15 & year==1960 & inrange(month,1,9))
				replace mw_nominal = 1.03  if (statefip==36 & industry==15 & year==1960 & inrange(month,10,12)) | (statefip==36 & industry==15 & year==1961) | (statefip==36 & industry==15 & year==1962 & inrange(month,1,9))
				replace mw_nominal = 1.15  if (statefip==36 & industry==15 & year==1962 & inrange(month,10,12)) | (statefip==36 & industry==15 & year==1963) | (statefip==36 & industry==15 & year==1964 & inrange(month,1,9))
				replace mw_nominal = 1.25  if (statefip==36 & industry==15 & year==1964 & inrange(month,10,12)) | (statefip==36 & industry==15 & inrange(year,19635,1966)) 
				replace mw_nominal = 1.40  if statefip==36 & industry==15 & year==1967 & month==1
			
				*retail trade
				replace mw_nominal = 0.425 if (statefip==36 & industry==11 & inrange(year,1950,1952)) | (statefip==36 & industry==11 & year==1953 &  month==1)
				replace mw_nominal = 0.68  if (statefip==36 & industry==11 & year==1953 & inrange(month,2,12))  | (statefip==36 & industry==11 & inrange(year,1954,1956)) | (statefip==36 & industry==11 & year==1957 & month==1)
				replace mw_nominal = 1.00  if (statefip==36 & industry==11 & year==1957 & inrange(month,2,12))  | (statefip==36 & industry==11 & inrange(year,1958,1961)) | (statefip==36 & industry==11 & year==1962 & inrange(month,1,8))
				replace mw_nominal = 1.15  if (statefip==36 & industry==11 & year==1962 & inrange(month,10,12)) | (statefip==36 & industry==11 & year==1963) | (statefip==36 & industry==11 & year==1964 & inrange(month,1,8))
				replace mw_nominal = 1.25  if (statefip==36 & industry==11 & year==1964 & inrange(month,10,12)) | (statefip==36 & industry==11 & inrange(year,1965,1966))
				replace mw_nominal = 1.40  if (statefip==36 & industry==11 & year==1967 & month==1)
					
				*restaurants
				replace mw_nominal = 0.40 if (statefip==36 & industry==10 & inrange(year,1950,1952)) | (statefip==36 & industry==10 & year==1953 &  month==1)
				replace mw_nominal = 0.68  if (statefip==36 & industry==10 & year==1953 & inrange(month,2,12)) | (statefip==36 & industry==10 & inrange(year,1954,1956)) | (statefip==36 & industry==10 & year==1957 & month==1) 
				replace mw_nominal = 1.00  if (statefip==36 & industry==10 & year==1957 & inrange(month,2,12)) | (statefip==36 & industry==10 & inrange(year,1958,1961)) | (statefip==36 & industry==10 & year==1962 & inrange(month,1,8))
				replace mw_nominal = 1.10  if (statefip==36 & industry==10 & year==1962 & inrange(month,10,12))| (statefip==36 & industry==10 & year==1963 & inrange(month,1,8)) 
				replace mw_nominal = 1.25  if (statefip==36 & industry==10 & year==1963 & inrange(month,9,12)) | (statefip==36 & industry==10 & inrange(year,1964,1966)) | (statefip==36 & industry==10 & year==1967 & month==1)			
					
				*agriculture: mw at the state level for farms with payroll <1200 in preceding year at $1.40 in 10/1969. Overlaps with federal legislation on agriculture so state specific legislation ignored here. 
				
			**D.C. 
				*men not covered by state mw law until 2/1967
					*1938 industries 
				replace mw_nominal = 0 if  (statefip ==11  & flsa_coverage == 1 & sex==1 & inrange(year,1950,1966)) | (statefip ==11  & flsa_coverage == 1 & sex==1 & year==1967 & month==1)
				replace mw_nominal = max(mw_federal,mw_by_state) if  (statefip ==11  & flsa_coverage == 1 & sex==1 & year==1967 & month>1) | (statefip ==11  & flsa_coverage == 1 & sex==1 & year>1967)				
					*1961 industries 
				replace mw_nominal = 0 if  (statefip ==11  & flsa_coverage == 2 & sex==1 & inrange(year,1950,1960)) | (statefip ==11  & flsa_coverage == 2 & sex==1 & year==1961 & month<10)
				replace mw_nominal = max(mw_1961_amendments,mw_by_state) if  (statefip ==11  & flsa_coverage == 2 & sex==1 & year==1961 & month>10) | (statefip ==11  & flsa_coverage == 2 & sex==1 & year>1961)				
					*1966 industries, except agriculture 
				replace mw_nominal = 0 if  (statefip ==11  & flsa_coverage == 3 & industry!=1 & sex==1 & inrange(year,1950,1966)) | (statefip ==11  & flsa_coverage == 3 & industry!=1 & sex==1 & year==1967 & month==1)
				replace mw_nominal = max(mw_1966_amendments_except_farms,mw_by_state) if  (statefip ==11  & flsa_coverage == 3 & industry!=1 & sex==1 & year==1967 & month>1) | (statefip ==11  & flsa_coverage == 3 & industry!=1 & sex==1 & year>1967)				
					*1966 industries, agriculture 
				replace mw_nominal = 0 if  (statefip ==11  & flsa_coverage == 3 & industry==1 & sex==1 & inrange(year,1950,1966)) | (statefip ==11  & flsa_coverage == 3 & industry==1 & sex==1 & year==1967 & month==1)
				replace mw_nominal = max(mw_1966_amendments_farms,mw_by_state) if  (statefip ==11  & flsa_coverage == 3 & industry==1 & sex==1 & year==1967 & month>1) | (statefip ==11  & flsa_coverage == 3 & industry==1 & sex==1 & year>1967)				

				*hotels and laundries, women only until 2/1967. Then mw laws cover both men and women.
				replace mw_nominal = 0.55 if statefip==11 & industry==15 & inrange(year,1950,1951) & sex==2
				replace mw_nominal = 0.75 if statefip==11 & industry==15 & inrange(year,1952,1954) & sex==2
				replace mw_nominal = 0.80 if statefip==11 & industry==15 & inrange(year,1955,1957) & sex==2
				replace mw_nominal = 0.90 if statefip==11 & industry==15 & inrange(year,1958,1963) & sex==2
				replace mw_nominal = 1.00 if statefip==11 & industry==15 & year==1963 & sex==2
				replace mw_nominal = 1.05 if statefip==11 & industry==15 & year==1964 & sex==2
				replace mw_nominal = 1.10 if (statefip==11 & industry==15 & inrange(year,1965,1966) & sex==2	) | (statefip==11 & industry==15 & year==1967 & month==1 & sex==2)	
				
					  *starting in 2/1967, men and women are covered by state mw laws
				replace mw_nominal = max(1.25,mw_1966_amendments_except_farms) if (statefip==11 & industry==15& year==1968 & month==1) | (statefip==11 & industry==15& year==1967 & month>1)	
				replace mw_nominal = max(1.40,mw_1966_amendments_except_farms) if (statefip==11 & industry==15 & year==1968 & month>1) | (statefip==11 & industry==15 & year==1969 & month==1)	
				replace mw_nominal = max(1.60,mw_1966_amendments_except_farms) if (statefip==11 & industry==15 & year==1969 & month>1) 
				replace mw_nominal = max(1.80,mw_1966_amendments_except_farms) if (statefip==11 & industry==15 & year==1970) | (statefip==11 & industry==15 & year==1971 & month==1)
				replace mw_nominal = max(2.00,mw_1966_amendments_except_farms) if (statefip==11 & industry==15 & year==1971 & month>1) 
				replace mw_nominal = max(2.25,mw_1966_amendments_except_farms) if (statefip==11 & industry==15 & inrange(year,1972,1973))
				replace mw_nominal = max(2.35,mw_1966_amendments_except_farms) if (statefip==11 & industry==15 & year==1974) | (statefip==11 & industry==15 & year==1975 & inrange(month,1,11))
				replace mw_nominal = max(2.75,mw_1966_amendments_except_farms) if (statefip==11 & industry==15 & year==1975 & month==12)
				replace mw_nominal = max(2.80,mw_1966_amendments_except_farms) if (statefip==11 & industry==15 & year>1975)
			
				*restaurants
				replace mw_nominal = 0.55 if statefip==11 & industry==10 & inrange(year,1950,1951) & sex==2
				replace mw_nominal = 0.75 if statefip==11 & industry==10 & inrange(year,1952,1954) & sex==2
				replace mw_nominal = 0.80 if statefip==11 & industry==10 & inrange(year,1955,1959) & sex==2
				replace mw_nominal = 0.90 if statefip==11 & industry==10 & inrange(year,1960,1963) & sex==2
				replace mw_nominal = 1.05 if statefip==11 & industry==10 & year==1964 & sex==2	
				replace mw_nominal = 1.10 if (statefip==11 & industry==10 & inrange(year,1965,1966) & sex==2	) | (statefip==11 & industry==10 & year==1967 & month==1 & sex==2)	
					  *starting in 2/1967, men and women are covered by state mw laws
				replace mw_nominal = max(1.25,mw_1966_amendments_except_farms) if (statefip==11 & industry==10& year==1968 & month==1) | (statefip==11 & industry==10& year==1967 & month>1)	
				replace mw_nominal = max(1.40,mw_1966_amendments_except_farms) if (statefip==11 & industry==10 & year==1968 & month>1) | (statefip==11 & industry==10 & year==1969 & month==1)	
				replace mw_nominal = max(1.60,mw_1966_amendments_except_farms) if (statefip==11 & industry==10 & year==1969 & month>1) | (statefip==11 & industry==10 & year==1970) | (statefip==11 & industry==10 & year==1971 & month<8)
				replace mw_nominal = max(2.05,mw_1966_amendments_except_farms) if statefip==11 & industry==10 & year==1971 & month>7
				replace mw_nominal = max(2.25,mw_1966_amendments_except_farms) if statefip==11 & industry==10 & inrange(year,1972,1975) 
				replace mw_nominal = max(2.80,mw_1966_amendments_except_farms) if statefip==11 & industry==10 & year>1975 
				
			
				*retail trade 
				replace mw_nominal = 0.55 if statefip==11 & industry==11 & inrange(year,1950,1952)  & sex==2
				replace mw_nominal = 0.75 if statefip==11 & industry==11 & inrange(year,1953,1956) & sex==2
				replace mw_nominal = 0.90 if statefip==11 & industry==11 & inrange(year,1957,1961) & sex==2
				replace mw_nominal = 1.05 if statefip==11 & industry==11 & year==1962 & sex==2	
				replace mw_nominal = 1.10 if (statefip==11 & industry==11 & inrange(year,1963,1964) & sex==2) 
				replace mw_nominal = 1.10 if (statefip==11 & industry==11 & inrange(year,1963,1964) & sex==2) 
				replace mw_nominal = 1.25 if (statefip==11 & industry==11 & inrange(year,1965,1966) & sex==2) | (statefip==11 & industry==11 & year==1967 & month==1 & sex==2)
					  *starting in 2/1967, men and women are covered by state mw laws
				replace mw_nominal = max(1.25,mw_1966_amendments_except_farms) if (statefip==11 & industry==11& year==1968 & month==1) | (statefip==11 & industry==11& year==1967 & month>1)	
				replace mw_nominal = max(1.40,mw_1966_amendments_except_farms) if (statefip==11 & industry==11 & year==1968 & month>1) | (statefip==11 & industry==11 & year==1969 & month==1)	
				replace mw_nominal = max(1.60,mw_1966_amendments_except_farms) if statefip==11 & industry==11 & year==1969 & inrange(month,2,6)
				replace mw_nominal = max(1.80,mw_1966_amendments_except_farms) if (statefip==11 & industry==11 & year==1969 & month>6) | (statefip==11 & industry==11 & year==1970 & month<7)
				replace mw_nominal = max(2.00,mw_1966_amendments_except_farms) if (statefip==11 & industry==11 & year==1970 & month>6)  | (statefip==11 & industry==11 & inrange(year,1971,1973))
				replace mw_nominal = max(2.25,mw_1966_amendments_except_farms) if statefip==11 & industry==11 & inrange(year,1973,1975)
				replace mw_nominal = max(2.50,mw_1966_amendments_except_farms) if statefip==11 & industry==11 & year>1975
				
				*private household workers
				replace mw_nominal = mw_federal if (statefip==11 & industry ==14 & year==1974 & month>4) | (statefip==11 & industry ==14 & year==1975)	
				replace mw_nominal = max(2.50,mw_federal) if statefip==11 & industry ==14 & inrange(year,1973,1978)
				replace mw_nominal = max(3.50,mw_federal) if statefip==11 & industry ==14 & year>1978
				
				
				*food manufacturing
					*women only
				replace mw_nominal = 0.75 if statefip==11 & industry==6 & inrange(year,1950,1958)  & sex==2
				replace mw_nominal = 1.10 if statefip==11 & industry==6 & inrange(year,1959,1966)  & sex==2
				replace mw_nominal = 1.25 if statefip==11 & industry==6 & year==1967 & month==1 & sex==2
					*men and women
				replace mw_nominal = max(1.25,mw_1966_amendments_except_farms) if (statefip==11 & industry==6 & year==1967 & month>1) | (statefip==11 & industry==6 & year==1968 & month==1)
				replace mw_nominal = max(1.40,mw_1966_amendments_except_farms) if (statefip==11 & industry==6 & year==1968 & month>1) | (statefip==11 & industry==6 & inrange(year,1969,1972))
				replace mw_nominal = max(2.46,mw_1966_amendments_except_farms) if statefip==11 & industry==6 & inrange(year,1973,1978)
				replace mw_nominal = max(3.50,mw_1966_amendments_except_farms) if statefip==11 & industry==6 & inrange(year,1979,1980)
				
			**Connecticut 
				*men and women in laundry, cleaning, hotel and restaurants
				*they have the mw_by_state for now. refine this later if time. 
				*agriculture
				replace mw_nominal = max(1.61,mw_1966_amendments_farms) if (statefip==09 & industry==1 & year==1971 & month>9) | (statefip==09 & industry==1 & year==1972 & month<10)
				replace mw_nominal = max(1.70,mw_1966_amendments_farms) if (statefip==09 & industry==1 & year==1972 & month>9) | (statefip==09 & industry==1 & year==1973 & month<10)
				replace mw_nominal = max(1.85,mw_1966_amendments_farms) if (statefip==09 & industry==1 & year==1973 & month>9) | (statefip==09 & industry==1 & inrange(year,1974,1977)) | (statefip==09 & industry==1 & year==1978 & month<7)
				replace mw_nominal = max(2.66,mw_1966_amendments_farms) if statefip==09 & industry==1 & year==1978 & month>6
				replace mw_nominal = max(2.91,mw_1966_amendments_farms) if statefip==09 & industry==1 & year==1979 
				replace mw_nominal = max(3.12,mw_1966_amendments_farms) if statefip==09 & industry==1 & year==1980 
				
			**Hawaii
				*men and women have the state mw law in all 1966 covered industries (except farms). refine later if time. 

			**Idaho
				*men and women have the state mw law in all 1966 covered industries (except farms). refine later if time. after 1967 give the max between state_mw and mw_1966_amendments_except_farms)
					
			**Illinois
				*first mw law in 1972
				*replace mw_nominal = max(mw_by_state,mw_1966_amendments_except_farms) if statefip==17 & flsa_coverage == 3 & industry!=1 & year>1971
				*br if statefip==17 & year==1971
			
			**Kansas
				*first mw law in 1978
				*replace mw_nominal = max(mw_by_state,mw_1966_amendments_except_farms) if statefip==20 & flsa_coverage == 3 & industry!=1 & year>1977
				
			
			**Indiana
				*probably over estimate the rate they get by giving them the state_mw				
				*give the state mw to men and women in all 1966 covered industries (except farms). refine later if time. after 1967 give the max between state_mw and mw_1966_amendments_except_farms)
				replace mw_nominal = 1.00 if (statefip==18 & flsa_coverage == 3 & industry!=1 & inlist(year,1965,1966)) |  (statefip==18 & flsa_coverage == 3 & industry!=1 & year==1967 & month==1) 
				replace mw_nominal = max(mw_by_state,mw_1966_amendments_except_farms) if (statefip==18 & flsa_coverage == 3 & industry!=1 & year==1967 & month>1) | (statefip==18 & flsa_coverage == 3 & industry!=1 & year>1967)
				
			**Kentucky
				*men not covered by state mw law until 1/1966
					*1938 industries 
				replace mw_nominal = 0 if  (statefip ==21  & flsa_coverage == 1 & sex==1 & inrange(year,1950,1965))
				replace mw_nominal = max(mw_federal,mw_by_state) if  (statefip ==21  & flsa_coverage == 1 & sex==1 & year>1966)		
					*1961 industries 
				replace mw_nominal = 0 if  (statefip ==21  & flsa_coverage == 2 & sex==1 & inrange(year,1950,1965)) 
				replace mw_nominal = max(mw_1961_amendments,mw_by_state) if  (statefip ==21  & flsa_coverage == 2 & sex==1 & year>1966)				
					*1966 industries, except agriculture 
				replace mw_nominal = 0 if  (statefip ==21  & flsa_coverage == 3 & industry!=1 & sex==1 & inrange(year,1950,1965)) 
				replace mw_nominal = max(mw_1966_amendments_except_farms,mw_by_state) if  (statefip ==21 & flsa_coverage == 3 & industry!=1 & sex==1 & year>1966) 
					*1966 industries, agriculture 
				replace mw_nominal = 0 if  (statefip ==21  & flsa_coverage == 3 & industry==1 & sex==1 & inrange(year,1950,1965)) 
				replace mw_nominal = max(mw_1966_amendments_farms,mw_by_state) if  (statefip ==21 & flsa_coverage == 3 & industry==1 & sex==1 & year==1967 & month>1) | (statefip ==21  & flsa_coverage == 3 & industry==1 & sex==1 & year>1967)				

				*wage orders for women only in all industries until 1966, except domestic service. Special rates for hotels and restaurants (close to the state mw law anyway). Refine it time later. 
				
			**Maine
				*state law introduced in 11/1959
				
			**Maryland
				*no law until 1965
			
			**Massachussets	
				*probably over estimate the rate they get by giving them the state_mw				
				*wage orders in all industries covered by the 1966 amendments (except farms and schools)
				replace mw_nominal = mw_by_state if statefip==25 & inlist(industry,2,4,6,10,11,15,16,18) & inrange(year,1950,1959)
				replace mw_nominal = 0.8*mw_by_state if (statefip==25 & inlist(industry,2,4,6,10,11,15,16,18) & inrange(year,1960,1966)) | (statefip==25 & inlist(industry,2,4,6,10,11,15,16,18) & year==1967 & month==1)
				replace mw_nominal = max(mw_by_state,mw_1966_amendments_except_farms) if (statefip==25 & inlist(industry,2,4,6,10,11,15,16,18) & year==1966 & month>1) | (statefip==25 & inlist(industry,2,4,6,10,11,15,16,18) & year>1966)
				
			**Michigan 
				*starting in 1965 
			
			**Minnesota
				*probably over estimate the rate they get by giving them the state_mw		
				*men covered only in Jan 1971 in the sectors where there are statutory boards. Statutory law in 1974.
				*wage orders only cover women in industries that will be covered in 1966. 
				replace mw_nominal	= mw_by_state if (statefip==27 & sex==2 & flsa_coverage == 3 & industry!=1 & inrange(year,1950,1966)) | (statefip==27 & sex==2 & flsa_coverage == 3 & industry!=1 & year==1967 & month==1)
				replace mw_nominal	= max(mw_by_state,mw_1966_amendments_except_farms) if (statefip==27 & sex==2 & flsa_coverage == 3 & industry!=1 & year==1967 & month>1) | (statefip==27 & sex==2 & flsa_coverage == 3 & industry!=1 & year>1967)  
			
			**Montana
				*starting in 1971
				
			**Nebraska
				*starting in 1967
				
			**Nevada
				*probably over estimate the rate they get by giving them the state_mw				
				*mw law covers all women 
			
			**New Hampshire
				*probably over estimate the rate they get by giving them the state_mw				
				*wage orders only cover women in industries that will be covered in 1966. 

			**New Jersey
				*probably over estimate the rate they get by giving them the state_mw				
				*wage orders only cover women in industries that will be covered in 1966. 

			**New Mexico
				*probably over estimate the rate they get by giving them the state_mw				
				*wage orders cover men and women in industries that will be covered in 1966. 
			
			**North Carolina
				*probably over estimate the rate they get by giving them the state_mw	
				*men and women covered from 1960
				*statefip==37
			
			**North Dakota
				*probably over estimate the rate they get by giving them the state_mw				
				*wage orders only cover women in laundries and cleaning services and manufacturing. Men covered in 1966.
				*men covered in 1966
			
			**Ohio
				*wage boards for women only in food manufacturing & restaurants (food occupations), lodging and laundry and cleaning services, until 1974
				*probably over estimate the rate they get by giving them the state_mw	
			
			**Oklahoma
			
			**Oregon
				*wage boards for women only. not in retail, restaurants, hotels, schools

				*wage orders for retail in july 1962
				
				*all adult men covered by state law starting in 9/1971
				*men not covered by state mw law until 9/1971
					*1938 industries 
				replace mw_nominal = 0 if  (statefip ==41  & flsa_coverage == 1 & sex==1 & inrange(year,1950,1970)) |  (statefip ==41  & flsa_coverage == 1 & sex==1 & year==1971 & inrange(month,1,8))
				replace mw_nominal = max(mw_federal,mw_by_state) if  (statefip ==41  & flsa_coverage == 1 & sex==1 & year==1971 & month>8) | (statefip ==41  & flsa_coverage == 1 & sex==1 & year>1971) 		
					*1961 industries 
				replace mw_nominal = 0 if  (statefip ==41  & flsa_coverage == 2 & sex==1 & inrange(year,1950,1970)) |  (statefip ==41  & flsa_coverage == 2 & sex==1 & year==1971 & inrange(month,1,8))
				replace mw_nominal = max(mw_1961_amendments,mw_by_state) if  (statefip ==41  & flsa_coverage == 2 & sex==1 & year==1971 & month>8) | (statefip ==41  & flsa_coverage == 2 & sex==1 & year>1971) 			
					*1966 industries, except agriculture 
				replace mw_nominal = 0 if  (statefip ==41  & flsa_coverage == 3 & industry!=1 & sex==1 & inrange(year,1950,1970)) | (statefip ==41  & flsa_coverage == 3 & industry!=1 & sex==1 &  year==1971 & inrange(month,1,8)) 
				replace mw_nominal = max(mw_1966_amendments_except_farms,mw_by_state) if  (statefip ==41 & flsa_coverage == 3 & industry!=1 & sex==1 & year==1971 & month>8) | (statefip ==41 & flsa_coverage == 3 & industry!=1 & sex==1 & year>1971)   
					*1966 industries, agriculture 
				replace mw_nominal = 0 if  (statefip ==41  & flsa_coverage == 3 & industry==1 & sex==1 & inrange(year,1950,1970)) |  (statefip ==41  & flsa_coverage == 3 & industry==1 & sex==1 & year==1971 & inrange(month,1,8))
				replace mw_nominal = max(mw_1966_amendments_farms,mw_by_state) if  (statefip ==41 & flsa_coverage == 3 & industry==1 & sex==1 & year==1971 & month>8) | (statefip ==41  & flsa_coverage == 3 & industry==1 & sex==1 & year>1971)				
				
			**Pennsylvania
				*wage boards for women only until 1967. in restaurans, laundries (ignored here as there are also hotels in this category) and cleaning services 
				*probably overestimates the rate they get
				*statefip==42 
				*br if statefip==42 
				
				*wage orders for women only starting in 1959 for retail, and hotels
				
			**Rhode Island
				*wage orders cover men and women in laundries, retail, rest and hotels
				*statefip==44

			**South Dakota
				*wage orders cover women only in laundries, retail (="mercantile establishment"), rest and hotels, food manufacturing
				*statefip==46
				
			**Texas
				*no law until 1970
				
			**Utah
				*wage orders cover women only, in retail trade, restaurant, laundry and cleaning services (ignored) and public housekeeping (ignored).
				*statefip==49
						
			**Vermont
				*state mw law covers fisheries, food manufacturing, restaurants, retail, hotels, laundries, entertainment and recreation services, nursing homes, schools (hospitals? ignored for now). 
				*statefip==50
								
			**Virginia
				*no law until 1975
				
			**Washington
				*wage orders cover only women until 1959, in food manufacturing, retail (mercantile), laundries and cleaning services (ignored here), amusement and recreation
				*statefip==53 
				*wage orders for men
				
			**West Virginia
				*no law until 1967
				
			**Wisonsin
				*wage orders cover women only from 1967 in hotels and motels.
				*statefip==55
				
			**Wyoming
				*state law covers men and women from 2/1955, except in agri, domestic workers and public employees
				*statefip==56
				*br if statefip==56 & year==1954
				
		*add variable labels
		label var mw_nominal 	"MW that effectively applies to a particular worker (by state, gender, industry), nominal dollars"		
		label var mw_by_state 	"State MW that applies to industries covered by 1938 FLSA, nominal dollars"
		label var flsa_coverage "Categorical variable indicating when the industry is covered by the law (1938, 1961, 1966, 1974 or after)"
	
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*3. Merge the monthly database with annual population counts by state and CPI (CPI-U-RS and CPI99)
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
	*source: Census Bureau, see data>population
	merge m:1 year statefip using "data/output/pop_by_state_and_region.dta"
	keep if inrange(year,1950,2015)
	drop _m

	order year month statefip state_abb state_name  division region region_abb state_group south sex industry flsa_coverage mw_* pop*
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*4. Save monthly database at the state*industry*gender level, all months 1950-2015 	
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
	saveold "data/output/mw_series_by_state_gender_industry_monthly_inter.dta", replace
