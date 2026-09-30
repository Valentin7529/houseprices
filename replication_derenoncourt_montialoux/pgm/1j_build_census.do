*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: 		Build census database.  
*first created: 03/14/2018
*last updated:  08/28/2020
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	

* 2.  Clean raw census data 1940-2017 

use "data/raw/census_raw_1940_2017.dta", clear

* Merge in historical MW coverage status
* Generate summary statistics variables
* Race and gender
g black=(race==2)
g white=(race==1)
g women=(sex==2)
g men=(sex==1)

la var black "Black"
la var white "White"
la var women "Women"
la var men "Men"

* Age and prime age vars
g flag_adults_25_64 = (age>=25 & age <=64) 
g flag_adults_21_64 = (age>=21 & age <=64)
g primeage=(age>=21 & age<=64)

la var age "Age"
la var flag_adults_25_64 "0/1: 1 if adults 25-64"
la var flag_adults_21_64 "0/1: 1 if adults 21-64"
la var primeage "Prime working age (21-64)"

* Education
* 1. Less than high school (up to 3 years of high school or educ<6)
* 2. High school and some college (between 4 years of high school and three years of college or (educ>=6 & educ<=9))
* 3. College (four years of college or more or educ>9)
* 4. High school plus (at least four years of high school or (hs==1 | college==1)
g lths=(educ<6)
g hs=(educ>=6 & educ<=9)
g college=(educ>9)
g hsplus=(hs==1 | college==1)

la var lths "Less than HS"
la var hs "High school graduate"
la var college "College graduate"
la var hsplus "HS or more"

* Work vars
g worker=(empstat==1 & labforce==2)
*g employed=(empstat==1)

la var worker "Employed and in the labor force"
*la var employed "Employed"

* Clerical flag: occ1950 codes between 300 and 390 are for clerical and kindred workers
g clerical_worker=(occ1950>=300 & occ1950<400)

la var clerical_worker "Clerical worker"

* Region
g south=(region>=31 & region<=33)

la var south "South"

	*create industry codes
		gen industry=.
		
		replace industry = 00 	if  inlist(ind1950,0,976,979,980,982,983,984,986,987,991,997,998)
		replace industry = 01 	if  ind1950==105
		replace industry = 02 	if  inlist(ind1950,116,126) 
		replace industry = 03 	if  inlist(ind1950,206,216,226,236) 
		replace industry = 04 	if  inlist(ind1950,246) 	
		replace industry = 05 	if  inrange(ind1950,306,399) 
		replace industry = 06 	if  inrange(ind1950,406,426) 
		replace industry = 07 	if  inrange(ind1950,429,499) 
		replace industry = 08 	if  inrange(ind1950,506,598) 		
		replace industry = 09 	if  inrange(ind1950,606,627)		
		replace industry = 10 	if  ind1950==679  	
		replace industry = 11 	if  inlist(ind1950,636,637,646,647,656,657,658,659,667,668,669,686,687,688,689,696,697,698,699)			
		replace industry = 12 	if  inrange(ind1950,716,756) 	
		replace industry = 13 	if  inrange(ind1950,806,817)
		replace industry = 14 	if  ind1950==826  	
		replace industry = 15 	if  inrange(ind1950,836,849) 
		replace industry = 16 	if  inrange(ind1950,856,859) 
		replace industry = 17 	if  inlist(ind1950,868,879,896,897,898,899) 
		replace industry = 18 	if  ind1950==869  	
		replace industry = 19 	if  ind1950==888  	
		replace industry = 22 	if  ind1950==906  	
		replace industry = 20 	if  ind1950==916  	
		replace industry = 21 	if  inlist(ind1950,926,936,946)  	
		replace industry = 23 	if  inlist(ind1950,995,999) 
		
		
		label var industry 				"Industry code"
		label define industry_lbl 	00	"NIU"
		label define industry_lbl	01	"Agriculture", add
		label define industry_lbl	02	"Forestry and Fishing", add		
		label define industry_lbl	03	"Mining", add
		label define industry_lbl	04	"Construction", add
		label define industry_lbl	05	"Durable manufacturing", add
		label define industry_lbl	06	"Food manufacturing", add
		label define industry_lbl	07	"Other non-durable manufacturing", add
		label define industry_lbl	08	"Transportation, Communication, and Other Utilities", add
		label define industry_lbl	09	"Wholesale Trade", add
		label define industry_lbl	10	"Restaurants", add						
		label define industry_lbl	11	"Retail Trade", add		
		label define industry_lbl	12	"Finance, Insurance, and Real Estate", add
		label define industry_lbl	13	"Business and Repair Services", add
		label define industry_lbl	14	"Private households", add
		label define industry_lbl	15	"Hotels, laundries, and other personal services", add
		label define industry_lbl	16	"Entertainment and Recreation Services", add
		label define industry_lbl	17	"Nursing homes and other professional services", add
		label define industry_lbl	18	"Hospitals", add
		label define industry_lbl	19	"Schools and other educational services", add
		label define industry_lbl	20	"Federal government", add
		label define industry_lbl	21	"State or local government", add
		label define industry_lbl	22	"Postal service", add
		label define industry_lbl	23	"Other", add
		
		label values industry industry_lbl

  	*create industry variables
		gen covered_1966 = (inlist(industry,1,10,15,16,17,18,19))
		label var covered_1966 "0/1: industry covered in 1966, i.e. treatment group"

		gen covered_1938 = (inlist(industry,2,3,5,6,7,8,9,12,13,23))
		label var covered_1938 "0/1: industry covered in 1938, i.e. control group"	

		gen covered_all = (!inlist(industry,4,11,14,20,21,22,00))
		label var covered_all "0/1: industry covered in 1938 or in 1966, i.e. either T or C group"
  
		gen ind_category =. 
		replace ind_category = 1 if inlist(industry,2,3,5,6,7,8,9,12,13,23)
		replace ind_category = 2 if inlist(industry,1,10,15,16,17,18,19)
		replace ind_category = 3 if inlist(industry,14)
		replace ind_category = 4 if inlist(industry,20,21,22)	
		replace ind_category = 5 if inlist(industry,4,11)	
		label var ind_category "Industry category :1.1938,2.1966,3.priv. hh,4.public,5.1961"
  
  
  *create universe of labor market	
		*employed
			*(i) 	in the labor force (labforce==2)
			*(ii) 	employed (inlist(empstat,1)), ie exlcudes unemployed, N/A, and not in labor force 
			*(iii) 	aged 21-64 (inrange(age,21,64))
			*(iv) 	wage worker, i.e., not self-employed or N/A (classwkr!=1), not unpaid family worker
			*(v)	not in group quarters (!inlist(gq,3,4,5))
			*(vi)	has positive, non missing variable incwage (incwage > 0 & incwage!=99998 & incwage!=9999998), and works more than 13 weeks a year, and at least 1 hr a week (!inlist(wkswork2,0,1) & !inlist(hrswork2,0))
			*(vii)	has industry and occupation code (!inlist(ind1950, 0,997,998) & !inlist(occ1950,997,998))
			gen flag_employed = (labforce==2 & empstat==1 & inrange(age,21,64) & classwkr!=1 & !inlist(gq,3,4,5) & !inlist(wkswork2,0,1) & !inlist(hrswork2,0) ///
											& !inlist(industry,00,.) & !inlist(occ1950,997,998) & incwage > 0 & incwage!=999998 & incwage!=999999)										
											
		*unemployed
			gen flag_unemployed = (labforce==2 & inlist(empstat,2)) & inrange(age,21,64) & !inlist(gq,3,4,5) & classwkr!=1
			*not in the labor force
			gen flag_nilf = (labforce==1 & inrange(age,21,64)) 
		*universe = in the labor force (employed+unemployed) + not in the labor force
			gen flag_universe = (flag_employed==1 | flag_unemployed==1 | flag_nilf==1)
		
		label var flag_employed 	"0/1: 1 if employed"
		label var flag_unemployed 	"0/1: 1 if unemployed"
		label var flag_nilf 		"0/1: 1 if nilf"
		label var flag_universe 	"0/1: 1 if in universe (adults 21-64 either emp, unemp or nilf)"		
	
  *annual wages 
		*see list of topcodes by year: https://usa.ipums.org/usa-action/variables/INCWAGE#codes_section
		gen flag_topcoded 	= ((incwage==5001 & year==1940) | (incwage==10000 & year==1950) | (incwage==25000 & year==1960) | (incwage==50000 & year==1970) | (incwage==75000 & year==1980))
								/*| (incwage==140000 & year==1990) | (incwage==200000 & year==2000))*/
		*For Census Year 1990, any observed value greater than the Top Code value of $140,000 was coded as the median value greater than $140,000 
			*within that observation's state.	
		*For Census Year 2000, higher amounts are coded as the state means of values above the listed Top Code value for that specific Census year.	
		label var flag_topcoded "0/1: 1 if obs with wage topcoded and needs replacement value"
		
		*lots of topcoded data in 1960 (30%) and 1980 (60%), otherwise <5%
			*evaluate importance of share of topcoded data among employed people, by year
		tab year if flag_topcoded & flag_employed
		egen rank_hi = rank(-incwage) if flag_employed==1 & flag_topcoded==0, by(year ind_category) unique
		egen hi1 = max(incwage) if flag_employed==1 & flag_topcoded==0, by(year ind_category)
		
		
		*replace missing values in hi1 (i.e. for topcoded data) by value of hi1 in the specific group
			gen hi1_nomiss = hi1
			bysort year ind_category (hi1_nomiss) : replace hi1_nomiss = hi1_nomiss[_n-1] if missing(hi1_nomiss) 	
		
		*change years to the years when the income was earned
		replace year=1939 if year==1940
		replace year=1949 if year==1950
		replace year=1959 if year==1960
		replace year=1969 if year==1970
		replace year=1979 if year==1980
		replace year=1989 if year==1990
		replace year=1999 if year==2000
		*for ACS samples, leave as is
		
		g year_census = 1940 if year==1939
		replace year_census = 1950 if year==1949
		replace year_census = 1960 if year==1959
		replace year_census = 1970 if year==1969
		replace year_census = 1980 if year==1979
		replace year_census = 1990 if year==1989
		replace year_census = 2000 if year==1999
		replace year_census = 2010 if year==2010
		replace year_census = 2017 if year==2017	
		
		
		*merge with inflation as measured in CPI-U-RS
		merge m:1 year using "data/raw/spd_cpi_u_rs_annual.dta"
		keep if inlist(year,1939,1949,1959,1969,1979,1989,1999,2010,2017)
		
		*For 1940 then to convert from CPI 1999 dollars to $2017, multiply by 1.471809485 (see spd_cpi_u_rs_annual_edited)
		**conversion in 1999 dollars (CPI), see: https://usa.ipums.org/usa/cpi99.shtml
		gen 		annual_wage =. 
		replace 	annual_wage = incwage*11.986*1.471809485   if flag_employed==1 & flag_topcoded==0 & year==1939
		replace 	annual_wage = hi1*1.5*11.986*1.471809485   if flag_employed==1 & flag_topcoded==1 & year==1939
		
		*For years after 1939 
		replace 	annual_wage = incwage*cpi_u_rs   if flag_employed==1 & flag_topcoded==0 & year==1949
		replace 	annual_wage = hi1*1.5*cpi_u_rs   if flag_employed==1 & flag_topcoded==1 & year==1949		
		
		replace 	annual_wage = incwage*cpi_u_rs   if flag_employed==1 & flag_topcoded==0 & year==1959
		replace 	annual_wage = hi1*1.5*cpi_u_rs   if flag_employed==1 & flag_topcoded==1 & year==1959		
		
		replace 	annual_wage = incwage*cpi_u_rs   if flag_employed==1 & flag_topcoded==0 & year==1969
		replace 	annual_wage = hi1*1.5*cpi_u_rs   if flag_employed==1 & flag_topcoded==1 & year==1969	
		
		replace 	annual_wage = incwage*cpi_u_rs   if flag_employed==1 & flag_topcoded==0 & year==1979
		replace 	annual_wage = hi1*1.5*cpi_u_rs   if flag_employed==1 & flag_topcoded==1 & year==1979			
		
		replace 	annual_wage = incwage*cpi_u_rs   if flag_employed==1 & flag_topcoded==0 & year==1989
		replace 	annual_wage = hi1*1.5*cpi_u_rs   if flag_employed==1 & flag_topcoded==1 & year==1989			
		
		replace 	annual_wage = incwage*cpi_u_rs   if flag_employed==1 & flag_topcoded==0 & year==1999
		replace 	annual_wage = hi1*1.5*cpi_u_rs   if flag_employed==1 & flag_topcoded==1 & year==1999			
		
		replace 	annual_wage = incwage*cpi_u_rs   if flag_employed==1 & flag_topcoded==0 & year==2010
		replace 	annual_wage = hi1*1.5*cpi_u_rs   if flag_employed==1 & flag_topcoded==1 & year==2010	
		
		replace 	annual_wage = incwage*cpi_u_rs   if flag_employed==1 & flag_topcoded==0 & year==2017
		replace 	annual_wage = hi1*1.5*cpi_u_rs   if flag_employed==1 & flag_topcoded==1 & year==2017	
		
		gen ln_annual_wage = . 
		replace ln_annual_wage = log(annual_wage) 
		
		label var 	annual_wage 	"pre-tax annual salary/wage of current year (2017 dollars), w treatment of topcoded data"		
		label var 	ln_annual_wage  "pre-tax log annual salary/wage of current year (2017 dollars), w treatment of topcoded data"		
	
save "data/output/census_clean_1940_2017.dta", replace	
