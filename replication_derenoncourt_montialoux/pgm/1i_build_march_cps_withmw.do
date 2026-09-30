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

clear all
set more off
set matsize 10000
set maxvar 10000


*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*1. merge the CPS at the individual level with MW series 
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%		
use "data/output/cps_master_individual_level_nomw.dta", clear	
		*drop hmw_federal_nominal- amw_1966FLSA
		merge m:1 year sex state_group industry using "data/output/mw_series_by_state_group_gender_industry_annually.dta"
		*since the early cps files are at the state group level, not at the state level, we remove the variables statefip state_name and state_abb 
		drop state_abb state_name statefip
		
		*4,434,497 obs with _m==1 (i.e. obs for which there are obs in the CPS data but not in the mw database) 
			*of which 4,431,299 obs (99.93%) correspond to NIU
			*of which 2,753 obs correspond to obs not in universe (i.e. self-employed or working less than 13 hours a week, etc.)
			*of which 445 obs correspond to obs that are in universe 
			*of which 96 obs correspond to obs for workers who are employed (flag_employed==1) and for which state is not identified (happens in 1962, 1963 and 1971)
			*1 obs in 1971 with weight = 0 (i.e. asecwt<0)
			
		rename mw_federal 						hmw_federal_nominal
		rename mw_nominal_yjSg 					hmw_nominal_yjSg
		rename mw_nominal_yjS 					hmw_nominal_yjS
		rename mw_nominal_yj 					hmw_nominal_yj
		rename mw_nominal_yt 					hmw_nominal_yt
		rename mw_1961_amendments 				hmw_1961FLSA_nominal
		rename mw_1966_amendments_farms 		hmw_1966FLSA_agri_nominal 
		rename mw_1966_amendments_except_farms 	hmw_1966FLSA_nominal
		

		*convert hourly mw (current $) in hourly wages (2017 dollars) using CPI-U-RS
		gen hmw_federal 						= hmw_federal_nominal 				* cpi_u_rs
		gen hmw_yjSg							= hmw_nominal_yjSg					* cpi_u_rs
		gen hmw_yjS								= hmw_nominal_yjS					* cpi_u_rs
		gen hmw_yj								= hmw_nominal_yj					* cpi_u_rs	
		gen hmw_yt								= hmw_nominal_yt					* cpi_u_rs
		gen hmw_1961FLSA 						= hmw_1961FLSA_nominal 				* cpi_u_rs
		gen hmw_1966FLSA_agri 					= hmw_1966FLSA_agri_nominal 		* cpi_u_rs
		gen hmw_1966FLSA 						= hmw_1966FLSA_nominal 				* cpi_u_rs
	
		
		*label variables
		label var hmw_federal 					"Hourly federal MW ($2017)"
		label var hmw_yjSg						"Hourly MW by year industry state_group gender ($2017)"	
		label var hmw_yjS						"Hourly MW by year industry state_group ($2017)"
		label var hmw_yj						"Hourly MW by year industry ($2017)"
		label var hmw_yt						"Hourly MW by year treatment/control industry ($2017)"	
		label var hmw_1961FLSA 					"Hourly MW introduced by 1961 FLSA($2017)"
		label var hmw_1966FLSA_agri 			"Hourly MW introduced by 1966 FLSA, agriculture($2017)"
		label var hmw_1966FLSA 					"Hourly MW introduced by 1966 FLSA, except agri ($2017)"	
		
		
		*create annual, full-time equivalent mw
		gen amw_federal 						= hmw_federal		*40*52
		gen amw_yjSg							= hmw_yjSg			*40*52
		gen amw_yjS								= hmw_yjS			*40*52
		gen amw_yj								= hmw_yj			*40*52	
		gen amw_yt								= hmw_yt			*40*52
		gen amw_1961FLSA 						= hmw_1961FLSA		*40*52
		gen amw_1966FLSA_agri 					= hmw_1966FLSA_agri	*40*52
		gen amw_1966FLSA 						= hmw_1966FLSA		*40*52	
		
		 
		*label variables
		label var amw_federal 					"Annual, FT equivalent, federal MW ($2017)"
		label var amw_yjSg						"Annual, FT equivalent MW, by year industry state_group gender ($2017)"	
		label var amw_yjS						"Annual, FT equivalent MW, by year industry state_group ($2017)"
		label var amw_yj						"Annual, FT equivalent MW, by year industry ($2017)"
		label var amw_yt						"Annual, FT equivalent MW, by year treatment/control industry ($2017)"		
		label var amw_1961FLSA 					"Annual, FT equivalent, 1961 FLSA MW ($2017)"
		label var amw_1966FLSA_agri 			"Annual, FT equivalent, 1966 FLSA MW, agriculture ($2017)"
		label var amw_1966FLSA 					"Annual, FT equivalent, 1966 FLSA MW, except agri ($2017)"

saveold "data/output/cps_master_individual_level.dta", replace
		
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
*2. Create the 1966 KI measure and the fraction of affected workers
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%		
	*1.MEASURES OF KAITZ INDEX
	*a.VERSION 1 --> using federal law only, year level 		
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
		
		*c. merge the KI_y measure in the master database
			use "data/output/cps_master_individual_level.dta", clear	
				cap drop _merge 
				merge m:1 year using `KI_y'
				tab _merge 
				drop _merge 
	*b.VERSION 2 --> using state mw laws, year level		
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
		
		*c. merge the KI_y measure in the master database
				use "data/output/cps_master_individual_level.dta", clear	
				cap drop _merge 
				merge m:1 year using `KI_y2'	
				tab _merge 
				drop _merge 
		
		
	*c. COMPUTE KAITZ INDEX at the state level
		*make sure do file "1gBIS_weights_workers_by_state_group_for_Kaitz_Index" run
		*make sure do file "1hBIS_mw_database_annual_state_group_for_Kaitz_Index" run
		use "data/output/cps_master_individual_level.dta", clear
		drop _merge
		merge m:1 year sex state_group industry using "data/output/mw_series_by_state_group_gender_industry_annually_for_Kaitz_Index.dta"
		*since the early cps files are at the state group level, not at the state level, we remove the variables statefip state_name and state_abb 
		rename mw_nominal_ySg 	hmw_nominal_ySg
		rename mw_nominal_yS 	hmw_nominal_yS

		*convert hourly mw (current $) in hourly wages (2017 dollars) using CPI-U-RS
		gen hmw_ySg				= hmw_nominal_ySg * cpi_u_rs
		gen hmw_yS				= hmw_nominal_yS * cpi_u_rs
		*label variables
		label var hmw_ySg	  "Hourly MW by year industry state_group gender ($2017)"	
		label var hmw_yS	  "Hourly MW by year industry state_group ($2017)"
		*create annual, full-time equivalent mw
		gen amw_ySg		  	  = hmw_ySg*40*52
		gen amw_yS		  	  = hmw_yS*40*52
		label var amw_ySg	  "Annual, FT equivalent MW, by year state_group gender ($2017)"	
		label var amw_yS	  "Annual, FT equivalent MW, by year state_group gender ($2017)"	
		drop _merge
		
		*Calculate the Kaitz index at the year (y) * state_group level (S) level (and take into account how mw legislation vary by states)
		*KI_yS =SUMj (N_yjS/N_yS) * (MinWage_yjS/MedianWage_yS)			
		use "data/output/cps_master_individual_level.dta", clear
					collapse (median) 	median_wage_yS = annual_wage /// 
							 (sum) 		N_yS=count [pw=weight]  ///
							 if fullpart==1 & wkswork2==6 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22 ///
							 , by (year state_group)
					tempfile N_yS
					save 	`N_yS'			
			
		use "data/output/cps_master_individual_level.dta", clear
					collapse (mean) hmw_yjS hmw_1966FLSA hmw_1966FLSA_agri hmw_federal hmw_1961FLSA annual_wage ///
							 (sum) 	N_yjS=count [pw=weight]  ///
							 if fullpart==1 & wkswork2==6 & inrange(age,25,55) & inlist(race,100,200) & state_group!=22 ///
							 , by (year time industry state_group)
			merge m:1 year state_group using `N_yS'
				tab _merge 
				drop _merge 
			
			
			*create annual, full-time equivalent mw
			keep if inrange(year,1961,1980)
			
			*br if hmw_yjS==. & industry!=00 & year!=1962 & inrange(year,1961,1980)
			*only 5 obs mong these 18 observations have a non-zero wage -- so we only use those 5 observations and through the other ones
			drop if hmw_yjS==. & inlist(annual_wage,0,.) & industry!=00 & year!=1962 & inrange(year,1961,1980)
			
			replace hmw_yjS = 0 				if hmw_yjS==. & inlist(industry,16) & state_group==13 & year==1964
			replace hmw_yjS = 8.384386  		if hmw_yjS==.& inlist(industry,2) & inlist(state_group,8,21) & year==1972
			replace hmw_yjS = 9.133975 			if hmw_yjS==. & inlist(industry,2) & inlist(state_group,7,5) & year==1979
						
			gen amw_yjS		  	  = hmw_yjS*40*52
			label var amw_yjS	  "Annual, FT equivalent MW, by year state_group  ($2017)"	
			br if amw_yjS==. & year!=1962 & industry!=00
			*if amw_yjS==. , it is because of either year==1962 or industry==00; tha's OK
					
			gen KI_yjS = N_yjS/N_yS *  amw_yjS/median_wage_yS
			sort year state_group industry
			
			bysort year state_group: egen KI_yS = total(KI_yjS)		
			gen KI_yS_percent = KI_yS * 100
			replace KI_yS_percent = . if year==1962
			replace KI_yS = . if year==1962

			label var N_yS			"Number of workers working FTFY by year and state_group" 
			label var N_yjS			"Number of workers working FTFY by year, industry and state_group" 
			label var KI_yS			"Kaitz index by year and state-group"
			label var KI_yjS		"Kaitz index by year, industry and state-group"
			label var KI_yS_percent	"Kaitz index by year and state-group (percent)"
			drop if year==1962
			keep year state_group KI_yS
			collapse (mean) KI_yS, by(year state_group)
			
			tempfile KI_yS 
			save `KI_yS'
			
			*use "data\output\KI_yjS.dta", clear	
			collapse (mean) KI_1966S = KI_yS if year==1966, by( state_group)
			tempfile KI_1966S 
			save `KI_1966S'
			
			*merge the KI_1966S measure in the master database
				use "data/output/cps_master_individual_level.dta", clear	
				cap drop _merge
				merge m:1 state_group using `KI_1966S'		
				tab _merge
				drop _merge
				merge m:1  year  state_group using `KI_yS'
				drop _merge
				
	*2.MEASURES OF FRACTION OF AFFECTED WORKERS: 1966 fraction of affected workers, as defined in Bailey et al. (2018)
		cap drop F_s1966 
		gen 	F_s1966 = . 
		replace F_s1966 =0.091	if state_group == 1
		replace F_s1966 =0.117	if state_group == 2
		replace F_s1966 =0.223	if state_group == 3
		replace F_s1966 =0.291	if state_group == 4
		replace F_s1966 =0.094	if state_group == 5
		replace F_s1966 =0.13	if state_group == 6
		replace F_s1966 =0.083	if state_group == 7
		replace F_s1966 =0.107	if state_group == 8
		replace F_s1966 =0.098	if state_group == 9
		replace F_s1966 =0.109	if state_group == 10
		replace F_s1966 =0.257	if state_group == 11
		replace F_s1966 =0.111	if state_group == 12
		replace F_s1966 =0.392	if state_group == 13
		replace F_s1966 =0.152	if state_group == 14
		replace F_s1966 =0.259	if state_group == 15
		replace F_s1966 =0.279	if state_group == 16
		replace F_s1966 =0.319	if state_group == 17
		replace F_s1966 =0.193	if state_group == 18
		replace F_s1966 =0.09	if state_group == 19
		replace F_s1966 =0.176	if state_group == 20
		replace F_s1966 =0.166	if state_group == 21
		
saveold "data/output/cps_master_individual_level.dta", replace
*note: this database contains all CPS obs whether in universe or not, whether state is identified or not. this is the most general CPS database we can have, and on which we conduct 
	*descriptive statistics and analysis (with varying selection criteria)

	
