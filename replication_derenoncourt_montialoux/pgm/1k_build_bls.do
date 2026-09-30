*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*program: 		Create dataset including observations and counterfactuals by industry*region  
*first created: 09/22/2019
*last updated:  08/28/2020		
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	

clear all
set more off
set matsize 10000
set maxvar 10000

use "data/raw/bls_industry_reports.dta", clear
	
	*(a) database on laundries and nursing homes, observed distributions	
	keep if area=="all" & gender=="mf" & occupation=="all"
	keep avg_hourly_wages thr bracketavg occupation occ_label region  nursing_1965* nursing_1967* laundries_1966* laundries_1967*
	
	*replace missing obs at top and bottom of distributions with zero & create nursing 1966 assuming growth one half the aggregate emp growth between 1965 and 1967
	foreach stat in "tot" {
		replace nursing_1965_`stat' = 0 if  nursing_1965_`stat' ==. 
		replace nursing_1967_`stat' = 0 if  nursing_1967_`stat' ==. 
	}
	g nursing_1966_tot = nursing_1965_tot * (1 + (((nursing_1967_nb-nursing_1965_nb)/2)/nursing_1965_nb))
	g nursing_1966_nb = nursing_1965_nb * (1 + (((nursing_1967_nb-nursing_1965_nb)/2)/nursing_1965_nb))
	foreach stat in "tot" {
		replace laundries_1966_`stat' = 0 if  laundries_1966_`stat' ==. 
		replace laundries_1967_`stat' = 0 if  laundries_1967_`stat' ==. 
	}
	drop nursing_1965*
	tempfile data_nursing_laundries
	save `data_nursing_laundries'
	
	*(b) database on hotels and restaurans for non-tipped workers, observed distributions		
	use "data/raw/bls_industry_reports.dta", clear			
	keep if area=="all" & gender=="mf" & occupation=="ntd"
	keep avg_hourly_wages thr bracketavg region  hotels_1966* hotels_1967* restaurants_1966* restaurants_1967*
	
	*(c) database all industries, observed distributions	
	*merge the two databases
	merge 1:1  avg_hourly_wages thr bracketavg region using `data_nursing_laundries'	
	keep if inlist(_m,2,3)
	drop _m
	sort region thr
	foreach stat in "tot" {
		replace hotels_1966_`stat' = 0 if  hotels_1966_`stat' ==. 
		replace hotels_1967_`stat' = 0 if  hotels_1967_`stat' ==. 
		replace restaurants_1966_`stat' = 0 if  restaurants_1966_`stat' ==. 
		replace restaurants_1967_`stat' = 0 if  restaurants_1967_`stat' ==. 				
	}
	drop bracketavg occupation occ_label
	order 	avg_hourly_wages thr region   ///
	laundries_1966_tot laundries_1966_nb laundries_1966_avg laundries_1967_tot laundries_1967_nb laundries_1967_avg ///
	hotels_1966_tot hotels_1966_nb hotels_1966_avg hotels_1967_tot hotels_1967_nb hotels_1967_avg ///
	restaurants_1966_tot restaurants_1966_nb restaurants_1966_avg restaurants_1967_tot restaurants_1967_nb restaurants_1967_avg ///
	nursing_1966_tot nursing_1966_nb /*nursing_1966_avg*/ nursing_1967_tot nursing_1967_nb nursing_1967_avg ///
	
	foreach industry in "laundries" "hotels"  "restaurants" "nursing"  {
		foreach region in "s" "nc" "ne"	"w" "us" {
			foreach year in "1966" "1967" {
				su `industry'_`year'_nb if region=="`region'"
				local `industry'_`region'_`year'_nb `r(mean)'		
				replace `industry'_`year'_nb = ``industry'_`region'_`year'_nb' if region=="`region'" 
			}
		}
	}
	
	foreach industry in "laundries" "hotels"  "restaurants" {
		foreach region in "s" "nc" "ne"	"w" "us" {
			foreach year in "1966" "1967" {
				su `industry'_`year'_avg if region=="`region'"
				local `industry'_`region'_`year'_avg `r(mean)'		
				replace `industry'_`year'_avg = ``industry'_`region'_`year'_avg' if region=="`region'" 
			}
		}
	}
	
	foreach region in "s" "nc" "ne"	"w" "us" {
		su nursing_1967_avg if region=="`region'"
		local nursing_1967_avg `r(mean)'	
		replace nursing_1967_avg = `nursing_1967_avg' if region=="`region'" 
	}
	
	*replace missing values in nursing_1966_tot by 0
	replace nursing_1966_tot = 0 if nursing_1966_tot == .
	replace nursing_1966_nb = round(nursing_1966_nb,1)
	
	*compute total all industries
	foreach stat in "tot"  {
		foreach year in "1966" "1967" {
			gen all_`year'_`stat' = laundries_`year'_`stat' + nursing_`year'_`stat' + hotels_`year'_`stat' + restaurants_`year'_`stat'  			
		}
	}
	
	bysort region: egen all_1966_nb =sum(all_1966_tot) 
	bysort region: egen all_1967_nb =sum(all_1967_tot) 
	
	replace all_1966_nb = round(all_1966_nb,1)
	sort region thr
	
	tempfile observed_database
	save `observed_database'
	
	*(d) create counterfactual distributions database for laundries, hot, rest, nursing homes and all idustries
	*(i) laundries, hotels and restaurants 1967 counterfactual calculated using 1966 observed distributions
	
	foreach industry in "laundries" "nursing" "hotels" "restaurants"  {
		foreach region  in  "s" "us" "nc" "ne" "w" {
	
			local ind_lab = "Laundries"
			if "`industry'" == "all"{
				local ind_lab = "All"
			}
			if "`industry'" == "hotels"{
				local ind_lab = "Hotels"
			}
			if "`industry'" == "restaurants"{
				local ind_lab = "Restaurants"
			}
			if "`industry'" == "nursing"{
				local ind_lab = "Nursing"
			}
			local reg_lab = "South"
			if "`region'"=="us"{
				local reg_lab = "US"
			}
			if "`region'"=="nc"{
				local reg_lab = "NC"
			}
			if "`region'"=="ne"{
				local reg_lab = "NE"
			}
			if "`region'"=="w"{
				local reg_lab = "W"
			}
			local year="1966"
			if "`industry'"=="nursing"{
				local year="1965"
			}
			
			use "data/raw/bls_industry_reports.dta", clear	
			local occtype "all"
			if "`industry'"=="hotels" | "`industry'"=="restaurants"{
				local occtype "ntd"
			}
	
			keep if (occupation=="`occtype'")  ///
			&	region=="`region'" & 	area=="all" &	gender=="mf" 
			
			keep avg_hourly_wages thr `industry'_*_tot `industry'_*_nb region
	
			* cap mkdir "tables/bls_calculations/temp/Input"
	
			* save "tables/bls_calculations/temp/Input/Input_`industry'_`region'_`year'_wage_bins.dta", replace
	
			* This loop will automatically use every single file in the "Input" folder and
			* save a separate output file for each, to be found in the "Output" folder under
			* the same name with the prefix "_Output".
	
			* use "/tables/bls_calculations/temp/Input/Input_`industry'_`region'_`year'_wage_bins.dta", clear
	
			set more off
	
			********************************************************************************
			* 1. Input --> Local variables
			********************************************************************************
			* Generate total number of obs to be simulated 
			egen pop_temp = sum(`industry'_`year'_tot)
			local pop = pop_temp
			drop pop_temp
	
			* ACTION REQUIRED: To simulate a fixed total number of individuals, 
			* (1) Comment Alternative 1
			* (2) Uncomment Alternative 2
			* (3) Change the right hand side of Alternative 2 (e.g., to simulate 
			* 1,000,000 individuals given the relative empirical distribution: 
			* local sim_pop = 1000000)
			*
	
			local factor=1
			if "`industry'"=="nursing"{
				local factor= 1 + (((`industry'_1967_nb-`industry'_`year'_nb)/2)/`industry'_`year'_nb)
			}
	
			di `factor'
	
			* Alternative 1: Simulated sample size = Empirical sample size
			local sim_pop = `pop'*`factor'
			di `sim_pop'
	
			* Alternative 2: Simulated sample size = Arbitrary integer
			*local sim_pop = 10000
	
			local sim_pop_factor = `sim_pop'/`pop'
			di `sim_pop_factor'
	
			* Count the number of wage bins
			egen bins_temp = count(thr)
			local bins = bins_temp
			drop bins_temp
	
			* Store the minimum and maximum wage of each bin as well as its population (and
			* cumulative population by wage bins for facilitate randomization later)
			local pop_cumul_temp = 0
			forvalues i = 1/`bins' {
				local bin`i'_min = thr[`i']
				local bin`i'_max = thr[`i'+1]
				local pop`i' = round(`industry'_`year'_tot[`i']*`sim_pop_factor')
				local pop_cumul_temp = `pop_cumul_temp'+`pop`i''
				local pop_cumul_temp`i' = `pop_cumul_temp'
			}
			* Minimum wage of the final wage bin
			local bin`bins'_min = thr[`bins']
	
			********************************************************************************
			* 2. Local variables --> Simulation/Output
			********************************************************************************
			* Generate individual level data
			clear
	
			local num_obs = round(`pop'*`sim_pop_factor')
			set obs `num_obs'
			gen ID = _n
	
			gen wage_min_temp = .
			gen wage_max_temp = .
			local j = 0
			* For each bin...
			forvalues k = 1/`bins' {
				local m = `k'-1
				* ...assign a maximum and minimum wage to each individual from which we will
				* draw a random number (uniformly between them)
				replace wage_min_temp = `bin`k'_min' if ID <= `pop_cumul_temp`k'' & ID > `pop_cumul_temp`k''-`pop`k''
				replace wage_max_temp = `bin`k'_max' if ID <= `pop_cumul_temp`k'' & ID > `pop_cumul_temp`k''-`pop`k''
			}
	
			gen wage = runiform()*(wage_max_temp-wage_min_temp)+wage_min_temp
	
			* For the last bin, we try two alternatives.
			* (1) Wages are distributed according to the absolute value of a
			*     zero-mean Normal with standard deviation 2 + the cut-off value for
			*     that bucket
			* replace wage = abs(rnormal(0,2))+wage_min_temp if wage == . & wage_max_temp == .
			* (2) Wages are distributed according to a uniform distribution between the
			*     cut-off value for that bucket and 1.5x that value.
			replace wage = runiform()*(wage_min_temp*1.5-wage_min_temp)+wage_min_temp if wage == . & wage_max_temp == .
	
			keep ID wage
	
			* cap mkdir "tables/bls_calculations/temp/Output"
	
			* save "tables/bls_calculations/temp/Output/Output_`industry'_`region'_`year'_wage_bins.dta", replace
	
			********************************************************************************
			* 3. Re-collapsing (Porpose: Check Code for Correctness)
			********************************************************************************
	
			local wage_adj = 1.04381145236
	
			if "`industry'"=="nursing"{
				local wage_adj = 1.1239592455
			}
	
			g wage_cf = wage * `wage_adj'
	
			local bins_list 0
	
			forvalues j = 1/`bins' {
				local bins_list `bins_list' `bin`j'_min'
			}
			local bins_list `bins_list' 1000
	
			egen bin = cut(wage_cf), at(`bins_list')
			* The output gives a count of observations per bin
			tab bin
	
			* Descriptive figure of laundries in the South: simulated observed and counterfactual wage distribution:
			if "`industry'"=="laundries" & "`region'"=="s"{
			twoway (hist wage,  bin(41) bcolor(myred%30) ) (hist wage_cf, bin(41) bcolor(myarticblue%30)  ), ///
			graphregion(color(white)) yla(,nogrid) ///
			legend(order(1 "1966" 2 "1967 CF")   position(2) ring(0))
			graph export "figures/`industry'_`region'_1966_1967cf_wage_distributions.png", replace
			}
			*title("`ind_lab' (`reg_lab')")
	
			local yearplus=`year'+1
			if "`industry'"=="nursing"{
				local yearplus=`year'+2
			}
			g `industry'_`yearplus'_cf_tot =1
			g thr=round(bin, .01)
			collapse (sum) `industry'_`yearplus'_cf_tot , by(thr)
			drop if thr==.
			g bin = string(thr)
			destring bin, replace
			drop thr
			rename bin thr
			gen region = "`region'"
			tempfile `industry'_`region'_`year'_wage_bins
			save ``industry'_`region'_`year'_wage_bins'
		
		}
	}
	
	foreach industry in "laundries" "hotels" "restaurants" {
	
	use ``industry'_nc_1966_wage_bins'
	append using ``industry'_ne_1966_wage_bins'
	append using ``industry'_s_1966_wage_bins'
	append using ``industry'_us_1966_wage_bins'
	append using ``industry'_w_1966_wage_bins'
	
	tempfile `industry'_1966_wage_bins
	save ``industry'_1966_wage_bins'
	*saveold "data/output/output_collapsed_`industry'_1966_wage_bins.dta", replace
	}
	
	use `laundries_1966_wage_bins'
	merge 1:1 thr region using `hotels_1966_wage_bins', nogen
	merge 1:1 thr region using `restaurants_1966_wage_bins', nogen
	
	sort region thr
	order thr region laundries* hotels* restaurants*
	
	tempfile counterfactual_1966_industries
	save 	 `counterfactual_1966_industries'
	
	*(ii) nursing homes 1967 counterfactual calculated using 1965 observed distributions
	
	use `nursing_nc_1965_wage_bins'
	append using `nursing_ne_1965_wage_bins'
	append using `nursing_s_1965_wage_bins'
	append using `nursing_us_1965_wage_bins'
	append using `nursing_w_1965_wage_bins'
	
	merge 1:1 thr region using `counterfactual_1966_industries', nogen
	
	sort region thr
	order thr region laundries* hotels* restaurants* *nursing*
	
	foreach industry in "laundries" "hotels"  "restaurants" "nursing" {
	foreach region in "s" "nc" "ne"	"w" "us" {
	replace `industry'_1967_cf_tot = 0 if `industry'_1967_cf_tot ==.  
	} 
	}
	
	gen all_1967_cf_tot = laundries_1967_cf_tot + hotels_1967_cf_tot + restaurants_1967_cf_tot + nursing_1967_cf_tot 			
	
	*create total employment counts variables for each industry in counterfactual distributions
	foreach industry in "laundries" "hotels"  "restaurants" "nursing" "all" {
	bysort region: egen `industry'_1967_cf_nb =sum(`industry'_1967_cf_tot)
	}	
	
	*(e) merge observed and counterfactual hourly wage distributions	
	merge 1:1 thr region using `observed_database', nogen
	
	foreach industry in "laundries" "hotels"  "restaurants" "nursing" "all" {
	replace `industry'_1967_cf_tot = 0 if `industry'_1967_cf_tot == . 
	}
	
	foreach industry in "laundries" "hotels"  "restaurants" "nursing" "all" {
	foreach yeartype in "1966" "1967_cf" "1967" {
	foreach region in "s" "nc" "ne"	 "w" "us" {
	su `industry'_`yeartype'_nb if region=="`region'", meanonly
	di `r(mean)'
	replace `industry'_`yeartype'_nb = `r(mean)' if `industry'_`yeartype'_nb ==. & region=="`region'"
	
	} 
	}
	}
	
	sort region thr	
	gen n=[_n]
	by region: gen obs = _n
	order avg_hourly_wages thr n obs region laundries_1966* laundries_1967* hotels_1966* hotels_1967* ///
	restaurants_1966* restaurants_1967* nursing_1966* nursing_1967* all_1966* all_1967*
	
	save "data/output/bls_industry_reports_bunching.dta", replace
