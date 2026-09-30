*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*program: 		Estimate employment effects using BLS data 
*first created: 01/19/2018
*last updated:  08/31/2020		
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	

clear all
set more off
set matsize 10000
set maxvar 10000

*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
* FIGURE 8b [PAPER]: Case study: laundries earnings distributions in the South (Distributions used in the bunching estimation)
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
use "data/output/bls_industry_reports_bunching.dta", clear

*FIGURES 8b: CASE STUDY: LAUNDRIES EARNINGS DISTRIBUTIONS IN THE SOUTH -- DISTRIBUTIONS USED IN THE BUNCHING ESTIMATION

foreach industry in "laundries" {
	foreach region  in  "s" {
	keep if region=="`region'"
		replace thr=.3 if thr<.35
		replace thr=.35 if thr >.3 & thr<.45
		replace thr=.45 if thr>.35 & thr<.55
		replace thr=.55 if thr>.45 & thr<.65
		replace thr=.65 if thr>.55 & thr<.75
		replace thr=.75 if thr>.65 & thr<.85
		replace thr=.85 if thr>.75 & thr<.95
		replace thr=.95 if thr>.85 & thr<1
		replace thr=1 if thr>.95 & thr<1.15
		replace thr=1.15 if thr>1 & thr<1.25
		replace thr=1.25 if thr>1.15 & thr<1.35
		replace thr=1.35 if thr>1.25 & thr<1.45
		replace thr=1.45 if thr>1.35 & thr<1.60
		replace thr=1.60 if thr>1.45 & thr<1.7
		replace thr=1.7 if thr>1.60 & thr<1.80
		replace thr=1.8 if thr>1.7 & thr<1.9
		replace thr=1.9 if thr>1.8 & thr<2
		replace thr=2 if thr>1.9 & thr<2.1
		replace thr=2.1 if thr>2 & thr<2.2
		replace thr=2.2 if thr>2.1 & thr<2.3
		replace thr=2.3 if thr>2.2 & thr<2.4
		replace thr=2.4 if thr>2.3 & thr<2.5

		* Collapse by thr
		collapse (sum) `industry'_1967_cf_tot `industry'_1966_tot `industry'_1967_tot , by(thr)

		* Generate a variable containing total 1966 employment. Will be used to normalize changes in employment below and at or above the MW.
		egen `industry'_1966_employment = sum(`industry'_1966_tot)

		* Re-normalize counterfactual and actual 1967 wage bins by 1966 employment
		replace `industry'_1967_cf_tot=`industry'_1967_cf_tot/`industry'_1966_employment
		replace `industry'_1967_tot=`industry'_1967_tot/`industry'_1966_employment

		* Calculate the running sum of the difference between the actual and the counterfactual distribution.
		* It is equivalent to the percentage difference in employment in each bin.
		g runningsum=`industry'_1967_tot-`industry'_1967_cf_tot

		* Plot the actual and counterfactual 1967 distributions
		* Plot options:
		local yscl "0(.05).15"
		if "`industry'"=="laundries"{
			local yscl "0(.10).40"
		}

		* Plot the actual and counterfactual 1967 distributions
		twoway 	(connected `industry'_1967_cf_tot thr, color(mydeepblue) msize(small) sort) ///
		(connected `industry'_1967_tot thr , lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort) ///
		(connected runningsum thr, color(gs13) msymbol(none) lstyle(dash) sort ), ///
		ytitle("Number of workers  relative to total 1966 employment") xtitle("Hourly wage bins ($)") ///			
		yscale(range(0(.1).5)) ylabel(0(.1).5) /// 
		xlabel(.15 "$0.15" .5 "$0.50" .75 "$0.75" .95 "$<1" 1 "$1" 1.15 "$1.15" 1.50 "$1.50" 1.60 "$1.60" 2.00 "$2.00" 2.50 "$2.50", alternate) ///			
		xline(1, lstyle(dash) lwidth(medium) lcolor(myarticblue)) ///
		legend(order(1 "1967 CF"  2 "1967 Obs" 3 "Difference: Obs - CF") ring(0) position(2) bmargin(small) color(gs1) c(1)  region(style(none))  ) graphregion(color(white))
		graph export "figures/laundries_cf_actual_s_1.png", replace
	}
}

*-------------------------------------------------------------------------------	
* FIGURE 9 & D13 [PAPER]: Missing and excess jobs in the BLS industry wage reports
*-------------------------------------------------------------------------------	

foreach thrmw in 1.1 1.15 {
	use "data/output/bls_industry_reports_bunching.dta", clear
	if `thrmw'==1.1{
		local thrmw_lab = "115" // corresponds to a threshold of 1.15 X MW
	}
	* Switch label for threshold above MW when using different threshold
	if `thrmw'==1.15{
		local thrmw_lab="120"
	}
	if `thrmw'==1{
		local thrmw_lab="105"
	}
	if `thrmw'==1.05{
		local thrmw_lab="110"
	}
	sort thr

	* Generate indicator for under minimum wage
	gen under_mw=(thr<1)

	* For excess mass count, only consider workers upto 1.15 X MW
	keep if thr<=`thrmw'

	* Collapse data and sum jobs by under MW vs. at and above
	* collapse (sum) `industry'_1967_tot `industry'_1967_cf_tot (mean) `industry'_1966_nb, by(under_mw)
	*@Lukas: First attempt
	collapse (sum) laundries_1967_tot laundries_1967_cf_tot hotels_1967_tot hotels_1967_cf_tot restaurants_1967_tot restaurants_1967_cf_tot nursing_1967_tot nursing_1967_cf_tot (mean) laundries_1966_nb hotels_1966_nb restaurants_1966_nb nursing_1966_nb , by(under_mw region)

	* Normalize missing and excess mass by pre-treatment (1966) total employment in that industry X region cell
	local listofvar ""
	foreach industry in "laundries" "hotels" "restaurants" "nursing"{
		g `industry'_mass=(`industry'_1967_tot-`industry'_1967_cf_tot)/`industry'_1966_nb
		replace `industry'_mass=-1*`industry'_mass if under_mw==1
		local listofvar `listofvar' `industry'_mass
	}

	keep region under_mw `listofvar'
	drop if region=="us"

	* Rename for long shape
	local i = 1
	foreach industry in "laundries" "hotels" "restaurants" "nursing"{
		rename `industry'_mass mass`i'
		local i=`i'+1
	}
	* Reshape long
	reshape long mass, i(region under_mw) j(industry_num)
	gen industry = ""
	local j = 1
	foreach industry in "laundries" "hotels" "restaurants" "nursing"{
		replace industry = "`industry'" if industry_num == `j'
		local j=`j'+1
	}
	drop industry_num
	g indreg=upper(substr(industry,1,1)) + " " + upper(region) 

	* Reshape the data wide
	reshape wide mass, i(industry region) j(under_mw)

	la var mass0 "45 degree line"
	la var mass1 "Missing Mass"

	qui: reg mass0 mass1 if industry=="nursing" | industry=="laundries"
	local coeff : di %4.3f _b[mass1]
	local coeff_se : di %4.3f _se[mass1]

	twoway (scatter mass0 mass1 if industry=="laundries", mlabel(indreg) msymbol(O) mlabpos(11) mlabcolor(mydeepblue) mcolor(mydeepblue)) ///
	(scatter mass0 mass1 if industry=="nursing", mlabel(indreg) msymbol(T) mlabpos(3) mlabcolor(myarticblue)  mcolor(myarticblue)) ///
	(scatter mass0 mass1 if industry=="hotels", mlabel(indreg) msymbol(D) mlabpos(7) mlabcolor(myred)  mcolor(myred)) ///
	(scatter mass0 mass1 if industry=="restaurants", mlabel(indreg) msymbol(T) mlabpos(3) mlabcolor(myarticblue)  mcolor(myarticblue)) ///
	(lfit mass0 mass1 , lcolor(gs7) ) ///
	(line mass0 mass0, lcolor(black) sort lpattern(dash)), graphregion(color(white)) yla(,nogrid) ytitle("Excess jobs relative pre-treatment employment") ///
	xtitle("Missing jobs relative pre-treatment employment") caption("Slope = `coeff' (`coeff_se')", ring(0) pos(11)) ///
	legend(order(6 5) label(1 "45 degree line" 2 "Linear fit" 3 "45 degree line") position(4) ring(0))
	graph export "figures/missing_excess_mass_all_`thrmw_lab'_1.pdf", replace
}


*-------------------------------------------------------------------------------	
* FIGURE F2 & F3 [PAPER]: Missing and excess jobs in the BLS industry wage reports, excluding high leverage points (N S; L S; H S; and N NC)
*-------------------------------------------------------------------------------	

foreach thrmw in 1.1 1.15 {
	use "data/output/bls_industry_reports_bunching.dta", clear
	if `thrmw'==1.1{
		local thrmw_lab = "115" // corresponds to a threshold of 1.15 X MW
	}
	* Switch label for threshold above MW when using different threshold
	if `thrmw'==1.15{
		local thrmw_lab="120"
	}
	if `thrmw'==1{
		local thrmw_lab="105"
	}
	if `thrmw'==1.05{
		local thrmw_lab="110"
	}
	sort thr

	* Generate indicator for under minimum wage
	gen under_mw=(thr<1)

	* For excess mass count, only consider workers upto 1.15 X MW
	keep if thr<=`thrmw'

	* Collapse data and sum jobs by under MW vs. at and above
	* collapse (sum) `industry'_1967_tot `industry'_1967_cf_tot (mean) `industry'_1966_nb, by(under_mw)
	*@Lukas: First attempt
	collapse (sum) laundries_1967_tot laundries_1967_cf_tot hotels_1967_tot hotels_1967_cf_tot restaurants_1967_tot restaurants_1967_cf_tot nursing_1967_tot nursing_1967_cf_tot (mean) laundries_1966_nb hotels_1966_nb restaurants_1966_nb nursing_1966_nb , by(under_mw region)

	* Normalize missing and excess mass by pre-treatment (1966) total employment in that industry X region cell
	local listofvar ""
	foreach industry in "laundries" "hotels" "restaurants" "nursing"{
		g `industry'_mass=(`industry'_1967_tot-`industry'_1967_cf_tot)/`industry'_1966_nb
		replace `industry'_mass=-1*`industry'_mass if under_mw==1
		local listofvar `listofvar' `industry'_mass
	}

	keep region under_mw `listofvar'
	drop if region=="us"

	* Rename for long shape
	local i = 1
	foreach industry in "laundries" "hotels" "restaurants" "nursing"{
		rename `industry'_mass mass`i'
		local i=`i'+1
	}
	* Reshape long
	reshape long mass, i(region under_mw) j(industry_num)
	gen industry = ""
	local j = 1
	foreach industry in "laundries" "hotels" "restaurants" "nursing"{
		replace industry = "`industry'" if industry_num == `j'
		local j=`j'+1
	}
	drop industry_num
	g indreg=upper(substr(industry,1,1)) + " " + upper(region) 
	replace indreg=indreg+ " (.45)" if indreg=="N W" & "`thrmw_lab'"=="115"
	replace indreg=indreg+ " (-.41)" if indreg=="N NE" & "`thrmw_lab'"=="115"
	replace indreg=indreg+ " (.66)" if indreg=="N W" & "`thrmw_lab'"=="120"
	replace indreg=indreg+ " (-.48)" if indreg=="N NE" & "`thrmw_lab'"=="120"
	
	* Reshape the data wide
	reshape wide mass, i(industry region) j(under_mw)

	la var mass0 "45 degree line"
	la var mass1 "Missing Mass"
	
	drop if region=="s" & industry=="nursing"
	drop if region=="s" & industry=="hotels"
	drop if region=="nc" & industry=="nursing"
	drop if region=="s" & industry=="laundries"

	qui: reg mass0 mass1 if industry=="nursing" | industry=="laundries" | industry=="hotels" | industry=="restaurants"
	local coeff : di %4.3f _b[mass1]
	local coeff_se : di %4.3f _se[mass1]

	twoway (scatter mass0 mass1 if industry=="laundries", mlabel(indreg) msymbol(O) mlabpos(11) mlabcolor(mydeepblue) mcolor(mydeepblue)) ///
	(scatter mass0 mass1 if industry=="nursing", mlabel(indreg) msymbol(T) mlabpos(3) mlabcolor(myarticblue)  mcolor(myarticblue)) ///
	(scatter mass0 mass1 if industry=="hotels", mlabel(indreg) msymbol(D) mlabpos(7) mlabcolor(myred)  mcolor(myred)) ///
	(scatter mass0 mass1 if industry=="restaurants", mlabel(indreg) msymbol(T) mlabpos(3) mlabcolor(myarticblue)  mcolor(myarticblue)) ///
	(line mass0 mass0, lcolor(black) sort lpattern(dash)), graphregion(color(white)) yla(,nogrid) ytitle("Excess jobs relative pre-treatment employment") ///
	xtitle("Missing jobs relative pre-treatment employment") ///
	legend(order(5 ) label(1 "45 degree line" ) position(11) ring(0))
	graph export "figures/missing_excess_mass_all_`thrmw_lab'_1_excl_ns_ls_hs_nnc.pdf", replace
}


*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
*TABLE 7 [PAPER]: EMPLOYMENT ELASTICITIES USING BUNCHING DESIGN
*%-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------	
use "data/output/bls_industry_reports_bunching.dta", clear
		
	*PART B. Compute employment effects of the reform using bunching methodology
	*%-----EMPLOYMENT---------------------------------------------------------------------------------------------------------------------		
		   *Cumulative number of workers by bin, within region, by industry
		   foreach industry in "laundries" "hotels"  "restaurants" "nursing" "all" {
		   foreach yeartype in "1966" "1967_cf" "1967"{ 
				by region: gen `industry'_`yeartype'_rs_emp =  sum(`industry'_`yeartype'_tot)
			}
			}
		   
		   *Employment change (counts) due to the min. wage, by bin, by industry*region
			foreach industry in "laundries" "hotels"  "restaurants" "nursing" "all" {
				gen `industry'_emp_change = `industry'_1967_tot - `industry'_1967_cf_tot 
			}
		   
		   *Employment growth (%) due to min. wage, by bin, i.e. "runnning sum", by industry*region
			foreach industry in "laundries" "hotels"  "restaurants" "nursing" "all" {
				gen `industry'_rs = `industry'_emp_change / `industry'_1966_nb
		    }		   	
			
		   *Cumulative running sum, i.e. area under the curve of the graph of the difference 
				*or the total percentage difference in employment up to that bin. 
			foreach industry in "laundries" "hotels"  "restaurants" "nursing" "all" {
				by region: gen `industry'_cumrs = sum(`industry'_rs)
			}	
			
			*Share of workers earnings below the mw
			foreach industry in "laundries" "hotels"  "restaurants" "nursing" "all" {
			foreach yeartype in "1966" "1967_cf" "1967" { 
			foreach region in "s" "nc"  "ne" "w"  "us" {
				su n if thr == .95 & region=="`region'", meanonly 
				local `industry'_`region'_`yeartype'_shbmw=`industry'_`yeartype'_rs_emp[`r(min)']/`industry'_`yeartype'_nb[`r(min)']	
				mat   `industry'_`region'_1966_shbmw = round(``industry'_`region'_1966_shbmw',0.01) // col2 of table 7
			} 
			}
			}

			
			*Missing mass (%) and excess mass (%) by industry*region for different thresholds
			forval thrmw = 1(.05)1.25{ 
					if `thrmw'==1{
					local thrmw_lab="105" 	// corresponds to a threshold of 1.05 X MW
					}					
					if `thrmw'==1.05{
					local thrmw_lab="110" 	// corresponds to a threshold of 1.10 X MW
					}
					if `thrmw'==1.1{
					local thrmw_lab = "115" // corresponds to a threshold of 1.15 X MW
					}
					if `thrmw'==1.15{
					local thrmw_lab="120"   // corresponds to a threshold of 1.20 X MW
					}
					if `thrmw'==1.2{
					local thrmw_lab="125"   // corresponds to a threshold of 1.25 X MW
					}
					
			foreach industry in "laundries" "hotels"  "restaurants" "nursing" "all" {
			foreach region in "s" "nc"  "ne" "w"  "us"{
				*>>missing mass (%)
				su n if thr == .95 & region=="`region'", meanonly        // Picks out the threshold below the MW, i.e. stricly below $1
				su `industry'_cumrs if  n==`r(min)'
				local `industry'_`region'_`thrmw_lab'_dB `r(mean)'
				mat `industry'_`region'_`thrmw_lab'_dB = round(``industry'_`region'_`thrmw_lab'_dB',0.01)  
				
				*>>excess mass (%)
				su n if thr == `thrmw' & region=="`region'", meanonly 	
				su `industry'_cumrs if n==`r(min)'
				local `industry'_`region'_`thrmw_lab'_dA = `r(mean)'-``industry'_`region'_`thrmw_lab'_dB'	
				mat `industry'_`region'_`thrmw_lab'_dA = round(``industry'_`region'_`thrmw_lab'_dA',0.01)  
				
				*>>>missing + excess mass (%) = employment change around the min. wage (%)
				local `industry'_`region'_`thrmw_lab'_dE = (``industry'_`region'_`thrmw_lab'_dB'+ ``industry'_`region'_`thrmw_lab'_dA')
				mat `industry'_`region'_`thrmw_lab'_dE = round(``industry'_`region'_`thrmw_lab'_dE',0.01)  

				*>>>missing + excess mass (%) = employment change around the min. wage (%)
				local `industry'_`region'_`thrmw_lab'_dAE = (``industry'_`region'_`thrmw_lab'_dE'/``industry'_`region'_1966_shbmw')
				mat `industry'_`region'_`thrmw_lab'_dAE = round(``industry'_`region'_`thrmw_lab'_dAE',0.01)  
				
				*>>>total employment change (not just around the min. wage but for the entire distribution)
				su n if thr == 3 & region=="`region'", meanonly 
				local `industry'_`region'_dETOT = (`industry'_cumrs[`r(min)'])
				mat `industry'_`region'_dETOT = round(``industry'_`region'_dETOT',0.01)  		
			} 
			}
			}
				*di `laundries_nc_dETOT'
				*di `restaurants_nc_105_dA'
				*di `laundries_nc_125_dB'
				*di `laundries_nc_125_dAE'
				*di `laundries_nc_125_dE'				
				*di `laundries_s_1966_shbmw'
				*di `laundries_nc_1966_shbmw'
				
			
					
	*%-----WAGES--------------------------------------------------------------------------------------------------------------------------			
			*Wage bill, by bin, by industry*region
			*(i) Average wage by bin = midpoint of the bin	
			gen avg_w = .
				local max = _N	

				foreach region in "s" "nc"  "ne" "w"  "us"{
					su n if region=="`region'", meanonly
					local n_`region' = `r(max)'
				}
				forval i=1(1)`max'{
					replace avg_w = thr[`i'] + (thr[`=`i'+1'] - thr[`i'])/2 if _n==`i'
				}	
					*the last avg_w should be equal to 1.5 threshold (pareto assumption)
					replace avg_w = thr[`max'-1]*1.5 if inlist(_n,`n_s',`n_nc',`n_ne',`n_w',`n_us',`max')
			
			*(ii) Wage bills, by industry*region*year   
			foreach industry in "laundries" "hotels" "restaurants" "nursing" "all" {
			foreach yeartype in "1966" "1967_cf" "1967"{ 

				*>>>wage bill per bin, by industry*region
				gen `industry'_`yeartype'_wb = avg_w*`industry'_`yeartype'_tot
				
				*>>>cumulative wage bill, by industry*region
				by region: gen `industry'_`yeartype'_wbrs = sum(`industry'_`yeartype'_wb)

				*>>>total wage bill, by industry*region
				by region: egen `industry'_`yeartype'_tot_wb = total(`industry'_`yeartype'_wb)
				
				*>>>average wages for all workers
				gen `industry'_`yeartype'_aw = `industry'_`yeartype'_tot_wb/`industry'_`yeartype'_nb

				}
				}
			
			foreach industry in "laundries" "hotels" "restaurants" "nursing" "all" {
				*>>>1967 counterfactual-1967 change in affected wage bill
				gen `industry'_dWB = avg_w*(`industry'_1967_tot-`industry'_1967_cf_tot)
				
				*>>>running sum of the difference (i.e. cumulative sum of the difference)
				by region: gen `industry'_dWB_rs = sum(`industry'_dWB)			
				}
				
			*(iii) Average wages for affected workers and percent change in affected wage bill
			forval thrmw = 1(.05)1.25{ 
					if `thrmw'==1{
					local thrmw_lab="105" 	// corresponds to a threshold of 1.05 X MW
					}					
					if `thrmw'==1.05{
					local thrmw_lab="110" 	// corresponds to a threshold of 1.10 X MW
					}
					if `thrmw'==1.1{
					local thrmw_lab = "115" // corresponds to a threshold of 1.15 X MW
					}
					if `thrmw'==1.15{
					local thrmw_lab="120"   // corresponds to a threshold of 1.20 X MW
					}
					if `thrmw'==1.2{
					local thrmw_lab="125"   // corresponds to a threshold of 1.25 X MW
					}
					
				foreach industry in "laundries" "hotels"  "restaurants" "nursing" "all" {
				foreach yeartype in "1966" "1967_cf" "1967" { 
				foreach region in "s" "nc"  "ne" "w"  "us" {
				*>>>average wages for affected workers
					su obs if thr == `thrmw' & region=="`region'", meanonly 
					di `r(min)'
					su `industry'_`yeartype'_wbrs if region=="`region'" & obs==`r(min)'
					local `industry'_`yeartype'_`region'_`thrmw_lab'_wbrs  = `r(min)'
					di ``industry'_`yeartype'_`region'_`thrmw_lab'_wbrs' 
					
					su obs if thr == `thrmw' & region=="`region'", meanonly 
					su 	`industry'_`yeartype'_rs_emp if region=="`region'" & obs==`r(min)'
					di  `r(min)'
					local `industry'_`region'_`yeartype'_`thrmw_lab'_aaw = ``industry'_`yeartype'_`region'_`thrmw_lab'_wbrs'/`r(min)'
					di ``industry'_`region'_`yeartype'_`thrmw_lab'_aaw' 
				
				*>>>percent change in affected wage bill
					su n if thr == 0.95 & region=="`region'", meanonly     
					local `industry'_`region'_1966_tot_wb_bmw = `industry'_1966_wbrs[`r(min)']				
					di ``industry'_`region'_1966_tot_wb_bmw'
					su n if thr == `thrmw' & region=="`region'", meanonly 
					local `industry'_`region'_`thrmw_lab'_dWB = (`industry'_dWB_rs[`r(min)'])/``industry'_`region'_1966_tot_wb_bmw'	
					di ``industry'_`region'_`thrmw_lab'_dWB'
				
				*>>>percent change in average wage (% of 1967 counterfactual distribution)
					su n if thr == `thrmw' & region=="`region'", meanonly 
					*di `r(min)'
					local `industry'_`region'_`thrmw_lab'_dAW = (`industry'_1967_aw[`r(min)']-`industry'_1967_cf_aw[`r(min)'])/`industry'_1967_cf_aw[`r(min)']
				    mat `industry'_`region'_`thrmw_lab'_dAW = round(``industry'_`region'_`thrmw_lab'_dAW',0.01)  
					
				*>>>percent change in affected average wage (% of 1967 counterfactual distribution)
					local `industry'_`region'_`thrmw_lab'_dAAW = (``industry'_`region'_`thrmw_lab'_dWB' - ((``industry'_`region'_`thrmw_lab'_dE')/``industry'_`region'_1966_shbmw'))/(1+((``industry'_`region'_`thrmw_lab'_dE')/``industry'_`region'_1966_shbmw'))
					mat `industry'_`region'_`thrmw_lab'_dAAW = round(``industry'_`region'_`thrmw_lab'_dAAW',0.01)  
					
				*>>>percent change in wage for all workers in the distribution /// does not depend on thrmw
					su n if thr == 3 & region=="`region'", meanonly 
					local `industry'_`region'_dAWTOT= (`industry'_1967_aw[`r(min)']-`industry'_1967_cf_aw[`r(min)'])/`industry'_1967_cf_aw[`r(min)']
				    mat `industry'_`region'_dAWTOT = round(``industry'_`region'_dAWTOT',0.01)  

				}
				}
				}
				}		
				
					*di `laundries_s_dAWTOT'
					*di `laundries_nc_105_dAW'
					*di `restaurants_s_1966_tot_wb_bmw'
					*di `restaurants_s_125_dWB'
					*di `laundries_s_125_dAAW'		

	*%-----EMPLOYMENT ELASTICITIES--------------------------------------------------------------------------------------------------------			
				forval thrmw = 1(.05)1.25{ 
					if `thrmw'==1{
					local thrmw_lab="105" 	// corresponds to a threshold of 1.05 X MW
					}					
					if `thrmw'==1.05{
					local thrmw_lab="110" 	// corresponds to a threshold of 1.10 X MW
					}
					if `thrmw'==1.1{
					local thrmw_lab = "115" // corresponds to a threshold of 1.15 X MW
					}
					if `thrmw'==1.15{
					local thrmw_lab="120"   // corresponds to a threshold of 1.20 X MW
					}
					if `thrmw'==1.2{
					local thrmw_lab="125"   // corresponds to a threshold of 1.25 X MW
					}
					
				foreach industry in "laundries" "hotels"  "restaurants" "nursing" "all" {
				foreach region in "s" "nc"  "ne" "w"  "us" {				
				*>>>total employment elasticity
					local `industry'_`region'_`thrmw_lab'_EE = ``industry'_`region'_`thrmw_lab'_dE'/``industry'_`region'_dAWTOT'
					mat `industry'_`region'_105_EE = round(``industry'_`region'_105_EE',0.01)  
					mat `industry'_`region'_110_EE = round(``industry'_`region'_110_EE',0.01) 
					mat `industry'_`region'_115_EE = round(``industry'_`region'_115_EE',0.01)  // col 4 table 7
					mat `industry'_`region'_120_EE = round(``industry'_`region'_120_EE',0.01)  // col 5 table 7
					mat `industry'_`region'_125_EE = round(``industry'_`region'_125_EE',0.01)
				

				}
				}
				}
				
				*di `laundries_s_120_EE'
				*di `laundries_s_120_AEE'
			
	*PART C. Descriptive statistics on BLS employment data			
				foreach industry in "laundries" "nursing" "restaurants" "hotels" "all" {
				foreach region in "s" "nc" "ne"	"w" "us" {
				  *>>>total employment by sector*region in 1966, source BLS
					su `industry'_1966_nb if region=="`region'"
					local `industry'_`region'_1966_nb `r(mean)'
					mat `industry'_`region'_1966_nb = round(``industry'_`region'_1966_nb',1)
				}
				}	
			
	**CREATE COLUMNS OF THE TABLE
	*Column (1) Total employment by sector*region in 1966, source BLS
			mat col1_laundries = (laundries_s_1966_nb\laundries_nc_1966_nb\laundries_ne_1966_nb\laundries_w_1966_nb)
			mat rownames col1_laundries="\hspace{3mm}{South}" "\hspace{3mm}{Midwest}" "\hspace{3mm}{Northeast}" "\hspace{3mm}{West}"

			mat col1_hotels = hotels_s_1966_nb\hotels_nc_1966_nb\hotels_ne_1966_nb\hotels_w_1966_nb
			mat rownames col1_hotels="\hspace{3mm}{South}" "\hspace{3mm}{Midwest}" "\hspace{3mm}{Northeast}" "\hspace{3mm}{West}"
			
			mat col1_restaurants = restaurants_s_1966_nb\restaurants_nc_1966_nb\restaurants_ne_1966_nb\restaurants_w_1966_nb
			mat rownames col1_restaurants="\hspace{3mm}{South}" "\hspace{3mm}{Midwest}" "\hspace{3mm}{Northeast}" "\hspace{3mm}{West}"
			
			mat col1_nursing = nursing_s_1966_nb\nursing_nc_1966_nb\nursing_ne_1966_nb\nursing_w_1966_nb
			mat rownames col1_nursing="\hspace{3mm}{South}" "\hspace{3mm}{Midwest}" "\hspace{3mm}{Northeast}" "\hspace{3mm}{West}"
			
			mat col1_all = all_us_1966_nb
			mat rownames col1_all="\hspace{3mm}{U.S.}"
		
			mat col1 = col1_laundries\col1_hotels\col1_restaurants\col1_nursing\col1_all
			
	 *Column (2) Fraction of affected workers (i.e. earning less than $1 in 1966), source BLS
			mat col2_laundries = 	(laundries_s_1966_shbmw\laundries_nc_1966_shbmw\laundries_ne_1966_shbmw\laundries_w_1966_shbmw)
			mat col2_hotels = 		(hotels_s_1966_shbmw\hotels_nc_1966_shbmw\hotels_ne_1966_shbmw\hotels_w_1966_shbmw)
			mat col2_restaurants = 	(restaurants_s_1966_shbmw\restaurants_nc_1966_shbmw\restaurants_ne_1966_shbmw\restaurants_w_1966_shbmw)
			mat col2_nursing = 		(nursing_s_1966_shbmw\nursing_nc_1966_shbmw\nursing_ne_1966_shbmw\nursing_w_1966_shbmw)
			mat col2_all = 			(all_us_1966_shbmw)
			mat col2 = col2_laundries\col2_hotels\col2_restaurants\col2_nursing\col2_all
			
	 *Column (4) Employment elasticity (Total) 1.15*MW
			mat col4_laundries = 	(laundries_s_115_EE\laundries_nc_115_EE\laundries_ne_115_EE\laundries_w_115_EE)
			mat col4_hotels = 		(hotels_s_115_EE\hotels_nc_115_EE\hotels_ne_115_EE\hotels_w_115_EE)
			mat col4_restaurants = 	(restaurants_s_115_EE\restaurants_nc_115_EE\restaurants_ne_115_EE\restaurants_w_115_EE)
			mat col4_nursing = 		(nursing_s_115_EE\nursing_nc_115_EE\nursing_ne_115_EE\nursing_w_115_EE)
			mat col4_all = 			(all_us_115_EE)
			mat col4 = col4_laundries\col4_hotels\col4_restaurants\col4_nursing\col4_all
				
	 *Column (5) Employment elasticity (Total) 1.20*MW
			mat col5_laundries = 	(laundries_s_120_EE\laundries_nc_120_EE\laundries_ne_120_EE\laundries_w_120_EE)
			mat col5_hotels = 		(hotels_s_120_EE\hotels_nc_120_EE\hotels_ne_120_EE\hotels_w_120_EE)
			mat col5_restaurants = 	(restaurants_s_120_EE\restaurants_nc_120_EE\restaurants_ne_120_EE\restaurants_w_120_EE)
			mat col5_nursing = 		(nursing_s_120_EE\nursing_nc_120_EE\nursing_ne_120_EE\nursing_w_120_EE)
			mat col5_all = 			(all_us_120_EE)
			mat col5 = col5_laundries\col5_hotels\col5_restaurants\col5_nursing\col5_all
						
								
						
	*Column (3)	Share of black workers, source March CPS 1968
		**By region
			local varlist "year year_cps state_group ind1950 industry occupation region sex race age flag_employed in_sample weight annual_wage hourly_wage weekly_wage"
			use `varlist' using "data/output/cps_master_individual_level.dta", clear	
			keep if year==1967 & inlist(ind1950,846,836,679,868)
			
			*laudries = (ind1950==846, Laundering, cleaning, and dyeing services)
			*hotels   = (ind1950==836, Hotels and lodging places)
			*restaurants = (ind1950==679, Eating and drinking places)
			*nursing homes = (ind1950==868, Medical and other health services, except hospitals)
			gen black  = (race==200)
			
			gen four_regions = . 
			replace four_regions = 1 if inlist(region,31,32,33)
			replace four_regions = 2 if inlist(region,21,22)
			replace four_regions = 3 if inlist(region,11,12)
			replace four_regions = 4 if inlist(region,41,42)
		
			*region = 1 south
			*region = 2 midwest
			*region = 3 northeast	
			*region = 4 west

			collapse (mean) black_share = black ///
					  if flag_employed & in_sample & inrange(age,21,64) & inlist(race,100,200) [w=weight], by(ind1950 four_regions)		
			replace black_share = round(black_share,0.01)
			mkmat black_share if ind1950==846, matrix(col3_laundries)
			mkmat black_share if ind1950==836, matrix(col3_hotels)
			mkmat black_share if ind1950==679, matrix(col3_restaurants)
			mkmat black_share if ind1950==868, matrix(col3_nursing)
			
			
		**For all industries, by region	
			local varlist "year year_cps state_group ind1950 industry occupation region sex race age flag_employed in_sample weight annual_wage hourly_wage weekly_wage"
			use `varlist' using "data/output/cps_master_individual_level.dta", clear	
			keep if year==1967 & inlist(ind1950,846,836,679,868)
			gen black  = (race==200)
			gen four_regions = . 
			replace four_regions = 1 if inlist(region,31,32,33)
			replace four_regions = 2 if inlist(region,21,22)
			replace four_regions = 3 if inlist(region,11,12)
			replace four_regions = 4 if inlist(region,41,42)
			collapse (mean) black_share = black ///
					  if flag_employed & in_sample & inrange(age,21,64) & inlist(race,100,200) [w=weight], by(four_regions)		
			replace black_share = round(black_share,0.01)
			mkmat black_share, matrix(col3_all_ind)

		**For all industries, all U.S.
			local varlist "year year_cps state_group ind1950 industry occupation region sex race age flag_employed in_sample weight annual_wage hourly_wage weekly_wage"
			use `varlist' using "data/output/cps_master_individual_level.dta", clear	
			keep if year==1967 & inlist(ind1950,846,836,679,868)
			gen black  = (race==200)
			collapse (mean) black_share = black ///
					  if flag_employed & in_sample & inrange(age,21,64) & inlist(race,100,200) & state_group!=22 [w=weight]
			replace black_share = round(black_share,0.01)
			mkmat black_share, matrix(col3_all_us)
			
			mat col3_all=col3_all_us
			mat col3 = col3_laundries\col3_hotels\col3_restaurants\col3_nursing\col3_all_us			   
				   
	 *create matrix with all statistics 	
			matrix laundries = (col1_laundries,col2_laundries,col3_laundries,col4_laundries,col5_laundries)
			matrix hotels = (col1_hotels,col2_hotels,col3_hotels,col4_hotels,col5_hotels)
			matrix restaurants = (col1_restaurants,col2_restaurants,col3_restaurants,col4_restaurants,col5_restaurants)
			matrix nursing = (col1_nursing,col2_nursing,col3_nursing,col4_nursing,col5_nursing)
			matrix all = (col1_all,col2_all,col3_all,col4_all,col5_all)
			
			matrix table_bunching = (laundries\hotels\restaurants\nursing\all) 	
			mat li table_bunching									
			
			esttab matrix(laundries,fmt(%12.0gc %3.2f %3.2f %3.2f %3.2f )) ///
				   using "tables/table_bunching.tex" , replace  label fragment ///
				   refcat(\hspace{3mm}{South} "\rule{0pt}{3ex}\emph{Laundries}", nolabel) /// 
				   nolines booktabs nonumbers nomtitles collabels(none)	
			esttab matrix(hotels,fmt(%12.0gc %3.2f %3.2f %3.2f %3.2f )) ///
				   using "tables/table_bunching.tex", append  label fragment ///
				   refcat(\hspace{3mm}{South} "\rule{0pt}{3ex}\emph{Hotels}", nolabel)  ///
				   nolines booktabs nonumbers nomtitles collabels(none)			   
			esttab matrix(restaurants,fmt(%12.0gc %3.2f %3.2f %3.2f %3.2f )) ///
				   using "tables/table_bunching.tex", append  label fragment ///
				   refcat(\hspace{3mm}{South} "\rule{0pt}{3ex}\emph{Restaurants}", nolabel)  ///
				   nolines booktabs nonumbers nomtitles collabels(none)			   
			esttab matrix(nursing,fmt(%12.0gc %3.2f %3.2f %3.2f %3.2f )) ///
				   using "tables/table_bunching.tex", append  label fragment ///
				   refcat(\hspace{3mm}{South} "\rule{0pt}{3ex}\emph{Nursing Homes}", nolabel)  ///
				   nolines booktabs nonumbers nomtitles collabels(none)			   
			esttab matrix(all,fmt(%12.0gc %3.2f %3.2f %3.2f %3.2f )) ///
				   using "tables/table_bunching.tex", append  label fragment ///
				   refcat(\hspace{3mm}{U.S.} "\rule{0pt}{3ex}\emph{All industries}", nolabel)  ///
				   nolines booktabs nonumbers nomtitles collabels(none)			   								   
				  
