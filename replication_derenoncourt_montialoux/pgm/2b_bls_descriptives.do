*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*program: This do file plots bunching graphs 
*first created: 07/17/2018
*last updated:  08/30/2020
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
clear all
set more off
set matsize 10000
set maxvar 10000

use "data/raw/bls_industry_reports.dta", clear	

*[PRELIMINARY]: Convert numbers of workers in nb of workers (1000s) for bunching graphs
	foreach y in "1963" "1966"  {
		replace hotels_`y'_tot 		= hotels_`y'_tot/1000
		replace restaurants_`y'_tot = restaurants_`y'_tot/1000 
		replace laundries_`y'_tot 	= laundries_`y'_tot/1000
		replace all_`y'_tot 		= all_`y'_tot/1000
		replace hot_rest_`y'_tot 	= hot_rest_`y'_tot/1000

		replace hotels_`y'_nb 		= hotels_`y'_nb/1000
		replace restaurants_`y'_nb 	= restaurants_`y'_nb/1000 
		replace laundries_`y'_nb 	= laundries_`y'_nb/1000
		replace all_`y'_nb  		= all_`y'_nb/1000
		replace hot_rest_`y'_nb  	= hot_rest_`y'_nb/1000
	
	}
	
	
	foreach y in "1965"   {
		*number of workers in 1000s
		replace nursing_`y'_tot 	= nursing_`y'_tot/1000
		replace nursing_`y'_nb 		= nursing_`y'_nb/1000
		replace all_`y'_tot 		= all_`y'_tot/1000
		replace all_`y'_nb  		= all_`y'_nb/1000
		
	}
	
	foreach y in "1967"  {
		*number of workers in 1000s
		replace hotels_`y'_tot 		= hotels_`y'_tot/1000
		replace restaurants_`y'_tot = restaurants_`y'_tot/1000 
		replace laundries_`y'_tot 	= laundries_`y'_tot/1000
		replace nursing_`y'_tot 	= nursing_`y'_tot/1000
		replace all_`y'_tot 		= all_`y'_tot/1000
		replace hot_rest_`y'_tot 	= hot_rest_`y'_tot/1000

		replace hotels_`y'_nb 		= hotels_`y'_nb/1000
		replace restaurants_`y'_nb 	= restaurants_`y'_nb/1000 
		replace laundries_`y'_nb 	= laundries_`y'_nb/1000
		replace nursing_`y'_nb 		= nursing_`y'_nb/1000
		replace all_`y'_nb  		= all_`y'_nb/1000
		replace hot_rest_`y'_nb  	= hot_rest_`y'_nb/1000
	
	}
	
	foreach y in "1968"  {
		*number of workers in 1000s
		replace laundries_`y'_tot 	= laundries_`y'_tot/1000
		replace nursing_`y'_tot 	= nursing_`y'_tot/1000
		replace all_`y'_tot 		= all_`y'_tot/1000

		replace laundries_`y'_nb 	= laundries_`y'_nb/1000
		replace nursing_`y'_nb 		= nursing_`y'_nb/1000
		replace all_`y'_nb  		= all_`y'_nb/1000
	
	}
	
*PART A.[PAPER] [BUNCHING GRAPHS: OBSERVED DISTRIBUTIONS] 
*FIGURES D2 & D3: HOURLY WAGE DISTRIBUTION IN LAUNDRIES (ALL AND INSIDE PLANT WORKERS ONLY)
	*------LAUNDRIES------------------------------------------------------------------------------
foreach a in "all"  {
	foreach r in "us" "ne" "nc" "s" "w" {
	foreach o in "all" "ipt"  {
	foreach g in "mf"  {
	
	twoway 	(connected laundries_1963_tot thr if region=="`r'" & occupation=="`o'" & area=="`a'" & gender=="`g'", color(mydeepblue) msize(small) sort) ///
			(connected laundries_1966_tot thr if region=="`r'" & occupation=="`o'" & area=="`a'" & gender=="`g'", lcolor(mydeepblue) mcolor(white) mlcolor(mydeepblue) msize(small) sort) ///
			(connected laundries_1967_tot thr if region=="`r'" & occupation=="`o'" & area=="`a'" & gender=="`g'", lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort) ///
			(connected laundries_1968_tot thr if region=="`r'" & occupation=="`o'" & area=="`a'" & gender=="`g'", color(myred) msize(small) sort), ///
			ytitle("Number of workers (1000s)") xtitle("Hourly wage bins ($)") ///
			xlabel(.15 "$0.15" .5 "$0.50" .75 "$0.75" .95 "$<1" 1 "$1" 1.15 "$1.15" 1.50 "$1.50" 1.60 "$1.60" 2.00 "$2.00" 2.50 "$2.50"  3.00 "$3.00", alternate) ///
			xline(1, lstyle(dash) lwidth(medium) lcolor(myarticblue)) xline(1.15, lstyle(dash) lwidth(vthin) lcolor(myred)) ///
			legend(order(1 "1963" 2 "1966" 3 "1967" 4 "1968") ring(0) position(2) bmargin(large) color(gs1) c(1)  region(style(none))  ) graphregion(color(white))

	gr export "figures/laundries_`r'_`o'_`g'.png", replace
	}	
	}
	}
	}
		
*FIGURES D4 to D7 : HOURLY WAGE DISTRIBUTION IN HOTELS & RESTAURANTS (TIPPED AND NON-TIPPED) 
*------HOTELS & RESTAURANTS (SEPARATLY & TOGETHER), AND ALL------------------------------------
foreach i in "hotels"  "restaurants" /*"hot_rest" "all"*/ {
foreach a in "all" /*"metropolitan" "nonmetropolitan"*/ {
	foreach r in /*"us"*/ "ne" "nc"  "s" "w" {
	foreach o in /*"all"*/ "td" "ntd" {
	foreach g in "mf" /*"f" "m"*/ {
	
	twoway 	(connected `i'_1963_tot thr if region=="`r'" & occupation=="`o'" & area=="`a'" & gender=="`g'", color(mydeepblue) msize(small) sort) ///
			(connected `i'_1966_tot thr if region=="`r'" & occupation=="`o'" & area=="`a'" & gender=="`g'", lcolor(mydeepblue) mcolor(white) mlcolor(mydeepblue) msize(small) sort) ///
			(connected `i'_1967_tot thr if region=="`r'" & occupation=="`o'" & area=="`a'" & gender=="`g'", lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort), ///
			ytitle("Number of workers (1000s)") xtitle("Hourly wage bins ($)") ///
			xlabel(.15 "$0.15" .5 "$0.50" .75 "$0.75" .95 "$<1" 1 "$1" 1.15 "$1.15" 1.50 "$1.50" 1.60 "$1.60" 2.00 "$2.00" 2.50 "$2.50"  3.00 "$3.00", alternate) ///
			xline(1, lpattern(solid) lwidth(medium) lcolor(myarticblue)) xline(0.5, lpattern(dash) lwidth(medium) lcolor(myarticblue))  ///
			legend(order(1 "1963" 2 "1966" 3 "1967") ring(0) position(2) bmargin(large) color(gs1) c(1)  region(style(none))  ) graphregion(color(white))

	gr export "figures/`i'_`r'_`o'_`g'.png", replace
	}	
	}
	}
	}
	}		
	
		
*FIGURES D8: HOURLY WAGE DISTRIBUTION IN NURSING HOMES 
*------NURSING HOMES------------------------------------------------------------------------------
foreach a in "all"  {
	foreach r in "us" "ne" "nc" "s" "w" {
	foreach o in "all"  {
	foreach g in "mf"  {
	
	twoway 	(connected nursing_1965_tot thr if region=="`r'" & occupation=="`o'" & area=="`a'" & gender=="`g'", color(mydeepblue) msize(small) sort) ///
			(connected nursing_1967_tot thr if region=="`r'" & occupation=="`o'" & area=="`a'" & gender=="`g'", lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort) ///
			(connected nursing_1968_tot thr if region=="`r'" & occupation=="`o'" & area=="`a'" & gender=="`g'", color(myred) msize(small) sort), ///
			ytitle("Number of workers (1000s)") xtitle("Hourly wage bins ($)") ///
			xlabel(.15 "$0.15" .5 "$0.50" .75 "$0.75" .95 "$<1" 1 "$1" 1.15 "$1.15" 1.50 "$1.50" 1.60 "$1.60" 2.00 "$2.00" 2.50 "$2.50"  3.00 "$3.00", alternate) ///
			xline(1, lstyle(dash) lwidth(medium) lcolor(myarticblue)) xline(1.15, lstyle(dash) lwidth(vthin) lcolor(myred)) ///
			legend(order(1 "1965"  2 "1967" 3 "1968") ring(0) position(2) bmargin(large) color(gs1) c(1)  region(style(none))  ) graphregion(color(white))

	gr export "figures/nursing_`r'_`o'_`g'.png", replace
	}	
	}
	}
	}
		

*FIGURES D9: HOURLY WAGE DISTRIBUTION IN SCHOOLS
*------SCHOOLS------------------------------------------------------------------------------
use "data/raw/bls_schools_1968_1969.dta", clear // Elementary and secondary schools only
	gen US_1968_october_tot=. 
	gen US_1969_march_tot=.
	
	gen NE_1968_october_tot=. 
	gen NE_1969_march_tot=.
	
	gen NC_1968_october_tot=. 
	gen NC_1969_march_tot=.
	
	gen S_1968_october_tot=. 
	gen S_1969_march_tot=.
	
	gen W_1968_october_tot=. 
	gen W_1969_march_tot=.
	
	rename *, lower
	
	foreach  r in "us" "ne" "nc" "s" "w" {
	foreach ym in "1968_october" "1969_march" {
	
	replace `r'_`ym'_tot 	=(`r'_`ym'_share * `r'_`ym'_total) /1000
	}
	}
	
	replace wage_bin=1.10 if wage_bin==0
	foreach  r in "us" "ne" "nc" "s" "w" {
	
	twoway 	(connected `r'_1968_october_tot wage_bin, lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small)  sort) ///
			(connected `r'_1969_march_tot wage_bin, color(myred) msize(small) sort), ///	
			ytitle("Number of workers (1000s)") xtitle("Hourly wage bins ($)") ///
			xlabel(1.15 "$<1.30"  1.30 "$1.30" 1.50 "$1.50" 1.60 "$1.60" 2.00 "$2.00" 2.50 "$2.50"  3.00 "$3.00" 3.50 ">$3.50", alternate) ///
			xline(1.15, lstyle(dash) lwidth(medium) lcolor(myarticblue)) xline(1.30, lstyle(dash) lwidth(vthin) lcolor(myred)) ///
			legend(order(1 "1968"  2 "1969") ring(0) position(2) bmargin(large) color(gs1) c(1)  region(style(none))  ) graphregion(color(white))
	gr export "figures/schools_`r'_all_mf.png", replace
	}		
	
*FIGURES D10: HOURLY WAGE DISTRIBUTION IN HOSPITALS
*------HOSPITALS------------------------------------------------------------------------------
use "data/raw/bls_hospitals_1966_1969.dta", clear
	
	gen US_1966_july_tot=. 
	gen US_1969_march_tot=.
	
	gen NE_1966_july_tot=. 
	gen NE_1969_march_tot=.
	
	gen NC_1966_july_tot=. 
	gen NC_1969_march_tot=.
	
	gen S_1966_july_tot=. 
	gen S_1969_march_tot=.
	
	gen W_1966_july_tot=. 
	gen W_1969_march_tot=.
	
	rename *, lower
	
	foreach  r in "us" "ne" "nc" "s" "w" {
	foreach ym in "1966_july" "1969_march" {
	
	replace `r'_`ym'_tot 	=(`r'_`ym'_share * `r'_`ym'_total) /1000
	}
	}
	
	replace wage_bin=1.25 if wage_bin==0
	foreach  r in "us"  "ne" "nc" "s" "w" {
	
	twoway 	(connected `r'_1966_july_tot wage_bin, color(mydeepblue) msize(small) sort) ///
			(connected `r'_1969_march_tot wage_bin, lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort), ///
			ytitle("Number of workers (1000s)") xtitle("Hourly wage bins ($)") ///
			xlabel(1.25 "$<1.30"  1.30 "$1.30" 1.50 "$1.50" 1.60 "$1.60" 2.00 "$2.00" 2.50 "$2.50"  3.00 ">$3.00", alternate) ///
			xline(1, lstyle(dash) lwidth(medium) lcolor(myarticblue)) xline(1.30, lstyle(dash) lwidth(vthin) lcolor(myred)) ///
			legend(order(1 "1966"  2 "1969") ring(0) position(2) bmargin(large) color(gs1) c(1)  region(style(none))  ) graphregion(color(white))
	gr export "figures/hospitals_`r'_all_mf.png", replace
	}	



*PART B. [SLIDES] [BUNCHING GRAPHS: OBSERVED DISTRIBUTIONS] 
*FIGURES [SLIDES ONLY]: HOURLY WAGE DISTRIBUTION IN LAUNDRIES IN SOUTH
use "data/raw/bls_industry_reports.dta", clear	

*[PRELIMINARY]: Convert numbers of workers in nb of workers (1000s) for bunching graphs
	foreach y in "1963" "1966"  {
		replace hotels_`y'_tot 		= hotels_`y'_tot/1000
		replace restaurants_`y'_tot = restaurants_`y'_tot/1000 
		replace laundries_`y'_tot 	= laundries_`y'_tot/1000
		replace all_`y'_tot 		= all_`y'_tot/1000
		replace hot_rest_`y'_tot 	= hot_rest_`y'_tot/1000

		replace hotels_`y'_nb 		= hotels_`y'_nb/1000
		replace restaurants_`y'_nb 	= restaurants_`y'_nb/1000 
		replace laundries_`y'_nb 	= laundries_`y'_nb/1000
		replace all_`y'_nb  		= all_`y'_nb/1000
		replace hot_rest_`y'_nb  	= hot_rest_`y'_nb/1000
	
	}
	
	
	foreach y in "1965"   {
		*number of workers in 1000s
		replace nursing_`y'_tot 	= nursing_`y'_tot/1000
		replace nursing_`y'_nb 		= nursing_`y'_nb/1000
		replace all_`y'_tot 		= all_`y'_tot/1000
		replace all_`y'_nb  		= all_`y'_nb/1000
		
	}
	
	foreach y in "1967"  {
		*number of workers in 1000s
		replace hotels_`y'_tot 		= hotels_`y'_tot/1000
		replace restaurants_`y'_tot = restaurants_`y'_tot/1000 
		replace laundries_`y'_tot 	= laundries_`y'_tot/1000
		replace nursing_`y'_tot 	= nursing_`y'_tot/1000
		replace all_`y'_tot 		= all_`y'_tot/1000
		replace hot_rest_`y'_tot 	= hot_rest_`y'_tot/1000

		replace hotels_`y'_nb 		= hotels_`y'_nb/1000
		replace restaurants_`y'_nb 	= restaurants_`y'_nb/1000 
		replace laundries_`y'_nb 	= laundries_`y'_nb/1000
		replace nursing_`y'_nb 		= nursing_`y'_nb/1000
		replace all_`y'_nb  		= all_`y'_nb/1000
		replace hot_rest_`y'_nb  	= hot_rest_`y'_nb/1000
	
	}
	
	foreach y in "1968"  {
		*number of workers in 1000s
		replace laundries_`y'_tot 	= laundries_`y'_tot/1000
		replace nursing_`y'_tot 	= nursing_`y'_tot/1000
		replace all_`y'_tot 		= all_`y'_tot/1000

		replace laundries_`y'_nb 	= laundries_`y'_nb/1000
		replace nursing_`y'_nb 		= nursing_`y'_nb/1000
		replace all_`y'_nb  		= all_`y'_nb/1000
	
	}

*[SLIDES ONLY] INTRO FIGURES FOR LAUNDRIES IN SOUTH
	twoway 		(connected laundries_1963_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(mydeepblue) msize(small) sort), ///
				ytitle("Number of workers (1000s)") xtitle("Hourly wage bins (nominal $)") ///
				xlabel( .5 "0.50" .75 "0.75"  1 "1" 1.15 "1.15" 1.50 "1.50"  2.00 "2.00" 2.50 "2.50"  3.00 "3.00") ylabel(0(20)60) ///
				legend(order(1 "1963") ring(0) position(2) bmargin(large) color(gs1) c(1) on region(style(none))  ) graphregion(color(white))
	gr export 	"figures/laundries_1963.png", replace					

	twoway 		(connected laundries_1963_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(mydeepblue) msize(small) sort) ///
				(connected laundries_1966_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(mydeepblue) mcolor(white) mlcolor(mydeepblue) msize(small) sort), ///
				ytitle("Number of workers (1000s)") xtitle("Hourly wage bins (nominal $)") ///
				xlabel( .5 "0.50" .75 "0.75"  1 "1" 1.15 "1.15" 1.50 "1.50"  2.00 "2.00" 2.50 "2.50"  3.00 "3.00") ylabel(0(20)60) ///
				legend(order(1 "1963" 2 "1966" ) ring(0) position(2) bmargin(large) color(gs1) c(1) on region(style(none))  ) graphregion(color(white))
	gr export 	"figures/laundries_1966.png", replace	
	
	twoway 		(connected laundries_1963_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(mydeepblue) msize(small) sort) ///
				(connected laundries_1966_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(mydeepblue) mcolor(white) mlcolor(mydeepblue) msize(small) sort), ///
				ytitle("Number of workers (1000s)") xtitle("Hourly wage bins (nominal $)") ///
				xlabel( .5 "0.50" .75 "0.75"  1 "1" 1.15 "1.15" 1.50 "1.50"  2.00 "2.00" 2.50 "2.50"  3.00 "3.00") ylabel(0(20)60) ///
				xline(1, lstyle(dash) lwidth(medium) lcolor(myarticblue))  ///
				legend(order(1 "1963" 2 "1966" ) ring(0) position(2) bmargin(large) color(gs1) c(1) on region(style(none))  ) graphregion(color(white)) ///
				text( 55 1.05 "$1 an hour = value at which" "min. wage is introduced in 1967", ///
				place(se) box c(myarticblue) bc(myarticblue) fc(white) lwidth(medium) just(left) margin(l+3 t+1 b+1) width(50.6)) 
	gr export 	"figures/laundries_1966_wbox.png", replace					
	
	twoway 		(connected laundries_1963_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(mydeepblue) msize(small) sort) ///
				(connected laundries_1966_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(mydeepblue) mcolor(white) mlcolor(mydeepblue) msize(small) sort) ///
				(connected laundries_1967_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort), ///
				ytitle("Number of workers (1000s)") xtitle("Hourly wage bins (nominal $)") ///
				xlabel( .5 "0.50" .75 "0.75"  1 "1" 1.15 "1.15" 1.50 "1.50"  2.00 "2.00" 2.50 "2.50"  3.00 "3.00") ylabel(0(20)60) ///
				xline(1, lstyle(dash) lwidth(medium) lcolor(myarticblue))  ///
				legend(order(1 "1963" 2 "1966" 3 "1967" ) ring(0) position(2) bmargin(large) color(gs1) c(1) on region(style(none))  ) graphregion(color(white)) ///
				text( 55 1.05 "$1 an hour = value at which" "min. wage is introduced in 1967", ///
				place(se) box c(myarticblue) bc(myarticblue) fc(white) lwidth(medium) just(left) margin(l+3 t+1 b+1) width(50.6)) 
	gr export 	"figures/laundries_1967_wbox.png", replace

*FIGURE [SLIDES ONLY]:	
	gen 	laundries_1967_tot_bottom = laundries_1967_tot
	replace laundries_1967_tot_bottom = . if thr>1.15 & region=="s" & occupation=="all" & area=="all" & gender=="mf"

	gen 	laundries_1966_tot_bottom = laundries_1966_tot
	replace laundries_1966_tot_bottom = . if thr>1.10 & region=="s" & occupation=="all" & area=="all" & gender=="mf"

	gen 	laundries_1967_tot_top = laundries_1967_tot
	replace laundries_1967_tot_top = . if thr<1.30 & region=="s" & occupation=="all" & area=="all" & gender=="mf"

	gen 	laundries_1966_tot_top = laundries_1966_tot
	replace laundries_1966_tot_top = . if thr<1.25 & region=="s" & occupation=="all" & area=="all" & gender=="mf"	
	
	*LAUNDRIES, BOTTOM
	twoway		(connected laundries_1966_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(mydeepblue) mcolor(white) mlcolor(mydeepblue) msize(small) sort) ///
				(connected laundries_1967_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort) ///
				(pcarrowi 57 0.95 57 1.10 (9), color(mydeepblue)) ///
				(pcarrowi 57 1.30 57 1.15 (3), color(mydeepblue)) ///
				(pci 	  0  1.10  60  1.10,  lcolor(mydeepblue) lpattern(dash)) ///  
				(pci 	  0  1.15  60  1.15,  lcolor(myarticblue) lpattern(solid)), ///  					
				ytitle("Number of workers (1000s)") xtitle("Hourly wage bins (nominal $)") ///
				xlabel( .5 "0.50" .75 "0.75"  1 "1" 1.15 "1.15" 1.50 "1.50"  2.00 "2.00" 2.50 "2.50"  3.00 "3.00") ylabel(0(20)60) ///
				legend(order(1 "1966" 2 "1967" ) ring(0) position(2) bmargin(large) color(gs1) c(1) on region(style(none))  ) graphregion(color(white)) ///
				text( 64 0.42 "Below 1.15*MW", ///
				size(medsmall) place(se) box c(mydeepblue) bc(mydeepblue) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(26)) ///	
				text( 59 0.48 "$1.10 in 1966", ///
				size(small) place(se) box c(mydeepblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(15)) ///
 				text( 59 1.35 "$1.15 in 1967", ///
				size(small) place(se) box c(mydeepblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(15)) 				
	gr export 	"figures/laundries_bunching_bottom_1.png", replace	

	twoway		(connected laundries_1966_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(mydeepblue) mcolor(white) mlcolor(mydeepblue) msize(small) sort) ///
				(connected laundries_1967_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort) ///
				(pcarrowi 57 0.95 57 1.10 (9), color(mydeepblue)) ///
				(pcarrowi 57 1.30 57 1.15 (3), color(mydeepblue)) ///
				(area 	   laundries_1967_tot_bottom thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(myarticblue) lwidth(none)) ///
				(area 	   laundries_1966_tot_bottom thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(mydeepblue) lwidth(none)) ///
				(pci 	  0  1.10  60  1.10,  lcolor(mydeepblue) lpattern(dash)) ///  
				(pci 	  0  1.15  60  1.15,  lcolor(myarticblue) lpattern(solid)), ///  					
				ytitle("Number of workers (1000s)") xtitle("Hourly wage bins (nominal $)") ///
				xlabel( .5 "0.50" .75 "0.75"  1 "1" 1.15 "1.15" 1.50 "1.50"  2.00 "2.00" 2.50 "2.50"  3.00 "3.00") ylabel(0(20)60) ///
				legend(order(1 "1966" 2 "1967" ) ring(0) position(2) bmargin(large) color(gs1) c(1) on region(style(none))  ) graphregion(color(white)) ///
				text( 64 0.42 "Below 1.15*MW", ///
				size(medsmall) place(se) box c(mydeepblue) bc(mydeepblue) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(26)) ///
				text(18 0.48 "74,384 workers", ///
				size(small) place(se) box c(myarticblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+0.5 t+0.5 b+0.5) width(20)) ///
				text(15 0.48 "73,815 workers", ///
				size(small) place(se) box c(mydeepblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+0.5 t+0.5 b+0.5) width(20)) ///				
				text( 59 0.48 "$1.10 in 1966", ///
				size(small) place(se) box c(mydeepblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(15)) ///
 				text( 59 1.35 "$1.15 in 1967", ///
				size(small) place(se) box c(mydeepblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(15)) 				
	gr export 	"figures/laundries_bunching_bottom_2.png", replace			
	
	*LAUNDRIES, TOP
	twoway		(connected laundries_1966_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(mydeepblue) mcolor(white) mlcolor(mydeepblue) msize(small) sort) ///
				(connected laundries_1967_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort) ///
				(pcarrowi 57 1.10 57 1.25 (9), color(mydeepblue)) ///
				(pcarrowi 57 1.45 57 1.30 (3), color(mydeepblue)) ///
				(area 	   laundries_1967_tot_top thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(myarticblue%40) lwidth(none)) ///
				(area 	   laundries_1966_tot_top thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(mydeepblue) lwidth(none)) ///
				(pci 	  0  1.25  60  1.25,  lcolor(mydeepblue) lpattern(dash)) ///  
				(pci 	  0  1.30  60  1.30,  lcolor(myarticblue) lpattern(solid)), /// 
				ytitle("Number of workers (1000s)") xtitle("Hourly wage bins (nominal $)") ///
				xlabel( .5 "0.50" .75 "0.75"  1 "1" 1.15 "1.15" 1.50 "1.50"  2.00 "2.00" 2.50 "2.50"  3.00 "3.00") ylabel(0(20)60) ///
				legend(order(1 "1966" 2 "1967" ) ring(0) position(2) bmargin(large) color(gs1) c(1) on region(style(none))  ) graphregion(color(white)) ///
				text(64 1.35 "Above 1.30*MW", ///
				size(medsmall) place(se) box c(mydeepblue) bc(mydeepblue) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(26)) ///
				text(18 1.35 "55,828 workers", ///
				size(small) place(se) box c(myarticblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+0.5 t+0.5 b+0.5) width(20)) ///
				text(15 1.35 "54,221 workers", ///
				size(small) place(se) box c(mydeepblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+0.5 t+0.5 b+0.5) width(20)) ///		
				text(59 0.65 "$1.25 in 1966", ///
				size(small) place(se) box c(mydeepblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(15)) ///
 				text(59 1.50 "$1.30 in 1967", ///
				size(small) place(se) box c(mydeepblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(15)) 						
	gr export 	"figures/laundries_bunching_top.png", replace		
	
	*LAUNDRIES, TOP & BOTTOM
	twoway		(connected laundries_1966_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(mydeepblue) mcolor(white) mlcolor(mydeepblue) msize(small) sort) ///
				(connected laundries_1967_tot thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort) ///
				(area 	   laundries_1967_tot_top thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(myarticblue%40) lwidth(none)) ///
				(area 	   laundries_1966_tot_top thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(mydeepblue) lwidth(none)) ///
				(area 	   laundries_1967_tot_bottom thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(myarticblue) lwidth(none)) ///
				(area 	   laundries_1966_tot_bottom thr if region=="s" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(mydeepblue) lwidth(none)) ///
				(pci 	  0  1.25  60  1.25,  lcolor(mydeepblue) lpattern(dash)) ///  
				(pci 	  0  1.30  60  1.30,  lcolor(myarticblue) lpattern(solid)) /// 				
				(pci 	  0  1.10  60  1.10,  lcolor(mydeepblue) lpattern(dash)) ///  
				(pci 	  0  1.15  60  1.15,  lcolor(myarticblue) lpattern(solid)), ///  					
				ytitle("Number of workers (1000s)") xtitle("Hourly wage bins (nominal $)") ///
				xlabel( .5 "0.50" .75 "0.75"  1 "1" 1.15 "1.15" 1.50 "1.50"  2.00 "2.00" 2.50 "2.50"  3.00 "3.00") ylabel(0(20)60) ///
				legend(order(1 "1966" 2 "1967" ) ring(0) position(2) bmargin(large) color(gs1) c(1) on region(style(none))  ) graphregion(color(white)) ///
				text( 64 0.42 "Below 1.15*MW", ///
				size(medsmall) place(se) box c(mydeepblue) bc(mydeepblue) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(26)) ///				
				text(64 1.35 "Above 1.30*MW", ///
				size(medsmall) place(se) box c(mydeepblue) bc(mydeepblue) fc(white) lwidth(medium) just(left) margin(l+1 t+1 b+1) width(26)) ///
				text(18.5 0.48 "74,384 workers", ///
				size(small) place(se) box c(myarticblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+0.5 t+0.5 b+0.5) width(20)) ///
				text(15.5 0.48 "73,815 workers", ///
				size(small) place(se) box c(mydeepblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+0.5 t+0.5 b+0.5) width(20)) ///	
				text(12.5 0.48 "= +1%", ///
				size(small) place(se) box c(mydeepblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+0.5 t+0.5 b+0.5) width(10)) ///					
				text(18.5 1.35 "55,828 workers", ///
				size(small) place(se) box c(myarticblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+0.5 t+0.5 b+0.5) width(20)) ///
				text(15.5 1.35 "54,221 workers", ///
				size(small) place(se) box c(mydeepblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+0.5 t+0.5 b+0.5) width(20)) ///
				text(12.5 1.35 "= +1%", ///
				size(small) place(se) box c(mydeepblue) bc(white) fc(white) lwidth(medium) just(left) margin(l+0.5 t+0.5 b+0.5) width(10)) 										
	gr export 	"figures/laundries_bunching_top&bottom.png", replace	

**EMPLOYMENT BUNCHING GRAPHS FOR SLIDES (ALL FOUR REGIONS FOR LAUNDRIES)
	*NORTHEAST
	twoway 	(connected laundries_1963_tot thr if region=="ne" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(mydeepblue) msize(small) sort) ///
			(connected laundries_1966_tot thr if region=="ne" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(mydeepblue) mcolor(white) mlcolor(mydeepblue) msize(small) sort) ///
			(connected laundries_1967_tot thr if region=="ne" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort) ///
			(connected laundries_1968_tot thr if region=="ne" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(myred) msize(small) sort), ///
			ytitle("Number of workers (1000s)") xtitle("Hourly wage bins (nominal $)") ///
			xlabel( .5 "0.50" .75 "0.75"  1 "1" 1.15 "1.15" 1.50 "1.50"  2.00 "2.00" 2.50 "2.50"  3.00 "3.00") ylabel(0(10)40) ///
			xline(1.25, lstyle(dash) lwidth(medium) lcolor(mydeepblue)) xline(1.5, lstyle(dash) lwidth(medium) lcolor(myarticblue)) xline(1.6, lstyle(dash) lwidth(medium) lcolor(myred)) ///
			legend(order(1 "1963" 2 "1966" 3 "1967" 4 "1968") ring(0) position(2) bmargin(large) color(gs1) c(1)  region(style(none))  ) graphregion(color(white))
	 gr export "figures/laundries_ne_1968.png", replace

	*WEST
	twoway 	(connected laundries_1963_tot thr if region=="w" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(mydeepblue) msize(small) sort) ///
			(connected laundries_1966_tot thr if region=="w" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(mydeepblue) mcolor(white) mlcolor(mydeepblue) msize(small) sort) ///
			(connected laundries_1967_tot thr if region=="w" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort) ///
			(connected laundries_1968_tot thr if region=="w" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(myred) msize(small) sort), ///
			ytitle("Number of workers (1000s)") xtitle("Hourly wage bins (nominal $)") ///
			xlabel( .5 "0.50" .75 "0.75"  1 "1" 1.15 "1.15" 1.50 "1.50"  2.00 "2.00" 2.50 "2.50"  3.00 "3.00") ylabel(0(2)16) ///
			xline(1.25, lstyle(dash) lwidth(medium) lcolor(mydeepblue)) xline(1.5, lstyle(dash) lwidth(medium) lcolor(myarticblue)) xline(1.60, lstyle(dash) lwidth(medium) lcolor(myred)) ///
			legend(order(1 "1963" 2 "1966" 3 "1967" 4 "1968") ring(0) position(2) bmargin(large) color(gs1) c(1)  region(style(none))  ) graphregion(color(white))
	 gr export "figures/laundries_w_1968.png", replace	 
	 
	*MIDWEST
	twoway 	(connected laundries_1963_tot thr if region=="nc" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(mydeepblue) msize(small) sort) ///
			(connected laundries_1966_tot thr if region=="nc" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(mydeepblue) mcolor(white) mlcolor(mydeepblue) msize(small) sort) ///
			(connected laundries_1967_tot thr if region=="nc" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, lcolor(myarticblue) mcolor(white) mlcolor(myarticblue) msize(small) sort) ///
			(connected laundries_1968_tot thr if region=="nc" & occupation=="all" & area=="all" & gender=="mf" & thr>0.4, color(myred) msize(small) sort), ///
			ytitle("Number of workers (1000s)") xtitle("Hourly wage bins (nominal $)") ///
			xlabel( .5 "0.50" .75 "0.75"  1 "1" 1.15 "1.15" 1.50 "1.50"  2.00 "2.00" 2.50 "2.50"  3.00 "3.00") ylabel(0(2)16) ///
			xline(0.8, lpattern(dash) lwidth(medium) lcolor(mydeepblue)) xline(0.9, lstyle(dash) lwidth(medium) lcolor(mydeepblue)) xline(1, lstyle(dash) lwidth(medium) lcolor(myarticblue)) xline(1.15, lstyle(dash) lwidth(medium) lcolor(myred)) ///
			legend(order(1 "1963" 2 "1966" 3 "1967" 4 "1968") ring(0) position(2) bmargin(large) color(gs1) c(1)  region(style(none))  ) graphregion(color(white))
	 gr export "figures/laundries_nc_1968.png", replace	 	
		