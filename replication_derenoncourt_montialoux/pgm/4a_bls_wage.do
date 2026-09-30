*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*program: insheets raw csv files containing digitized wage distributions, clean them and prepare the dataset for bunching.  
*first created: 11/13/2018
*last updated:  08/31/2020
*structure: 	1.create BLS dataset with control industries
*				2.wage effect of the reform
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	
clear all
set more off
set matsize 10000
set maxvar 10000

use "data/raw/bls_iwr_wage_effect.dta", clear	

*%----------------------------------------------------------------------------------------------------------------------------
*PART1: BUILD DATASET WITH MEASURE OF AVERAGE WAGES IN $2017
*%----------------------------------------------------------------------------------------------------------------------------
	drop ln_avgwage
	
	*merge with data on inflation	
	merge m:1 year using "$path/data/raw/spd_cpi_u_rs_annual.dta"
			drop if !inrange(year,1961,1979) 
			drop if year==1977
			drop _merge
			label var cpi_u_rs "Annual average CPI-U-RS Using Current Methods All items (2017=100)"
					
	*convert average_wage in $2017 dollars using cpi_u_rs
	gen avg_wage = average_wage *cpi_u_rs
	
	label var avg_wage "average wage in $2017"
	label var average_wage "average wage in nominal dollars"
	
	*log average wage
	g ln_avgwage=log(avg_wage)
	
	*Drop post-1969 because data for treated industries incomplete after 1970
	keep if year<=1970

	* Generate strict sample (years where control and treatment industries are available)
	g strict_sample=(year==1963 | year==1965 | year==1966 | year==1967 | year==1968 | year==1969)

	* Keeping only industries with pre- and post-reform observations available (drops movie theaters and schools)
	egen startyear=min(year),by(industry)
	replace strict_sample=0 if startyear>1966
	
	egen endyear=max(year),by(industry)
	replace strict_sample=0 if endyear<1967

	encode region, gen(regcode)
		
*%----------------------------------------------------------------------------------------------------------------------------
*PART2: WAGE AND EMPLOYMENT EFFECT OF THE REFORM
*%----------------------------------------------------------------------------------------------------------------------------
	*WAGE EFFECT -- SIMPLE INTERACTION -- same specification as in our baseline CPS regression 
		**full sample
		reg ln_avgwage i.newly##post i.indcode i.year i.regcode, cluster(indcode)
		eststo  aw_model1_full_sample
				estadd local hastimefe 		"Y"
				estadd local hasindustryfe 	"Y"
				estadd local hasregionfe 	"Y"
				
						
		**strict sample
		reg ln_avgwage i.newly##post i.indcode i.regcode i.year if strict_sample, cluster(indcode)
		eststo  aw_model1_strict_sample
				estadd local hastimefe 		"Y"						
				estadd local hasindustryfe 	"Y"
				estadd local hasregionfe 	"Y"						
	
	
	*WAGE EFFECT -- TRIPLE INTERACTION --proxy for black vs. white workers
		**full sample
		reg ln_avgwage i.newly##post##south i.indcode i.regcode i.year, cluster(indcode)
		eststo  aw_model2_full_sample
				estadd local hastimefe 		"Y"
				estadd local hasindustryfe 	"Y"
				estadd local hasregionfe 	"Y"
				
						
		**strict sample
		reg ln_avgwage i.newly##post##south i.indcode i.regcode i.year if strict_sample, cluster(indcode)
		eststo  aw_model2_strict_sample
				estadd local hastimefe 		"Y"						
				estadd local hasindustryfe 	"Y"
				estadd local hasregionfe 	"Y"			
	
	
	*output table
	esttab  aw_model1_full_sample aw_model1_strict_sample aw_model2_full_sample aw_model2_strict_sample   ///
			using "tables/table_hw_bls_2models.tex",  replace label fragment ///
			nolines  posthead(\cmidrule{2-5}) prefoot(\midrule) postfoot(\bottomrule \bottomrule) booktabs	///							
			nonumbers mtitle("Full sample" "Strict sample" "Full sample" "Strict sample") collabels(none)  ///
			cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///						
			keep(1.newly_covered#1.post 1.newly_covered#1.post#1.south)   ///
			coeflabel(1.newly_covered#1.post "Covered in 1967 $\times$ & & & & \\ \hspace{0.5cm}{1967-1969}" 1.newly_covered#1.post#1.south "\hspace{0.5cm}{1967-1969 $\times$ South}") ///
			stats(N hastimefe  hasindustryfe hasregionfe, ///
			fmt(%11.0gc) label("Observations" "Time FE" "Industry FE" "Region FE")) onecell 
