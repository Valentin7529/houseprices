*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: weights used to compute average mw by year*state*industry*gender  
*first created: 03/14/2018
*last updated:  08/28/2020
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	

****************************************************************************************************
**WEIGHTS: POPULATION BY STATE (used to go from the state to the state_group level)
****************************************************************************************************
*import files containing state population 
	foreach series in "s10to17" "s00to09" "s90to99" "s85to89" "s80to84" "s76to79" "s70to75" {
	import excel using "data/raw/pop_by_state.xlsx", firstrow sheet("`series'") clear
	cap drop A	
		tempfile `series'
		save ``series'' 
	}	
		
	import excel using "data/raw/pop_by_state.xlsx", firstrow sheet("s65to69") clear
	drop A
		forval y=1965/1969 {
			gen y_`y' = y`y'*1000
			drop y`y'
			rename y_`y' y`y'
			}
		tempfile s65to69
		save `s65to69'
	
	import excel using "data/raw/pop_by_state.xlsx", firstrow sheet("s60to64") clear
	drop A
		forval y=1960/1964 {
			gen y_`y' = y`y'*1000
			drop y`y'
			rename y_`y' y`y'
			}
		tempfile s60to64
		save `s60to64'	
		
	import excel using "data/raw/pop_by_state.xlsx", firstrow sheet("s55to59") clear
	drop A
		forval y=1955/1959 {
			gen y_`y' = y`y'*1000
			drop y`y'
			rename y_`y' y`y'
			}
		tempfile s55to59
		save `s55to59'		
	
	import excel using "data/raw/pop_by_state.xlsx", firstrow sheet("s50to54") clear
	drop A
		forval y=1950/1954 {
			gen y_`y' = y`y'*1000
			drop y`y'
			rename y_`y' y`y'
			}
		tempfile s50to54
		save `s50to54'			

*merge them together to get a database with population at the state*year level (wide format)		
	merge 1:1 state_abb using `s55to59'
		drop _merge
	merge 1:1 state_abb using `s60to64'
		drop _merge
	merge 1:1 state_abb using `s65to69'
		drop _merge
	merge 1:1 state_abb using `s70to75'
		drop _merge
	merge 1:1 state_abb statefip using `s76to79'
		drop _merge
	merge 1:1 state_abb using `s80to84'
		drop _merge
	merge 1:1 state_abb using `s85to89'
		drop _merge
	merge 1:1 statefip using `s90to99'
		drop _merge
	merge 1:1 statefip using `s00to09'
		drop _merge								
	merge 1:1 statefip using `s10to17'
		drop _merge			
	
*merge with crosswalk for states to region and reshape in long format	
	*drop state_name
	order statefip state_abb state_name
	merge 1:1 statefip  using "data/raw/crosswalk_state_regions.dta"		
		drop _m
		
		reshape long y, i(statefip state_abb state_name region_abb) j(year) 
		drop if !inrange(year,1950,2016)
		rename y pop_by_state
		
		
*create population estimates by region 
		egen pop_by_region = total(pop_by_state), by(region_abb year)		
		
		gen pop_by_state_rounded = round(pop_by_state)
		gen pop_by_region_rounded = round(pop_by_region)
		
		drop pop_by_state pop_by_region
		rename pop_by_state_rounded pop_by_state
		rename pop_by_region_rounded pop_by_region
		
		format pop_by_state  %12.0gc
		format pop_by_region  %15.0gc
		
		label var pop_by_state 	"Population estimates by states (Census Bureau)"	
		label var pop_by_region "Population estimates by regions (Census Bureau)"		
		
saveold "data/output/pop_by_state_and_region.dta", replace		



