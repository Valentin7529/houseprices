*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: 		Build crosswalk_industry.dta: get a homogeneous definition of industry categories in March CPS 1962-2016.  
*first created: 03/14/2018
*last updated:  08/28/2020
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	


*import sheet crosswalk from industrial codes spreadsheet
import excel using "data/raw/spd_industry_codes.xlsx", sheet("crosswalk") firstrow case(lower) clear

		label var ind_var 		"CPS industry variable used for the crosswalk"
		label var ind_type 		"Ind. type used for the crosswalk: 1 ind 1962; 2 ind 1963-1967; 3 ind 1968-2016"
		label var industry 		"Unified industry code for 1962-2016"


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


saveold "data/output/crosswalk_industry.dta", replace
