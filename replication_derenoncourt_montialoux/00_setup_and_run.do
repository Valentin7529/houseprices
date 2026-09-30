*------------------------------------------------------------------------------
* Setup + lancement de la replication
* Derenoncourt & Montialoux (2021), "Minimum Wages and Racial Inequality", QJE
*
* A lancer A LA PLACE de pgm/0_master.do (qui l'appelle a la fin).
* Seule chose a modifier : le chemin ci-dessous.
*------------------------------------------------------------------------------
clear all
set more off
set maxvar 10000

*>>> ACTION REQUIRED : dossier "replication" dezippe (celui qui contient data/, pgm/, color_palette/ ...)
global path "C:/Users/TOI/Documents/replication"
cd "$path"

*--- 1. Packages SSC (une seule fois) ------------------------------------------
foreach p in winsor2 hotdeck estout maptile spmap shp2dta reghdfe ftools egenmore coefplot blindschemes {
    cap which `p'
    if _rc ssc install `p', replace
}
* estout fournit esttab/estpost ; blindschemes fournit le scheme plotplain (3b)
cap reghdfe, compile

*--- 2. Fonds de carte pour maptile (figures du 2a) -----------------------------
cap maptile_install using "http://files.michaelstepner.com/geo_state.zip"
cap maptile_install using "http://files.michaelstepner.com/geo_statehex.zip"

*--- 3. Palette de couleurs (myred, myarticblue, mydeepblue, mybeige) ----------
* Les fichiers color-*.style doivent etre trouvables par Stata.
adopath + "$path/color_palette"
discard

*--- 4. Dossiers de sortie (certains mkdir sont commentes dans les do-files) ----
foreach d in data/output figures tables tables/bls_calculations ///
             tables/bls_calculations/temp tables/bls_calculations/temp/Input ///
             tables/bls_calculations/temp/Output {
    cap mkdir "`d'"
}

*--- 5. Verification des donnees brutes ------------------------------------------
local raw march_cps_1962_2017.dta census_raw_1940_2017.dta VZ_state_monthly.dta ///
    bls_industry_reports.dta bls_iwr_wage_effect.dta bls_schools_1968_1969.dta  ///
    bls_hospitals_1966_1969.dta spd_cpi_u_rs_annual.dta crosswalk_state_regions.dta ///
    pop_by_state.xlsx spd_mw_series_men_monthly.xlsx spd_mw_series_women_monthly.xlsx ///
    spd_industry_codes.xlsx spd_states_codes.xlsx ///
    spd_crosswalk_states_cps_cepr_org_statefips.xlsx
local manque 0
foreach f of local raw {
    cap confirm file "data/raw/`f'"
    if _rc {
        di as error "MANQUANT : data/raw/`f'"
        local manque 1
    }
}
* fichier lu par 3b mais jamais cree par les programmes : doit etre fourni
cap confirm file "data/output/ssa_unemployment_1962_1966.dta"
if _rc di as error "MANQUANT : data/output/ssa_unemployment_1962_1966.dta (fourni dans le package ? sinon le 3b plantera)"
if `manque' {
    di as error "Donnees brutes incompletes : voir README.md"
    exit 601
}

*--- 6. Lancement --------------------------------------------------------------
* (0_master.do refait un "cd $path" avec global path = "[.....]" : on saute
*  son en-tete et on execute directement la sequence des do-files.)
log using "replication_log.smcl", replace
foreach f in 1a_crosswalk_industry 1b_crosswalk_states 1c_crosswalk_states_cps_org_statefip ///
    1d_build_march_cps_nomw 1e_weights_pop_by_state 1f_mw_database_monthly_state          ///
    1g_weights_workers_by_state_group 1gBIS_weights_workers_by_state_group_for_Kaitz_Index  ///
    1h_mw_database_annual_state_group 1hBIS_mw_database_annual_state_group_for_Kaitz_Index  ///
    1i_build_march_cps_withmw 1j_build_census 1k_build_bls                                ///
    2a_cps_census_descriptives 2b_bls_descriptives                                        ///
    3a_cps_wage 3b_cps_employment 3c_cps_racial_gaps                                      ///
    4a_bls_wage 4b_bls_employment {
    di as result _n "===== `f' ====="
    do "pgm/`f'.do"
    cd "$path"
}
log close
