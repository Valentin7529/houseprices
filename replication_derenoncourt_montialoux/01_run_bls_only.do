*------------------------------------------------------------------------------
* Partie BLS seule (Industry Wage Reports) : ne depend PAS des CPS / Census.
* Fichiers requis dans data/raw/ :
*   bls_industry_reports.dta, bls_schools_1968_1969.dta,
*   bls_hospitals_1966_1969.dta, bls_iwr_wage_effect.dta,
*   spd_cpi_u_rs_annual.dta (pour 4a uniquement)
* Lancer 00_setup_and_run.do une premiere fois avant (packages + palette),
* ou au moins les etapes 1 a 4 de ce fichier.
*------------------------------------------------------------------------------
clear all
set more off

*>>> ACTION REQUIRED : meme chemin que dans 00_setup_and_run.do
global path "C:/Users/TOI/Documents/houseprices/replication_derenoncourt_montialoux"
cd "$path"
adopath + "$path/color_palette"
discard
foreach d in data/output figures tables {
    cap mkdir "`d'"
}

log using "replication_bls_log.smcl", replace
foreach f in 1k_build_bls 2b_bls_descriptives 4a_bls_wage {
    di as result _n "===== `f' ====="
    do "pgm/`f'.do"
    cd "$path"
}
* 4b_bls_employment a besoin de data/output/cps_master_individual_level.dta
* (produit par la chaine CPS 1d -> 1i) : a lancer plus tard.
log close
