# Réplication — Derenoncourt & Montialoux, *Minimum Wages and Racial Inequality* (QJE 2021)

## 1. Ce qu'il te faut

- **Stata** (≥ 14 ; les programmes utilisent `saveold`, `reghdfe`, `maptile`). Le package est 100 % Stata.
- Le **replication package complet** (pas seulement `pgm/`) : il se trouve sur le Harvard Dataverse du QJE
  (cherche « Minimum Wages and Racial Inequality » sur dataverse.harvard.edu) ou sur la page d'Ellora Derenoncourt.
  Le README dit que l'archive contient toutes les données brutes — tu n'as normalement **rien à télécharger toi-même**.

## 2. Arborescence attendue

Les do-files utilisent des chemins relatifs à la racine `replication/` :

```
replication/
├── pgm/                 <- les 21 do-files (ne pas utiliser pgm/old/)
├── color_palette/       <- fichiers .style
├── data/
│   ├── raw/             <- données brutes (liste ci-dessous)
│   └── output/          <- créé/rempli par les programmes
├── figures/
└── tables/
```

Fichiers lus dans `data/raw/` (vérifiés automatiquement par `00_setup_and_run.do`) :

| Fichier | Source | Utilisé par |
|---|---|---|
| `march_cps_1962_2017.dta` | IPUMS-CPS, ASEC 1962-2017 | 1d |
| `census_raw_1940_2017.dta` | IPUMS-USA, recensements 1940-2010 + ACS 2017 | 1j |
| `VZ_state_monthly.dta` | Vaghul & Zipperer (Equitable Growth), SMIC par État | 1f |
| `spd_mw_series_men/women_monthly.xlsx` | séries de SMIC construites par les auteurs | 1f |
| `pop_by_state.xlsx` | Census Bureau, population par État | 1e |
| `spd_cpi_u_rs_annual.dta` | CPI-U-RS | 1d, 1j |
| `bls_*.dta` (4 fichiers) | BLS Industry Wage Reports numérisés | 1k, 2b, 4a |
| `spd_industry_codes.xlsx`, `spd_states_codes.xlsx`, `spd_crosswalk_states_cps_cepr_org_statefips.xlsx`, `crosswalk_state_regions.dta` | crosswalks | 1a-1c, 1e |

⚠️ `data/output/ssa_unemployment_1962_1966.dta` est lu par `3b` mais **aucun programme ne le crée** : il doit être
fourni dans le package. Sinon, écrire aux auteurs.

### Si un des gros fichiers IPUMS manque
(arrive quand l'archive exclut les microdonnées pour des raisons de licence)

- **CPS** : https://cps.ipums.org → ASEC 1962-2017, variables listées ligne 15 de `1d_build_march_cps_nomw.do`
  (`year ind ind1950 ind50ly ind1990 ind90ly cpi99 incwage age sex empstat wkswork2 ahrsworkt gq classwkr fullpart
  labforce classwly asecwt reportyr region statefip metro metarea metfips race educ occ occly qocc occ1950 occ50ly
  occ1990 occ90ly marst inctot incbus incfarm incunern`), format .dta, renommer en `march_cps_1962_2017.dta`.
- **Census** : https://usa.ipums.org → échantillons 1940, 1950, 1960, 1970, 1980, 1990, 2000, ACS 2010 et 2017 ;
  variables lues dans `1j_build_census.do` : `year race sex age educ school incwage empstat labforce occ1950 ind1950
  classwkr wkswork2 hrswork2 gq cpi99` + une variable de poids nommée `weight` (= `perwt` IPUMS, à renommer). Renommer en `census_raw_1940_2017.dta`.
  Les extraits refaits aujourd'hui peuvent différer légèrement (IPUMS révise ses codes) → petits écarts possibles.

## 3. Lancer

1. Dézipper le package, placer `00_setup_and_run.do` à la racine `replication/`.
2. Modifier **une seule ligne** : `global path "…/replication"`.
3. `do 00_setup_and_run.do` — il installe les packages SSC (y compris `coefplot` et `blindschemes`, oubliés dans
   la liste du master original), les fonds de carte `maptile`, la palette de couleurs, crée les dossiers manquants
   (`tables/bls_calculations/temp/Input|Output`, dont le `mkdir` est commenté dans `1k`), vérifie les données puis
   exécute les 20 do-files dans l'ordre du `0_master.do` avec un log.

Temps de calcul : le 3b (≈ 4 000 lignes de `reghdfe`) est de loin le plus long ; compter plusieurs heures au total.

## 4. Pièges repérés dans le code

- `pgm/old/` = anciennes versions, à ignorer.
- `1d` contient un bloc commenté (`/* … */`) d'imputation `hotdeck` qui lit `data/raw/imp1.dta` : il n'est pas exécuté,
  pas besoin de ce fichier.
- `set matsize 10000` peut renvoyer un message sous Stata MP récent : sans conséquence.
- La correspondance tableaux/figures ↔ do-files est dans `DM2019_list_tables&figures.xls` à la racine du package.
