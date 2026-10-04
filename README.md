# medical-students-mental-health-scoping-review

# Global gaps in how we assess undergraduate medical students' mental health: a scoping review

Data and R code reproducing all figures and numbers of the review (541 studies, 1980-2025).

## Repository structure

```
data/
  data_extraction_sheet.xlsx    one row per included study + data dictionary
  scale_catalogue.xlsx          catalogue of the measurement instruments (with domain and subdomain)
  thematic_framework.xlsx       thematic framework (Table 1): domain > subdomain > instruments
  pubmed_records_per_year.csv   PubMed records per publication year (NCBI E-utilities, query YYYY[dp])
code/
  00_data_management.R          data checks, country table (English name, map name, continent)
  01_descriptive_statistics.R   numbers of the Results section
  02_code_carte_rond.R          Figure 2 (bubble map)
  03_time_plot.R                Figure 3 (temporal trends) and Poisson p-trends
  04_treemap.R                  Figure 4 (treemap), domain and subdomain counts, domains per study
  05_Supplemental_Figures.R     eFigure 1 (domains before/from 2020), eFigure 2 (domains by region),
                                eFigure 3 (included studies per million PubMed publications)
  run_all.R                     runs all scripts in order
outputs/
  derived_data/  figures/  tables/  logs/
```

## How to run

Set the working directory to the repository root, then run:

```r
source("code/run_all.R")
```

All paths are relative to the repository root. The console output of each script is saved in `outputs/logs/`.

Packages: readxl, tidyverse, writexl, sf, rnaturalearth, rnaturalearthdata, treemapify, scales. Tested with R 4.5.1 (see `outputs/logs/sessionInfo.txt`).

## Definitions

- Instrument, subdomain and domain: `data/thematic_framework.xlsx` (each acronym belongs to one subdomain).
- Domain and subdomain counts are numbers of distinct studies (`N_studies_distinct`, `outputs/tables/04_domain_subdomain_counts_formatted.csv`);
  `Total_Count` in `outputs/tables/domain_counts.csv` is the sum of the subdomain counts (study-subdomain pairs).
- International study: more than one country (`n_countries > 1`).
- Continent of a multi-country study: continent of the first listed country. England, Scotland and Wales are grouped as United Kingdom.
- Periods: studies published before 2020 vs from 2020 to 2025. 
- eFigure 3: included studies of the year / PubMed records of the same year, per million PubMed publications (square-root scale).

## License

- Code (`code/`): MIT License, see `LICENSE`.
- Data (`data/`) and outputs (`outputs/`): Creative Commons Attribution 4.0 International (CC BY 4.0), see `LICENSE-DATA`.
