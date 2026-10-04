# Data management: checks of the extraction sheet and country table

library(readxl)
library(tidyverse)
library(writexl)
library(rnaturalearth)

data    <- read_excel("data/data_extraction_sheet.xlsx", sheet = "Data extraction sheet")
domains <- read_excel("data/thematic_framework.xlsx", sheet = 1)
catalog <- read_excel("data/scale_catalogue.xlsx", sheet = "Scale details")

dir.create("outputs/derived_data", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

dim(data)

# Checks ----
scale_cols <- paste0("scale_", 1:15)
binary_vars <- c("longitudinal", "interventional", "multicenter", "covid", "sex_bias", "gender_bias",
                 "sexual_orientation_bias", "racial_bias", "socioeconomic_bias")

all(as.matrix(data[binary_vars]) %in% c(0, 1))

# Scales ----
domains_map <- domains %>%
  fill(Domain, .direction = "down") %>%
  pivot_longer(starts_with("scale_"), values_to = "scale_acronym", values_drop_na = TRUE) %>%
  mutate(scale_acronym = str_to_upper(str_trim(scale_acronym))) %>%
  distinct(Domain, Subdomain, scale_acronym)

studies_long <- data %>%
  select(study_id, all_of(scale_cols)) %>%
  pivot_longer(all_of(scale_cols), values_to = "scale_acronym", values_drop_na = TRUE) %>%
  mutate(scale_acronym = str_to_upper(str_trim(scale_acronym)))

# used in the studies but not in the framework
studies_long %>% anti_join(domains_map, by = "scale_acronym") %>% count(scale_acronym)

# in more than one subdomain
domains_map %>% count(scale_acronym) %>% filter(n > 1)

# framework vs catalogue
catalog_acronyms <- str_to_upper(str_trim(catalog$Acronym))
setdiff(domains_map$scale_acronym, catalog_acronyms)
setdiff(catalog_acronyms, domains_map$scale_acronym)

catalog %>%
  mutate(scale_acronym = str_to_upper(str_trim(Acronym))) %>%
  inner_join(domains_map, by = "scale_acronym") %>%
  filter(`Domain (thematic framework)` != Domain | `Subdomain (thematic framework)` != Subdomain) %>%
  nrow()

# Countries ----
country_lookup <- tribble(
  ~country,                              ~country_en,              ~map_name,                  ~continent,      ~un_state,
  # North America
  "United States",                       "United States",          "United States of America", "North America", TRUE,
  "Canada",                              "Canada",                 "Canada",                   "North America", TRUE,
  "Mexico",                              "Mexico",                 "Mexico",                   "North America", TRUE,
  "Puerto Rico",                         "Puerto Rico",            "Puerto Rico",              "North America", FALSE,
  "Trinidad and Tobago",                 "Trinidad and Tobago",    "Trinidad and Tobago",      "North America", TRUE,
  # South America
  "Brazil",                              "Brazil",                 "Brazil",                   "South America", TRUE,
  "Chile",                               "Chile",                  "Chile",                    "South America", TRUE,
  "Colombia",                            "Colombia",               "Colombia",                 "South America", TRUE,
  "Paraguay",                            "Paraguay",               "Paraguay",                 "South America", TRUE,
  "Peru",                                "Peru",                   "Peru",                     "South America", TRUE,
  "Venezuela",                           "Venezuela",              "Venezuela",                "South America", TRUE,
  # Europe
  "Austria",                             "Austria",                "Austria",                  "Europe",        TRUE,
  "Belarus",                             "Belarus",                "Belarus",                  "Europe",        TRUE,
  "Bosnia and Herzegovina",              "Bosnia and Herzegovina", "Bosnia and Herz.",         "Europe",        TRUE,
  "Croatia",                             "Croatia",                "Croatia",                  "Europe",        TRUE,
  "Cyprus",                              "Cyprus",                 "Cyprus",                   "Europe",        TRUE,
  "Czechia",                             "Czechia",                "Czechia",                  "Europe",        TRUE,
  "Denmark",                             "Denmark",                "Denmark",                  "Europe",        TRUE,
  "France",                              "France",                 "France",                   "Europe",        TRUE,
  "Germany",                             "Germany",                "Germany",                  "Europe",        TRUE,
  "Greece",                              "Greece",                 "Greece",                   "Europe",        TRUE,
  "Hungary",                             "Hungary",                "Hungary",                  "Europe",        TRUE,
  "Ireland",                             "Ireland",                "Ireland",                  "Europe",        TRUE,
  "Italy",                               "Italy",                  "Italy",                    "Europe",        TRUE,
  "Latvia",                              "Latvia",                 "Latvia",                   "Europe",        TRUE,
  "Lithuania",                           "Lithuania",              "Lithuania",                "Europe",        TRUE,
  "Netherlands",                         "Netherlands",            "Netherlands",              "Europe",        TRUE,
  "Norway",                              "Norway",                 "Norway",                   "Europe",        TRUE,
  "Poland",                              "Poland",                 "Poland",                   "Europe",        TRUE,
  "Portugal",                            "Portugal",               "Portugal",                 "Europe",        TRUE,
  "Romania",                             "Romania",                "Romania",                  "Europe",        TRUE,
  "Russia",                              "Russia",                 "Russia",                   "Europe",        TRUE,
  "Serbia",                              "Serbia",                 "Serbia",                   "Europe",        TRUE,
  "Slovakia",                            "Slovakia",               "Slovakia",                 "Europe",        TRUE,
  "Slovenia",                            "Slovenia",               "Slovenia",                 "Europe",        TRUE,
  "Spain",                               "Spain",                  "Spain",                    "Europe",        TRUE,
  "Sweden",                              "Sweden",                 "Sweden",                   "Europe",        TRUE,
  "Switzerland",                         "Switzerland",            "Switzerland",              "Europe",        TRUE,
  "Ukraine",                             "Ukraine",                "Ukraine",                  "Europe",        TRUE,
  "United Kingdom",                      "United Kingdom",         "United Kingdom",           "Europe",        TRUE,
  "England",                             "United Kingdom",         "United Kingdom",           "Europe",        TRUE,
  "Scotland",                            "United Kingdom",         "United Kingdom",           "Europe",        TRUE,
  "Wales",                               "United Kingdom",         "United Kingdom",           "Europe",        TRUE,
  # Asia
  "Bahrain",                             "Bahrain",                "Bahrain",                  "Asia",          TRUE,
  "Bangladesh",                          "Bangladesh",             "Bangladesh",               "Asia",          TRUE,
  "China",                               "China",                  "China",                    "Asia",          TRUE,
  "Georgia",                             "Georgia",                "Georgia",                  "Asia",          TRUE,
  "Hong Kong",                           "Hong Kong",              "Hong Kong",                "Asia",          FALSE,
  "India",                               "India",                  "India",                    "Asia",          TRUE,
  "Indonesia",                           "Indonesia",              "Indonesia",                "Asia",          TRUE,
  "Iran",                                "Iran",                   "Iran",                     "Asia",          TRUE,
  "Iraq",                                "Iraq",                   "Iraq",                     "Asia",          TRUE,
  "Israel",                              "Israel",                 "Israel",                   "Asia",          TRUE,
  "Japan",                               "Japan",                  "Japan",                    "Asia",          TRUE,
  "Jordan",                              "Jordan",                 "Jordan",                   "Asia",          TRUE,
  "Kazakhstan",                          "Kazakhstan",             "Kazakhstan",               "Asia",          TRUE,
  "Lebanon",                             "Lebanon",                "Lebanon",                  "Asia",          TRUE,
  "Malaysia",                            "Malaysia",               "Malaysia",                 "Asia",          TRUE,
  "Nepal",                               "Nepal",                  "Nepal",                    "Asia",          TRUE,
  "Oman",                                "Oman",                   "Oman",                     "Asia",          TRUE,
  "Pakistan",                            "Pakistan",               "Pakistan",                 "Asia",          TRUE,
  "Palestine",                           "Palestine",              "Palestine",                "Asia",          TRUE,
  "Qatar",                               "Qatar",                  "Qatar",                    "Asia",          TRUE,
  "Saudi Arabia",                        "Saudi Arabia",           "Saudi Arabia",             "Asia",          TRUE,
  "South Korea",                         "South Korea",            "South Korea",              "Asia",          TRUE,
  "Sri Lanka",                           "Sri Lanka",              "Sri Lanka",                "Asia",          TRUE,
  "Syria",                               "Syria",                  "Syria",                    "Asia",          TRUE,
  "Thailand",                            "Thailand",               "Thailand",                 "Asia",          TRUE,
  "Turkey",                              "Turkey",                 "Turkey",                   "Asia",          TRUE,
  "United Arab Emirates",                "United Arab Emirates",   "United Arab Emirates",     "Asia",          TRUE,
  "Vietnam",                             "Vietnam",                "Vietnam",                  "Asia",          TRUE,
  "Yemen",                               "Yemen",                  "Yemen",                    "Asia",          TRUE,
  "unspecified Middle Eastern country",  NA,                       NA,                         "Asia",          NA,
  # Africa
  "Egypt",                               "Egypt",                  "Egypt",                    "Africa",        TRUE,
  "Ethiopia",                            "Ethiopia",               "Ethiopia",                 "Africa",        TRUE,
  "Kenya",                               "Kenya",                  "Kenya",                    "Africa",        TRUE,
  "Libya",                               "Libya",                  "Libya",                    "Africa",        TRUE,
  "Morocco",                             "Morocco",                "Morocco",                  "Africa",        TRUE,
  "Namibia",                             "Namibia",                "Namibia",                  "Africa",        TRUE,
  "Nigeria",                             "Nigeria",                "Nigeria",                  "Africa",        TRUE,
  "Rwanda",                              "Rwanda",                 "Rwanda",                   "Africa",        TRUE,
  "Somalia",                             "Somalia",                "Somalia",                  "Africa",        TRUE,
  "South Africa",                        "South Africa",           "South Africa",             "Africa",        TRUE,
  "Sudan",                               "Sudan",                  "Sudan",                    "Africa",        TRUE,
  "Tanzania",                            "Tanzania",               "Tanzania",                 "Africa",        TRUE,
  "Tunisia",                             "Tunisia",                "Tunisia",                  "Africa",        TRUE,
  "Uganda",                              "Uganda",                 "Uganda",                   "Africa",        TRUE,
  # Oceania
  "Australia",                           "Australia",              "Australia",                "Oceania",       TRUE,
  "New Zealand",                         "New Zealand",            "New Zealand",              "Oceania",       TRUE
)

# one row per study and country, in the order of the extraction sheet
data_country <- data %>%
  select(study_id, title, year, country_list = country, n_countries) %>%
  mutate(country = country_list) %>%
  separate_rows(country, sep = ",") %>%
  mutate(country = str_trim(country)) %>%
  group_by(study_id) %>%
  mutate(country_rank = row_number()) %>%
  ungroup() %>%
  left_join(country_lookup, by = "country")

data_country %>% filter(is.na(continent)) %>% distinct(country)
setdiff(na.omit(country_lookup$map_name), ne_countries(scale = "medium")$name)
n_distinct(na.omit(data_country$country_en))

write.csv(country_lookup, "outputs/derived_data/country_lookup.csv", row.names = FALSE, na = "")
write_xlsx(data_country, "outputs/derived_data/data_country.xlsx")
