# Numbers reported in the Results section

library(readxl)
library(tidyverse)

data         <- read_excel("data/data_extraction_sheet.xlsx", sheet = "Data extraction sheet")
data_country <- read_excel("outputs/derived_data/data_country.xlsx")
domains      <- read_excel("data/thematic_framework.xlsx", sheet = 1)
catalog      <- read_excel("data/scale_catalogue.xlsx", sheet = "Scale details")

N <- nrow(data)

n_pct <- function(item, n, d = N) {
  pct <- 100 * n / d
  tibble(item, n, N = d,
         text = sprintf("%d/%d (%s)", n, d, ifelse(pct < 1, sprintf("%.1f%%", pct), sprintf("%.0f%%", pct))))
}
value <- function(item, n, text = format(n, big.mark = ",")) {
  tibble(item, n, N = NA, text)
}
med_iqr <- function(x) sprintf("%.0f [%.0f-%.0f]", median(x), quantile(x, 0.25), quantile(x, 0.75))

# Characteristics ----
countries <- data_country %>% filter(!is.na(country_en)) %>% distinct(country_en, un_state)

characteristics <- bind_rows(
  value("Included studies", N),
  value("Total number of students", sum(data$size)),
  value("Students per study, median [IQR]", median(data$size), med_iqr(data$size)),
  value("Countries/territories represented (UK nations grouped)", nrow(countries)),
  value("  of which UN member/observer states", sum(countries$un_state)),
  value("  of which territories (Hong Kong, Puerto Rico)", sum(!countries$un_state)),
  value("UN member/observer states not represented (out of 195)", 195 - sum(countries$un_state)),
  value("Continents represented", n_distinct(data_country$continent)),
  n_pct("Cross-sectional (longitudinal = 0)", sum(data$longitudinal == 0)),
  n_pct("Longitudinal", sum(data$longitudinal == 1)),
  n_pct("Observational (interventional = 0)", sum(data$interventional == 0)),
  n_pct("Interventional", sum(data$interventional == 1)),
  n_pct("Single-centre (multicenter = 0)", sum(data$multicenter == 0)),
  n_pct("Multicentre", sum(data$multicenter == 1)),
  n_pct("Single-country (n_countries = 1)", sum(data$n_countries == 1)),
  n_pct("International (n_countries > 1)", sum(data$n_countries > 1))
)

# Geography ----
country_table <- data_country %>%
  filter(!is.na(country_en)) %>%
  distinct(study_id, country_en, continent, n_countries) %>%
  group_by(country_en, continent) %>%
  summarise(n_single_country = sum(n_countries == 1),
            n_all_studies = n(), .groups = "drop") %>%
  mutate(pct_single_country = 100 * n_single_country / N) %>%
  arrange(desc(n_single_country), desc(n_all_studies), country_en)
write_csv(country_table, "outputs/tables/01_country_counts.csv")

top5 <- head(country_table, 5)

# continent of the first listed country
continent_first <- data_country %>%
  filter(country_rank == 1) %>%
  count(continent, name = "n_studies") %>%
  mutate(pct = 100 * n_studies / N) %>%
  arrange(desc(n_studies))
write_csv(continent_first, "outputs/tables/01_continent_counts_first_country.csv")

geography <- bind_rows(
  n_pct(paste("Top 5 country:", top5$country_en), top5$n_single_country),
  n_pct("Five leading countries together", sum(top5$n_single_country)),
  n_pct("Remaining studies (all other countries and multi-country studies)", N - sum(top5$n_single_country)),
  value("Number of remaining countries/territories", nrow(countries) - 5),
  n_pct(paste("Continent (first listed country):", continent_first$continent), continent_first$n_studies)
)

# Time ----
year_table <- data %>% count(year, name = "n_studies") %>% mutate(pct = 100 * n_studies / N)
write_csv(year_table, "outputs/tables/01_studies_per_year.csv")

temporal <- bind_rows(
  n_pct("Published 1980-1999", sum(data$year <= 1999)),
  n_pct("Published 2000-2019", sum(data$year >= 2000 & data$year <= 2019)),
  n_pct("Published from 2020 onwards", sum(data$year >= 2020)),
  n_pct("Conducted during the COVID-19 pandemic (covid = 1)", sum(data$covid == 1)),
  value("First / last publication year", min(data$year), paste0(min(data$year), "-", max(data$year)))
)

period <- ifelse(data$year < 2020, "Before 2020", "From 2020")

design_test <- function(characteristic, x) {
  pre <- x[period == "Before 2020"]
  post <- x[period == "From 2020"]
  test <- chisq.test(table(period, x))
  tibble(characteristic,
         before_2020 = sprintf("%d/%d (%.0f%%)", sum(pre), length(pre), 100 * sum(pre) / length(pre)),
         from_2020 = sprintf("%d/%d (%.0f%%)", sum(post), length(post), 100 * sum(post) / length(post)),
         chi2 = unname(test$statistic),
         p_value = test$p.value,
         p_fmt = ifelse(test$p.value < 0.001, "<0.001", sprintf("%.2f", test$p.value)))
}

design_by_period <- bind_rows(
  design_test("Cross-sectional", data$longitudinal == 0),
  design_test("Observational", data$interventional == 0),
  design_test("Single-centre", data$multicenter == 0)
)
design_by_period
write_csv(design_by_period, "outputs/tables/01_design_by_period.csv")

# Instruments ----
scale_use <- data %>%
  select(study_id, starts_with("scale_")) %>%
  pivot_longer(starts_with("scale_"), values_to = "scale_acronym", values_drop_na = TRUE) %>%
  mutate(scale_acronym = str_trim(scale_acronym)) %>%
  distinct(study_id, scale_acronym) %>%
  count(scale_acronym, name = "n_studies", sort = TRUE)
write_csv(scale_use, "outputs/tables/01_scale_frequency.csv")

nb_scales_dist <- data %>% count(nb_scales, name = "n_studies") %>% mutate(pct = 100 * n_studies / N)
write_csv(nb_scales_dist, "outputs/tables/01_nb_scales_distribution.csv")

domains_map <- domains %>%
  fill(Domain, .direction = "down") %>%
  pivot_longer(starts_with("scale_"), values_to = "scale_acronym", values_drop_na = TRUE) %>%
  mutate(scale_acronym = str_to_upper(str_trim(scale_acronym))) %>%
  distinct(scale_acronym, Domain)

top20_domains <- scale_use %>%
  slice_max(n_studies, n = 20) %>%
  mutate(scale_acronym = str_to_upper(scale_acronym)) %>%
  left_join(domains_map, by = "scale_acronym") %>%
  count(Domain, name = "n_scales", sort = TRUE)
top20_domains

instruments <- bind_rows(
  value("Distinct instruments used in the included studies", nrow(scale_use)),
  value("Instruments listed in the scale catalogue", nrow(catalog)),
  value("Items per study, median [IQR]", median(data$n_questions), med_iqr(data$n_questions)),
  value("Items per study, range", max(data$n_questions), paste0(min(data$n_questions), "-", max(data$n_questions))),
  n_pct("20 items or fewer", sum(data$n_questions <= 20)),
  n_pct("21 to 99 items", sum(data$n_questions >= 21 & data$n_questions <= 99)),
  n_pct("100 items or more", sum(data$n_questions >= 100)),
  n_pct("1 instrument", sum(data$nb_scales == 1)),
  n_pct("2 instruments", sum(data$nb_scales == 2)),
  n_pct("3 instruments", sum(data$nb_scales == 3)),
  n_pct("4 to 6 instruments", sum(data$nb_scales >= 4 & data$nb_scales <= 6)),
  n_pct("7 or more instruments", sum(data$nb_scales >= 7)),
  value("Instruments per study, median [IQR]", median(data$nb_scales), med_iqr(data$nb_scales)),
  n_pct(paste("Top-20 scales in domain:", top20_domains$Domain), top20_domains$n_scales, sum(top20_domains$n_scales))
)

# Equity ----
n_equity <- data$sex_bias + data$gender_bias + data$sexual_orientation_bias + data$racial_bias + data$socioeconomic_bias

equity <- bind_rows(
  n_pct("Sex", sum(data$sex_bias == 1)),
  n_pct("Race or ethnicity", sum(data$racial_bias == 1)),
  n_pct("Socioeconomic status", sum(data$socioeconomic_bias == 1)),
  n_pct("Gender identity", sum(data$gender_bias == 1)),
  n_pct("Sexual orientation", sum(data$sexual_orientation_bias == 1)),
  n_pct("Gender identity or sexual orientation", sum(data$gender_bias == 1 | data$sexual_orientation_bias == 1)),
  n_pct("No equity-related variable used", sum(n_equity == 0)),
  n_pct("Two or more equity-related variables used", sum(n_equity >= 2))
)

results <- bind_rows(Characteristics = characteristics, Geography = geography, Temporal = temporal,
                     Instruments = instruments, Equity = equity, .id = "section")
print(results, n = Inf)
write_csv(results, "outputs/tables/01_descriptive_statistics.csv")
