# Figure 4: thematic domains and subdomains (treemap)

library(readxl)
library(tidyverse)
library(treemapify)
library(grid)

df_studies <- read_excel("data/data_extraction_sheet.xlsx", sheet = "Data extraction sheet")
df_domains <- read_excel("data/thematic_framework.xlsx", sheet = 1)

N_studies <- nrow(df_studies)

studies_long <- df_studies %>%
  select(study_id, starts_with("scale_")) %>%
  pivot_longer(starts_with("scale_"), names_to = "scale_col_index", values_to = "scale_acronym", values_drop_na = TRUE) %>%
  mutate(scale_acronym = str_to_upper(str_trim(scale_acronym)))

domains_map <- df_domains %>%
  fill(Domain, .direction = "down") %>%
  select(Domain, Subdomain, starts_with("scale_")) %>%
  pivot_longer(starts_with("scale_"), names_to = "scale_col_index", values_to = "scale_acronym", values_drop_na = TRUE) %>%
  mutate(scale_acronym = str_to_upper(str_trim(scale_acronym))) %>%
  distinct(Domain, Subdomain, scale_acronym)

# scales not found in the framework
studies_long %>%
  anti_join(domains_map, by = "scale_acronym") %>%
  count(scale_acronym, sort = TRUE)

joined <- studies_long %>%
  left_join(domains_map, by = "scale_acronym", relationship = "many-to-many") %>%
  filter(!is.na(Domain))

# I. Subdomains ----
subdomain_counts <- joined %>%
  group_by(Subdomain) %>%
  summarise(Total_Count = n_distinct(study_id), .groups = "drop") %>%
  arrange(desc(Total_Count))

print(subdomain_counts, n = 40)
write_csv2(subdomain_counts, "outputs/tables/subdomain_counts.csv")

# II. Domains ----
# Total_Count = sum of the subdomain counts, N_studies_distinct = number of studies
domain_counts <- joined %>%
  group_by(Domain, Subdomain) %>%
  summarise(Subdomain_Count = n_distinct(study_id), .groups = "drop") %>%
  group_by(Domain) %>%
  summarise(Total_Count = sum(Subdomain_Count), .groups = "drop") %>%
  left_join(joined %>% group_by(Domain) %>% summarise(N_studies_distinct = n_distinct(study_id)), by = "Domain") %>%
  arrange(desc(Total_Count))

domain_counts
write_csv2(domain_counts, "outputs/tables/domain_counts.csv")

domain_subdomain_formatted <- joined %>%
  group_by(Domain, Subdomain) %>%
  summarise(n = n_distinct(study_id), .groups = "drop") %>%
  bind_rows(domain_counts %>% transmute(Domain, n = N_studies_distinct, Subdomain = "(any subdomain - distinct studies)")) %>%
  mutate(pct = 100 * n / N_studies,
         manuscript = sprintf("%d/%d, %s", n, N_studies, ifelse(pct < 1, sprintf("%.1f%%", pct), sprintf("%.0f%%", pct)))) %>%
  arrange(Domain, desc(Subdomain == "(any subdomain - distinct studies)"), desc(n))

print(domain_subdomain_formatted, n = 40)
write_csv(domain_subdomain_formatted, "outputs/tables/04_domain_subdomain_counts_formatted.csv")

# III. Treemap ----
plot_data <- joined %>%
  filter(!is.na(Domain), !is.na(Subdomain)) %>%
  group_by(Domain, Subdomain) %>%
  summarise(Count = n_distinct(study_id), .groups = "drop") %>%
  filter(Count > 0)

bmj_palette <- c("#005689", "#E04E39", "#0096C7", "#F0AB00", "#6F7276", "#2D6A4F", "#8C1D40")

gg_treemap <- ggplot(plot_data, aes(area = Count, fill = Domain, label = Subdomain, subgroup = Domain)) +
  geom_treemap(
    aes(alpha = Count),
    layout  = "squarified",
    colour  = "white",
    size    = 2,
    radius  = unit(6, "pt")
  ) +
  scale_fill_manual(values = bmj_palette) +
  scale_alpha_continuous(range = c(0.6, 1), guide = "none") +
  geom_treemap_text(
    colour   = "white",
    place    = "center",
    grow     = FALSE,
    reflow   = TRUE,
    family   = "sans",
    fontface = "bold",
    min.size = 1
  ) +
  theme_void() +
  theme(
    plot.margin      = margin(10, 10, 10, 10),
    legend.position  = "none"
  )

print(gg_treemap)
ggsave("outputs/figures/treemap_clean_manual_edit.pdf", plot = gg_treemap, width = 12, height = 9, device = cairo_pdf)

# IV. Domains and subdomains per study ----
per_study <- studies_long %>%
  left_join(domains_map, by = "scale_acronym", relationship = "many-to-many") %>%
  group_by(study_id) %>%
  summarise(
    n_domains    = n_distinct(Domain, na.rm = TRUE),
    n_subdomains = n_distinct(Subdomain, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    n_domains    = if_else(n_domains == 0, 1L, n_domains),
    n_subdomains = if_else(n_subdomains == 0, 1L, n_subdomains)
  )

nrow(per_study)
write_csv(per_study, "outputs/tables/04_domains_subdomains_per_study.csv")

per_study %>%
  summarise(
    domains    = sprintf("%.0f [%.0f-%.0f]", median(n_domains), quantile(n_domains, 0.25), quantile(n_domains, 0.75)),
    subdomains = sprintf("%.0f [%.0f-%.0f]", median(n_subdomains), quantile(n_subdomains, 0.25), quantile(n_subdomains, 0.75))
  )

# V. Studies with a single domain / subdomain ----
per_study %>%
  summarise(
    n_total          = n(),
    single_domain    = sprintf("%d/%d (%.1f%%)", sum(n_domains == 1), n(), mean(n_domains == 1) * 100),
    single_subdomain = sprintf("%d/%d (%.1f%%)", sum(n_subdomains == 1), n(), mean(n_subdomains == 1) * 100)
  )

# psychological symptoms only
joined %>%
  group_by(study_id) %>%
  summarise(psy_only = all(Domain == "Psychological Symptoms & Distress")) %>%
  summarise(n = sum(psy_only))
