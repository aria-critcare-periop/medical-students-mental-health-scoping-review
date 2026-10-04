# Supplemental figures
# eFigure 1: domains before vs from 2020
# eFigure 2: domains by world region
# eFigure 3: included studies per million PubMed publications

library(readxl)
library(tidyverse)
library(scales)

df_raw       <- read_excel("data/data_extraction_sheet.xlsx", sheet = "Data extraction sheet")
data_country <- read_excel("outputs/derived_data/data_country.xlsx")

domains_map <- read_excel("data/thematic_framework.xlsx", sheet = 1) %>%
  fill(Domain, .direction = "down") %>%
  pivot_longer(starts_with("scale_"), values_to = "scale_clean", values_drop_na = TRUE) %>%
  mutate(scale_clean = str_to_upper(str_trim(scale_clean))) %>%
  distinct(scale_clean, domain = Domain)

# continent of the first listed country
df <- df_raw %>%
  left_join(data_country %>% filter(country_rank == 1) %>% select(study_id, continent), by = "study_id") %>%
  mutate(period = ifelse(year < 2020, "Pre-COVID (before 2020)", "COVID-era (2020+)"))

table(df$continent)

df_long <- df %>%
  select(study_id, year, continent, period, starts_with("scale_")) %>%
  pivot_longer(starts_with("scale_"), names_to = "scale_num", values_to = "scale_name", values_drop_na = TRUE) %>%
  mutate(scale_clean = str_to_upper(str_trim(scale_name))) %>%
  left_join(domains_map, by = "scale_clean") %>%
  filter(!is.na(domain))

table(df_long$domain)

domain_order <- c(
  "Psychological Symptoms & Distress",
  "Physical Health & Lifestyle",
  "Academic Environment",
  "Psychological Resources",
  "Social Support"
)

domain_colors <- c(
  "Psychological Symptoms & Distress" = "#E63946",
  "Physical Health & Lifestyle"       = "#2A9D8F",
  "Academic Environment"              = "#F4A261",
  "Psychological Resources"           = "#457B9D",
  "Social Support"                    = "#8338EC"
)

# eFigure 2: domains x continents ----
n_by_continent_all <- df %>%
  count(continent, name = "n_total")

df_domain_continent <- df_long %>%
  group_by(continent, domain) %>%
  summarise(n_studies = n_distinct(study_id), .groups = "drop") %>%
  left_join(n_by_continent_all, by = "continent") %>%
  mutate(pct = n_studies / n_total * 100)

continent_order <- n_by_continent_all %>%
  arrange(desc(n_total)) %>%
  pull(continent)

continent_labels <- setNames(paste0(n_by_continent_all$continent, "\n(n=", n_by_continent_all$n_total, ")"),
                             n_by_continent_all$continent)

df_domain_continent_plot <- df_domain_continent %>%
  mutate(
    continent = factor(continent, levels = continent_order),
    domain    = factor(domain, levels = rev(domain_order))
  )

p1_new <- ggplot(df_domain_continent_plot, aes(x = continent, y = pct, fill = domain)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.92) +
  geom_text(
    aes(label = paste0(round(pct, 0), "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.4, size = 2.6, fontface = "bold"
  ) +
  scale_fill_manual(values = domain_colors, name = NULL) +
  scale_x_discrete(labels = continent_labels) +
  scale_y_continuous(limits = c(0, 110), labels = label_percent(scale = 1), breaks = seq(0, 100, 25)) +
  labs(x = NULL, y = "% of studies assessing this domain") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    legend.text     = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    axis.text.x     = element_text(size = 9, lineheight = 1.2)
  )

ggsave("outputs/figures/Figure_A_domains_x_regions.png", p1_new, width = 12, height = 6, dpi = 300, bg = "white")

# eFigure 1: domains x period ----
n_per_period <- df %>%
  count(period, name = "n_total")

df_domain_period <- df_long %>%
  group_by(period, domain) %>%
  summarise(n_studies = n_distinct(study_id), .groups = "drop") %>%
  left_join(n_per_period, by = "period") %>%
  mutate(pct = n_studies / n_total * 100,
         period = factor(period, levels = c("Pre-COVID (before 2020)", "COVID-era (2020+)")))

# chi-square test for each domain
pval_results <- tibble(domain = domain_order, p_value = NA_real_)
for (i in 1:5) {
  has_domain <- df$study_id %in% df_long$study_id[df_long$domain == domain_order[i]]
  pval_results$p_value[i] <- chisq.test(table(df$period, has_domain))$p.value
}

pval_results <- pval_results %>%
  mutate(p_label = paste(ifelse(p_value < 0.001, "p<0.001", paste("p =", round(p_value, 3))),
                         case_when(p_value < 0.001 ~ "***", p_value < 0.01 ~ "**", p_value < 0.05 ~ "*", TRUE ~ "")))
pval_results

p2 <- ggplot(df_domain_period, aes(x = reorder(domain, pct), y = pct, fill = period)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65, alpha = 0.92) +
  geom_text(aes(label = paste0(round(pct, 0), "%")),
            position = position_dodge(width = 0.75),
            hjust = -0.1, size = 2.8, fontface = "bold") +
  geom_text(
    data = pval_results %>% mutate(domain = factor(domain)),
    aes(x = domain, y = 103, label = p_label),
    inherit.aes = FALSE,
    hjust    = 0,
    size     = 2.8,
    color    = ifelse(pval_results$p_value < 0.05, "#C1121F", "black"),
    fontface = ifelse(pval_results$p_value < 0.05, "bold", "plain")
  ) +
  scale_fill_manual(
    values = c("Pre-COVID (before 2020)" = "#457B9D", "COVID-era (2020+)" = "#E63946"),
    name = NULL
  ) +
  scale_y_continuous(limits = c(0, 130), breaks = seq(0, 100, 25), labels = label_percent(scale = 1)) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "% of studies assessing this domain") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "top",
    panel.grid.major.y = element_blank(),
    plot.margin        = margin(t = 5, r = 80, b = 5, l = 5)
  )

ggsave("outputs/figures/Figure_B_domains_temporal.png", p2, width = 12, height = 6, dpi = 300, bg = "white")

# eFigure 3: studies per million PubMed publications ----
# PubMed records per publication year (query "YYYY[dp]"), downloaded once from the NCBI E-utilities
if (!file.exists("data/pubmed_records_per_year.csv")) {
  pubmed <- tibble(year = 1980:2025, pubmed_records = NA_real_, query = "YYYY[dp]", retrieved_on = as.character(Sys.Date()))
  for (i in 1:nrow(pubmed)) {
    url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&rettype=count&retmode=json&term=",
                  pubmed$year[i], "%5Bdp%5D")
    pubmed$pubmed_records[i] <- as.numeric(jsonlite::fromJSON(url)$esearchresult$count)
    Sys.sleep(0.4)
  }
  write_csv(pubmed, "data/pubmed_records_per_year.csv")
}

pubmed <- read_csv("data/pubmed_records_per_year.csv")

df_pubmed_adj <- df %>%
  count(year, name = "n_studies") %>%
  complete(year = 1980:2025, fill = list(n_studies = 0)) %>%
  left_join(pubmed %>% select(year, pubmed_records), by = "year") %>%
  mutate(studies_per_million_pubmed = n_studies / pubmed_records * 1e6)

p4 <- ggplot(df_pubmed_adj, aes(x = year, y = studies_per_million_pubmed)) +
  geom_line(color = "#005689", linewidth = 1.2) +
  geom_point(color = "#005689", size = 2, shape = 21, fill = "white", stroke = 1.2) +
  scale_x_continuous(breaks = seq(1980, 2025, by = 5)) +
  scale_y_sqrt(breaks = c(0, 5, 10, 20, 30, 40, 50)) +
  theme_classic() +
  theme(
    text = element_text(family = "sans"),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14, color = "black"),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5)
  ) +
  labs(
    x = "Year of Publication",
    y = "Studies per million PubMed publications\n(square-root scale)"
  )

ggsave("outputs/figures/Figure_D_studies_adjusted_pubmed.png", p4, width = 10, height = 6, dpi = 300, bg = "white")
print(df_pubmed_adj, n = Inf)

# Tables ----
write_csv(df_domain_period %>% arrange(domain, period) %>% left_join(pval_results, by = "domain"),
          "outputs/tables/Table_domains_by_period.csv")
write_csv(df_domain_continent %>% arrange(continent, domain), "outputs/tables/Table_domains_by_region.csv")
write_csv(df_pubmed_adj, "outputs/tables/Table_studies_adjusted_pubmed.csv")
