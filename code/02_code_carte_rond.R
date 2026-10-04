# Figure 2: geographic distribution of the included studies (bubble map)

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(grid)

data <- read_excel("data/data_extraction_sheet.xlsx", sheet = "Data extraction sheet")
country_lookup <- read.csv("outputs/derived_data/country_lookup.csv", na.strings = "")

# England, Scotland and Wales are counted once as United Kingdom
country_long <- data %>%
  separate_rows(country, sep = ",") %>%
  mutate(country = str_trim(country)) %>%
  left_join(country_lookup %>% select(country, map_name), by = "country") %>%
  filter(!is.na(map_name)) %>%
  distinct(study_id, country_en = map_name)

country_counts <- country_long %>%
  group_by(country_en) %>%
  summarise(n_studies = n(), .groups = "drop")

print(country_counts, n = 100)
readr::write_csv2(country_counts %>% arrange(desc(n_studies)), "outputs/tables/02_country_counts_map.csv")

world <- ne_countries(scale = "medium", returnclass = "sf")

world_data <- world %>%
  left_join(country_counts, by = c("name" = "country_en"))

setdiff(country_counts$country_en, world$name)

continent_counts <- country_counts %>%
  left_join(distinct(country_lookup, country_en = map_name, continent), by = "country_en") %>%
  group_by(continent) %>%
  summarise(total_studies = sum(n_studies), .groups = "drop") %>%
  arrange(desc(total_studies))

continent_counts
readr::write_csv2(continent_counts, "outputs/tables/02_continent_counts_map.csv")

# Bubble map ----
world_bg <- world_data %>%
  filter(name != "Antarctica")

bubble_sf <- world_bg %>%
  filter(!is.na(n_studies) & n_studies > 0) %>%
  mutate(
    n_size = pmin(n_studies, 50),
    geometry = st_point_on_surface(geometry),
    n_size_disp = scales::rescale(n_size, to = c(3, 36))
  )

p_bubble <- ggplot() +
  geom_sf(data = world_bg,
          aes(fill = !is.na(n_studies)),
          color = "grey70", linewidth = 0.2) +
  scale_fill_manual(values = c("TRUE" = "#c8d8e8", "FALSE" = "#eef2f6"), guide = "none") +
  geom_sf(
    data = bubble_sf,
    aes(size = n_size_disp, color = n_studies),
    alpha = 0.9
  ) +
  scale_color_gradient(
    name = "Number of studies",
    low  = "#56B1F7",
    high = "#132B43",
    limits = c(0, 50),
    oob = scales::squish
  ) +
  scale_size_identity(guide = "none") +
  coord_sf(expand = FALSE) +
  labs(title = "Geographic distribution of included studies") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.key.width = unit(1.6, "cm")
  ) +
  guides(color = guide_colorbar(order = 1))

print(p_bubble)
ggsave("outputs/figures/Figure_2_bubble_map.pdf", p_bubble, width = 14, height = 8, device = cairo_pdf)
