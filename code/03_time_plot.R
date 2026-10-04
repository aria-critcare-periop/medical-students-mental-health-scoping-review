# Figure 3: temporal trends 2000-2025 and Poisson p-trends

library(readxl)
library(tidyverse)

df <- read_excel("data/data_extraction_sheet.xlsx", sheet = "Data extraction sheet")

legend_order <- c(
  "Total Studies",
  "Multicenter Studies",
  "International Studies",
  "Sex Bias Assessment",
  "Racial Bias Assessment",
  "Gender & Sexual Orientation Bias Assessment",
  "Socioeconomic Bias Assessment"
)

df_agg <- df %>%
  filter(year >= 2000 & year <= 2025) %>%
  group_by(year) %>%
  summarise(
    `Total Studies` = n(),
    `Multicenter Studies` = sum(multicenter == 1, na.rm = TRUE),
    `International Studies` = sum(n_countries > 1, na.rm = TRUE),
    `Sex Bias Assessment` = sum(sex_bias == 1, na.rm = TRUE),
    `Gender & Sexual Orientation Bias Assessment` = sum(gender_bias == 1 | sexual_orientation_bias == 1, na.rm = TRUE),
    `Racial Bias Assessment` = sum(racial_bias == 1, na.rm = TRUE),
    `Socioeconomic Bias Assessment` = sum(socioeconomic_bias == 1, na.rm = TRUE)
  )
write_csv(df_agg, "outputs/tables/03_counts_per_year.csv")

plot_data <- df_agg %>%
  pivot_longer(cols = -year, names_to = "Category", values_to = "Count") %>%
  mutate(Category = factor(Category, levels = legend_order))

bmj_colors <- c(
  "Total Studies" = "black",
  "Multicenter Studies" = "#005689",
  "International Studies" = "#0096C7",
  "Sex Bias Assessment" = "#E04E39",
  "Racial Bias Assessment" = "#2D6A4F",
  "Gender & Sexual Orientation Bias Assessment" = "#F0AB00",
  "Socioeconomic Bias Assessment" = "#7B4F9E"
)

line_types <- c(
  "Total Studies" = "solid",
  "Multicenter Studies" = "solid",
  "International Studies" = "solid",
  "Sex Bias Assessment" = "45",
  "Racial Bias Assessment" = "45",
  "Gender & Sexual Orientation Bias Assessment" = "45",
  "Socioeconomic Bias Assessment" = "45"
)

main_font <- "sans" # Arial

gg_trend_ordered <- ggplot(plot_data, aes(x = year, y = Count, color = Category, linetype = Category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(values = bmj_colors) +
  scale_linetype_manual(values = line_types) +
  scale_x_continuous(breaks = seq(2000, 2024, by = 4)) +
  theme_classic() +
  theme(
    text = element_text(family = main_font),
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 14, color = "black"),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
    legend.position = "inside",
    legend.position.inside = c(0.35, 0.75),
    legend.background = element_rect(fill = alpha("white", 0.9), color = NA),
    legend.key.width = unit(1.5, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  labs(
    x = "Year of Publication",
    y = "Number of Studies"
  )

print(gg_trend_ordered)
ggsave("outputs/figures/trend_ordered_arial.pdf", plot = gg_trend_ordered, width = 10, height = 6, device = cairo_pdf)

# P-trends: yearly count ~ year, Poisson ----
p_trend <- function(category) {
  model <- glm(Count ~ year, data = filter(plot_data, Category == category), family = poisson(link = "log"))
  tibble(Category = category,
         IRR_per_year = round(exp(coef(model)["year"]), 3),
         p_trend = summary(model)$coefficients["year", "Pr(>|z|)"])
}

ptrend_counts <- bind_rows(lapply(legend_order, p_trend)) %>%
  mutate(p_trend_fmt = case_when(
    p_trend < 0.001 ~ "<0.001",
    p_trend < 0.01  ~ sprintf("%.3f", p_trend),
    TRUE            ~ sprintf("%.2f", p_trend)
  ))

ptrend_counts
write_csv(ptrend_counts, "outputs/tables/03_ptrend_poisson.csv")
