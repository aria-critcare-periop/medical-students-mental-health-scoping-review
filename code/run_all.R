# Runs all the analyses from the repository root. Console output is saved in outputs/logs/

scripts <- c("00_data_management.R",
             "01_descriptive_statistics.R",
             "02_code_carte_rond.R",
             "03_time_plot.R",
             "04_treemap.R",
             "05_Supplemental_Figures.R")

dir.create("outputs/logs", recursive = TRUE, showWarnings = FALSE)
pdf(NULL)

for (s in scripts) {
  sink(file.path("outputs/logs", sub("\\.R$", ".log", s)))
  source(file.path("code", s), print.eval = TRUE)
  sink()
}

dev.off()
writeLines(capture.output(sessionInfo()), "outputs/logs/sessionInfo.txt")
