# evaluate missingness of variables



# load external packages
packages_all = c("data.table", "haven", "dplyr", "visdat", "ggplot2")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# constants
input_folder <- 'output/rda/'
output_folder <- 'output/'
# data loading
load(paste0(input_folder, 'VARIABLES', '.Rda'))
load(paste0(input_folder, 'ALL-EDITED', '.Rda'))




# missingness assessment
# missingness for non-clustering variables
non_clustering_variables <- as.vector(VARIABLES[missingness_plot == 'non-clustering', 'name', with = FALSE])
dt_non_clustering <- dt[, .SD, .SDcols = non_clustering_variables$name]
non_clustering_missingness_plot <- vis_miss(dt_non_clustering, warn_large_data = FALSE)
ggsave(paste0(output_folder, 'svg/', 'non-clustering-missingness.svg'), non_clustering_missingness_plot, dpi = 300)
# calculate variables with highest missingness
non_clustering_most_missing <- sapply(dt[, .SD, .SDcols = non_clustering_variables$name], function(x) mean(is.na(x)))
non_clustering_most_missing <- sort(non_clustering_most_missing, decreasing = TRUE)
non_clustering_most_missing_names <- names(non_clustering_most_missing)[1:10]
non_clustering_most_missing_values <- round(non_clustering_most_missing[1:10]*100, digits = 2)
print('most missing non-clustering variables:')
for (i in 1:10) {
  print(paste0(i, ': ', non_clustering_most_missing_names[i], ': ', non_clustering_most_missing_values[i], '%'))
}

# misssingness for clustering variables
clustering_variables <- as.vector(VARIABLES[missingness_plot == 'clustering', 'name', with = FALSE])
dt_clustering <- dt[, .SD, .SDcols = clustering_variables$name]
clustering_missingness_plot <- vis_miss(dt_clustering, warn_large_data = FALSE)
ggsave(paste0(output_folder, 'svg/', 'clustering-missingness.svg'), clustering_missingness_plot, dpi = 300)
# calculate variables with highest missingness
clustering_most_missing <- sapply(dt[, .SD, .SDcols = clustering_variables$name], function(x) mean(is.na(x)))
clustering_most_missing <- sort(clustering_most_missing, decreasing = TRUE)
clustering_most_missing_names <- names(clustering_most_missing)[1:10]
clustering_most_missing_values <- round(clustering_most_missing[1:10]*100, digits = 2)
print('most missing clustering variables:')
for (i in 1:10) {
  print(paste0(i, ': ', clustering_most_missing_names[i], ': ', clustering_most_missing_values[i], '%'))
}

# missingness for asthma_physician_diagnosed_age
physician_diagnosed_asthmatics <- dt[asthma_physician_diagnosed == 1, 'asthma_physician_diagnosed_age', with = FALSE]
physician_diagnosed_asthmatics_missingness_plot <- vis_miss(physician_diagnosed_asthmatics, warn_large_data = FALSE)
ggsave(paste0(output_folder, 'svg/', 'physician-diagnosed-asthmatics-missingness.svg'), physician_diagnosed_asthmatics_missingness_plot, dpi = 300)

# missingness for cigarettes_per_day
cigarettes_per_day <- dt[smoking_currently == 1, 'cigarettes_per_day', with = FALSE]
cigarettes_per_day_missingness_plot <- vis_miss(cigarettes_per_day, warn_large_data = FALSE)
ggsave(paste0(output_folder, 'svg/', 'cigarettes-per-day-missingness.svg'), cigarettes_per_day_missingness_plot, dpi = 300)

# missingness for age_start_smoking
start_smoking_age <- dt[smoking_currently == 1 | smoking_previously == 1, 'age_start_smoking', with = FALSE]
start_smoking_age_missingness_plot <- vis_miss(start_smoking_age, warn_large_data = FALSE)
ggsave(paste0(output_folder, 'svg/', 'start-smoking-age-missingness.svg'), start_smoking_age_missingness_plot, dpi = 300)

# missingness for age_quit_smoking
quit_smoking_age <- dt[smoking_previously == 1, 'age_quit_smoking', with = FALSE]
quit_smoking_age_missingness_plot <- vis_miss(quit_smoking_age, warn_large_data = FALSE)
ggsave(paste0(output_folder, 'svg/', 'quit-smoking-age-missingness.svg'), quit_smoking_age_missingness_plot, dpi = 300)

# missingness for all variables
all_variables <- as.vector(VARIABLES[missingness_plot != FALSE, 'name', with = FALSE])
dt_all <- dt[, .SD, .SDcols = all_variables$name]
all_missingness_plot <- vis_miss(dt_all, warn_large_data = FALSE)
ggsave(paste0(output_folder, 'svg/', 'all-missingness.svg'), all_missingness_plot, dpi = 300)
# calculate FMI
calculate_fmi <- function(column) {
  sum(is.na(column)) / length(column)
}
fmi_values <- sapply(dt_all, calculate_fmi)
overall_fmi <- mean(fmi_values, na.rm = TRUE)
print('FMI')
print(overall_fmi)