# observe the distribution of variables prior to imputation



# load external packages
packages_all = c("data.table", "dplyr", "haven", "gtsummary")
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



# define the composite smoking status variable
dt[, smoking_status := ifelse(smoking_currently == 1, 1, ifelse(smoking_previously == 1, 2, ifelse(smoking_currently == 0 & smoking_previously == 0, 0, NA)))]



# load all variables
all_variables <- as.vector(VARIABLES[characteristics_plot == TRUE | VARIABLES$name == 'cohort', 'name', with = FALSE])
print(class(all_variables))
dt_all <- dt[, .SD, .SDcols = all_variables$name]
# summarize all variables and save to .docx
table <- dt_all %>%
    tbl_summary(
        by = cohort,
        statistic = list(all_continuous() ~ "{mean} ({sd})")
        ) %>%
    add_overall() %>%
    add_p() %>%
    add_n() %>%
    bold_labels() %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = paste0(output_folder, 'docx/', 'all-pre-imputed', '.docx'))