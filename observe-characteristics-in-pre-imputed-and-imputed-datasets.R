# observe the distribution of variables in the pre-imputed and imputed data side-by-side



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
random_i <- sample(1:100, 1)
# data loading
load(paste0(input_folder, 'VARIABLES', '.Rda'))
load(paste0(input_folder, 'ALL-IMPUTED-', random_i, '.Rda'))
dt_imputed <- copy(dt_working)
rm(dt_working)
dt_imputed$imputed <- TRUE
load(paste0(input_folder, 'ALL-EDITED', '.Rda'))
dt$imputed <- FALSE
# combine datasets
dt_all <- rbind(dt_edited, dt_imputed)
print(dim(dt))



# load all variables
variables <- VARIABLES[cluster_variable == TRUE & (imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2'), 'name', with = FALSE]
variables <- rbind(variables, list(name = 'imputed'))
print(class(variables))

dt_all <- dt_all[, .SD, .SDcols = variables$name]
print(dim(dt_all))
# summarize all variables and save to .docx
table <- dt_all %>%
    tbl_summary(
        by = imputed,
        statistic = list(all_continuous() ~ "{mean} ({sd})")
        ) %>%
    add_overall() %>%
    add_p() %>%
    add_n() %>%
    bold_labels() %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = paste0(output_folder, 'docx/', 'all-pre-imputed-and-imputed', '.docx'))