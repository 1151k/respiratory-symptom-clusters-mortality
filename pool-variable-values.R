# pool variable values across datasets (by cohort)



# Load packages
packages_all = c("data.table", "dplyr", "mice", "miceafter")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
    install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# constants
i_min <- 1
i_max <- 100
variable_name <- 'woken_by_sob' # this variable is changed to the variable of interest, e.g., woken_by_sob



# load variables (used to determine if variable is binary or numeric)
load('output/rda/VARIABLES.Rda')
variable_type <- VARIABLES[name == variable_name, 'variable_type', with = FALSE]



# fetch all data
variable_df <- data.table()
for (dataset_i in i_min:i_max) {
    print(paste('dataset', dataset_i))
    df <- readRDS(paste0('output/rda/mortality-loaded-data-', dataset_i, '.Rda'))
    df <- df[, c('cluster', variable_name), with = FALSE]    
    df[, dataset_id := dataset_i]
    variable_df <- rbind(variable_df, df[, .SD, .SDcols = c('dataset_id', 'cluster', variable_name)])
}
print('ALL LOADED')



# if 'numeric' variable, take the mean of the column values, else take the proportion of 1s
if (variable_type == 'numeric') {
    # from the variable_df data.table, take the mean of the variable_name column by cluster
    pooled_value <- variable_df[, mean(get(variable_name)), by = cluster]
} else {
    # from the variable_df data.table, take the proportion of 1s in the variable_name column by cluster
    pooled_value <- variable_df[, mean(get(variable_name) == 1), by = cluster]
}
# print the pooled values
print('POOLED VALUES BY CLUSTER')
print(pooled_value)



# calculate the overall pooled value
if (variable_type == 'numeric') {
    # from the variable_df data.table, take the mean of the variable_name column
    pooled_value <- mean(variable_df[, get(variable_name)])
} else {
    # from the variable_df data.table, take the proportion of 1s in the variable_name column
    pooled_value <- mean(variable_df[, get(variable_name) == 1])
}
# print the overall pooled value
print('OVERALL POOLED VALUE')
print(pooled_value)