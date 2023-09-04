# prepare data for clustering (i.e., remove unnecessary columns)



# Load packages
packages_all = c("data.table")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
    install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# constants
i_range <- 1:100



# load variables
load('output/rda/VARIABLES.Rda')
cluster_variables <- VARIABLES[cluster_variable == TRUE, 'name', with = FALSE]
cluster_and_id_variables <- copy(cluster_variables)
cluster_and_id_variables <- rbind(cluster_and_id_variables, list(name = 'person_id'))
cluster_and_id_variables <- rbind(cluster_and_id_variables, list(name = 'cohort'))
non_cluster_variables <- as.vector(VARIABLES[cluster_variable == FALSE, 'name', with = FALSE])



for (dataset_i in i_range) {
    print(paste('dataset', dataset_i))
    load(paste0('output/rda/ALL-IMPUTED-', dataset_i, '.Rda'))
    
    # load data
    df_original <- copy(dt_working)
    df <- copy(dt_working)
    df <- df[, .SD, .SDcols = cluster_and_id_variables$name]
    df_respiratory_symptoms <- df[df[, rowSums(.SD == 1) >= 1, .SDcols = cluster_variables$name]]
    df_respiratory_symptoms <- df_respiratory_symptoms[order(person_id, cohort)]

    # make a variable combining id and cohort
    df_respiratory_symptoms$person_id_cohort <- paste(df_respiratory_symptoms$person_id, df_respiratory_symptoms$cohort, sep = '_')
    df_ids <- df_respiratory_symptoms[, c('person_id_cohort')]
    save(df_ids, file = paste0('output/rda/person_id_cohort-', dataset_i, '.Rda'))

    # get clustering data (subset of variables from those with symptoms)
    df_clustering <- df_respiratory_symptoms[, cluster_variables$name, with = FALSE]
    print(dim(df_clustering))
    # save raw clustering data
    save(df_clustering, file = paste0('output/rda/cluster-input-', dataset_i, '.Rda'))
}