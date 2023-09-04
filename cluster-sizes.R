# get sizes of clusters



# Load packages
packages_all = c("data.table", "dplyr", "mice", "miceafter")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
    install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# constants
i_min <- 1
i_max <- 3



# fetch all data
pooled_df <- data.table()
for (dataset_i in i_min:i_max) {
    print(paste('dataset', dataset_i))
    df <- readRDS(paste0('output/rda/mortality-loaded-data-', dataset_i, '.Rda'))
    df <- df[, c('cluster'), with = FALSE]
    df[, dataset_id := dataset_i]
    pooled_df <- rbind(pooled_df, df[, .SD, .SDcols = c('dataset_id', 'cluster')])
}
print('ALL LOADED')



# for each cluster, get the average number of subjects per dataset as well as percentage of the total number of subjects in that dataset
for (k in c(0:6)) {
    # get the average number of subjects in cluster k as well as the percentage of the total number of subjects in that cluster
    cluster_k <- pooled_df[cluster == k]
    cluster_k <- cluster_k[, .N, by = dataset_id]
    cluster_k[, n := mean(N), by = dataset_id]
    cluster_k[, perc := N / 63060, by = dataset_id]
    cluster_k <- cluster_k[, .(dataset_id, n, perc)]
    avg_perc <- mean(cluster_k$perc)
    avg_n <- mean(cluster_k$n)
    print(paste('Cluster', k))
    print(cluster_k)
    print('avg perc')
    print(avg_perc)
    print('avg n')
    print(round(avg_n))
    print('---')
}