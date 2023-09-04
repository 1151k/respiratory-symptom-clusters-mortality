# observe the distribution of variables in derived clusters



# Load packages
packages_all = c("data.table", "gtsummary", "flextable", "dplyr", "reticulate")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
use_python("/usr/local/bin/python3")



# constants
k <- 7 # number of clusters
i_range <- c(1:100)



for (dataset_i in i_range) {
    # print dataset and k
    print(paste('dataset =', dataset_i))
    print(paste('k =', k))

    # load subject data
    load(paste0('output/rda/', 'ALL-IMPUTED-', dataset_i, '.Rda'))
    df <- dt_working
    np <- import("numpy")
    cluster_labels <- np$load(paste0('output/npy/', 'labels-', dataset_i, '-', k, '.npy'))

    # load variables
    load(paste0('output/rda/', 'VARIABLES.Rda'))
    cluster_variables <- VARIABLES[cluster_variable == TRUE, 'name', with = FALSE]
    cluster_and_id_variables <- copy(cluster_variables)
    cluster_and_id_variables <- rbind(cluster_and_id_variables, list(name = 'person_id'))
    non_cluster_variables <- as.vector(VARIABLES[cluster_variable == FALSE, 'name', with = FALSE])
    characteristics_variables <- VARIABLES[characteristics_plot == TRUE, 'name', with = FALSE]
    characteristics_variables <- rbind(characteristics_variables, list(name = 'cluster'))

    # separate subjects with and without respiratory symptoms
    df_respiratory_symptoms <- df[df[, rowSums(.SD == 1) >= 1, .SDcols = cluster_variables$name]]
    df_no_respiratory_symptoms <- df[df[, rowSums(.SD == 0) == length(.SD), .SDcols = cluster_variables$name]]
    df_respiratory_symptoms <- df_respiratory_symptoms[order(person_id, cohort)]
    df_no_respiratory_symptoms <- df_no_respiratory_symptoms[order(person_id, cohort)]
    n_no_respiratory_symptoms <- nrow(df_no_respiratory_symptoms)
    n_respiratory_symptoms <- nrow(df_respiratory_symptoms)

    # assign clusters correctly
    df_respiratory_symptoms$cluster <- cluster_labels

    # combine data
    df_clustered <- rbind(df_respiratory_symptoms, df_no_respiratory_symptoms)

    # show the median and IQR of CCI in each cluster
    print('show mean and standard deviation for CCI in each cluster')
    print(df_clustered[, lapply(.SD, mean), by = cluster, .SDcols = 'CCI'])
    print(df_clustered[, lapply(.SD, sd), by = cluster, .SDcols = 'CCI'])

    # count average number of 1 in the columns from cluster_variables$name by cohort
    df_symptom_count <- copy(df_clustered)
    df_symptom_count[, (cluster_variables$name) := lapply(.SD, as.numeric), .SDcols = cluster_variables$name]
    df_symptom_count[, (cluster_variables$name) := lapply(.SD, `-`, 1), .SDcols = cluster_variables$name]
    df_symptom_count[, symptoms_n := rowSums(.SD), .SDcols = cluster_variables$name]
    # get unique clusters
    unique_clusters <- unique(df_clustered$cluster)
    # get median number of symptoms_n and IQR, by cluster
    for (unique_cluster in unique_clusters) {
        # get subset of df_symptom_count for the current cluster
        df_symptom_count_subset <- df_symptom_count[df_symptom_count$cluster == unique_cluster, ]
        # get median number of symptoms_n
        median_symptoms_n <- median(df_symptom_count_subset$symptoms_n)
        # get lower quartile of symptoms_n
        lower_quartile_symptoms_n <- quantile(df_symptom_count_subset$symptoms_n, 0.25)
        # get upper quartile of symptoms_n
        upper_quartile_symptoms_n <- quantile(df_symptom_count_subset$symptoms_n, 0.75)
        # get lower limit of symptoms_n
        lower_limit_symptoms_n <- lower_quartile_symptoms_n - 1.5 * (upper_quartile_symptoms_n - lower_quartile_symptoms_n)
        # get upper limit of symptoms_n
        upper_limit_symptoms_n <- upper_quartile_symptoms_n + 1.5 * (upper_quartile_symptoms_n - lower_quartile_symptoms_n)
        # print the results
        print(paste0('cluster ', unique_cluster, ': median = ', median_symptoms_n, ', IQR = [', lower_quartile_symptoms_n, ', ', upper_quartile_symptoms_n, '], limits = [', lower_limit_symptoms_n, ', ', upper_limit_symptoms_n, ']'))
    }

    # reduce the columns to the ones that will be shown in the characteristics tables
    df_clustered <- df_clustered[, characteristics_variables$name, with = FALSE]

    # remake the CCI variable, so that it is a factor with values 0, 1-2, or 3 (denoting ≥3)
    df_clustered$CCI[df_clustered$CCI == 1 | df_clustered$CCI == 2] <- 1
    df_clustered$CCI[df_clustered$CCI >= 3] <- 2
    # make factor variable of CCI with levels 0,1,2,3,andNA
    df_clustered$CCI <- factor(df_clustered$CCI, levels = c(0, 1, 2), labels = c('0', '1-2', '≥3'))



    # get frequencies of positive answers for each cluster and each cluster (respiratory symptom) variable
    results <- data.table()
    # loop over each cluster
    for (c in unique(df_clustered$cluster)) {
        if (!is.na(c)) {
            # subset the data for the current cluster
            df_subset <- df_clustered[cluster == c]
            df_subset <- df_subset[, cluster_variables$name, with = FALSE]
            for (col in colnames(df_subset)) {
                if (col != 'cluster') {
                    positive = nrow(df_subset[eval(as.name(col)) == "1"])
                    percentage = round(positive / nrow(df_subset) * 100, 1)
                    results <- rbind(results, data.table(cluster = c, variable = col, perc = percentage))
                }
            }
        }
    }
    results <- results[order(cluster)]
    # present the results
    for (c in unique(results$cluster)) {
        if (!is.na(c)) {
            results_subset <- results[cluster == c]
            results_subset <- results_subset[order(-perc)]
            print(paste0('cluster ', c, ' dataset ', dataset_i, ' (', nrow(df_clustered[cluster == c]), ' subjects)'))
            print(results_subset[1:31,])
        }
    }

    # select all clusters and asymptomatics (assigned to 99)
    df_clustered$cluster[is.na(df_clustered$cluster)] <- 99
    # when comparing clusters to each other (and not to asymptomatics), uncomment the following line and comment out the above line
    # df_clustered <- df_clustered[!is.na(cluster),]
    df_clustered$cluster[df_clustered$cluster == 0] <- 100
    df_clustered$cluster[df_clustered$cluster == 1] <- 200
    df_clustered$cluster[df_clustered$cluster == 2] <- 300
    df_clustered$cluster[df_clustered$cluster == 3] <- 400
    df_clustered$cluster[df_clustered$cluster == 4] <- 500
    df_clustered$cluster[df_clustered$cluster == 5] <- 600
    df_clustered$cluster[df_clustered$cluster == 6] <- 700
    df_clustered$cluster[df_clustered$cluster == 7] <- 800
    df_clustered$cluster[df_clustered$cluster == 8] <- 900
    df_clustered$cluster[df_clustered$cluster == 9] <- 1000
    df_clustered$cluster[df_clustered$cluster == 99] <- 1 # not used when only comparing symptomatic groups
    df_clustered$cluster[df_clustered$cluster == 100] <- 2
    df_clustered$cluster[df_clustered$cluster == 200] <- 3
    df_clustered$cluster[df_clustered$cluster == 300] <- 8
    df_clustered$cluster[df_clustered$cluster == 400] <- 6
    df_clustered$cluster[df_clustered$cluster == 500] <- 5
    df_clustered$cluster[df_clustered$cluster == 600] <- 7
    df_clustered$cluster[df_clustered$cluster == 700] <- 4
    df_clustered$cluster[df_clustered$cluster == 800] <- 9
    df_clustered$cluster[df_clustered$cluster == 900] <- 10
    df_clustered$cluster[df_clustered$cluster == 1000] <- 11



    # compare cluster x with the rest
    # uncomment the following two lines if you want to compare cluster x with the rest
    # focus_cluster <- 6
    # df_clustered$cluster[df_clustered$cluster != focus_cluster] <- 99



    # summarize data
    table <- df_clustered %>%
        tbl_summary(
            by = cluster
            , statistic = list(all_continuous() ~ "{mean} ± {sd}", all_categorical() ~ "{p}")
        ) %>%
        add_p() %>%
        bold_labels() %>%
        as_flex_table() %>%
        flextable::save_as_docx(path = paste0('output/docx/', 'characteristics-clusters', '-', k, '-', dataset_i, '.docx'))
}