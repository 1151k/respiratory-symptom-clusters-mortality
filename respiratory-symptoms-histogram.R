# generate a histogram of respiratory symptoms occurence



# Load packages
packages_all = c("data.table", "dplyr", "mice", "miceafter", "ggplot2")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
    install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# constants
i_min <- 1
i_max <- 100
debug <- FALSE



# load variables
load('output/rda/VARIABLES.Rda')
cluster_variables <- VARIABLES[cluster_variable == TRUE, 'name', with = FALSE]
cluster_and_id_variables <- copy(cluster_variables)
cluster_and_id_variables <- rbind(cluster_and_id_variables, list(name = 'person_id'))
cluster_and_id_variables <- rbind(cluster_and_id_variables, list(name = 'cohort'))
non_cluster_variables <- as.vector(VARIABLES[cluster_variable == FALSE, 'name', with = FALSE])



symptom_df <- data.table()
for (dataset_i in i_min:i_max) {
    print(paste('DATASET', dataset_i))
    load(paste0('output/rda/ALL-IMPUTED-', dataset_i, '.Rda'))
    df <- copy(dt_working)
    df <- df[, .SD, .SDcols = cluster_and_id_variables$name]
    df[, symptom_count := rowSums(.SD == 1), .SDcols = cluster_variables$name]
    df[, dataset_id := dataset_i]
    symptom_df <- rbind(symptom_df, df[, .(dataset_id, symptom_count)])
}



# compile "any symptoms"
milist <- df2milist(symptom_df, 'dataset_id', keep = FALSE)
props_any_symptom <- with(milist, expr = prop_wald(symptom_count > 0))
pooled_any_symptom <- pool_prop_wilson(props_any_symptom, 0.95)



# compile n symptoms
y_mins <- c()
y_maxs <- c()
percentages <- c()
for (i in 0:31) {
    milist <- df2milist(symptom_df, 'dataset_id', keep = FALSE)
    props_any_symptom <- with(milist, expr = prop_wald(symptom_count == i))
    pooled_any_symptom <- pool_prop_wilson(props_any_symptom, 0.95)
    if (i > 20) {
        percentages[21] <- percentages[21] + pooled_any_symptom[[1,1]]*100
    } else {
        percentages <- c(percentages, pooled_any_symptom[[1,1]]*100)
    }
    y_mins <- c(y_mins, pooled_any_symptom[[1,2]]*100)
    y_maxs <- c(y_maxs, pooled_any_symptom[[1,3]]*100)
}

# create a list of symptoms
symptoms <- rep(1:length(percentages), percentages)
# calculate the average
average <- mean(symptoms)
print('Average number of symptoms:')
print(average)
# calculate the standard deviation
standard_deviation <- sd(symptoms)
print('Standard deviation of number of symptoms:')
print(standard_deviation)

# make plot
ggplot_data <- data.frame(
    x = c(0:20),
    y = c(percentages)
)
symptom_plot <- ggplot(ggplot_data) +
    geom_bar(aes(x = x,
                y = y),
            fill = '#8aaec6',
            position = position_dodge(width = .9),
            stat="identity") +
    geom_text(aes(x=x,
                    y=y+2.0,
                    label=ifelse(y < 0.1, '<0.1', round(y, 1))
                ),
                    size = 4.1,
                    angle=90,
                    fontface = 'bold',
                    color = '#2b343b') +
    labs(x="Number of symptoms",
        y="Frequency (%)") +
    scale_y_continuous(expand=c(0,0),limits=c(0,44)) +
    scale_x_continuous(breaks = seq(0, 20, 1))
ggsave(filename = paste0('output/svg/respiratory-symptoms-histogram.svg'), plot = symptom_plot, dpi = 300)