# add mortality (outcome) data to the imputed datasets



# Load packages
packages_all = c("data.table", "gtsummary", "flextable", "dplyr", "reticulate", "haven")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
use_python("/usr/local/bin/python3")



# constants
i_range <- c(1:100)



for (dataset_i in i_range) {
    # load subject data
    load(paste0('output/rda/', 'ALL-IMPUTED-', dataset_i, '.Rda'))
    df <- dt_working
    np <- import("numpy")
    cluster_labels <- np$load(paste0('output/npy/', 'labels-', dataset_i, '-', k, '.npy'))
    print(paste('DATASET', dataset_i))

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
    df_no_respiratory_symptoms$cluster <- NA

    # combine data
    df_clustered <- rbind(df_respiratory_symptoms, df_no_respiratory_symptoms)
    df_clustered$cluster[is.na(df_clustered$cluster)] <- 99



    ##############################
    # BEGIN mortality
    # WSAS
    # fetch and format/define columns
    WSAS_register_data <- haven::read_sav(paste0('input/sav/', 'WSAS-mortality', '.sav'))
    print('loaded WSAS mortality data')
    WSAS_register_data <- WSAS_register_data %>% select(c("LopNr", "DODSDAT", "ULORSAK"))
    WSAS_register_data <- WSAS_register_data %>% rename(person_id = LopNr, mortality_cause = ULORSAK, time_deceased = DODSDAT)
    WSAS_register_data$time_deceased <- as.Date(paste(substr(WSAS_register_data$time_deceased, 1, 4), substr(WSAS_register_data$time_deceased, 5, 6), substr(WSAS_register_data$time_deceased, 7, 8), sep = "-"), format = "%Y-%m-%d")
    WSAS_register_data$person_id <- as.numeric(WSAS_register_data$person_id)
    # cardiovascular mortality
    WSAS_register_data$time_deceased_cardiovascular <- as.Date(NA)
    WSAS_register_data$time_deceased_cardiovascular[startsWith(WSAS_register_data$mortality_cause, 'I') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'I') == TRUE]
    # heart failure mortality
    WSAS_register_data$time_deceased_heart_failure <- as.Date(NA)
    WSAS_register_data$time_deceased_heart_failure[startsWith(WSAS_register_data$mortality_cause, 'I50') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'I50') == TRUE]
    # coronary artery mortality
    WSAS_register_data$time_deceased_coronary_heart <- as.Date(NA)
    WSAS_register_data$time_deceased_coronary_heart[startsWith(WSAS_register_data$mortality_cause, 'I20') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I21') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I22') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I23') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I24') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I25') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'I20') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I21') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I22') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I23') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I24') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I25') == TRUE]
    # arrhythmia mortality
    WSAS_register_data$time_deceased_arrhythmia <- as.Date(NA)
    WSAS_register_data$time_deceased_arrhythmia[startsWith(WSAS_register_data$mortality_cause, 'I44') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I45') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I46') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I47') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I48') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I49') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'I44') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I45') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I46') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I47') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I48') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I49') == TRUE]
    # cerebrovascular mortality
    WSAS_register_data$time_deceased_cerebrovascular <- as.Date(NA)
    WSAS_register_data$time_deceased_cerebrovascular[startsWith(WSAS_register_data$mortality_cause, 'I6') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'I6') == TRUE]
    # peripeheral vascular mortality
    WSAS_register_data$time_deceased_peripheral_vascular <- as.Date(NA)
    WSAS_register_data$time_deceased_peripheral_vascular[startsWith(WSAS_register_data$mortality_cause, 'I7') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I8') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I26') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'I7') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I8') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'I26') == TRUE]
    # neurological mortality
    WSAS_register_data$time_deceased_neurological <- as.Date(NA)
    WSAS_register_data$time_deceased_neurological[startsWith(WSAS_register_data$mortality_cause, 'G') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'G') == TRUE]
    # respiratory mortality
    WSAS_register_data$time_deceased_respiratory <- as.Date(NA)
    WSAS_register_data$time_deceased_respiratory[startsWith(WSAS_register_data$mortality_cause, 'J') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'J') == TRUE]
    # chronic lower respiratory disease mortality
    WSAS_register_data$time_deceased_lower_respiratory <- as.Date(NA)
    WSAS_register_data$time_deceased_lower_respiratory[startsWith(WSAS_register_data$mortality_cause, 'J40') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J41') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J42') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J43') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J44') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J45') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J46') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J47') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'J40') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J41') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J42') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J43') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J44') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J45') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J46') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J47') == TRUE]
    # copd mortality
    WSAS_register_data$time_deceased_copd <- as.Date(NA)
    WSAS_register_data$time_deceased_copd[startsWith(WSAS_register_data$mortality_cause, 'J43') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J44') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'J43') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J44') == TRUE]
    # lower respiratory infection mortality
    WSAS_register_data$time_deceased_lower_respiratory_infection <- as.Date(NA)
    WSAS_register_data$time_deceased_lower_respiratory_infection[startsWith(WSAS_register_data$mortality_cause, 'J09') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J1') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J2') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'U071') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'UO72') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'J09') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J1') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'J2') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'U071') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'UO72') == TRUE]
    # gastrointestinal mortality
    WSAS_register_data$time_deceased_gastrointestinal <- as.Date(NA)
    WSAS_register_data$time_deceased_gastrointestinal[startsWith(WSAS_register_data$mortality_cause, 'K') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'K') == TRUE]
    # cancer mortality
    WSAS_register_data$time_deceased_cancer <- as.Date(NA)
    WSAS_register_data$time_deceased_cancer[startsWith(WSAS_register_data$mortality_cause, 'C') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'C') == TRUE]
    # lung cancer mortality
    WSAS_register_data$time_deceased_lung_cancer <- as.Date(NA)
    WSAS_register_data$time_deceased_lung_cancer[startsWith(WSAS_register_data$mortality_cause, 'C33') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'C34') == TRUE] <- WSAS_register_data$time_deceased[startsWith(WSAS_register_data$mortality_cause, 'C33') == TRUE | startsWith(WSAS_register_data$mortality_cause, 'C34') == TRUE]
    # drop mortality cause column
    WSAS_register_data <- WSAS_register_data %>% select(-mortality_cause)
    # load a copy of all WSAS participants and match with register data
    df_WSAS <- copy(df_clustered[df_clustered$cohort == 'WSAS-I-2008' | df_clustered$cohort == 'WSAS-II-2016'])
    df_WSAS <- df_WSAS %>% select(-starts_with('time_deceased'))
    df_WSAS <- merge(df_WSAS, WSAS_register_data, by = 'person_id', all.x = TRUE)

    # OLIN
    # fetch and format/define columns
    OLIN_register_data <- haven::read_sav(paste0('input/sav/',  'OLIN-mortality.sav'))
    print('loaded OLIN mortality data')
    OLIN_register_data <- OLIN_register_data %>% select(c("LopNr", "DODSDAT", "ULORSAK"))
    OLIN_register_data <- OLIN_register_data %>% rename(person_id = LopNr, time_deceased = DODSDAT, mortality_cause = ULORSAK)
    OLIN_register_data$time_deceased <- as.Date(paste(substr(OLIN_register_data$time_deceased, 1, 4), substr(OLIN_register_data$time_deceased, 5, 6), substr(OLIN_register_data$time_deceased, 7, 8), sep = "-"), format = "%Y-%m-%d")
    OLIN_register_data$person_id <- as.numeric(OLIN_register_data$person_id)
    # cardiovascular mortality
    OLIN_register_data$time_deceased_cardiovascular <- as.Date(NA)
    OLIN_register_data$time_deceased_cardiovascular[startsWith(OLIN_register_data$mortality_cause, 'I') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'I') == TRUE]
    # heart failure mortality
    OLIN_register_data$time_deceased_heart_failure <- as.Date(NA)
    OLIN_register_data$time_deceased_heart_failure[startsWith(OLIN_register_data$mortality_cause, 'I50') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'I50') == TRUE]
    # coronary artery mortality
    OLIN_register_data$time_deceased_coronary_heart <- as.Date(NA)
    OLIN_register_data$time_deceased_coronary_heart[startsWith(OLIN_register_data$mortality_cause, 'I20') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I21') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I22') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I23') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I24') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I25') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'I20') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I21') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I22') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I23') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I24') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I25') == TRUE]
    # arrhythmia mortality
    OLIN_register_data$time_deceased_arrhythmia <- as.Date(NA)
    OLIN_register_data$time_deceased_arrhythmia[startsWith(OLIN_register_data$mortality_cause, 'I44') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I45') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I46') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I47') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I48') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I49') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'I44') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I45') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I46') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I47') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I48') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I49') == TRUE]
    # cerebrovascular mortality
    OLIN_register_data$time_deceased_cerebrovascular <- as.Date(NA)
    OLIN_register_data$time_deceased_cerebrovascular[startsWith(OLIN_register_data$mortality_cause, 'I6') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'I6') == TRUE]
    # peripheral vascular mortality
    OLIN_register_data$time_deceased_peripheral_vascular <- as.Date(NA)
    OLIN_register_data$time_deceased_peripheral_vascular[startsWith(OLIN_register_data$mortality_cause, 'I7') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I8') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I26') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'I7') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I7') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I8') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'I26') == TRUE]
    # neurological mortality
    OLIN_register_data$time_deceased_neurological <- as.Date(NA)
    OLIN_register_data$time_deceased_neurological[startsWith(OLIN_register_data$mortality_cause, 'G') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'G') == TRUE]
    # respiratory mortality
    OLIN_register_data$time_deceased_respiratory <- as.Date(NA)
    OLIN_register_data$time_deceased_respiratory[startsWith(OLIN_register_data$mortality_cause, 'J') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'J') == TRUE]
    # chronic lower respiratory disease mortality
    OLIN_register_data$time_deceased_lower_respiratory <- as.Date(NA)
    OLIN_register_data$time_deceased_lower_respiratory[startsWith(OLIN_register_data$mortality_cause, 'J40') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J41') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J42') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J43') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J44') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J45') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J46') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J47') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'J40') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J41') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J42') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J43') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J44') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J45') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J46') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J47') == TRUE]
    # copd mortality
    OLIN_register_data$time_deceased_copd <- as.Date(NA)
    OLIN_register_data$time_deceased_copd[startsWith(OLIN_register_data$mortality_cause, 'J43') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J44') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'J43') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J44') == TRUE]
    # lower respiratory infection mortality
    OLIN_register_data$time_deceased_lower_respiratory_infection <- as.Date(NA)
    OLIN_register_data$time_deceased_lower_respiratory_infection[startsWith(OLIN_register_data$mortality_cause, 'J09') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J1') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J2') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'U071') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'U072') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'J09') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J1') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'J2') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'U071') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'U072') == TRUE]
    # gastrointestinal mortality
    OLIN_register_data$time_deceased_gastrointestinal <- as.Date(NA)
    OLIN_register_data$time_deceased_gastrointestinal[startsWith(OLIN_register_data$mortality_cause, 'K') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'K') == TRUE]
    # cancer mortality
    OLIN_register_data$time_deceased_cancer <- as.Date(NA)
    OLIN_register_data$time_deceased_cancer[startsWith(OLIN_register_data$mortality_cause, 'C') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'C') == TRUE]
    # lung cancer mortality
    OLIN_register_data$time_deceased_lung_cancer <- as.Date(NA)
    OLIN_register_data$time_deceased_lung_cancer[startsWith(OLIN_register_data$mortality_cause, 'C33') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'C34') == TRUE] <- OLIN_register_data$time_deceased[startsWith(OLIN_register_data$mortality_cause, 'C33') == TRUE | startsWith(OLIN_register_data$mortality_cause, 'C34') == TRUE]
    # drop mortality cause column
    OLIN_register_data <- OLIN_register_data %>% select(-mortality_cause)
    # load a copy of all OLIN participants and match with register data
    df_OLIN <- copy(df_clustered[df_clustered$cohort == 'OLIN-IV-1996' | df_clustered$cohort == 'OLIN-VI-2006' | df_clustered$cohort == 'OLIN-VII-2016'])
    df_OLIN <- df_OLIN %>% select(-starts_with('time_deceased'))
    df_OLIN <- merge(df_OLIN, OLIN_register_data, by = 'person_id', all.x = TRUE)

    # merge OLIN and WSAS
    df_clustered <- rbind(df_OLIN, df_WSAS)

    # save to Rda
    df <- copy(df_clustered)
    df <- as.data.table(df)
    saveRDS(df, file = paste0('output/rda/mortality-loaded-data-', dataset_i, '.Rda'))
    # END mortality
    ##############################
}