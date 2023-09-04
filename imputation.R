# impute data, evaluate imputation, and form composite variables



# load external packages
packages_all = c("data.table", "dplyr", "ggplot2", "haven", "miceRanger")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# constants
input_folder <- 'output/rda/'
output_folder <- 'output/'
datasets_n <- 100
max_iter <- 20
verbose <- TRUE



# variables and data loading
load(paste0(input_folder, 'VARIABLES', '.Rda'))
load(paste0(input_folder, 'ALL-EDITED', '.Rda'))
dependent_variables_level_1 <- as.vector(VARIABLES[(imputation_order == 'independent' | imputation_order == 'dependent-1'), 'name', with = FALSE])
dependent_variables_level_2 <- as.vector(VARIABLES[(imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2'), 'name', with = FALSE])
independent_variables <- as.vector(VARIABLES[imputation_order == 'independent', 'name', with = FALSE])
asthma_diagnosis_age_variables <- as.vector(VARIABLES[(imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2' | imputation_order == 'dependent-asthma'), 'name', with = FALSE])
smoking_variables <- as.vector(VARIABLES[(imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2' | imputation_order == 'dependent-smoker'), 'name', with = FALSE])
ever_smoking_variables <- as.vector(VARIABLES[(imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2' | imputation_order == 'dependent-ever-smoker'), 'name', with = FALSE])
ex_smoker_variables <- as.vector(VARIABLES[(imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2' | imputation_order == 'dependent-ex-smoker'), 'name', with = FALSE])



# select independent variables
dt_independent <- dt[, independent_variables$name, with = FALSE]
# impute independent variables
mice_independent <- miceRanger(
    dt_independent,
    m = datasets_n,
    maxiter = max_iter,
    verbose = verbose
)
imputed_independent_datasets <- miceRanger::completeData(mice_independent)
# evaluate imputation
model_oob_error_independent <- plotModelError(mice_independent)
ggsave(paste0(output_folder, 'svg/', 'plot-model-oob-error-independent.svg'), model_oob_error_independent, dpi = 300)

# select level-1 dependent variables
for (i in 1:datasets_n) {
    print(paste('RUNNING SUBSTEPS FOR', i, 'DATASET'))
    dt_working <- copy(dt)
    imputed_independent_dataset <- imputed_independent_datasets[[i]]
    for (independent_variable in independent_variables) {
        # update independent variables with imputed values
        dt_working[, (independent_variable) := imputed_independent_dataset[, (independent_variable), with = FALSE]]
    }
    # update values of dependent variables
    dt_working$asthma_physician_diagnosed[dt_working$asthma_self_reported == 0] <- 0
    dt_working$asthma_physician_diagnosed_age[dt_working$asthma_physician_diagnosed == 0] <- NA
    dt_working$copd_physician_diagnosed[dt_working$copd_self_reported == 0] <- 0
    dt_working$rash_6m_12m[dt_working$rash_6m == 0] <- 0
    dt_working$rash_only_hands[dt_working$rash_6m == 0] <- 0
    dt_working$cigarettes_per_day[dt_working$smoking_currently == 0] <- NA
    dt_working$age_start_smoking[dt_working$smoking_currently == 0 & dt_working$smoking_previously == 0] <- NA
    dt_working$age_quit_smoking[dt_working$smoking_previously == 0] <- NA
    dt_working$attack_12m[dt_working$attack_10y == 0] <- 0
    dt_working$wheezing_sob_12m[dt_working$wheezing_12m == 0] <- 0
    dt_working$wheezing_wo_cold_12m[dt_working$wheezing_12m == 0] <- 0
    dt_working$cough_productive_3m[dt_working$cough_productive_recurrent == 0] <- 0
    dt_working$cough_productive_3m_2y[dt_working$cough_productive_3m == 0] <- 0
    dt_working$rhinitis_12m[dt_working$rhinitis == 0] <- 0
    dt_working$rhinitis_5d[dt_working$rhinitis_currently == 0] <- 0
    dt_working$rhinitis_5d_5w[dt_working$rhinitis_currently == 0] <- 0
    dt_working$rhinitis_conjunctivitis[dt_working$rhinitis_currently == 0] <- 0
    dt_working$smoking_previously[dt_working$smoking_currently == 1] <- 0
    dt_working$bmi <- dt_working$weight / (dt_working$height / 100) ^ 2
    # select level-1 dependent data
    dt_dependent_level_1 <- dt_working[, dependent_variables_level_1$name, with = FALSE]
    # impute level-1 dependent variables
    mice_dependent_level_1 <- miceRanger(
        dt_dependent_level_1,
        m = datasets_n,
        maxiter = max_iter,
        verbose = verbose
    )
    imputed_dependent_level_1_dataset <- miceRanger::completeData(mice_dependent_level_1)[[runif(1, 1, datasets_n)]] # select random dataset
    # evaluate imputation
    model_oob_error_dependent_level_1 <- plotModelError(mice_dependent_level_1)
    ggsave(paste0(output_folder, 'svg/', 'plot-model-oob-error-dependent-level-1.svg'), model_oob_error_dependent_level_1, dpi = 300)
    # select level-2 dependent variables
    dependent_variables_level_2 <- as.vector(VARIABLES[cluster_variable == TRUE & (imputation_order == 'independent' | imputation_order == 'dependent-1' | imputation_order == 'dependent-2'), 'name', with = FALSE])
    # update level-1 dependent variables with imputed values
    dt_working[, (dependent_variables_level_1$name) := imputed_dependent_level_1_dataset[, (dependent_variables_level_1$name), with = FALSE]]
    # update values of dependent variables
    dt_working$cough_productive_3m_2y[dt_working$cough_productive_3m == 0] <- 0
    dt_working$rhinitis_5d_5w[dt_working$rhinitis_5d == 0] <- 0
    dt_working$smoking_status <- ifelse(
        dt_working$smoking_currently == 1,
        1,
        ifelse(
            dt_working$smoking_previously == 1,
            2,
            0
        )
    )
    dt_dependent_level_2 <- dt_working[, dependent_variables_level_2$name, with = FALSE]
    # impute level-2 dependent variables
    mice_dependent_level_2 <- miceRanger(
        dt_dependent_level_2,
        m = datasets_n,
        maxiter = max_iter,
        verbose = verbose
    )
    imputed_dependent_level_2_dataset <- miceRanger::completeData(mice_dependent_level_2)[[runif(1, 1, datasets_n)]] # select random dataset
    # evaluate imputation
    model_oob_error_dependent_level_2 <- plotModelError(mice_dependent_level_2)
    ggsave(paste0(output_folder, 'svg/', 'plot-model-oob-error-dependent-level-2.svg'), model_oob_error_dependent_level_2, dpi = 300)
    # update level-2 dependent variables with imputed values
    dt_working[, (dependent_variables_level_2$name) := imputed_dependent_level_2_dataset[, (dependent_variables_level_2$name), with = FALSE]]
    # impute diagnosis age in subjects with asthma_physician_diagnosed == 1
    dt_asthma <- dt_working[asthma_physician_diagnosed == 1, asthma_diagnosis_age_variables$name, with = FALSE]
    mice_asthma <- miceRanger(
        dt_asthma,
        m = datasets_n,
        maxiter = max_iter,
        verbose = verbose
    )
    imputed_asthma_dataset <- miceRanger::completeData(mice_asthma)[[runif(1, 1, datasets_n)]] # select random dataset
    # evaluate imputation
    model_oob_error_asthma <- plotModelError(mice_asthma)
    ggsave(paste0(output_folder, 'svg/', 'plot-model-oob-error-asthma.svg'), model_oob_error_asthma, dpi = 300)
    # update asthma diagnosis age variables with imputed values
    dt_working[asthma_physician_diagnosed == 1, asthma_diagnosis_age_variables$name] <- imputed_asthma_dataset[, asthma_diagnosis_age_variables$name, with = FALSE]
    # impute cigarettes per day in current smokers
    dt_smoking <- dt_working[smoking_currently == 1, smoking_variables$name, with = FALSE]
    mice_smoking <- miceRanger(
        dt_smoking,
        m = datasets_n,
        maxiter = max_iter,
        verbose = verbose
    )
    imputed_smoking_dataset <- miceRanger::completeData(mice_smoking)[[runif(1, 1, datasets_n)]] # select random dataset
    # evaluate imputation
    model_oob_error_smoking <- plotModelError(mice_smoking)
    ggsave(paste0(output_folder, 'svg/', 'plot-model-oob-error-smoking.svg'), model_oob_error_smoking, dpi = 300)
    # update smoking variables with imputed values
    dt_working[smoking_currently == 1, smoking_variables$name] <- imputed_smoking_dataset[, smoking_variables$name, with = FALSE]
    # impute starting age of smoking in ever smokers
    dt_ever_smoking <- dt_working[smoking_currently == 1 | smoking_previously == 1, ever_smoking_variables$name, with = FALSE]
    mice_ever_smoking <- miceRanger(
        dt_ever_smoking,
        m = datasets_n,
        maxiter = max_iter,
        verbose = verbose
    )
    imputed_ever_smoking_dataset <- miceRanger::completeData(mice_ever_smoking)[[runif(1, 1, datasets_n)]] # select random dataset
    # evaluate imputation
    model_oob_error_ever_smoking <- plotModelError(mice_ever_smoking)
    ggsave(paste0(output_folder, 'svg/', 'plot-model-oob-error-ever-smoking.svg'), model_oob_error_ever_smoking, dpi = 300)
    # update ever smoking variables with imputed values
    dt_working[smoking_currently == 1 | smoking_previously == 1, ever_smoking_variables$name] <- imputed_ever_smoking_dataset[, ever_smoking_variables$name, with = FALSE]
    # impute qutting age of smoking in past smokers
    dt_past_smoking <- dt_working[smoking_previously == 1, ex_smoker_variables$name, with = FALSE]
    mice_past_smoking <- miceRanger(
        dt_past_smoking,
        m = datasets_n,
        maxiter = max_iter,
        verbose = verbose
    )
    imputed_past_smoking_dataset <- miceRanger::completeData(mice_past_smoking)[[runif(1, 1, datasets_n)]] # select random dataset
    # evaluate imputation
    model_oob_error_past_smoking <- plotModelError(mice_past_smoking)
    ggsave(paste0(output_folder, 'svg/', 'plot-model-oob-error-past-smoking.svg'), model_oob_error_past_smoking, dpi = 300)
    # update past smoking variables with imputed values
    dt_working[smoking_previously == 1, ex_smoker_variables$name] <- imputed_past_smoking_dataset[, ex_smoker_variables$name, with = FALSE]

    # save imputed data
    save(dt_working, file = paste0(output_folder, 'rda/', 'ALL-IMPUTED-', i, '.Rda'))
    rm(dt_working)
}