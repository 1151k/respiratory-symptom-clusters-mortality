# combine data from all cohorts into one file



# load external packages
packages_all = c("data.table", "dplyr", "foreign", "haven")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))



# file/cohort namings
input_folder <- 'input/'
output_folder <- 'output/'
output_name <- 'ALL-EDITED'
input_names <- c(
    'OLIN-IV-1996',
    'OLIN-VI-2006',
    'OLIN-VII-2016',
    'WSAS-I-2008',
    'WSAS-II-2016'
)
# load variable data
load(paste0(output_folder, 'rda/VARIABLES.Rda'))
names_standardized <- VARIABLES$name
# set data.table that all cohorts will be merged to
merged_data <- data.table()
# debug mode (print more info)
debug <- FALSE



# loop cohorts, rename accordingly and update coding, and lastly add to merged data.table
for (cohort in input_names) {
    # read data
    data <- setDT(read_sav(paste0(input_folder, 'sav/', cohort, ".sav")))
    if (debug) { print(paste('opened', cohort)) }
    # loop standardized variable names
    for (name_standardized in names_standardized) {
        if (name_standardized == 'cohort') {
            data[, 'cohort' := cohort]
        } else {
            # get variable cohort-specific variable name
            name_cohort_specific <- as.character(VARIABLES[name == name_standardized, cohort, with = FALSE])
            # update variable name in data to standardized name
            data.table::setnames(data, name_cohort_specific, name_standardized, skip_absent = TRUE)
            if (!any(names(data) == name_standardized)) {
                data[, (name_standardized) := NA]
            }
        }
    }
    if (debug) { print('updated variable names') }
    # select only standardized variables (relevant for study)
    data <- data[, ..names_standardized]
    if (debug) { print('selected standardized variables') }
    # cohort-specific variable modification
    if (cohort == 'OLIN-IV-1996') {
        data$gender <- data$gender - 1 # men = 0, women = 1, as currently they're 1 and 2, respectively
        data$asthma_family_history[data$asthma_family_history == 2] <- NA
        data$asthma_self_reported[data$asthma_self_reported == 2] <- NA
        data$asthma_physician_diagnosed[data$asthma_physician_diagnosed == 2] <- NA
        data$asthma_physician_diagnosed_age[data$asthma_physician_diagnosed_age > 74] <- NA
        data$asthma_medication_use[data$asthma_medication_use == 2] <- NA
        data$copd_family_history[data$copd_family_history == 2] <- NA
        data$copd_self_reported[data$copd_self_reported == 2] <- NA
        data$copd_physician_diagnosed[data$copd_physician_diagnosed == 2] <- NA
        data$rhinitis_conjunctivitis_family_history[data$rhinitis_conjunctivitis_family_history == 2] <- NA
        data$other_lung_disease_self_reported[data$other_lung_disease_self_reported == 2] <- NA
        data$smoking_currently[data$smoking_currently == 3] <- NA # one person had a typo value here
        data$smoking_previously[data$smoking_previously == 3] <- 2 # three persons had a typo value here
        data$smoking_previously[data$smoking_previously == 2] <- NA
        data$cigarettes_per_day[data$cigarettes_per_day == 5] <- NA
        data$age_start_smoking[data$age_start_smoking > 74] <- NA
        data$age_quit_smoking[data$age_quit_smoking > 74] <- NA
        data$attack_10y[data$attack_10y == 2] <- NA
        data$attack_12m[data$attack_12m == 2] <- NA
        data$attack_exercise[data$attack_exercise == 2] <- NA
        data$attack_cold[data$attack_cold == 2] <- NA
        data$attack_dust[data$attack_dust == 2] <- NA
        data$attack_tobacco[data$attack_tobacco == 2] <- NA
        data$attack_fume[data$attack_fume == 2] <- NA
        data$attack_dust_tobacco_fume <- ifelse(data$attack_dust == 1 | data$attack_tobacco == 1 | data$attack_fume == 1, 1, 0)
        data$attack_pollen[data$attack_pollen == 2] <- NA
        data$attack_fur[data$attack_fur == 2] <- NA
        data$wheezing_12m[data$wheezing_12m == 2] <- NA
        data$wheezing_sob_12m[data$wheezing_sob_12m == 2] <- NA
        data$wheezing_wo_cold_12m[data$wheezing_wo_cold_12m == 2] <- NA
        data$wheezing_recurrent[data$wheezing_recurrent == 2] <- NA
        data$cough_longstanding_12m[data$cough_longstanding_12m == 2] <- NA
        data$cough_productive_recurrent[data$cough_productive_recurrent == 2] <- NA
        data$cough_productive_3m[data$cough_productive_3m == 2] <- NA
        data$cough_productive_3m_2y[data$cough_productive_3m_2y == 2] <- NA
        data$dyspnea_ground_level_walking[data$dyspnea_ground_level_walking == 2] <- NA
        data$woken_by_chest_tightness[data$woken_by_chest_tightness == 2] <- NA
        data$rhinitis[data$rhinitis == 2] <- NA
    } else if (cohort == 'OLIN-VI-2006') {
        data$gender <- data$gender - 1 # men = 0, women = 1, as the coding is 1 and 2, respectively
        data$asthma_family_history[data$asthma_family_history == 2] <- NA
        data$asthma_self_reported[data$asthma_self_reported == 2] <- NA
        data$asthma_physician_diagnosed[data$asthma_physician_diagnosed == 2] <- NA
        data$asthma_physician_diagnosed_age[data$asthma_physician_diagnosed_age > 69] <- NA
        data$asthma_medication_use[data$asthma_medication_use == 2] <- NA
        data$copd_family_history[data$copd_family_history == 2] <- NA
        data$copd_self_reported[data$copd_self_reported == 2] <- NA
        data$copd_physician_diagnosed[data$copd_physician_diagnosed == 2] <- NA
        data$rhinitis_conjunctivitis_family_history[data$rhinitis_conjunctivitis_family_history == 2] <- NA
        data$other_lung_disease_self_reported[data$other_lung_disease_self_reported == 2] <- NA
        data$smoking_currently[data$smoking_currently == 2] <- NA
        data$smoking_previously[data$smoking_previously == 2] <- NA
        data$age_start_smoking[data$age_start_smoking > 69] <- NA
        data$age_quit_smoking[data$age_quit_smoking > 69] <- NA
        data$rural_childhood[data$rural_childhood == 2] <- NA
        data$farm_childhood[data$farm_childhood == 2] <- NA
        data$occupational_vgdf_exposure[data$occupational_vgdf_exposure == 2] <- NA
        data$times_exercise_per_week[data$times_exercise_per_week >= 7] <- 400
        data$times_exercise_per_week[data$times_exercise_per_week >= 4 & data$times_exercise_per_week < 7] <- 300
        data$times_exercise_per_week[data$times_exercise_per_week >= 2 & data$times_exercise_per_week < 4] <- 200
        data$times_exercise_per_week[data$times_exercise_per_week == 400] <- 4
        data$times_exercise_per_week[data$times_exercise_per_week == 300] <- 3
        data$times_exercise_per_week[data$times_exercise_per_week == 200] <- 2
        data$attack_10y[data$attack_10y == 2] <- NA
        data$attack_12m[data$attack_12m == 2] <- NA
        data$attack_exercise[data$attack_exercise == 2] <- NA
        data$attack_cold[data$attack_cold == 2] <- NA
        data$attack_dust[data$attack_dust == 2] <- NA
        data$attack_tobacco[data$attack_tobacco == 2] <- NA
        data$attack_fume[data$attack_fume == 2] <- NA
        data$attack_dust_tobacco_fume <- ifelse(data$attack_dust == 1 | data$attack_tobacco == 1 | data$attack_fume == 1, 1, 0)
        data$attack_pollen[data$attack_pollen == 2] <- NA
        data$attack_fur[data$attack_fur == 2] <- NA
        data$dyspnea_painkiller[data$dyspnea_painkiller == 2] <- NA
        data$wheezing_12m[data$wheezing_12m == 2] <- NA
        data$wheezing_sob_12m[data$wheezing_sob_12m == 2] <- NA
        data$wheezing_wo_cold_12m[data$wheezing_wo_cold_12m == 2] <- NA
        data$wheezing_recurrent[data$wheezing_recurrent == 2] <- NA
        data$cough_longstanding_12m[data$cough_longstanding_12m == 2] <- NA
        data$cough_productive_recurrent[data$cough_productive_recurrent == 2] <- NA
        data$cough_productive_3m[data$cough_productive_3m == 2] <- NA
        data$cough_productive_3m_2y[data$cough_productive_3m_2y == 2] <- NA
        data$dyspnea_ground_level_walking[data$dyspnea_ground_level_walking == 2] <- NA
        data$woken_by_chest_tightness[data$woken_by_chest_tightness == 2] <- NA
        data$rhinitis[data$rhinitis == 2] <- NA
        data$nasal_obstruction_recurrent[data$nasal_obstruction_recurrent == 2] <- NA
        data$rhinorrhea_recurrent[data$rhinorrhea_recurrent == 2] <- NA
    } else if (cohort == 'OLIN-VII-2016') {
        data$gender[data$gender == 0] <- 100
        data$gender[data$gender == 1] <- 0
        data$gender[data$gender == 100] <- 1
        data$asthma_family_history[data$asthma_family_history == 2] <- 0
        data$asthma_self_reported[data$asthma_self_reported == 2] <- 0
        data$asthma_physician_diagnosed[data$asthma_physician_diagnosed == 2] <- 0
        data$asthma_physician_diagnosed_age[data$asthma_physician_diagnosed_age > 79] <- NA
        data$asthma_medication_use[data$asthma_medication_use == 2] <- 0
        data$copd_family_history[data$copd_family_history == 2] <- 0
        data$copd_self_reported[data$copd_self_reported == 2] <- 0
        data$copd_physician_diagnosed[data$copd_physician_diagnosed == 2] <- 0
        data$copd_medication_use[data$copd_medication_use == 2] <- 0
        data$rhinitis_conjunctivitis_family_history[data$rhinitis_conjunctivitis_family_history == 2] <- 0
        data$rash_6m[data$rash_6m == 2] <- 0
        data$rash_6m_12m[data$rash_6m_12m == 2] <- 0
        data$rash_only_hands[data$rash_only_hands == 2] <- 0
        data$other_lung_disease_self_reported[data$other_lung_disease_self_reported == 2] <- 0
        data$smoking_currently[data$smoking_currently == 2] <- 0
        data$smoking_previously[data$smoking_previously == 2] <- 0
        data$age_start_smoking[data$age_start_smoking > 79] <- NA
        data$age_quit_smoking[data$age_quit_smoking > 79] <- NA
        data$rural_childhood[data$rural_childhood == 2] <- 0
        data$farm_childhood[data$farm_childhood == 2] <- 0
        data$occupational_vgdf_exposure[data$occupational_vgdf_exposure == 2] <- 0
        data$times_exercise_per_week[data$times_exercise_per_week > 4] <- 0
        data$times_exercise_per_week[data$times_exercise_per_week == 4] <- 100
        data$times_exercise_per_week[data$times_exercise_per_week == 3] <- 200
        data$times_exercise_per_week[data$times_exercise_per_week == 2] <- 300
        data$times_exercise_per_week[data$times_exercise_per_week == 1] <- 400
        data$times_exercise_per_week[data$times_exercise_per_week == 400] <- 4
        data$times_exercise_per_week[data$times_exercise_per_week == 300] <- 3
        data$times_exercise_per_week[data$times_exercise_per_week == 200] <- 2
        data$times_exercise_per_week[data$times_exercise_per_week == 100] <- 1
        data$attack_10y[data$attack_10y == 2] <- 0
        data$attack_12m[data$attack_12m == 2] <- 0
        data$attack_exercise[data$attack_exercise == 2] <- 0
        data$attack_cold[data$attack_cold == 2] <- 0
        data$attack_dust_tobacco_fume[data$attack_dust_tobacco_fume == 2] <- 0
        data$attack_pollen[data$attack_pollen == 2] <- 0
        data$attack_fur[data$attack_fur == 2] <- 0
        data$wheezing_12m[data$wheezing_12m == 2] <- 0
        data$wheezing_sob_12m[data$wheezing_sob_12m == 2] <- 0
        data$wheezing_wo_cold_12m[data$wheezing_wo_cold_12m == 2] <- 0
        data$wheezing_recurrent[data$wheezing_recurrent == 2] <- 0
        data$cough_longstanding_12m[data$cough_longstanding_12m == 2] <- 0
        data$cough_productive_recurrent[data$cough_productive_recurrent == 2] <- 0
        data$cough_productive_3m[data$cough_productive_3m == 2] <- 0
        data$cough_productive_3m_2y[data$cough_productive_3m_2y == 2] <- 0
        data$dyspnea_ground_level_walking[data$dyspnea_ground_level_walking == 2] <- 0
        data$rhinitis[data$rhinitis == 2] <- 0
        data$nasal_obstruction_recurrent[data$nasal_obstruction_recurrent == 2] <- 0
        data$rhinorrhea_recurrent[data$rhinorrhea_recurrent == 2] <- 0
    } else if (cohort == 'WSAS-I-2008') {
        data$age <- 2008 - as.numeric(data$age)
        data$gender <- data$gender - 1 # 0 = man, 1 = woman, while it was 1 and 2, initially, respectively
        data$asthma_family_history[data$asthma_family_history == 99] <- NA
        data$asthma_physician_diagnosed_age[data$asthma_physician_diagnosed_age > 75] <- NA
        data$copd_family_history[data$copd_family_history == 99] <- NA
        data$rhinitis_conjunctivitis_family_history[data$rhinitis_conjunctivitis_family_history == 99] <- NA
        data$cigarettes_per_day <- data$cigarettes_per_day + 1
        data$age_start_smoking[data$age_start_smoking > 75] <- NA
        data$age_quit_smoking[data$age_quit_smoking > 75] <- NA
        data$rural_childhood[data$rural_childhood == 99] <- NA
        data$farm_childhood[data$farm_childhood == 99] <- NA
        data$occupational_vgdf_exposure[data$occupational_vgdf_exposure == 99] <- NA
        data$times_exercise_per_week[data$times_exercise_per_week >= 5] <- 0
        data$times_exercise_per_week[data$times_exercise_per_week == 4] <- 100
        data$times_exercise_per_week[data$times_exercise_per_week == 3] <- 200
        data$times_exercise_per_week[data$times_exercise_per_week == 2] <- 300
        data$times_exercise_per_week[data$times_exercise_per_week == 1] <- 400
        data$times_exercise_per_week[data$times_exercise_per_week == 400] <- 4
        data$times_exercise_per_week[data$times_exercise_per_week == 300] <- 3
        data$times_exercise_per_week[data$times_exercise_per_week == 200] <- 2
        data$times_exercise_per_week[data$times_exercise_per_week == 100] <- 1
    } else if (cohort == 'WSAS-II-2016') {
        data$age <- 2016 - data$age
        data$gender <- data$gender - 1 # 0 = man, 1 = woman, while it was 1 and 2, initially, respectively
        data$asthma_family_history[data$asthma_family_history == 99] <- NA
        data$asthma_physician_diagnosed_age[data$asthma_physician_diagnosed_age > 75] <- NA
        data$copd_family_history[data$copd_family_history == 99] <- NA
        data$rhinitis_conjunctivitis_family_history[data$rhinitis_conjunctivitis_family_history == 99] <- NA
        data$smoking_currently[data$smoking_currently == 99] <- NA
        data$smoking_previously[data$smoking_previously == 99] <- NA
        data$cigarettes_per_day <- data$cigarettes_per_day + 1
        data$age_start_smoking[data$age_start_smoking > 75] <- NA
        data$age_quit_smoking[data$age_quit_smoking > 75] <- NA
        data$rural_childhood[data$rural_childhood == 99] <- NA
        data$farm_childhood[data$farm_childhood == 99] <- NA
        data$occupational_vgdf_exposure[data$occupational_vgdf_exposure == 99] <- NA
        data$times_exercise_per_week[data$times_exercise_per_week >= 5] <- 0
        data$times_exercise_per_week[data$times_exercise_per_week == 4] <- 100
        data$times_exercise_per_week[data$times_exercise_per_week == 3] <- 200
        data$times_exercise_per_week[data$times_exercise_per_week == 2] <- 300
        data$times_exercise_per_week[data$times_exercise_per_week == 1] <- 400
        data$times_exercise_per_week[data$times_exercise_per_week == 400] <- 4
        data$times_exercise_per_week[data$times_exercise_per_week == 300] <- 3
        data$times_exercise_per_week[data$times_exercise_per_week == 200] <- 2
        data$times_exercise_per_week[data$times_exercise_per_week == 100] <- 1
    }
    if (debug) { print('performed cohort-specific data cleaning') }
    # update data type where needed
    data$age <- as.numeric(data$age) # background variables
    data$gender <- as.factor(data$gender)
    data$height <- as.numeric(data$height)
    data$weight <- as.numeric(data$weight)
    data$bmi <- as.numeric(data$bmi)
    data$asthma_family_history <- as.factor(data$asthma_family_history)
    data$asthma_self_reported <- as.factor(data$asthma_self_reported)
    data$asthma_physician_diagnosed <- as.factor(data$asthma_physician_diagnosed)
    data$asthma_physician_diagnosed_age <- as.numeric(data$asthma_physician_diagnosed_age)
    data$asthma_medication_use <- as.factor(data$asthma_medication_use)
    data$asthma_hospitalization <- as.factor(data$asthma_hospitalization)
    data$copd_family_history <- as.factor(data$copd_family_history)
    data$copd_self_reported <- as.factor(data$copd_self_reported)
    data$copd_physician_diagnosed <- as.factor(data$copd_physician_diagnosed)
    data$copd_medication_use <- as.factor(data$copd_medication_use)
    data$rhinitis_conjunctivitis_family_history <- as.factor(data$rhinitis_conjunctivitis_family_history)
    data$chronic_sinusitis_physician_diagnosed <- as.factor(data$chronic_sinusitis_physician_diagnosed)
    data$rash_6m <- as.factor(data$rash_6m)
    data$rash_6m_12m <- as.factor(data$rash_6m_12m)
    data$eczema_skin_allergy <- as.factor(data$eczema_skin_allergy)
    data$rash_only_hands <- as.factor(data$rash_only_hands)
    data$snoring_loudly <- as.factor(data$snoring_loudly)
    data$difficulty_falling_asleep <- as.factor(data$difficulty_falling_asleep)
    data$waking_up_night <- as.factor(data$waking_up_night)
    data$sleepy_during_day <- as.factor(data$sleepy_during_day)
    data$waking_up_unable_fall_asleep <- as.factor(data$waking_up_unable_fall_asleep)
    data$sleep_medication_use <- as.factor(data$sleep_medication_use)
    data$other_lung_disease_self_reported <- as.factor(data$other_lung_disease_self_reported)
    data$antihypertensive_medication_use <- as.factor(data$antihypertensive_medication_use)
    data$diabetes_medication_use <- as.factor(data$diabetes_medication_use)
    data$smoking_currently <- as.factor(data$smoking_currently)
    levels(data$smoking_currently) <- c(0, 1, 9)
    data$smoking_previously <- as.factor(data$smoking_previously)
    data$smoking_status <- as.factor(data$smoking_status)
    data$cigarettes_per_day <- as.factor(data$cigarettes_per_day)
    data$age_start_smoking <- as.numeric(data$age_start_smoking)
    data$age_quit_smoking <- as.numeric(data$age_quit_smoking)
    data$daily_snuff_6m <- as.factor(data$daily_snuff_6m)
    data$daily_snuff_6m_currently <- as.factor(data$daily_snuff_6m_currently)
    data$snuff_status <- as.factor(data$snuff_status)
    data$rural_childhood <- as.factor(data$rural_childhood)
    data$farm_childhood <- as.factor(data$farm_childhood)
    data$occupational_vgdf_exposure <- as.factor(data$occupational_vgdf_exposure)
    data$highest_academic_degree <- as.factor(data$highest_academic_degree)
    levels(data$highest_academic_degree) <- c(1, 2, 3, 4, 5, 6, 7, 100, 200, 300)
    data$SEI <- as.numeric(data$SEI)
    data$times_exercise_per_week <- as.factor(data$times_exercise_per_week)
    data$CCI <- as.factor(data$CCI)
    data$time_0 <- as.Date(data$time_0, format = '%Y-%m-%d') # mortality variables
    data$deceased <- as.factor(data$deceased)
    data$time_deceased_cardiovascular <- as.factor(data$time_deceased_cardiovascular)
    data$time_deceased_neurological <- as.factor(data$time_deceased_neurological)
    data$time_deceased_respiratory <- as.factor(data$time_deceased_respiratory)
    data$time_deceased_lower_respiratory <- as.factor(data$time_deceased_lower_respiratory)
    data$time_deceased_asthma <- as.factor(data$time_deceased_asthma)
    data$time_deceased_cancer <- as.factor(data$time_deceased_cancer)
    data$time_deceased_lung_cancer <- as.factor(data$time_deceased_lung_cancer)
    data$time_deceased_other <- as.factor(data$time_deceased_other)
    data$time_deceased <- as.Date(data$time_deceased, format = '%Y-%m-%d')
    data$time_deceased_cardiovascular <- as.Date(data$time_deceased_cardiovascular, format = '%Y-%m-%d')
    data$time_deceased_neurological <- as.Date(data$time_deceased_neurological, format = '%Y-%m-%d')
    data$time_deceased_respiratory <- as.Date(data$time_deceased_respiratory, format = '%Y-%m-%d')
    data$time_deceased_lower_respiratory <- as.Date(data$time_deceased_lower_respiratory, format = '%Y-%m-%d')
    data$time_deceased_asthma <- as.Date(data$time_deceased_asthma, format = '%Y-%m-%d')
    data$time_deceased_cancer <- as.Date(data$time_deceased_cancer, format = '%Y-%m-%d')
    data$time_deceased_lung_cancer <- as.Date(data$time_deceased_lung_cancer, format = '%Y-%m-%d')
    data$time_deceased_other <- as.Date(data$time_deceased_other, format = '%Y-%m-%d')
    data$respiratory_symptoms <- as.factor(data$respiratory_symptoms) # aggregated details on respiratory symptoms
    data$respiratory_symptoms_n <- as.factor(data$respiratory_symptoms_n)
    data$attack_10y <- as.factor(data$attack_10y) # respiratory symptom variables
    data$attack_12m <- as.factor(data$attack_12m)
    data$attack_exercise <- as.factor(data$attack_exercise)
    data$attack_cold <- as.factor(data$attack_cold)
    data$attack_dust <- as.factor(data$attack_dust)
    data$attack_tobacco <- as.factor(data$attack_tobacco)
    data$attack_fume <- as.factor(data$attack_fume)
    data$attack_dust_tobacco_fume <- as.factor(data$attack_dust_tobacco_fume)
    data$attack_cold_dust_tobacco_fume <- as.factor(data$attack_cold_dust_tobacco_fume)
    data$attack_pollen <- as.factor(data$attack_pollen)
    data$attack_fur <- as.factor(data$attack_fur)
    data$attack_pollen_fur <- as.factor(data$attack_pollen_fur)
    data$dyspnea_painkiller <- as.factor(data$dyspnea_painkiller)
    data$wheezing_12m <- as.factor(data$wheezing_12m)
    data$wheezing_sob_12m <- as.factor(data$wheezing_sob_12m)
    data$wheezing_wo_cold_12m <- as.factor(data$wheezing_wo_cold_12m)
    data$wheezing_recurrent <- as.factor(data$wheezing_recurrent)
    data$cough_longstanding_12m <- as.factor(data$cough_longstanding_12m)
    data$cough_productive_recurrent <- as.factor(data$cough_productive_recurrent)
    data$cough_productive_3m <- as.factor(data$cough_productive_3m)
    data$cough_productive_3m_2y <- as.factor(data$cough_productive_3m_2y)
    data$dyspnea_ground_level_walking <- as.factor(data$dyspnea_ground_level_walking)
    data$woken_by_sob <- as.factor(data$woken_by_sob)
    data$woken_by_cough <- as.factor(data$woken_by_cough)
    data$woken_by_chest_tightness <- as.factor(data$woken_by_chest_tightness)
    data$woken_by_sob_cough_chest_tightness <- as.factor(data$woken_by_sob_cough_chest_tightness)
    data$rhinitis <- as.factor(data$rhinitis)
    data$rhinitis_currently <- as.factor(data$rhinitis_currently)
    data$rhinitis_12m <- as.factor(data$rhinitis_12m)
    data$rhinitis_5d <- as.factor(data$rhinitis_5d)
    data$rhinitis_5d_5w <- as.factor(data$rhinitis_5d_5w)
    data$rhinitis_conjunctivitis <- as.factor(data$rhinitis_conjunctivitis)
    data$nasal_obstruction_recurrent <- as.factor(data$nasal_obstruction_recurrent)
    data$rhinorrhea_recurrent <- as.factor(data$rhinorrhea_recurrent)
    data$nasal_obstruction_13w <- as.factor(data$nasal_obstruction_13w)
    data$aching_sinus_13w <- as.factor(data$aching_sinus_13w)
    data$nasal_secretion_13w <- as.factor(data$nasal_secretion_13w)
    data$reduced_smell_13w <- as.factor(data$reduced_smell_13w)
    # add cohort to merged data
    merged_data <- rbind(merged_data, data)
}



# function to remove outliers according to 1.5 IQR rule
remove_outliers <- function(feature) {
    if (debug) print(paste('removing outliers for feature', feature))
    feature_data <- as.numeric(unlist(merged_data[, feature, with = FALSE]))
    min_value <- min(feature_data, na.rm = TRUE)
    max_value <- max(feature_data, na.rm = TRUE)
    iqr_value <- IQR(feature_data, na.rm = TRUE)
    threshold_value <- 1.5 * iqr_value
    lower_threshold <- quantile(feature_data, 0.25, na.rm = TRUE) - threshold_value
    upper_threshold <- quantile(feature_data, 0.75, na.rm = TRUE) + threshold_value
    below_threshold_n <- nrow(merged_data[merged_data[[feature]] < lower_threshold, ])
    above_threshold_n <- nrow(merged_data[merged_data[[feature]] > upper_threshold, ])
    below_threshold_percentage <- round(below_threshold_n / nrow(merged_data) * 100, digits = 2)
    above_threshold_percentage <- round(above_threshold_n / nrow(merged_data) * 100, digits = 2)
    if (debug) print(paste('prior to removal, min =', min_value, ', max =', max_value))
    if (debug) print(paste0(below_threshold_n, ' (', below_threshold_percentage, '%) observations below threshold and ', above_threshold_n, ' (', above_threshold_percentage, '%) observations above threshold'))
    merged_data[merged_data[[feature]] < lower_threshold] <- NA
    merged_data[merged_data[[feature]] > upper_threshold] <- NA
    feature_data <- as.numeric(unlist(merged_data[, feature, with = FALSE]))
    min_value <- min(feature_data, na.rm = TRUE)
    max_value <- max(feature_data, na.rm = TRUE)
    if (debug) print(paste('after removal, min =', min_value, ', max =', max_value))
    return(merged_data[, feature, with = FALSE])
}



#############################################
### BEGIN CHARLSON COMORBIDITY INDEX CALCULATION
calculate_cci <- function(patients, Matrix) {
    #########################################################################################################
    # This script was based on the publication:
    #  
    #  Adaptation of the Charlson comorbidity index for register-based research in Sweden. 
    #  Jonas F. Ludvigsson, Peter Appelros, Johan Askling, Liisa Byberg, Juan-Jesus Carrero, Anna Mia Ekström, Magnus Ekström, Karin Ekström Smedby, 
    #  Hannes Hagström, Stefan James, Bengt Järvholm, Karl Michaelsson, Nancy L. Pedersen, Helene Sundelin, Kristina Sundquist, Johan Sundström. Clinical Epidemiology, 2021:13.
    #
    # The actual scripts (available in R, STATA and SAS) were created by Bjorn Roelstraete and Jonas Söderling. Data management: Mariam Lashkariani

    # GitHub: https://github.com/bjoroeKI/Charlson-comorbidity-index-revisited
    ##########################################################################################################

    ###################################################################################################################################################################
    ## CCI

    # Myocardial_infarction
    icd7  <- "\\<420,1"
    icd8  <- "\\<410|\\<411|\\<412,01|\\<412,91"
    icd9  <- "\\<410|\\<412"
    icd10 <- "\\<I21|\\<I22|\\<I252"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9  <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10 <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Myocardial_infarction=datum,diagnos.Myocardial_infarction=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Myocardial_infarction=if_else(!is.na(date.Myocardial_infarction),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Congestive_heart_failure
    icd7  <- "\\<422,21|\\<422,22|\\<434,1|\\<434,2"
    icd8  <- "\\<425,08|\\<425,09|\\<427|\\<427,1|\\<428"
    icd9  <- paste(c("\\<402A", "402B", "402X", "404A","404B","404X","425E","425F","425H","425W","425X","428"),collapse="|\\<")
    icd10 <- "\\<I110|\\<I130|\\<I132|\\<I255|\\<I420|\\<I426|\\<I427|\\<I428|\\<I429|\\<I43|\\<I50"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10 <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Congestive_heart_failure=datum,diagnos.Congestive_heart_failure=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Congestive_heart_failure=if_else(!is.na(date.Congestive_heart_failure),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Peripheral_vascular_disease
    icd7  <- "\\<450,1|\\<451|\\<453"
    icd8  <- "\\<440|\\<441|\\<443,1|\\<443,9"
    icd9  <- "\\<440|\\<441|\\<443B|\\<443X|\\<447B|\\<557"
    icd10 <- "\\<I70|\\<I71|\\<I731|\\<I738|\\<I739|\\<I771|\\<I790|\\<I792|\\<K55"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Peripheral_vascular_disease=datum,diagnos.Peripheral_vascular_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Peripheral_vascular_disease=if_else(!is.na(date.Peripheral_vascular_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Cerebrovascular_disease
    icd7  <- paste(c("\\<330",331:334),collapse="|\\<")
    icd8  <- "\\<430|\\<431|\\<432|\\<433|\\<434|\\<435|\\<436|\\<437|\\<438"
    icd9  <- "\\<430|\\<431|\\<432|\\<433|\\<434|\\<435|\\<436|\\<437|\\<438"
    icd10 <- "\\<G45|\\<I60|\\<I61|\\<I62|\\<I63|\\<I64|\\<I67|\\<I69"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Cerebrovascular_disease=datum,diagnos.Cerebrovascular_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Cerebrovascular_disease=if_else(!is.na(date.Cerebrovascular_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Chronic_obstructive_pulmonary_disease
    icd7  <- "\\<502|\\<527,1"
    icd8  <- "\\<491|\\<492"
    icd9  <- "\\<491|\\<492|\\<496"
    icd10 <- "\\<J43|\\<J44"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Chronic_obstructive_pulmonary_disease=datum,diagnos.Chronic_obstructive_pulmonary_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Chronic_obstructive_pulmonary_disease=if_else(!is.na(date.Chronic_obstructive_pulmonary_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Chronic_other_pulmonary_disease
    icd7  <- paste(c("\\<241",501,523:526),collapse="|\\<")
    icd8  <- paste(c("\\<490",493,515:518),collapse="|\\<")
    icd9  <- paste(c("\\<490",493:495,500:508,516,517),collapse="|\\<")
    icd10 <- paste(c("\\<J45",41,42,46,47,60:70),collapse="|\\<J")

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Chronic_other_pulmonary_disease=datum,diagnos.Chronic_other_pulmonary_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Chronic_other_pulmonary_disease=if_else(!is.na(date.Chronic_other_pulmonary_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Rheumatic_disease
    icd7  <- paste(c("\\<710",722,"722,23","456,0","456,1","456,2","456,3"),collapse="|\\<")
    icd8  <- paste(c("\\<446",696,"712,0","712,1","712,2","712,3","712,5", 716, 734, "734,1", "734,9"),collapse="|\\<")
    icd9  <- paste(c("\\<446","696A","710A","710B","710C","710D","710E",714,"719D",720,725),collapse="|\\<")
    icd10 <- paste(c("\\<M05","06",123,"070","071","072","073","08",13,30,313:316,32:34,350:351,353,45:46),collapse="|\\<M")

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Rheumatic_disease=datum,diagnos.Rheumatic_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Rheumatic_disease=if_else(!is.na(date.Rheumatic_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Dementia
    icd7  <- "\\<304|\\<305"
    icd8  <- "\\<290"
    icd9  <- "\\<290|\\<294B|\\<331A|\\<331B|\\<331C|\\<331X"
    icd10 <- "\\<F00|\\<F01|\\<F02|\\<F03|\\<F051|\\<G30|\\<G311|\\<G319"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Dementia=datum,diagnos.Dementia=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Dementia=if_else(!is.na(date.Dementia),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Hemiplegia
    icd7 <- "\\<351|\\<352|\\<357"
    icd8 <- "\\<343|\\<344"
    icd9 <- "\\<342|\\<343|\\<344A|\\<344B|\\<344C|\\<344D|\\<344E|\\<344F"
    icd10 <- "\\<G114|\\<G80|\\<G81|\\<G82|\\<G830|\\<G831|\\<G832|\\<G833|\\<G838"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Hemiplegia=datum,diagnos.Hemiplegia=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Hemiplegia=if_else(!is.na(date.Hemiplegia),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Diabetes_without_chronic_complication
    icd7 <- "\\<260,09"
    icd8 <-  "\\<250|\\<250,07|\\<250,08"
    icd9 <- "\\<250A|\\<250B|\\<250C"
    icd10 <- "\\<E100|\\<E101|\\<E110|\\<E111|\\<E120|\\<E121|\\<E130|\\<E131|\\<E140|\\<E141"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Diabetes_without_chronic_complication=datum,diagnos.Diabetes_without_chronic_complication=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Diabetes_without_chronic_complication=if_else(!is.na(date.Diabetes_without_chronic_complication),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Diabetes_with_chronic_complication
    icd7 <- "\\<260,2|\\<260,21|\\<260,29|\\<260,3|\\<260,4|\\<260,49|\\<260,99"
    icd8 <- "\\<250,01|\\<250,02|\\<250,03|\\<250,04|\\<250,05"
    icd9 <- "\\<250D|\\<250E|\\<250F|\\<250G"
    icd10 <- "\\<E102|\\<E103|\\<E104|\\<E105|\\<E107|\\<E112|\\<E113|\\<E114|\\<E115|\\<E116|\\<E117|\\<E122|\\<E123|\\<E124|\\<E125|\\<E126|\\<E127|\\<E132|\\<E133|\\<E134|\\<E135|\\<E136|\\<E137|\\<E142|\\<E143|\\<E144|\\<E145|\\<E146|\\<E147"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Diabetes_with_chronic_complication=datum,diagnos.Diabetes_with_chronic_complication=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Diabetes_with_chronic_complication=if_else(!is.na(date.Diabetes_with_chronic_complication),1,0,missing=0))
    Matrix <- Matrix %>% mutate(Diabetes_without_chronic_complication=if_else(Diabetes_with_chronic_complication==1,0,Diabetes_without_chronic_complication))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Renal_disease
    icd7 <- "\\<592|\\<593|\\<792"
    icd8 <- "\\<582|\\<583|\\<584|\\<792|\\<593|\\<403,99|\\<404,99|\\<792,99|\\<Y29,01"
    icd9 <- "\\<403A|\\<403B|\\<403X|\\<582|\\<583|\\<585|\\<586|\\<588A|\\<V42A|\\<V45B|\\<V56"
    icd10 <- "\\<I120|\\<I131|\\<N032|\\<N033|\\<N034|\\<N035|\\<N036|\\<N037|\\<N052|\\<N053|\\<N054|\\<N055|\\<N056|\\<N057|\\<N11|\\<N18|\\<N19|\\<N250|\\<Q611|\\<Q612|\\<Q613|\\<Q614|\\<Z49|\\<Z940|\\<Z992"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Renal_disease=datum,diagnos.Renal_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Renal_disease=if_else(!is.na(date.Renal_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Mild_liver_disease
    icd7  <- "\\<581"
    icd8  <- "\\<070|\\<571|\\<573"
    icd9 <-  "\\<070|\\<571C|\\<571E|\\<571F|\\<573"
    icd10 <- "\\<B15|\\<B16|\\<B17|\\<B18|\\<B19|\\<K703|\\<K709|\\<K73|\\<K746|\\<K754"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Mild_liver_disease=datum,diagnos.Mild_liver_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Mild_liver_disease=if_else(!is.na(date.Mild_liver_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # liver special
    icd8  <- "\\<785,3"
    icd9 <- "\\<789F"
    icd10 <- "\\<R18"

    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.liver_special=datum,diagnos.liver_special=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Liver_special=if_else(!is.na(date.liver_special),1,0,missing=0))
    rm(icd8, icd9, icd10, ICD8, ICD9, ICD10, ptnts)

    # moderate severe liver disease
    icd7 <- "\\<462,1"
    icd8 <- "\\<456|\\<571,9|\\<573,02"
    icd9 <- "\\<456A|\\<456B|\\<456C|\\<572C|\\<572D|\\<572E"
    icd10 <-  "\\<I850|\\<I859|\\<I982|\\<I983"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Severe_liver_disease=datum,diagnos.Severe_liver_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Severe_liver_disease=if_else(!is.na(date.Severe_liver_disease),1,0,missing=0))
    Matrix <- Matrix %>% mutate(Severe_liver_disease=if_else(Mild_liver_disease==1 & Liver_special==1,1,Severe_liver_disease))
    Matrix <- Matrix %>% mutate(Mild_liver_disease=if_else(Severe_liver_disease==1,0,Mild_liver_disease))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Peptic_ulcer_disease
    icd7  <- "\\<540|\\<541|\\<542"
    icd8  <- "\\<531|\\<532|\\<533|\\<534"
    icd9 <- "\\<531|\\<532|\\<533|\\<534"
    icd10 <-"\\<K25|\\<K26|\\<K27|\\<K28"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Peptic_ulcer_disease=datum,diagnos.Peptic_ulcer_disease=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Peptic_ulcer_disease=if_else(!is.na(date.Peptic_ulcer_disease),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Malignancy
    icd7   <- paste(paste("\\<",paste(140:190,collapse = "|\\<"),sep=""), paste("|\\<",paste(192:197,collapse = "|\\<"),sep=""), paste("|\\<",paste(200:204,collapse = "|\\<"),sep=""),sep="")
    icd8   <- paste(paste("\\<",paste(c(140:172,174),collapse = "|\\<"),sep=""), paste("|\\<",paste(c(180:207,209),collapse = "|\\<"),sep=""),sep="")
    icd9   <- paste(paste("\\<",paste(140:208,collapse = "|\\<"),sep=""),sep="")
    icd10  <- paste("\\<C00|\\<C0",paste(1:9,collapse = "|\\<C0",sep=""),paste("|\\<C",paste(10:76,collapse = "|\\<C"),sep=""),paste("|\\<C",paste(81:86,collapse = "|\\<C"),sep=""),paste("|\\<C",paste(88:97,collapse = "|\\<C"),sep=""),sep="")

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.malignancy=datum,diagnos.malignancy=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Malignancy=if_else(!is.na(date.malignancy),1,0,missing=0))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Metastatic_cancer
    icd7 <- "\\<156,91|\\<198|\\<199"
    icd8 <- "\\<196|\\<197|\\<198|\\<199"
    icd9 <- "\\<196|\\<197|\\<198|\\<199A|\\<199B"
    icd10 <- "\\<C77|\\<C78|\\<C79|\\<C80"

    ICD7  <- patients[patients$datum<19690000,][grep(icd7,patients[patients$datum<19690000,]$diagnos),]
    ICD8  <- patients[patients$datum >= 19690000 & patients$datum < 19870000,][grep(icd8,patients[patients$datum >= 19690000 & patients$datum < 19870000,]$diagnos),]
    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD7,ICD8,ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Metastatic_solid_tumor=datum,diagnos.Metastatic_solid_tumor=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Metastatic_solid_tumor=if_else(!is.na(date.Metastatic_solid_tumor),1,0,missing=0))
    Matrix <- Matrix %>% mutate(Malignancy=if_else(Metastatic_solid_tumor==1,0,Malignancy))
    rm(icd7, icd8, icd9, icd10, ICD7, ICD8, ICD9, ICD10, ptnts)

    # Aids
    icd9  <- "\\<079J|\\<279K"
    icd10 <- "\\<B20|\\<B21|\\<B22|\\<B23|\\<B24|\\<F024|\\<O987|\\<R75|\\<Z114|\\<Z219|\\<Z717"

    ICD9 <- patients[patients$datum >= 19870000 & patients$datum < 19980000,][grep(icd9,patients[patients$datum >= 19870000 & patients$datum < 19980000,]$diagnos),]
    ICD10  <- patients[patients$datum >= 19970000,][grep(icd10,patients[patients$datum >= 19970000,]$diagnos),]
    ptnts <- bind_rows(ICD9,ICD10) %>% group_by(group) %>% filter(row_number(datum)==1) %>% ungroup %>% rename(date.Aids=datum,diagnos.Aids=diagnos) 
    Matrix <- left_join(Matrix,ptnts,by=c("group"="group"),copy=T)
    Matrix <- Matrix %>% mutate(Aids=if_else(!is.na(date.Aids),1,0,missing=0))
    rm(icd9, icd10, ICD9, ICD10, ptnts)

    # Calculate the unweighted comorbidity index
    Matrix$CCIunw <- Matrix$Myocardial_infarction + Matrix$Congestive_heart_failure + Matrix$Peripheral_vascular_disease + 
                Matrix$Cerebrovascular_disease + Matrix$Chronic_obstructive_pulmonary_disease + Matrix$Chronic_other_pulmonary_disease + 
                Matrix$Rheumatic_disease + Matrix$Dementia + Matrix$Hemiplegia + Matrix$Diabetes_without_chronic_complication + 
                Matrix$Diabetes_with_chronic_complication + Matrix$Renal_disease + Matrix$Mild_liver_disease + Matrix$Severe_liver_disease + 
                Matrix$Peptic_ulcer_disease + Matrix$Malignancy + Matrix$Metastatic_solid_tumor + Matrix$Aids 

    # Calculate the weighted comorbidity index
    Matrix$CCIw <- Matrix$Myocardial_infarction + Matrix$Congestive_heart_failure + Matrix$Peripheral_vascular_disease + 
                Matrix$Cerebrovascular_disease + Matrix$Chronic_obstructive_pulmonary_disease + Matrix$Chronic_other_pulmonary_disease + 
                Matrix$Rheumatic_disease + Matrix$Dementia + 2*Matrix$Hemiplegia + Matrix$Diabetes_without_chronic_complication + 
                2*Matrix$Diabetes_with_chronic_complication + 2*Matrix$Renal_disease + Matrix$Mild_liver_disease + 3*Matrix$Severe_liver_disease + 
                Matrix$Peptic_ulcer_disease + 2*Matrix$Malignancy + 6*Matrix$Metastatic_solid_tumor + 6*Matrix$Aids 

    # Delete date and diagnos information in case not needed 
    Matrix <- select(Matrix, -contains("."))

    return(Matrix)
}
### END CHARLSON COMORBIDITY INDEX CALCULATION
#############################################



# universal (cohort non-specific) variable modification
merged_data$height <- remove_outliers('height')
merged_data$weight <- remove_outliers('weight')
# asthma
merged_data$asthma_self_reported[(merged_data$asthma_self_reported == 0 & merged_data$asthma_physician_diagnosed == 1)] <- NA
merged_data$asthma_physician_diagnosed[(merged_data$asthma_self_reported == 0 | is.na(merged_data$asthma_self_reported)) & merged_data$asthma_physician_diagnosed == 1] <- NA
merged_data$asthma_physician_diagnosed_age[merged_data$asthma_physician_diagnosed != 1] <- NA
merged_data$asthma_hospitalization[merged_data$asthma_self_reported == 0 & is.na(merged_data$asthma_hospitalization)] <- 0
# copd
merged_data$copd_self_reported[(merged_data$copd_self_reported == 0 & merged_data$copd_physician_diagnosed == 1)] <- NA
merged_data$copd_physician_diagnosed[(merged_data$copd_self_reported == 0 | is.na(merged_data$copd_self_reported)) & merged_data$copd_physician_diagnosed == 1] <- NA
# rash
merged_data$rash_6m_12m[merged_data$rash_6m == 0 & is.na(merged_data$rash_6m_12m)] <- 0
merged_data$rash_only_hands[merged_data$rash_6m == 0 & is.na(merged_data$rash_only_hands)] <- 0
# smoking and snuff
merged_data$smoking_currently[merged_data$smoking_currently == 1 & merged_data$smoking_previously == 1] <- 9 # replace with 9 as a "mockup" value to remove nonsensical smoking_previosly values
merged_data$smoking_previously[merged_data$smoking_currently == 9 & merged_data$smoking_previously == 1] <- NA
merged_data$smoking_currently[merged_data$smoking_currently == 9] <- NA
merged_data$smoking_previously[merged_data$smoking_currently == 1 & is.na(merged_data$smoking_previously)] <- 0
merged_data$cigarettes_per_day[merged_data$smoking_currently != 1] <- NA
merged_data$age_start_smoking[merged_data$smoking_currently != 1 & merged_data$smoking_previously != 1] <- NA
merged_data$age_quit_smoking[merged_data$smoking_previously != 1] <- NA
merged_data$daily_snuff_6m_currently[merged_data$daily_snuff_6m == 0 & is.na(merged_data$daily_snuff_6m_currently)] <- 0
# attacks
merged_data$attack_12m[merged_data$attack_10y == 0 & is.na(merged_data$attack_12m)] <- 0
# wheezing
merged_data$wheezing_sob_12m[merged_data$wheezing_12m == 0 & is.na(merged_data$wheezing_sob_12m)] <- 0
merged_data$wheezing_wo_cold_12m[merged_data$wheezing_12m == 0 & is.na(merged_data$wheezing_wo_cold_12m)] <- 0
# coughing
merged_data$cough_productive_3m[merged_data$cough_productive_recurrent == 0 & is.na(merged_data$cough_productive_3m)] <- 0
merged_data$cough_productive_3m_2y[((merged_data$cough_productive_recurrent == 1 & merged_data$cough_productive_3m == 0) | (merged_data$cough_productive_recurrent == 0 & (merged_data$cough_productive_3m == 0 | is.nan(merged_data$cough_productive_3m)))) & is.na(merged_data$cough_productive_3m_2y)] <- 0
# rhinitis
merged_data$rhinitis_currently[merged_data$rhinitis == 0 & is.na(merged_data$rhinitis_currently)] <- 0
merged_data$rhinitis_12m[merged_data$rhinitis_currently == 0 & is.na(merged_data$rhinitis_12m)] <- 0
merged_data$rhinitis_5d[merged_data$rhinitis_currently == 0 & is.na(merged_data$rhinitis_5d)] <- 0
merged_data$rhinitis_5d_5w[((merged_data$rhinitis_currently == 0 & (merged_data$rhinitis_5d == 0 | is.na(merged_data$rhinitis_5d))) | (merged_data$rhinitis_currently == 1 & merged_data$rhinitis_5d == 0)) & is.na(merged_data$rhinitis_5d_5w)] <- 0
merged_data$rhinitis_conjunctivitis[merged_data$rhinitis_currently == 0 & is.na(merged_data$rhinitis_conjunctivitis)] <- 0
# highest academic degree
merged_data$highest_academic_degree[merged_data$highest_academic_degree == 6 | merged_data$highest_academic_degree == 7] <- 300
merged_data$highest_academic_degree[merged_data$highest_academic_degree == 3 | merged_data$highest_academic_degree == 4 | merged_data$highest_academic_degree == 5] <- 200
merged_data$highest_academic_degree[merged_data$highest_academic_degree == 1 | merged_data$highest_academic_degree == 2] <- 100
merged_data$highest_academic_degree[merged_data$highest_academic_degree == 300] <- 3
merged_data$highest_academic_degree[merged_data$highest_academic_degree == 200] <- 2
merged_data$highest_academic_degree[merged_data$highest_academic_degree == 100] <- 1
merged_data[, highest_academic_degree := droplevels(highest_academic_degree)]
# CCI
merged_data$CCI <- '0'
# SEI
merged_data$SEI <- merged_data$SEI %% 100
merged_data$SEI <- ifelse(
    merged_data$SEI == 1 | merged_data$SEI == 2 | merged_data$SEI == 3,
    7,
    ifelse(
        merged_data$SEI == 11 | merged_data$SEI == 12,
        1,
        ifelse(
            merged_data$SEI == 21 | merged_data$SEI == 22,
            2,
            ifelse(
                merged_data$SEI == 33 | merged_data$SEI == 34 | merged_data$SEI == 35 | merged_data$SEI == 36,
                3,
                ifelse(
                    merged_data$SEI == 44 | merged_data$SEI == 45 | merged_data$SEI == 46,
                    4,
                    ifelse(
                        merged_data$SEI == 54 | merged_data$SEI == 55 | merged_data$SEI == 56 | merged_data$SEI == 57 | merged_data$SEI == 60,
                        5,
                        ifelse(
                            merged_data$SEI == 76 | merged_data$SEI == 77 | merged_data$SEI == 78 | merged_data$SEI == 79 | merged_data$SEI == 86 | merged_data$SEI == 87 | merged_data$SEI == 89,
                            6,
                            8
                        )
                    )
                )
            )
        )
    )
)



##############################
# BEGIN CHARLSON COMORBIDITY INDEX
# WSAS
outpatient_data <- read_sav(paste0(input_folder, 'sav/',  'WSAS-outpatient-care.sav'))
print('loaded WSAS outpatient data')
inpatient_data <- read_sav(paste0(input_folder, 'sav/',  'WSAS-inpatient-care.sav'))
print('loaded WSAS inpatient data')
# select relevant columns from comorbidity datasets
outpatient_data <- outpatient_data %>% select(LopNr, INDATUMA, DIA1, DIA2, DIA3, DIA4, DIA5, DIA6, DIA7, DIA8, DIA9, DIA10, DIA11, DIA12, DIA13, DIA14, DIA15, DIA16, DIA17, DIA18, DIA19, DIA20, DIA21, DIA22, DIA23, DIA24, DIA25, DIA26, DIA27, DIA28, DIA29, DIA30)
outpatient_data$diagnos <- paste(outpatient_data$DIA1, outpatient_data$DIA2, outpatient_data$DIA3, outpatient_data$DIA4, outpatient_data$DIA5, outpatient_data$DIA6, outpatient_data$DIA7, outpatient_data$DIA8, outpatient_data$DIA9, outpatient_data$DIA10, outpatient_data$DIA11, outpatient_data$DIA12, outpatient_data$DIA13, outpatient_data$DIA14, outpatient_data$DIA15, outpatient_data$DIA16, outpatient_data$DIA17, outpatient_data$DIA18, outpatient_data$DIA19, outpatient_data$DIA20, outpatient_data$DIA21, outpatient_data$DIA22, outpatient_data$DIA23, outpatient_data$DIA24, outpatient_data$DIA25, outpatient_data$DIA26, outpatient_data$DIA27, outpatient_data$DIA28, outpatient_data$DIA29, outpatient_data$DIA30)
outpatient_data <- outpatient_data %>% select(LopNr, INDATUMA, diagnos)
outpatient_data <- rename(outpatient_data, person_id = LopNr, datum = INDATUMA)
outpatient_data$person_id <- as.numeric(outpatient_data$person_id)
# inpatient data
inpatient_data <- inpatient_data %>% select(LopNr, INDATUMA, DIA1, DIA2, DIA3, DIA4, DIA5, DIA6, DIA7, DIA8, DIA9, DIA10, DIA11, DIA12, DIA13, DIA14, DIA15, DIA16, DIA17, DIA18, DIA19, DIA20, DIA21, DIA22, DIA23, DIA24, DIA25, DIA26, DIA27, DIA28, DIA29, DIA30)
inpatient_data$diagnos <- paste(inpatient_data$DIA1, inpatient_data$DIA2, inpatient_data$DIA3, inpatient_data$DIA4, inpatient_data$DIA5, inpatient_data$DIA6, inpatient_data$DIA7, inpatient_data$DIA8, inpatient_data$DIA9, inpatient_data$DIA10, inpatient_data$DIA11, inpatient_data$DIA12, inpatient_data$DIA13, inpatient_data$DIA14, inpatient_data$DIA15, inpatient_data$DIA16, inpatient_data$DIA17, inpatient_data$DIA18, inpatient_data$DIA19, inpatient_data$DIA20, inpatient_data$DIA21, inpatient_data$DIA22, inpatient_data$DIA23, inpatient_data$DIA24, inpatient_data$DIA25, inpatient_data$DIA26, inpatient_data$DIA27, inpatient_data$DIA28, inpatient_data$DIA29, inpatient_data$DIA30)
inpatient_data <- inpatient_data %>% select(LopNr, INDATUMA, diagnos)
inpatient_data <- rename(inpatient_data, person_id = LopNr, datum = INDATUMA)
inpatient_data$person_id <- as.numeric(inpatient_data$person_id)
# combine outpatient and inpatient data
comorbidity_data <- rbind(inpatient_data, outpatient_data)
print('combined WSAS comorbidity data')
# prepare for CCI calculation
comorbidity_data <- rename(comorbidity_data, group = person_id)
comorbidity_data <- comorbidity_data %>% select(group, datum, diagnos)
comorbidity_data$diagnos <- trimws(comorbidity_data$diagnos)
Matrix <- distinct(comorbidity_data, group)
Matrix <- calculate_cci(comorbidity_data, Matrix)
Matrix <- select(Matrix, group, CCIw)
Matrix <- rename(Matrix, person_id = group)
wsas_data <- copy(merged_data[merged_data$cohort == 'WSAS-I-2008' | merged_data$cohort == 'WSAS-II-2016'])
wsas_data <- merge(wsas_data, Matrix, by = 'person_id', all.x = TRUE)
wsas_data[, CCI := ifelse(!is.na(CCIw), CCIw, 0)]
wsas_data$CCIw <- NULL
# OLIN
outpatient_data <- read_sav(paste0(input_folder, 'sav/', 'OLIN-outpatient-care.sav'))
print('loaded OLIN outpatient data')
inpatient_data <- foreign::read.spss(paste0(input_folder, 'sav/', 'OLIN-inpatient-care.sav'), to.data.frame = TRUE, password = 'XXXXXXXXXXXXXXXXXX')
print('loaded OLIN inpatient data')
# outpatient data
outpatient_data <- outpatient_data %>% select(LopNr, INDATUMA, HDIA, DIAGNOS)
outpatient_data$diagnos <- paste(outpatient_data$HDIA, outpatient_data$DIAGNOS)
outpatient_data$diagnos <- trimws(outpatient_data$diagnos)
outpatient_data <- rename(outpatient_data, person_id = LopNr, datum = INDATUMA)
outpatient_data$person_id <- as.numeric(outpatient_data$person_id)
# inpatient data
inpatient_data <- inpatient_data %>% select(LopNr, INDATUMA, HDIA, DIAGNOS)
inpatient_data$diagnos <- paste(inpatient_data$HDIA, inpatient_data$DIAGNOS)
inpatient_data$diagnos <- trimws(inpatient_data$diagnos)
inpatient_data <- rename(inpatient_data, person_id = LopNr, datum = INDATUMA)
inpatient_data$person_id <- as.numeric(inpatient_data$person_id)
# combine outpatient and inpatient data
comorbidity_data <- rbind(inpatient_data, outpatient_data)
print('combined OLIN comorbidity data')
# prepare for CCI calculation
comorbidity_data <- rename(comorbidity_data, group = person_id)
comorbidity_data <- comorbidity_data %>% select(group, datum, diagnos)
comorbidity_data$diagnos <- trimws(comorbidity_data$diagnos)
Matrix <- distinct(comorbidity_data, group)
Matrix <- calculate_cci(comorbidity_data, Matrix)
Matrix <- select(Matrix, group, CCIw)
Matrix <- rename(Matrix, person_id = group)
olin_data <- copy(merged_data[merged_data$cohort == 'OLIN-IV-1996' | merged_data$cohort == 'OLIN-VI-2006' | merged_data$cohort == 'OLIN-VII-2016'])
olin_data <- merge(olin_data, Matrix, by = 'person_id', all.x = TRUE)
olin_data[, CCI := ifelse(!is.na(CCIw), CCIw, 0)]
olin_data$CCIw <- NULL
# merge data
merged_data <- rbind(wsas_data, olin_data)
# END CHARLSON COMORBIDITY INDEX
##############################



# set unique_id (incremental integer)
merged_data[, unique_id := 1:.N]

# save merged data
if (debug) { print(table(merged_data$attack_12m, useNA = 'ifany')) }
dt <- merged_data
save(dt, file = paste0(output_folder, 'rda/', output_name, '.Rda'))