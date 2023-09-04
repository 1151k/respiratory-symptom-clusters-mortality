# calculate hazard ratio and corresponding 95%CI, please see comments for the (un)commenting one ought to make to perform all (sub)analayses



# Load packages
packages_all = c("cmprsk", "haven", "dplyr", "survival", "survminer", "ggplot2")
packages_installed = packages_all %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  install.packages(packages_all[!packages_installed])
}
invisible(lapply(packages_all, library, character.only = TRUE))
# Load custom scripts, functions, and data
load('output/rda/VARIABLES.Rda')



# constants
investigated_decease_causes <- list( # comment/uncomment according to specific analysis (such as the more limited list for the most common causes of respiratory and cardiovascular death)
    'time_deceased_cardiovascular',
    'time_deceased_heart_failure',
    'time_deceased_coronary_heart',
    'time_deceased_arrhythmia',
    'time_deceased_cerebrovascular',
    'time_deceased_peripheral_vascular',
    'time_deceased_neurological',
    'time_deceased_respiratory',
    'time_deceased_lower_respiratory',
    'time_deceased_copd',
    'time_deceased_lower_respiratory_infection',
    'time_deceased_gastrointestinal',
    'time_deceased_cancer',
    'time_deceased_lung_cancer'
)
subgroup <- '' # placeholder for subgroup name (don't change this)
gender_selected <- '' # when investigating by subgroup of gender, set to either 0 (men) or 1 (women)
age_selected <- '' # when investigating by subgroup of age, set to either '<30', '30-60', or '>60'
CCI_selected <- '' # when investigating by subgroup of CCI, set to either '0', '1-2', or '≥3'
cause_specific_analysis <- TRUE # set to TRUE to perform cause-specific analysis, set to FALSE to perform all-cause analysis
i_range <- c(1:100)
k <- 7
end_date_OLIN <- as.Date("2016-12-31")
end_date_WSAS <- as.Date("2021-12-31")
investigated_deceased_codes <- list(
    'time_deceased_cardiovascular' = 1,
    'time_deceased_heart_failure' = 2,
    'time_deceased_coronary_heart' = 3,
    'time_deceased_arrhythmia' = 4,
    'time_deceased_cerebrovascular' = 5,
    'time_deceased_peripheral_vascular' = 6,
    'time_deceased_neurological' = 7,
    'time_deceased_respiratory' = 8,
    'time_deceased_lower_respiratory' = 9,
    'time_deceased_copd' = 10,
    'time_deceased_lower_respiratory_infection' = 11,
    'time_deceased_gastrointestinal' = 12,
    'time_deceased_cancer' = 13,
    'time_deceased_lung_cancer' = 14
)
gender_data <- list(
    'men' = 0,
    'women' = 1
)



# begin looping through datasets and the outcomes
for (investigated_decease_cause in investigated_decease_causes) {
    if (cause_specific_analysis == TRUE) {
        print(paste0('BEGIN', ' - ', investigated_decease_cause))
    } else {
        print('BEGIN ALL-CAUSE MORTALITY ANALYSIS')
    }
    # create dataframes for storing results
    for (i in 1:k) {
        df_name <- paste0('c', i, '_csv')
        assign(df_name, data.frame('point_estimate' = numeric(), 'lower_bound' = numeric(), 'upper_bound' = numeric()))
    }
    for (dataset_i in i_range) {
        print(paste('DATASET', dataset_i))
        # load and prepare data
        df <- readRDS(paste0('output/rda/mortality-loaded-data-', dataset_i, '.Rda'))
        # select only subjects from cohorts other than OLIN-VII-2016
        df <- df[df$cohort != 'OLIN-VII-2016',]

        # add start date for each cohort
        df$start_date <- ifelse(
            df$cohort == 'OLIN-IV-1996',
            '1996-01-01',
            ifelse(
                df$cohort == 'WSAS-I-2008',
                '2008-01-01',
                ifelse(
                    df$cohort == 'OLIN-VI-2006',
                    '2006-01-01',
                    ifelse(
                        df$cohort == 'WSAS-II-2016',
                        '2016-01-01',
                        NA
                    )
                )
            )
        )

        # calculate survival time and set deceased where appropriate
        if (CCI_selected != '') {
            if (CCI_selected == '0') {
                df <- df[df$CCI == 0, ]
            } else if (CCI_selected == '1-2') {
                df <- df[df$CCI == 1 | df$CCI == 2, ]
            } else if (CCI_selected == '≥3') {
                df <- df[df$CCI >= 3, ]
            }
            subgroup <- paste0('CCI-', CCI_selected)
        }
        if (age_selected != '') {
            if (age_selected == '>60') {
                df <- df[df$age > 60, ]
            } else if (age_selected == '30-60') {
                df <- df[df$age >= 30 & df$age <= 60, ]
            } else if (age_selected == '<30') {
                df <- df[df$age < 30, ]
            } else if (age_selected == '≤60') {
                df <- df[df$age <= 60, ]
            }
            subgroup <- paste0('age-', age_selected)
        }
        if (gender_selected != '') {
            df <- df[df$gender == gender_data[gender_selected], ]
            subgroup <- paste0('gender-', gender_selected)
        }
        if (cause_specific_analysis == TRUE) {
            df$deceased <- ifelse(!is.na(df[[investigated_decease_cause]]), 1, 0)
        } else {
            df$deceased <- ifelse(!is.na(df$time_deceased), 1, 0)
        }
        # deceased codes
        df$decease_code <- ifelse(
            !is.na(df[[investigated_decease_cause]]),
            investigated_deceased_codes[[investigated_decease_cause]],
            ifelse(
                !is.na(df$time_deceased),
                15, # other cause of death
                0
            )
        )
        df$time_deceased <- ifelse(
            !is.na(df$time_deceased),
            difftime(df$time_deceased, df$start_date, units = 'days'),
            ifelse(
                df$cohort == 'OLIN-IV-1996' | df$cohort == 'OLIN-VI-2006',
                difftime(end_date_OLIN, df$start_date, units = 'days'),
                ifelse(
                    df$cohort == 'WSAS-I-2008' | df$cohort == 'WSAS-II-2016',
                    difftime(end_date_WSAS, df$start_date, units = 'days'),
                    NA
                )
            )
        )
        df$time_deceased <- as.numeric(df$time_deceased)
        print('years of follow-up')
        print(sum(df$time_deceased)/365)
        # remove unnecessary columns
        df <- df[, c('cluster', 'decease_code', 'time_deceased', 'deceased', 'age', 'gender', 'asthma_physician_diagnosed', 'rhinitis_currently', 'chronic_sinusitis_physician_diagnosed', 'copd_physician_diagnosed', 'other_lung_disease_self_reported', 'antihypertensive_medication_use', 'diabetes_medication_use', 'sleep_medication_use', 'highest_academic_degree', 'rural_childhood', 'farm_childhood', 'bmi', 'smoking_status', 'SEI', 'occupational_vgdf_exposure', 'CCI')]

        # rename clusters
        df$cluster[is.na(df$cluster)] <- 99
        df$cluster[df$cluster == 0] <- 100
        df$cluster[df$cluster == 1] <- 200
        df$cluster[df$cluster == 2] <- 300
        df$cluster[df$cluster == 3] <- 400
        df$cluster[df$cluster == 4] <- 500
        df$cluster[df$cluster == 5] <- 600
        df$cluster[df$cluster == 6] <- 700
        df$cluster[df$cluster == 7] <- 800
        df$cluster[df$cluster == 8] <- 900
        df$cluster[df$cluster == 9] <- 1000
        df$cluster[df$cluster == 99] <- 1
        df$cluster[df$cluster == 100] <- 2
        df$cluster[df$cluster == 200] <- 3
        df$cluster[df$cluster == 300] <- 8
        df$cluster[df$cluster == 400] <- 6
        df$cluster[df$cluster == 500] <- 5
        df$cluster[df$cluster == 600] <- 7
        df$cluster[df$cluster == 700] <- 4
        df$cluster[df$cluster == 800] <- 9
        df$cluster[df$cluster == 900] <- 10
        df$cluster[df$cluster == 1000] <- 11
        df$cluster <- as.factor(df$cluster)

        # transform CCI and SEI
        df$CCI <- as.numeric(df$CCI)
        df$CCI[df$CCI > 3] <- 3
        df$SEI <- as.numeric(df$SEI)
        df$SEI[df$SEI > 6] <- 7

        # Cox proportional hazards model
        if (cause_specific_analysis == FALSE) {
            print('Cox proportional hazards model')
            # Kaplan-Meier curves
            sfit <- survfit(Surv(time_deceased, deceased)~cluster, data=df)
            gg <- ggsurvplot(sfit,
                    palette = c("#999999", "#76B64C", "#BFB03B", "#E99251", "#C7695E", "#8553AD", "#608097", "#64B7B1"),
                    surv.scale = "percent",
                    risk.table=TRUE,
                    legend.labs = c("Asymptomatic", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7"),
                    xlab = "Time (years)",
                    ylab = "Probability of survival (%)",
                    fontsize = 4,
                    xscale = 'd_y',
                    risk.table.title = 'Number at risk',
                    # break axis at 1 year intervals
                    break.x.by = 365.25*7
            )
            gg$plot <- gg$plot + guides(color = guide_legend(override.aes = list(color = c("#999999", "#76B64C", "#BFB03B", "#E99251", "#C7695E", "#8553AD", "#608097", "#64B7B1"))))
            gg$plot <- gg$plot + theme(
                strip.text.x = element_text(hjust = 0),
                plot.margin = margin(5, 20, 5, 5)
            )
            ggsave(paste0('output/svg/km-curve.svg'), plot = gg$plot, dpi = 300, width = 8, height = 5)
            ggsave(paste0('output/svg/km-table.svg'), plot = gg$table, dpi = 300, width = 8, height = 3)
            # below is the full set of accounted for covariates; the list is changed accordingly when performing subanalyses etc.
            # + age + factor(smoking_status) + bmi + factor(gender) + factor(highest_academic_degree) + factor(SEI) + factor(occupational_vgdf_exposure) + factor(CCI)
            fit = coxph(Surv(time_deceased, deceased) ~ factor(cluster) + age + factor(CCI) + factor(gender) + factor(smoking_status) + bmi + factor(highest_academic_degree) + factor(occupational_vgdf_exposure) + factor(SEI), data = mutate(df, cluster = relevel(cluster, ref=1)))
            print(summary(fit))
            # loop schoenfeld_residual_plots and save each plot in an svg in the current folder
            proprtionality_test <- cox.zph(fit)
            print(proprtionality_test)
            schoenfeld_residual_plots <- survminer::ggcoxzph(proprtionality_test)
            for (i in 1:length(schoenfeld_residual_plots)) {
                ggsave(paste0('output/png/schoenfeld-residual-', names(schoenfeld_residual_plots)[i], '.png'), plot = schoenfeld_residual_plots[[i]], dpi = 200, width = 8, height = 4.5)
            }
            # save results for pooling
            for (j in 1:k) {
                csv_row <- c(summary(fit)$conf.int[j, c(1,3,4)])
                df_name <- paste0('c', j, '_csv')
                assign(df_name, rbind(get(df_name), csv_row))
            }
        }

        # Fine-Gray model
        if (cause_specific_analysis == TRUE) {
            print('Fine-Gray')
            if (gender_selected != '') {
                print('gender-specific analysis selected')
                cov1 <- model.matrix(~ factor(cluster) + age + factor(smoking_status) + bmi + factor(highest_academic_degree) + factor(SEI) + factor(occupational_vgdf_exposure) + factor(CCI), data = df) [, -1] # complete: + age + factor(smoking_status) + bmi + factor(highest_academic_degree) + factor(SEI) + factor(occupational_vgdf_exposure) + factor(CCI)
            } else if (CCI_selected != '') {
                print('CCI-specific analysis selected')
                cov1 <- model.matrix(~ factor(cluster) + age + factor(gender) + factor(smoking_status) + bmi + factor(highest_academic_degree) + factor(SEI) + factor(occupational_vgdf_exposure), data = df) [, -1] # complete: + age + factor(gender) + factor(smoking_status) + bmi + factor(highest_academic_degree) + factor(SEI) + factor(occupational_vgdf_exposure)
            } else if (age_selected != '') {
                print('age-specific analysis selected')
                cov1 <- model.matrix(~ factor(cluster) + factor(gender) + factor(smoking_status) + bmi + factor(highest_academic_degree) + factor(SEI) + factor(CCI) + factor(occupational_vgdf_exposure), data = df) [, -1] # complete: + factor(gender) + factor(smoking_status) + bmi + factor(highest_academic_degree) + factor(SEI) + factor(CCI) + factor(occupational_vgdf_exposure)
            } else {
                print('no specific analysis selected')
                cov1 <- model.matrix(~ factor(cluster) + age + factor(gender) + factor(smoking_status) + bmi + factor(highest_academic_degree) + factor(SEI) + factor(CCI) + factor(occupational_vgdf_exposure), data = df) [, -1] # complete: age + factor(gender) + factor(smoking_status) + bmi + factor(highest_acadeemic_degree) + factor(SEI) + factor(CCI) + factor(occupational_vgdf_exposure)
            }
            CRR1 <- crr(ftime = df$time_deceased, fstatus = df$decease_code, failcode = investigated_deceased_codes[[investigated_decease_cause]], cencode = 0, cov1 = cov1, maxiter = 100) # cov2 = cov1[,CLUSTERNUMBER], tf = function(t) t, is added to the cov1 list when performing the analysis with time-dependent covariates (replace CLUSTERNUMBER with the cluster number of interest)
            summary(CRR1)
            print(summary(CRR1))
            # draw Schoenfeld plots for each cluster level
            par(mfrow=c(3,3))
            for (j in 1:7) {
                scatter.smooth(
                    CRR1$uft, CRR1$res[,j],
                    main =names(CRR1$coef)[j],
                    xlab = "Failure time",
                    ylab ="Schoenfeld residuals"
                )
            }
            # save to file for pooling
            for (j in 1:k) {
                csv_row <- c(summary(CRR1)$coef[j,2], summary(CRR1)$conf.int[j,3], summary(CRR1)$conf.int[j,4])
                df_name <- paste0('c', j, '_csv')
                assign(df_name, rbind(get(df_name), csv_row))
            }
        }
        # just some general information to keep track of the progress
        print(investigated_decease_cause)
        print(paste(sum(df$deceased), 'cases'))
        print(paste('dataset', dataset_i))
        print('--------')
    } # dataset loop
    # save results
    if (subgroup == '') { subgroup <- 'all' }
    if (cause_specific_analysis == FALSE) { 
        analyzed_outome <- 'all-cause'
    } else {
        analyzed_outome <- investigated_decease_cause
    }
    for (l in 1:k) {
        df_name <- paste0('c', l, '_csv')
        write.csv(get(df_name), paste0('output/csv/results-', analyzed_outome, '-', subgroup, '-', l, '.csv'))
    }
} # outcome loop