### All-cause and cause-specific mortality in respiratory symptom clusters: a population-based multicohort study

Code used to produce the results in the paper titled "All-cause and cause-specific mortality in respiratory symptom clusters: a population-based multicohort study".

Starting with the raw data in .sav (SPSS) files, the Python/R files were executed in the following order:

1. create-variables.R *(to construct an .Rda file subsequently used to reference the utilized variables and their characteristics)*
2. cleaning-and-combination-of-datasets.R *(to standardize variable naming, clean data, combine the datasets, as well as add Charlson Comorbidity Index [CCI] measures for each subject)*
3. missingness-assessment.R *(assess and visualize the missingness in the raw, non-imputed data)*
4. observe-characteristics-in-pre-imputed-datasets.R *(create summarizing table describing the raw, non-imputed data)*
5. imputation.R *(impute data, assess imputation, and form composite variables)*
6. observe-characteristics-in-pre-imputed-and-imputed-datasets.R *(create summarizing table comparing the raw, non-imputed data and the imputed data, with the additional use of pool-variable-values.R to pool variable values across the imputed datasets)*
7. prepare-data-for-clustering.R *(prepare imputed data for clustering)*
8. lsh-k-representatives.py *(perform cluster analysis)*
9. observe-characteristics-in-clusters.R *(create summarizing table comparing the clusters [and asymptomatic subjects], with the additional use of pool-variable-values.R to pool variable values across the imputed datasets)*
10. compactness.R *(measure compactness as an additional internal validation measure)*
11. jaccard.R *(measure cluster-wise stability as an additional internal validation measure)*
12. line-plot.R *(create line plots for average silhouette score, clustering objective function, and compactness)*
13. add-mortality-data.R *(add mortality [outcome] data for each subject)*
14. calculate-hazard-ratio.R *(perform mortality analyses)*
15. pool-point-estimates-and-confidence-intervals.R *(combine estimates and corresponding confidence intervals)*
16. cluster-sizes.R *(calculate the size of each cluster)*
17. respiratory-symptoms-histogram.R *(create histogram depicting how many subjects report of x number of respiratory symptoms)*