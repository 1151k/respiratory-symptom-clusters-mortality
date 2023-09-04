# pool point estimates and confidence intervals from the multiple mortality analyses



import math
import csv
import numpy as np
import sys
from scipy import stats



# filename for the csv file containing the point estimates and confidence intervals
filename = 'output/csv/results-XXXXXX.csv' # replace XXXXXX with the name of the csv file (format: results-outcome-group-cluster, e.g., all-all-4)



# read point estimates and confidence intervals from mortality analyses
with open(filename, newline='') as csvfile:
    csv = list(csv.reader(csvfile))
    csv = [[float(x) for x in row] for row in csv]
    point_estimates = [row[0] for row in csv]
    ci_lower_limits = [row[1] for row in csv]
    ci_upper_limits = [row[2] for row in csv]
point_estimates = [math.log(x) for x in point_estimates]
ci_lower_limits = [math.log(x) for x in ci_lower_limits]
ci_upper_limits = [math.log(x) for x in ci_upper_limits]
n_imputations = len(point_estimates)



# calculate the pooled point estimate, first log-converting
pooled_point_estimate = sum([log_hr for log_hr in point_estimates]) / n_imputations

# make a new variable called variance, which will be a list of the within-imputation variances
within_imputation_variance = [((log_ci_upper - log_ci_lower) / (2 * 1.96)) ** 2 for log_ci_upper, log_ci_lower in zip(ci_upper_limits, ci_lower_limits)]
pooled_within_imputation_variance = sum(within_imputation_variance) / n_imputations

# calculate the between-imputation variance, with pooled_point_estimate being the pooled mean
between_imputation_variance = sum([(log_point_estimate - pooled_point_estimate) ** 2 for log_point_estimate in point_estimates]) / (n_imputations - 1)

# calculate the total variance according to Rubin's rule
total_variance = pooled_within_imputation_variance + between_imputation_variance*(1+1/n_imputations)

# calculate the pooled hazard ratio and confidence interval according to Rubin's rule
# calculate degrees of freedom based on https://bookdown.org/mwheymans/bookmi/rubins-rules.html
n = 56206
k = 70
l = (between_imputation_variance+(between_imputation_variance/n_imputations))/total_variance
dof_old = (n_imputations-1)/l
dof_observed = (((n-k)+1)/((n-k)+3))*(n-k)*(1-l)
dof_adjusted = (dof_old * dof_observed) / (dof_old + dof_observed)
alpha = 0.05
t = stats.t.ppf(1 - alpha/2, dof_adjusted)

# calculate the pooled hazard ratio and confidence interval
pooled_hazard_ratio = math.exp(pooled_point_estimate)
pooled_ci_lower = math.exp(pooled_point_estimate - t * (total_variance ** 0.5))
pooled_ci_upper = math.exp(pooled_point_estimate + t * (total_variance ** 0.5))

# print pooled hazard ratio and confidence interval
print('results for ' + filename)
print('pooled hazard ratio and 95% CI')
print((pooled_hazard_ratio, pooled_ci_lower, pooled_ci_upper))