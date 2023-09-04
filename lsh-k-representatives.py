# perform cluster analysis



# load external libraries
import numpy as np
from LSHkRepresentatives.LSHkRepresentatives_Init import LSHkRepresentatives_Init
import pyreadr
from sklearn.metrics import silhouette_score
from datetime import datetime



# constants
k_min = 2
k_max = 10
dataset_min = 1
dataset_max = 100
metric = 'manhattan'
debug = True
verbose = 3 if debug else 0



# stability test (remove columns)
def remove_columns(arr, percentage):
    num_cols = arr.shape[1]
    num_cols_to_remove = int(num_cols * percentage / 100)
    cols_to_remove = np.random.choice(num_cols, num_cols_to_remove, replace=False)
    return np.delete(arr, cols_to_remove, axis=1)
# stability test (add noise)
def add_noise(arr, p):
    mask = np.random.choice([True, False], size=arr.shape, p=[p, 1-p])
    noise = np.random.choice([0, 1], size=arr.shape)
    arr[mask] = noise[mask]
    return arr



# perform clustering
ss_list_csv = []
objective_function_cost_list_csv = []
for dataset_i in range(dataset_min, dataset_max + 1):
    # load data
    X = np.asarray(pyreadr.read_r(f'output/rda/cluster-input-{dataset_i}.Rda')['df_clustering'])
    X = X.astype(int)
    if debug:
        print(f'DATASET: {dataset_i}')
        print(f'Starting time: {datetime.now().strftime("%H:%M:%S")}')
        print('============')
    # list of scores/values
    ss_list = []
    objective_function_cost_list = []
    for n_clusters in range(k_min, k_max + 1):
        # X = remove_columns(X, %) # uncomment to remove columns
        # X = add_noise(X, %) # uncomment to add noise
        Y = np.random.randint(low=0, high=6, size=len(X)) # random labels, just as a placeholder for the mandatory input arguments
        if debug:
            print(f'Shape: {X.shape}')
        kreps = LSHkRepresentatives_Init(
            X,
            Y,
            n_clusters = n_clusters,
            verbose = verbose
        )
        kreps.SetupLSH()
        # objective function value is returned to this script after changing the return value of DoCluster() to the cluster labels and the objective function cost (all_costs[best]) as the second return value (separated by comma)
        cluster_labels, objective_function_cost = kreps.DoCluster()
        if debug:
            print(f'k: {n_clusters}')
            print(f'Done at: ({datetime.now().strftime("%H:%M:%S")})')
        # save labels
        np.save(f'output/npy/labels-{dataset_i}-{n_clusters}.npy', cluster_labels)
        # get and append silhouete score
        ss = silhouette_score(X, cluster_labels, metric=metric)
        ss_list.append(ss)
        # append objective function cost
        objective_function_cost_list.append(objective_function_cost)
        if debug:
            print('------')
    # add to csv
    ss_list_csv.append(ss_list)
    objective_function_cost_list_csv.append(objective_function_cost_list)
    if debug:
        print('===========')



# save to csv
np.savetxt(f'output/csv/silhouette-score.csv', ss_list_csv, delimiter=',', fmt="%.6f")
np.savetxt(f'output/csv/objective-function.csv', objective_function_cost_list_csv, delimiter=',', fmt="%.6f")