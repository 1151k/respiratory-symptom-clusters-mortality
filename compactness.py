# calculate compactness in the derived clusters



import numpy as np
import pyreadr
from scipy.spatial.distance import cityblock
from sklearn.metrics import pairwise_distances



# constants
k_min = 2
k_max = 10
dataset_min = 1
dataset_max = 100
metric = 'cityblock'



# calculate average within-cluster distance
def avg_within_cluster_distance(data, labels):
    n_clusters = len(set(labels))
    distances_manhattan = []
    for i in range(n_clusters):
        cluster_data = data[labels == i]
        distance_manhattan = np.mean(pairwise_distances(cluster_data, metric=metric))
        distances_manhattan.append(distance_manhattan)
    mean_distance_manhattan = np.mean(distances_manhattan)
    print(f'avg distance: {mean_distance_manhattan}')
    compactness_Y.append(mean_distance_manhattan)



# iterate over datasets
compactness_Y_csv = []
for dataset_i in range(dataset_min, dataset_max + 1):
    # load data
    X = np.asarray(pyreadr.read_r(f'output/rda/cluster-input-{dataset_i}.Rda')['df_clustering'])
    X = X.astype(int)
    print(f'Dataset: {dataset_i}')
    compactness_Y = []
    # loop over k
    for k in range(k_min, k_max + 1):
        iteration_labels_url = f'output/npy/labels-{dataset_i}-{k}.npy'
        labels = np.array(np.load(iteration_labels_url))
        print(f'k: {k}')
        avg_within_cluster_distance(X, labels)
        print('----')
    # add to csv
    compactness_Y_csv.append(compactness_Y)
    print('=====================')



# save to csv
np.savetxt('output/csv/compactness.csv', compactness_Y_csv, delimiter=',', fmt="%.6f")