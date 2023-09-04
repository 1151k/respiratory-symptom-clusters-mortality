import numpy as np
import pyreadr
from sklearn.metrics import jaccard_score



# constants
k_min = 2
k_max = 10
dataset_min = 2
dataset_max = 100
ground_truth_dataset = 1



# function to assign labels to clusters
def assign_labels(working_labels_truth, working_labels_iteration):
    unique_labels = np.unique(working_labels_iteration)
    for label in unique_labels:
        mask = working_labels_iteration == label
        corresponding_truth_label = np.argmax(np.bincount(working_labels_truth[mask]))
        working_labels_iteration[mask] = corresponding_truth_label
    return working_labels_iteration



# initalize a list to hold all jaccard scores
js_list_raw = []



# iterate over datasets
for dataset_i in range(dataset_min, dataset_max + 1):
    # load data
    X = np.asarray(pyreadr.read_r(f'output/rda/cluster-input-{dataset_i}.Rda')['df_clustering'])
    X = X.astype(int)
    IDs = np.asarray(pyreadr.read_r(f'output/rda/person_id_cohort-{dataset_i}.Rda')['df_ids'])
    js_list_k = {k: [] for k in range(k_min, k_max + 1)}
    print(f'Dataset: {dataset_i}')

    for k in range(k_min, k_max + 1):
        print(f'k: {k}')

        # ground truth IDs and labels
        ground_truth_IDs = np.asarray(pyreadr.read_r(f'output/rda/person_id_cohort-{ground_truth_dataset}.Rda')['df_ids'])
        ground_truth_labels_name = f'output/npy/labels-{ground_truth_dataset}-{k}.npy'
        ground_truth_labels = np.load(ground_truth_labels_name)

        # generate corresponding labels from clustering iteration
        iteration_labels_url = f'output/npy/labels-{dataset_i}-{k}.npy'
        labels = np.array(np.load(iteration_labels_url))
        working_labels_iteration = np.array([])
        working_labels_truth = np.array([])
        working_IDs = np.array([])
        for i, ID in enumerate(IDs):
            if ID in ground_truth_IDs:
                working_labels_iteration = np.append(working_labels_iteration, labels[i])
                working_labels_truth = np.append(working_labels_truth, ground_truth_labels[np.where(ground_truth_IDs == ID)[0][0]])
                working_IDs = np.append(working_IDs, ID)
        working_labels_truth = working_labels_truth.astype(int)
        working_labels_iteration = working_labels_iteration.astype(int)
        working_labels_iteration = assign_labels(working_labels_truth, working_labels_iteration)
        # calculate jaccard score
        js = jaccard_score(working_labels_truth, working_labels_iteration, average=None)

        # js = np.random.rand(k)
        print(f'Jaccard score: {js}')
        # add to dictionary
        js_list_k[k] = js
    js_list_raw.append(js_list_k)
    print('----')
print('===========')


# loop over dictionary and print average jaccard scores for each k and present the average jaccard score for each k/cluster
for k in range(k_min, k_max + 1):
    k_vals = []
    print(f'Average Jaccard score for k={k}')
    for dataset_dict in js_list_raw:
        k_vals.append(dataset_dict[k])
    for i in range(1, k + 1):
        print(f'Cluster {i}: {np.mean([x[i - 1] for x in k_vals])}')