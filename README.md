# mRNA-based Prostate Cancer Classification

In this project, I train micro RNA-based classification models to accurately identify prostate cancer. The dataset used in the project is avaiable [here](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE112264). This project has been completed as part of *CSE 5717 - Data mining* course.

Prostate cancer is one of the common cancer types. While some types of prostate cancer grow slowly and may not need only minimal or no treatment, some other types can be very aggressive and can grow quickly. 

Prostate cancers that is detected early, has the best chance for successful treatment. However, the high false rate of prostate-specific antigens (PSA) may often lead to *negative prostate biopsies*, which does not definitively exclude the presence of cancer and often requires further investigation.


## Project Target

* Comparing *Prostate Cancer miRNA* and *healthy control miRNA*, which might help determining the divergences among the groups.
* Comparing *Negative Biopsy miRNA* to *healthy control miRNA*, to understand the deviation from normal miRNA in the Negative Biopsy miRNA.
* Work on training classifiers on detecting prostate cancer and negative prostate biopsies. 


## Applied Methods:

* Performing dimension reduction techniques, e.g., *PCA* and *tSNE* - to understand divergences among groups in lower dimensions.
* Performing clustering methods, e.g., *kmeans* - to see whether clusters can be generated based on the data.
* Training classification models, using *k-NN* and *Random Forest* algorithms.

## Considerations for Classification

* Evaluation Metric: *Accuracy*
* Repeated cross validation - *10 fold 10 repeats each*
* Train/test split - *75%/25%*
* Hyperparameter tuning for the classification
* Imbalance resulation techniques, e.g., *up* and *down* sampling


## Model Performance

Best fitted trained model performed well for classification, with over *95%* accuracy. 

## Project Takeaways:

* mRNA based classification works fairly well for spearting prostate cancer patients from negative biopsies that can improve early detection of prostate cancer, which is crucial for successful treatment. 

* Accuracy of detecting negative biopsy patients from healthy individuals was also found very good, which may enable early detection and chance for close observation for further development of prostate cancer. 
