# Discrimination of PDPs in Water Samples: A Machine Learning Approach Using TIC Data (HS-GC-IMS)

## 📌 Overview 

This repository contains the code and workflows for the analysis of hydrocarbon contamination in water samples using Headspace Gas Chromatography-Ion Mobility Spectrometry (HS-GC-IMS) in combination with machine learning techniques. The project aims to develop classification models for detecting and differentiating hydrocarbons in complex aqueous matrices.

---

## 📂 Project Structure

The repository is structured as follows:

```
├── figures/                             # Generated figures from data analysis
├── scripts/                             # Contains scripts for data analysis
│   ├── eda                              # Exploratory Data Analysis (EDA)
│       ├── missing_values_hsgcims_waters.R
│       ├── outliers_hsgcims_waters.R
│   ├── tic                              # Total Ion Chromatogram (TIC) ploting 
│       ├── tic_hsgcims_waters.R
│   ├── unsupervised_algorithms          # Unsupervised Machine Learning (HCA, PCA)
│       ├── hca_hsgcims_waters.R
│       ├── pca_hsgcims_waters.R
│   ├── supervised_algorithms            # Supervised Machine Learning (SVM, RF)
│       ├── svm_hsgcims_waters.R
│       ├── rf_hsgcims_waters.R
├── requirements.txt                     # Required R packages
├── README.md                            # Project documentation
├── .gitignore                           # Ignore file
├── LICENSE                              # License file
```

---

## 🔄 Workflow

The data analysis workflow follows these main steps:

1. Exploratory Data Analysis (EDA)

- Detection of missing values and outliers

- Visualization of Total Ion Chromatogram (TIC) profiles based on contamination classes

- Exploratory assessment of the dataset using Hierarchical Clustering Analysis (HCA) and Principal Component Analysis (PCA)

2. Supervised Machine Learning

Random Forest (RF) and Support Vector Machine (SVM) models are developed for classification. Three feature selection strategies are applied:

- Using all predictors
- Selection via the Boruta algorithm
- Selection via Genetic Algorithm (GA)

Standard machine learning workflow applied:

- Splitting the dataset into training (70%) and test (30%) sets.
- Feature extraction is performed exclusively on the training set.
- Data is scaled and centered after splitting and feature extraction.
- Class balancing strategies such as undersampling and SMOTE are applied to the training set.
- Model training includes hyperparameter tuning and cross-validation.
- Evaluation is conducted on the test set to assess final performance.

---

## 🖥️ Software and Dependencies

The analysis is conducted in R (v4.4.0) within Visual Studio Code. The required R packages are specified in *requirements.txt*, and include:

- **Data handling**: readxl (v1.4.3), writexl (v1.5.1), dplyr (v1.1.4), tidyr (v1.3.1), data.table (v1.16.2)
- **Visualization**: ggplot2 (v3.5.1), ggcorrplot (v0.1.4.1), gridExtra (v2.3), viridis (v0.6.5), ggrepel (v0.9.6), reshape2 (v1.4.4)
- **Clustering & Dimensionality Reduction**: stats, factoextra (v1.0.7), cluster (v2.1.6)
- **Feature Selection**: Boruta (v8.0.0), GA (v3.2.4), recipes (v1.1.0), themis (v1.0.2)
- **Machine Learning**: caret (v6.0.94), doParallel (v1.0.17)

---

## 🚀 How to Use the Repository

1. Clone the repository
   
```bash
git clone https://github.com/Marta-Barea/tic-hs-gc-ims-waters-ml.git
cd tic-hs-gc-ims-waters-ml
```

2. Set up the R environment

Ensure you have R (v4.4.0 or later) installed. Install the required packages using the requirements.txt file.

___

## 🤝 Collaboration 

This project is conducted in collaboration with the Department of Analytical Chemistry, University of Cádiz (UCA).

---

## 📜 License

This project is licensed under the GNU GENERAL PUBLIC License. See `LICENSE` for details.

