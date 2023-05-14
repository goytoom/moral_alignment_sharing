# Targeting Audiences Moral Values via Moral Framing Increases Spread of Misinformation

This is the repository for the paper Moral Alignment Shapes Responses To Shared Content. The repository contains all necessary codes, data, and result files, to replicate the project.

# Code Overview
## Study 1
- `check_classifiers.R`: robustness checks; creates spreadsheet to manually check stance detection based on users' tweets. Also calculate inter-correlations of moral language classifications.
- `example_messages.R`: produces example messages for each type of moral language, and stance (pro/antivax)
- `results_summary.R`: summarizes results; produces easy to read tables
- `run_models_rt.R`: fits models to predict tweets' retweet count
- `run_models_fav.R`: fits models to predict tweets' favourite count
- `dataset_moral.py`: creates train/test data (preprocessing+embeddings) from annotated training data set (MFTC)
- `moral_classifier.py`: trains and evaluates moral classifier
- `predict_foundations.py`: classifies moral language for all tweet files
- `merge_results.py`: merges annotated tweet files into a single file
- `tokenization.py`: auxiliary file for tokenizing inputs (used in training file creation)

## Study 2a
- `prepare_data.R`: Formats and cleans data. Afterwards, manual selection of best stimuli based on response pattern.

## Study 2b
- `prepare_data.R`: Formats and cleans data. 
- `run_models.R`: fits statistical models with cleaned data
- `run_model_mediation.R`: fits mediation models
- `results_summary.R`: summarizes model outputs. Creates easy to read tables.

## Study 3
- `prepare_data.R`: Formats and cleans data. 
- `run_models.R`: fits statistical models with cleaned data
- `run_mediation.R`: fits mediation models
- `results_summary.R`: summarizes model outputs. Creates easy to read tables.
- `prepare_data_order.R` and `run_models_order.R` prepare and run the additional analyses of stimuli presentation order effects respectively.

# Data Overview
## Study 1
- `data/tweets/`: hydrated tweets (one file per month)
- `data/stance/`: output of stance detection. hydrated tweets with stance (pro/antivax) annotations.
- `mftc_cleaned.csv`: Annotated training data (MFTC).

## Study 2a
- `qualtrics_export.csv`: raw survey data as exported from qualtrics

## Study 2b
- `qualtrics_export.csv`: raw survey data as exported from qualtrics

## Study 3
- `qualtrics_export_R.csv`: raw survey data as exported from qualtrics