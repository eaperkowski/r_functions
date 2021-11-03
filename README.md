# R helper functions

This is a repo to store helper functions that may or may not be useful. List of functions and their purpose:

  - backEmmeans: Back-transforms Emmeans when LMEMs are built on a transformed response variable. Function is from "RVAideMemoire" R package. Could not load package directly into R due to errors associated with "mixOmics" dependency

  - bootstrap_diff_means:

  - perc_change: calculates percent change between two values

  - selectClimate: Takes a dataset and subsets it based on a selected number of days before a given sampling date. This function was originally created to subset climate data based on different day intervals

  - stomatal_limitation: calculates Γ* and the Michaelis-Menton for CO2 in Rubisco per functions by SmithPlantEcophysLab/optimal_vcmax repo. Then, calculates stomatal limitation following equations described in Sharkey & Farquhar (1982)

  - summary_stat_calc: Provides mean, standard deviation, standard error, and confidence intervals using a column of data and the number of data observations

  - temp_standardize: standardizes Vcmax, Jmax, and Rd estimates to single temperature. Vcmax and Jmax are temperature standardized using a modified Arrhenius function as explained in Kattge & Knorr (2007), while Rd is temperature standardized using a log polynomial from O'Sullivan (2013) and parameters set using Heskel et al. (2016). Function is capable of standardizing to any temperature.
