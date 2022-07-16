#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 13 09:30:11 2022

@author: joern
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jan  1 16:14:53 2022

@author: joern
"""
# %% imports
import os
os.chdir("/home/joern/Aktuell/AI4pain/09Originale")
from compute_PCA import perform_pca
from sklearn.preprocessing import StandardScaler, MinMaxScaler
from sklearn.datasets import load_digits
import pandas as pd

# %% Get data and export them to csv

data = load_digits()
dfdata = pd.DataFrame(data.data)
dfdata.columns = data["feature_names"]
dfdata.columns

dfdata["digits"] = data.target
dfdata.to_csv("DigitsExample.csv", index=False)

# %% PCA
PCA_digits_data = dfdata.copy()
PCA_digits_data.drop(["digits"], axis=1, inplace=True)
PCA_digits_data_scaled = pd.DataFrame(StandardScaler().fit_transform(
    PCA_digits_data), columns=PCA_digits_data.columns)

PCA_digits, PCA_digits_feature_importance = perform_pca(
    PCA_digits_data, target=data.target, PC_criterion="ExplainedVar", minvar=0.95, plotReduced=1, biplot=False)

digits_PCA_projected = PCA_digits.fit_transform(PCA_digits_data)

PCA_digits_data_minmax = pd.DataFrame(MinMaxScaler().fit_transform(
    PCA_digits_data), columns=PCA_digits_data.columns)


pd.DataFrame(digits_PCA_projected).to_csv(
    "digits_PCA_projected.csv", index=False)
PCA_digits_data_minmax.to_csv("digits_PCA_minmax.csv", index=False)
