import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import random
import shap
from sklearn import metrics
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.model_selection import GridSearchCV
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from catboost import CatBoostClassifier
from sklearn.ensemble import AdaBoostClassifier
from sklearn.model_selection import KFold
import optuna
import optuna.visualization as vis
import joblib
from xgboost import XGBClassifier
from sklearn import tree
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, confusion_matrix, roc_curve, roc_auc_score, auc, precision_score, recall_score, precision_recall_curve, classification_report
np.random.seed(123)

states = np.array([35,33,31])    
thr_opts = np.array([0.1405, 0.0590,  0.0137])
i = 0

for sg_num in states:
    thr_opt = thr_opts[i]
    # data processing
    name_epi = 'data/epi_'+str(sg_num)+'.csv'
    df_epi = pd.read_csv(name_epi)
    df_epi = df_epi.drop(columns = ['ID_MN_RESI'])
    name_lab = 'data/lab_'+str(sg_num)+'.csv'
    name_epi = 'data/epi_'+str(sg_num)+'.csv'
    df_lab = pd.read_csv(name_lab)
    df_epi = pd.read_csv(name_epi)
    epi_resi = df_epi['ID_MN_RESI']
    df_lab = df_lab.drop(columns = ['ID_MN_RESI'])
    df_epi = df_epi.drop(columns = ['ID_MN_RESI'])
    denv_lab = sum(df_lab['CHIK'] == 0) 
    chik_lab = sum(df_lab['CHIK'] == 1) 
    denv_epi = sum(df_epi['CHIK'] == 0) 
    chik_epi = sum(df_epi['CHIK'] == 1) 
    df_lab = pd.get_dummies(df_lab, columns = ['fx_etaria'])
    df_epi = pd.get_dummies(df_epi, columns = ['fx_etaria'])
    df_epi = df_epi.reindex(columns = df_lab.columns, fill_value=0)
    df_lab = df_lab.drop_duplicates()
    X_lab = df_lab.drop(columns = ['CHIK'])
    y_lab = df_lab['CHIK']
    X_epi = df_epi.drop(columns = ['CHIK'])
    y_epi = df_epi['CHIK']
    # loading the correct model
    name_model = 'results/'+str(sg_num)+'_xgboost.pkl'
    grid_model = joblib.load(name_model)
    # doing correction to the epidemiological data
    y_epi_proba = grid_model.predict_proba(X_epi)[:,1]
    y_epi_final = y_epi_proba.copy()
    y_epi_final[y_epi_proba >= thr_opt] = 1
    y_epi_final[y_epi_proba < thr_opt] = 0
    y_epi_final = pd.Series(y_epi_final, name = 'CHIK_CORR')
    muns_epi_final = pd.Series(epi_resi, name = 'ID_MN_RESI')
    df_epi_corr = pd.concat([df_epi, y_epi_final], axis = 1)
    df_epi_corr = pd.concat([df_epi_corr, muns_epi_final], axis = 1)
    name_corr = 'results/epi_corr_'+str(sg_num)+'.csv'
    df_epi_corr.to_csv(name_corr, index=False)
    i = i + 1




