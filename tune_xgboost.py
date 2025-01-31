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


def save_classifier(study, y_test, y_test_proba, y_test_final, thr_opt, fpr, tpr, file_path):
    with open(file_path, 'w') as file:
        file.write("=" * 40 + "\n")
        file.write("Study Summary\n")
        file.write("-" * 40 + "\n")
        file.write(f"Best Parameters: {study.best_params}\n")
        file.write(f"Best AUC-ROC Value: {study.best_value:.4f}\n\n")
        file.write("Best Model Results\n")
        file.write("-" * 40 + "\n")
        file.write("Test Metrics:\n")
        file.write(f"Optimal Threshold: {thr_opt:.4f}\n")
        file.write(f"ROC-AUC Score: {roc_auc_score(y_test, y_test_proba):.4f}\n")
        file.write(f"FPR: {fpr:.4f}\n")
        file.write(f"TPR: {tpr:.4f}\n")
        file.write("Confusion Matrix:\n")
        file.write(f"{confusion_matrix(y_test, y_test_final)}\n\n")
        file.write("Classification Report:\n")
        file.write(classification_report(y_test, y_test_final))
        file.write("\n" + "=" * 40 + "\n")
        
        
def objective_xg(trial):
    xg_params = {
        "n_estimators": trial.suggest_int("n_estimators", low = 100, high = 2000),
        "max_depth": trial.suggest_int("max_depth", 3, 10),
        "learning_rate": trial.suggest_loguniform('learning_rate', 1e-3, 1e-1),
        "min_child_weight": trial.suggest_int("min_child_weight", low = 1, high = 10),
    }
    kf = KFold(n_splits = 5, shuffle = True, random_state = 123)
    values_test = []
    xg = XGBClassifier(**xg_params)
    for trf_index, tef_index in kf.split(X_train): # train and test folds of train data (only data we use now)
        X_trf, X_tef = X_train.iloc[trf_index], X_train.iloc[tef_index]
        y_trf, y_tef = y_train.iloc[trf_index], y_train.iloc[tef_index]
        xg.fit(X_trf, y_trf)
        values_test.append(roc_auc_score(y_tef, xg.predict_proba(X_tef)[:,1]))
    return np.mean(np.array(values_test))

        
#states = np.array([35,33,31])    
states = np.array([31])

for sg_num in states:
    name_lab = 'data/lab_'+str(sg_num)+'.csv'
    name_epi = 'data/epi_'+str(sg_num)+'.csv'
    df_lab = pd.read_csv(name_lab)
    df_epi = pd.read_csv(name_epi)
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
    X_train, X_test, y_train, y_test = train_test_split(X_lab, y_lab, stratify=y_lab, test_size = 0.3, random_state = 0)
    X_test, X_val, y_test, y_val = train_test_split(X_lab, y_lab, stratify=y_lab, test_size = 0.5, random_state = 0)
    study = optuna.create_study(direction = 'maximize')
    study.optimize(objective_xg, n_trials = 50)
    grid = XGBClassifier(**study.best_params)
    grid.fit(X_train, y_train)
    joblib.dump(grid, str(sg_num)+'_xgboost.pkl')
    y_val_pred = grid.predict(X_val)
    y_val_proba = grid.predict_proba(X_val)[:,1]
    fpr, tpr, thresholds = roc_curve(y_val, y_val_proba, pos_label = 1)
    n_arg = np.argmin(abs(tpr-(1-fpr)))
    thr_opt = thresholds[n_arg]
    plt.plot(fpr, tpr, '-o', linewidth=2, label = 'XGBoost')
    plt.plot([0,1], [0,1], 'k--' )
    plt.scatter(fpr[n_arg],tpr[n_arg], color = 'red', s = 100)
    plt.ylabel('TPR')
    plt.xlabel('FPR')
    plt.legend()
    file_path = str(sg_num)+'_xgboost.png'
    plt.savefig(file_path, dpi=300, bbox_inches='tight') 
    plt.show()
    y_test_proba = grid.predict_proba(X_test)[:,1]
    y_test_final = y_test_proba.copy()
    y_test_final[y_test_proba >= thr_opt] = 1
    y_test_final[y_test_proba < thr_opt] = 0
    file_path = str(sg_num)+'_xgboost.txt'
    save_classifier(study, y_test, y_test_proba, y_test_final, thr_opt, fpr[n_arg], tpr[n_arg], file_path)
        
        
    

