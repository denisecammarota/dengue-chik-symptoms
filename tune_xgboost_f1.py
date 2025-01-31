# Required Libraries
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import joblib
import optuna
from sklearn.metrics import precision_recall_curve, auc, f1_score, confusion_matrix, classification_report
from sklearn.model_selection import KFold, train_test_split
from xgboost import XGBClassifier
np.random.seed(123)

# Function to Save Classifier Results
def save_classifier(study, y_test, y_test_proba, y_test_final, thr_opt, precision, recall, file_path):
    with open(file_path, 'w') as file:
        file.write("=" * 40 + "\n")
        file.write("Study Summary\n")
        file.write("-" * 40 + "\n")
        file.write(f"Best Parameters: {study.best_params}\n")
        file.write(f"Best AUC-PR Value: {study.best_value:.4f}\n\n")
        file.write("Best Model Results\n")
        file.write("-" * 40 + "\n")
        file.write("Test Metrics:\n")
        file.write(f"Optimal Threshold: {thr_opt:.4f}\n")
        file.write(f"AUC-PR Score: {auc(recall, precision):.4f}\n")
        file.write("Confusion Matrix:\n")
        file.write(f"{confusion_matrix(y_test, y_test_final)}\n\n")
        file.write("Classification Report:\n")
        file.write(classification_report(y_test, y_test_final))
        file.write("\n" + "=" * 40 + "\n")

# Objective Function for XGBoost Hyperparameter Optimization
def objective_xg(trial):
    xg_params = {
        "n_estimators": trial.suggest_int("n_estimators", low=100, high=2000),
        "max_depth": trial.suggest_int("max_depth", 3, 10),
        "learning_rate": trial.suggest_loguniform('learning_rate', 1e-3, 1e-1),
        "min_child_weight": trial.suggest_int("min_child_weight", low=1, high=10),
    }
    kf = KFold(n_splits=5, shuffle=True, random_state=123)
    values_test = []
    xg = XGBClassifier(**xg_params)
    
    # Use AUC-PR instead of AUC-ROC
    for trf_index, tef_index in kf.split(X_train):
        X_trf, X_tef = X_train.iloc[trf_index], X_train.iloc[tef_index]
        y_trf, y_tef = y_train.iloc[trf_index], y_train.iloc[tef_index]
        xg.fit(X_trf, y_trf)
        y_proba = xg.predict_proba(X_tef)[:, 1]
        precision, recall, _ = precision_recall_curve(y_tef, y_proba)
        values_test.append(auc(recall, precision))
    return np.mean(values_test)

# Main Loop for State Analysis
states = np.array([31, 33, 35])  # Example for one state

for sg_num in states:
    # Load Data
    name_lab = 'data/lab_' + str(sg_num) + '.csv'
    name_epi = 'data/epi_' + str(sg_num) + '.csv'
    df_lab = pd.read_csv(name_lab).drop(columns=['ID_MN_RESI']).drop_duplicates()
    df_epi = pd.read_csv(name_epi).drop(columns=['ID_MN_RESI'])
    df_lab = pd.get_dummies(df_lab, columns=['fx_etaria'])
    df_epi = pd.get_dummies(df_epi, columns=['fx_etaria'])
    df_epi = df_epi.reindex(columns=df_lab.columns, fill_value=0)
    X_lab = df_lab.drop(columns=['CHIK'])
    y_lab = df_lab['CHIK']
    X_train, X_test, y_train, y_test = train_test_split(X_lab, y_lab, stratify=y_lab, test_size = 0.3, random_state=0)
    X_train, X_val, y_train, y_val = train_test_split(X_train, y_train, stratify=y_train, test_size = 0.5, random_state=0)
    
    # Optimize Model (using cross-validation to maximize AUC-PR)
    study = optuna.create_study(direction='maximize')
    study.optimize(objective_xg, n_trials=10)
    best_model = XGBClassifier(**study.best_params)
    best_model.fit(X_train, y_train)
    joblib.dump(best_model, str(sg_num) + '_xgboost.pkl')
    
    # Evaluate on Validation Data for Threshold Optimization
    y_val_proba = best_model.predict_proba(X_val)[:, 1]
    precision, recall, thresholds = precision_recall_curve(y_val, y_val_proba)
    
    # Optimal Threshold by Maximizing F1-Score on Validation Data
    f1_scores = [f1_score(y_val, y_val_proba >= t) for t in thresholds]
    thr_opt = thresholds[np.argmax(f1_scores)]
    
    # Plot Precision-Recall Curve
    plt.plot(recall, precision, marker='.', label=f'AUC-PR: {auc(recall, precision):.4f}')
    plt.scatter(recall[np.argmax(f1_scores)], precision[np.argmax(f1_scores)], color='red', label=f'Best F1-Score: {max(f1_scores):.4f}')
    plt.xlabel('Recall')
    plt.ylabel('Precision')
    plt.title('Precision-Recall Curve')
    plt.legend()
    plt.grid()
    file_path = str(sg_num) + '_pr_curve.png'
    plt.savefig(file_path, dpi=300, bbox_inches='tight')
    plt.show()
    
    # Evaluate Final Model on Test Data
    y_test_proba = best_model.predict_proba(X_test)[:, 1]
    y_test_final = (y_test_proba >= thr_opt).astype(int)
    
    # Save Results
    file_path = str(sg_num) + '_xgboost_results.txt'
    save_classifier(study, y_test, y_test_proba, y_test_final, thr_opt, precision, recall, file_path)
