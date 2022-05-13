# caretAddOn
__Additional functionalities for the R package 'caret'__

This package includes additional functionalities for the caret package, including new models, new summaries, and new general-purpose functions.
It is conceived to be used by students of my academic teachings on supervised learning.

New models:
- `loglm_mean`: linear regression on log transformed response variable, prediction through expected value
- `loglm_median`: linear regression on log transformed response variable, prediction through median
- `svm_radial`: support vector machines/regression with radial kernel

New custom summaries:
- `customSummaryClass`: summary for classification tasks, including AUC and optimized specificity/sensitivity in case of class unbalance
- `customSummaryReg`: summary for regression taks, including RMSE, MAE, MAPE and R-squared

New general-purpose functions:
- `addTerms`: addition of polynomial and/or logarithmic terms to a formula
- `stepCV`: stepwise pruning of explanatory variables through cross-validation
- `bestTune`: extraction of the best tuning of hyperparameters
- `enetCoef`: extraction of coefficients at best tuning of hyperparameters for an object of class `glmnet`
- `trainPlot`: graphic displaying any one metric as a function of any one hyperparameter
- `importancePlot`: graphic of variable importance for objects of class `rpart` and `randomForest`
- `rocPlot`: graphic displaying the roc curve (only for classification tasks)
- `predPlot`: graphic displaying the scatterplot of observed versus predicted values (only for regression tasks)
