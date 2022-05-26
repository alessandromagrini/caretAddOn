# caretAddOn
__Additional functionalities for the R package 'caret'__

The `caretAddOn` package includes additional functionalities for the `caret` package, including new models, new summaries, and new general-purpose functions.
It is conceived to be used by students of my academic teachings on supervised learning.

New models:
- `loglm_mean`: linear regression on log transformed response variable, with prediction made through the conditional expected value
- `loglm_median`: linear regression on log transformed response variable, with prediction made through the conditional median
- `svm_radial`: support vector machine/regression with radial kernel

New custom summaries:
- `customSummaryClass`: summary for classification tasks, including AUC and optimized specificity/sensitivity in case of class unbalance
- `customSummaryReg`: summary for regression taks, including RMSE, MAE, MAPE and R-squared

New general-purpose functions:
- `corPlot`: correlogram
- `scatPlot`: scatterplot between two quantitative variables with superimposed regression curve (linear, polynomial, log-linear, log-log)
- `addTerms`: addition of polynomial and/or logarithmic terms to a formula
- `stepCV`: stepwise pruning of explanatory variables through cross-validation
- `bestTune`: best tuning of hyperparameters
- `enetCoef`: coefficients at best tuning of hyperparameters for an object of class `glmnet`
- `trainPlot`: graphic displaying any one metric as a function of any one hyperparameter
- `importancePlot`: graphic of variable importance for objects of class `rpart` and `randomForest`
- `rocPlot`: roc curve (only for binary classification)
- `predPlot`: scatterplot of observed versus predicted values (only for regression tasks)
- `cookDist`: Cook's distance between observed and predicted values (not implemented for multiple classification)
