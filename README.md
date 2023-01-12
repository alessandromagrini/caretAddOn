# caretAddOn
__Additional functionalities for the R package 'caret'__

This package includes additional functionalities for the `caret` package, including new models, new summaries, and new general-purpose functions.
Reference for the `caret` package:

Max Kuhn (2022). caret: Classification and Regression Training. R package version 6.0-92. CRAN page: <a href="https://CRAN.R-project.org/package=caret">https://CRAN.R-project.org/package=caret</a>.
Github page: <a href="https://github.com/topepo/caret">https://github.com/topepo/caret</a>

New models:
- `loglm_mean`: linear regression on log transformed response variable, with prediction made through the conditional expected value
- `loglm_median`: linear regression on log transformed response variable, with prediction made through the conditional median
- `svm_linear`: support vector machine/regression with linear kernel
- `svm_radial`: support vector machine/regression with radial kernel

New custom summaries:
- `customSummaryClass`: summary for classification tasks, including AUC and optimized specificity/sensitivity in case of class unbalance
- `customSummaryReg`: summary for regression taks, including RMSE, MAE, MAPE, and R-squared
- `customSummaryReg_median`: summary for regression taks, including the median version of RMSE, MAE, and MAPE (robust to outliers)

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
- `iErr`: individual prediction errors (only for regression tasks)
