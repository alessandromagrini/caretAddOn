# caretAddOn
__Additional functionalities for the R package 'caret'__

This package includes additional functionalities for the `caret` package, including new models, new summaries, new graphics, and new general purpose functions.
Reference for the `caret` package:

M. Kuhn (2022). caret: Classification and Regression Training. R package version 6.0-92. CRAN page: <a href="https://CRAN.R-project.org/package=caret">https://CRAN.R-project.org/package=caret</a>.
Github page: <a href="https://github.com/topepo/caret">https://github.com/topepo/caret</a>

New models:
- `loglm`: linear regression on log transformed response variable, with prediction made through either the conditional expected value (`bias.adj=TRUE') or the conditional median (`bias.adj=FALSE')
- `svm_linear`: support vector machine/regression with linear kernel
- `svm_radial`: support vector machine/regression with radial kernel

New summaries:
- `customSummaryClass`: summary for classification tasks, including AUC and optimized specificity/sensitivity in case of class unbalance
- `customSummaryReg`: summary for regression taks, including RMSE, MAE, MAPE, and R-squared
- `customSummaryReg_median`: summary for regression taks, including the median version of RMSE, MAE, and MAPE (robust to outliers)

New general-purpose functions:
- `bestTune`: best tuning of hyperparameters
- `fitted` method for class `train`: cross-validation prediction for each unit
- `addTerms`: addition of polynomial and/or logarithmic terms to a formula
- `stepCV`: stepwise pruning of explanatory variables through cross-validation
- `enetCoef`: coefficients at best tuning of hyperparameters for an object of class `glmnet`
- `vifCalc`: computation of variance inflation factors, conceived to check collinearity among the explanatory variables

New graphics:
- `multiBoxPlot`: boxplots for multiple variables
- `multiScatPlot`: scatterplots or boxplots displaying the relationship between the response variable and each explanatory variable
- `corPlot`: correlogram conceived to check collinearity among the explanatory variables
- `trainPlot`: graphic displaying any one metric as a function of any one hyperparameter
- `importancePlot`: graphic of variable importance for objects of class `rpart` and `randomForest`
- `rocPlot`: roc curve (only for binary classification)
- `predPlot`: scatterplot of observed values versus predicted values (only for regression tasks)
