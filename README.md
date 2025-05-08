# caretAddOn
__Additional functionalities for the R package 'caret'__

This package contains additional functionalities for the `caret` package, including new models, new summaries, graphical tools, general-purpose functions, and datasets.
Reference for the `caret` package:

M. Kuhn (2022). caret: Classification and Regression Training. R package version 6.0-92. CRAN page: <a href="https://CRAN.R-project.org/package=caret">https://CRAN.R-project.org/package=caret</a>.
Github page: <a href="https://github.com/topepo/caret">https://github.com/topepo/caret</a>

New models:
- `loglm`: linear regression on log transformed response variable, with prediction made through either the conditional expected value (`bias.adj=TRUE`) or the conditional median (`bias.adj=FALSE`)
- `svm_linear`: support vector machine/regression with linear kernel
- `svm_radial`: support vector machine/regression with radial kernel

New summaries:
- `customSummaryClass`: summary for classification tasks, including AUC and optimized specificity/sensitivity in case of class imbalance
- `customSummaryReg`: summary for regression tasks, including RMSE, MAE, MAPE, and R-squared

New graphics:
- `multiPairPlot`: bivariate graphics displaying the relationship between one response variable and several explanatory variables
- `corPlot`: correlogram, conceived to check collinearity among the explanatory variables
- `trainPlot`: graphic displaying any one metric as a function of any one hyperparameter
- `importancePlot`: graphic displaying variable importance metrics computed through the function `importanceCalc` (see below)
- `rocPlot`: ROC curve (only for binary classification)
- `densPlot`: graphic displaying kernel density estimations for class probabilities (only for binary classification), conceived to assess the discriminative power
- `predPlot`: scatterplot of observed values versus predicted values (only for regression tasks)

New general-purpose functions:
- `bestTune`: best tuning of hyperparameters
- `fitted` (S3 method for class `train`): cross-validation prediction for each unit
- `importanceCalc`: computation of variable importance metrics. For models of class `lm` or `glm`, the proportion of explained deviance is used in place of the absolute t-statistic.
- `vifCalc`: computation of variance inflation factors, conceived to check collinearity among the explanatory variables
- `addTerms`: addition of polynomial and/or logarithmic terms to a formula
- `stepCV`: backward selection of explanatory variables through cross-validation
- `stepAIC_train`: first, stepwise selection of explanatory variables is performed through information criteria, then cross-validation is run to compute performance metrics (faster than `stepCV`)
- `enetCoef`: coefficients at best tuning of hyperparameters for an object of class `glmnet`
