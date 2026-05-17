###  NEW MODELS  ###

# extended generalized linear model
glm_extended <- list(
  label = "Extended generalized linear model",
  library = "MASS",
  type = c("Regression", "Classification"),
  parameters = data.frame(
    parameter = "dummy",
    class = "numeric",
    label = "dummy"
    ),
  grid = function(x, y, len = NULL, search = "grid") {
    data.frame(dummy = 1)
    },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    dots <- list(...)
    max.deg <- if (!is.null(dots$max.deg)) as.integer(dots$max.deg) else 1
    log.terms <- if (!is.null(dots$log.terms)) isTRUE(dots$log.terms) else FALSE
    log.response <- if (!is.null(dots$log.response)) isTRUE(dots$log.response) else FALSE
    bias.adj <- if (!is.null(dots$bias.adj)) isTRUE(dots$bias.adj) else TRUE
    use_stepAIC <- if (!is.null(dots$stepAIC)) isTRUE(dots$stepAIC) else FALSE
    use_glm <- if (!is.null(dots$use.glm)) isTRUE(dots$use.glm) else FALSE
    fixed_names <- c(
      "max.deg", "log.terms", "log.response", "bias.adj",
      "stepAIC", "use.glm",
      "step.direction", "step.k", "step.trace",
      "step.scope", "step.steps", "step.keep"
      )
    model_args <- dots
    model_args[fixed_names] <- NULL
    model_args$family <- NULL
    data <- as.data.frame(x)
    bt <- function(v) paste0("`", v, "`")
    make_formula <- function(data, max.deg, log.terms) {
      rhs <- vapply(names(data), bt, character(1))
      for (v in names(data)) {
        z <- data[[v]]
        if (is.numeric(z)) {
          n_unique <- length(unique(z[!is.na(z)]))
          if (n_unique >= 3) {
            if (max.deg >= 2) {
              for (d in 2:max.deg) {
                rhs <- c(rhs, paste0("I(", bt(v), "^", d, ")"))
                }
              }
            if (log.terms && all(z > 0, na.rm = TRUE)) {
              rhs <- c(rhs, paste0("log(", bt(v), ")"))
              }
            }
          }
        }
      as.formula(paste(".outcome ~", paste(rhs, collapse = " + ")))
      }
    check_family_domain <- function(y, fam, is_class) {
      fam_name <- fam$family
      if (is_class) {
        if (!(fam_name %in% c("binomial", "quasibinomial"))) {
          stop("Family ",fam_name," is invalid for classification tasks")
          }
        } else {
        if (fam_name %in% c("binomial", "quasibinomial")) {
          stop("Family ",fam_name," is invalid for regression tasks")
          }
        if (fam_name %in% c("Gamma", "inverse.gaussian")) {
          if (any(y <= 0, na.rm = TRUE)) {
            stop("Family ", fam_name, " requires y > 0.")
            }
          }
        if (fam_name %in% c("poisson", "quasipoisson")) {
          if (any(y < 0, na.rm = TRUE)) {
            stop("Family ", fam_name, " requires y >= 0.")
            }
          if (any(abs(y - round(y)) > .Machine$double.eps^0.5, na.rm = TRUE)) {
            warning("Family ", fam_name," is used but y has non-integer values")
            }
          }
        }
      invisible(TRUE)
      }
    form <- make_formula(
      data = data,
      max.deg = max.deg,
      log.terms = log.terms
      )
    is_class <- is.factor(y)
    if (is_class) {
      if (length(levels(y)) != 2) {
        stop("Implemented for binary classification tasks only")
        }
      if (log.response) log.response <- FALSE
      if (bias.adj) bias.adj <- FALSE
      fam <- if (!is.null(dots$family)) dots$family else stats::binomial()
      check_family_domain(
        y = y,
        fam = fam,
        is_class = TRUE
        )
      data$.outcome <- y
      fit <- do.call(
        stats::glm,
        c(
          list(
            formula = form,
            data = data,
            weights = wts,
            family = fam
            ),
          model_args
          )
        )
      model_engine <- "glm"
      } else {
      fam <- dots$family
      if (!is.null(fam)) use_glm <- TRUE
      if (isTRUE(use_glm)) {
        if (log.response) log.response <- FALSE
        if (bias.adj) bias.adj <- FALSE
        if (is.null(fam)) fam <- stats::gaussian()
        check_family_domain(
          y = y,
          fam = fam,
          is_class = FALSE
          )
        data$.outcome <- y
        fit <- do.call(
          stats::glm,
          c(
            list(
              formula = form,
              data = data,
              weights = wts,
              family = fam
              ),
            model_args
            )
          )
        model_engine <- "glm"
        } else {
        if (log.response) {
          if (any(y <= 0, na.rm = TRUE)) {
            stop("log.response = TRUE requires y > 0.")
            }
          data$.outcome <- log(y)
          } else {
          data$.outcome <- y
          bias.adj <- FALSE
          }
        fit <- do.call(
          stats::lm,
          c(
            list(
              formula = form,
              data = data,
              weights = wts
              ),
            model_args
            )
          )
        model_engine <- "lm"
        }
      }
    if (use_stepAIC) {
      step_args <- list(
        direction = if (!is.null(dots$step.direction)) dots$step.direction else "both",
        trace = if (!is.null(dots$step.trace)) dots$step.trace else FALSE
        )
      if (!is.null(dots$step.k)) step_args$k <- dots$step.k
      if (!is.null(dots$step.scope)) step_args$scope <- dots$step.scope
      if (!is.null(dots$step.steps)) step_args$steps <- dots$step.steps
      if (!is.null(dots$step.keep)) step_args$keep <- dots$step.keep
      fit <- do.call(
        MASS::stepAIC,
        c(list(object = fit), step_args)
        )
      }
    fit$call <- if (model_engine == "glm") {
      quote(glm(formula = extended_formula))
      } else {
      quote(lm(formula = extended_formula))
      }
    fit$max.deg <- max.deg
    fit$log.terms <- log.terms
    fit$log.response <- log.response
    fit$bias.adj <- bias.adj
    fit$stepAIC <- use_stepAIC
    fit$use.glm <- use_glm
    fit$expanded_formula <- form
    fit$obsLevels <- lev
    fit$is_class <- is_class
    fit$model_engine <- model_engine
    fit
    },
  predict = function(modelFit, newdata, submodels = NULL) {
    p <- predict(
      modelFit,
      newdata = as.data.frame(newdata),
      type = "response"
      )
    if (isTRUE(modelFit$is_class)) {
      out <- ifelse(
        p >= 0.5,
        modelFit$obsLevels[2],
        modelFit$obsLevels[1]
        )
      factor(out, levels = modelFit$obsLevels)
      } else {
      if (isTRUE(modelFit$log.response)) {
        if (isTRUE(modelFit$bias.adj)) {
          sigma2 <- summary(modelFit)$sigma^2
          p <- exp(p + sigma2 / 2)
          } else {
          p <- exp(p)
          }
        }
      as.numeric(p)
      }
    },
  prob = function(modelFit, newdata, submodels = NULL) {
    p <- predict(
      modelFit,
      newdata = as.data.frame(newdata),
      type = "response"
      )
    out <- data.frame(1 - p, p)
    colnames(out) <- modelFit$obsLevels
    out
    },
  levels = function(x) {
    x$obsLevels
    },
  varImp = function (object, ...) {
    # proportional increase in deviance when each term is dropped
    imp <- suppressWarnings(drop1(object, test = "Chisq"))
    dev <- imp$`Sum of Sq`
    if (is.null(dev)) dev <- imp$Deviance
    names(dev) <- rownames(imp)
    dev <- dev[setdiff(names(dev), "<none>")]
    dev <- dev[!is.na(dev)]
    if (length(dev) == 0 || sum(dev) == 0) {
      out <- data.frame(Overall = numeric(0))
      } else {
      out <- data.frame(Overall = dev / sum(dev))
      }
    out
    },
  sort = function(x) x
  )

# svm linear model
svm_linear <- caret::getModelInfo("svmLinear2", regex = FALSE)[[1]]
svm_linear$label <- "Linear support vector machine"
svm_linear$varImp <- function(object, ...) {
  varImps <- abs(t(t(object$coefs)%*%object$SV))
  out <- data.frame(varImps)
  colnames(out) <- "Overall"
  if(!is.null(names(varImps))) rownames(out) <- names(varImps)
  out
  }

# svm radial model
svm_radial <- list(
  label = "Support vector machine with radial kernel",
  library = "e1071",
  type = c("Regression", "Classification"),
  parameters = data.frame(parameter = c("cost", "gamma"),
                          class = c("numeric", "numeric"),
                          label = c("cost", "gamma")),
  grid = function(x, y, len = NULL, search = NULL) {
    sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
    return( expand.grid(gamma = mean(as.vector(sigmas[-2])),
                        cost  = 2 ^((1:len) - 3)))
    },
  loop = NULL,
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    if(any(names(list(...)) == "probability") | is.numeric(y)) {
      out <- svm(x = as.matrix(x), y = y,
                 kernel = "radial",
                 cost  = param$cost,
                 gamma = param$gamma,
                 ...)
      } else {
      out <- svm(x = as.matrix(x), y = y,
                 kernel = "radial",
                 cost  = param$cost,
                 gamma = param$gamma,
                 probability = classProbs,
                 ...)
      }
    out
    },
  predict = function(modelFit, newdata, submodels = NULL) {
    predict(modelFit, newdata)
    },
  prob = function(modelFit, newdata, submodels = NULL) {
    out <- predict(modelFit, newdata, probability = TRUE)
    attr(out, "probabilities")
    },
  varImp = function(object, ...) {
    varImps <- abs(t(t(object$coefs)%*%object$SV))
    out <- data.frame(varImps)
    colnames(out) <- "Overall"
    if(!is.null(names(varImps))) rownames(out) <- names(varImps)
    out
    },
  predictors = function(x, ...){
    out <- if(!is.null(x$terms)) predictors.terms(x$terms) else x$xNames
    if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if(is.null(out)) out <-NA
    out
    },
  levels = function(x) x$levels,
  sort = function(x) x[order(x$cost, x$gamma),]
  )

# regression with penalized cubic splines
gam_mgcv <- list(
  label = "Regression with penalized cubic splines",
  library = "mgcv",
  type = c("Regression", "Classification"),
  parameters = data.frame(
    parameter = "max_k",
    class = "numeric",
    label = "Maximum basis dimension"
    ),
  grid = function(x, y, len = NULL, search = "grid") {
    data.frame(max_k = 10)
    },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    dots <- list(...)
    data <- as.data.frame(x)
    bs_type <- ifelse(is.null(dots$bs), "cs", dots$bs)
    if (!is.null(dots$bs)) {
      dots$bs <- NULL
      }
    rhs <- vapply(names(data), function(v) {
      z <- data[[v]]
      if (is.numeric(z)) {
        n_unique <- length(unique(z[!is.na(z)]))
        if (n_unique >= 5) {
          k_var <- min(param$max_k, n_unique - 1)
          paste0("s(", v,", bs = '", bs_type,"', k = ", k_var,")")
          } else {
          v
          }
        } else {
        v
        }
      }, character(1))
    form <- as.formula(
      paste(".outcome ~", paste(rhs, collapse = " + "))
      )
    if (is.factor(y)) {
      if (length(levels(y)) != 2) {
        stop("Implemented for binary classification tasks only")
        }
      data$.outcome <- as.numeric(y == lev[2])
      default_family <- binomial()
      } else {
      data$.outcome <- y
      default_family <- gaussian()
      }
    if (is.null(dots$family)) {
      fam <- default_family
      } else {
      fam <- dots$family
      dots$family <- NULL
      }
    if (is.null(dots$method)) {
      dots$method <- "REML"
      }
    fit <- do.call(
      mgcv::gam,
      c(list(
        formula = form, data = data, weights = wts, family = fam
        ), dots)
      )
    fit$obsLevels <- lev
    fit$max_k <- param$max_k
    fit$gam_formula <- form
    fit$bs_type <- bs_type
    fit
    },
  predict = function(modelFit, newdata, submodels = NULL) {
    p <- predict(
      modelFit,
      newdata = as.data.frame(newdata),
      type = "response"
      )
    if (modelFit$family$family == "binomial") {
      out <- ifelse(
        p >= 0.5,
        modelFit$obsLevels[2],
        modelFit$obsLevels[1]
        )
      factor(out, levels = modelFit$obsLevels)
      } else {
      as.numeric(p)
      }
    },
  prob = function(modelFit, newdata, submodels = NULL) {
    p <- predict(
      modelFit,
      newdata = as.data.frame(newdata),
      type = "response"
      )
    out <- data.frame(1-p, p)
    colnames(out) <- modelFit$obsLevels
    out
    },
  levels = function(x) {
    x$obsLevels
    },
  sort = function(x) {
    x[order(x$max_k), ]
    },
  varImp = function(object, ...) {
    s <- summary(object)
    out <- data.frame(
      Overall = numeric(0)
      )
    if (!is.null(s$s.table) && nrow(s$s.table) > 0) {
      st <- as.data.frame(s$s.table)
      smooth_names <- rownames(st)
      smooth_vars <- gsub("^s\\((.*)\\)$", "\\1", smooth_names)
      stat_col <- intersect(
        c("F", "Chi.sq", "Chi.sq."),
        colnames(st)
        )[1]
      if (!is.na(stat_col)) {
        vals <- st[["edf"]] * st[[stat_col]]
        vals[is.na(vals)] <- 0
        out <- rbind(
          out,
          data.frame(
            Overall = vals,
            row.names = smooth_vars
            )
          )
        }
      }
    if (!is.null(s$p.table) && nrow(s$p.table) > 0) {
      pt <- as.data.frame(s$p.table)
      param_names <- rownames(pt)
      param_names <- param_names[param_names != "(Intercept)"]
      stat_col <- intersect(
        c("t value", "z value"),
        colnames(pt)
        )[1]
      if (length(param_names) > 0 && !is.na(stat_col)) {
        vals <- abs(pt[param_names, stat_col])
        vals[is.na(vals)] <- 0
        out <- rbind(
          out,
          data.frame(
            Overall = vals,
            row.names = param_names
            )
          )
        }
      }
    out <- out[order(out$Overall, decreasing = TRUE), , drop = FALSE]
    out
    }
  )


###  NEW SUMMARIES  ###

# summary for classification tasks
customSummaryClass <- function(data, lev = NULL, model = NULL) {
  if (length(lev) == 2) {
    rocObject <- try(
      pROC::roc(data$obs, data[, lev[2]], direction = "<", quiet = TRUE),
      silent = TRUE
      )
    if (inherits(rocObject, "try-error")) {
      out <- c(
        Accuracy = NA, Spec = NA, Sens = NA,
        cut_youden = NA, Spec_youden = NA, Sens_youden = NA
        )
      return(out)
      }
    bayes <- pROC::coords(
      rocObject,
      x = 0.5,
      ret = c("specificity", "sensitivity")
      )
    youd <- pROC::coords(
      rocObject,
      x = "best",
      best.method = "youden",
      ret = c("threshold", "specificity", "sensitivity")
      )
    if (nrow(youd) > 1) {
      d <- abs(youd$specificity - youd$sensitivity)
      youd <- youd[which.min(d), , drop = FALSE]
      }
    if (is.infinite(youd$threshold) && youd$threshold > 0)
      youd$threshold <- 1
    if (is.infinite(youd$threshold) && youd$threshold < 0)
      youd$threshold <- 0
    out <- c(
      Accuracy = as.numeric(rocObject$auc),
      Spec = bayes$specificity,
      Sens = bayes$sensitivity,
      cut_youden = youd$threshold,
      Spec_youden = youd$specificity,
      Sens_youden = youd$sensitivity
      )
    out
    } else {
    cmat <- table(data$obs, data$pred)
    acc0 <- sum(diag(cmat)) / sum(cmat)
    acc <- c()
    for (i in 1:nrow(cmat)) {
      acc[i] <- cmat[i, i] / sum(cmat[i, ])
      }
    names(acc) <- rownames(cmat)
    c(Accuracy = acc0, acc)
    }
  }

# summary for regression tasks
customSummaryReg <- function(data, lev = NULL, model = NULL) {
  yobs <- data$obs
  ypred <- data$pred
  mse <- mean((yobs-ypred)^2)
  mae <- mean(abs(yobs-ypred))
  rmse <- sqrt(mse)
  rel_rmse <- rmse/sd(yobs,na.rm=T)
  rel_mae <- mae/mean(abs(yobs-mean(yobs,na.rm=T)),na.rm=T)
  if(var(ypred)>0) rsq <- (cor(yobs,ypred))^2 else rsq <- NA
  if(sum(yobs<=0)==0) {
    mape <- 100*mean(abs((yobs-ypred)/yobs))
    out <- c(mse,rmse,mae,rel_rmse,rel_mae,mape,rsq)
    names(out) <- c("MSE","RMSE","MAE","Relative_RMSE","Relative_MAE","MAPE","R-squared")
    } else {
    out <- c(mse,rmse,mae,rel_rmse,rel_mae,rsq)
    names(out) <- c("MSE","RMSE","MAE","Relative_RMSE","Relative_MAE","R-squared")
    }
  out
  }


###  NEW FUNCTIONS  ###

# get metrics at best tuning
bestTune <- function(caret_fit) {
  best <- which(rownames(caret_fit$results)==rownames(caret_fit$bestTune))
  caret_fit$results[best,]
  }

# get CV predictions - fitted method for class 'train'
fitted.train <- function(object, ...) {
  tab <- object$pred
  if(is.null(tab)) stop("Argument 'savePredictions' is not set to 'final' in trainControl()")
  if(identical(object$modelType,"Regression")) {
    pred <- do.call(c,lapply(split(tab[,"pred"],tab[,"rowIndex"]),mean))
    obs <- do.call(c,lapply(split(tab[,"obs"],tab[,"rowIndex"]),function(z){z[1]}))
    } else {
    lev <- object$levels
    predList <- list()
    for(i in 1:length(lev)) {
      if((lev[i]%in%colnames(tab))==F) stop("Argument 'classProbs' is not set to TRUE in trainControl()")
      predList[[i]] <- do.call(c,lapply(split(tab[,lev[i]],tab[,"rowIndex"]),mean))
      }
    pred <- do.call(cbind,predList)
    colnames(pred) <- lev
    obs <- factor(do.call(c,lapply(split(tab[,"obs"],tab[,"rowIndex"]),function(z){z[1]})), levels=lev)
    }
  ind <- as.numeric(names(obs))
  tab <- data.frame(id=rownames(object$trainingData)[ind], observed=obs, predicted=pred)
  tab
  }

# variance inflation factors
vifCalc <- function(data) {
  dataOK <- data.frame(model.matrix(~., data=data)[,-1,drop=F])
  nomi <- colnames(dataOK)
  vif <- c()
  for(i in 1:length(nomi)) {
    form <- paste0(nomi[i],"~.")
    mod <- lm(formula(form), data=dataOK)
    vif[i] <- 1/(1-summary(mod)$r.squared)
    }
  names(vif) <- nomi
  vif
  }

# get coefficients at best tuning for an elastic net
enetCoef <- function(caret_fit) {
  if(identical(caret_fit$method,"glmnet")==F) stop("Implemented for method 'glmnet' only",call.=F)
  as.matrix(predict(caret_fit$finalModel, type="coefficients",
    s=caret_fit$bestTune$lambda))[,1]
  }


###  NEW GRAPHICS  ###

# plot of grid search
trainPlot <- function(caret_fit, par=NULL, metric=NULL, ylab=NULL, xlab=NULL, vcol="red", ...) {
  if(is.null(metric)) metric <- caret_fit$metric
  if(is.null(par)) par <- colnames(caret_fit$bestTune)[1]
  if(is.null(ylab)) ylab <- metric
  if(is.null(xlab)) xlab <- par
  plot(caret_fit$results[,par], caret_fit$results[,metric], ylab=ylab, xlab=xlab, type="l", ...)
  abline(v=caret_fit$bestTune[1,par], col=vcol)
  }

# plot of variable importance
importancePlot <- function(caret_fit, scale=TRUE, ordered=TRUE, ylab="", add.grid=TRUE, line.lty=1, cex.points=0.8, cex.names=0.8, dist.names=0.5, ...) {
  auximp <- varImp(caret_fit, scale=scale)$importance
  imp <- auximp[,1]
  names(imp) <- rownames(auximp)
  if(ordered) imp <- sort(imp, decreasing=T)
  if(!is.null(imp)) {
    plot(imp, type="n", xaxt="n", xlab="", ylab=ylab, ...)
    if(add.grid) grid()
    points(imp, cex=cex.points, ...)
    segments(1:length(imp), 0, 1:length(imp), imp, lty=line.lty)
    axis(1, at=1:length(imp), labels=names(imp), las=2, cex.axis=cex.names, tick=F, mgp=c(3,dist.names,0))
    box()
    }
  }

# roc curve (binary classification only)
rocPlot <- function(caret_fit, lwd=2, quiet=TRUE, ...) {
  if(identical(caret_fit$modelType,"Classification")==F & length(caret_fit$levels)==2) stop("Implemented for binary classification tasks only",call.=F)
  tab <- caret_fit$pred
  pred <- do.call(c,lapply(split(tab[,caret_fit$levels[2]],tab[,"rowIndex"]),mean))
  obs <- do.call(c,lapply(split(tab[,"obs"],tab[,"rowIndex"]),function(z){z[1]}))
  suppressWarnings(
    rocObj <- pROC::roc(response=obs, predictor=pred, lwd=lwd, quiet=quiet, ...)
    )
  plot(rocObj, ...)
  }

# density plot (binary classification only)
densPlot <- function(caret_fit, main="", lty=c(1,1), lwd=c(1,1), col=c("blue","red"), cut.lty=2, cut.col=1, legend.pos="topright", legend.text=c("neg.","pos."), legend.cex=0.8, ...) {
  if(identical(caret_fit$modelType,"Classification")==F & length(caret_fit$levels)==2) stop("Implemented for binary classification tasks only",call.=F)
  tab <- caret_fit$pred
  pred <- do.call(c,lapply(split(tab[,caret_fit$levels[2]],tab[,"rowIndex"]),mean))
  obs <- do.call(c,lapply(split(tab[,"obs"],tab[,"rowIndex"]),function(z){z[1]}))
  suppressWarnings(
    rocObj <- pROC::roc(response=obs, predictor=pred, quiet=T)
    )
  cut <- pROC::coords(rocObj, x="best", ret="threshold")
  p1 <- pred[which(obs==caret_fit$levels[1])]
  p2 <- pred[which(obs==caret_fit$levels[2])]
  if(length(lty)<2) lty <- rep(lty,2)
  if(length(lwd)<2) lty <- rep(lwd,2)
  if(length(col)<2) lty <- rep(col,2)
  plot(density(p1), main=main, lwd=lwd[1], lty=lty[1], col=col[1], ...)
  lines(density(p2), lwd=lwd[2], lty=lty[2], col=col[2])
  abline(v=cut, lty=cut.lty, col=cut.col)
  legend(legend.pos, legend=legend.text, lty=1, col=col, cex=legend.cex, bty="n")
  }

# multiple bivariate plots
multiPairPlot <- function(y.name, x.names=NULL, data, coef=1.5, outliers=TRUE, axis.size=6, label.size=10, point.size=0.6, smooth.method="gam", smooth.size=0.6, smooth.color="blue", ...) {
  if(!is.logical(outliers)) outliers <- F else outliers <- outliers[1]
  if(is.null(x.names)) x.names <- colnames(data)
  yaux <- intersect(y.name,colnames(data))
  x.names <- setdiff(intersect(x.names,colnames(data)),yaux)
  if(length(yaux)==0) stop("No valid variable name in argument 'y.name'",call.=F) 
  if(length(x.names)==0) stop("No valid variable name in argument 'x.names'",call.=F) 
  y.name <- yaux[1]
  pp <- list()
  if(is.numeric(data[,y.name])) {
    for(i in 1:length(x.names)) {
      pp[[i]] <- eval(substitute(
        ggplot() +
          geom_point(aes(y=data[,y.name],x=data[,x.names[i]]), size=point.size, ...) +
          geom_smooth(aes(y=data[,y.name],x=data[,x.names[i]]), formula=y~x, size=smooth.size, color=smooth.color, se=T, na.rm=T, method=smooth.method) +
          labs(x=x.names[i], y=y.name) +
          theme(axis.text=element_text(size=axis.size),
                axis.title=element_text(size=label.size))
        ), list(i=i))
      }
    } else {
    for(i in 1:length(x.names)) {
      if(is.numeric(data[,x.names[i]])&&nlevels(factor(data[,x.names[i]]))>2) {
        if(outliers) {
          irng <- range(data[,x.names[i]],na.rm=T)
          icol <- "black"
          } else {
          irng <- range(boxplot(data[,x.names[i]]~data[,y.name], coef=coef, plot=F)$stats[c(1,5),])
          icol <- NA
          }
        pp[[i]] <- eval(substitute(
          ggplot() +
            geom_boxplot(aes(x=data[,y.name],y=data[,x.names[i]]), coef=coef, outlier.size=point.size, outlier.color=icol, ...) +
            coord_cartesian(ylim=irng) +
            labs(y=x.names[i], x=y.name) +
            theme(axis.text=element_text(size=axis.size),
                  axis.title=element_text(size=label.size))
          ), list(i=i))
        } else {
        idat <- as.data.frame(prop.table(table(data[,y.name],data[,x.names[i]]),1))
        pp[[i]] <- eval(substitute(
          ggplot(idat, aes(x=Var1,y=Freq,fill=Var2)) +
          geom_bar(position="stack", stat="identity", ...) +
          scale_x_discrete() +
          scale_fill_grey(start=0.4, end=0.8) +
          labs(x=y.name, y="", fill=x.names[i]) +
          theme(axis.text=element_text(size=axis.size),
                axis.title=element_text(size=label.size),
                legend.title=element_text(size=label.size),
                legend.text=element_text(size=0.9*label.size),
                #legend.position="top",
                legend.key.size=unit(0.5,"line"))
          ), list(i=i))
        }
      }
    }
  cowplot::plot_grid(plotlist=pp)
  }

# observed versus predicted values (regression only)
predPlot <- function(caret_fit, xlab="observed", ylab="predicted", cex=0.8, col="black",  add.grid=TRUE, show.id=FALSE, ...) {
  if(identical(caret_fit$modelType,"Regression")==F) stop("Implemented for regression tasks only",call.=F)
  fit <- fitted(caret_fit)
  plot(fit$observed, fit$predicted, xlab=xlab, ylab=ylab, type="n", ...)
  if(add.grid) grid()
  if(show.id) {
    text(fit$observed, fit$predicted, labels=fit$id, cex=cex, col=col)
    } else {
    points(fit$observed, fit$predicted, cex=cex, col=col)
    }
  abline(0,1)
  box()
  }

# correlogram
corPlot <- function(data, upper.panel=panel.cor, ...) {
  dataOK <- model.matrix(~., data=data)[,-1,drop=F]
  suppressWarnings(
    corrgram(dataOK, upper.panel=upper.panel, ...)
    )
  }