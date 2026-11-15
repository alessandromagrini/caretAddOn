###  NEW MODELS  ###

# extended generalized linear model
glm_extended <- list(
  label = "Extended generalized linear model",
  library = "MASS",
  type = c("Regression", "Classification"),
  parameters = data.frame(
    parameter = "none",
    class = "numeric",
    label = "No tuning parameter"
    ),
  grid = function(x, y, len = NULL, search = "grid") {
    data.frame(none = 1)
    },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    dots <- list(...)
    max.deg <- if (!is.null(dots$max.deg)) as.integer(dots$max.deg) else 1
    log.terms <- if (!is.null(dots$log.terms)) isTRUE(dots$log.terms) else FALSE
    log.response <- if (!is.null(dots$log.response)) isTRUE(dots$log.response) else FALSE
    bias.adj <- if (!is.null(dots$bias.adj)) isTRUE(dots$bias.adj) else FALSE
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
    fit$call$formula <- formula(fit)
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
    form <- as.formula(paste(".outcome ~", paste(rhs, collapse = " + ")))
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
  ok <- is.finite(data$obs) & is.finite(data$pred)
  yobs  <- data$obs[ok]
  ypred <- data$pred[ok]
  mse <- mean((yobs-ypred)^2)
  mae <- mean(abs(yobs-ypred))
  rmse <- sqrt(mse)
  rel_rmse <- rmse/sd(yobs, na.rm=T)
  rel_mae <- mae/mean(abs(yobs-mean(yobs, na.rm=T)),na.rm=T)
  rsq <- mape <- NA
  if (length(ypred) >= 2L && var(ypred, na.rm=T) > 0) {
    rsq <- (cor(yobs, ypred, use="pairwise.complete.obs"))^2
    }
  segni <- unique(sign(na.omit(yobs)))
  if (length(segni) == 1L && segni != 0) {
    mape <- 100*mean(abs((yobs-ypred)/yobs), na.rm=T)
    }
  out <- c(mse,rmse,mae,rel_rmse,rel_mae,mape,rsq)
  names(out) <- c("MSE","RMSE","MAE","Relative_RMSE","Relative_MAE","MAPE","R-squared")
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

# plot of partial effects in a gam model (reference: mean for quantitative Xs, mode for qualitative Xs)
gamPlot <- function(gam.model, x.name, n.grid=100, type="link", level=0.95, xlab=NULL, ylab=NULL, ylim=NULL, ...) {
  if(inherits(gam.model, "train")) gam.model <- gam.model$finalModel
  if(inherits(gam.model, "gam")==F) stop("Argument 'gam.model' must be an object of class 'gam'")
  if(length(x.name)>1) x.name <- x.name[1]
  x_all <- unlist(lapply(gam.model$smooth, function(z) z$term))
  if((x.name %in% x_all)==FALSE) stop("Variable '", x.name, "' has not a spline term")
  data <- gam.model$model
  type <- match.arg(type)
  if(is.numeric(level)) level <- max(min(1,level[1]),0)
  if(!is.numeric(level) || is.na(level) || level==1) {
    level <- 0.95
    warning("Invalid argument 'level': it was set to 0.95")
    }
  #
  Mode <- function(x) {
    x_obs <- x[!is.na(x)]
    ux <- unique(x_obs)
    ux[which.max(tabulate(match(x_obs, ux)))]
    }
  #
  ref_row <- lapply(data, function(z) {
    if (is.numeric(z)) {
      mean(z, na.rm = TRUE)
      } else if (is.factor(z)) {
      factor(as.character(Mode(z)), levels = levels(z), ordered = is.ordered(z))
      } else if (is.character(z)) {
      Mode(z)
      } else if (is.logical(z)) {
      Mode(z)
      } else {
      z[which(!is.na(z))[1]]
      }
    })
  ref_row <- as.data.frame(ref_row, stringsAsFactors = FALSE)
  x <- data[[x.name]]
  if (is.numeric(x)) {
    x_grid <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n.grid)
    } else {
    x_grid <- if (is.factor(x)) levels(x) else sort(unique(x))
    }
  newdata <- ref_row[rep(1, length(x_grid)), , drop = FALSE]
  if (is.factor(x)) {
    newdata[[x.name]] <- factor(x_grid, levels = levels(x), ordered = is.ordered(x))
    } else {
    newdata[[x.name]] <- x_grid
    }
  zcrit <- qnorm(1-(1-level)/2)
  form_gam <- gam.model$call$formula
  form_glm <- as.formula(
    gsub("s\\(([^,\\)]+).*?\\)", "\\1", paste(deparse(form_gam),collapse=" "))
    )
  glm.model <- update(gam.model, form_glm)
  p1 <- predict(glm.model, newdata=newdata, type=type, se.fit=TRUE)
  fit1 <- as.numeric(p1$fit)
  se1  <- as.numeric(p1$se.fit)
  lo1  <- fit1 - zcrit * se1
  hi1  <- fit1 + zcrit * se1
  col1_fill <- grDevices::adjustcolor("red", alpha.f=.2)
  p2 <- predict(gam.model, newdata=newdata, type=type, se.fit=TRUE)
  fit2 <- as.numeric(p2$fit)
  se2  <- as.numeric(p2$se.fit)
  lo2  <- fit2 - zcrit * se2
  hi2  <- fit2 + zcrit * se2
  col2_fill <- grDevices::adjustcolor("blue", alpha.f=.2)
  if(is.null(xlab)) xlab <- x.name 
  if(is.null(ylab)) ylab <- "Partial effect"
  if(is.null(ylim)) ylim <- range(c(lo1,lo2,hi1,hi2))
  plot(x_grid, fit2, type = "n", xlab=xlab, ylab=ylab, ylim=ylim, ...)
  grid()
  polygon(c(x_grid, rev(x_grid)), c(lo1, rev(hi1)),
          col = col1_fill, border = NA)
  lines(x_grid, fit1, col="red")
  polygon(c(x_grid, rev(x_grid)), c(lo2, rev(hi2)),
          col = col2_fill, border = NA)
  lines(x_grid, fit2, col="blue")
  box()
  }


# permutation variable importance
permVarImp <- function(model, data, nsim = 10, metric = NULL, ...) {
  if (!is.numeric(nsim) || length(nsim) != 1L ||
      !is.finite(nsim) || nsim < 1 || nsim != floor(nsim)) {
    stop("Argument 'nsim' must be a positive integer.")
    }
  supported <- c("train", "glm", "lm", "rpart","randomForest", "ranger")
  is_model <- function(x) inherits(x, supported)
  if (is_model(model)) {
    out <- permImp_fun(model = model, data = data, nsim = nsim, metric = metric, ...)
    } else if (is.list(model) && length(model) > 0L && all(vapply(model, is_model, logical(1)))) {
    impList <- lapply(model, permImp_fun, data = data, nsim = nsim, metric = metric, ...)
    xnam <- unique(unlist(lapply(impList, function(x) names(x$estimate)), use.names = FALSE))
    make_tab <- function(component) {
      do.call(cbind, lapply(impList, function(x) {
        z <- setNames(numeric(length(xnam)), xnam)
        z[names(x[[component]])] <- x[[component]]
        z
        }))
      }
    mVet <- sapply(impList, function(x){x$metric})
    out <- list(estimate = make_tab("estimate"), mc.error = make_tab("mc.error"), metric=mVet)
    } else {
    stop("Argument 'model' must be an object or a list of objects of the following classes: ",paste(supported, collapse=", "))
    }
  class(out) <- c("permVarImp", class(out))
  out
  }

# function to compute permutation importance (auxiliary)
permImp_fun <- function(model, data, nsim = 10, metric = NULL, ...) {
  dots <- list(...)
  event_level <- dots[["event_level", exact = TRUE]]
  dots[["event_level"]] <- NULL
  if (is.null(event_level)) event_level <- "second"
  target <- dots[["target", exact = TRUE]]
  dots[["target"]] <- NULL
  if (!is.data.frame(data)) stop("Argument 'data' must be a data frame.")
  if (inherits(model, "glm")) {
    y_fit <- if (!is.null(model$terms)) all.vars(model$terms[[2L]]) else NULL
    if (!is.null(model$obsLevels) || (identical(y_fit, ".outcome") && !".outcome" %in% names(data))) {
      stop("The complete 'train' object must be supplied")
      }
    }
  task <- if (inherits(model, "train")) {
    if (identical(model$modelType, "Classification")) {
      "classification"
      } else if (identical(model$modelType, "Regression")) {
      "regression"
      } else {
      stop("Unknown task '",model$modelType,"'")
      }
    } else if (inherits(model, "glm")) {
    if (model$family$family %in% c("binomial", "quasibinomial")) {
      "classification"
      } else {
      "regression"
      }
    } else if (inherits(model, "ranger")) {
    if (model$treetype %in% c("Probability estimation", "Classification")) {
      "classification"
      } else if (identical(model$treetype, "Regression")) {
      "regression"
      } else {
      stop("Invalid task '",model$treetype,"'")
      }
    } else if (inherits(model, "randomForest")) {
    if (identical(model$type, "classification")) {
      "classification"
      } else if (identical(model$type, "regression")) {
      "regression"
      } else {
      stop("Invalid task '",model$type,"'")
      }
    #
    } else if (inherits(model, "rpart")) {
    if (identical(model$method, "class")) {
      "classification"
      } else {
      "regression"
      }
    } else {  ## lm
    "regression"
    }
  #
  if (is.null(target)) {
    # use the formula, when available, only to identify the response
    f <- model$terms
    if (!is.null(f) && inherits(f, "formula")) {
      candidate <- all.vars(f[[2L]])
      if (length(candidate) == 1L && candidate %in% names(data)) target <- candidate
      }
    if (is.null(target) && inherits(model, "ranger")) {
      candidate <- model$dependent.variable.name
      if (length(candidate) == 1L && candidate %in% names(data)) target <- candidate
      }
    if (is.null(target)) {
      y_train <- if (inherits(model, "train")) {
        model$trainingData[[".outcome"]]
        } else if (inherits(model, "randomForest")) {
        model$y
        } else {
        NULL
        }
      if (!is.null(y_train) && length(y_train) == nrow(data)) {
        matches <- names(data)[vapply(data, function(x) {
          identical(unname(as.character(x)), unname(as.character(y_train)))
          }, logical(1))]
        if (length(matches) == 1L) target <- matches
        }
      }
    if (is.null(target) && identical(task, "classification")) {
      candidate <- names(data)[vapply(data, function(x) {
        length(unique(stats::na.omit(x))) == 2L
        }, logical(1))]
      if (length(candidate) == 1L) target <- candidate
      }
    if (is.null(target)) stop("The response variable cannot be identified")
    }
  if (length(target) != 1L || !is.character(target) || !target %in% names(data)) {
    stop("'target' must name a column in 'data'.")
    }
  features <- if (!is.null(model$terms)) {
    all.vars(stats::delete.response(stats::terms(model$terms)))
    } else if (inherits(model, "train")) {
    if (!is.null(model$trainingData)) {
      setdiff(names(model$trainingData), ".outcome")
      } else {
      model$coefnames
      }
    } else if (inherits(model, "ranger")) {
    model$forest$independent.variable.names
    } else if (inherits(model, "randomForest")) {
    c(rownames(model$importance), names(model$importance))
    } else {
    NULL
    }
  if (!length(features)) stop("The model seems to have no predictors")
  missing_features <- setdiff(features, names(data))
  if (length(missing_features)) stop("Variable '", missing_features[1L], "' not found in 'data'.")
  if (identical(task, "classification")) {
    if (!is.factor(data[[target]])) {
      data[[target]] <- factor(data[[target]])
      }
    if (nlevels(data[[target]]) != 2L) {
      stop("For classification, the response variable must be binary")
      }
    event_level <- match.arg(event_level, c("second", "first"))
    if (is.null(metric)) metric <- "roc_auc"
    class_metrics <- c("accuracy", "bal_accuracy", "youden")
    prob_metrics <- c("roc_auc", "pr_auc", "logloss", "brier")
    if (!is.character(metric) || length(metric) != 1L || is.na(metric) || !metric %in% c(class_metrics, prob_metrics)) {
      stop("Invalid metric: '",,"'. For classification, use ome among: ", paste(c(class_metrics, prob_metrics), collapse = ", "))
      }
    livelli <- if (inherits(model, "train")) {
      model$levels
      } else if (inherits(model, "glm")) {
      y_model <- model$model[[1L]]
      if (is.factor(y_model)) levels(y_model) else levels(data[[target]])
      } else if (inherits(model, "ranger")) {
      model$forest$levels
      } else if (inherits(model, "randomForest")) {
      levels(model$y)
      } else {
      attr(model, "ylevels")  # rpart
      }
    if (length(livelli) != 2L || !setequal(levels(data[[target]]), livelli)) {
      stop("Response levels in 'data' are inconsistent with the model")
      }
    data[[target]] <- factor(as.character(data[[target]]), levels = livelli)
    evento <- livelli[if (event_level == "second") 2L else 1L]
    if (metric %in% prob_metrics && inherits(model, "ranger") && !identical(model$treetype, "Probability estimation")) {
      stop("Metric '",metric,"' requires a ranger model fitted with 'probability = TRUE'")
      }
    pfun <- function(object, newdata) {
      if (metric %in% prob_metrics) {
        if (inherits(object, "glm")) {
          p <- stats::predict(object, newdata = newdata, type = "response")
          return(as.numeric(if (event_level == "second") p else 1 - p))
          }
        p <- if (inherits(object, "ranger")) {
          predict(object, data = newdata)$predictions
          } else {
          predict(object, newdata = newdata, type = "prob")
          }
        if (!(is.matrix(p) || is.data.frame(p)) || !evento %in% colnames(p)) {
          stop("Predictions for class '", evento, "' not found")
          }
        return(as.numeric(p[, evento]))
        }
      # accuracy, bal_accuracy, youden: predicted classes are needed
      pred <- if (inherits(object, "glm")) {
        p <- stats::predict(object, newdata = newdata, type = "response")
        ifelse(p >= 0.5, livelli[2L], livelli[1L])
        } else if (inherits(object, "ranger")) {
        predict(object, data = newdata)$predictions
        } else if (inherits(object, "train")) {
        predict(object, newdata = newdata, type = "raw")
        } else if (inherits(object, "rpart")) {
        predict(object, newdata = newdata, type = "class")
        } else {
        predict(object, newdata = newdata, type = "response")
        }
      if (is.matrix(pred) || is.data.frame(pred)) {
        pred <- colnames(pred)[max.col(as.matrix(pred), ties.method = "first")]
        }
      factor(as.character(pred), levels = livelli)
      }
    } else {
    if (!is.numeric(data[[target]])) {
      stop("The task is regression but the response variable is not numeric")
      }
    if (is.null(metric)) metric <- "rmse"
    regression_metrics <- c("mae", "mape", "rmse", "rsq", "rsq_trad")
    if (!is.character(metric) || length(metric) != 1L || is.na(metric) || !metric %in% regression_metrics) {
      stop("Invalid regression metric. Use one among: ", paste(regression_metrics, collapse = ", "))
      }
    if (identical(metric, "mape")) {
      segni <- unique(sign(na.omit(data[[target]])))
      if (length(segni) != 1L || segni == 0) {
        stop("Metric 'mape' is undefined. Use one among: ", paste(setdiff(regression_metrics,"mape"), collapse = ", "))
        }
      }
    pfun <- function(object, newdata) {
      p <- if (inherits(object, "ranger")) {
        predict(object, data = newdata)$predictions
        } else if (inherits(object, "rpart")) {
        predict(object, newdata = newdata, type = "vector")
        } else if (inherits(object, "glm")) {
        stats::predict(object, newdata = newdata, type = "response")
        } else {
        predict(object, newdata = newdata)
        }
      as.numeric(p)
      }
    }
  out <- do.call(vip::vi_permute, c(list(
    object = model, train = data, target = target,
    feature_names = features, metric = metric, nsim = nsim,
    event_level = if (task == "classification") event_level else NULL,
    pred_wrapper = pfun), dots))
  out_est <- out[,"Importance"]
  if(nsim>1) {
    out_se <- out[,"StDev"]/sqrt(nsim)
    } else {
    out_se <- rep(0, nrow(out))  
    }
  names(out_est) <- names(out_se) <- out[,"Variable"]
  metric_out <- if(is.null(metric)) attr(out, "metric") else metric
  list(estimate=out_est, mc.error=out_se, metric=metric_out)
  }

# print method for class 'permVarImp'
print.permVarImp <- function(x, digits=8, ...) {
  cat("Permutation variable importance (metric: '",names(sort(table(x$metric))[1]),"')\n", sep="")
  print(round(x$estimate, digits=digits))
  }


###  NEW GRAPHICS  ###

# plot method for class 'permVarImp'
plot.permVarImp <- function(x, ordered = TRUE, title = "", cex.axis = 1, cex.title = 1, mgp = c(3, 0.5, 0), add.legend = TRUE, legend.pos = "topright", cex.legend = 0.9, grid.col = "grey85", grid.lty = 2, ...) {
  est <- x$estimate
  if(ordered) {
    ord <- order(
      if (is.matrix(est)) est[, 1L] else est,
      decreasing = TRUE, na.last = TRUE)
    est <- if (is.matrix(est)) {
      est[ord, , drop = FALSE]
      } else {
      est[ord]
      }
    }
  multiple <- is.matrix(est)
  heights <- if (multiple) {
    t(est)
    } else {
    matrix(as.numeric(est), nrow = 1L, dimnames = list(NULL, names(est)))
    }
  pos <- barplot(heights, beside = multiple, las = 2,
    axisnames = FALSE, cex.axis = cex.axis,
    main = title, cex.main = cex.title,
    panel.first = grid(nx = NA, ny = NULL, col = grid.col, lty = grid.lty),
    legend.text = multiple && add.legend,
    args.legend = list(x = legend.pos, bty = "n", cex = cex.legend)
    )
  centri <- if (is.matrix(pos)) colMeans(pos) else pos
  axis(1, at = centri, labels = colnames(heights),
    tick = FALSE, las = 2, cex.axis = cex.axis, mgp = mgp)
  }

# plot of grid search
trainPlot <- function(caret_fit, par=NULL, metric=NULL, ylab=NULL, xlab=NULL, vcol="red", ...) {
  if(is.null(metric)) metric <- caret_fit$metric
  if(is.null(par)) par <- colnames(caret_fit$bestTune)[1]
  if(is.null(ylab)) ylab <- metric
  if(is.null(xlab)) xlab <- par
  plot(caret_fit$results[,par], caret_fit$results[,metric], ylab=ylab, xlab=xlab, type="l", ...)
  abline(v=caret_fit$bestTune[1,par], col=vcol)
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
multiPairPlot <- function(y.name, x.names=NULL, data, coef=1.5, outliers=TRUE, axis.size=6, label.size=10, point.size=0.6, smooth.size=0.6, smooth.color="blue", ...) {
  if(!is.logical(outliers)) outliers <- F else outliers <- outliers[1]
  if(is.null(x.names)) x.names <- colnames(data)
  yaux <- intersect(y.name,colnames(data))
  x.names <- setdiff(intersect(x.names,colnames(data)),yaux)
  if(length(yaux)==0) stop("No valid variable name in argument 'y.name'",call.=F) 
  if(length(x.names)==0) stop("No valid variable name in argument 'x.names'",call.=F) 
  y.name <- yaux[1]
  pp <- list()
  if(is.numeric(data[,y.name])) {
    for (i in seq_along(x.names)) {
      idat <- data.frame(x = data[[x.names[i]]], y = data[[y.name]])
      smooth_layer <- NULL
      if (is.numeric(idat$x)) {
        iok <- is.finite(idat$x) & is.finite(idat$y)
        iuni <- length(unique(idat$x[iok]))
        if (iuni >= 4) {
          formula_smooth <- as.formula(sprintf("y ~ s(x, bs = 'cs', k = %d)", min(10L, iuni)))
          metodo_smooth <- "gam"
          } else if (iuni >= 2) {
          formula_smooth <- y ~ x
          metodo_smooth <- "lm"
          }
        if (iuni >= 2) {
          smooth_layer <- geom_smooth(data = idat[iok, , drop = FALSE],
            formula = formula_smooth, method = metodo_smooth,
            linewidth = smooth.size, color = smooth.color, se = TRUE)
          }
        }
      pp[[i]] <- ggplot(idat, aes(x = x, y = y)) +
        geom_point(size = point.size, ...) +
        smooth_layer +
        labs(x = x.names[i], y = y.name) +
        theme(
          axis.text = element_text(size = axis.size),
          axis.title = element_text(size = label.size)
        )
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
predPlot <- function(caret_fit, xlab="observed", ylab="predicted", cex=0.8, col="black",  add.grid=TRUE, show.row=FALSE, ...) {
  if(identical(caret_fit$modelType,"Regression")==F) stop("Implemented for regression tasks only",call.=F)
  fit <- fitted(caret_fit)
  plot(fit$observed, fit$predicted, xlab=xlab, ylab=ylab, type="n", ...)
  if(add.grid) grid()
  if(show.row) {
    #text(fit$observed, fit$predicted, labels=fit$id, cex=cex, col=col)
    text(fit$observed, fit$predicted, labels=rownames(fit), cex=cex, col=col)
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


###  EM IMPUTATION  ###

# box-cox transformation (auxiliary)
makeBoxCox <- function(x, par=NULL, a=NULL) {
  if(is.null(a)) {
    if(sum(x<=0,na.rm=T)==0) a <- 0 else a <- -min(x,na.rm=T)
    if(sum(x+a==0,na.rm=T)>0) a <- a+min((x+a)[which(x+a>0)],na.rm=T)/2
    }
  if(is.null(par)) {
    #f <- function(lam) {
    #  z <- ((x+a)^lam-1)/lam
    #  sum(dnorm(z,mean(z),sd(z),log=T),na.rm=T)
    #  }
    #par <- optimize(f, interval=c(-2,2), maximum=T)$maximum
    bc <- MASS::boxcox(lm(I(x+a)~1, y=T), lambda=seq(-2,2,0.001), plotit=F)
    par <- bc$x[which.max(bc$y)]
    z <- ((x+a)^par-1)/par
    } else if(par==1) {
    a <- 0
    z <- x
    } else if(par==0) {
    z <- log(x+a)  
    } else {
    z <- ((x+a)^par-1)/par
    }
  attr(z,"offset") <- a
  attr(z,"lambda") <- par
  z
  }

# invert box-cox transformation (auxiliary)
invBoxCox <- function(z, par, a, ymin=NULL) {
  if(par==1) {
    z
    } else {
    if(par==0) {
      exp(z)-a  
      } else {
      y <- (z*par+1)^(1/par)-a
      if(sum(is.nan(y))>0) {
        if(is.null(ymin)) {
          y[which(is.na(y))] <- -a
          } else {
          y[which(is.na(y))] <- ymin
          }
        }
      y
      }
    }
  }

# observed data log likelihood (auxiliary)
obsLogLik_mvn <- function(X, R, Z, B, Sigma) {
  n <- nrow(X)
  ll <- 0
  for(i in seq_len(n)) {
    obs <- which(!R[i, ])
    if(length(obs) == 0) next
    mu <- as.vector(B %*% Z[i, ])
    xo <- X[i, obs]
    muo <- mu[obs]
    Soo <- Sigma[obs, obs, drop=FALSE]
    cf <- chol(Soo)
    r <- xo - muo
    logdet <- 2 * sum(log(diag(cf)))
    quad <- sum(backsolve(cf, r, transpose = TRUE)^2)
    pobs <- length(obs)
    ll <- ll - 0.5 * (pobs * log(2*pi) + logdet + quad)
    }
  ll
  }

# solve function with tryCatch (auxiliary)
solve_safe <- function(A, b) {
  tryS <- tryCatch(solve(A, b),  error = function(e){NULL}) 
  if(is.null(tryS)) {
    stop("Matrix inversion failed due to near singularity")
    } else {
    tryS
    }
  }

# imputation using EM algorithm
mvnEM <- function(x.names, data, exogenous=NULL, box.cox=1, tol.abs=1e-4, tol.rel=1e-8, maxit=500, quiet=FALSE) {
  # check arguments
  if(!is.data.frame(data)) stop("Argument 'data' must be a data.frame.")
  x.names <- intersect(x.names, colnames(data))
  non.quant <- x.names[!sapply(data[, x.names, drop=FALSE], is.numeric)]
  if(length(non.quant) > 0) {
    exogenous <- unique(c(exogenous, non.quant))
    x.names <- setdiff(x.names, non.quant)
    }
  all.miss <- sapply(data[, x.names, drop=FALSE], function(x) all(is.na(x)))
  if(any(all.miss)) {
    aux <- which(all.miss)[1]
    stop("Variable '", names(all.miss)[aux], "' is completely missing.")
    }
  x.names <- x.names[!all.miss]
  if(length(x.names) == 0) stop("No valid quantitative variables in argument 'x.names'")
  if(!is.null(exogenous)) {
    exogenous <- intersect(exogenous, colnames(data))
    ex.miss <- sapply(data[, exogenous, drop=FALSE], function(x) any(is.na(x)))
    if(any(ex.miss)) {
      aux <- which(ex.miss)[1]
      stop("Variable '", names(ex.miss)[aux], "' contains missing values.")
      }
    }
  if(!is.numeric(tol.abs) || length(tol.abs) != 1 || is.na(tol.abs) || tol.abs <= 0) tol.abs <- 1e-4
  if(!is.numeric(tol.rel) || length(tol.rel) != 1 || is.na(tol.rel) || tol.rel <= 0) tol.rel <- 1e-8
  if(!is.numeric(maxit) || length(maxit) != 1 || is.na(maxit) || maxit < 1) {
    maxit <- 500
    } else {
    maxit <- as.integer(maxit)
    }
  if(!is.logical(quiet) || length(quiet) != 1 || is.na(quiet))
    quiet <- FALSE
  # single-variable case: mean imputation
  if(length(x.names) == 1) {
    dataI <- data
    xname <- x.names[1]
    dataI[is.na(dataI[, xname]), xname] <- mean(dataI[, xname], na.rm=TRUE)
    return(list(
      data.imputed = dataI,
      data.original = data
      ))
    }
  #
  isBC <- offset <- rep(0,length(x.names))
  lambda <- rep(1,length(x.names))
  ymin <- apply(data[,x.names],2,min,na.rm=T)
  names(isBC) <- names(lambda) <- names(offset) <- names(ymin) <- x.names
  dataI <- data
  auto.bc <- FALSE
  lambda[] <- 1
  if(is.null(box.cox) || identical(box.cox, TRUE) || (is.character(box.cox) && length(box.cox) == 1 && tolower(box.cox) == "auto")) {
    auto.bc <- TRUE
    } else if(is.numeric(box.cox)) {
    if(length(box.cox) == 1) {
      lambda[] <- box.cox
      } else if(!is.null(names(box.cox))) {
      valid <- names(box.cox) %in% x.names
      lambda[names(box.cox)[valid]] <- box.cox[valid]
      }
    } else {
    lambda[] <- 1
    }
  # box-cox transformation
  if(auto.bc || any(lambda != 1)) {
    if(quiet == FALSE) {
      cat("\r")
      cat("Performing Box-Cox transformation ...")
      flush.console()
      }
    for(i in seq_along(x.names)) {
      ix <- data[, x.names[i]]
      if(sum(ix < 0, na.rm=TRUE) == 0) {
        if(auto.bc) {
          ibc <- makeBoxCox(ix)
          } else if(lambda[x.names[i]] != 1) {
          ibc <- makeBoxCox(ix, par=lambda[x.names[i]])
          } else {
          next
          }
        isBC[x.names[i]] <- 1
        dataI[, x.names[i]] <- ibc
        lambda[x.names[i]] <- attr(ibc, "lambda")
        offset[x.names[i]] <- attr(ibc, "offset")
        }
      }
    }
  # missing indicators
  isMiss <- matrix(nrow=nrow(data),ncol=length(x.names)) 
  colnames(isMiss) <- x.names
  for(i in 1:length(x.names)) {
    isMiss[,x.names[i]] <- 1*is.na(data[,x.names[i]])
    }
  for(i in 1:length(x.names)) {
    ind <- which(isMiss[,x.names[i]]==1)
    if(length(ind)>0) dataI[ind,x.names[i]] <- mean(dataI[,x.names[i]],na.rm=T)
    }
  # Z: model matrix for exogenous variables
  if (is.null(exogenous)) {
    Z <- matrix(1, nrow(dataI), 1)
    colnames(Z) <- "(Intercept)"
    } else {
    Z <- model.matrix(
      as.formula(paste("~", paste(exogenous, collapse = "+"))),
      data = dataI
      )
    }
  X <- as.matrix(dataI[, x.names])
  R <- as.matrix(isMiss[, x.names]) == 1
  # initialization: univariate mean
  Xhat <- X
  for (j in seq_len(ncol(Xhat))) {
    Xhat[is.na(Xhat[, j]), j] <- mean(Xhat[, j], na.rm = TRUE)
    }
  XtX <- crossprod(Z)
  B <- t(solve_safe(XtX, crossprod(Z, Xhat)))
  Sigma <- cov(Xhat - Z %*% t(B)) * (nrow(Xhat) - 1) / nrow(Xhat)
  ll.trace <- numeric(maxit)
  ll_old <- -Inf
  EX_old <- Xhat
  # EM cycle
  for (iter in seq_len(maxit)) {
    EX <- X
    EV <- array(0, dim = c(nrow(X), ncol(X), ncol(X)))
    for (i in seq_len(nrow(X))) {
      obs <- which(!R[i, ])
      mis <- which(R[i, ])
      mu <- as.vector(B %*% Z[i, ])
      if (length(mis) == 0) {
        EX[i, ] <- X[i, ]
        } else if (length(obs) == 0) {
        EX[i, mis] <- mu[mis]
        EV[i, mis, mis] <- Sigma[mis, mis, drop = FALSE]
        } else {
        Soo <- Sigma[obs, obs, drop = FALSE]
        Smo <- Sigma[mis, obs, drop = FALSE]
        Som <- Sigma[obs, mis, drop = FALSE]
        Smm <- Sigma[mis, mis, drop = FALSE]
        adj <- Smo %*% solve_safe(Soo, X[i, obs] - mu[obs])
        EX[i, obs] <- X[i, obs]
        EX[i, mis] <- mu[mis] + adj
        EV[i, mis, mis] <- Smm - Smo %*% solve_safe(Soo, Som)
        }
      }
    B_new <- t(solve_safe(XtX, crossprod(Z, EX)))
    Sigma_new <- matrix(0, ncol(X), ncol(X))
    for (i in seq_len(nrow(X))) {
      e <- EX[i, ] - as.vector(B_new %*% Z[i, ])
      Sigma_new <- Sigma_new + tcrossprod(e) + EV[i, , ]
      }
    Sigma_new <- Sigma_new / nrow(X)
    B_back <- B
    Sigma_back <- Sigma
    B <- B_new
    Sigma <- Sigma_new
    ll <- obsLogLik_mvn(X, R, Z, B, Sigma)
    ll.trace[iter] <- ll
    if (!quiet) {
      cat("\rEM iteration ", iter, ". Log likelihood: ", ll, sep = "")
      flush.console()
      }
    stop.reason <- "max_iter_reached"
    if(iter > 1) {
      ll.abs.change <- abs(ll - ll_old)
      ll.rel.change <- ll.abs.change / (abs(ll_old) + .Machine$double.eps)
      if(ll < ll_old) {
        B <- B_back
        Sigma <- Sigma_back
        EX <- EX_old
        ll <- ll_old
        ll.trace[iter] <- ll_old
        stop.reason <- "logLik_decreased"
        break
        }
      if(ll.abs.change < tol.abs && ll.rel.change < tol.rel) {
        stop.reason <- "convergence"
        break
        }
      }
    EX_old <- EX
    ll_old <- ll
    }
  if(quiet==FALSE) {
    cat("\n")
    switch(stop.reason,
           "convergence" = cat("Converged\n"),
           "max_iter_reached" = cat("Maximum number of iterations reached\n"),
           "logLik_decreased" = cat("Log likelihood decreased => rollback\n")
           )
    }
  ll.trace <- ll.trace[seq_len(iter)]
  colnames(Sigma) <- rownames(Sigma) <- x.names
  dataI[, x.names] <- EX
  dataI.orig <- dataI
  # conditional distribution of missing values
  missing.mu <- vector("list", nrow(X))
  missing.Sigma <- vector("list", nrow(X))
  for(i in seq_len(nrow(X))) {
    mis <- which(R[i, ])
    if(length(mis) > 0) {
      missing.mu[[i]] <- EX[i, mis]
      names(missing.mu[[i]]) <- x.names[mis]
      missing.Sigma[[i]] <- EV[i, , ][mis, mis, drop = FALSE]
      colnames(missing.Sigma[[i]]) <- rownames(missing.Sigma[[i]]) <- x.names[mis]
      } else {
      missing.mu[[i]] <- NULL
      missing.Sigma[[i]] <- NULL
      }
    }
  # imputation uncertainty on transformed scale
  cond.var <- matrix(NA, nrow = nrow(X), ncol = ncol(X))
  colnames(cond.var) <- x.names
  for(i in seq_len(nrow(X))) {
    mis <- which(R[i, ])
    if(length(mis) > 0) {
      cond.var[i, mis] <- diag(EV[i, , ][mis, mis, drop = FALSE])
      }
    }
  cond.se <- sqrt(cond.var)
  sd_z <- apply(X, 2, sd, na.rm = TRUE)
  cond.se.rel <- sweep(cond.se, 2, sd_z, "/")
  # back-transformation
  if(any(isBC == 1)) {
    for(i in 1:length(x.names)) {
      if(isBC[x.names[i]]==1) {
        dataI.orig[,x.names[i]] <- invBoxCox(dataI[,x.names[i]], par=lambda[x.names[i]], a=offset[x.names[i]], ymin=ymin[i])
        if(ymin[x.names[i]] >= 0) dataI.orig[, x.names[i]] <- pmax(dataI.orig[, x.names[i]], ymin[x.names[i]])
        }
      }
    }
  # imputation uncertainty on original scale
  cond.var.orig <- cond.var
  cond.se.orig <- cond.se
  if(any(isBC == 1)) {
    for(j in seq_along(x.names)) {
      xname <- x.names[j]
      if(isBC[xname] == 1) {
        zhat <- dataI[, xname]
        lam <- lambda[xname]
        if(lam == 0) {
          deriv <- exp(zhat)
          } else {
          deriv <- (lam * zhat + 1)^(1 / lam - 1)
          }
        cond.var.orig[, j] <- deriv^2 * cond.var[, j]
        cond.se.orig[, j] <- abs(deriv) * cond.se[, j]
        }
      }
    }
  sd_y <- sapply(data[, x.names, drop = FALSE], sd, na.rm = TRUE)
  cond.se.rel.orig <- sweep(cond.se.orig, 2, sd_y, "/")
  error_transf <- cbind(
    rmse = sqrt(colMeans(cond.var, na.rm = TRUE)),
    rmse_rel = sqrt(colMeans(cond.var, na.rm = TRUE))/sd_z
    )
  error_orig <- cbind(
    rmse = sqrt(colMeans(cond.var.orig, na.rm = TRUE)),
    rmse_rel = sqrt(colMeans(cond.var.orig, na.rm = TRUE))/sd_y
    )
  # output
  out <- list(
    data.imputed = dataI.orig,
    data.original = data,
    call = list(x.names=x.names, exogenous=exogenous),
    box.cox = list(
      parameters = cbind(lambda, offset),
      data.transformed = dataI[,x.names]
      ),
    B = B,
    Sigma = Sigma,
    logLik = ll,
    missing.conditional = list(
      mu = missing.mu,
      Sigma = missing.Sigma
      ),
    imputation.rmse = error_orig,
    algorithm = list(
      tol.abs = tol.abs, tol.rel = tol.rel, maxit = maxit,
      iterations = iter, logLik.trace = ll.trace,
      stop.reason = stop.reason
      )
    )
  class(out) <- "mvnEM"
  out
  }

# print method for class 'mvnEM'
print.mvnEM <- function(x, ...) {
  cat("Multivariate normal model estimated via the EM algorithm\n")
  cat("  (",length(x$call$x.names)," endogenous and ",length(x$call$exogenous)," exogenous variables)\n",sep="")
  nmiss <- apply(x$data.original[,x$call$x.names],2,function(x){mean(is.na(x))})
  cat("Average missingness per endogenous variable: ",round(100*mean(nmiss),1),"%\n",sep="")
  cat("Average relative RMSE of imputation: ",mean(x$imputation.rmse[,"rmse_rel"],na.rm=T),"\n",sep="")
  }

# summary method for class 'mvnEM'
summary.mvnEM <- function(object, ...) {
  list(box.cox=object$box.cox$parameters,
       B=object$B,
       Sigma=object$Sigma,
       imputation.rmse=object$imputation.rmse
       )
  }

# simulation of complete datasets
mvnEM_simul <- function(x, nsim = 1) {
  if(!inherits(x, "mvnEM")) {
    stop("Argument 'x' must be an object of class 'mvnEM'")
    }
  if(!is.numeric(nsim) || length(nsim) != 1 || is.na(nsim) || nsim < 1) {
    nsim <- 1
    }
  nsim <- as.integer(nsim)
  x.names <- x$call$x.names
  missing.mu <- x$missing.conditional$mu
  missing.Sigma <- x$missing.conditional$Sigma
  lambda <- x$box.cox$parameters[, "lambda"]
  offset <- x$box.cox$parameters[, "offset"]
  out <- vector("list", nsim)
  for(m in seq_len(nsim)) {
    Xsim <- as.data.frame(x$box.cox$data.transformed)
    for(i in seq_along(missing.mu)) {
      if(!is.null(missing.mu[[i]]) && length(missing.mu[[i]]) > 0) {
        mis.names <- names(missing.mu[[i]])
        if(length(missing.mu[[i]]) == 1) {
          Xsim[i, mis.names] <- rnorm(
            n = 1,
            mean = missing.mu[[i]],
            sd = sqrt(missing.Sigma[[i]][1, 1])
            )
          } else {
          Xsim[i, mis.names] <- MASS::mvrnorm(
            n = 1,
            mu = missing.mu[[i]],
            Sigma = missing.Sigma[[i]]
            )
          }
        }
      }
    data.sim <- x$data.original
    for(j in seq_along(x.names)) {
      xname <- x.names[j]
      ymin <- min(x$data.original[, xname], na.rm = TRUE)
      data.sim[, xname] <- invBoxCox(
        Xsim[, xname],
        par = lambda[xname],
        a = offset[xname],
        ymin = ymin
        )
      if(ymin >= 0) data.sim[, xname] <- pmax(data.sim[, xname], ymin)
      }
    out[[m]] <- data.sim
    }
  out
  }