# linear regression in log scale - mean
loglm_mean <- getModelInfo("lm", regex = FALSE)[[1]]
loglm_mean$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
  dat$.outcome <- log(y)
  if(!is.null(wts)) {
    if(param$intercept)
      out <- lm(.outcome ~ ., data = dat, weights = wts, ...)
    else
      out <- lm(.outcome ~ 0 + ., data = dat, weights = wts, ...)
    } else {
    if(param$intercept)
      out <- lm(.outcome ~ ., data = dat, ...)
    else
      out <- lm(.outcome ~ 0 + ., data = dat, ...)
    }
  out
  }
loglm_mean$predict <- function(modelFit, newdata, submodels = NULL) {
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
  exp(predict(modelFit, newdata)+0.5*summary(modelFit)$sigma^2)
  }

# linear regression in log scale - median
loglm_median <- loglm_mean
loglm_median$predict <- function(modelFit, newdata, submodels = NULL) {
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
  exp(predict(modelFit, newdata))
  }

# svm radial model
svm_radial <- list(
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
  varImp = NULL,
  predictors = function(x, ...){
    out <- if(!is.null(x$terms)) predictors.terms(x$terms) else x$xNames
    if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
    if(is.null(out)) out <-NA
    out
    },
  levels = function(x) x$levels,
  sort = function(x) x[order(x$cost, x$gamma),]
  )

# summary for classification tasks
customSummaryClass <- function(data, lev = NULL, model = NULL) {
  if(length(lev)==2) {
    rocObject <- try(pROC::roc(data$obs, data[, lev[2]], direction = "<", quiet = TRUE), silent = TRUE)
    #if(inherits(rocObject, "try-error")) {
    #  rocAUC <- NA
    #  } else {
    #  rocAUC <- rocObject$auc
    #  }
    bayes <- pROC::coords(rocObject, x=0.5, ret=c("spec","sens"), transpose=T)
    youd <- pROC::coords(rocObject, x="best", ret=c("spec","sens","thresh"), transpose=T)
    if(is.matrix(youd)) {
      d <- apply(youd,2,function(x){abs(x[1]-x[2])})
      youd <- youd[,which.min(d)]
      }
    out <- c(#rocAUC,
             #specificity(data[, "pred"], data[, "obs"], lev[1]),
             #sensitivity(data[, "pred"], data[, "obs"], lev[2]),
             rocObject$auc, unlist(bayes),unlist(youd)
             )
    names(out) <- c("Accuracy","Spec","Sens","Spec_youden", "Sens_youden","cut_youden")
    out
    } else {
    cmat <- table(data$obs,data$pred)
    acc0 <- sum(diag(cmat))/sum(cmat)
    acc <- c()
    for(i in 1:nrow(cmat)) acc[i] <- cmat[i,i]/sum(cmat[i,])
    names(acc) <- rownames(cmat)
    c(Accuracy=acc0,acc)
    }
  }

# summary for regression tasks
customSummaryReg <- function(data, lev = NULL, model = NULL) {
  yobs <- data$obs
  ypred <- data$pred
  mse <- mean((yobs-ypred)^2)
  mae <- mean(abs(yobs-ypred))
  if(var(ypred)>0) rsq <- (cor(yobs,ypred))^2 else rsq <- NA
  if(sum(yobs<=0)==0) {
    mape <- 100*mean(abs((yobs-ypred)/yobs))
    out <- c(mse,sqrt(mse),mae,mape,rsq)
    names(out) <- c("MSE","RMSE","MAE","MAPE","R-squared")    
    } else {
    out <- c(mse,sqrt(mse),mae,rsq)
    names(out) <- c("MSE","RMSE","MAE","R-squared")
    }
  out
  }

# get metrics at best tuning
bestTune <- function(caret_fit) {
  best <- which(rownames(caret_fit$results)==rownames(caret_fit$bestTune))
  caret_fit$results[best,]
  }

# get coefficients at best tuning for an elastic net
enetCoef <- function(enet_caret_fit) {
  as.matrix(predict(enet_caret_fit$finalModel, type="coefficients",
    s=enet_caret_fit$bestTune$lambda))[,1]
  }

# metric versus parameter plot
trainPlot <- function(caret_fit, par=NULL, metric=NULL, r.max=10, ylab=NULL, xlab=NULL, vcol="red", ...) {
  if(is.null(metric)) metric <- caret_fit$metric
  if(is.null(par)) par <- colnames(caret_fit$bestTune)[1]
  if(is.null(ylab)) ylab <- metric
  if(is.null(xlab)) xlab <- par
  plot(caret_fit$results[,par], caret_fit$results[,metric], ylab=ylab, xlab=xlab, type="l", ...)
  abline(v=caret_fit$bestTune, col=vcol)
  }

# extract variables in formula (auxiliary)
extrVar <- function(formula, data) {
  labels(terms(formula[-2], data=data))
  }

# add polynomial and/or logarithmic terms to a formula
addTerms <- function(formula, max.deg=2, add.log=FALSE, data) {
  nomi <- extrVar(formula, data=data)
  if(length(nomi)>0) {
    xstr <- c()
    for(i in 1:length(nomi)) {
      xstr <- c(xstr, nomi[i])
      if(nomi[i] %in% colnames(data)) {
        ix <- data[,nomi[i]]
        if(is.numeric(ix)) {
          if(!identical(sort(unique(na.omit(ix))),c(0,1))) {
            if(add.log & sum(ix<=0)==0) xstr <- c(xstr, paste0("log(",nomi[i],")"))
            xstr <- c(xstr, paste0("I(",nomi[i],"^",2:max.deg,")"))
            }
          }
        }
      }
    formula(paste0(as.character(formula)[2],"~",paste0(xstr,collapse="+")))
    } else {
    formula
    }
  }

# backward stepwise selection via cross-validation
stepCV <- function(formula, data, method, trControl, max.deg=2, add.log=FALSE, maximize=FALSE, quiet=FALSE, ...) {
  mseOK <- Inf
  formOK <- addTerms(formula, max.deg=max.deg, add.log=add.log, data=data)
  fine <- 0
  ind <- 1
  while(fine==0) {
    auxmse <- c()
    auxform <- auxmod <- list()
    nomi <- extrVar(formOK)
    for(i in 1:length(nomi)) {
      if(quiet==F) cat('\r',"Step ",ind,". Checked ",i,"/",length(nomi)," variables",sep="")
      auxform[[i]] <- update(formOK, formula(paste0(".~.-",nomi[i])))
      itrain <- suppressWarnings(
        train(auxform[[i]], data=data, method=method, trControl=trControl, maximize=maximize, ...)
        )
      imse <- itrain$results[rownames(itrain$bestTune),itrain$perfNames[1]]
      auxmse[i] <- ifelse(maximize,-1,1)*imse
      auxmod[[i]] <- itrain
      }
    mseCurrent <- auxmse[which.min(auxmse)]
    if(mseCurrent<mseOK) {
      mseOK <- mseCurrent
      formOK <- auxform[[which.min(auxmse)]]
      modOK <- auxmod[[which.min(auxmse)]]
      } else {
      fine <- 1
      }
    ind <- ind+1
    if(quiet==F) cat("\n")
    }
  if(quiet==F) cat("End with ",length(extrVar(formOK))," variables",sep="","\n")
  modOK
  }

# plot of variable importance
importancePlot <- function(caret_fit, ylab="", add.grid=TRUE, cex.names=0.9, dist.names=0.5, ...) {
  mod <- caret_fit$finalModel
  if(identical(class(mod),"randomForest")) {
    imp0 <- randomForest::importance(mod)
    imp <- imp0[,1]
    names(imp) <- rownames(imp0)
    } else if(identical(class(mod),"rpart")) {
    imp <- mod$variable.importance
    } else {
    stop("Not available for class '",class(mod),"'",sep="")  
    }
  impS <- (imp-min(imp))/(max(imp)-min(imp))
  impOK <- sort(impS)
  #if(is.null(ylab)) ylab <- "Mean decrease in impurity"
  plot(impOK, type="n", xaxt="n", xlab="", ylab=ylab, ...)
  if(add.grid) grid()
  points(impOK)
  axis(1, at=1:length(impOK), labels=names(impOK), las=2, cex.axis=cex.names, tick=F, mgp=c(3,dist.names,0))
  box()
  }

# roc curve
rocPlot <- function(caret_fit, ...) {
  tab <- caret_fit$pred
  #pred <- do.call(c,lapply(split(tab[,x$levels[2]],tab[,"rowIndex"]),mean))
  #obs <- do.call(c,lapply(split(tab[,"obs"],tab[,"rowIndex"]),function(z){z[1]}))
  suppressWarnings(
    rocObj <- pROC::roc(response=tab[,"obs"], predictor=tab[,caret_fit$levels[2]], quiet=T)
    )
  plot(rocObj, ...)
  }
