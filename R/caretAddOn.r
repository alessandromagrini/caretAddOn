# linear regression in log scale - mean
loglm_mean <- caret::getModelInfo("lm", regex = FALSE)[[1]]
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

# svm linear model
svm_linear <- caret::getModelInfo("svmLinear2", regex = FALSE)[[1]]
svm_linear$varImp <- function(object, ...) {
  varImps <- abs(t(t(object$coefs)%*%object$SV))
  out <- data.frame(varImps)
  colnames(out) <- "Overall"
  if(!is.null(names(varImps))) rownames(out) <- names(varImps)
  out
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
    if(youd[3]==Inf) youd[3] <- 1
    if(youd[3]==-Inf) youd[3] <- 0
    out <- c(rocObject$auc, unlist(bayes),unlist(youd))
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

# summary for regression tasks - mean metrics
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

# summary for regression tasks - median metrics
customSummaryReg_median <- function(data, lev = NULL, model = NULL) {
  yobs <- data$obs
  ypred <- data$pred
  mse <- median((yobs-ypred)^2)
  mae <- median(abs(yobs-ypred))
  if(var(ypred)>0) rsq <- (cor(yobs,ypred))^2 else rsq <- NA
  if(sum(yobs<=0)==0) {
    mape <- 100*median(abs((yobs-ypred)/yobs))
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
enetCoef <- function(caret_fit) {
  if(identical(caret_fit$method,"glmnet")==F) stop("Implemented for method 'glmnet' only",call.=F)
  as.matrix(predict(caret_fit$finalModel, type="coefficients",
    s=caret_fit$bestTune$lambda))[,1]
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
    nomi <- extrVar(formOK,data=data)
    for(i in 1:length(nomi)) {
      if(quiet==F) cat('\r',"Step ",ind,". Checked ",i,"/",length(nomi)," variables",sep="")
      auxform[[i]] <- update(formOK, formula(paste0(".~.-",nomi[i])))
      itrain <- suppressWarnings(
        caret::train(auxform[[i]], data=data, method=method, trControl=trControl, maximize=maximize, ...)
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
  if(quiet==F) cat("End with ",length(extrVar(formOK,data=data))," variables",sep="","\n")
  modOK
  }

# plot of variable importance
importancePlot <- function(caret_fit, ylab="", add.grid=TRUE, cex.points=0.8, cex.names=0.8, dist.names=0.5, ...) {
  imp0 <- tryCatch(caret::varImp(caret_fit)$importance,error=function(e){NULL})
  if(!is.null(imp0)) {
    imp <- imp0[,1]
    names(imp) <- rownames(imp0)
    impS <- imp/sum(imp)
    impOK <- sort(impS)
    plot(impOK, type="n", xaxt="n", xlab="", ylab=ylab, ...)
    if(add.grid) grid()
    points(impOK, cex=cex.points)
    axis(1, at=1:length(impOK), labels=names(impOK), las=2, cex.axis=cex.names, tick=F, mgp=c(3,dist.names,0))
    box()
    }
  }

# roc curve
rocPlot <- function(caret_fit, lwd=2, quiet=TRUE, ...) {
  if(identical(caret_fit$modelType,"Classification")==F & length(caret_fit$levels)==2) stop("Implemented for binary classification tasks only",call.=F)
  tab <- caret_fit$pred
  pred <- do.call(c,lapply(split(tab[,caret_fit$levels[2]],tab[,"rowIndex"]),mean))
  obs <- do.call(c,lapply(split(tab[,"obs"],tab[,"rowIndex"]),function(z){z[1]}))
  suppressWarnings(
    #rocObj <- pROC::roc(response=tab[,"obs"], predictor=tab[,caret_fit$levels[2]], lwd=lwd, quiet=quiet, ...)
    rocObj <- pROC::roc(response=obs, predictor=pred, lwd=lwd, quiet=quiet, ...)
    )
  plot(rocObj, ...)
  }

# observed versus predicted values
predPlot <- function(caret_fit, cex=0.8, col=1, xlab="observed", ylab="predicted", add.grid=TRUE, ...) {
  if(identical(caret_fit$modelType,"Regression")==F) stop("Implemented for regression tasks only",call.=F)
  tab <- caret_fit$pred
  pred <- do.call(c,lapply(split(tab[,"pred"],tab[,"rowIndex"]),mean))
  obs <- do.call(c,lapply(split(tab[,"obs"],tab[,"rowIndex"]),function(z){z[1]}))  
  plot(obs, pred, xlab=xlab, ylab=ylab, type="n", ...)
  if(add.grid) grid()
  points(obs, pred, cex=cex, col=col)
  abline(0,1)
  box()
  }

# individual prediction errors
iErr <- function(caret_fit, metric=NULL) {
  if(identical(caret_fit$modelType,"Regression")==F) stop("Implemented for regression tasks only",call.=F)
  tab <- caret_fit$pred
  pred <- do.call(c,lapply(split(tab[,"pred"],tab[,"rowIndex"]),mean))
  obs <- do.call(c,lapply(split(tab[,"obs"],tab[,"rowIndex"]),function(z){z[1]}))  
  RMSE <- sqrt((pred-obs)^2)
  MAE <- abs(pred-obs)
  if(sum(obs<=0)==0) MAPE <- abs((pred-obs)/obs) else mape <- NA 
  mat <- cbind(RMSE,MAE,MAPE) 
  rownames(mat) <- names(obs)
  if(!is.null(metric)) {
    metric <- toupper(metric)
    aux <- setdiff(metric,colnames(mat))
    if(length(aux)>0) stop("Metric '",aux[1],"' unavailable: use one among 'RMSE', 'MAE', and 'MAPE'",call.=F)
    mat[,metric]
    } else {
    mat
    }
  }

# cook's distance
cookDist <- function(caret_fit, plot=TRUE, print=TRUE, cex=0.6, ...) {
  tab <- caret_fit$pred
  if(identical(caret_fit$modelType,"Regression")) {
    pred <- do.call(c,lapply(split(tab[,"pred"],tab[,"rowIndex"]),mean))
    obs <- do.call(c,lapply(split(tab[,"obs"],tab[,"rowIndex"]),function(z){z[1]}))  
    distance <- cooks.distance(lm(obs~pred))
    } else if(length(caret_fit$levels)==2) {
    tab <- caret_fit$pred
    pred <- do.call(c,lapply(split(tab[,caret_fit$levels[2]],tab[,"rowIndex"]),mean))
    obs <- do.call(c,lapply(split(tab[,"obs"],tab[,"rowIndex"]),function(z){z[1]}))
    distance <- cooks.distance(glm(obs~pred, family="binomial"))
    } else {
    stop("Not implemented for multiple classification",call.=F)  
    }
  if(plot) {
    plot(distance, type="n", ...)
    text(distance, labels=names(distance), cex=cex)
    }
  if(print) sort(distance, decreasing=T)
  #sort(boxplot(distance, range=range, plot=F)$out, decreasing=T)
  }

# scatterplot with regression curve
scatPlot <- function(y.name, x.name, data, log.y=FALSE, log.x=FALSE, deg=0, orig.scale=TRUE, nval.min=3, xlab=NULL, ylab=NULL, add.grid=TRUE, points.col="grey40", points.cex=0.6, line.lty=1, line.lwd=1, line.col=1, add=FALSE, ...) {
  y <- data[,y.name]
  x <- data[,x.name]
  if(is.null(ylab)) ylab <- y.name
  if(is.null(xlab)) xlab <- x.name
  if(is.numeric(x) && (identical(x,round(x)) & nlevels(factor(x))<nval.min)) x <- factor(x)
  if(is.numeric(y)) {
    if(is.numeric(x) && nlevels(factor(x))>2) {
      if(sum(y<=0)>0) log.y <- F
      if(sum(x<=0)>0) log.x <- F
      if(log.y) {
        fy <- "log(y)"
        y0 <- log(y)
        if(orig.scale==F & is.null(ylab)) ylab <- paste0("log(",y.name,")")
        } else {
        fy <- "y"
        y0 <- y
        }
      if(log.x) {
        fx <- "log(x)"
        x0 <- log(x)
        if(orig.scale==F & is.null(xlab)) xlab <- paste0("log(",x.name,")")
        } else {
        fx <- "x"
        x0 <- x
        }
      if(deg>0) {
        xseq <- seq(min(x,na.rm=T),max(x,na.rm=T),length=100)
        form <- paste0(fy,"~poly(",fx,",deg)")
        mod <- lm(formula(form))
        xpred <- predict(mod, data.frame(x=xseq))
        }
      if(orig.scale) {
        if(add==F) plot(x, y, ylab=ylab, xlab=xlab, type="n", ...)
        if(add.grid) grid()
        if(add==F) points(x, y, col=points.col, cex=points.cex)
        if(deg>0) {
          if(log.y) xpred <- exp(xpred)
          lines(xseq, xpred, col=line.col, lwd=line.lwd, lty=line.lty)
          }
        } else {
        if(add==F) plot(x0, y0, ylab=ylab, xlab=xlab, type="n", ...)
        if(add.grid) grid()
        if(add==F) points(x0, y0, col=points.col, cex=points.cex)
        if(log.x) xseq0 <- log(xseq) else xseq0 <- xseq
        if(deg>0) lines(xseq0, xpred, col=line.col, lwd=line.lwd, lty=line.lty)
        }
      } else {
      if(add.grid) {
        plot(factor(x), y, xlab=xlab, ylab=ylab, frame=F, col=NA, border=NA, ...)
        grid()
        plot(factor(x), y, xlab=NULL, ylab=NULL, col=NA, add=T, ...)
        } else {
        plot(factor(x), y, xlab=xlab, ylab=ylab, ...)  
        }
      }
    } else {
    if(is.numeric(x)) {
      if(add.grid) {
        plot(factor(y), x, xlab=ylab, ylab=xlab, frame=F, col=NA, border=NA, ...)
        grid()
        plot(factor(y), x, xlab=NULL, ylab=NULL, col=NA, add=T, ...)
        } else {
        plot(factor(y), x, xlab=ylab, ylab=xlab, ...)  
        }
      } else {
      #tab <- t(prop.table(table(y,x),1))
      #bp <- barplot(tab, beside=T, ylim=c(0,1), xlab=ylab, ylab=paste0("Pr(",x.name," = ?)"))
      #text(bp, tab+0.05, labels=levels(factor(x)), cex=0.8, xpd=T)
      tab <- t(prop.table(table(x,y),1))
      bp <- barplot(tab, beside=T, ylim=c(0,1), xlab=xlab, ylab=paste0("Pr(",y.name," = ?)"))
      text(bp, tab+0.05, labels=levels(factor(y)), cex=0.8, xpd=T)
      }
    }
  box()
  }

# correlogram
corPlot <- function(data, upper.panel=panel.cor, ...) {
  dataOK <- model.matrix(~.,data=data)[,-1,drop=F]
  suppressWarnings(
    corrgram(dataOK, upper.panel=upper.panel, ...)
    )
  }
