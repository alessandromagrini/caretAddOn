###  NEW MODELS  ###

# log-linear regression model
loglm <- caret::getModelInfo("lm", regex = FALSE)[[1]]
loglm$fit <- function(x, y, wts, param, lev, last, classProbs, bias.adj=TRUE, ...) {
  dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors=TRUE)
  dat$.outcome <- log(y)
  if(!is.null(wts)) {
    if(param$intercept) {
      out <- lm(.outcome ~ ., data = dat, weights = wts, ...)
      } else {
      out <- lm(.outcome ~ 0 + ., data = dat, weights = wts, ...)
      }
    } else {
    if(param$intercept) {
      out <- lm(.outcome ~ ., data = dat, ...)
      } else {
      out <- lm(.outcome ~ 0 + ., data = dat, ...)
      }
    }
  out$bias.adj <- bias.adj
  out
  }
loglm$predict <- function(modelFit, newdata, submodels = NULL) {
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
  if(modelFit$bias.adj) adj <- 0.5*summary(modelFit)$sigma^2 else adj <- 0
  exp(predict(modelFit, newdata)+adj)
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


###  NEW SUMMARIES  ###

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
    youd <- pROC::coords(rocObject, x="best", ret=c("thresh","spec","sens"), transpose=T)
    if(is.matrix(youd)) {
      d <- apply(youd,2,function(x){abs(x[2]-x[3])})
      youd <- youd[,which.min(d)]
      }
    if(youd[1]==Inf) youd[1] <- 1
    if(youd[1]==-Inf) youd[1] <- 0
    out <- c(rocObject$auc, unlist(bayes), unlist(youd))
    names(out) <- c("Accuracy","Spec","Sens", "cut_youden", "Spec_youden", "Sens_youden")
    out
    } else {
    cmat <- table(data$obs,data$pred)
    acc0 <- sum(diag(cmat))/sum(cmat)
    acc <- c()
    for(i in 1:nrow(cmat)) acc[i] <- cmat[i,i]/sum(cmat[i,])
    names(acc) <- rownames(cmat)
    c(Accuracy=acc0, acc)
    }
  }

# summary for regression tasks - mean metrics
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

# extract variables in formula (auxiliary)
extrVar <- function(formula, data) {
  labels(terms(formula, data=data))
  }

# add polynomial and/or logarithmic terms to a formula
addTerms <- function(formula, max.deg=1, add.log=FALSE, data) {
  if(!is.numeric(max.deg)) max.deg <- 1 else max.deg <- max(1,round(max.deg))
  if(!is.logical(add.log)) add.log <- F else add.log <- add.log[1]
  nomi <- extrVar(formula, data=data)
  if(length(nomi)>0 & max.deg>1) {
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
  if(!is.logical(maximize)) maximize <- F else maximize <- maximize[1]
  if(!is.logical(quiet)) quiet <- F else quiet <- quiet[1]
  mseOK <- Inf
  formOK <- addTerms(formula, max.deg=max.deg, add.log=add.log, data=data)
  fine <- 0
  ind <- 1
  while(fine==0) {
    auxmse <- c()
    auxform <- auxmod <- list()
    nomi <- extrVar(formOK,data=data)
    for(i in 1:length(nomi)) {
      if(quiet==F) cat('\r',"Step ",ind,". Evaluated ",i,"/",length(nomi)," terms",sep="")
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
  if(quiet==F) cat("End with ",length(extrVar(formOK,data=data))," terms",sep="","\n")
  modOK
  }


###  NEW GRAPHICS  ###

# metric versus parameter plot
trainPlot <- function(caret_fit, par=NULL, metric=NULL, ylab=NULL, xlab=NULL, vcol="red", ...) {
  if(is.null(metric)) metric <- caret_fit$metric
  if(is.null(par)) par <- colnames(caret_fit$bestTune)[1]
  if(is.null(ylab)) ylab <- metric
  if(is.null(xlab)) xlab <- par
  plot(caret_fit$results[,par], caret_fit$results[,metric], ylab=ylab, xlab=xlab, type="l", ...)
  abline(v=caret_fit$bestTune, col=vcol)
  }

# variable importance
importanceCalc <- function(caret_fit, ordered=FALSE) {
  if(!is.logical(ordered)) ordered <- T else ordered <- ordered[1]
  imp0 <- tryCatch(caret::varImp(caret_fit, scale=FALSE)$importance, error=function(e){NULL})
  if(sum(class(caret_fit$finalModel)%in%c("lm","glm"))>0) {
    imp2 <- drop1(caret_fit$finalModel)
    dev <- imp2$Deviance
    names(dev) <- rownames(imp2)
    imp <- dev[setdiff(names(dev),"<none>")]
    if(ordered) impS <- sort(imp,decreasing=T) else impS <- imp
    attr(impS,"metric") <- "proportion of deviance"
    impS/sum(impS)
    } else {
    if(!is.null(imp0)) {
      imp <- imp0[,1]
      names(imp) <- rownames(imp0)
      if(ordered) impS <- sort(imp,decreasing=T) else impS <- imp
      impS/sum(impS)
      }
    }
  }

# plot of variable importance
importancePlot <- function(caret_fit, ordered=TRUE, ylab="", add.grid=TRUE, line.lty=1, cex.points=0.8, cex.names=0.8, dist.names=0.5, ...) {
  imp <- importanceCalc(caret_fit, ordered=ordered)
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
densPlot <- function(caret_fit, main="", lty=c(1,1), lwd=c(1,1), col=c("blue","red"), cut.lty=2, cut.col=1, ...) {
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