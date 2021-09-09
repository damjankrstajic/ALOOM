check.ALOOM.method <- function(a.method)
{
  if(is.list(a.method)) 
  {
    methodNames  <- c("library", "parameters") 
    libraryNames <- c("rf","glmnet") 
    nameCheck <- methodNames %in% names(a.method) 
    if(!all(nameCheck)) stop(paste("some required components are missing:",
                                   paste(methodNames[!nameCheck], collapse = ", ")),
                             call. = FALSE)
    libraryCheck <- a.method$library %in% libraryNames
    if(!all(libraryCheck)) stop(paste("method$library ", a.method$library, " is not in:",
                                paste(libraryNames, collapse = ", ")),
                             call. = FALSE)

    if(a.method$library=="rf" && (! names(a.method$parameters) %in% c("ntree"))) 
    {
      stop("method$library=rf should have a list method$parameters with ntree name",
      call. = FALSE)
    }

    if(a.method$library=="glmnet" && (! names(a.method$parameters) %in% c("alpha","lambda"))) 
    {
      stop("method$library=glmnet should have a list method$parameters with alpha and lambda",
      call. = FALSE)
    }

  } else stop("method is expected to be a list", call. = FALSE) 

}

original <- function(train.x,train.y,test.x,
                     method=list(library="rf",parameters=list(ntree=1000)))
{
  check.ALOOM.method(method)  

  if (method$library=="rf")
  {
    suppressPackageStartupMessages(library(randomForest))
    if ("seed.number" %in% names(method)) set.seed(method$seed.number)
    fit.RF.L <- randomForest(train.x,train.y,ntree=method$parameters$ntree)

    predictedY            <- as.vector(predict(fit.RF.L,test.x,type="response"))
    predictedProbs        <- predict(fit.RF.L,test.x,type="prob")
    predictedProbabilityY <- predictedProbs[,2]

  } else if (method$library=="glmnet")
  {
    suppressPackageStartupMessages(library(glmnet))
    if ("seed.number" %in% names(method)) set.seed(method$seed.number)
    fit.glmnet.L <- glmnet(train.x, train.y, family="binomial",
                           lambda=method$parameters$lambda, alpha=method$parameters$alpha)

    predictedY            <- as.vector(predict(fit.glmnet.L,test.x,type="class"))
    predictedProbabilityY <- as.vector(predict(fit.glmnet.L,test.x,type="response"))

  } else
  {
    stop(paste("original() does not support",method$library), call.=FALSE)
  }

  list(predicted.y=predictedY, predicted.probs=predictedProbabilityY)
}

ALOOM <- function(train.x,train.y,test.x,
                  method=list(library="rf",parameters=list(ntree=1000)))
{
  check.ALOOM.method(method)  

  mnAllPredictions   <- matrix(nrow=nrow(test.x),ncol=nrow(train.x))
  mnAllProbabilities <- matrix(nrow=nrow(test.x),ncol=nrow(train.x))

  rownames(mnAllProbabilities) <- rownames(test.x)
  colnames(mnAllProbabilities) <- paste0("without_",rownames(train.x))

  for (i in 1:nrow(train.x))
  {
    x <- train.x[-i,]
    y <- train.y[-i]

    if (method$library=="rf")
    {
      suppressPackageStartupMessages(library(randomForest))
      if ("seed.number" %in% names(method)) set.seed(method$seed.number)
      fit.RF.L <- randomForest(x,y,ntree=method$parameters$ntree)

      predictedY            <- as.vector(predict(fit.RF.L,test.x,type="response"))
      predictedProbs        <- as.vector(predict(fit.RF.L,test.x,type="prob"))
      predictedProbabilityY <- predictedProbs[,2]

      mnAllPredictions[,i]   <- predictedY
      mnAllProbabilities[,i] <- predictedProbabilityY
      cat(i)

    } else if (method$library=="glmnet")
    {
      suppressPackageStartupMessages(library(glmnet))
      if ("seed.number" %in% names(method)) set.seed(method$seed.number)
      fit.glmnet.L <- glmnet(x, y, family="binomial",
                             lambda=method$parameters$lambda, alpha=method$parameters$alpha)

      predictedY            <- as.vector(predict(fit.glmnet.L,test.x,type="class"))
      predictedProbabilityY <- as.vector(predict(fit.glmnet.L,test.x,type="response"))

      mnAllPredictions[,i]   <- predictedY
      mnAllProbabilities[,i] <- predictedProbabilityY
      cat(i)

    } else
    {
      stop(paste("ALOOM() does not support",method$library), call.=FALSE)
    }
  }

  predictedNA.Y <- predictedY

  for (row.number in 1:nrow(mnAllPredictions))
  {
    if (length(unique(mnAllPredictions[row.number,]))!=1)
    {
      predictedNA.Y[row.number] <- "NA"
    }
  }

  aloomMean   <- apply(mnAllProbabilities,1,mean)
  aloomMedian <- apply(mnAllProbabilities,1,median)
  aloomMin    <- apply(mnAllProbabilities,1,min)
  aloomMax    <- apply(mnAllProbabilities,1,max)

  list(predicted.y=predictedNA.Y, all.predicted.probabilities=mnAllProbabilities,
       aloom.mean=aloomMean, aloom.median=aloomMedian, aloom.min=aloomMin, aloom.max=aloomMax)
}
