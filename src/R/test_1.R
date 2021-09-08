#!/usr/bin/Rscript

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

    if(names(a.method$library)=="rf" && (! names(a.method$parameters) %in% c("ntree"))) 
    {
      stop("method$library=rf should have a list method$parameters with ntree name",
      call. = FALSE)
    }

  } else stop("method is expected to be a list", call. = FALSE) 

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
      fit.RF.L <- randomForest(x,y,ntree=method$parameters$ntree)

      predictedY            <- as.vector(predict(fit.RF.L,test.x,type="response"))
      predictedProbs        <- predict(fit.RF.L,test.x,type="prob")
      predictedProbabilityY <- predictedProbs[,2]

      mnAllPredictions[,i]   <- predictedY
      mnAllProbabilities[,i] <- predictedProbabilityY
      cat(i)
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

suppressPackageStartupMessages(library(QSARdata))
suppressPackageStartupMessages(library(caret))

data(bbb2)

row.has.na <- apply(bbb2_Dragon, 1, function(x){any(is.na(x))})
bbb2_Dragon <- bbb2_Dragon[!row.has.na,]
bbb2_Outcome <- bbb2_Outcome[!row.has.na,]

set.seed(1)
lvFolds <- createFolds(bbb2_Outcome[,2],k=2)
mnLearningX   <- bbb2_Dragon[-lvFolds[[1]],-1]
mnValidationX <- bbb2_Dragon[lvFolds[[1]],-1]
pfLearningY   <- bbb2_Outcome[-lvFolds[[1]],2]
#pfValidationY <- bbb2_Outcome[lvFolds[[1]],2]

lvALOOM <- ALOOM(mnLearningX,pfLearningY,mnValidationX)

data1 <- data.frame(id=rownames(mnValidationX),
                    aloom.prediction=lvALOOM$predicted.y,
                    aloom.min=lvALOOM$aloom.min,
                    aloom.mean=lvALOOM$aloom.mean,
                    aloom.max=lvALOOM$aloom.max)

data2 <- data.frame(id=rownames(mnValidationX),lvALOOM$all.predicted.probabilities)

filename1 <- "~/predicted_na_rf_all.csv"
write.csv(data1,file=filename1,row.names=F,quote=F)
filename2 <- "~/all_predicted_probs.csv"
write.csv(data2,file=filename2,row.names=F,quote=F)
pcLine <- paste("\nPredictions are stored in the files ",filename1,filename2,"\n\n")
cat(pcLine)
