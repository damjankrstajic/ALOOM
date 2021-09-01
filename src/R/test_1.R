#!/usr/bin/Rscript

ALOOM <- function(train.x,train.y,test.x)
{

  mnAllPredictions   <- matrix(nrow=nrow(test.x),ncol=nrow(train.x))
  mnAllProbabilities <- matrix(nrow=nrow(test.x),ncol=nrow(train.x))

  for (i in 1:nrow(train.x))
  {
    x <- train.x[-i,]
    y <- train.y[-i]
    fit.RF.L <- randomForest(x,y,ntree=1000)

    predictedY            <- as.vector(predict(fit.RF.L,test.x,type="response"))
    predictedProbs        <- predict(fit.RF.L,test.x,type="prob")
    predictedProbabilityY <- predictedProbs[,2]

    mnAllPredictions[,i]   <- predictedY
    mnAllProbabilities[,i] <- predictedProbabilityY
    cat(i)
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

  list(predicted.y=predictedNA.Y, aloom.mean=aloomMean, aloom.median=aloomMedian, aloom.min=aloomMin, aloom.max=aloomMax)
}

suppressPackageStartupMessages(library(QSARdata))
suppressPackageStartupMessages(library(randomForest))
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

data        <- data.frame(id=rownames(mnValidationX),
                          aloom.prediction=lvALOOM$predicted.y,
                          aloom.min=lvALOOM$aloom.min,
                          aloom.mean=lvALOOM$aloom.mean,
                          aloom.max=lvALOOM$aloom.max)
filename1   <- "predicted_na_rf_all.csv"
write.csv(data,file=filename1,row.names=F,quote=F)
pcLine <- paste("\nPredictions are stored in the file ",filename1,"\n\n")
cat(pcLine)
