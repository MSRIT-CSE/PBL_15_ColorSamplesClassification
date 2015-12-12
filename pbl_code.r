#Importing dataset

skin_data_set <- read.delim("~/desktop/Skin_NonSkin.txt", header=FALSE)

#view (skin_data_set)
colnames(skin_data_set) <- c("B","G","R","Result")

#SPlit into train and test
library(caret)
set.seed(1278)
#rm(skinTest,skinTrain,trainIndex)
trainIndex <- createDataPartition(skin_data_set$Result, p = .75,
                                  list = FALSE,
                                  times = 1)
skinTrain <- skin_data_set[ trainIndex, ]
skinTestRaw <- skin_data_set[ -trainIndex, ]
skinTestMod <- skin_data_set[ -trainIndex, -4]
#rm(skinTest,skinTrain,trainIndex)


normalizedSkinTrain <- skinTrain

norm.fun = function(x){ 
  (x - min(x))/(max(x) - min(x)) 
}

normalizedSkinTrain = apply(normalizedSkinTrain, 2, norm.fun)

library(neuralnet)

res <- neuralnet(formula = Result ~ B + G + R, 
                 data = normalizedSkinTrain, 
                 hidden = 10, 
                 learningrate = 0.3, 
                 lifesign = "full", 
                 lifesign.step = 1,
                 err.fct = "sse",
                 act.fct = "logistic",
                 algorithm = "rprop+",
                 linear.output = FALSE)


normalizedSkinTest = apply(skinTestMod, 2, norm.fun)

finalres <- compute(x = res, covariate = normalizedSkinTest)

ultimateRes <- ifelse((finalres$net.result) < 0.5,1,2)

testRes <-  cbind(skinTestRaw$Result,ultimateRes)

confusionMatrix(data = as.factor(x = ultimateRes),
                reference = as.factor(x = skinTestRaw$Result))

apply(X = ultimateRes, MARGIN = 2,FUN = function(x) length(which(x == 2)))

