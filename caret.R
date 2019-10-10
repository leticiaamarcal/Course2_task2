# #instalar pacotes
# install.packages("caret", dependencies = c("Depends", "Suggests"))
# install.packages("caret", dependencies = c("Depends", "Suggests","Imports"))
# install.packages("generics")
# install.packages("gower")
# install.packages("ggplot2")
# install.packages("labeling")
# install.packages("klaR")
# install.packages("MASS")

#chamar livrarias
library(caret)
library(mlbench)
library(ggplot2)

data(Sonar)

#definir seeds
set.seed(107)

#partir o dado para poder treinar
inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)

#ver estrutura
str(inTrain)

#criar train e test
training <- Sonar[ inTrain,]
testing  <- Sonar[-inTrain,]

#checar a tamanho de cada amostra
nrow(training)
nrow(testing)

#criar o modelo. ele pode ser criado apenas com as 3 primeiras linhas. O resto é ajustar
#os parâmetros para otimizar o modelo
#linha 1 é o target/label e depois os atributos que vai usar no modelo (. significa todos)
#método é o algoritimo que vai usar
plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                preProc = c("center", "scale"),
                tuneLength = 15)                  

#fazer cross-validation fora da fórmula geral e adicionar nela depois
ctrl <- trainControl(method = "repeatedcv", repeats = 3)

#adicionando parâmetros
plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                preProc = c("center", "scale"),
                tuneLength = 15,
                trControl = ctrl)

#adicionar parâmetros a cross-validation
ctrl <- trainControl(method = "repeatedcv", repeats = 3, classProbs = TRUE, 
              summaryFunction = twoClassSummary)

#set a seed
set.seed(123)

#adicionar uma métrica/ ROC was used to select the optimal model using the largest value
plsFit <- train(Class ~ ., data = training, method = "pls", preProc = c("center", "scale"),
  tuneLength = 15, trControl = ctrl, metric = "ROC")

#checar/ plsFit é meu modelo
plsFit

#ggplot
ggplot(plsFit)

#predict os valores na parte testing to dataset
plsClasses <- predict(plsFit, newdata = testing)

#checar estrutura
str(plsClasses)

#To predict new samples, predict.train can be used. 
#For classification models, the default behavior is to calculate the predicted class. 
#The option type = "prob" can be used to compute class probabilities from the model
plsProbs <- predict(plsFit, newdata = testing, type = "prob")

#
head(plsProbs)

#confusion matrix
confusionMatrix(data = plsClasses, testing$Class)

#To illustrate, a custom grid is used
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)

#escolher semente
set.seed(123)

#novo modelo com um novo método
rdaFit <- train(Class ~ ., data = training, method = "rda", 
                tuneGrid = rdaGrid, trControl = ctrl, metric = "ROC")

#ver informações do modelo
rdaFit

#predict no testing data o novo modelo
testing$Class_Pred <- predict(rdaFit, newdata = testing)
#poderia colocar rdaClasses <- predict(rdaFit, newdata = testing), mas aí não teria comparação
#quando coloca o dataset $ e o nome de uma coluna que não existe, você cria uma nova
#ps: a nova coluna tem que ter o mesmo tamanho do dataset

#ver as colunas 61 e 62 (a class e a coluna que criou)
testing[,61:62]

#ver métricas
confusionMatrix(rdaClasses, testing$Class)
#          Reference
#Prediction  M  R
#         M 25  5
#         R  2 19
#isso significa 7 erro (5 + 2) e 44 (25 + 19) acertos. Isso da parte de testing

#The resamples function can be used to collect, summarize and contrast the resampling results. 
#Since the random number seeds were initialized to the same value prior to calling `train}, 
#the same folds were used for each model
resamps <- resamples(list(pls = plsFit, rda = rdaFit))

#resumo do objeto
summary(resamps)

#chart Bland-Altman
xyplot(resamps, what = "BlandAltman")

#achar a diferença dos dois modelos
diffs <- diff(resamps)

#resumo
summary(diffs)

#info1: caret uses two grid methods for tuning: Automatic Grid (automated tuning) and 
#Manual Grid (you specify the parameter values). TuneLength can also be added to training 
#with Automatic Grids for models with numeric hyperparameter values (K value in KNN, for example). 
#This method uses caret's 'best guess' for the numeric parameter values and will limit the model 
#runs according to the number of tuneLengths, but does not work on non-numeric values and will 
#increase training time(s), which is not necessarily a bad thing.
#info2: This is a good way of getting start with automated tuning if you are using models 
#that support such. Otherwise, caret might spend a great deal of time building many models 
#until the process has been optimized.
#info3: caret can also use a random search for tuning hyperparameters. 
#tuneLength isn't used when using Random Search


#expand.grid(..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)
