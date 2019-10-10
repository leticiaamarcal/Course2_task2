#abrir livraria
library("readr")
library("caret")
library("ggplot2")
library("lattice")
library("C50")

#upload the data
survey <- read.csv("C:/Users/letic/Desktop/CompleteResponses.csv")

#entender o dado
summary(survey)
is.na(survey)
str(survey)
is(survey)

#saber qual tipo de variável é seu atributo
is(survey$elevel)
is(survey$zipcode)
is(survey$salary)
is(survey$age)
is(survey$car)
is(survey$credit)
is(survey$brand)

#entender atributos
attributes(survey$elevel)
attributes(survey$salary)
attributes(survey$zipcode)

#transformar brand em factor
survey$brand<- as.factor(survey$brand)

#ver se funcionou
is(survey$brand)

#transformar zipcode em factor
survey$zipcode<- as.factor(survey$zipcode)

#ver se funcionou
is(survey$zipcode)

#transformar cars em factor
survey$car<- as.factor(survey$car)

#ver se funcionou
is(survey$car)

#transformar elevel em factor
survey$elevel<- as.factor(survey$elevel)

#ver se funcionou
is(survey$elevel)

#agora vou train the model para ter uma visão melhor e depois eu recomeço o processo
#para poder otimizar

#escolher a seed
set.seed(123)

#partir o dado para poder treinar
inTrain <- createDataPartition(y = survey$brand, p = .75, list = FALSE)

#criar o train e o test
training <- survey[ inTrain,]
testing  <- survey[-inTrain,]

#checar a tamanho de cada amostra
nrow(training)
nrow(testing)

#criar cross validation
crossV <- trainControl(method = "repeatedcv", repeats = 10)

#train the model
C5treeModel <- train(brand ~ .,
                     data = training,
                     method = "C5.0Tree",
                     preProc = c("center", "scale"),
                     tuneLength = 2,
                     trControl = crossV)

#checar meu modelo (métrica)
C5treeModel

#métrica desse modelo
#Accuracy : 0.8469967
#Kappa : 0.6832923

#ver como o modelo priorizou cada atributo
varImp(C5treeModel)
varImp(C5treeModel, sacale=FALSE)
#mesmo resultado

#colocar o varImp dentro de um objeto pra poder plotar
plotVarimp <- varImp(C5treeModel)

#plotar varImp/ colocar paramêtro para visualizar só os que importam 
plot(plotVarimp, top=5)

#predictions com o test
Predi_Test_C5treeModel <- predict(C5treeModel, testing)

#ver a accuracy no test
confusionMatrix(testing$brand, Predi_Test_C5treeModel)

#métricas do test
#Accuracy : 0.9212 
#Kappa : 0.833  

#set seed
set.seed(123)

#run the model com atribute salary
C5TM_salary <- train(brand ~ salary,
                     data = training,
                     method = "C5.0Tree",
                     preProc = c("center", "scale"),
                     tuneLength = 2,
                     trControl = crossV)

#checar métrica
C5TM_salary

#predictions com o test
Predi_Test_C5TM_salary <- predict(C5TM_salary, testing)

#ver a accuracy no test
confusionMatrix(testing$brand, Predi_Test_C5TM_salary)

#set seed
set.seed(123)

#run the model com atribute salary, age
C5TM_salary_age <- train(brand ~ salary + age,
                         data = training,
                         method = "C5.0Tree",
                         preProc = c("center", "scale"),
                         tuneLength = 2,
                         trControl = crossV)

#checar métrica
C5TM_salary_age

#métricas
#Accuracy : 0.8975057
#Kappa : 0.7841541

#predictions com o test
Predi_Test_C5TM_salary_age <- predict(C5TM_salary_age, testing)

#ver a accuracy no test
confusionMatrix(testing$brand, Predi_Test_C5TM_salary_age)

#métricas
#Accuracy : 0.9244                                
#Kappa : 0.8387 

#set seed
set.seed(123)

#run the model com atribute salary, age, zipcode
C5TM_salary_age_zip <- train(brand ~ salary + age + zipcode,
                             data = training,
                             method = "C5.0Tree",
                             preProc = c("center", "scale"),
                             tuneLength = 2,
                             trControl = crossV)

#checar métrica
C5TM_salary_age_zip

#métricas
#Accuracy : 0.8699009
#Kappa : 0.726712

#predictions com o test
Predi_Test_C5TM_salary_age_zip <- predict(C5TM_salary_age_zip, testing)

#ver a accuracy no test
confusionMatrix(testing$brand, Predi_Test_C5TM_salary_age_zip)

#métricas
#Accuracy : 0.9228         
#Kappa : 0.8363

#set seed
set.seed(123)

#run the model com atribute salary, age, zipcode, credit
C5TM_salary_age_zip_cred <- train(brand ~ salary + age + zipcode + credit,
                                  data = training,
                                  method = "C5.0Tree",
                                  preProc = c("center", "scale"),
                                  tuneLength = 2,
                                  trControl = crossV)

#checar métrica
C5TM_salary_age_zip_cred

#métricas
#Accuracy: 0.8697393   
#Kappa: 0.7263673

#predictions com o test
Predi_Test_C5TM_salary_age_zip_cred <- predict(C5TM_salary_age_zip_cred, testing)

#ver a accuracy no test
confusionMatrix(testing$brand, Predi_Test_C5TM_salary_age_zip_cred)

#métricas
#Accuracy : 0.9228         
#Kappa : 0.8363  

#set seed
set.seed(123)

#run the model com atribute salary, age, zipcode, credit, car
C5TM_sa_age_zip_cred_car <- train(brand ~ salary + age + zipcode + credit + car,
                                  data = training,
                                  method = "C5.0Tree",
                                  preProc = c("center", "scale"),
                                  tuneLength = 2,
                                  trControl = crossV)

#checar métrica
C5TM_sa_age_zip_cred_car

#métricas
#Accuracy: 0.8490426
#Kappa: 0.6920992    

#predictions com o test
Predi_Test_C5TM_sa_age_zip_cred_car <- predict(C5TM_sa_age_zip_cred_car, testing)

#ver a accuracy no test
confusionMatrix(testing$brand, Predi_Test_C5TM_sa_age_zip_cred_car)

#métricas
#Accuracy : 0.9208          
#Kappa : 0.832 

#set seed
set.seed(123)

#run the model com atribute salary, age, zipcode, credit, car, elevel
C5TM_sa_age_zip_cred_car_e <- train(brand ~ salary + age + zipcode + credit + car + elevel,
                                    data = training,
                                    method = "C5.0Tree",
                                    preProc = c("center", "scale"),
                                    tuneLength = 2,
                                    trControl = crossV)

#checar métrica
C5TM_sa_age_zip_cred_car_e

#métricas
#Accuracy: 0.847951  
#Kappa: 0.6891494 

#predictions com o test
Predi_Test_C5TM_sa_age_zip_cred_car_e <- predict(C5TM_sa_age_zip_cred_car_e, testing)

#ver a accuracy no test
confusionMatrix(testing$brand, Predi_Test_C5TM_sa_age_zip_cred_car_e)

#métricas
#Accuracy : 0.9212
#Kappa : 0.833   


