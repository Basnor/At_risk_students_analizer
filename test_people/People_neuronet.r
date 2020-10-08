
## -----ЧТЕНИЕ-ВЫБОРКИ-ДЛЯ-ОБУЧЕНИЯ--------------------------------------
PeopleTrain <-read.csv(file = "PeopleTrain.csv", header = TRUE, sep = ";", dec = ",")

# нормализации данных
PeopTrain <- list(class = PeopleTrain$Class, data = PeopleTrain[,c(2:13)])
rm(PeopleTrain)
Y_train = PeopTrain$class
X_train = PeopTrain$data
x_scale_train = scale(X_train, center = TRUE, scale = TRUE)
rm(X_train)

#normalize <- function(x){
#  return((x - min(x)) / (max(x) - min(x)))
#}

#x_norm <- as.data.frame(lapply(X, normalize))
#summary(x_norm$Height)


## -----ЧТЕНИЕ-ВЫБОРКИ-ДЛЯ-ТЕСТИРОВАНИЯ----------------------------------

PeopleTest <-read.csv(file = "PeopleTest.csv", header = TRUE, sep = ",", dec = ",")
PeopTest <- list(class = PeopleTest$Class, data = PeopleTest[,c(1:12)])
rm(PeopleTest)
Y_test = PeopTest$class
X_test = PeopTest$data
x_scale_test = scale(X_test, center = TRUE, scale = TRUE)
rm(X_test)


## -----ОБУЧЕНИЕ-МОДЕЛИ-НА-ДАННЫХ----------------------------------------
#install.packages("neuralnet")
require(neuralnet)

softplus <- function(x) { log(1 + exp(x)) }
people_model <- neuralnet(Y_train ~ ., data = data.frame(x_scale_train,Y_train), 
                          hidden = c(12,8), act.fct = softplus)
#plot(people_model)


#c(30,40,12)
## -----ОЦЕНКА-ЭФФЕКТИВНОСТИ-МОДЕЛИ--------------------------------------

model_results <- compute(people_model, x_scale_test)
predicted_class <- model_results$net.result
print(predicted_class)


## -----ОШИБКИ-----------------------------------------------------------

predicted_class <- data.frame("class" = ifelse(max.col(predicted_class[ ,1:4]) == 1, "MN",
                                        ifelse(max.col(predicted_class[ ,1:4]) == 2, "MS", 
                                        ifelse(max.col(predicted_class[ ,1:4]) == 3, "FN", "FS"))))

# confusion matrix function
#install.packages("caret", dependencies=TRUE)
#install.packages("ggplot2")
library(caret)
factor_predicted_class <- factor(predicted_class$class, levels =  c("FN", "FS", "MN", "MS"))

cm = confusionMatrix(Y_test, factor_predicted_class)
print(cm)



