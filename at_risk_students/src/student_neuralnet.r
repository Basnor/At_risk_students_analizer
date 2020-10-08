
## -----ВЫБОРКА-ДЛЯ-ОБУЧЕНИЯ-И-ТЕСТИРОВАНИЯ------------------------------
library(readxl)
Students <- read_excel("data/StudentsTrain_4sem.xlsx")

# Рандомное разделение на тестовые и обучающие индексы
StudTrain_index <- sample(1:nrow(Students), 0.7 * nrow(Students))
StudTest_index <- setdiff(1:nrow(Students), StudTrain_index)

# Списки с компонентами class и data
StudTrain <- list(class = Students$Статус[StudTrain_index], 
                  data = Students[StudTrain_index,c(5:ncol(Students))])

StudTest <- list(class = Students$Статус[StudTest_index], 
                 data = Students[StudTest_index,c(5:ncol(Students))])

# Удаляем пустые колонки, образовавшиеся в обучающей выборке (в тестовой удаляем тоже)
nullrow <- which(apply(StudTrain$data, 2, var) == 0)
StudTrain$data <- StudTrain$data[-c(nullrow)]
StudTest$data <- StudTest$data[-c(nullrow)]

nullrow <- which(apply(StudTest$data, 2, var) == 0)
StudTrain$data <- StudTrain$data[-c(nullrow)]
StudTest$data <- StudTest$data[-c(nullrow)]
rm(nullrow)

X_train = StudTrain$data
X_train = data.matrix(X_train)
X_train = scale(X_train, center = TRUE, scale = TRUE)
Y_train = as.factor(StudTrain$class)


X_test = StudTest$data
X_test = data.matrix(X_test)
X_test = scale(X_test, center = TRUE, scale = TRUE)
Y_test = as.factor(StudTest$class)


## -----ОБУЧЕНИЕ-МОДЕЛИ-НА-ДАННЫХ----------------------------------------
#install.packages("neuralnet")
require(neuralnet)

softplus <- function(x) { log(1 + exp(x)) }
#summary(data.frame(X_train,Y_train))
students_model <- neuralnet(Y_train ~ ., data = data.frame(X_train,Y_train), 
                          hidden = c(80,25), act.fct = softplus, rep = 1, linear.output=TRUE, stepmax=1e7)
#plot(students_model)


## -----ОЦЕНКА-ЭФФЕКТИВНОСТИ-МОДЕЛИ--------------------------------------

model_results <- predict(students_model, X_test)
print(model_results)


## -----ОШИБКИ-----------------------------------------------------------

predicted_class <- data.frame("class" = ifelse(max.col(model_results[ ,1:2]) == 1, "Выбыл", "Продолжил"))

# confusion matrix function
#install.packages("caret", dependencies=TRUE)
#install.packages("ggplot2")
library(caret)
factor_class <- factor(predicted_class$class, levels =  c("Выбыл", "Продолжил"))

cm = confusionMatrix(Y_test, factor_class)
print(cm)




