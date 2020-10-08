## ----ПАКЕТЫ-(графики/статистика)-----------------------------------------
library(knitr)
knitr::opts_chunk$set(dpi = 100, echo= TRUE, warning=FALSE, message=FALSE, fig.align = 'center',
                      fig.show=TRUE, fig.keep = 'all', out.width = '50%')
library(mixOmics)

## ----ЧТЕНИЕ-ОБУЧАЮЩЕЙ-ВЫБОРКИ--------------------------------------------
library(readxl)
StudentTrain <- read_excel("data/StudentsTrain_4sem.xlsx")

# Рандомное разделение на тестовые и обучающие индексы
StudTrain_index <- sample(1:nrow(StudentTrain), 0.7 * nrow(StudentTrain))
StudTest_index <- setdiff(1:nrow(StudentTrain), StudTrain_index)

#список с компонентами class и data
StudTrain <- list(class = StudentTrain$Статус[StudTrain_index], 
                  data = StudentTrain[StudTrain_index,c(5:ncol(StudentTrain))])

#print(unique(StudTrain$data))

nullrow <- which(apply(StudTrain$data, 2, var) == 0)
StudTrain$data <- StudTrain$data[-c(nullrow)]

X = StudTrain$data
X = data.matrix(X)
Y = StudTrain$class
#dim(X) - размерность таблицы

## ----МЕТОД-ГЛАВНЫХ-КОМПОНЕНТ---------------------------------------------

pca.StudTrain = pca(X, ncomp = 10, center = TRUE, scale = TRUE)
#Вклад компонент (достаточно 4 шт.) - PC1 и PC2 дают наибольший вклад
plot(pca.StudTrain)

#Берем 1-2 компоненту
plotIndiv(pca.StudTrain, comp = 1:2,
          group = Y, ind.names = FALSE,
          ellipse = FALSE, legend = TRUE, title = 'PCA on StudentData')
show(pca.StudTrain$x)

print(pca.StudTrain)
#Следующие копмоненты
#plotIndiv(pca.StudTrain, comp = 5:6,
# group = Y, ind.names = FALSE,
# ellipse = FALSE, legend = TRUE, title = 'PCA on PeopleData PC5 PC6')
rm(pca.StudTrain)

## ----МЕТОД-PLS-ДИСКРИМИНАЦИИ------------------------------------------
StudTrain.plsda <- plsda(X, Y, ncomp = 10)
plotIndiv(StudTrain.plsda , comp = 1:2,
          group = Y, ind.names = FALSE,
          ellipse = TRUE, legend = TRUE, title = 'PLSDA on StudentData')

plotIndiv(StudTrain.plsda, comp = 1:2,
          group = Y, ind.names = FALSE, title = "Mahalanobis dist", legend = TRUE,
          background = background.predict(StudTrain.plsda, comp.predicted=2, 
                                          dist = "mahalanobis.dist"))

barplot(StudTrain.plsda$explained_variance$X, xlab="Components", ylab="Explained variance")

barplot(StudTrain.plsda$explained_variance$X, xlab="Components", ylab="Explained variance")
print(StudTrain.plsda$explained_variance$X)
print(sum(StudTrain.plsda$explained_variance$X))

## ----КЛАССИФИКАЦИОННЫЕ-ХАРАКТЕРИСТИКИ-МОДЕЛИ-PLS----------------------
#folds - кратность проверочных значений
#auc - считать площадь под кривой
#nrepeat - количество тестовых прогонов для определения ошибки
perf.plsda.StudTrain <- perf(StudTrain.plsda, folds = 5,
                             auc = TRUE, nrepeat = 10)
plot(perf.plsda.StudTrain, col = color.mixo(5:7), sd = TRUE, legend.position = "horizontal")
show(perf.plsda.StudTrain$auc)

#roc.comp - компонент, который будет отображаться
auc.plsda = auroc(StudTrain.plsda, roc.comp = 3)

rm(perf.plsda.StudTrain)
rm(auc.plsda)

## ----ЧТЕНИЕ-ВЫБОРКИ-ДЛЯ-ТЕСТИРОВАНИЯ----------------------------------
#список с компонентами data и class
StudTest <- list(class = StudentTrain$Статус[StudTest_index], 
                 data = StudentTrain[StudTest_index,c(5:ncol(StudentTrain))])
StudTest$data <- StudTest$data[-c(nullrow)]
StudTest$data <- data.matrix(StudTest$data)

## —---ПРЕДСКАЗАНИЕ-НА-ОСНОВЕ-PLSDA-----------------------------------—
StudTest.predict <- predict(StudTrain.plsda, StudTest$data, dist = "mahalanobis.dist")
Prediction <- StudTest.predict$class$mahalanobis.dist[, 2]

cbind(StudTest$class, Prediction)


#Ошибки 1 и 2 рода
correct_type1 <- 0
correct_type2 <- 0
error_type1 <- 0
error_type2 <- 0
for (i in 1:length(StudTest$class)) {
  if(StudTest$class[i] == Prediction[i] & Prediction[i] == "Не отчисленный"){
    correct_type1 <- correct_type1 + 1;
  }
  if(StudTest$class[i] == Prediction[i] & Prediction[i] == "Отчисленный"){
    correct_type2 <- correct_type2 + 1;
  }
    if(StudTest$class[i] != Prediction[i] & StudTest$class[i] == "Не отчисленный"){
  error_type1 <- error_type1 + 1;
  }
    if(StudTest$class[i] != Prediction[i] & StudTest$class[i] == "Отчисленный"){
  error_type2 <- error_type2 + 1;
  }
}

print("Не отчисленный")
show(correct_type1)
print("Отчисленный")
show(correct_type2)
print("Должен быть Не отчисленный, но Отчисленный")
show(error_type1)
print("Должен быть Отчисленный, но Не отчисленный")
show(error_type2)


#Отображение графиков для тестовой выборки
StudTest.plsda <- plsda(StudTest$data, Prediction, ncomp = 3)

plotIndiv(StudTest.plsda , comp = 1:2,
          group = Prediction, ind.names = FALSE,
          ellipse = TRUE, legend = TRUE, title = 'PLSDA on StudentTest PC1 PC2', 
          dist = "mahalanobis.dist")

#plotIndiv(StudTest.plsda , comp = 3:4,
# group = Prediction, ind.names = FALSE,
# ellipse = TRUE, legend = TRUE, title = 'PLSDA on StudentTest PC3 PC4')

plotIndiv(StudTest.plsda, comp = 1:2,
          group = Prediction, ind.names = TRUE, title = "Max distance",
          legend = TRUE, background = 
            background.predict(StudTest.plsda, comp.predicted=2, dist = "max.dist"))

plotIndiv(StudTest.plsda, comp = 1:2,
          group = Prediction, ind.names = TRUE, title = "Mahalanobis distance",
          legend = TRUE, background = 
            background.predict(StudTest.plsda,
                               comp.predicted=2, dist = "mahalanobis.dist"))

plotIndiv(StudTest.plsda, comp = 1:2,
          group = Prediction, ind.names = TRUE, title = "Centroids distance",
          legend = TRUE, background =
            background.predict(StudTest.plsda, comp.predicted=2, dist = "centroids.dist"))
