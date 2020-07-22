## ----ПАКЕТЫ-(графики/статистика)-----------------------------------------
library(knitr)
knitr::opts_chunk$set(dpi = 100, echo= TRUE, warning=FALSE, message=FALSE, fig.align = 'center', 
                      fig.show=TRUE, fig.keep = 'all', out.width = '50%') 
library(mixOmics)

## ----ЧТЕНИЕ-ОБУЧАЮЩЕЙ-ВЫБОРКИ--------------------------------------------
library(readr)
PeopleTrain <- read_csv("StudentTrain_4sem.csv")
#View(df)

#список с компонентами class и data
# 29 - 2 семестр
# 57 - 4 семестр
PeopTrain <- list(class = PeopleTrain$Class, data = PeopleTrain[,c(2:57)])
rm(PeopleTrain)

X = PeopTrain$data
Y = PeopTrain$class 
#dim(X) - размерность таблицы

## ----МЕТОД-ГЛАВНЫХ-КОМПОНЕНТ---------------------------------------------
pca.PeopTrain = pca(X, ncomp = 6, center = FALSE, scale = FALSE)
#Вклад компонент (достаточно 4 шт.) - PC1 и PC2 дают наибольший вклад
plot(pca.PeopTrain)

#Берем 1-2 компоненту
plotIndiv(pca.PeopTrain, comp = 1:2,
          group = Y, ind.names = FALSE, 
          ellipse = FALSE, legend = TRUE, title = 'PCA on PeopleData PC1 PC2')
show(pca.PeopTrain$x)
#Следующие копмоненты
#plotIndiv(pca.PeopTrain, comp = 5:6,
#          group = Y, ind.names = FALSE, 
#          ellipse = FALSE, legend = TRUE, title = 'PCA on PeopleData PC5 PC6')
rm(pca.PeopTrain)

## -----МЕТОД-PLS-ДИСКРИМИНАЦИИ------------------------------------------
PeopTrain.plsda <- plsda(X, Y, ncomp = 4)
plotIndiv(PeopTrain.plsda , comp = 1:2,
          group = Y, ind.names = FALSE, 
          ellipse = TRUE, legend = TRUE, title = 'PLSDA on PeopleData')

plotIndiv(PeopTrain.plsda, comp = 1:2,
          group = Y, ind.names = FALSE, title = "Maximum distance", legend = TRUE, 
          background = background.predict(PeopTrain.plsda, comp.predicted=2, dist = "max.dist"))

## -----КЛАССИФИКАЦИОННЫЕ-ХАРАКТЕРИСТИКИ-МОДЕЛИ-PLS----------------------
#folds - кратность проверочных значений
#auc - считать площадь под кривой
#nrepeat - количество тестовых прогонов для определения ошибки
perf.plsda.PeopTrain <- perf(PeopTrain.plsda, folds = 5, 
                             auc = TRUE, nrepeat = 10) 
plot(perf.plsda.PeopTrain, col = color.mixo(5:7), sd = TRUE, legend.position = "horizontal")
show(perf.plsda.PeopTrain$auc)

#roc.comp - компонент, который будет отображаться
auc.plsda = auroc(PeopTrain.plsda, roc.comp = 1)

rm(perf.plsda.PeopTrain)
rm(auc.plsda)

## -----ЧТЕНИЕ-ВЫБОРКИ-ДЛЯ-ТЕСТИРОВАНИЯ----------------------------------
library(readr)
PeopleTest <- read_csv("StudentTrain_2sem.csv")


#список с компонентами data и class
PeopTest <- list(data = PeopleTest[,c(2:27)], class = PeopleTest$Class)
#rm(PeopleTest)

## -----ПРЕДСКАЗАНИЕ-НА-ОСНОВЕ-PLSDA-------------------------------------
PeopTest.predict <- predict(PeopTrain.plsda, PeopTest$data, dist = "max.dist")
Prediction <- PeopTest.predict$class$max.dist[, 2]

cbind(PeopleTest$Class, Prediction)

err <- 0
for (i in 1:length(PeopleTest$Class)) {
  if(PeopleTest$Class[i] != Prediction[i]){
    err <- err + 1;
  }
}

show(err)

PeopTest.plsda <- plsda(PeopTest$data, Prediction, ncomp = 4)

plotIndiv(PeopTest.plsda , comp = 1:2,
          group = Prediction, ind.names = FALSE, 
          ellipse = TRUE, legend = TRUE, title = 'PLSDA on PeopleTest PC1 PC2')

#plotIndiv(PeopTest.plsda , comp = 3:4,
#          group = Prediction, ind.names = FALSE, 
#          ellipse = TRUE, legend = TRUE, title = 'PLSDA on PeopleTest PC3 PC4')

plotIndiv(PeopTest.plsda, comp = 1:2,
          group = Prediction, ind.names = TRUE, title = "Max distance",
          legend = TRUE, background = 
            background.predict(PeopTest.plsda, comp.predicted=2, dist = "max.dist"))

plotIndiv(PeopTest.plsda, comp = 1:2,
          group = Prediction, ind.names = TRUE, title = "Mahalanobis distance",
          legend = TRUE,  background = 
            background.predict(PeopTest.plsda, comp.predicted=2, dist = "mahalanobis.dist"))

plotIndiv(PeopTest.plsda, comp = 1:2,
          group = Prediction, ind.names = TRUE, title = "Centroids distance",
          legend = TRUE,  background = 
            background.predict(PeopTest.plsda, comp.predicted=2, dist = "centroids.dist"))

