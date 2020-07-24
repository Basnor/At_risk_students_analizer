library(readxl)
df <- read_excel("data/Student.xlsx")

# Переименуем 'Учебный год' в 'Год'
for (j in 1:nrow(df)) {
  df[["Год"]][j] <- strtrim(df[["Учебный год"]][j], 4)
}
df <- df[-c(5)]
rm(j)

## ----УДАЛЯЕМ-ВЫБРОСЫ---------------------------------------------------
# Сортировка по Студент, Год, Семестр
df <- df[order(df$Студент, df$Год, df$Семестр), ]
# Список ID студентов с выбросами (перевод на другой курс)
iddel <- list()
for (i in 2:(nrow(df)-1)) {
  if (df$Студент[i] == df$Студент[i-1] & df$Семестр[i] < df$Семестр[i-1]){
    iddel <- append(iddel, df$Студент[i])
  }
}
iddel <- unique(iddel)
rm(i)
# Удаление выбросов
df <- df[!df$Студент %in% iddel, ]
rm(iddel)

## ----ДОБАВЛЯЕМ-СТАТУС-НЕ-ОТЧИСЛЕН-НА-N-СЕМЕСТР---------------------------
# Добавляем статус выпускник всем, кто доучился до N семестра
N <- 5
for (i in 1:nrow(df)){
  if (df$Семестр[i] == N){
    df$Статус[i] <- "Не отчисленный"
  }
}
rm(i)
# Сортировка по Студент и Семестр
df <- df[order(df$Студент, df$Семестр),]

# Ищем максимальное количество записей об 1 студенте
countStudent <- aggregate(data.frame(count = df$Студент), list(value = df$Студент), length)
maxNum <- sort(countStudent$count, decreasing = TRUE)[1]

# Заменяем статус на 'Выпускник' для всех id выпускника
for (i in 1:nrow(df)){
  if (df$Статус[i] == "Не отчисленный" & df$Семестр[i] == N) {
    id <- df$Студент[i]
    
    for (j in (i-min(i-1, maxNum)):i){
      if (df$Студент[j] == id) {
        df$Статус[j] <- "Не отчисленный"
      }
    }
  }
}
rm(i)
rm(countStudent)
rm(maxNum)

## ----УДАЛЯЕМ-ДУБЛИКАТЫ-----------------------------------------------
#Удалить дубликаты
dfaggr <- aggregate(df, by=list(df$Студент, df$Семестр, df$Дисциплина), function(x) max(x))
df <- dfaggr[,4:13]
rm(dfaggr)


## ----УДАЛЯЕМ-ЛИШНИЕ-СЕМЕСТРЫ-----------------------------------------
# Отобрать строки с 1 по N-1 семестр
N <- N - 1
selectedStudents <- data.frame()
for (i in 1:nrow(df)){
  if (df$Семестр[i] <= N) {
    selectedStudents <- rbind(selectedStudents, df[i,])
  }
}

## ----ЗАПИСЬ-ОТФИЛЬТРОВАННЫХ-И-КЛАССИФИЦИРОВАННЫХ-СТУДЕНТОВ-----------
# При проблемах с совместимостью кодировок выполнять так
install.packages("writexl")
library(writexl)
write_xlsx(df,"data/Clean_and_Classified_Students_4.xlsx")

## ----ИНФОРМАЦИЯ-ДЛЯ-ТАБЛИЦЫ-------------------------------------------
uniqueDis <- unique(selectedStudents$Студент)
print(length(uniqueDis))
uniqueDis <- unique(selectedStudents$Группа)
print(length(uniqueDis))
uniqueDis <- unique(selectedStudents$Дисциплина)
print(length(uniqueDis))

# Ищем максимальное и инимальное количество записей об 1 студенте
countStudent <- aggregate(data.frame(count = selectedStudents$Студент), list(value = selectedStudents$Студент), length)
meanNum <- mean(countStudent$count)
maxNum <- sort(countStudent$count, decreasing = TRUE)[1]
minNum <- sort(countStudent$count, decreasing = FALSE)[1]

print(min(selectedStudents$Год))

Отчисленный <- nrow(selectedStudents[selectedStudents$Статус == 'Отчисленный',])
print(Отчисленный)

Выпускник <- nrow(selectedStudents[selectedStudents$Статус == 'Не отчисленный',])
print(Выпускник)
