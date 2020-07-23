library(readxl)
df <- read_excel("data/Classified_Students.xlsx")

# Учебный год в Год
for (j in 1:nrow(df)) {
  df[["Год"]][j] <- strtrim(df[["Учебный год"]][j], 4)
}
df <- df[-c(5)]

# Сортировка по Студент, Год, Семестр
dfsort <- df[order(df$Студент, df$Год, df$Семестр), ]

# Список ID студентов с выбросами (перевод на другой курс)
iddel <- list()
for (i in 2:(nrow(dfsort)-1)) {
  if (dfsort$Студент[i] == dfsort$Студент[i-1] & dfsort$Семестр[i] < dfsort$Семестр[i-1]){
    iddel <- append(iddel, dfsort$Студент[i])
  }
}
iddel <- unique(iddel)

# Удаление выбросов
dfnorm <- dfsort[!dfsort$Студент %in% iddel, ]

# Отобрать строки с 1 по 4 семестр
selectedStudents <- data.frame()
for (i in 1:nrow(dfnorm)){
  if (dfnorm$Семестр[i] <= 4) {
    selectedStudents <- rbind(selectedStudents, dfnorm[i,])
  }
}

#Удалить дубликаты
dfaggr <- aggregate(selectedStudents, by=list(selectedStudents$Студент, selectedStudents$Семестр, selectedStudents$Дисциплина), function(x) max(x))
dffinal <- dfaggr[,4:13]


# При проблемах с совместимостью кодировок выполнять так
library(writexl)
write_xlsx(dffinal,"data/Students_without_dup_4.xlsx")
