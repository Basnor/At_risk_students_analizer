library(readxl)
df <- read_excel("Students.xlsx")

# Добавляем статус выпускник всем, кто доучился до 8 семестра
for (i in 1:nrow(df)){
  if (df$Семестр[i] == 8){
    df$Статус[i] <- "Выпускник"
  }
}

# Сортировка по Студент и Семестр
df <- df[order(df$Студент, df$Семестр),]

# Ищем максимальное количество записей об 1 студенте
countStudent <- aggregate(data.frame(count = df$Студент), list(value = df$Студент), length)
maxNum <- sort(countStudent$count, decreasing = TRUE)[1]

# Заменяем статус на 'Выпускник' для всех id выпускника
for (i in 1:nrow(df)){
  print(i)
  if (df$Статус[i] == "Выпускник" & df$Семестр[i] == 8) {
    id <- df$Студент[i]
    
    for (j in (i-min(i-1, maxNum)):i){
      if (df$Студент[j] == id) {
        df$Статус[j] <- "Выпускник"
      }
    }
  }
}

# При проблемах с совместимостью кодировок выполнять так
install.packages("writexl")
library(writexl)
write_xlsx(df,"Students_ISHITR_Classified.xlsx")

# Если только на Linux выполняем
#write.csv(df,"Students_09_03_01.csv", row.names = TRUE)


