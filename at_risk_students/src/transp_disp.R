library(readxl)
df <- read_excel("data/Clean_and_Classified_Students_4.xlsx")

# Изменить оценки на число
for (i in 1:nrow(df)){
  switch(df$Оценка[i], 
         отлично={
           df$Оценка[i] <- 5
         },
         зачтено={
           df$Оценка[i] <- 5  
         },
         хорошо={
           df$Оценка[i] <- 4
         },
         удовлетворительно={
           df$Оценка[i] <- 3 
         },
         неудовлетворительно={
           df$Оценка[i] <- 2
         },
         незачет={
           df$Оценка[i] <- 2
         }
  )
}

# Объединяем столбцы семестр и дисциплина
uniqueDf <- unique(df[c("Семестр", "Дисциплина")])
uniqueDf[c("Уникальная дисциплина")] <- paste(uniqueDf$Семестр, uniqueDf$Дисциплина)
uniqueDf <- uniqueDf[order(uniqueDf$Семестр),]
#print(uniqueDf)

# Создаем транспонентную таблицу с дисциплинами в качестве столбцов
dfT<-data.frame(matrix(vector(),ncol=nrow(uniqueDf) + 4))
colnames(dfT) <- c("Статус", "Студент", "Группа", "Год", uniqueDf[["Уникальная дисциплина"]])
print(dfT)

# Заполняем строки транспонентной таблицы
uniqueStudents <- unique(df$Студент)
for (i in 1:length(uniqueStudents)) {
  print(i)
  dfT[nrow(dfT)+1,] <- 0
  for (j in 1:length(df$Студент)) {
    if (uniqueStudents[i] == df$Студент[j]){
      dfT$Статус[i]<- df$Статус[j]
      dfT$Студент[i] <- df$Студент[j]
      dfT$Группа[i] <- df$Группа[j]
      dfT$Год[i] <- df$Год[j]
      
      for (k in 1:length(uniqueDf[["Уникальная дисциплина"]])) {
        if (uniqueDf[["Уникальная дисциплина"]][k] == paste(df$Семестр[j], df$Дисциплина[j])) {
          dfT[c(uniqueDf[["Уникальная дисциплина"]][k])][i,] <- df$Оценка[j]
        }
      }
    }
  }
}

library(writexl)
write_xlsx(dfT,"data/StudentsTrain_4sem.xlsx")
