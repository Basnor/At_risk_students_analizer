library(readr)
df <- read_csv("Students_without_dup.csv")
df <- df[,2:9]
#View(df)

# Странный студент
for (i in 1:length(df$Студент)) {
  if (df$Студент[i] == '5A4C326845782B4E4E457769393147565438345349513D3D') {
    df <- df[-c(i), ]
  }
}

# Изменить оценки на число
for (i in 1:nrow(df)){
  print(df$Оценка[i])
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

uniqueDf <- unique(df[c("Семестр", "Дисциплина")])
uniqueDf[c("Уникальная дисциплина")] <- paste(uniqueDf$Семестр, uniqueDf$Дисциплина)
uniqueDf <- uniqueDf[order(uniqueDf$Семестр),]
print(uniqueDf)

dfT<-data.frame(matrix(vector(),ncol=nrow(uniqueDf) + 4))
colnames(dfT) <- c("Статус", "Студент", "Группа", "Год поступления", uniqueDf[["Уникальная дисциплина"]])
print(dfT)


uniqueStudents <- unique(df$Студент)

for (i in  1:length(uniqueStudents)) {
  dfT[nrow(dfT)+1,] <- 0
  for (j in  1:length(df$Студент)) {
    if (uniqueStudents[i] == df$Студент[j]){
      dfT$Статус[i]<- df$Статус2[j]
      dfT$Студент[i] <- df$Студент[j]
      dfT$Группа[i] <- df$Группа[j]
      dfT[["Год поступления"]][i] <- strtrim(df[["Учебный год"]][j], 4)

      for (k in 1:length(uniqueDf[["Уникальная дисциплина"]])) {
        if (uniqueDf[["Уникальная дисциплина"]][k] == paste(df$Семестр[j], df$Дисциплина[j])) {
          dfT[c(uniqueDf[["Уникальная дисциплина"]][k])][i,] <- df$Оценка[j]
        }
      }
      
    }
  }
}




#print(dfT[c("Год_поступления")][24,])
write.csv(dfT,"StudentTrain_4sem.csv", row.names = FALSE)

