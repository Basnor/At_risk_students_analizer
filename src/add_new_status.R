library(readxl)
df <- read_excel("Students.xlsx")
#View(df)

# Не запускать!!!! Сначала подумай и переделай нормально. (каким-то чудом сработало)
for (i in 1:length(df$Статус2)){
  if (df$Статус2[i] == "Выпускник") {
    
    id <- df$Студент[i]
    
    for (j in 1:length(df$Студент)){
      if (df$Студент[j] == id) {
        df$Статус2[j] <- "Выпускник"
      }
    }
  }
}

write.csv(df,"Students_09_03_01.csv", row.names = TRUE)


