library(readxl)
df <- read_excel("Students_09_03_01.xlsx", 
                           sheet = "Лист4")
View(df)

#Удалить дубликаты
dfaggr <- aggregate(df, by=list(df$Студент, df$Семестр, df$Дисциплина), function(x) max(x)) 
dffinal <- dfaggr[,4:11]

write.csv(dffinal,"Students_without_dup.csv", row.names = TRUE)
