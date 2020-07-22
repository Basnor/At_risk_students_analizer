library(readxl)
df <- read_excel("data/Classified_Students.xlsx")

# Отобрать строки с 1 по 4 семестр
selectedStudents <- data.frame()
for (i in 1:nrow(df)){
  if (df$Семестр[i] <= 4) {
    selectedStudents <- rbind(selectedStudents, df[i,])
  }
}

#Удалить дубликаты
dfaggr <- aggregate(selectedStudents, by=list(selectedStudents$Студент, selectedStudents$Семестр, selectedStudents$Дисциплина), function(x) max(x))
dffinal <- dfaggr[,4:13]

# При проблемах с совместимостью кодировок выполнять так
library(writexl)
write_xlsx(dffinal,"data/Students_without_dup.xlsx")
