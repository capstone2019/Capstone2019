library(agricolae)
library(coin)

med <- read.csv('data/reshaped_med.csv', stringsAsFactors = F)
enroll <- read.csv('data/enroll.csv', stringsAsFactors = F)

colnames(enroll)[colnames(enroll)=="Member.ID.Encrypted"] <- "member_id"
colnames(enroll)[colnames(enroll)=="Incurred.Year"] <- "year"
colnames(enroll)[colnames(enroll)=="Total.Medical.Member.Months"] <- "month"
colnames(enroll)[colnames(enroll)=="Age"] <- "age"

data <- merge(x = med, y = enroll, by = c('member_id', 'year', 'age'), all.x = TRUE)

get_kruskal <- function(binary){
  kruskal.test(binary ~ month, data = data) 
}

detail <- function(binary){
  kruskal(data$month, binary, console = TRUE)
}

asthma <- get_kruskal(data$binary_asthma)
asthma_detail <- detail(data$binary_asthma)


