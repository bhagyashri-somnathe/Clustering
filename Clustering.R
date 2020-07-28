## Crime Data

crime_data <- read.csv(file.choose())
attach(crime_data)
summary(crime_data)

## normalizes data as they are in diff types

normalize_data <- scale(crime_data[,2:5]) ## converting data into z scores
d <- dist(normalize_data,method = "euclidean") ## distance matrix

fit <- hclust(d,method = "complete") ## use hiearachical clustering 
str(fit)

plot(fit) ## show dendogram
plot(fit,hang = -1)

rect.hclust(fit,k=3,border = "blue")

groups <- cutree(fit,k=3)
groups

membership <- as.matrix(groups)
View(membership)

final <- data.frame(crime_data,membership)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

write.csv(final1,file = "Crime_Data.csv",row.names = FALSE)
getwd()


## EastWestAirline

library(readxl)

EWAirlines2 <- read_excel(file.choose())

eWAirline <- EWAirlines2[1:3999,c(2,7:11)]

summary(eWAirline)

## Normalize data

normalize_airline <- scale(eWAirline)

dist_airline <- dist(normalize_airline,method = "euclidean")
fit_airline <- hclust(dist_airline,method = "complete")
str(fit_airline)

plot(fit_airline)

plot(fit_airline,hand=-1)

rect.hclust(fit_airline,k=8,border = "red")

group_airline <- cutree(fit_airline,k=8)
group_airline

member_airline <- as.matrix(group_airline)
View(member_airline)

final_airline <- data.frame(EWAirlines2,member_airline)
final1_airline <- final_airline[,c(ncol(final_airline),1:(ncol(final_airline)-1))]



library(readxl) ## k means

EWAirlines2 <- read_excel(file.choose())

eWAirline <- EWAirlines2[1:3999,c(2,7:11)]

summary(eWAirline)

normalize_airline <- scale(eWAirline)

fit_airline1 <- kmeans(normalize_airline,8)
str(fit_airline1)

fit_airline1$cluster

install.packages("plyr")
library(plyr)
install.packages("kselection")
library(kselection)
final_airline3 <- data.frame(EWAirlines2,fit_airline1$cluster)
final_airline3

final_airline4 <- final_airline3[,c(ncol(final_airline3),1:(ncol(final_airline3)-1))]
View(final_airline4)

aggregate(final_airline4[,2:13],by=list(fit_airline1$cluster),FUN=mean)


## k selection

library(kselection)
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 8)
k <- kselection(normalize_airline,parallel = TRUE,k_threshold=0.95,max_centers = 12)
k
