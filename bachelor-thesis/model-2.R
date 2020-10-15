library(car)
library(perturb)
library(MASS)
library(MPV)
library(ggplot2)
library(reshape2)
library(olsrr)
library(DAAG)
library(boot)
library(forecast)

setwd("D:/School (KTH)/Kandidatexamensarbete/Modell_2")


##HENRIK LÖPTUR nr 3

henrik_3 <- read.table("Henrik-3.csv", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE)
henrik_3_acc <- read.table("Henrik-3S10.csv", header = TRUE, sep=",", dec=".")

henrik_3$time_since_start <- (henrik_3$t - henrik_3$t[1])/1000

henrik_3$delta_distance <- c()
henrik_3$delta_distance[1] <- 0

for(i in 2:8887) {
henrik_3$delta_distance[i] <- henrik_3$distance[i]-henrik_3$distance[i-1]
}
for(i in 1:152975){
  henrik_3_acc$time_since_start[i] <- (3077/152975)*i
}

#SPM
henrik_3[henrik_3 == "nil"] <- 0
henrik_3[,13] <- as.numeric(as.character(henrik_3[,13]))

#5 seconds averages
space_vector <- seq(from = 0, to = 8887, by = 5)

henrik_3$time_interval <- cut(henrik_3$time_since_start,
                              breaks = c(-Inf,space_vector,Inf),
                              right = FALSE)
henrik_3_acc$time_interval <- cut(henrik_3_acc$time_since_start,
                                  breaks = c(-Inf, space_vector, Inf),
                                  right = FALSE)

henrik_3_acc2 <- aggregate(henrik_3_acc[,c(2,3,4,5)],
                           by = list(time_interval = henrik_3_acc$time_interval),
                           FUN = mean, na.rm = TRUE)
#choose those columns that will be aggregated by sums
#add_time_intervals
henrik_3_sum <- data.frame(henrik_3$delta_distance)
henrik_3_sum$time_interval <- henrik_3$time_interval

#aggregate by time interval
henrik_3_sum2 <- aggregate(henrik_3_sum$henrik_3.delta_distance,
                           by = list(time_interval = henrik_3_sum$time_interval),
                           FUN = sum, na.rm = TRUE)
colnames(henrik_3_sum2) <- c("time_interval","delta_distance")

#choose columns that will be aggregated by mean
henrik_3_mean <- henrik_3[,-c(25)]
henrik_3_mean2 <- aggregate(henrik_3_mean[,c(11:13,15:23)],
                            by = list(time_interval = henrik_3_mean$time_interval),
                            FUN = mean, na.rm =TRUE)

henrik_3_data <- merge(henrik_3_mean2, henrik_3_sum2, by = "time_interval", sort = FALSE)
henrik_3_data <- merge(henrik_3_data, henrik_3_acc2, by = "time_interval", sort = FALSE)

henrik_3_data$weight <- 68

henrik_3_data$VO2 <- 0.178*henrik_3_data$weight*(henrik_3_data$delta_distance/1000)

#cut distance and other columns due to multicoll
henrik_3_data_2 <- henrik_3_data[,-c(1,11,14)]

#HENRIK LÖPTUR NR 4

henrik_4 <- read.table("Henrik-4.csv", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE)
henrik_4_acc <- read.table("Henrik-4S10.csv", header = TRUE, sep=",", dec=".")

henrik_4$time_since_start <- (henrik_4$t - henrik_4$t[1])/1000

henrik_4$delta_distance <- c()
henrik_4$delta_distance[1] <- 0

for(i in 2:9772) {
  henrik_4$delta_distance[i] <- henrik_4$distance[i]-henrik_4$distance[i-1]
}
for(i in 1:167600){
  henrik_4_acc$time_since_start[i] <- (3370/167600)*i
}

#SPM
henrik_4[henrik_4 == "nil"] <- 0
henrik_4[,13] <- as.numeric(as.character(henrik_4[,13]))

#5 seconds averages
space_vector_2 <- seq(from = 0, to = 9772, by = 5)

henrik_4$time_interval <- cut(henrik_4$time_since_start,
                              breaks = c(-Inf,space_vector,Inf),
                              right = FALSE)
henrik_4_acc$time_interval <- cut(henrik_4_acc$time_since_start,
                                  breaks = c(-Inf, space_vector, Inf),
                                  right = FALSE)

henrik_4_acc2 <- aggregate(henrik_4_acc[,c(2,3,4,5)],
                           by = list(time_interval = henrik_4_acc$time_interval),
                           FUN = mean, na.rm = TRUE)
#choose those columns that will be aggregated by sums
#add_time_intervals
henrik_4_sum <- data.frame(henrik_4$delta_distance)
henrik_4_sum$time_interval <- henrik_4$time_interval

#aggregate by time interval
henrik_4_sum2 <- aggregate(henrik_4_sum$henrik_4.delta_distance,
                           by = list(time_interval = henrik_4_sum$time_interval),
                           FUN = sum, na.rm = TRUE)
colnames(henrik_4_sum2) <- c("time_interval","delta_distance")

#choose columns that will be aggregated by mean
henrik_4_mean <- henrik_4[,-c(25)]
henrik_4_mean2 <- aggregate(henrik_4_mean[,c(11:13,15:23)],
                            by = list(time_interval = henrik_4_mean$time_interval),
                            FUN = mean, na.rm =TRUE)

henrik_4_data <- merge(henrik_4_mean2, henrik_4_sum2, by = "time_interval", sort = FALSE)
henrik_4_data <- merge(henrik_4_data, henrik_4_acc2, by = "time_interval", sort = FALSE)

henrik_4_data$weight <- 68

henrik_4_data$VO2 <- 0.178*henrik_4_data$weight*(henrik_4_data$delta_distance/1000)

#cut distance and other columns due to multicoll
henrik_4_data_2 <- henrik_4_data[,-c(1,11,14)]

###HENRIK 5
#HENRIK LÖPTUR NR 4

henrik_5 <- read.table("Henrik-5.csv", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE)
henrik_5_acc <- read.table("Henrik-5S10.csv", header = TRUE, sep=",", dec=".")

henrik_5$time_since_start <- (henrik_5$t - henrik_5$t[1])/1000

henrik_5$delta_distance <- c()
henrik_5$delta_distance[1] <- 0

for(i in 2:8521) {
  henrik_5$delta_distance[i] <- henrik_5$distance[i]-henrik_5$distance[i-1]
}
for(i in 1:145450){
  henrik_5_acc$time_since_start[i] <- (2926/145450)*i
}

#SPM
henrik_5[henrik_5 == "nil"] <- 0
henrik_5[,13] <- as.numeric(as.character(henrik_5[,13]))

#5 seconds averages
space_vector_3 <- seq(from = 0, to = 8521, by = 5)

henrik_5$time_interval <- cut(henrik_5$time_since_start,
                              breaks = c(-Inf,space_vector,Inf),
                              right = FALSE)
henrik_5_acc$time_interval <- cut(henrik_5_acc$time_since_start,
                                  breaks = c(-Inf, space_vector, Inf),
                                  right = FALSE)

henrik_5_acc2 <- aggregate(henrik_5_acc[,c(2,3,4,5)],
                           by = list(time_interval = henrik_5_acc$time_interval),
                           FUN = mean, na.rm = TRUE)
#choose those columns that will be aggregated by sums
#add_time_intervals
henrik_5_sum <- data.frame(henrik_5$delta_distance)
henrik_5_sum$time_interval <- henrik_5$time_interval

#aggregate by time interval
henrik_5_sum2 <- aggregate(henrik_5_sum$henrik_5.delta_distance,
                           by = list(time_interval = henrik_5_sum$time_interval),
                           FUN = sum, na.rm = TRUE)
colnames(henrik_5_sum2) <- c("time_interval","delta_distance")

#choose columns that will be aggregated by mean
henrik_5_mean <- henrik_5[,-c(25)]
henrik_5_mean2 <- aggregate(henrik_5_mean[,c(11:13,15:23)],
                            by = list(time_interval = henrik_5_mean$time_interval),
                            FUN = mean, na.rm =TRUE)

henrik_5_data <- merge(henrik_5_mean2, henrik_5_sum2, by = "time_interval", sort = FALSE)
henrik_5_data <- merge(henrik_5_data, henrik_5_acc2, by = "time_interval", sort = FALSE)

henrik_5_data$weight <- 68

henrik_5_data$VO2 <- 0.178*henrik_5_data$weight*(henrik_5_data$delta_distance/1000)

#cut distance and other columns due to multicoll
henrik_5_data_2 <- henrik_5_data[,-c(1,11,14)]

###HENRIK 6
#HENRIK LÖPTUR NR 6

henrik_6 <- read.table("Henrik-6.csv", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE)
henrik_6_acc <- read.table("Henrik-6S10.csv", header = TRUE, sep=",", dec=".")

henrik_6$time_since_start <- (henrik_6$t - henrik_6$t[1])/1000

henrik_6$delta_distance <- c()
henrik_6$delta_distance[1] <- 0

for(i in 2:8715) {
  henrik_6$delta_distance[i] <- henrik_6$distance[i]-henrik_6$distance[i-1]
}
for(i in 1:149750){
  henrik_6_acc$time_since_start[i] <- (3011/149750)*i
}

#SPM
henrik_6[henrik_6 == "nil"] <- 0
henrik_6[,13] <- as.numeric(as.character(henrik_6[,13]))

#5 seconds averages
space_vector_3 <- seq(from = 0, to = 8715, by = 5)

henrik_6$time_interval <- cut(henrik_6$time_since_start,
                              breaks = c(-Inf,space_vector,Inf),
                              right = FALSE)
henrik_6_acc$time_interval <- cut(henrik_6_acc$time_since_start,
                                  breaks = c(-Inf, space_vector, Inf),
                                  right = FALSE)

henrik_6_acc2 <- aggregate(henrik_6_acc[,c(2,3,4,5)],
                           by = list(time_interval = henrik_6_acc$time_interval),
                           FUN = mean, na.rm = TRUE)
#choose those columns that will be aggregated by sums
#add_time_intervals
henrik_6_sum <- data.frame(henrik_6$delta_distance)
henrik_6_sum$time_interval <- henrik_6$time_interval

#aggregate by time interval
henrik_6_sum2 <- aggregate(henrik_6_sum$henrik_6.delta_distance,
                           by = list(time_interval = henrik_6_sum$time_interval),
                           FUN = sum, na.rm = TRUE)
colnames(henrik_6_sum2) <- c("time_interval","delta_distance")

#choose columns that will be aggregated by mean
henrik_6_mean <- henrik_6[,-c(25)]
henrik_6_mean2 <- aggregate(henrik_6_mean[,c(11:13,15:23)],
                            by = list(time_interval = henrik_6_mean$time_interval),
                            FUN = mean, na.rm =TRUE)

henrik_6_data <- merge(henrik_6_mean2, henrik_6_sum2, by = "time_interval", sort = FALSE)
henrik_6_data <- merge(henrik_6_data, henrik_6_acc2, by = "time_interval", sort = FALSE)

henrik_6_data$weight <- 68

henrik_6_data$VO2 <- 0.178*henrik_6_data$weight*(henrik_6_data$delta_distance/1000)

#cut distance and other columns due to multicoll
henrik_6_data_2 <- henrik_6_data[,-c(1,11,14)]


###HENRIK 7
#HENRIK LÖPTUR NR 7

henrik_7 <- read.table("Henrik-7.csv", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE)
henrik_7_acc <- read.table("Henrik-7S10.csv", header = TRUE, sep=",", dec=".")

henrik_7$time_since_start <- (henrik_7$t - henrik_7$t[1])/1000

henrik_7$delta_distance <- c()
henrik_7$delta_distance[1] <- 0

for(i in 2:9630) {
  henrik_7$delta_distance[i] <- henrik_7$distance[i]-henrik_7$distance[i-1]
}
for(i in 1:157750){
  henrik_7_acc$time_since_start[i] <- (3174/157750)*i
}

#SPM
henrik_7[henrik_7 == "nil"] <- 0
henrik_7[,13] <- as.numeric(as.character(henrik_7[,13]))

#5 seconds averages
space_vector_3 <- seq(from = 0, to = 9630, by = 5)

henrik_7$time_interval <- cut(henrik_7$time_since_start,
                              breaks = c(-Inf,space_vector,Inf),
                              right = FALSE)
henrik_7_acc$time_interval <- cut(henrik_7_acc$time_since_start,
                                  breaks = c(-Inf, space_vector, Inf),
                                  right = FALSE)

henrik_7_acc2 <- aggregate(henrik_7_acc[,c(2,3,4,5)],
                           by = list(time_interval = henrik_7_acc$time_interval),
                           FUN = mean, na.rm = TRUE)
#choose those columns that will be aggregated by sums
#add_time_intervals
henrik_7_sum <- data.frame(henrik_7$delta_distance)
henrik_7_sum$time_interval <- henrik_7$time_interval

#aggregate by time interval
henrik_7_sum2 <- aggregate(henrik_7_sum$henrik_7.delta_distance,
                           by = list(time_interval = henrik_7_sum$time_interval),
                           FUN = sum, na.rm = TRUE)
colnames(henrik_7_sum2) <- c("time_interval","delta_distance")

#choose columns that will be aggregated by mean
henrik_7_mean <- henrik_7[,-c(25)]
henrik_7_mean2 <- aggregate(henrik_7_mean[,c(11:13,15:23)],
                            by = list(time_interval = henrik_7_mean$time_interval),
                            FUN = mean, na.rm =TRUE)

henrik_7_data <- merge(henrik_7_mean2, henrik_7_sum2, by = "time_interval", sort = FALSE)
henrik_7_data <- merge(henrik_7_data, henrik_7_acc2, by = "time_interval", sort = FALSE)

henrik_7_data$weight <- 68

henrik_7_data$VO2 <- 0.178*henrik_7_data$weight*(henrik_7_data$delta_distance/1000)

#cut distance and other columns due to multicoll
henrik_7_data_2 <- henrik_7_data[,-c(1,11,14)]



###KEVIN#########3
##KEVIN NR 1

kevin_1 <- read.table("Kevin-1.csv", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE)
kevin_1_acc <- read.table("Kevin-1S10.csv", header = TRUE, sep=",", dec=".")

kevin_1$time_since_start <- (kevin_1$t - kevin_1$t[1])/1000

kevin_1$delta_distance <- c()
kevin_1$delta_distance[1] <- 0

for(i in 2:nrow(kevin_1)) {
  kevin_1$delta_distance[i] <- kevin_1$distance[i]-kevin_1$distance[i-1]
}
for(i in 1:nrow(kevin_1_acc)){
  kevin_1_acc$time_since_start[i] <- (max(kevin_1$time_since_start)/nrow(kevin_1_acc))*i
}

#SPM
kevin_1[kevin_1 == "nil"] <- 0
kevin_1[,13] <- as.numeric(as.character(kevin_1[,13]))

#5 seconds averages
space_vector_3 <- seq(from = 0, to = 9630, by = 5)

kevin_1$time_interval <- cut(kevin_1$time_since_start,
                              breaks = c(-Inf,space_vector,Inf),
                              right = FALSE)
kevin_1_acc$time_interval <- cut(kevin_1_acc$time_since_start,
                                  breaks = c(-Inf, space_vector, Inf),
                                  right = FALSE)

kevin_1_acc2 <- aggregate(kevin_1_acc[,c(2,3,4,5)],
                           by = list(time_interval = kevin_1_acc$time_interval),
                           FUN = mean, na.rm = TRUE)
#choose those columns that will be aggregated by sums
#add_time_intervals
kevin_1_sum <- data.frame(kevin_1$delta_distance)
kevin_1_sum$time_interval <- kevin_1$time_interval

#aggregate by time interval
kevin_1_sum2 <- aggregate(kevin_1_sum$kevin_1.delta_distance,
                           by = list(time_interval = kevin_1_sum$time_interval),
                           FUN = sum, na.rm = TRUE)
colnames(kevin_1_sum2) <- c("time_interval","delta_distance")

#choose columns that will be aggregated by mean
kevin_1_mean <- kevin_1[,-c(25)]
kevin_1_mean2 <- aggregate(kevin_1_mean[,c(11:13,15:23)],
                            by = list(time_interval = kevin_1_mean$time_interval),
                            FUN = mean, na.rm =TRUE)

kevin_1_data <- merge(kevin_1_mean2, kevin_1_sum2, by = "time_interval", sort = FALSE)
kevin_1_data <- merge(kevin_1_data, kevin_1_acc2, by = "time_interval", sort = FALSE)

kevin_1_data$weight <- 84

kevin_1_data$VO2 <- 0.178*kevin_1_data$weight*(kevin_1_data$delta_distance/1000)

#cut distance and other columns due to multicoll
kevin_1_data_2 <- kevin_1_data[,-c(1,11,14)]

###KEVIN#########3
##KEVIN NR 2

kevin_2 <- read.table("Kevin-2.csv", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE)
kevin_2_acc <- read.table("Kevin-2S10.csv", header = TRUE, sep=",", dec=".")

kevin_2$time_since_start <- (kevin_2$t - kevin_2$t[1])/1000

kevin_2$delta_distance <- c()
kevin_2$delta_distance[1] <- 0

for(i in 2:nrow(kevin_2)) {
  kevin_2$delta_distance[i] <- kevin_2$distance[i]-kevin_2$distance[i-1]
}
for(i in 1:nrow(kevin_2_acc)){
  kevin_2_acc$time_since_start[i] <- (max(kevin_2$time_since_start)/nrow(kevin_2_acc))*i
}

#SPM
kevin_2[kevin_2 == "nil"] <- 0
kevin_2[,13] <- as.numeric(as.character(kevin_2[,13]))

#5 seconds averages
space_vector_3 <- seq(from = 0, to = 9630, by = 5)

kevin_2$time_interval <- cut(kevin_2$time_since_start,
                             breaks = c(-Inf,space_vector,Inf),
                             right = FALSE)
kevin_2_acc$time_interval <- cut(kevin_2_acc$time_since_start,
                                 breaks = c(-Inf, space_vector, Inf),
                                 right = FALSE)

kevin_2_acc2 <- aggregate(kevin_2_acc[,c(2,3,4,5)],
                          by = list(time_interval = kevin_2_acc$time_interval),
                          FUN = mean, na.rm = TRUE)
#choose those columns that will be aggregated by sums
#add_time_intervals
kevin_2_sum <- data.frame(kevin_2$delta_distance)
kevin_2_sum$time_interval <- kevin_2$time_interval

#aggregate by time interval
kevin_2_sum2 <- aggregate(kevin_2_sum$kevin_2.delta_distance,
                          by = list(time_interval = kevin_2_sum$time_interval),
                          FUN = sum, na.rm = TRUE)
colnames(kevin_2_sum2) <- c("time_interval","delta_distance")

#choose columns that will be aggregated by mean
kevin_2_mean <- kevin_2[,-c(25)]
kevin_2_mean2 <- aggregate(kevin_2_mean[,c(11:13,15:23)],
                           by = list(time_interval = kevin_2_mean$time_interval),
                           FUN = mean, na.rm =TRUE)

kevin_2_data <- merge(kevin_2_mean2, kevin_2_sum2, by = "time_interval", sort = FALSE)
kevin_2_data <- merge(kevin_2_data, kevin_2_acc2, by = "time_interval", sort = FALSE)

kevin_2_data$weight <- 84

kevin_2_data$VO2 <- 0.178*kevin_2_data$weight*(kevin_2_data$delta_distance/1000)

#cut distance and other columns due to multicoll
kevin_2_data_2 <- kevin_2_data[,-c(1,11,14)]



#vikt kevin 84
#vikt magnus 78

###KEVIN 3

kevin_3 <- read.table("Kevin-3.csv", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE)
kevin_3_acc <- read.table("Kevin-3S10.csv", header = TRUE, sep=",", dec=".")

kevin_3$time_since_start <- (kevin_3$t - kevin_3$t[1])/1000

kevin_3$delta_distance <- c()
kevin_3$delta_distance[1] <- 0

for(i in 2:nrow(kevin_3)) {
  kevin_3$delta_distance[i] <- kevin_3$distance[i]-kevin_3$distance[i-1]
}
for(i in 1:nrow(kevin_3_acc)){
  kevin_3_acc$time_since_start[i] <- (max(kevin_3$time_since_start)/nrow(kevin_3_acc))*i
}

#SPM
kevin_3[kevin_3 == "nil"] <- 0
kevin_3[,13] <- as.numeric(as.character(kevin_3[,13]))

#5 seconds averages
space_vector_3 <- seq(from = 0, to = 9630, by = 5)

kevin_3$time_interval <- cut(kevin_3$time_since_start,
                             breaks = c(-Inf,space_vector,Inf),
                             right = FALSE)
kevin_3_acc$time_interval <- cut(kevin_3_acc$time_since_start,
                                 breaks = c(-Inf, space_vector, Inf),
                                 right = FALSE)

kevin_3_acc2 <- aggregate(kevin_3_acc[,c(2,3,4,5)],
                          by = list(time_interval = kevin_3_acc$time_interval),
                          FUN = mean, na.rm = TRUE)
#choose those columns that will be aggregated by sums
#add_time_intervals
kevin_3_sum <- data.frame(kevin_3$delta_distance)
kevin_3_sum$time_interval <- kevin_3$time_interval

#aggregate by time interval
kevin_3_sum2 <- aggregate(kevin_3_sum$kevin_3.delta_distance,
                          by = list(time_interval = kevin_3_sum$time_interval),
                          FUN = sum, na.rm = TRUE)
colnames(kevin_3_sum2) <- c("time_interval","delta_distance")

#choose columns that will be aggregated by mean
kevin_3_mean <- kevin_3[,-c(25)]
kevin_3_mean2 <- aggregate(kevin_3_mean[,c(11:13,15:23)],
                           by = list(time_interval = kevin_3_mean$time_interval),
                           FUN = mean, na.rm =TRUE)

kevin_3_data <- merge(kevin_3_mean2, kevin_3_sum2, by = "time_interval", sort = FALSE)
kevin_3_data <- merge(kevin_3_data, kevin_3_acc2, by = "time_interval", sort = FALSE)

kevin_3_data$weight <- 84

kevin_3_data$VO2 <- 0.178*kevin_3_data$weight*(kevin_3_data$delta_distance/1000)

#cut distance and other columns due to multicoll
kevin_3_data_2 <- kevin_3_data[,-c(1,11,14)]

###MAGNUS 1

magnus_1 <- read.table("Magnus-1.csv", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE)
magnus_1_acc <- read.table("Magnus-1S10.csv", header = TRUE, sep=",", dec=".")

magnus_1$time_since_start <- (magnus_1$t - magnus_1$t[1])/1000

magnus_1$delta_distance <- c()
magnus_1$delta_distance[1] <- 0

for(i in 2:nrow(magnus_1)) {
  magnus_1$delta_distance[i] <- magnus_1$distance[i]-magnus_1$distance[i-1]
}
for(i in 1:nrow(magnus_1_acc)){
  magnus_1_acc$time_since_start[i] <- (max(magnus_1$time_since_start)/nrow(magnus_1_acc))*i
}

#SPM
magnus_1[magnus_1 == "nil"] <- 0
magnus_1[,13] <- as.numeric(as.character(magnus_1[,13]))

#5 seconds averages
space_vector_3 <- seq(from = 0, to = 9630, by = 5)

magnus_1$time_interval <- cut(magnus_1$time_since_start,
                             breaks = c(-Inf,space_vector,Inf),
                             right = FALSE)
magnus_1_acc$time_interval <- cut(magnus_1_acc$time_since_start,
                                 breaks = c(-Inf, space_vector, Inf),
                                 right = FALSE)

magnus_1_acc2 <- aggregate(magnus_1_acc[,c(2,3,4,5)],
                          by = list(time_interval = magnus_1_acc$time_interval),
                          FUN = mean, na.rm = TRUE)
#choose those columns that will be aggregated by sums
#add_time_intervals
magnus_1_sum <- data.frame(magnus_1$delta_distance)
magnus_1_sum$time_interval <- magnus_1$time_interval

#aggregate by time interval
magnus_1_sum2 <- aggregate(magnus_1_sum$magnus_1.delta_distance,
                          by = list(time_interval = magnus_1_sum$time_interval),
                          FUN = sum, na.rm = TRUE)
colnames(magnus_1_sum2) <- c("time_interval","delta_distance")

#choose columns that will be aggregated by mean
magnus_1_mean <- magnus_1[,-c(25)]
magnus_1_mean2 <- aggregate(magnus_1_mean[,c(11:13,15:23)],
                           by = list(time_interval = magnus_1_mean$time_interval),
                           FUN = mean, na.rm =TRUE)

magnus_1_data <- merge(magnus_1_mean2, magnus_1_sum2, by = "time_interval", sort = FALSE)
magnus_1_data <- merge(magnus_1_data, magnus_1_acc2, by = "time_interval", sort = FALSE)

magnus_1_data$weight <- 78

magnus_1_data$VO2 <- 0.178*magnus_1_data$weight*(magnus_1_data$delta_distance/1000)

#cut distance and other columns due to multicoll
magnus_1_data_2 <- magnus_1_data[,-c(1,11,14)]
















############################################33
#MERGE ALL
data_full <- rbind(henrik_3_data_2, henrik_4_data_2, henrik_5_data_2,henrik_6_data_2,henrik_7_data_2,
                          kevin_1_data_2, kevin_2_data_2, kevin_3_data_2, magnus_1_data_2)

#CUT 1330 to 1341 due to data incorrections
data_full <- data_full[-c(1330:1341),]
data_full <- data_full[-c(1042,1043),]

#FIX NaN rows to 0
data_full[data_full == "NaN"] <- 0
#change VO2 to branch standard
data_full$VO2 <- data_full$VO2 * 1000
#VO2 per minute
data_full$delta_time <- c()
data_full$delta_time[1]<-0
for(i in 2:nrow(data_full)){
  data_full$delta_time[i] <- data_full$time_since_start[i]-data_full$time_since_start[i-1]
}
data_full$VO2_per_kg <- data_full$VO2/(data_full$weight*(data_full$delta_time/60))
#remove multicoll columns
data_full <- data_full[,-c(16:18)]

colnames(data_full)[9] <- "HR"

VO2_model <- lm(VO2_per_kg~.,data = data_full)

summary(VO2_model)

#PLOTS OF VARIABLES
body_melt <- melt(data_full,"VO2") 
ggplot(body_melt,aes(x=value ,y=VO2))+geom_point()+facet_wrap(~variable,scales="free")+geom_smooth(method="lm")


cutoff_values_covratio <- c(1+3*ncol(model.matrix(VO2_model))/(nrow(model.matrix(VO2_model))), 
                            1-3*ncol(model.matrix(VO2_model))/(nrow(model.matrix(VO2_model))))
cutoff_values_dffit <- 2*sqrt(ncol(model.matrix(VO2_model))/(nrow(model.matrix(VO2_model))))




## corr-matrix
X1 <- model.matrix(VO2_model)
condition_number <- kappa(X1)#lower than full model!!
X <- X1[,-1] #remove intercept column
res <- cor(X)
res

#Residual plots

par(mfrow=c(2,2))
plot(VO2_model$fitted.values, VO2_model$residuals, main = "Regular Residuals")
plot(VO2_model$fitted.values, rstandard(VO2_model), main = "Standardized residuals", xlab = "Fitted values",
     ylab = "Standard residuals")
plot(VO2_model$fitted.values, studres(VO2_model), main = "Studentized residuals")
plot(VO2_model$fitted.values, rstudent(VO2_model), main = "R-Student residuals")

#Influence plots
par(mfrow=c(2,3))
plot(VO2_model, which=1:5)
influencePlot(VO2_model)

#Influence measures
infl <- influence.measures(VO2_model)
summary(infl)

forward <- ols_step_forward_p(VO2_model)
plot(forward)

backward <- ols_step_backward_p(VO2_model)
plot(backward)

VO2_best_model <- forward$model
summary(VO2_best_model)

par(mfrow=c(2,2))
plot(VO2_best_model$fitted.values, VO2_best_model$residuals, main = "Regular Residuals")
plot(VO2_best_model$fitted.values, rstandard(VO2_best_model), main = "Standardized residuals")
plot(VO2_best_model$fitted.values, studres(VO2_best_model), main = "Studentized residuals")
plot(VO2_best_model$fitted.values, rstudent(VO2_best_model), main = "R-Student residuals")

#cross-validation
cv_best_model <- CVlm(data = data_full, form.lm = formula(VO2_best_model), m=10,plotit = "Observed")

attributes(cv_best_model)$ms
#very close to 0

