


setwd("D:/School (KTH)/Kandidatexamensarbete/VO2test")

kevin_v02 <- read.table("kev_VO2.csv", header = TRUE, sep = ",", dec = ".")
kevin_acc <- read.table("113492.csv", header = TRUE, sep=",", dec=".")
kevin_kpi <- read.table("113492S10.csv", header = TRUE, sep="\t", dec=".", stringsAsFactors = FALSE)

kevin_kpi$time_since_start <- (kevin_kpi$t - kevin_kpi$t[1])/1000

#started measuring VO2 after 12 minutes (720 seconds)
kevin_kpi <- kevin_kpi[-c(1:1551),]
#restart time
kevin_kpi$time_since_start <- kevin_kpi$time_since_start - kevin_kpi$time_since_start[1]

#SPM
kevin_kpi[kevin_kpi == "nil"] <- 0
kevin_kpi[,13] <- as.numeric(as.character(kevin_kpi[,13]))

#time_since_start for acc-data
for(i in 1:nrow(kevin_acc)){
  kevin_acc$time_since_start[i] <- (max(kevin_kpi$time_since_start)/nrow(kevin_acc))*i
}

#5-second average for both
space_vector <- seq(from = 0, to = 1773, by = 5)

kevin_kpi$time_interval <- cut(kevin_kpi$time_since_start,
                             breaks = c(-Inf,space_vector,Inf),
                             right = FALSE)
kevin_acc$time_interval <- cut(kevin_acc$time_since_start,
                                 breaks = c(-Inf, space_vector, Inf),
                                 right = FALSE)

kevin_acc2 <- aggregate(kevin_acc[,c(2,3,4,5)],
                          by = list(time_interval = kevin_acc$time_interval),
                          FUN = mean, na.rm = TRUE)
kevin_kpi2 <- aggregate(kevin_kpi[,c(11:13,15:24)],
                        by = list(time_interval = kevin_kpi$time_interval),
                        FUN = mean, na.rm = TRUE)

#merge both Racefox-data
test_data <- merge(kevin_acc2,kevin_kpi2,by="time_interval", sort = FALSE)

#5 second averages for vo2
space_vector_VO2 <- seq(from = 0, to = 1773, by = 5)

kevin_v02$time_interval <- cut(kevin_v02$time_since_start,
                               breaks = c(-Inf,space_vector_VO2,Inf),
                               right = FALSE)

kevin_VO2 <- aggregate(kevin_v02[,c(2,6)],
                       by = list(time_interval = kevin_v02$time_interval),
                       FUN = mean, na.rm = TRUE)
colnames(kevin_VO2)[3] <- "VO2_per_kg"



test_data_2 <- merge(kevin_VO2,test_data, by = "time_interval", sort = FALSE)
test_data_2$time_since_start <- (test_data_2$time_since_start + test_data_2$time_since_start.x + 
                                   test_data_2$time_since_start.y)/3
test_data_2 <- test_data_2[,-c(1,7,20)]
colnames(test_data_2)[14]<-"HR"

predict_GIH <- predict(best_model_fit, newdata= test_data_2)

predict_GIH <- data.frame(predict_GIH)
compare_GIH <- cbind(predict_GIH, test_data_2$VO2_per_kg)
mean(apply(compare_GIH, 1, min)/apply(compare_GIH, 1, max))


predict_assump <- predict(VO2_best_model, newdata = test_data_2)
combare_assump <- cbind(predict_assump, test_data_2$VO2_per_kg)
mean(apply(combare_assump, 1, min)/apply(combare_assump, 1, max))


