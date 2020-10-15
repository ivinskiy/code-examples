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
library(ridge)

setwd("D:/School (KTH)/Kandidatexamensarbete/Modell_3")

kevin_data_racefox <- read.table("112054.csv", header = TRUE, sep = ",", dec = ".")
kevin_data_racefox_kpi <- read.table("112054S10.csv", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE)
kevin_data_stryd <- read.table("stryd-2019.04.07-milen.csv", header = TRUE, sep = ",", dec = ".")

kevin_data_racefox_kpi$time_since_start <- (kevin_data_racefox_kpi$t - kevin_data_racefox_kpi$t[1])/1000

for(i in 1:151275){
kevin_data_racefox$time_since_start[i] <- 0.02*i}


kevin_data_racefox_kpi[kevin_data_racefox_kpi == "nil"] <- 0
kevin_data_racefox_kpi[,13] <- as.numeric(as.character(kevin_data_racefox_kpi[,13]))

space_vector_racefox_kpi <- seq(from = 0, to = 3017, by = 1)
space_vector_racefox <- seq(from = 0, to = 3017, by = 1)

kevin_data_racefox$t_seconds <- cut(kevin_data_racefox$time_since_start, 
                                    breaks = c(-Inf, space_vector_racefox, Inf),
                                    right = FALSE)

kevin_data_racefox_kpi$t_seconds <- cut(kevin_data_racefox_kpi$time_since_start,
                                        breaks = c(-Inf, space_vector_racefox_kpi, Inf),
                                        right = FALSE)

kevin_data_racefox2 <- aggregate(kevin_data_racefox[,2:5], by = list(t_seconds = kevin_data_racefox$t_seconds),
                                 FUN = mean, na.rm = TRUE)

kevin_data_racefox_kpi2 <- aggregate(kevin_data_racefox_kpi[,c(11,12,13,15:24)],
                                     by = list(t_seconds = kevin_data_racefox_kpi$t_seconds),
                                     FUN = mean, na.rm = TRUE)

#kevin_data_stryd$t_seconds <- cut(kevin_data_stryd$secs,
 #                                 breaks = c(-Inf,space_vector_racefox,Inf),
  #                                right = FALSE)

#due to differences in time, assume that they all started the same, will merge all observations
#after second 3017 to one.
#AND CUT THE AGGREGATED OBSERVATIONS FROM BOTH
kevin_data_racefox2 <- head(kevin_data_racefox2, -1)
kevin_data_racefox_kpi2 <- head(kevin_data_racefox_kpi2, -1)

#merge all the data_files
kevin_data2 <- merge(kevin_data_racefox2,kevin_data_racefox_kpi2, by = c("t_seconds"), sort = FALSE)
kevin_data <- cbind(kevin_data2, kevin_data_stryd)

#common time since start, average of all three.
kevin_data$time_since_start <- (kevin_data$time_since_start.x+kevin_data$time_since_start.y+kevin_data$secs)/3

#data did not measure HR from beginning, cut first 413 rows due to that
kevin_data <- kevin_data[-c(1:413),]
#remove redundant columns
kevin_data <- kevin_data[,-c(1,5,18,19:24,26:41)]


# kevin_data$delta_t_secs <- c()
# kevin_data$delta_t_secs[1] <- 0
# for(i in 2:2604){
#   kevin_data$delta_t_secs[i] <- kevin_data$time_since_start[i]-kevin_data$time_since_start[i-1]
# }
# 
# kevin_data$energy <- kevin_data$watts*kevin_data$delta_t_secs

# kevin_model <- lm(energy~acc_x + acc_y + acc_z + GCT + VO + rFootsym+lFootsym+diffFootsym
#                   +landingDrag+maxY+heartRate+distance+speed+elevation+time_since_start, data = kevin_data)

kevin_model <- lm(watts~., data = kevin_data)

#+.^2
#I(acc_x^2)+I(acc_y^2)+I(acc_z^2)+I(GCT^2)+I(VO^2)+I(rFootsym^2)
# +I(lFootsym^2)+I(diffFootsym^2)+I(landingDrag^2)+I(maxY^2)+I(heartRate^2)+
#   I(SPM^2)+I(distance^2)+I(speed^2)+I(elevation^2)+
#   I(time_since_start^2

summary(kevin_model)

body_melt <- melt(kevin_data,"watts") 
ggplot(body_melt,aes(x=value ,y=watts))+geom_point()+facet_wrap(~variable,scales="free")+geom_smooth(method="lm")


par(mfrow=c(2,2))
qqnorm(rstandard(kevin_model)) #takes the standardized residuals
qqnorm(kevin_model$residuals)

#Residual plots

par(mfrow=c(2,2))
plot(kevin_model$fitted.values, kevin_model$residuals, main = "Regular Residuals")
plot(kevin_model$fitted.values, rstandard(kevin_model), main = "Standardized residuals", xlab = "Fitted values",
     ylab = "Standard residuals")
plot(kevin_model$fitted.values, studres(kevin_model), main = "Studentized residuals")
plot(kevin_model$fitted.values, rstudent(kevin_model), main = "R-Student residuals")

#Influence plots
par(mfrow=c(2,3))
plot(kevin_model, which=1:5)
influencePlot(kevin_model)

vif(kevin_model)
PRESS(kevin_model)

backward_selection <- ols_step_backward_p(kevin_model)
plot(backward_selection)

forward_selection <- ols_step_forward_p(kevin_model)
plot(forward_selection)


#best model on backward elimination
kevin_best_model <- backward_selection$model
summary(kevin_best_model)


par(mfrow=c(2,2))
plot(kevin_best_model$fitted.values, kevin_best_model$residuals, main = "Regular Residuals")
plot(kevin_best_model$fitted.values, rstandard(kevin_best_model), main = "Standardized residuals")
plot(kevin_best_model$fitted.values, studres(kevin_best_model), main = "Studentized residuals")
plot(kevin_best_model$fitted.values, rstudent(kevin_best_model), main = "R-Student residuals")

#cross-validation on model
cv_best_model <- CVlm(data = kevin_data, form.lm = formula(kevin_best_model), m=10,plotit = "Observed")

attributes(cv_best_model)$ms

#remove distnance due to multicoll
kevin_data_mod <- kevin_data[,-c(13)]
best_model_2 <- lm(watts~.+.^2,kevin_data_mod)
plot(best_model_2$fitted.values, rstandard(best_model_2), main = "Standardized residuals")


cutoff_values_covratio <- c(1+3*ncol(model.matrix(best_model_2))/(nrow(model.matrix(best_model_2))), 
                            1-3*ncol(model.matrix(best_model_2))/(nrow(model.matrix(best_model_2))))
cutoff_values_dffit <- 2*sqrt(ncol(model.matrix(best_model_2))/(nrow(model.matrix(best_model_2))))


forw <- ols_step_forward_p(best_model_2)
plot(forw)

