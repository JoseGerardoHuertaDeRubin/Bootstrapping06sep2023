#IMPORT DATA
prostmat <- read.csv("http://hastie.su.domains/CASI_files/DATA/prostmat.csv")
View(prostmat)
##################################
library(stats)

# Bootstrap the Z-statistics
zscore<-function(d,i) {
  sic<-c(d[i,51:102])
  co<-c(d[i,1:50])
  m1<-mean(as.double(unlist(co)))
  m2<-mean(as.double(unlist(sic)))
  zz<-(m1-m2)/sqrt(1/50+1/52)
  return(zz)}
library(boot)
bootstrapz<- boot(prostmat, statistic =zscore , R = 10)
bootstrapz
print(bootstrapz$t0)
summary(bootstrapz)
z_scores<-bootstrapz$t
hist(z_scores)

#######################NOW USING THE ORIGNAL DATA BUT WITH Z SCORES#############
#Dividing the groups function

statz<- function(data, indices) {
  patient_group <- data[indices, 51:102]  # Select the patient group columns
  control_group <- data[indices, 1:50]  # Select the control group columns
  mean_diff <- colMeans(patient_group) - colMeans(control_group)  # Compute mean difference
  return(mean_diff)}
set.seed(123)

# Perform bootstrapping
results <- boot(data = prostmat, statistic = statz, R = 1000)

# Compute Z-scores from the bootstrapped results
z_scores <- (results$t - mean(results$t)) /1
print(z_scores)
significant_genes2<- which(z_scores> critical_value | z_scores< -critical_value)
significant_genes2
