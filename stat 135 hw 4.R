#part A
data <- read.csv('cholesterol.csv')
library(tidyr)
library(ggplot2)


data_long <- data %>%
  pivot_longer(cols = c("whole_grain", "white"), 
               names_to = "method", 
               values_to = "value")

ggplot(data_long, aes(x = method, y = value, fill = method)) +
  geom_boxplot() +
  scale_fill_manual(values = c("whole_grain" = "lightblue", "white" = "lightcoral")) +
  labs(x = "Method", y = "Value", title = "Whole Grain and White Boxplot") +
  theme_minimal()

#part b
t_test_result <- t.test(data$whole_grain, data$white, paired = TRUE, alternative = 'less')
print(t_test_result)

#part c
wilcox_test_signed <- wilcox.test(data$whole_grain, data$white, paired = TRUE, alternative = 'less')
print(wilcox_test_signed)


#problem 4
#part a
n_sim <- 10000
n <- 25       
mean_diff <- 0.6 
sd_diff <- 1    
alpha <- 0.05

rejections <- rep(NA, n_sim)

for (i in 1:n_sim) {
  # Draw 25 pair differences from N(0.6, 1)
  D <- rnorm(n, mean = mean_diff, sd = sd_diff)
  
  pval <- t.test(D)$p.value
  
  rejections[i] <- pval < alpha
}

power_estimate <- mean(rejections)
power_estimate

#part b
n_sim <- 10000  
n <- 25    
mean_diff <- 0.6 
sd_diff <- 1  
alpha <- 0.05

rejections <- rep(NA, n_sim)

for (i in 1:n_sim) {
  # Draw 25 pair differences from N(0.6, 1)
  D <- rnorm(n, mean = mean_diff, sd = sd_diff)
  

  pval <- wilcox.test(D)$p.value
  

  rejections[i] <- pval < alpha
}


power_estimate <- mean(rejections)
power_estimate

#part e
n_sim <- 10000  
n <- 25         
mean_diff <- 2    
alpha <- 0.05


rejections_ttest <- rep(NA, n_sim)

for (i in 1:n_sim) {
  #25 differences of D = 2 + T, where T is distributed t(1)
  D <- 2 + rt(n, df = 1)
  
  
  pval <- t.test(D)$p.value
  
  
  rejections_ttest[i] <- pval < alpha
}


power_ttest <- mean(rejections_ttest)
power_ttest

rejections_wilcox <- rep(NA, n_sim)

for (i in 1:n_sim) {
  #25 differences of D = 2 + T, where T is distributed t(1)
  D <- 2 + rt(n, df = 1)
  
 
  pval <- wilcox.test(D)$p.value
  

  rejections_wilcox[i] <- pval < alpha
}


power_wilcox <- mean(rejections_wilcox)
power_wilcox