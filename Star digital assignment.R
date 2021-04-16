#librarys 
library(dplyr)

#read data
SD_data <- read.csv('C:\\Users\\darsh\\OneDrive\\Winter Quarter\\BANA 277 - Cust & Social Analytics\\New folder\\star_digital(1).csv')
head(SD_data)
summary(SD_data)

# Question 1
model_1 <- glm(purchase ~ test, data=SD_data, family= binomial()) 
summary(model_1)
exp(coef(model_1))
t.test(SD_data$purchase~SD_data$test)

# Question 2
new_SD_data <- mutate(SD_data, Timp=imp_1+imp_2+imp_3+imp_4+imp_5+imp_6)
new_SD_data <- mutate(new_SD_data, imp_15=imp_1+imp_2+imp_3+imp_4+imp_5)

model_2 <- glm(purchase ~ Timp, data=new_SD_data,family=binomial())
summary(model_2)
exp(coef(model_2))

model_2.1 <- glm(purchase ~ Timp*test, data=new_SD_data,family=binomial())
summary(model_2.1)
exp(coef(model_2.1))

# Question 3

model_3 <- glm(purchase ~  imp_6 + imp_6*test + imp_15 + imp_15*test, data=new_SD_data,family=binomial())
summary(model_3)
exp(coef(model_3))

# Question 4
new_SD_data$offset <- 6.48 
model_4 <- glm(purchase ~  imp_6 + imp_6*test + imp_15 + imp_15*test,offset = new_SD_data$offset, data=new_SD_data,family=binomial())
summary(model_4)
exp(coef(model_4))

Average_15 <- mean(imp_1+imp_2+imp_3+imp_4+imp_5)
Average_15

Average_6 <- mean(imp_6)
Average_6