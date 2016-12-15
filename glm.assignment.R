y## assignment 4 

library(ggplot2)
library(dplyr)

getwd()

list.files()

df1 <- read.csv("data.fish.csv")

# effect of age on reproductive success 

# visual examination 

ggplot(df1, aes(age, number.eggs))+
  geom_point()+
  ylim(0,1000)+
  geom_smooth(method = "lm", se = FALSE)
 
# lot of 0´s, count data, clustered, bounded at zero, non homogenity in variance 

# type of data = count data (discret data )
# wich distribution = 

# c) name for data set with many 0´s 
#bounded data set, 
# how to deal with it -> wrong long-transform the data, use a glm with a poisson distribution 

# select number of eggs > 0 

# try it with dplyr 
#df3 <- df1 %>% select(number.eggs > 1)

df2 <- df1[df1$number.eggs >1,]


# log transform number of eggs 
df2$logeggs <- log(df2$number.eggs)

# fit a linear model 
reg1 <- lm(logeggs ~ age, data = df2, na.action = na.exclude)

# show the distribution of the residuals 
hist(reg1$residuals)

# try the same with ggplot 
ggplot(df2, aes(reg1$residuals))+
  geom_histogram()

# log transformation is not appropriate 


