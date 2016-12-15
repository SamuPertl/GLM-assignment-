d <-read.csv("data.fish.csv", stringsAsFactors = F)

d <- d[d$year == "2011", ]
# lets put insome jitter functions and transparent colours

jitter.t <- 20  # this is great for papers with many papers

col.t <- c("#FF0000", "#00FF00") #(lists the colours)
alpha.t <- c(50, "FF")  # this is the final 2 digits, and lowering the value gives greater transparency
plot(d$age, jitter(d$number.eggs, jitter.t), pch = 16, col = paste(col.t[1], alpha.t[1], sep=""))
lm.d<-lm(d$number.eggs ~ d$age)
abline(lm.d)
hist(d$number.eggs)

# Q d) Defining the dataset without number of eggs = 0
d1 <- d[d$number.eggs > 0, ]

# Q e) log transform the data (in a new column)
d1$log.egg <- log(d1$number.eggs)

lm.d1 <- lm(d1$log.egg ~ d1$age, na.action = na.exclude)
summary(lm.d1)
abline(lm.d1)

# Q f) creating predicted values
plot(d1$age, jitter(d1$log.egg, jitter.t), pch = 16, col = paste(col.t[1], alpha.t[1], sep=""), xlim = c(0,24))
abline(lm.d1)


#Q h) generalised linear model 
glm1 <- glm(number.eggs ~ age, data = d, na.action = na.exclude, family = poisson)
summary(glm1)

#Q i) Method 1, plot line using fitted value
plot(d$age, jitter(d$number.eggs, jitter.t), pch = 16, col = paste(col.t[1], alpha.t[1], sep=""))
coef(glm1)
line(d$age, glm1$fitted.values)



library(ggplot2)
glm1$model$fitted <- predict(glm1, type = "response")
ggplot(glm1)+
  geom_jitter(aes(age, number.eggs))+ 
  geom_line(aes(age, fitted))
