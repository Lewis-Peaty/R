library(readxl)
setwd("C:\\Users\\n043341\\Documents\\Documents\\Software\\R")
d <- read_excel("STA3300_Ass1_Q6.xlsx",sheet = "Sheet1")
d2 <- as.data.frame(cbind(d[d$Sample=="A",2],d[d$Sample=="B",2]))
colnames(d2) <- c("Sample A","Sample B")
d$Sample <- as.factor(d$Sample)

summary(d2$`Sample A`)
summary(d2$`Sample B`)

#T test
#t.test(d$Value ~ d$Sample)
ttest <- t.test(d2$`Sample B`,d2$`Sample A`,conf.level = 0.95,paired = T) #Different syntax for same test (easier to understand)
(ttest)

#Anova
anova <- aov(Value ~ Sample,data=d) #Just use stacked format...
summary(anova)

#Regression
fit <- lm(Value ~ Sample,data=d) #Just use stacked format...
anova(fit)
summary(fit)

#Note to self
#Observe the F statistic for ANOVA and regression is the same (well, scaled) as the P-value
#for the T-Test.

### PLAYING AROUND
#Manual ANOVA parameters
y <- c(20,15,16,18,22,16,20,21,11,14,18,13)
y <- matrix(y,nrow=4,ncol=3,byrow = T)
u <- mean(y)
t_a <- mean(y[,1]) - u
t_b <- mean(y[,2]) - u
t_c <- mean(y[,3]) - u
err <- y - u
err[,1] <- err[,1] - t_a
err[,2] <- err[,2] - t_b
err[,3] <- err[,3] - t_c
err

y <- c(26.26,43.31,46.92,60.18,45.47,50.48,45.53,45.62,45.41,51.95,60.96,58.01,68.40,61.70,55.09,39.42,41.74,34.25,43.23,67.69,57.91,56.70,43.96,50.16,46.01,55.51,58.10,37.62,37.79,34.91,48.15,50.21,33.98,49.61,55.66,39.15)
y <- as.data.frame(matrix(y,nrow=9,ncol=4,byrow=T))
summary(y)

ano <- aov( values ~ ind  ,stack(y)) #TAKE NOTE OF THIS STACK FUNCTION (ALSO UNSTACK)
summary(ano)

fit <- lm( values ~ ind  ,stack(y))
anova(fit)
summary(fit)

###/PLAYING AROUND
