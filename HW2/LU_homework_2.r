cl <- read.csv("/Users/12103/Desktop/CS555 HW/HW2/Calorie.csv", header = T)
p <- cl$p
np <- na.omit(cl$np)
#1
par(mfrow=c(1,1))
hist(cl$p,xlab = "Calories", main = "Participants",breaks = 5)
par(mfrow=c(1,1),mar = c(1, 1, 1, 1))
hist(cl$np,xlab = "Calories", main = "Non-Participants",breaks = 5)
#2
#step4:
mean <- mean(p)
s <- sd(p)
n <- length(p)
t <- (mean - 425)/ (s/sqrt(n))
absolute_t <- abs(t)
absolute_t
#step5
t.test(p, mu = mean,alternative = "two.sided")

#3
t.test(p, mu = mean, alternative = "two.sided", conf.level = 0.9)

#4
#step4: 
mean_1 <- mean(p)
mean_2 <- mean(np)
s_1 <- sd(p)
s_2 <- sd(np)
n_1 <- length(p)
n_2 <- length(np)
t <- (mean_1-mean_2)/(sqrt((s_1^2/n_1)+(s_2^2/n_2)))
t
#step5: 
t.test(p,np,alternative = "greater", conf.level = 0.95)


#extra credit
before <- c(158, 185, 176, 172, 164, 234, 258, 200, 228, 246, 198, 221, 236, 255, 231)
after <- c(163, 182, 188, 150, 161, 220, 235, 191, 228, 237, 209, 220, 222, 268, 234)
differ<- after - before
t.test(differ, mu = 0, alternative = "two.sided")

