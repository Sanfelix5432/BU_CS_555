#Question 1
data <- read.csv("C:/Users/12103/Desktop/CS555 HW/HW1/LU_Homework1.csv", header = FALSE)
print(data)
data <- unlist(data, use.names = FALSE)
print(data)

#Question 2
hist(data,xlim = c(0,17), breaks = seq(0, 15, 1), ylim = c(0, 25))
#Question 3
summary <- summary(data)
print(summary)
IQR <- 7.00 - 4.00
print(IQR)
MIN <- 4 - 1.5 * IQR
MAX <- 7 + 1.5 * IQR
print(MIN)
print(MAX)
table <- table(data)
print(table)
std <- sd(data)
print(std)

#Question 4
p_less_10 <- pnorm(10, 5, 3)
p_greater_6 <- 1 - pnorm(6, 5, 3/sqrt(35))
percentage <- p_less_10*100
print(sprintf("The probability of patients less than 10 days is %f percent ",percentage))
percentage <- p_greater_6*100
print(sprintf("The probability of patients greater than 6 days is %f percent ",percentage))


