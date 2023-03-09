# read in data
data <- read.csv("/Users/12103/Desktop/CS555 HW/HW3/fish_mercury.csv")
#Q1
# create scatterplot with number of meals on the x-axis and mercury levels on the y-axis
plot(data$Number.of.meals.with.fish, data$Total.Mercury.in.mg.g,
     xlab = "Number of meals with fish", ylab = "Total Mercury (mg/g)",
     main = "Scatterplot of number of meals with fish and mercury levels")
#Q2
cor(data$Number.of.meals.with.fish, data$Total.Mercury.in.mg.g)
#Q3
model <- lm(Total.Mercury.in.mg.g ~ Number.of.meals.with.fish, data=data)
summary(model)
plot(data$Number.of.meals.with.fish, data$Total.Mercury.in.mg.g, 
     xlab="Number of meals with fish", ylab="Total Mercury in mg/g", 
     main="Scatterplot of Number of meals with fish and Total Mercury levels")
abline(model, col="red")
#Q4
# fitting the linear regression model
model <- lm(`Total Mercury in mg/g` ~ `Number of meals with fish`, data = data)

# extracting the regression coefficients
beta0 <- coef(model)[1]
beta1 <- coef(model)[2]

# interpreting the coefficients in the context of the data set
cat("The estimate for beta1 is", beta1, "which means that on average, for each additional meal with fish consumed per week, the total mercury in mg/g increases by", beta1, "mg/g.\n")
cat("The estimate for beta0 is", beta0, "which means that when the number of meals with fish consumed per week is 0, the total mercury in mg/g is", beta0, "mg/g.\n")
#Q5
# ANOVA table
anova(model)
# standard error of beta1
summary(model)$coefficients[2, 2]
# F-test for beta1 = 0
summary(model)$fstatistic
# 5-step procedure for testing beta1 = 0 at alpha = 0.05
# Step 1: State the null and alternative hypotheses
# H0: beta1 = 0
# Ha: beta1 != 0
# Step 2: Determine the test statistic
# F-test, so the test statistic is the F-statistic from the ANOVA table
# Step 3: Determine the p-value
# From the ANOVA table, we see that the p-value is less than 0.05
# Step 4: Make a decision
# Since the p-value is less than 0.05, we reject the null hypothesis.
# Step 5: Interpret the results
# We have sufficient evidence to conclude that there is a significant linear relationship between the number of meals with fish consumed per week and the total mercury in mg/g.
# R-squared value
summary(model)$r.squared
# 90% confidence interval for beta1
confint(model, level = 0.90)

