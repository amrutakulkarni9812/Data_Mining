# Course CS 513 A
# Amruta Kulkarni
# Midterm Question 4

# Clear the environment
rm(list = ls())
"Question 4: Exploratory Data Analysis"
# Set working directory to C:\Junk
setwd("C:/Junk")

# Read the churn data set into R environment
churn <- read.csv("churn_workbook.csv", header = TRUE, sep = ',')

# Check the first few values in churn data set to verify the data set is loaded correctly 
head(churn)
sort(churn$night_minutes)

" Exploratory analysis on Night Minutes column:"
# Find minimum
min(churn$night_minutes)
# Find maximum
max(churn$night_minutes)
# Find median
median(churn$night_minutes)
# Find the first quartile
quantile(churn$night_minutes)
# The first quartile contains values between 23.2 and 167.0 
# Plot a boxplot
b <- boxplot(churn$night_minutes)
# Plot the histogram 
hist(churn$night_minutes)
# Find outliers
out <- b$out
sort(out)
# There are 30 outliers in night minutes column

" Exploratory analysis on Day Minutes column:"
# Find minimum
min(churn$day_minutes)
# Find maximum
max(churn$day_minutes)
# Find median
median(churn$day_minutes)
# Find the first quartile
quantile(churn$day_minutes)
# The first quartile contains values between 0 and 143.7 
# Plot a boxplot
b <- boxplot(churn$day_minutes)
# Plot the histogram 
hist(churn$day_minutes)
# Find outliers
out <- b$out
sort(out)
# There are 25 outliers in day minutes column

"Create a two way table between International Plan and Voice Plan"
t <- table(InternationalPlan = churn[,'international_plan'], 
           VoicemailPlan = churn[,'voice_mail_plan'])
t

"Question 5:"

# 1. What is the probability of a randomly selected customer having both an international and a
# voice plan?
pBothplans <- t[2,2]/sum(t)
pBothplans <- 100 * pBothplans
# This probability is 2.76%


# 2. What is the probability of a randomly selected customer being a "churner" (churn=True.)?
churner <- churn[churn == 'True.']
pchurn <- 100 * (length(churner)/length(churn$churn))
pchurn
# This probability is 14.49%


# 3. Given a customer is a "churner" what is the probability of the customer having an 
#International plan?
# P(International Plan) | Customer is churner
t <- table(churn$international_plan, churn$churn)
p <- 100 * (t[2,2] / sum(t[,2]))
p
# This probability is 28.36%

# 4. Given a customer is a "churner" what is the probability of the customer having a 
#Voice plan'?

# P(Voice Plan) | Customer is churner
t <- table(churn$voice_mail_plan, churn$churn)
p <- 100 * (t[2,2] / sum(t[,2]))
p
# This probability is 16.56%

# 5. Are churning and having international plan independent?
t <- table(churn$international_plan, churn$churn)
pBoth <- t[2,2]/sum(t)
pBoth  # Probability of having international plan and curning


pchurn <- 483/ 3333 # Probability of churning
pinter = 323/3333 # Probability of international plan
p2 <- (pchurn * pinter) # Product of individual probabilities
p2
# Since the probability of having both International plan and churning is 0.041 and it is not equal to 
# the poduct of individual probabilities of International plan and Churning which is0.014, hence these events are dependent

