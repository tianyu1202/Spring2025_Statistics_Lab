library(psych)

# (a) Reading the data and creating a histogram of the salaries
# Reading the data from the 'salary.txt' file
data <- read.table("SALARY.TXT", header = TRUE, stringsAsFactors = FALSE)

# Extracting the SALARY column
salaries <- data$SALARY

# Creating a histogram of the salaries
dev.new()
hist(salaries, main = "Histogram of Weekly Salaries", xlab = "Salaries", ylab = "Frequency")

# (b) Histogram with at least 15 intervals
# Determine the range of the salary data
min_salary <- min(salaries)
max_salary <- max(salaries)

# Create a sequence of break points with at least 15 intervals
break_points <- seq(from = min_salary, to = max_salary, length.out = 16)

# Creating a histogram with custom break points
dev.new()
hist(salaries, breaks = break_points, main = "Histogram of Weekly Salaries with Custom Breaks",
     xlab = "Salaries", ylab = "Frequency")

# (c) Boxplot of the salaries
dev.new()
boxplot(salaries, main = "Boxplot of Weekly Salaries", ylab = "Salaries")

# (d) Side-by-side boxplots of the salaries by gender
dev.new()
boxplot(SALARY ~ GENDER, data = data,
        main = "Boxplot of Weekly Salaries by Gender",
        xlab = "Gender", ylab = "Salaries",
        names = c("Female", "Male"))

# (e) Summary statistics for the whole group
overall_summary <- describe(data$SALARY)
print(overall_summary)

# Summary statistics for each gender
male_summary <- describe(data$SALARY[data$GENDER == "M"])
female_summary <- describe(data$SALARY[data$GENDER == "F"])

# Printing the summary statistics for male and female
print(male_summary)
print(female_summary)

# (f) Calculate the first and third quartiles
Q1 <- quantile(data$SALARY, 0.25)
Q3 <- quantile(data$SALARY, 0.75)

# Calculate the Interquartile Range (IQR)
IQR <- Q3 - Q1

# Determine the outlier boundaries
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Determine outliers
outliers <- data$SALARY[data$SALARY < lower_bound | data$SALARY > upper_bound]

# Display the results
cat("Lower Bound for Outliers:", lower_bound, "\n")
cat("Upper Bound for Outliers:", upper_bound, "\n")
cat("Outliers in the Salary Data:", outliers, "\n")
