library(ggplot2)

# Read the data
ischemic_hd <- read.csv("ischemic_hd.csv")

# Scatter plot: Duration of Angina vs. Time to Angina
ggplot(ischemic_hd, aes(x = time, y = duration)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Scatter Plot of Duration of Angina vs. Time to Angina",
       x = "Time to Angina (seconds)",
       y = "Duration of Angina (seconds)") +
  theme_minimal()

# Scatter plot with regression line
ggplot(ischemic_hd, aes(x = time, y = duration)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Scatter Plot with Regression Line",
       x = "Time to Angina (seconds)",
       y = "Duration of Angina (seconds)") +
  theme_minimal()

# Pearson correlation test
cor.test(ischemic_hd$time, ischemic_hd$duration, method = "pearson")

# Linear regression analysis
model <- lm(duration ~ time, data = ischemic_hd)
summary(model)

# Histogram: Distribution of Time to Angina
ggplot(ischemic_hd, aes(x = time)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Time to Angina", x = "Time to Angina (seconds)", y = "Frequency")

# Histogram: Distribution of Duration of Angina
ggplot(ischemic_hd, aes(x = duration)) +
  geom_histogram(binwidth = 50, fill = "lightcoral", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Duration of Angina", x = "Duration of Angina (seconds)", y = "Frequency")

# Additional correlation analysis with permutation tests

# Observed Pearson and Spearman correlations
obs_pearson <- cor(ischemic_hd$time, ischemic_hd$duration, method = "pearson")
obs_spearman <- cor(ischemic_hd$time, ischemic_hd$duration, method = "spearman")

# Number of permutations
n_permutations <- 10000

# Initialize vectors to store permutation results
pearson_permutations <- numeric(n_permutations)
spearman_permutations <- numeric(n_permutations)

# Permutation Test for Pearson correlation
set.seed(123) # Set seed for reproducibility
for (i in 1:n_permutations) {
  # Permute the "time" variable
  permuted_time <- sample(ischemic_hd$time)
  # Calculate the Pearson correlation for the permuted data
  pearson_permutations[i] <- cor(permuted_time, ischemic_hd$duration, method = "pearson")
}

# Permutation Test for Spearman correlation
set.seed(123) # Set seed for reproducibility
for (i in 1:n_permutations) {
  # Permute the "time" variable
  permuted_time <- sample(ischemic_hd$time)
  # Calculate the Spearman correlation for the permuted data
  spearman_permutations[i] <- cor(permuted_time, ischemic_hd$duration, method = "spearman")
}

# Calculate p-values
pearson_p_value <- mean(abs(pearson_permutations) >= abs(obs_pearson))
spearman_p_value <- mean(abs(spearman_permutations) >= abs(obs_spearman))

# Print results
cat("Observed Pearson correlation:", obs_pearson, "\n")
cat("Observed Spearman correlation:", obs_spearman, "\n")
cat("Permutation p-value for Pearson:", pearson_p_value, "\n")
cat("Permutation p-value for Spearman:", spearman_p_value, "\n")
