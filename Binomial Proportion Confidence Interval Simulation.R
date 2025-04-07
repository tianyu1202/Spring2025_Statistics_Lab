# Set seed for reproducibility
set.seed(123)

# Simulation parameters
n <- 60           # Sample size
p <- 0.12         # True probability
alpha <- 0.10     # Significance level for 90% Confidence Interval, alpha = 1 - 0.90
kappa <- qnorm(1 - alpha / 2)  # Z-value from standard normal distribution for the confidence interval
simulations <- 100000  # Number of simulations

# Initialize variables to record coverage
wald_cover <- 0
wilson_cover <- 0

# Run simulations
for (i in 1:simulations) {
  X <- rbinom(1, n, p)  # Sample from binomial distribution
  p_hat <- X / n        # Sample proportion
  q_hat <- 1 - p_hat    # 1 - sample proportion
  
  # Calculate Wald Confidence Interval
  wald_se <- sqrt(p_hat * q_hat / n)  # Standard error
  wald_lower <- p_hat - kappa * wald_se  # Wald lower bound
  wald_upper <- p_hat + kappa * wald_se  # Wald upper bound
  
  # Calculate Wilson Confidence Interval
  wilson_center <- (X + kappa^2 / 2) / (n + kappa^2)  # Center of Wilson interval
  wilson_margin <- kappa * sqrt((p_hat * q_hat + kappa^2 / (4 * n)) / n) / (1 + kappa^2 / n)  # Margin of Wilson interval
  wilson_lower <- wilson_center - wilson_margin  # Wilson lower bound
  wilson_upper <- wilson_center + wilson_margin  # Wilson upper bound
  
  # Check if true probability p is within the confidence intervals
  if (wald_lower <= p && p <= wald_upper) {
    wald_cover <- wald_cover + 1  # Count if Wald interval covers the true value
  }
  if (wilson_lower <= p && p <= wilson_upper) {
    wilson_cover <- wilson_cover + 1  # Count if Wilson interval covers the true value
  }
}

# Calculate and print empirical coverage of each interval
wald_coverage <- wald_cover / simulations
wilson_coverage <- wilson_cover / simulations

cat("Empirical coverage of Wald interval:", wald_coverage, "\n")
cat("Empirical coverage of Wilson interval:", wilson_coverage, "\n")
