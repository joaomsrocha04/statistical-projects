############################################################
# Statistical Inference Project
# Topic: Log-concave density estimation and two-sample comparison
# Dataset: diamonds (ggplot2)
# Author: João Rocha
############################################################

# ==========================================================
# 1. Packages and setup
# ==========================================================
pkgs <- c("logcondens", "ggplot2")
new <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new)

library(logcondens)
library(ggplot2)

set.seed(123)

if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("outputs")) dir.create("outputs")

# ==========================================================
# 2. Data preparation
# ==========================================================
data(diamonds)

dat <- subset(diamonds, cut %in% c("Ideal", "Premium"))

x1 <- log(dat$price[dat$cut == "Ideal"])
x2 <- log(dat$price[dat$cut == "Premium"])

n1 <- length(x1)
n2 <- length(x2)

# ==========================================================
# 3. Exploratory analysis
# ==========================================================
png("figures/fig1_histograms.png", width = 800, height = 350)
par(mfrow = c(1, 2))
hist(x1, prob = TRUE, col = "gray85", border = "white",
     main = "Ideal cut (log-price)", xlab = "log(price)")
hist(x2, prob = TRUE, col = "gray85", border = "white",
     main = "Premium cut (log-price)", xlab = "log(price)")
dev.off()

png("figures/fig2_ecdf.png", width = 500, height = 400)
plot(ecdf(x1), col = "blue", lwd = 2,
     main = "Empirical CDFs of log(price)",
     xlab = "log(price)", ylab = "ECDF")
lines(ecdf(x2), col = "red", lwd = 2)
legend("bottomright", c("Ideal", "Premium"),
       col = c("blue", "red"), lwd = 2)
dev.off()

# ==========================================================
# 4. Density estimation (SAFE PLOTTING)
# ==========================================================
lc1 <- logConDens(x1)
lc2 <- logConDens(x2)

kde1 <- density(x1)
kde2 <- density(x2)

# Extract finite ranges
ymax1 <- max(kde1$y)
ymax2 <- max(kde2$y)

png("figures/fig3_density_comparison.png", width = 800, height = 350)
par(mfrow = c(1, 2))

plot(kde1, lwd = 2, col = "black",
     main = "Ideal cut",
     xlab = "log(price)", ylab = "Density",
     ylim = c(0, ymax1))
lines(lc1$x, exp(lc1$phi), lwd = 2, col = "blue")
legend("topright", c("KDE", "Log-concave"),
       lwd = 2, col = c("black", "blue"))

plot(kde2, lwd = 2, col = "black",
     main = "Premium cut",
     xlab = "log(price)", ylab = "Density",
     ylim = c(0, ymax2))
lines(lc2$x, exp(lc2$phi), lwd = 2, col = "red")
legend("topright", c("KDE", "Log-concave"),
       lwd = 2, col = c("black", "red"))

dev.off()

# ==========================================================
# 5. Two-sample inference
# ==========================================================
ks_res <- ks.test(x1, x2)
wilcox_res <- wilcox.test(x1, x2)

fmt_p <- function(p) {
  if (p < 2.2e-16) "< 2.2e-16" else formatC(p, format = "e", digits = 2)
}

sink("outputs/two_sample_tests.txt")
cat("Two-sample inference on log(price)\n\n")
cat("Sample sizes:", n1, "and", n2, "\n\n")

cat("Kolmogorov–Smirnov test\n")
cat("Statistic:", round(ks_res$statistic, 4), "\n")
cat("p-value:", fmt_p(ks_res$p.value), "\n\n")

cat("Wilcoxon rank-sum test\n")
cat("Statistic:", wilcox_res$statistic, "\n")
cat("p-value:", fmt_p(wilcox_res$p.value), "\n")
sink()

# ==========================================================
# 6. Summary statistics
# ==========================================================
summary_stats <- data.frame(
  Group = c("Ideal", "Premium"),
  Mean = c(mean(x1), mean(x2)),
  Median = c(median(x1), median(x2)),
  SD = c(sd(x1), sd(x2))
)

write.csv(summary_stats,
          "outputs/summary_statistics.csv",
          row.names = FALSE)

# ==========================================================
# 7. Console summary
# ==========================================================
cat("\n================ SUMMARY ================\n")
cat("Groups: Ideal vs Premium\n")
cat("Sample sizes:", n1, "and", n2, "\n")
cat("Mean log-prices:",
    round(mean(x1), 3), "vs",
    round(mean(x2), 3), "\n")
cat("KS p-value:", fmt_p(ks_res$p.value), "\n")
cat("Wilcoxon p-value:", fmt_p(wilcox_res$p.value), "\n")
cat("========================================\n")

