# ============================================================
# Simulated ESM Data
# ============================================================
# Creates a data set with the same structure as example_esm.csv:
# 21 days x 5 beeps = 105 rows, 7 columns, all ratings 0-100.

set.seed(123)  # makes the simulation reproducible

n_days  <- 21
n_beeps <- 5
n       <- n_days * n_beeps

# Day and beep columns
day  <- rep(1:n_days, each = n_beeps)
beep <- rep(1:n_beeps, times = n_days)

# Helper: keep values between 0 and 100 and round to whole numbers
clamp <- function(x) round(pmin(pmax(x, 0), 100))

# Depressed: has a temporal pattern (autoregressive, AR(1)).
# Each beep depends on the previous beep (0.7 = strength of carry-over),
# so forecasting models should be able to pick this up.
depressed    <- numeric(n)
depressed[1] <- 10
for (t in 2:n) {
  depressed[t] <- 0.7 * depressed[t - 1] +
    0.3 * runif(1, 0, 30) +
    rnorm(1, 0, 5)
  depressed[t] <- min(max(depressed[t], 0), 100)  # stay within 0-100
}

# Anxious: mostly low, with occasional spikes (about 10%)
anxious_spike <- rbinom(n, 1, 0.10)
anxious <- ifelse(anxious_spike == 1,
                  runif(n, 30, 85),
                  rexp(n, rate = 1 / 4))

# Stressed: spread out across the scale, a bit higher when more depressed
stressed <- runif(n, 0, 80) + 0.5 * depressed

# Affect valence: high on average, lower when depressed/anxious/stressed
affect_valence <- 92 - 0.6 * depressed - 0.2 * anxious - 0.1 * stressed +
  rnorm(n, 0, 8)

# Life satisfaction: closely follows affect valence
life_satisfaction <- affect_valence + rnorm(n, 0, 8)

# Combine into a data frame
esm_data <- data.frame(
  day               = day,
  beep              = beep,
  depressed         = clamp(depressed),
  anxious           = clamp(anxious),
  stressed          = clamp(stressed),
  affect_valence    = clamp(affect_valence),
  life_satisfaction = clamp(life_satisfaction)
)

# Look at the result
head(esm_data)
summary(esm_data)

# Save as CSV in the same format as the example (ratings written as 0.0, 66.0, ...)
esm_out <- esm_data
rating_cols <- c("depressed", "anxious", "stressed",
                 "affect_valence", "life_satisfaction")
esm_out[rating_cols] <- lapply(esm_out[rating_cols], function(x) sprintf("%.1f", x))

# Saved into the test_data folder (run this script from the repository root)
write.csv(esm_out, "test_data/simulated_esm.csv", row.names = FALSE, quote = FALSE)

# Quick check: lag-1 autocorrelation of depressed (should be clearly above 0)
acf(esm_data$depressed, lag.max = 1, plot = FALSE)