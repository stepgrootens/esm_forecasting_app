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

# Depressed: mostly low, with occasional "bad moments" (about 20%)
bad_moment <- rbinom(n, 1, 0.20)
depressed  <- ifelse(bad_moment == 1,
                     runif(n, 35, 90),
                     rexp(n, rate = 1 / 6))

# Anxious: mostly low, with occasional spikes (about 10%)
anxious_spike <- rbinom(n, 1, 0.10)
anxious <- ifelse(anxious_spike == 1,
                  runif(n, 30, 85),
                  rexp(n, rate = 1 / 4))

# Stressed: spread out across the scale, a bit higher during bad moments
stressed <- runif(n, 0, 80) + 10 * bad_moment

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

write.csv(esm_out, "test_data/simulated_esm.csv", row.names = FALSE, quote = FALSE)