setwd("/Users/jarrettphillips/desktop/HACSim Simulation Study Paper/Supplemental Information/p = 0.80/hypothetical species")

out <- read.table(file.choose(), sep = ",") # read in data

# large-sample CI based on CLT

ci.low <- out$V2 - qnorm(0.975) * out$V3 # lower CI limit
ci.up <- out$V2 + qnorm(0.975) * out$V3 # upper CI limit

ci <- cbind(ci.low, ci.up)

count <- 0 # initialize counter

for (i in 1:nrow(ci)) {
  if ((ci[i, 2] >= 0.90) || ((ci[i, 1] <= 0.90) && (ci[i, 2] >= 0.90))) { # Is p = 0.95 in or above the CI  (including the endpoints)?
    count = count + 1 # update counter
  }
}

(coverage <- count / nrow(ci)) # coverage probability

binom.test(count, nrow(ci), p = 0.90, conf.level = 0.95) # Exact binomial test using Clopper-Pearson CI
