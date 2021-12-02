set.seed(100)
n = 13

# 10 Trials of sample size 13 (each row a trial)
X_orig = matrix(rnorm(n*10), nrow = 10)

# -----------------------------------------------------------------

# Probability Mass 3.6 (m_3_6)
m_3_6 = rep(-1,13)
for(i in 0:12) {
  m_3_6[i+1] = pbinom(6, 13, (i)/13) - pbinom(6,13, (i+1)/13)
}
m_3_6 = round(m_3_6, 4)

# Method 3.6
X = t(apply(X_orig, 1, sort))
med_X = X[,7] # the column of medians
X = X - med_X 
exp_val = X %*% as.matrix(m_3_6); print(round(exp_val, digits = 3))
print(paste0("Average: ", round(mean(exp_val), digits = 3)))
print(paste0("SD: ", round(sd(exp_val), digits = 3)))

# -----------------------------------------------------------------

# Probability Mass 3.10 (m_3_10)
m_3_10 = rep(1,25)
for(i in 0:24) {
  m_3_10[i+1] = pbinom(6, 13, (i)/25) - pbinom(6,13, (i+1)/25)
}
m_3_10[25] = 0
m_3_10 = round(m_3_10, 4)

# Setting up the symmetric form
X_3_10 = matrix(nrow = 10, ncol = 25)
small_X = t(apply(X_orig, 1, sort))[,-7]
X_3_10[,1:12] = small_X
X_3_10[,13] = med_X
X_3_10[,14:25] = 2*med_X - small_X
X_3_10 = t(apply(X_3_10, 1, sort))

X_3_10 = X_3_10 - med_X
exp_val = X_3_10 %*% as.matrix(m_3_10); print(exp_val)
print(paste0("Average: ", round(mean(exp_val), digits = 3)))
print(paste0("SD: ", round(sd(exp_val), digits = 3)))


# -----------------------------------------------------------------
set.seed(100)
n = 13

# 10 Trials of sample size 13 (each row a trial)
X_orig = matrix(rnorm(n*10), nrow = 10)

# Smoothed Bootstrap

#One trial so far
# I need 50 replicates of 13 data points
bootSample <- function(x, d, s_z) {
  x_star = rep(NA, length(x))
  c = 1/sqrt(1 + s_z) # value of c suggested in paper
  for(i in 1:length(x)) {
    z_i = runif(1, -d/2, d/2)
    x_star[i] = mean(x) + c * (sample(x,1) - mean(x) + sd(x) * z_i)
  }
  return(x_star)
}

d = c(0,0.25,0.5,1) # values for d
s_z = d^2 / 12 # variance of a Uniform(-d/2, d/2)

mean_estimates = matrix(nrow = 10, ncol = 4)
for (j in 1:10) { # number of trials
  x = X_orig[j,]
  med = median(x)
  sd = sd(x)
  for(k in 1:4) { # length of d 
    r_vec = rep(1,50)
    for (i in 1:50) { # N = 50 MC simulations
      temp = bootSample(x, d[k], s_z[k])
      r_vec[i] = abs(median(temp) - med) / sd
    }
    mean_estimates[j,k] = mean(r_vec)
  }
  
}

colnames(mean_estimates) = c("d = 0", "d = 0.25", "d = 0.5", "d = 1")
print(mean_estimates)
print("Average:"); print(colMeans(mean_estimates))
print("Standard Deviation:"); print(apply(mean_estimates, 2, sd))
