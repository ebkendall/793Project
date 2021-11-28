set.seed(100)
n = 13

est_exp = matrix(nrow=10,ncol=2)

# Probability Mass 3.6 (m_3_6)
m_3_6 = rep(-1,13)
for(i in 0:12) {
  m_3_6[i+1] = pbinom(6, 13, (i)/13) - pbinom(6,13, (i+1)/13)
}
m_3_6 = round(m_3_6, 4)

# Probability Mass 3.10 (m_3_10)
m_3_10 = rep(1,26)
for(i in 0:25) {
  m_3_10[i+1] = pbinom(6, 13, (i)/25) - pbinom(6,13, (i+1)/25)
}
m_3_10[26] = 0
m_3_10 = round(m_3_10, 4)


for (j in 1:10) {
  
  X = matrix(rnorm(n*10), nrow = 10)
  
  # Method 3.6
  # Fix the Trial j sample and then bootstrap sample 50 times
  temp = rep(0,50)
  for(k in 1:50) {
    ind = sort(sample(1:13, replace = T, prob = rep(1/13,13)))
    x = X[j,ind]
    x_m = median(x) 
    for(l in 1:13) {
      temp[k] = temp[k] + (x[l] - x_m)^2 * m_3_6[ind[l]]
    }
  }
  est_exp[j,1] = mean(temp)
  
  # Method 3.10
  temp = rep(0,50)
  # making the symmetric distribution about the median
  d_x = sort(X[j,])
  d_x_med = d_x[7]
  d_x = d_x[-7]
  d_x = sort(c(d_x, 2*d_x_med - d_x, d_x_med))
  
  for(k in 1:50) {
    ind = sort(sample(1:25, replace = T, prob = rep(1/25,25)))
    #x = sort(sample(d_x, replace = T, prob = rep(1/25,25)))
    x = d_x[ind]
    x_m = median(x)
    for(l in 1:25) {
      temp[k] = temp[k] + (x[l] - x_m)^2 * m_3_10[ind[l]]
    }
  }
  est_exp[j,2] = mean(temp)

}

# 
# temp = rep(0,50)
# for(k in 1:50) {
#   x = sort(sample(X[j,], replace = T, prob = rep(1/13,13)))
#   x_m = median(x) 
#   for(l in 1:13) {
#     temp[k] = temp[k] + (x[l] - x_m)^2 * m_3_6[l]
#   }
# }
# est_exp[j,1] = mean(temp)




