library(ggplot2)
library(dplyr)
library(reshape2)
#1.1
analytical_posterior <- rbeta(1000, 135, 67)
hist(analytical_posterior)
#1.2
y <- c(10, 15, 15, 14, 14, 14, 13, 11, 12, 16)
theta_grid <- seq(from=0, to=1, length=1000)
df.posterior <- data.frame(matrix(ncol=3, nrow=length(theta_grid)))
colnames(df.posterior) <- c('theta', 'lkl', 'prior')
for (i in 1:length(theta_grid)){
  lkl <- prod(dbinom(y, 20, theta_grid[i]))
  prior <- dbeta(theta_grid[i],1,1)
  df.posterior[i,] <- c(theta_grid[i], lkl, prior)
}
df.posterior$ML <- rep(sum(df.posterior$lkl*df.posterior$prior), 1000)
df.posterior <- df.posterior %>% 
  mutate(posterior = lkl*prior/ML)
plot(df.posterior$theta, df.posterior$posterior)
#1.3
df.mc <- data.frame(matrix(ncol=2, nrow=1000))
colnames(df.mc) <- c('theta_mc', 'lkl')
for (i in 1:1000){
  theta_i <- rbeta(1, 1, 1)
  lkl <- prod(dbinom(y, 20, theta_i))
  df.mc[i,] <- c(theta_i, lkl)
}
ML <- mean(df.mc$lkl)
ML
#1.4
proposed <- rbeta(10000, 5, 3)
df.importance <- data.frame(proposed=proposed)
df.importance$lkl <- NA
for (i in 1:10000){
  df.importance$lkl[i] <- prod(dbinom(y, 20, proposed[i]))
}
df.importance$prior <- dbeta(df.importance$proposed,1,1)
df.importance$proposal <- dbeta(df.importance$proposed, 5, 3)
df.importance$weights <- (df.importance$lkl)*
                         (df.importance$prior)/
                         (df.importance$proposal)
df.importance$weights <- df.importance$weights/sum(df.importance$weights)
importance_samples <- sample(proposed, size=2500, replace=TRUE,
                            prob=df.importance$weights)
hist(importance_samples)
#1.5
nsamp <- 10000
theta_chain <- rep(NA, nsamp)
theta_chain[1] <- rbeta(1, 1, 1)
i <- 1
step <- 0.08
while (i < nsamp){
  proposal_theta <- rnorm(1, theta_chain[i], step)
  if (proposal_theta>0 & proposal_theta<1){
    post_new <- prod(dbinom(y, 20, proposal_theta))*
                dbeta(proposal_theta, 1, 1)
    post_prev <- prod(dbinom(y, 20, theta_chain[i]))*
                 dbeta(theta_chain[i], 1, 1)
    hastings_ratio <- (post_new*dnorm(theta_chain[i], proposal_theta, step))/
                      (post_prev*dnorm(proposal_theta, theta_chain[i], step))
    p_str <- min(hastings_ratio, 1)
    if (p_str > runif(1,0,1)){
      theta_chain[i+1] <- proposal_theta
      i <- i+1
    }
  }
}
hist(theta_chain)

#1.6
posteriors <- data.frame(analytical_posterior, importance_samples, theta_chain)
ggplot(melt(posteriors), aes(x=value,colour=variable))+
  geom_density(linewidth=1.2) + theme_bw() + xlab("theta") +
  theme(legend.title=element_blank(), legend.position = "right")
  
