library(truncnorm)
library(ggplot2)
dat <- read.csv("/Users/ishaanjain/CGS698/CGS698/Assn3/word-recognition-times.csv",
                sep=",",header = T)[,-1]

#2.1
for (i in 1:4000){
  if (dat$type[i]=='word'){
    dat$type[i] = 0
  }else{
    dat$type[i] = 1
  }
}
dat$type <- as.numeric(dat$type)

nsamp <- 4000
alpha_chain <- rep(NA, nsamp)
beta_chain <- rep(NA, nsamp)

alpha_chain[1] <- rnorm(1, 400, 50)
beta_chain[1] <- rtruncnorm(n=1, a=0, b=Inf, mean=0, sd=50)

i <- 1
reject <- 0
step <- 0.1

while(i < nsamp){
  proposal_alpha <- rnorm(1, alpha_chain[i], step)
  proposal_beta <- rtruncnorm(1, a=0, b=Inf, mean=beta_chain[i], sd=step)
  
  mu_new <- proposal_alpha + proposal_beta*dat$type
  mu_old <- alpha_chain[i] + beta_chain[i]*dat$type
  
  post_new <- sum(dnorm(dat$RT, mu_new, 30, log=TRUE))+
    dnorm(proposal_alpha, 400, 50,log=TRUE)+
    log(dtruncnorm(x=proposal_beta, a=0, b=Inf,mean=0,sd=50))
  post_prev <- sum(dnorm(dat$RT, mu_old, 30,log=TRUE))+
    dnorm(alpha_chain[i], 400, 50, log=TRUE)+
    log(dtruncnorm(x=beta_chain[i], a=0, b=Inf, mean=0, sd=50))
  
  hastings_ratio <-
    exp((post_new+dnorm(alpha_chain[i],proposal_alpha,step,log=TRUE)+
           log(dtruncnorm(x=beta_chain[i],mean=proposal_beta,sd=step,a=0,b=Inf)))-
          (post_prev+dnorm(proposal_alpha,alpha_chain[i],step,log=TRUE)+
             log(dtruncnorm(x=proposal_beta,mean=beta_chain[i],sd=step,a=0,b=Inf))))
  p_str <- min(hastings_ratio, 1)
  if (p_str > runif(1,0,1)){
    alpha_chain[i+1] <- proposal_alpha
    beta_chain[i+1] <- proposal_beta
    i <- i+1
  }else{
    reject <- reject + 1
  }
}

posteriors <- data.frame(alpha_chain, beta_chain)
ggplot(posteriors[-(1:2000),], aes(x=alpha_chain))+
  theme_bw() + geom_density(linewidth=1.2) + xlab(expression(alpha))

ggplot(posteriors[-(1:2000),], aes(x=beta_chain))+
  theme_bw() + geom_density(linewidth=1.2) + xlab(expression(beta))

#2.2

quantile(alpha_chain, probs=c(0.025, 0.975))
quantile(beta_chain, probs=c(0.025, 0.975))

