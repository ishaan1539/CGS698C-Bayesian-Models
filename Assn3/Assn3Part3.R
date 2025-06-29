true_mu <- 800
true_var <- 100 #sigma^2
y <- rnorm(500,mean=true_mu,sd=sqrt(true_var))
hist(y)
sigma <- sqrt(true_var)
mu <- true_mu

#Gradient functions
gradient <- function(mu,sigma,y,n,m,s,a,b){
  grad_mu <- (((n*mu)-sum(y))/(sigma^2))+((mu-m)/(s^2))
  grad_sigma <- (n/sigma)-(sum((y-mu)^2)/(sigma^3))+((sigma-a)/(b^2))
  return(c(grad_mu,grad_sigma))
}
#Potential energy function
V <- function(mu,sigma,y,n,m,s,a,b){
  nlpd <- -(sum(dnorm(y,mu,sigma,log=T))+dnorm(mu,m,s,log=T)+dnorm(sigma,a,b,log=T))
  nlpd
}
#HMC sampler
HMC <- function(y,n,m,s,a,b,step,L,initial_q,nsamp,nburn){
  mu_chain <- rep(NA,nsamp)
  sigma_chain <- rep(NA,nsamp)
  reject <- 0
  #Initialization of Markov chain
  mu_chain[1] <- initial_q[1]
  sigma_chain[1] <- initial_q[2]
  #Evolution of Markov chain
  i <- 1
  while(i < nsamp){
    q <- c(mu_chain[i],sigma_chain[i]) 
    p <- rnorm(length(q),0,1) # Current position of the particle
    # Generate random momentum at the current position
    current_q <- q
    current_p <- p
    current_V = V(current_q[1],current_q[2],y,n,m,s,a,b) # Current potential energy
    current_T = sum(current_p
                    ^2)/2 # Current kinetic energy
    # Take L leapfrog steps
    for(l in 1:L){
      # Change in momentum in 'step/2' time
      p <- p-((step/2)*gradient(q[1],q[2],y,n,m,s,a,b))
      # Change in position in 'step' time
      q <- q + step*p
      # Change in momentum in 'step/2' time
      p <- p-((step/2)*gradient(q[1],q[2],y,n,m,s,a,b))
    }
    proposed_q <- q
    proposed_p <- p
    proposed_V = V(proposed_q[1],proposed_q[2],y,n,m,s,a,b) # Proposed potential energy
    proposed_T = sum(proposed_p
                     ^2)/2 # Proposed kinetic energy
    accept.prob <- min(1,exp(current_V+current_T-proposed_V-proposed_T))
    # Accept/reject the proposed position q
    if(accept.prob>runif(1,0,1)){
      mu_chain[i+1] <- proposed_q[1]
      sigma_chain[i+1] <- proposed_q[2]
      i <- i+1
    }else{
      reject <- reject+1
    }
  }
  posteriors <- data.frame(mu_chain,sigma_chain)[-(1:nburn),]
  posteriors$sample_id <- 1:nrow(posteriors)
  posteriors
}

#3.1
df.posterior <- HMC(y=y,n=length(y), # data
                    m=400,s=5,a=10,b=2, # priors
                    step=0.02, # step-size
                    L=12, # no. of leapfrog steps
                    initial_q=c(1000,11), # Chain initialization
                    nsamp=6000, # total number of samples
                    nburn=2000) # number of burn-in samples
library(ggplot2)
ggplot(df.posterior[-(1:2000),],aes(x=mu_chain))+
  geom_density(linewidth=1.2)+theme_bw()+xlab(expression(mu))+
  geom_vline(xintercept=800,size=1.5,color="red")
ggplot(df.posterior[-(1:2000),],aes(x=sigma_chain))+
  geom_density(linewidth=1.2)+theme_bw()+xlab(expression(sigma))+
  geom_vline(xintercept=10,size=1.5,color="red")

#3.4
df.posterior <- HMC(y=y,n=length(y), # data
                    m=1000,s=20,a=10,b=2, # priors
                    step=0.02, # step-size
                    L=12, # no. of leapfrog steps
                    initial_q=c(1000,11), # Chain initialization
                    nsamp=6000, # total number of samples
                    nburn=2000) # number of burn-in samples
#Inspect Chains
df.posterior$id <- 1:4000
ggplot(df.posterior[-(1:2000),],aes(x=id,y=mu_chain))+
  geom_line(size=1.2,color="blue")+
  theme_bw()+xlab("mu chain")
ggplot(df.posterior[-(1:2000),],aes(x=id,y=sigma_chain))+
  geom_line(size=1.2,color="blue")+
  theme_bw()+xlab("sigma chain")



