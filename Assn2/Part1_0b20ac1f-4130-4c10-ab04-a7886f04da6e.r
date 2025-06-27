library(ggplot2)
#1.1
posterior_0.75 <- 11*prod(dbinom(x=7, size=10, prob=0.75))
posterior_0.25 <- 11*prod(dbinom(x=7, size=10, prob=0.25))
posterior_1 <- 11*prod(dbinom(x=7, size=10, prob=1))
cat("1.1) a) Posterior at 0.75:", posterior_0.75, "\n")
cat("1.1) b) Posterior at 0.25:", posterior_0.25, "\n")
cat("1.1) c) Posterior at 1.00:", posterior_1, "\n")
#1.2
theta <- seq(from=0, to=1, by=0.01)
posterior <- data.frame(theta=theta)
posterior$pst <- NA
for (i in 1:length(theta)){
  posterior$pst[i] <- 11*prod(dbinom(x=7, size=10, prob=theta[i]))
}
ggplot(posterior, aes(x=theta, y=pst))+geom_line(linewidth=1, color="red")+
  theme_bw() + xlab(expression(theta)) + ylab("Posterior Distribution")
#1.3
max_index <- which.max(posterior$pst)
theta_max <- posterior$theta[max_index]
ggplot(posterior, aes(x=theta, y=pst))+geom_line(linewidth=1, color="red")+
  theme_bw()+xlab(expression(theta))+ ylab("Posterior Distribution")+
  geom_vline(xintercept=theta_max, color ='orange',linetype='dashed')+
  annotate("text", x=theta_max, y=max(posterior$pst), label=paste("theta ==",
      round(theta_max, 2)), parse=TRUE, vjust = 0, hjust = 1.1, color = 'blue')
#1.4
posterior$prior <- 1
posterior$lkl <- NA
for (i in 1:length(theta)){
  posterior$lkl[i] <- prod(dbinom(x=7, size=10, prob=theta[i]))
}
df.lkl_prior_pst <- melt(posterior, id=c('theta'))
df.lkl_prior_pst$variable <- ifelse(df.lkl_prior_pst$variable=="lkl",
                                    "Likelihood", 
                                    ifelse(df.lkl_prior_pst$variable=='pst',
                                           'Posterior', 'Prior'))
ggplot(df.lkl_prior_pst, aes(x=theta, y=value, color=variable)) + geom_line(size=1)+
  theme_bw() + xlab(expression(theta)) + ylab("") +
  scale_x_continuous(limits = c(0,1))+
  facet_wrap(~variable, scales='free_y', ncol=1) +
  scale_color_manual(values=c("blue", "orange", "red"))

