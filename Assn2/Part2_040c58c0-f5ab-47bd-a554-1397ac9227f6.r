library(ggplot2)
#2.1
y <- c(300, 270, 390, 450, 500, 290, 680, 450)
pst_300 <- prod(dnorm(y, 300, sd=50))*dnorm(300, mean = 250, sd = 50)
pst_900 <- prod(dnorm(y, 900, sd=50))*dnorm(900, mean = 250, sd = 50)
pst_50 <- prod(dnorm(y, 50, sd=50))*dnorm(50, mean = 250, sd = 50)
cat("Unnormalized Posterior at 300:", pst_300, "\n")
cat("Unnormalized Posterior at 900:", pst_900, "\n")
cat("Unnormalized Posterior at 50:", pst_50, "\n")
#2.2
mu <- seq(from=0, to=500, by=0.1)
posterior <- data.frame(mu=mu)
posterior$pst <- NA
posterior$prior <- NA
for (i in 1:length(mu)){
  posterior$prior[i] <- dnorm(mu[i], mean=250, sd=50)
}
for (i in 1:length(mu)){
  posterior$pst[i] <- prod(dnorm(y, mu[i], sd=50))*posterior$prior[i]
}
ggplot(posterior, aes(x=mu, y=pst))+geom_line(size=1,color='red')+
  theme_bw()+xlab(expression(mu))+ylab("Unnormalized Posterior Disrtribution")+
  scale_x_continuous(limits=c(300, 500))
#2.3
df.pst_prior = melt(posterior, id=c('mu'))
df.pst_prior$variable <- ifelse(df.pst_prior$variable=="prior",'Prior','Posterior')
ggplot(df.pst_prior, aes(x=mu, y=value, color=variable))+geom_line(linewidth=1)+
  theme_bw()+xlab(expression(mu))+ylab("")+
  scale_x_continuous(limits=c(0,500))+
  facet_wrap(~variable, scales='free_y',ncol=1)+
  scale_color_manual(values=c('blue','orange'))
