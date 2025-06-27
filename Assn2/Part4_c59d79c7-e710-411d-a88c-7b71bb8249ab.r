library(ggplot2)

#4.5.1
mu <- rnorm(1000, 300, 60)
dat <- read.table(
  "https://raw.githubusercontent.com/yadavhimanshu059/CGS698C/main/notes/Module-2/recognition.csv",
  sep=",",header = T)[,-1]
posterior_null <- data.frame(mu=mu)
posterior_null$prior <- NA
for(i in 1:length(mu)){
  posterior_null$prior[i] <- dnorm(mu[i], 300, 50)
}
posterior_null$pst <- NA
for(i in 1:length(mu)){
  posterior_null$pst[i] <- prod(dnorm(dat$Tw,mean=mu[i],sd=60))*
                           prod(dnorm(dat$Tnw,mean=mu[i],sd=60))*
                           posterior_null$prior[i]
}
ggplot(posterior_null, aes(x=mu, y=pst)) + geom_line(size=1,color='red')+
         theme_bw() + xlab(expression(mu)) + 
         ylab("Unnormalized Posterior Distribution Null")
#4.5.2
library(truncnorm)
delta <- rtruncnorm(1000, a=0, b=Inf, mean=0, sd=50)
rt_word <- rnorm(1000 ,mean=mu, sd=50)
rt_nonword <- rnorm(1000, mean = mu+delta, sd=60)
df_lexical<- data.frame(
  RT = c(rt_word, rt_nonword),
  Type = rep(c("word", "nonword"), each=1000)
)
ggplot(df_lexical, aes(x=RT, fill=Type))+geom_histogram(binwidth = 20, alpha=0.7)+
  theme_bw() + facet_grid(~Type)
#4.5.3
df_lexical$Model <- "Lexical"
df_null <- data.frame(
  RT = c(rt_word, rt_word),
  Type = rep(c("word", "nonword"), each=1000),
  Model = rep("Null", 2000)
)
combined_df <- rbind(df_lexical, df_null)
ggplot(combined_df, aes(x=RT, fill=Type))+geom_histogram(binwidth = 20,alpha=0.7)+
  facet_grid(Type~Model) + theme_bw()
#4.5.4
df_obs <- data.frame(
  RT = c(dat$Tw, dat$Tnw),
  Type = rep(c("word", "nonword"), each=nrow(dat)),
  Model = "Observed"
)
df_all <- rbind(df_null, df_lexical, df_obs)
ggplot(df_all, aes(x=RT, fill=Model))+geom_histogram(binwidth = 20, alpha=0.7)+
  theme_minimal()+facet_wrap(Type~Model, scale="free")

#4.5.5


