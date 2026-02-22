library(ggplot2)
library(dplyr)

raw <- read.csv('position.csv',header=TRUE)
data <- tibble(raw)

ypred <- function(t_s){
  -4.91*t_s^2 + 4.97
}

# make figure 2 a plot of position vs time
fig <- ggplot(data) +
    geom_point(aes(x=t_s,y=y_m,shape=type)) +
    geom_function(fun=ypred,color='blue') + 
    ylab('$y$, \\unit{\\meter}') +
    xlab('$t$, \\unit{\\second}') +
    theme_bw(base_size=8) +
    theme(legend.position="inside",
	legend.position.inside=c(0.95,0.95),
	legend.justification.inside=c("right","top"),
	legend.key.size=unit(4,"pt"),
	legend.title=element_blank())
ggsave('fig2.svg',plot=fig,width=3.4167,height=2,units="in")
    


# calculate model stuff
bloop <- mutate(data,tsquared=t_s^2)
model1 <- lm(y_m~tsquared,bloop)
model2 <- lm(y_m~tsquared:type,bloop)
print(anova(model1,model2))

# give groupwise anyway
cricket <- filter(bloop,type=='cricket')
tennis <- filter(bloop,type=='tennis')
pong <- filter(bloop,type=='pong')

print(summary(lm(y_m~tsquared,cricket)))
print(summary(lm(y_m~tsquared,tennis)))
print(summary(lm(y_m~tsquared,pong)))
