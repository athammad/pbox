library(pbox)

### Load Data ###
data("SEAex",package = "pbox")
SEAex$Year<-c(1901:2022)
setnames(SEAex,"avgRegion","Region Mean")
SEAex_long <- melt(SEAex, id.vars = "Year", variable.name = "Country")

# Plot using ggplot with facets
p<-ggplot(SEAex_long, aes(x = Year, y = value, color = Country)) +
  geom_line(color = "black") +  # Set all lines to black
  labs(x = "Year", y = "Temperature °C") +
  ggtitle("") +
  facet_grid(Country ~ ., scales = "free_y") +
  theme(legend.position = "none",panel.spacing.y = unit(10, "pt")) +theme_bw()
ggsave(plot=p,"./Paper/Plots/ts.png", width = 20, height = 11, units = "cm")

### Table Descriptive Statistics ###
summary(SEAex)
stargazer::stargazer(SEAex,summary.stat=c('mean','SD','p25','median','p75',"min","max"),
                     digits=2,
                     title = "Summary statistics",
                     out="./Paper/Tables/sumstats.tex",label ='sumstats')
### Plot data ###


### Create PBOX obj ###
# Set pbox
pbx<-set_pbox(SEAex,verbose=FALSE)
print(pbx)


### Explore Porbability Space ###


#Get marginal distribution
pbx["Malaysia:33",]
#Get Joint distribution
pbx["Malaysia:33 & Vietnam:34",]
#Get Joint distribution
pbx["Vietnam:31", "avgRegion:26"]
#Conditional distribution Pr(X <= x, Y <= y) / Pr(Y <= y)
pbx["Malaysia:33 & Vietnam:31", "avgRegion:26"]
#Conditional distribution Pr(X <= x, Y <= y) / Pr(Y = y)
pbx["Malaysia:33 & Vietnam:31", "avgRegion:26",fixed=TRUE]
# Joint distribution with values set on their respective mean value
pbx["mean:c(Vietnam,Thailand)",lower.tail=T]
# Joint distribution with values set on their respective median value
pbx["median:c(Vietnam, Thailand)",lower.tail=T]
# Joint distribution with xxxx
pbx["Malaysia:33 & mean:c(Vietnam, Thailand)",lower.tail=T]
# Conditional distribution distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
pbx["Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)"]
