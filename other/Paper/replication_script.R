library(pbox)
library(ggplot2)
library(data.table)
library(stargazer)
### Load Data ###
data("SEAex",package = "pbox")
SEAex$Year<-c(1901:2022)
#setnames(SEAex,"avgRegion","Region Mean")
SEAex_long <- melt(SEAex, id.vars = "Year", variable.name = "Country")

# Plot using ggplot with facets
p<-ggplot(SEAex_long, aes(x = Year, y = value, color = Country)) +
  geom_line(color = "black") +  # Set all lines to black
  labs(x = "Year", y = "Temperature °C") +
  ggtitle("") +
  facet_grid(Country ~ ., scales = "free_y") +
  theme(legend.position = "none",panel.spacing.y = unit(10, "pt")) +theme_bw()
ggsave(plot=p,"./other/Paper/Plots/ts.png", width = 20, height = 11, units = "cm")

### Table Descriptive Statistics ###
summary(SEAex)
stargazer::stargazer(SEAex,summary.stat=c('mean','SD','p25','median','p75',"min","max"),
                     digits=2,
                     title = "Summary statistics",
                     out="./Paper/Tables/sumstats.tex",label ='sumstats')


### Create PBOX obj ###
# Set pbox
data("SEAex",package = "pbox")
pbx<-set_pbox(SEAex)
print(pbx)


### Explore Probability Space ###
#Get marginal distribution
qpbox(pbx,"Malaysia:33")
#Get Joint distribution
qpbox(pbx,"Malaysia:33 & Vietnam:34")
#Get Joint distribution
qpbox(pbx,"Vietnam:31", "avgRegion:26")
#Conditional distribution Pr(X <= x, Y <= y) / Pr(Y <= y)
qpbox(pbx,"Malaysia:33 & Vietnam:31", "avgRegion:26")
#Conditional distribution Pr(X <= x, Y <= y) / Pr(Y = y)
qpbox(pbx,"Malaysia:33 & Vietnam:31", "avgRegion:26",fixed=TRUE)
# Joint distribution with values set on their respective mean value
qpbox(pbx,"mean:c(Vietnam,Thailand)",lower.tail=T)
# Joint distribution with values set on their respective median value
qpbox(pbx,"median:c(Vietnam, Thailand)",lower.tail=T)
# Joint distribution
qpbox(pbx,"Malaysia:33 & mean:c(Vietnam,Thailand)",lower.tail=T)
# Conditional distribution distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
qpbox(pbx,"Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)")
#CI
qpbox(pbx,"Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)", CI=TRUE,fixed=TRUE)


### Grid Search ###
grid_results<-grid_pbox(pbx, mj = c("Vietnam", "Malaysia"))
grid_results$probs
grid_results[which.max(grid_results$probs),]
grid_results[which.min(grid_results$probs),]



### Scenario Analysis ###
seq(-3,3,1)

sc_results<-scenario_pbox(pbx,mj = "Vietnam:31 & avgRegion:26",
                          param_list = list(Vietnam="mu"),range=seq(-3,3,1))

# Create a data frame
data <- data.frame(
  SD = names(sc_results),
  P = as.vector(unlist(sc_results))
)
data$SD <- factor(data$SD, levels = data$SD)
# Plot
p<-ggplot(data, aes(x = SD, y = P)) +
  geom_point() +
  geom_line(group = 1) +
  labs(title = "", x = "SD from the mean", y = "P(Vietnam:31 & avgRegion:26)") +
  theme_bw()
ggsave(plot=p,"./other/Paper/Plots/plotScen.png", width = 20, height = 11, units = "cm")


