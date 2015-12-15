# A script for plotting a csv with 3 columns of numeric data.
# Plot is a heatmap, plotting Var 3 vs Var 1, Var 2.
# Var 1, Var 2 are binned prior to plotting.
# Does not assume Var 1, Var 2 are a unique row key- an aggregate function (mean) is run on each bin prior to plotting.

require(plotly) # Hipster plot library

setwd("H:/Documents/Software/R") # Change to wherever this script & data is

jdata <- read.csv("JeremyData.csv", sep=",") 

cuts <- 100 # Number of bins - 1

# Bin the MinCurrent and FixedCurrent
jdata$cMinCurrent <- cut(jdata$MinCurrent, cuts, labels = FALSE)
jdata$cFixedCurrent <- cut(jdata$FixedCurrent, cuts, labels = FALSE)

# Un-normalise bin labels 
jdata$cFixedCurrent <- jdata$cFixedCurrent / cuts * max(jdata$FixedCurrent)
jdata$cMinCurrent <- jdata$cMinCurrent /cuts * max(jdata$MinCurrent) 

# Compute mean Fitness for-each bin
jdata2 <- aggregate(Fitness ~ cMinCurrent + cFixedCurrent, jdata, mean )

# Crosstab view of the previous line (unused)
#plotme <- xtabs(formula = jdata2$Fitness ~ jdata2$cFixedCurrent + jdata2$cMinCurrent)


# Axis titles
x<-list(title="Min Current")
y<-list(title="Fixed Current")
Fitness <- jdata2$Fitness # Couldn't figure out how to change colour scale title...

plot_ly(y=jdata2$cFixedCurrent, x=jdata2$cMinCurrent, z=Fitness,type="heatmap") %>%
          layout(xaxis = x, yaxis = y)
 
