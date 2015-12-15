# A script for plotting a csv with 3 columns of numeric data.
# Plot is a heatmap, plotting Var 3 vs Var 1, Var 2.
# Var 1, Var 2 are binned prior to plotting.
# Does not assume Var 1, Var 2 are a unique row key- an aggregate function (mean) is run on each bin prior to plotting.

require(plotly) # Hipster plot library

setwd("H:/Documents/Software/R") # Change to wherever this script & data is

jdata <- read.csv("JData.csv", sep=",") 

cuts <- 100 # Number of bins - 1

# Bin the Var1 and Var2
jdata$cVar1 <- cut(jdata$Var1, cuts, labels = FALSE)
jdata$cVar2 <- cut(jdata$Var2, cuts, labels = FALSE)

# Un-normalise bin labels 
jdata$cVar2 <- jdata$cVar2 / cuts * max(jdata$Var2)
jdata$cVar1 <- jdata$cVar1 /cuts * max(jdata$Var1) 

# Compute mean Var3 for-each bin
jdata2 <- aggregate(Var3 ~ cVar1 + cVar2, jdata, mean )

# Crosstab view of the previous line (unused)
#plotme <- xtabs(formula = jdata2$Var3 ~ jdata2$cVar2 + jdata2$cVar1)


# Axis titles
x<-list(title="Min Current")
y<-list(title="Fixed Current")
Var3 <- jdata2$Var3 # Couldn't figure out how to change colour scale title...

plot_ly(y=jdata2$cVar2, x=jdata2$cVar1, z=Var3,type="heatmap") %>%
          layout(xaxis = x, yaxis = y)
 
