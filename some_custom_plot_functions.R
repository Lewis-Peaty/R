#####------- Some Parametrised Plot Functions ------####
# These functions are pre-customised GGPlots to deal with recurring tasks such
# as plotting dates or ages.
#
# There is also an example of the function calls required to save plots

############-------------- 1.0 Imports ----------------###########
library(ggplot2)
library(scales)

############-------------- 2.0 Saving your plots ----------------###########
# Change value to save plots in working directory
save_plots <- TRUE
if(save_plots){ splot <- function(fn) {png(filename = fn,
                                           width =1000,
                                           height = 480,
                                           units = "px"
)} } else { splot <- function(fn) {print("Saving plots deactivated")}}

#Set filetype - remember to update the previous functions also...
filetype <- "png"

#Example of saving a plot:
dplot <- as.data.frame(table(cut.Date(as.Date(d$Date_Installed), seq.Date(as.Date(X_AXES_LOWER_LIMIT),as.Date(X_AXES_UPPER_LIMIT),by = "year"))))
tstring <- "AC Install Dates"
splot(fn = paste(sep="",filetype," ",tstring, ".",filetype))
wp_date_plot(dplot, tstring = tstring)


#################-------------- 4.0 Custom Plot Functions -------------################

#################-------------- 4.1 Parametrised date bar plot -------------################
wp_date_plot <- function(df,tstring) {
  t <- paste(tstring, " [ n =", as.character(sum(df$Freq)), "]")
  ggplot(df, aes(x = as.Date(Var1), y = Freq)) +
    scale_x_date(labels = date_format("%Y"),limits=as.Date(c(X_AXES_LOWER_LIMIT,X_AXES_UPPER_LIMIT))) +
    labs(title = t, y = "Count", x = "Date") +
    theme(plot.title = element_text(size = rel(3)),  #size = 2x base font
          axis.text = element_text( angle=45, colour = "black", size = 24),
          axis.title = element_text(colour = "black", size = 30),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "coral")) +  
    geom_bar(stat = "identity", colour = "beige", fill = "brown3")
}

#################-------------- 4.2 Parametrised bar age plot -------------################
# 2.0 Parametrised age plot function.
#Expects x axis to be NUMERIC
wp_age_plot <- function(df,tstring) {
  t <- paste(tstring, " [ n =", as.character(sum(df$Freq)), "]")
  ggplot(df, aes(x = Var1, y = Freq)) +
    scale_x_continuous(breaks = (seq(0,101,10))) + 
    labs(title = t, y = "Count", x = "Age") +
    theme(plot.title = element_text(size = rel(3)),  #size = 2x base font
          axis.text = element_text( angle=45, colour = "black", size = 24),
          axis.title = element_text(colour = "black", size = 30),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "coral")) +  
    geom_bar(stat = "identity", colour = "beige", fill = "brown3")
}

#################-------------- 4.3 Parametrised age line plot with multiple series overlay -------------################
# 3.0 .
#Expects x axis to be NUMERIC
#Beware of attempting to use "fill"...
wp_age_overlay_plot <- function(df,tstring) {
  t <- paste(tstring)
  ggplot(df, aes(x = Age, y =value, colour=variable)) +
    scale_x_continuous(breaks = (seq(0,101,10))) + 
    labs(title = t, y = "Count", x = "Age") +
    theme(plot.title = element_text(size = rel(3)),  #size = 2x base font
          axis.text = element_text( angle=45, colour = "black", size = 24),
          axis.title = element_text(colour = "black", size = 30),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "coral")) +  
    geom_line(stat = "identity", size=1.2)
}