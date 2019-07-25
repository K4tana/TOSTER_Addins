# eqplot is a tool to plot multiple EQ tests. 
# Parameters: xval: a character vector to define x-axis labels for the different plots you're comparing, mostly for categories, 
# should the need arise. The default of xval is set to NULL. Any element created by eqplot can be modified with 
# ggplot2 modifiers. eqplot therefore provides a rough framework that displays a simple 
#but effective way of presenting multiple equivalence tests
#------------------------------------------------------------------------------------------
#input. 
library(TOSTER)
L1<- TOSTtwo.raw(m1=63965.83, m2=65799.17, sd1=17987.89,sd2=17971.16, n1=706, n2=706, low_eqbound = -3198 , high_eqbound = 3198 ,
                      alpha=0.05, var.equal = F, plot = T, verbose = T)
L2 <- TOSTtwo.raw(m1=73289.51, m2=74920.41, sd1=19877.33,sd2=20626.45, n1=753, n2=753, low_eqbound = -3664 , high_eqbound = 3664 ,
                      alpha=0.05, var.equal = F, plot = T, verbose = T)
L3<- TOSTtwo.raw(m1=76024.44, m2=74147.40, sd1=20920.53,sd2=19614.33, n1=790, n2=790, low_eqbound = -3801 , high_eqbound = 3801 ,
                        alpha=0.05, var.equal = F, plot = T, verbose = T)
L4 <- TOSTtwo.raw(m1=74530.92, m2=73227.33, sd1=20264.43,sd2=19343.64, n1=300, n2=300, low_eqbound = -3726 , high_eqbound = 3726 ,
                       alpha=0.05, var.equal = F, plot = T, verbose = T)
#------------------------------------------------------------------------------------------
eqplot <- function(z,..., xval=NULL)
{ 
  options(scipen = 999)
  library(ggplot2)
  library(dplyr)
  if(is.data.frame(z)==F)
  {
    eqt <- eqtable(z,...) #in case multiple EQ test objects are used, calls eqtable
  }
  else
  {
    eqt <- z # if there already was a data frame.
  }
  if(is.null(xval)) #checking default values of xval
  {
    xval <- c(1:nrow(eqt))
    warning("No xval input detected. Using numeric x-axis labels (default)!")
  }
  eqt <- cbind(eqt,xval) #binding x-axis categories to eq-table.
  pl <- ggplot(eqt, aes(x=xval, y=diff))+ 
        geom_errorbar(data=eqt, aes(ymin= low_eqbound, ymax= high_eqbound), width=.2, color ='black',
                      position = position_dodge(width = 1), linetype=2, size=0.7)+
        geom_pointrange(mapping=aes(ymin=LL_CI_TOST, ymax=UL_CI_TOST), 
                        size=1, fill="white", shape=22, position= position_dodge(1), colour="blue")+
        geom_hline(yintercept=0) #creating the graphs
      pl+
        theme_bw()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) #standard ggplot2 modifiers.
      warning("This plot can be modified using any ggplot function or parameter. Consider tweaking the plot until you're satisfied!")
}
#----------------------------------------------------------------------------------------
eqplot(L1,L2, L3, L4, xval = c(1,2,3,4)) #
