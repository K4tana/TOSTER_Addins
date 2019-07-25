#Code for a possible TOSTER-addition.
#creates a ggplot friendly table for multiple equivalence test results out of either a vector of 
# EQ Test objects or single EQ test objects separated by a comma. 
# Single objects also possible. It basically transforms the list mode of eq test outputs into a data frame
 
eqtable <- function (x,...) 
{ 
  library(dplyr)
  options(scipen=10)
  f <- function(z)
    {
    A <- unlist(z, use.names = T) %>% matrix(ncol = 13, byrow=T) %>% data.frame(stringsAsFactors=FALSE)
    colnames(A) <- names(z[1:13])
    return(A)
    }
  if(nargs()>1)
    {
    if("TOST_df" %in% names(x))
    {
      A <- c(x,...)
      EQ_Table <- f(A)
    }
    else
    {
      stop("No equivalence test object detected")
    }
  }
  else
  {
    if("TOST_df" %in% names(x))
    {
      EQ_Table <- f(x)
    }
    else
    {
      stop("No equivalence test object detected")
    }
  }
}


