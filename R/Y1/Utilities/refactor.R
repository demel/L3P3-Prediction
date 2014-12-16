library(plyr)
refactor<-function(data)
{colwise(function(col) {if (is.factor(col)) factor(col)  else col }) (data)
}