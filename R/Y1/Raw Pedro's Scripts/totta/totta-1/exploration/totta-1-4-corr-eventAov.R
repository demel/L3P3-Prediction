library(dplyr)


noDates <- function(.data)
{# remove columns with single-level factors. Used for anova
  select(.data, laply(.data,function(c){class(c)[1]!="POSIXct"}))
}
#to check table classes
# laply(evTableAov1,function(c){class(c)[1]})

noSingleLevelColumns <- function(.data)
{# remove columns with single-level factors. Used for anova
    # first we need to refactor all columns
  #TODO: reimplement this using t<-table(x); t>0
  refactor<-function(col)
  {if (is.factor(col)) factor(col)
   else col}
  .data <-colwise(refactor)(.data)  
  select(.data, laply(.data,function(c){length(table(c))>1}))
}
#to check table lengths
# laply(evTableAov1,function(c){length(table(c))})


evTableAov<-tbl_df(eventTable)
evTableAov<- evTableAov %.% 
  mutate (respTime=as.numeric(clearedOn-date), isCleared=!is.na(clearedOn)) %.% 
  noDates() %.% noSingleLevelColumns()  # remove date columns 

evTableAov1 <- evTableAov %.%
  filter(isCleared)  %.%
  noSingleLevelColumns()      # remove single-level cols
#NOTE: this op can't be chained with previous ones!

#summary(aov(respTime~severity+type+node+clearedBy,evTableAov))
summary(aov(respTime~.,evTableAov1))
summary(aov(isCleared~.,evTableAov %.% select(-respTime)))
evTableAov<- evTableAov %.% mutate (respTime=clearedOn-date)