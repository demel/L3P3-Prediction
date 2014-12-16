removeNAs.factor <- function (f,replacement="NA")
{
  f.str<-as.character(f)
  f.str[is.na(f.str)]<-replacement
  return(as.factor(f.str))
}


removeNAs.num <- function (n, replacement=0)
{
  n[is.na(n)]<-replacement
  n
}

removeNAs.string <-function(n, replacement="")
{
  n[is.na(n)]<-replacement
  n
}
  
removeNAs.df <- function(df, replace.num=0, replace.fac="NA", replace.string="")
{
 colwise(function(c)
  {
    c.class<-class(c)[1]
    switch(c.class,
           numeric=removeNAs.num(c,replace.num),
           character=removeNAs.string(c,replace.string),
           factor=removeNAs.factor(c,replace.fac),
           c)
    }) (df)
}


#tmpf<-sample(tmp$type,200)

