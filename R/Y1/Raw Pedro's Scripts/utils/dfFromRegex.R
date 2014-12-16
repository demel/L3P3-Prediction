library(gsubfn)

dfFromRegex <- function(lines, regex)
{
  cat ("Regex: ",regex, "\n")
  cat("Data lines: ", length(lines), "\n")
  df<-strapplyc(lines, regex, simplify = "rbind")
  cat("Data frame dims: ", dim(df), "\n")
  df
}

dfFromRegexPerl <- function(lines, regex)
{
  cat ("Regex: ",regex, "\n")
  cat("Data lines: ", length(lines), "\n")
  df<-strapply(lines, regex, FUN="c", simplify = "rbind", perl=TRUE)
  cat("Data frame dims: ", dim(df), "\n")
  df
}

