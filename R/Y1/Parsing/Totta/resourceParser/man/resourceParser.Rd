\name{resourceParser}
\alias{resourceParser}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
resourceParser
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
resourceParser(resource)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resource}{
%%     ~~Describe \code{resource} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (resource) 
{
    if (is.null(resource) | length(resource) < 2) {
        return(data.frame())
    }
    breaks <- cut(1:length(resource), which(nchar(resource) == 
        0))
    splitResource <- split(resource, breaks)
    dfList <- llply(splitResource, csvToDf, skip = 1)
    dfList <- dfList[-1]
    dfList <- dfList[laply(dfList, is.data.frame)]
    dfListDims <- laply(dfList, dim)
    if (!is.null(dim(dfListDims))) {
        dfList <- dfList[dfListDims[, 1] != 0]
    }
    else {
        return(data.frame())
    }
    prevNameList <- list(0)
    for (i in 1:length(dfList)) {
        nameList <- laply(strsplit(names(dfList[[i]][-1]), " "), 
            tail, n = 1)
        for (name in nameList) {
            if (any(prevNameList == name)) {
                name <- paste(name, i, sep = "_")
            }
            prevNameList[[name]] <- name
        }
        names(dfList[[i]])[-1] <- name
        names(dfList[[i]])[1] <- "date"
    }
    df <- Reduce(function(d1, d2) {
        join(d1, d2, by = "date")
    }, dfList)
    df$date <- mdy_hm(df$date)
    for (i in 2:length(df)) {
        fac <- df[, i]
        levels(fac) <- gsub(" ", "", levels(fac))
        levels(fac) <- gsub("Percent", "", levels(fac))
        levels(fac) <- gsub("<", "", levels(fac))
        df[, i] <- as.numeric(as.character(fac))
    }
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
