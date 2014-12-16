\name{csvToDf}
\alias{csvToDf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
csvToDf
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
csvToDf(csvLines, skip = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{csvLines}{
%%     ~~Describe \code{csvLines} here~~
}
  \item{skip}{
%%     ~~Describe \code{skip} here~~
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
function (csvLines, skip = 0) 
{
    regex <- "\"([^\\\"]*)\""
    if (skip > 0) 
        csvLines <- csvLines[-skip]
    csvLines <- csvLines[csvLines != ""]
    if (length(csvLines) < 1) {
        return(data.frame())
    }
    df <- data.frame(t(strapplyc(csvLines, regex, simplify = TRUE)))
    names(df) <- laply(df[1, ], as.character)
    df <- df[-1, ]
    return(df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
