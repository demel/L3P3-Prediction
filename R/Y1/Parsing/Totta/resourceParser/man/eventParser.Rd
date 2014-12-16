\name{eventParser}
\alias{eventParser}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
eventParser
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eventParser(events)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{events}{
%%     ~~Describe \code{events} here~~
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
function (events) 
{
    tcEvents <- textConnection(events)
    events <- read.table(tcEvents, header = TRUE, sep = ",", 
        colClasses = "character")
    close(tcEvents)
    if (ncol(events) < 4) {
        return(data.frame(parseError = file_name))
    }
    if (nrow(events) < 1) {
        return(data.frame(emptyLog = file_name))
    }
    Sys.setlocale("LC_TIME", "Spanish_Spain.1252")
    date <- as.POSIXct(strptime(events$Created.On, format = "\%d-\%b-\%Y \%H:\%M:\%S"))
    dateClear <- as.POSIXct(strptime(events$Cleared.On, format = "\%d-\%b-\%Y \%H:\%M:\%S"))
    if (length(date) == nrow(events) || any(is.na(date)) == FALSE) {
        events$Created.On <- date
    }
    else {
        return(data.frame(parseError = file_name))
    }
    if (length(dateClear) == nrow(events)) {
        events$Cleared.On <- dateClear
    }
    if (!is.null(events$Event.Type)) {
        events$Event.Type <- paste("t.", (events$Event.Type), 
            sep = "")
    }
    return(events)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
