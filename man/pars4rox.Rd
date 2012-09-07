\name{pars4rox}
\alias{pars4rox}
\title{
Copy the arguments of a function and parameters are created.
}
\description{
Copy the arguments of a function and parameters are created.
}
\usage{
pars4rox(pars, dots = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pars}{
%%     ~~Describe \code{pars} here~~
}
  \item{dots}{
%%     ~~Describe \code{dots} here~~
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
function (pars, dots = FALSE) 
{
    require(qdap)
    x <- paste0("#' @param ", spaste(qdap::qcv(terms = pars, 
        split = ",")))
    x <- qdap::Trim(sapply(strsplit(x, "="), function(x) x[1]))
    zz <- matrix(rbind(x), ncol = 1)
    if (dots) {
        zz <- rbind(zz, "#' @param \\ldots ")
    }
    dimnames(zz) <- list(c(rep("", nrow(zz))), c(""))
    writeClipboard(noquote(zz), format = 1)
    return(noquote(zz))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
