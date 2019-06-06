\name{dpmf}
\alias{dpmf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
dpmf
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Create a probbility mass function.
}
\usage{
dpmf(x, pmf_vec, support_vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
a number, or a vector whose class is the same as \code{support_vec}
}
  \item{pmf_vec}{
%%     ~~Describe \code{pmf_vec} here~~
a numeric vector, where each element falls in (0, 1) and sums up to 1
}
  \item{support_vec}{
%%     ~~Describe \code{support_vec} here~~
a vector with the same length as \code{pmf_vec} where each entry of \code{support_vec} is the input that corresponds to the probability in \code{pmf_vec}; if missing, it is replaced with \code{0:(length(pmf_vec) - 1)}.
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
Junkyu Park
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{rpmf}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
