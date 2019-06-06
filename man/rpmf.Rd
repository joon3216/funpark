\name{rpmf}
\alias{rpmf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
rpmf
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Draw random samples from a probability mass function using the \href{https://en.wikipedia.org/wiki/Inverse_transform_sampling}{inverse transform sampling}.
}
\usage{
rpmf(n, pmf, support, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
an integer >= 1
}
  \item{pmf}{
%%     ~~Describe \code{pmf} here~~
a probability mass function which takes elements of an ambient space of pmf's support (i.e. an input that makes pmf return a probability) in its first argument. This pmf must be able to take an array of elements and return an array of probabilities. Each output of this \code{pmf} must fall in (0, 1), and every possible output, including 0, must sum up to 1.
}
  \item{support}{
%%     ~~Describe \code{support} here~~
a vector which consists of the elements of the support of \code{pmf}, i.e. \code{sum(pmf(support, ...)) == 1 && !(0 %in% pmf(support, ...))}
}
  \item{...}{
an additional argument of \code{pmf}
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
\code{\link{dpmf}}
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
