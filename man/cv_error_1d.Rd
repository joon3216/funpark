% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_validation_fs.R
\name{cv_error_1d}
\alias{cv_error_1d}
\title{cv_error_1d}
\usage{
cv_error_1d(y, k, lambda)
}
\arguments{
\item{y}{An one-dimensinoal numeric vector.}

\item{k}{A natural number; the number of folds.}

\item{lambda}{The same as \code{lambda} in \code{fusion_estimates}.}
}
\value{
A list:
  \itemize{
    \item \code{error}: mean of all losses computed
    \item \code{losses}: losses computed at each fold
  }
}
\description{
Compute the \code{k}-fold cross-validation error of fusion estimates of
\code{y} trained with the tuning parameter \code{lambda}.
}
\examples{
set.seed(1024)
n <- 1000
t <- 1:n
f <- function(t) {t / 250 - .5}
g <- function(t) {-(.25 / 449) * t + 250 / 449}
true_theta <- c(rep(0, 249), f(250:500), rep(.75, 50), g(551:1000))
y <- true_theta + rnorm(1000, 0, 0.1)
cv_error_1d(y = y, k = 5, lambda = 1)
}
\seealso{
\href{https://joon3216.github.io/research_materials/2019/cross_validation_fs}{Cross-validation for fusion estimates}
}
\author{
Junkyu Park
}
