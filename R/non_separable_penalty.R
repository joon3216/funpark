
#' fusion_estimates
#'
#' Find the fusion estimates of given \code{y}. Start iteration with \code{theta}
#' and the tuning parameter \code{lambda}.
#'
#' @usage fusion_estimates(y, theta, lambda, max_iter = 100, eps = 1e-5)
#' @param y A vector of numbers.
#' @param theta A vector of numbers that has the same length as \code{y}. If
#'   missing, then it is replaced with \code{y}.
#' @param lambda A positive number; the tuning parameter.
#' @param max_iter A natural number; the maximum iteration. \code{100} by
#'   default.
#' @param eps A positive number; \code{1e-05} by default.
#' @return A list:
#' \itemize{
#'   \item \code{theta}: Resulting fusion estimates.
#'   \item \code{phi}: Differences in \code{theta}.
#'   \item \code{lambda}: A specified \code{lambda} when running the function.
#'   \item \code{iter}: The number of iterations.
#'   \item \code{costs}: Collection of costs at each iteration.
#' }
#' @author Junkyu Park
#' @seealso
#'   \href{https://joon3216.github.io/research_materials/2018/non_separable_penalty}{Dealing with a non-separable penalty}
#' @examples
#' set.seed(1024)
#' n <- 1000
#' t <- 1:n
#' f <- function(t) {t / 250 - .5}
#' g <- function(t) {-(.25 / 449) * t + 250 / 449}
#' true_theta <- c(rep(0, 249), f(250:500), rep(.75, 50), g(551:1000))
#' y <- true_theta + rnorm(1000, 0, 0.1)
#' start <- rep(.5, n)
#' result <- fusion_estimates(y, theta = Start, lambda = 1, max_iter = 1000)
#' @export
fusion_estimates <- function(y, theta, lambda, max_iter = 100, eps = 1e-5) {

    n <- length(y)
    if (missing(theta)) {theta <- y}
    if (length(theta) != n) {
        stop(paste0(
            '\nError in fusion_estimates():\n',
            'The length of given initial theta is ', length(theta),
            ', which is not equal to length(y) == ', n, '.'
        ))
    }
    phi <- diff(theta)
    phisums_old <- cumsum(phi)
    theta_1_new <- (sum(y) - sum(phisums_old)) / n
    cost <- sum((y - theta)^2) + lambda * sum(abs(phi))
    costs <- NULL
    costs[1] <- cost # costs
    there_is_a_progress <- T
    iter <- 0
    while (there_is_a_progress & iter < max_iter) {
        # Store new phi_1 (= 0) to phi_n in phi_new
        phi_new <- numeric(length = n)
        for (j in 2:n) {
            phisums_new <- cumsum(phi_new)
            req <- sum(
                phisums_old[(j - 1):(n - 1)] -
                    phisums_old[j - 1] + phisums_new[j - 1]
            )
            discri <- sum(y[j:n]) - (n - j + 1) * theta_1_new - req
            if (discri < -lambda / 2) {
                phi_new[j] <- (discri + lambda / 2) / (n - j + 1)
            } else if (discri > lambda / 2) {
                phi_new[j] <- (discri - lambda / 2) / (n - j + 1)
            } # already 0 otherwise
        }
        phi_new <- phi_new[-1]
        phisums_new <- phisums_new[-1]
        theta <- c(theta_1_new, theta_1_new + phisums_new)
        cost <- sum((y - theta)^2) + lambda * sum(abs(phi_new))
        theta_1_new <- (sum(y) - sum(phisums_new)) / n
        phisums_old <- phisums_new
        iter <- iter + 1
        costs[iter + 1] <- cost
        there_is_a_progress <- !(abs(costs[iter] - cost) <= eps)
    }
    list(
        theta = theta,
        phi = phi_new,
        lambda = lambda,
        iter = iter,
        costs = costs # the first cost is calculated at iteration 0
    )
}
