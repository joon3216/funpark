
split_data_1d <- function(y, num_folds, fold) {
    # y: a numeric vector
    # num_folds: a natural number <= number of observations, e.g. 5
    # fold: a natural number <= num_folds, e.g. 3

    n <- length(y)
    indices <- 1:n
    assign_folds <- rep(1:num_folds, n %/% num_folds + 1)[indices]
    partition <- split(indices, assign_folds)
    fold_indices <- partition[[fold]]
    y_fold <- y[fold_indices]
    y_rest <- y[-fold_indices]
    list(
        y_fold = list(
            data = y_fold,
            indices = fold_indices
        ),
        y_rest = list(
            data = y_rest,
            indices = (1:n)[!(1:n %in% fold_indices)]
        )
    )
}



interpolate_1d <- function(t, theta_rest, indices_rest, n) {
    # t: a number, or a numeric vector whose minimum is at least 1 and
    #    whose maximum is at most n
    # theta_rest: a numeric vector
    # indices_rest: an integer vector of length length(theta_rest)
    # n: an integer; the length of "full" data

    indices <- sort(indices_rest)
    if (max(t) > n || min(t) < 1) {
        stop(paste0(
            '\nError in interpolate_1d():\n',
            'Extrapolation not available;\n',
            'either max(t) > length of full data or min(t) < 1 happened'
        ))
    }
    if (length(theta_rest) != length(indices)) {
        stop(paste0(
            '\nError in interpolate_1d():\n',
            'length(theta_rest) != length(indices)'
        ))
    }
    sapply(t, function(d){theta_rest[which.min(abs(d - indices))]})
}



loss_1d_fold <- function(y, theta, indices_fold) {
    # y: a numeric vector of length n
    # theta: a numeric vector of length < n
    # indices_fold: an integer vector of length n - length(theta)

    n <- length(y)
    indices_fold <- sort(indices_fold)
    y_fold <- y[indices_fold]
    indices_rest <- (1:n)[!(1:n %in% indices_fold)]
    interpolate <- interpolate_1d(indices_fold, theta, indices_rest, n)
    mean((interpolate - y_fold)^2)
}



loss_1d <- function(y, theta) {
    # y: the same as in loss_1d_fold
    # theta: a numeric vector of length n

    mean((y - theta)^2)
}



#' cv_error_1d
#'
#' Compute the \code{k}-fold cross-validation error of fusion estimates of
#' \code{y} trained with the tuning parameter \code{lambda}.
#'
#' @usage cv_error_1d(y, k, lambda)
#' @param y An one-dimensinoal numeric vector.
#' @param k A natural number; the number of folds.
#' @param lambda The same as \code{lambda} in \code{fusion_estimates}.
#' @return A list:
#'   \itemize{
#'     \item \code{error}: mean of all losses computed
#'     \item \code{losses}: losses computed at each fold
#'   }
#' @author Junkyu Park
#' @seealso
#'   \href{https://joon3216.github.io/research_materials/2019/cross_validation_fs}{Cross-validation for fusion estimates}
#' @examples
#' set.seed(1024)
#' n <- 1000
#' t <- 1:n
#' f <- function(t) {t / 250 - .5}
#' g <- function(t) {-(.25 / 449) * t + 250 / 449}
#' true_theta <- c(rep(0, 249), f(250:500), rep(.75, 50), g(551:1000))
#' y <- true_theta + rnorm(1000, 0, 0.1)
#' cv_error_1d(y = y, k = 5, lambda = 1)
#' @export
cv_error_1d <- function(y, k, lambda) {

    losses <- numeric(length = k)
    for (s in 1:k) {
        split_s <- split_data_1d(y, k, s)
        fusion_s <- fusion_estimates(split_s$y_rest$data, lambda = lambda)
        losses[s] <- loss_1d_fold(y, fusion_s$theta, split_s$y_fold$indices)
    }
    list(error = mean(losses), losses = losses)
}
