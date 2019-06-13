
#' simulate_na
#'
#' Randomly replace some components in \code{X_complete} with \code{NA} based
#' on \code{na_rate} specified.
#'
#' @usage simulate_na(X_complete, na_rate)
#' @param X_complete A data or a matrix with no \code{NA}'s.
#' @param na_rate A number in (0, 1).
#' @return A list:
#'   \itemize{
#'     \item \code{X}: A generated data with missing components.
#'     \item \code{C}: \code{!is.na(X)}
#'     \item \code{na_rate}: A specified \code{na_rate} when running the
#'       function.
#'     \item \code{na_rate_actual}: The actual proportion of missing components
#'       after running the function.
#'   }
#' @author Junkyu Park
#' @seealso
#'   \code{\link{impute_em}},
#'   \href{https://joon3216.github.io/research_materials/2019/em_imputation}{Imputing missing data using EM algorithm}
#' @examples
#' set.seed(1024)
#' mu <- c(1, 2, 6)
#' Sigma <- matrix(c(118, 62, 44, 62, 49, 17, 44, 17, 21), nrow = 3)
#' n <- 400
#' X_truth <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
#' result <- simulate_na(X_truth, na_rate = .4)
#' @export
simulate_na <- function(X_complete, na_rate) {
    # X_complete: a data frame or a matrix with no NA's
    # na_rate: a number in (0, 1)

    # Create C matrix; entry is FALSE if missing, and TRUE if observed
    nr <- nrow(X_complete); nc <- ncol(X_complete)
    C <- matrix(runif(nr * nc) > na_rate, nrow = nr)

    # Check for which i's we have all components become missing
    checker <- which(rowSums(C) == 0)
    if (length(checker) == 0) {
        # Every X_i has at least one component that is observed,
        # which is what we want
        X_complete[!C] <- NA
    } else {
        # Otherwise, randomly "revive" some components in such X_i's
        for (i in checker) {
            C[i, sample(nc, ceiling(runif(1, max = nc)))] <- TRUE
        }
        X_complete[!C] <- NA
    }
    list(
        X = X_complete,
        C = C,
        na_rate = na_rate,
        na_rate_actual = sum(!C) / (nr * nc)
    )
}



#' impute_em
#'
#' Impute missing components in \code{X} using the EM algorithm, assuming
#' each row is a sample of multivariate normal distribution.
#'
#' @usage impute_em(X, max_iter = 3000, eps = 1e-08)
#' @param X A data or a matrix with or without \code{NA}'s.
#' @param max_iter A natural number; the number of iterations.
#'   \code{3000} by default.
#' @param eps A small positive number; 1e-08 by default.
#' @return A list:
#'   \itemize{
#'     \item \code{mu}: The resulting mean vector estimate. If \code{X} (input)
#'       has no missing components, then this is the MLE.
#'     \item \code{Sigma}: The resulting variance matrix estimate. If \code{X}
#'       (input) has no missing components, then this is the MLE.
#'     \item \code{X_imputed}: An imputed data.
#'     \item \code{C}: \code{!is.na(X)}, where \code{X} is the input.
#'     \item \code{iter}: The number of iterations.
#'   }
#' @author Junkyu Park
#' @seealso
#'   \code{\link{simulate_na}},
#'   \href{https://joon3216.github.io/research_materials/2019/em_imputation}{Imputing missing data using EM algorithm}
#' @examples
#' set.seed(1024)
#' mu <- c(1, 2, 6)
#' Sigma <- matrix(c(118, 62, 44, 62, 49, 17, 44, 17, 21), nrow = 3)
#' n <- 400
#' X_truth <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
#' result <- simulate_na(X_truth, na_rate = .4)
#' result_imputed <- impute_em(result$X)
#' @export
impute_em <- function(X, max_iter = 3000, eps = 1e-08) {
    # X: a data frame or a matrix, possibly with some NA's
    # max_iter: a natural number; 3000 by default
    # eps: a positive real number; 1e-08 by default

    nr <- nrow(X)
    nc <- ncol(X)
    C <- !is.na(X) # the C matrix

    # Collect M_i and O_i's
    Ms <- t(1:nc * t(!C))
    Os <- t(1:nc * t(C))
    M <- lapply(1:nr, function(d) {Ms[d, ][Ms[d, ] != 0]})
    O <- lapply(1:nr, function(d) {Os[d, ][Os[d, ] != 0]})

    # Generate Mu_0 and Sigma_0
    Mu <- colMeans(X, na.rm = T)
    S <- var(X, na.rm = T)
    if (is.na(sum(S))) { # S contains at least one NA
        S <- diag(apply(X, 2, var, na.rm = T))
    }
    Mu_tilde <- S_tilde <- vector('list', length = nr)
    X_tilde <- X
    no_conv <- T
    iter <- 0
    while (no_conv & iter < max_iter) {
        for (i in 1:nr) {
            S_tilde[[i]] <- matrix(rep(0, nc^2), nrow = nc)
            if (length(O[[i]]) != nc) { # consider only nonempty M[[i]]'s
                S_MM <- S[M[[i]], M[[i]]]
                S_MO <- matrix(S[M[[i]], O[[i]]], nrow = length(M[[i]]))
                S_OM <- t(S_MO)
                S_OO <- S[O[[i]], O[[i]]]
                Mu_tilde[[i]] <- Mu[M[[i]]] +
                    S_MO %*% solve(S_OO) %*% (X[i, O[[i]]] - Mu[O[[i]]])
                X_tilde[i, M[[i]]] <- as.numeric(Mu_tilde[[i]])
                S_MM.O <- S_MM - S_MO %*% solve(S_OO) %*% S_OM
                zero_matrix <- matrix(rep(0, nc^2), nrow = nc)
                zero_matrix[M[[i]], M[[i]]] <- S_MM.O
                S_tilde[[i]] <- zero_matrix
            }
        }
        Mu_new <- colMeans(X_tilde)
        S_new <- ((nr - 1) / nr) * var(X_tilde) + Reduce('+', S_tilde) / nr
        no_conv <- !(
            norm(Mu - Mu_new, type = '2') < eps &&
                norm(S - S_new, type = '2') < eps
        )
        Mu <- Mu_new
        S <- S_new
        iter <- iter + 1
    }
    list(mu = Mu, Sigma = S, X_imputed = X_tilde, C = C, iter = iter)
}
