

#' binarize_binom
#'
#' Transform a data with binomial responses into a data with binary response.
#'
#' @usage binarize_binom(dat, responses, variable.name = NULL)
#' @param dat A data whose features are categorical, and has two integer
#'   columns that correspond to binomial responses
#' @param responses A character vector of length 2 where each element is the
#'   names of columns that store the counts of positive and negative
#'   responses, in this order
#' @param variable.name NULL by default; a character that will be used for
#'   the name of new binary response column after binarization; if NULL,
#'   then a randomly generated character will be used
#' @return A data.table that has the same categorical features as \code{dat},
#'   and has \code{variable.name} column as its binary response
#' @author Junkyu Park
#' @seealso
#'   \code{\link{binarize_pois}}
#'   \code{\link{change_form}}
#' @examples
#' # data(rich_binom)
#' binarize_binom(rich_binom, c('rich', 'not_rich'))
#' binarize_binom(rich_binom, c('rich', 'not_rich'), 'is_rich')
#' @export
#' @import data.table
binarize_binom <- function(dat, responses, variable.name = NULL) {

    # Generate random names to avoid the same names as in features
    all_alphabets <- c(
        'A', 'a', 'B', 'b', 'C', 'c', 'D', 'd', 'E', 'e', 'F', 'f',
        'G', 'g', 'H', 'h', 'I', 'i', 'J', 'j', 'K', 'k', 'L', 'l',
        'M', 'm', 'N', 'n', 'O', 'o', 'P', 'p', 'Q', 'q', 'R', 'r',
        'S', 's', 'T', 't', 'U', 'u', 'V', 'v', 'W', 'w', 'X', 'x',
        'Y', 'y', 'Z', 'z'
    )
    dalphabet <- function(x){
        dpmf(
            x,
            rep(1, length(all_alphabets)) / length(all_alphabets),
            all_alphabets
        )
    }
    separator <-
        paste0(rpmf(10, dalphabet, all_alphabets), collapse = '')
    united <-
        paste0(rpmf(10, dalphabet, all_alphabets), collapse = '')

    # Setup
    if (!('data.table' %in% class(dat))) {data.table::setDT(dat)}
    col_names <- colnames(dat)
    id_vars <- col_names[!(col_names %in% responses)]
    if (is.null(variable.name)) {
        variable_name <-
            paste0(rpmf(10, dalphabet, all_alphabets), collapse = '')
        message('Randomly generated variable name used: ', variable_name)
    } else {
        variable_name <- variable.name
    }
    value_name <-
        paste0(rpmf(10, dalphabet, all_alphabets), collapse = '')

    # Transform into the form that is used in poisson regression
    dat <- data.table::melt(
        dat,
        id.vars = id_vars,
        variable.name = variable_name,
        value.name = value_name
    )
    id_vars <- c(id_vars, variable_name)

    # Binarize
    dat <- eval(parse(text = paste0(
        'dat[', value_name, ' != 0, ',
        '.(', united,
        ' = do.call(paste, c(.SD, sep = \"', separator,
        '\")), ', value_name, '), .SDcols = id_vars]'
    )))
    dat <- dat[
        ,
        list(
            result = rep(
                eval(parse(text = paste0('dat$\"', united, '\"'))),
                eval(parse(text = paste0('dat$\"', value_name, '\"')))
            )
        )
    ][
        , # requires data.table ver >= 1.9.6 because of tstrsplit
        c(id_vars) := tstrsplit(result, separator, fixed = T)
    ][
        ,
        c(id_vars),
        with = F
    ]
    dat
}



#' binarize_pois
#'
#' Transform a data with a count response into a data with binary response.
#'
#' @usage binarize_pois(dat, response)
#' @param dat A data whose features are categorical, and has a column of
#'   natural numbers that correspond to the count response
#' @param response A character; the name of column that stores the count
#'   response
#' @return A data.table that has all of categorical features in \code{dat},
#'   where each given combination of features (rows) are populated by integers
#'   based on \code{response}. The function will not keep those combinations
#'   with a zero count.
#' @author Junkyu Park
#' @seealso
#'   \code{\link{binarize_binom}}
#'   \code{\link{change_form}}
#' @examples
#' # data(rich_pois)
#' binarize_pois(rich_pois, 'count')
#' @export
#' @import data.table
binarize_pois <- function(dat, response) {

    # Generate random names to avoid the same names as in features
    all_alphabets <- c(
        'A', 'a', 'B', 'b', 'C', 'c', 'D', 'd', 'E', 'e', 'F', 'f',
        'G', 'g', 'H', 'h', 'I', 'i', 'J', 'j', 'K', 'k', 'L', 'l',
        'M', 'm', 'N', 'n', 'O', 'o', 'P', 'p', 'Q', 'q', 'R', 'r',
        'S', 's', 'T', 't', 'U', 'u', 'V', 'v', 'W', 'w', 'X', 'x',
        'Y', 'y', 'Z', 'z'
    )
    dalphabet <- function(x){
        dpmf(
            x,
            rep(1, length(all_alphabets)) / length(all_alphabets),
            all_alphabets
        )
    }
    separator <-
        paste0(rpmf(10, dalphabet, all_alphabets), collapse = '')
    united <-
        paste0(rpmf(10, dalphabet, all_alphabets), collapse = '')

    # Setup
    if (!('data.table' %in% class(dat))) {setDT(dat)}
    col_names <- colnames(dat)
    id_vars <- col_names[!(col_names %in% response)]
    variable_name <-
        paste0(rpmf(10, dalphabet, all_alphabets), collapse = '')
    value_name <-
        paste0(rpmf(10, dalphabet, all_alphabets), collapse = '')

    # Binarize
    dat <- eval(parse(text = paste0(
        'dat[', response, ' != 0, ',
        '.(', united, ' = do.call(paste, c(.SD, sep = \"', separator,
        '\")), ', response, '), .SDcols = id_vars]'
    )))
    dat <- dat[
        ,
        list(
            result = rep(
                eval(parse(text = paste0('dat$\"', united, '\"'))),
                eval(parse(text = paste0('dat$\"', response, '\"')))
            )
        )
    ][
        , # requires data.table ver >= 1.9.6 because of tstrsplit
        c(id_vars) := tstrsplit(result, separator, fixed = T)
    ][
        ,
        c(id_vars),
        with = F
    ]
    dat
}



#' change_form
#'
#' Transform a data from one form into another.
#'
#' @usage change_form(dat, from, to,
#'             old_response, category, new_response)
#' @param dat A data whose features are categorical and response(s) is/are
#'   nonnegative integer vector(s).
#' @param from A character; either 'binary', 'binomial', or 'poisson'
#' @param to A character != from; either 'binary', 'binomial', or 'poisson'
#' @param old_response (always specified) a character vector of:
#'   length 1 if from = 'binary' or 'poisson'; the name of column
#'   in dat that stores a response/count, or;
#'   lenght 2 if from = 'binomial'; the names of columns in dat
#'   that store positive and negative case counts, in this order.
#' @param category (specified only if to = 'poisson') a character vector of:
#'   length 1 if from = 'binomial'; the new name of column that
#'   will store two names in old_response as positive and
#'   negative cases, in this order, or;
#'   length 2 if from = 'binary'; the new names for positive and
#'   negative cases in the binary response column, in this order.
#' @param new_response (specified only if from != 'poisson') a character
#'   vector of:
#'   length 1 if to = 'binary' or 'poisson'; the name of the new
#'   column in new data that will store either a binary or count
#'   response, or;
#'   length 2 if to = 'binomial'; the names of two columns in
#'   new data that will store positive and negative case counts,
#'   in this order.
#' @return A data.table that has transformed from the form specified in
#'   \code{from} to the form specified in \code{to}.
#' @author Junkyu Park
#' @seealso
#'   \code{\link{binarize_binom}}
#'   \code{\link{binarize_pois}}
#' @examples
#' change_form(
#'     rich,
#'     from = 'binary', to = 'binomial',
#'     old_response = 'is_rich',
#'     new_response = c('rich', 'not_rich')
#' )
#' change_form(
#'     rich,
#'     from = 'binary', to = 'poisson',
#'     old_response = 'is_rich',
#'     category = c('rich', 'not_rich'),
#'     new_response = 'count'
#' )
#' change_form(
#'     rich_binom,
#'     from = 'binomial', to = 'binary',
#'     old_response = c('rich', 'not_rich'),
#'     new_response = 'is_rich'
#' )
#' change_form(
#'     rich_binom,
#'     from = 'binomial', to = 'poisson',
#'     old_response = c('rich', 'not_rich'),
#'     category = 'is_rich',
#'     new_response = 'count'
#' )
#' change_form(
#'     rich_pois,
#'     from = 'poisson', to = 'binary',
#'     old_response = 'count'
#' )
#' change_form(
#'     rich_pois,
#'     from = 'poisson', to = 'binomial',
#'     old_response = 'count',
#'     category = 'is_rich'
#' )
#' @export
#' @import data.table
change_form <- function(dat, from, to,
                        old_response, category, new_response) {

    if (!('data.table' %in% class(dat))) {data.table::setDT(dat)}
    col_names <- colnames(dat)
    id_vars <- col_names[!(col_names %in% old_response)]
    id_vars_collapsed <- paste0(id_vars, collapse =',')
    if (from == 'binary') {
        if (to == 'binomial') {
            return(eval(parse(text = paste0(
                'dat[, ',
                '.(', new_response[1] , ' = sum(', old_response, '), ',
                new_response[2], ' = sum(!', old_response, ')), ',
                'by = \"', id_vars_collapsed, '\"]'
            ))))
        } else if (to == 'poisson') {
            dat <- eval(parse(text = paste0(
                'dat[, ',
                '.(', category[1] , ' = sum(', old_response, '), ',
                category[2], ' = sum(!', old_response, ')), ',
                'by = \"', id_vars_collapsed, '\"]'
            )))
            return(suppressWarnings(data.table::melt(
                dat,
                id.vars = id_vars,
                variable.name = old_response,
                value.name = new_response
            )))
        } else {
            stop(
                '\"to\" must be either \"binomial\" or \"poisson\"',
                ' if \"from\" = \"binary\"'
            )
        }
    } else if (from == 'binomial') {
        if (to == 'binary') {
            return(binarize_binom(dat, old_response, new_response))
        } else if (to == 'poisson') {
            data.table::melt(
                dat,
                id.vars = id_vars,
                variable.name = category,
                value.name = new_response
            )
        } else {
            stop(
                '\"to\" must be either \"binary\" or \"poisson\"',
                ' if \"from\" = \"binomial\"'
            )
        }
    } else if (from == 'poisson') {
        if (to == 'binary') {
            return(binarize_pois(dat, old_response))
        } else if (to == 'binomial') {
            id_vars <- id_vars[!(id_vars %in% category)]
            id_vars_fmlr <- paste0(id_vars, collapse = ' + ')
            fmlr <- as.formula(paste0(id_vars_fmlr, ' ~ ', category))
            return(data.table::dcast(dat, fmlr, value.var = old_response))
        } else {
            stop(
                '\"to\" must be either \"binary\" or \"binomial\"',
                ' if \"from\" = \"poisson\"'
            )
        }
    } else {
        stop(paste0(
            '\"from\" must be either',
            ' \"binary\", \"binomial\", or \"poisson\"'
        ))
    }
}



#' CI_auc
#'
#' Calculate a confidence interval of AUC by bootstrapping.
#'
#' @usage CI_auc(dat, fmlr, R = 500, type = 'norm', ...)
#' @param dat A data whose response is binary (0 and 1).
#' @param fmlr A formula for the logistic regression with logit link.
#' @param R The same as \code{R} in boot::boot.
#' @param type The same as \code{type} in boot::boot.ci
#' @param \dots Additional arguments of boot::boot.ci
#' @return A \code{bootci} object.
#' @author Junkyu Park
#' @seealso
#'   \code{\link{binarize_binom}}
#'   \code{\link{binarize_pois}}
#'   \code{\link{change_form}}
#'   \code{\link{plot_roc}}
#' @examples
#' # library(data.table)
#' data(femsmoke, package = 'faraway')
#' femsmoke_binary <- binarize_pois(femsmoke, 'y')
#' femsmoke_binary[, dead := ifelse(dead == 'yes', 1, 0)]
#' CI_auc(femsmoke_binary, dead ~ smoker + age)
#' @export
CI_auc <- function(dat, fmlr, R = 500, type = 'norm', ...) {

    AUC_boot <- function(dat, i) {
        y <- dat[i, ]
        mod <- glm(fmlr, family = binomial, data = y)
        ests <- predict(mod, type = 'response')
        response <- as.character(fmlr[2])
        actual <- eval(parse(text = paste0('y$\"', response, '\"')))
        suppressMessages(as.numeric(pROC::auc(pROC::roc(actual, ests))))
    }
    boot_output <- boot::boot(dat, statistic = AUC_boot, R = R)
    boot::boot.ci(boot_output, type = type, ...)
}



#' plot_roc
#'
#' Plot the ROC curve of the logistic regression with logit link.
#'
#' @usage plot_roc(dat, fmlr)
#' @param dat A data whose response is binary (0 and 1).
#' @param fmlr A formula for the logistic regression with logit link.
#' @return A \code{ggplot} object.
#' @author Junkyu Park
#' @seealso
#'   \code{\link{binarize_binom}},
#'   \code{\link{binarize_pois}}
#'   \code{\link{change_form}}
#'   \code{\link{CI_auc}}
#' @examples
#' # data(nodal, package = 'SMPracticals')
#' plot_roc(nodal, r ~ stage + xray + acid)
#' @export
#' @import ggplot2
plot_roc <- function(dat, fmlr) {

    mod <- glm(formula = fmlr, family = binomial, data = dat)
    ests <- predict(mod, type = 'response')
    response <- as.character(fmlr[2])
    actual <- eval(parse(text = paste0('dat$\"', response, '\"')))
    roc_result <- pROC::roc(actual, ests)
    roc_table <- data.table::data.table(
        TPR = roc_result$sensitivities,
        FPR = 1 - roc_result$specificities,
        thresholds = roc_result$thresholds
    )[
        order(TPR)
    ]
    ggplot(roc_table, aes(FPR, TPR, label = round(thresholds, 4))) +
        geom_point() +
        ggrepel::geom_label_repel(
            box.padding = 0.3,
            point.padding = 0.3,
            segment.color = "grey50"
        ) +
        geom_line() +
        geom_segment(
            aes(x = 0, y = 0, xend = 1, yend = 1),
            col = "red", linetype = "dashed"
        ) +
        annotate(
            "text", x = 1, y = .05, hjust = 1,
            label = paste0(
                "AUC : ", round(as.numeric(pROC::auc(roc_result)), 4)
            )
        ) +
        labs(
            x = "False positive rate",
            y = "True positive rate",
            title = "ROC curve",
            subtitle = paste0(response, " ~ ", as.character(fmlr[3]))
        )
}
