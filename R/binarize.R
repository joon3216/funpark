
source('R/pgf.R')

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
#' @seealso \code{\link{binarize_pois}}
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
#' @seealso \code{\link{binarize_binom}}
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



change_form <- function(dat, from, to,
                        old_response, category, new_response) {
    # dat: a data with categorical features; a response column/columns
    #      must be numeric (0 and 1 if from = 'binary', or nonnegative
    #      integers if from is either 'binomial' or 'poisson').
    # from: a character; either 'binary', 'binomial', or 'poisson'
    # to: a character != from; either 'binary', 'binomial', or 'poisson'
    # old_response: (always specified) a character vector of:
    #     * length 1 if from = 'binary' or 'poisson'; the name of column
    #       in dat that stores a response/count
    #     * lenght 2 if from = 'binomial'; the names of columns in dat
    #       that store positive and negative case counts, in this order.
    # category: (specified only if to = 'poisson') a character vector of:
    #     * length 1 if from = 'binomial'; the new name of column that
    #       will store two names in old_response as positive and
    #       negative cases, in this order.
    #     * length 2 if from = 'binary'; the new names for positive and
    #       negative cases in the binary response column, in this order.
    # new_response: (specified only if from != 'poisson') a character
    #               vector of:
    #     * length 1 if to = 'binary' or 'poisson'; the name of the new
    #       column in new data that will store either a binary or count
    #       response
    #     * length 2 if to = 'binomial'; the names of two columns in
    #       new data that will store positive and negative case counts,
    #       in this order.

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
