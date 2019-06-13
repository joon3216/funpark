
#' rich dataset
#'
#' A sample dataset to test functions. This dataset has a binary response.
#'
#' @docType data
#' @usage data(rich)
#' @format A data.table with 10 rows and 3 variables:
#' \describe{
#'     \item{gender}{gender, either \code{M}ale or \code{F}emale}
#'     \item{criminal_record}{indicating whether a person has a criminal record (\code{yes}) or not (\code{no})}
#'     \item{is_rich}{integer column, \code{0} if not rich and \code{1} if rich}
#' }
#' @keywords datasets
#' @examples
#' data(rich)
#' change_form(
#'     rich,
#'     from = 'binary', to = 'binomial',
#'     old_response = 'is_rich',
#'     new_response = c('rich', 'not_rich')
#' )
"rich"



#' rich_binom dataset
#'
#' A sample dataset to test functions. This dataset has binomial responses.
#'
#' @docType data
#' @usage data(rich_binom)
#' @format A data.table with 4 rows and 4 variables:
#' \describe{
#'     \item{gender}{gender, either \code{M}ale or \code{F}emale}
#'     \item{criminal_record}{indicating whether a person has a criminal record (\code{yes}) or not (\code{no})}
#'     \item{rich}{the number of rich people that satisfies \code{(gender, criminal_record)} combination}
#'     \item{not_rich}{the number of non-rich people that satisfies the same combination as in \code{rich}}
#' }
#' @keywords datasets
#' @examples
#' data(rich_binom)
#' binarize_binom(rich_binom, c('rich', 'not_rich'), 'is_rich')
"rich_binom"



#' rich_pois dataset
#'
#' A sample dataset to test functions. This dataset has a count response.
#'
#' @docType data
#' @usage data(rich_pois)
#' @format A data.table with 8 rows and 4 variables:
#' \describe{
#'     \item{gender}{gender, either \code{M}ale or \code{F}emale}
#'     \item{criminal_record}{indicating whether a person has a criminal record (\code{yes}) or not (\code{no})}
#'     \item{is_rich}{factors, \code{rich} if a person is rich and \code{not_rich} otherwise}
#'     \item{count}{the number of people that satisfies \code{(gender, criminal_record, is_rich)} combination}
#' }
#' @keywords datasets
#' @examples
#' data(rich_pois)
#' binarize_pois(rich_pois, 'count')
"rich_pois"
