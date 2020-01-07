

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param .data PARAM_DESCRIPTION
#' @param var PARAM_DESCRIPTION, Default: -1
#' @param ... PARAM_DESCRIPTION
#' @param decreasing PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[tidyselect]{vars_pull}}
#'  \code{\link[rlang]{nse-defuse}}
#' @rdname .pull_distinctly
#' @noRd 
#' @importFrom tidyselect vars_pull
#' @importFrom rlang enquo
.pull_distinctly <- function(.data, var = -1, ..., decreasing = FALSE)  {
  var <- tidyselect::vars_pull(names(.data), !!rlang::enquo(var))
  sort(unique(.data[[var]]), decreasing = decreasing, ...)
}