
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param leagues_meta PARAM_DESCRIPTION
#' @param msg_prefix PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[glue]{glue}}
#' @rdname .stop_for_league
#' @noRd
#' @importFrom glue glue
.stop_for_league <- function(leagues_meta, msg_prefix = '') {
  leagues <- leagues_meta %>% .pull_distinctly(league)
  leagues_chr <- leagues %>% paste0(sep = '', collapse = ', ')
  stop(
    glue::glue(
      '{msg_prefix}Try one of the following: 
      {leagues_chr}'
    ),
    call. = FALSE
  )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param leagues_meta PARAM_DESCRIPTION
#' @param msg_prefix PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[glue]{glue}}
#' @rdname .stop_for_year
#' @noRd
#' @importFrom glue glue
.stop_for_year <- function(leagues_meta, msg_prefix = '') {
  years <- leagues_meta %>% .pull_distinctly(year)
  years_chr <- years %>% paste0(sep = '', collapse = ', ')
  stop(
    glue::glue(
      '{msg_prefix}Try one of the following: 
      {years_chr}'
    ),
    call. = FALSE
  )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param league_teams_meta PARAM_DESCRIPTION
#' @param msg_prefix PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[glue]{glue}}
#' @rdname .stop_for_team
#' @noRd
#' @importFrom glue glue
.stop_for_team <- function(league_teams_meta, msg_prefix = '') {
  teams <- league_teams_meta %>% .pull_distinctly(team)
  teams_chr <- teams %>% paste0(sep = '', collapse = ', ')
  stop(
    glue::glue(
      '{msg_prefix}Try one of the following: 
      {teams_chr}'
    ),
    call. = FALSE
  )
}