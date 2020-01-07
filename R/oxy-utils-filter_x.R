
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param leagues_meta PARAM_DESCRIPTION
#' @param league PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{filter}}
#' @rdname .filter_league
#' @noRd
#' @importFrom dplyr filter
.filter_league <- function(leagues_meta, league) {
  stopifnot(is.character(league))
  stopifnot(length(league) == 1L)
  res <-
    leagues_meta %>% 
    dplyr::filter(league == !!league)
  
  if(nrow(res) == 0L) {
    # stop(sprintf('Invalid `league` (%s)', league), call. = FALSE)
    .stop_for_league(leagues_meta, sprintf('`league` (%s) is invalid. ', league))
  }
  res
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param leagues_meta PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{filter}}
#' @rdname .filter_year
#' @noRd
#' @importFrom dplyr filter
.filter_year <- function(leagues_meta, year) {
  stopifnot(is.numeric(year)) # Really, it should be an integer, but we can let this pass.
  stopifnot(length(year) == 1L)
  
  res <-
    leagues_meta %>% 
    dplyr::filter(year == !!year)
  
  if(nrow(res) == 0L) {
    # Don't think there should ever be more than 1 `league`, given the way the rest of the functions are designed.
    leagues <- leagues_meta %>% .pull_distinctly(league)
    leagues_chr <- leagues %>% paste0(sep = '', collapse = '\n')
    n_leagues <- length(leagues) == 1L
    if(n_leagues) {
      leagues_chr <- sprintf('given `league = %s`', leagues)
    } else if(n_leagues > 1L) {
      leagues_chr <- sprintf('multiple (%s) `league`s', n_leagues)
    } else {
      leagues_chr <- 'no `league`'
    }
    msg_suffix <- sprintf(' (%s).', leagues_chr)
    .stop_for_year(leagues_meta, sprintf('`year` (%s) is invalid %s', year, msg_suffix))
  }
  res
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param league_teams_meta PARAM_DESCRIPTION
#' @param team PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{filter}}
#' @rdname .filter_team
#' @noRd
#' @importFrom dplyr filter
.filter_team <- function(league_teams_meta, team) {
  stopifnot(is.character(team))
  stopifnot(length(team) == 1L)
  res <-
    league_teams_meta %>% 
    dplyr::filter(team == !!team)
  
  if(nrow(res) == 0L) {
    # Don't think there should ever be more than 1 `league`, given the way the rest of the functions are designed.
    leagues <- league_teams_meta %>% .pull_distinctly(league)
    leagues_chr <- leagues %>% paste0(sep = '', collapse = '\n')
    n_leagues <- length(leagues) == 1L
    if(n_leagues) {
      leagues_chr <- sprintf('given `league = %s`', leagues)
    } else if(n_leagues > 1L) {
      leagues_chr <- sprintf('multiple (%s) `league`s', n_leagues)
    } else {
      leagues_chr <- 'no `league`.'
    }
    
    # Don't think there should ever be more than 1 `year`, given the way the rest of the functions are designed.
    years <- league_teams_meta %>% .pull_distinctly(year)
    years_chr <- years %>% paste0(sep = '', collapse = '\n')
    n_years <- length(years) == 1L
    if(n_years) {
      years_chr <- sprintf('`year = %s`', years)
    } else if(n_years > 1L) {
      years_chr <- sprintf('multiple (%s) years', n_years)
    } else {
      years_chr <- 'no `year`'
    }
    
    msg_suffix <- sprintf(' (%s and %s).', leagues_chr, years_chr)
    .stop_for_team(league_teams_meta, sprintf('`team` (%s) is invalid%s. ', team, msg_suffix))
  }
  res
}
