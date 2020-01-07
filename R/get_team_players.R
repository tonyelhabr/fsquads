
.get_team_players_1 <- function(url) {
  page <- url %>% xml2::read_html()
  
  df <- 
    page  %>% 
    rvest::html_table(header = TRUE) %>% 
    purrr::pluck(1) %>% 
    janitor::clean_names() %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(name != '') %>% 
    dplyr::mutate_all(~ifelse(. == '', NA, .))
  
  res <-
    df %>% 
    dplyr::filter(
      !(name %in% c('Players no longer at this club', 'Name'))
    ) %>% 
    dplyr::mutate_at(dplyr::vars(number, weight), as.integer) %>% 
    dplyr::mutate_at(dplyr::vars(height), as.double) %>% 
    dplyr::mutate_at(dplyr::vars(date_of_birth), lubridate::dmy)
  res
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param league PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
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
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[tidyr]{nest}}
#' @rdname .get_team_players
#' @noRd
#' @importFrom dplyr mutate select
#' @importFrom purrr map
#' @importFrom tidyr unnest
get_team_players_1 <- memoise::memoise(.get_team_players_1)

.get_team_players <- function(league, year, team) {
  
  if(missing(team)) {
    .stop_for_league(leagues_meta, '`team` must be specified. ')
  }
  
  league_teams_meta <- get_league_teams_meta(league = league, year = year)
  
  league_teams_meta_filt <- league_teams_meta %>% .filter_team(team)
  stopifnot(nrow(league_teams_meta_filt) == 1L) # Probably don't even need to check for this (given that the prior checks are passed).
  
  # url <- league_teams_meta_filt %>% dplyr::pull(url)
  # res <- url %>% get_team_players_1()
  res <-
    league_teams_meta_filt %>% 
    dplyr::mutate(data = purrr::map(url, get_team_players_1)) %>% 
    dplyr::select(-url) %>% 
    tidyr::unnest(data)
  res
  res
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param league PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
#' @param team PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'    # Works
#'    get_team_players(league = 'faprem', year = 2016L, team = 'Tottenham Hotspur')
#'    
#'    # Fails because 'engprem' only started to be used beginning in 2018
#'    get_team_players(league = 'engprem', year = 2016L, team = 'Tottenham Hotspur')
#'    
#'    # Fails (for reason mentioned above).
#'    get_team_players(league = 'faprem', year = 2018L, team = 'Tottenham Hotspur')
#'    
#'    # Fails due to invalid team name.
#'    get_team_players(league = 'faprem', year = 2016L, team = 'Tottenham')
#' }
#' }
#' @seealso 
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[tidyr]{nest}}
#' @rdname get_team_players
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom purrr map
#' @importFrom tidyr unnest
get_team_players <- memoise::memoise(.get_team_players)
