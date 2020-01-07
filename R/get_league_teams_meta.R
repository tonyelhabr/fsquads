
.get_league_teams_meta_1 <- function(url) {
  page <- url %>% xml2::read_html()
  nodes_teams <- 
    page %>% 
    rvest::html_node('body') %>% 
    rvest::html_nodes('h5') %>% 
    rvest::html_children()
  teams <- nodes_teams %>% rvest::html_text()
  slugs <- 
    nodes_teams %>% 
    rvest::html_attr('href')
  url_prefix <- url %>% str_replace('(^.*[0-9])\\/?.*htm$', '\\1')
  urls <-
    sprintf('%s/%s', url_prefix, slugs)
  res <-
    tibble::tibble(
      team = teams,
      url = urls
    )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param url PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[xml2]{read_xml}}
#'  \code{\link[rvest]{html_nodes}},\code{\link[rvest]{html_text}}
#'  \code{\link[tibble]{tibble}}
#' @rdname .get_league_teams_meta_1
#' @noRd
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_nodes html_children html_text html_attr
#' @importFrom tibble tibble
get_league_teams_meta_1 <- memoise::memoise(.get_league_teams_meta_1)

.get_league_teams_meta <- function(league, year) {
  
  leagues_meta <- get_leagues_meta()
  
  if(missing(league)) {
    .stop_for_league(leagues_meta, '`league` must be specified. ')
  }
  
  if(missing(year)) {
    .stop_for_year(leagues_meta, '`year` must be specified. ')
  }
  
  leagues_meta_filt_1 <- leagues_meta %>% .filter_league(league)
  leagues_meta_filt_2 <- leagues_meta_filt_1 %>% .filter_year(year  = year)
  stopifnot(nrow(leagues_meta_filt_2) == 1L) # Probably don't even need to check for this (given that the prior checks are passed).
  
  # url <- leagues_meta_filt_2 %>% pull(url)
  # res <- url %>% get_league_teams_meta_1()
  res <-
    leagues_meta_filt_2 %>% 
    dplyr::mutate(data = purrr::map(url, get_league_teams_meta_1)) %>% 
    dplyr::select(-url) %>% 
    tidyr::unnest(data)
  res
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param league PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   leagues_meta <- get_leagues_meta()
#'   leagues_meta
#' }
#' }
#' @seealso 
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[tidyr]{nest}}
#' @rdname get_league_teams_meta
#' @export 
#' @importFrom dplyr mutate select
#' @importFrom purrr map
#' @importFrom tidyr unnest
get_league_teams_meta <- memoise::memoise(.get_league_teams_meta)
