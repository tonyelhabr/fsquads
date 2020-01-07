

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
#'  \code{\link[stringr]{str_replace}},\code{\link[stringr]{str_sub}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{select}}
#' @rdname get_leagues_meta_1
#' @noRd
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr
#' @importFrom stringr str_replace str_replace_all str_sub
#' @importFrom tibble tibble
#' @importFrom dplyr mutate_at vars select
.get_leagues_meta_1 <- function(url) {
  page <- url %>% xml2::read_html()
  slugs <- page %>% rvest::html_nodes(xpath = '//*[@id="main"]/table/tr/td/a') %>% rvest::html_attr('href')
  # Or
  # slugs <- page %>% rvest::html_nodes('#main > table > tr > td > a') %>% rvest::html_attr('href')
  # countries <- slugs %>% str_subset('(^.*)\\/([1-2].*$)', '\\1')
  # seasons <- slugs %>% str_subset('(^.*)\\', '\\2')
  rgx <- '(^.*)\\/([0-9-]+)\\/(.*)[.]htm$'
  url_prefix <- url %>% stringr::str_replace('k\\/.*$', 'k/')
  res <-
    tibble::tibble(slug = slugs) %>% 
    dplyr::mutate_at(
      dplyr::vars(slug),
      list(
        country = ~stringr::str_replace_all(., rgx, '\\1'),
        season = ~stringr::str_replace_all(., rgx, '\\2'),
        league = ~stringr::str_replace_all(., rgx, '\\3'),
        url = ~sprintf('%s%s', url_prefix, .)
      )
    ) %>%
    dplyr::mutate_at(dplyr::vars(season), list(year = ~stringr::str_sub(., 1L, 4L) %>% as.integer())) %>% 
    dplyr::select(-slug)
  res
}

get_leagues_meta_1 <- memoise::memoise(.get_leagues_meta_1)

.get_leagues_meta <- function() {
  urls <- 
    c(
      current = 'http://www.footballsquads.co.uk/squads.htm', 
      archive = 'http://www.footballsquads.co.uk/archive.htm'
    )
  
  res_prelim <-
    urls %>% 
    tibble::tibble(url = .) %>% 
    dplyr::mutate(
      res = purrr::map(url, get_leagues_meta_1)
    ) %>% 
    dplyr::select(-url) %>% 
    # Or
    # select(res) %>% 
    tidyr::unnest(res) %>% 
    # select(country, league, year, season, url) %>% 
    dplyr::select(league, year, url) %>% 
    dplyr::arrange(league, year)
  
  # # Notably, 'engprem' was 'faprem' prior to `2018`.
  # faprem <-
  #   res_prelim %>% 
  #   dplyr::filter(league == 'faprem')
  # 
  # engprem <-
  #   res_prelim %>% 
  #   dplyr::filter(league == 'engprem')
  # 
  # # ... more to duplicate these leagues.
  # 
  # res <-
  #   res_prelim %>% 
  #   # Hard-coding fixes. (The `url` should not be changed.)
  #   dplyr::mutate_at(
  #     dplyr::vars(league),
  #     ~dplyr::case_when(
  #       . == 'faprem' & year <= 2017L ~ 'engprem',
  #       TRUE ~ .
  #     )
  #   )
  res <- res_prelim
  res
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'   league_teams_meta <- get_league_teams_meta(league = 'fagprem', year = 2016L)
#'   league_teams_meta
#' a}
#' }
#' @seealso 
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{arrange}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[tidyr]{nest}}
#' @rdname get_leagues_meta
#' @export 
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select arrange
#' @importFrom purrr map
#' @importFrom tidyr unnest
get_leagues_meta <- memoise::memoise(.get_leagues_meta)