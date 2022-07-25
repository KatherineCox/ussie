#' Make a standard tibble for league-play data
#'
#' Given a league-play data frame from {engsoccerdata}, returns a tibble with
#' standardised column-names and types, e.g. `date` is a `Date`.
#'
#' @param data_engsoc data obtained from {engsoccerdata}.
#' @param country name of the country for the data
#'
#' @return a tibble with columns `country`, `date`, `season`, `tier`, `home`,
#'    `visitor`, `goals_home`, `goals_visitor`.
#' @export
#'
#' @examples
#' uss_make_matches(engsoccerdata::spain, "Spain")
uss_make_matches <- function(data_engsoc, country) {
  result <-
    data_engsoc |>
    tibble::as_tibble() |>
    dplyr::transmute(
      country = as.character(country),
      tier = factor(tier, levels = c("1", "2", "3", "4")),
      season = as.integer(Season),
      date = as.Date(Date),
      home = as.character(home),
      visitor = as.character(visitor),
      goals_home = as.integer(hgoal),
      goals_visitor = as.integer(vgoal)
    )
  result
}
