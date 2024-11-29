#' Preprocessing of the speeches corpus
#' 
#' @description 
#' Parse the variables into correct R format.
#' 
#' @param corpus The speeches corpus.
#' 
#' @keywords Internal
speeches_parse_variables <- function(corpus){
  corpus$dok_hangar_id <- as.integer(corpus$dok_hangar_id)
  corpus$dok_id <- as.factor(corpus$dok_id)
  corpus$dok_titel <- as.factor(corpus$dok_titel)
  corpus$dok_rm <- as.factor(corpus$dok_rm)
  corpus$dok_nummer <- as.factor(corpus$dok_nummer)
  corpus$dok_datum <- stringr::str_sub(corpus$dok_datum, 1L, 10L)
  corpus$dok_datum <- as.Date(corpus$dok_datum)
  corpus$avsnittsrubrik <- as.factor(corpus$avsnittsrubrik)
  corpus$kammaraktivitet <- as.factor(corpus$kammaraktivitet)
  corpus$intressent_id <- as.factor(corpus$intressent_id)
  corpus$replik <- as.factor(corpus$replik)
  
  corpus$talare <- as.factor(corpus$talare)
  corpus$parti <- as.factor(corpus$parti)
  corpus$anforande_nummer <- as.integer(corpus$anforande_nummer)
  
  corpus$systemdatum <- stringr::str_sub(corpus$systemdatum, 1L, 10L)
  corpus$systemdatum <- as.Date(corpus$systemdatum)
  
  #corpus$underrubrik <- as.factor(corpus$underrubrik)
  
  corpus
}

#' @rdname speeches_parse_variables
#' @keywords Internal
speeches_remove_observations <- function(corpus){
  # Remove erroneous observations
  to_remove <- corpus$anforandetext == "null" | stringr::str_count(corpus$anforandetext, " ") == 0 | nchar(corpus$anforandetext) == 0
  removed_corpus <- corpus[to_remove, ]
  corpus <- corpus[!to_remove, ]
  corpus
}

