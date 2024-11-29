#' Compute a test_set indicator
#'
#' @param corpus a \code{speeches} corpus.
#'
#' @keywords Internal
create_debate_ids <- function(corpus){
  # Assert corpus
  checkmate::assert_class(corpus, "tbl_df")
  
  # Create no variable (to reorder)
  corpus$no <- 1:nrow(corpus)
  
  # Order by date, avnittsrubrik and anforande_no
  corpus <- dplyr::arrange(corpus, dok_datum, dok_nummer, avsnittsrubrik, anforande_nummer)

  # Create debate_id
  corpus$debate_id <- as.factor(paste(corpus$dok_datum, corpus$dok_nummer, corpus$avsnittsrubrik))
  corpus$debate_id <- as.numeric(corpus$debate_id)
  
  # Create debate_speech_no
  corpus <- dplyr::group_by(corpus, debate_id)
  corpus <- dplyr::mutate(corpus, debate_speech_no = row_number())
    
  # Reorder back and remove no variable
  corpus <- dplyr::arrange(corpus, no)
  corpus$no <- NULL
  corpus
}

