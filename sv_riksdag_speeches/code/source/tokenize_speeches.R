#' Tokenize the corpus
#'
#' Tokenize and remove stopwords and rare words from the \code{speeches} corpus. It tokenizes based solely on one space.
#' 
#' @param corpus a \code{speeches} corpus to tokenize.
#' @param rare_word_limit The rare word limit to use (tokens for word types occuring less or equal to the limit is removed).
#' @param stop_list a character vector
#' 
#' @examples 
#' data("stop_words_se")
#' data("speeches")
#' speeches_tokenized <- tokenize_speeches(speeches, 10, stop_words_se)
#' 
#' @export
tokenize_speeches <- function(corpus, rare_word_limit = 10, stop_list = NULL){
  checkmate::assert_class(corpus, "tbl_df")
  checkmate::assert_subset(c("anforande_id", "anforandetext"), names(corpus))
  checkmate::assert_integerish(rare_word_limit, lower = 0)
  checkmate::assert_character(stop_list, any.missing = FALSE, null.ok = TRUE)
    
  # Create one-token-per-row object
  txt <- dplyr::data_frame(anforande_id = as.factor(corpus$anforande_id), txt = corpus$anforandetext)
  txt <- tidytext::unnest_tokens(txt, word, txt, token = stringr::str_split, pattern = " ", to_lower = FALSE)
  tkns <- nrow(txt)
  message(tkns, " tokens in data.")
  
  # Remove stop words
  if(!is.null(stop_list)){
    txt$row <- 1L:nrow(txt)
    stop_word <- dplyr::data_frame(word = stop_list)
    txt <- dplyr::anti_join(txt, stop_word, by = "word")
    txt <- dplyr::arrange(txt, row)
    txt$row <- NULL
    message(tkns - nrow(txt), " stop word tokens removed.")
    tkns <- nrow(txt)
  }
  
  # Calculate and remove rare words
  if(rare_word_limit > 0){
    rare_words <- dplyr::count(txt, word) 
    rare_words <- dplyr::filter(rare_words, n <= rare_word_limit)
    txt$row <- 1L:nrow(txt)
    txt <- dplyr::anti_join(txt, rare_words, by = "word")
    txt <- dplyr::arrange(txt, row)
    txt$row <- NULL
    message(tkns - nrow(txt), " rare word tokens removed.")
    tkns <- nrow(txt)
  }
  
  names(txt) <- c("anforande_id", "token")
  txt$token <- as.factor(txt$token)
  txt
}