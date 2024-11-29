#' Clean the parti variable in the corpus file
#'
#' @param parti a factor vector from parti in a \code{speeches} \code{data_frame}
#' @param talare a factor vector from talare in a \code{speeches} \code{data_frame}
#' @param party_errors_file a csv file containing the variables \code{wrong_parti}, \code{true_parti} and \code{talare}
#'
#' @keywords Internal
parti_clean <- function(parti, talare, party_errors_file = "RPackage/data-raw/data_errors/party_errors.csv"){
  checkmate::assert_factor(parti)
  checkmate::assert_factor(talare)
  checkmate::assert_file_exists(party_errors_file)
  
  parti <- as.character(parti)
  talare <- as.character(talare)
  parti <- tolower(parti)
  
  parti[parti=="kds"] <- "kd"
  parti[parti=="fp"] <- "l"
  parti[parti=="-"] <- "null"
  parti[nchar(parti) > 4] <- "null"

  talare_parti <- stringr::str_extract(talare, pattern = "\\(.+\\)")
  talare_parti <- stringr::str_extract(talare_parti, pattern = "[a-z]+")
  talare_parti[is.na(talare_parti)] <- "null"
  
  talare_parti[talare_parti=="kds"] <- "kd"
  talare_parti[talare_parti=="fp"] <- "l"
  talare_parti[talare_parti=="-"] <- "null"
  
  party_errors <- read.csv(party_errors_file, stringsAsFactors = FALSE)
  for(i in 1:nrow(party_errors)){
    idx <- which(party_errors$wrong_parti[i] == parti & party_errors$talare[i] == talare)
    if(length(idx) > 0) parti[idx] <- party_errors$true_parti[i]
  }
  
  diff_idx <- which(talare_parti != parti)
  if(length(diff_idx) > 0){
    warning(length(diff_idx), " differences in party between variable 'talare' and 'parti'.")
  }
    
  # x <- dplyr::data_frame(parti = parti[diff_idx], talare_parti = talare_parti[diff_idx], talare = talare[diff_idx], parti_freq = 0, talare_parti_freq = 0)
  # for(i in 1:nrow(x)){
  #  tab <- table(parti[talare == x$talare[i]])
  #  x$parti_freq[i] <- tab[x$parti[i]]
  #  x$talare_parti_freq[i] <- tab[x$talare_parti[i]]
  #}
  # parti[diff_idx] <- talare_parti[diff_idx]
  
  parti <- as.factor(parti)
  parti
}

