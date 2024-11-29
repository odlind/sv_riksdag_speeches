#' Clean the parti variable in the corpus file
#'
#' @param talare a character vector from speaker in corpus file
#'
#' @keywords Internal
talare_clean <- function(talare){
  talare <- as.character(talare)
  talare <- tolower(talare)
  talare <- stringr::str_replace_all(talare, "replik", "")
  talare <- stringr::str_trim(talare)
  
  talare <- stringr::str_replace_all(talare, "^.+minister(n)?", " ")
  talare <- stringr::str_replace_all(talare, "^.+mininister", " ")
  talare <- stringr::str_replace_all(talare, "^.+president(en)?", " ")
  talare <- stringr::str_replace_all(talare, "^.*statsrådet", " ")
  # talare[stringr::str_detect(talare, "^.*minist.+")]
  talare <- stringr::str_replace_all(talare, "^.*minist.+", " ")
  
  talare <- stringr::str_trim(talare)

  talare <- stringr::str_replace_all(talare, "\\(kds\\)", "\\(kd\\)")  
  talare <- stringr::str_replace_all(talare, "\\(fp\\)", "\\(l\\)")  
  talare <- stringr::str_replace_all(talare, "©", "\\(c\\)")  

  talare <- stringr::str_replace_all(talare, "\\(v$", "\\(v\\)")  
  talare <- stringr::str_replace_all(talare, "\\(s$", "\\(s\\)")  
  
  talare <- stringr::str_replace_all(talare, "null", "namn saknas")
  talare <- stringr::str_replace_all(talare, ".*kommer att vara.*", "namn saknas")
  talare <- stringr::str_replace_all(talare, "^en$", "namn saknas")
  
  talare <- stringr::str_replace_all(talare, "talmanen", "talmannen")
  talare <- stringr::str_replace_all(talare, ".*f.rste vice talman ", "")
  talare <- stringr::str_replace_all(talare, ".*andre vice talman ", "")
  talare <- stringr::str_replace_all(talare, ".*tredje vice talman ", "")
  
  talare <- stringr::str_replace_all(talare, "andre vice talman", "andre vice talmannen")
  talare <- stringr::str_replace_all(talare, ".*av andre vice talmannen därefter.*", "andre vice talmannen")
  talare <- stringr::str_replace_all(talare, ".*av andre vice talmannennen därefter.*", "andre vice talmannen")
  talare <- stringr::str_replace_all(talare, ".*av förste vice talmannen därefter.*", "förste vice talmannen")
  talare <- stringr::str_replace_all(talare, "talmannnen", "talmannen")
  
  talare <- stringr::str_replace_all(talare, "( )+", " ")
  # talare[stringr::str_detect(talare, "([:lower:])\\(")] 
  talare <- stringr::str_replace_all(talare, "([:lower:])\\(", "\\1 \\(")
  # talare[stringr::str_detect(talare, "( )*-( )*")]
  talare <- stringr::str_replace_all(talare, "( )*-( )*", "-")

  speaker_errors <- get_speaker_errors()
  for(i in 1:nrow(speaker_errors)){
    error_exists <- any(stringr::str_detect(string = talare, pattern = speaker_errors[i,1]))
    if(!error_exists) cat(paste0(speaker_errors[i,1], " don't exist.\n"))
    talare <- stringr::str_replace_all(talare, speaker_errors[i,1], speaker_errors[i,2])
  }

  talare <- stringr::str_trim(talare)
  
  # table(talare)
  # table(talare[stringr::str_detect(talare, "björkman")])
  talare <- as.factor(talare)
  
  talare
}

#' Get errors in speaker from csv file
#' @keywords Internal
#' @rdname talare_clean
get_speaker_errors <- function(){
  speaker_errors <- read.csv2("3. Data and Method/Data/rcrpsriksdag/RPackage/data-raw/data_errors/speaker_errors.csv", stringsAsFactors = FALSE)
  speaker_errors[,1] <- stringr::str_replace_all(speaker_errors[,1], "\\(", "\\\\\\(")
  speaker_errors[,2] <- stringr::str_replace_all(speaker_errors[,2], "\\(", "\\\\\\(")
  speaker_errors[,1] <- stringr::str_replace_all(speaker_errors[,1], "\\)", "\\\\\\)")
  speaker_errors[,2] <- stringr::str_replace_all(speaker_errors[,2], "\\)", "\\\\\\)")
  speaker_errors
}
