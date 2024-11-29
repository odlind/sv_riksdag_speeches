#' Create debate type
#'
#' @param avsnittsrubrik Avsnittsrubrik to use to identify \code{debate_type}
#' @param kammaraktivitet Kammaraktivitet to use to identify \code{debate_type}
#'
#' @keywords Internal
create_debate_type <- function(avsnittsrubrik, kammaraktivitet){
  checkmate::assert_character(avsnittsrubrik, any.missing = FALSE)
  checkmate::assert_character(kammaraktivitet, any.missing = FALSE)
  
  avsnittsrubrik <- tolower(as.character(avsnittsrubrik))
  avsnittsrubrik <- stringr::str_trim(avsnittsrubrik)
  debate_type <- character(length(avsnittsrubrik))
  
  # table(avsnittsrubrik[stringr::str_detect(avsnittsrubrik, "partiledardebatt") & !stringr::str_detect(avsnittsrubrik, "meddelande")])
  debate_type[stringr::str_detect(avsnittsrubrik, "partiledardebatt") & 
                !stringr::str_detect(avsnittsrubrik, "meddelande")] <- "Party leader debate"
  
  # table(avsnittsrubrik[stringr::str_detect(avsnittsrubrik, "fr.gestund") & !stringr::str_detect(avsnittsrubrik, "meddelande")])
  debate_type[stringr::str_detect(avsnittsrubrik, "fr.gestund") & 
                !stringr::str_detect(avsnittsrubrik, "meddelande")] <- "Question time"
  # table(avsnittsrubrik[stringr::str_detect(avsnittsrubrik, "muntliga fr.gor till regeringen") & !stringr::str_detect(avsnittsrubrik, "meddelande")])
  debate_type[stringr::str_detect(avsnittsrubrik, "muntliga fr.gor till regeringen")] <- "Question time"
  debate_type[stringr::str_detect(kammaraktivitet, "statsministerns fr.gestund")] <- "Question time"
  debate_type[stringr::str_detect(kammaraktivitet, "fr.gestund")] <- "Question time"
  
  # table(avsnittsrubrik[stringr::str_detect(avsnittsrubrik, "interpellation") & !stringr::str_detect(avsnittsrubrik, "meddelande")])
  debate_type[stringr::str_detect(avsnittsrubrik, "interpellation") & 
                !stringr::str_detect(avsnittsrubrik, "meddelande")] <- "Interpellation"
  debate_type[stringr::str_detect(kammaraktivitet, "interpellationsdebatt")] <- "Interpellation"
  
  # table(avsnittsrubrik[stringr::str_detect(avsnittsrubrik, "allm.npolitisk") & !stringr::str_detect(avsnittsrubrik, "meddelande|partiledardebatt")])
  debate_type[stringr::str_detect(avsnittsrubrik, "allm.npolitisk") & 
                !stringr::str_detect(avsnittsrubrik, "meddelande|partiledardebatt")] <- "General debate"
  debate_type[stringr::str_detect(kammaraktivitet, "allm.npolitisk debatt")] <- "General debate"
  debate_type[stringr::str_detect(kammaraktivitet, "debatt vid allm.n debattimme")] <- "General debate"
  
  debate_type[stringr::str_detect(speeches$avsnittsrubrik, "regeringen") & 
                stringr::str_detect(speeches$avsnittsrubrik, "[Ii]nformation")] <- "Information from government"
  debate_type[stringr::str_detect(kammaraktivitet, "information fr.n regeringen")] <- "Information from government"
  
  # table(debate_type)
  
  # table(avsnittsrubrik[stringr::str_detect(avsnittsrubrik, "remissdebatt")])
  
  # tab <- table(avsnittsrubrik[debate_type == ""])
  # mat <- as.matrix(tab[order(tab, decreasing = TRUE)][1:100])
  
  debate_type[debate_type == ""] <- "Other debates/speeches"
  as.factor(debate_type)
}