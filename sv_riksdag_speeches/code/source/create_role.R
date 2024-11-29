#' Clean the parti variable in the corpus file
#'
#' @param talare the raw talare variable
#'
#' @keywords Internal
create_role <- function(talare){
  talare <- as.character(talare)
  talare <- tolower(talare)
  role <- character(length(talare))
  # Create role variable
  tmp <- stringr::str_extract(talare, "[a-ö -]+minister[a-z]*")
  role[!is.na(tmp)] <- tmp[!is.na(tmp)]
  tmp <- stringr::str_extract(talare, "^.*talman[a-z]*")
  role[!is.na(tmp)] <- tmp[!is.na(tmp)]
  tmp <- stringr::str_extract(talare, "^.*president[a-z]*")
  role[!is.na(tmp)] <- tmp[!is.na(tmp)]  
  tmp <- stringr::str_extract(talare, "^.*statsråd[a-z]*")
  role[!is.na(tmp)] <- tmp[!is.na(tmp)]
  tmp <- stringr::str_extract(talare, "^.*konungen.*$")
  role[!is.na(tmp)] <- tmp[!is.na(tmp)]  
  
  # Clean variables
  # Remove last n and en
  role <- stringr::str_replace(role, pattern = "en$", "")
  role <- stringr::str_replace(role, pattern = "n*$", "")
  role <- stringr::str_trim(role)
  role <- stringr::str_replace(role, pattern = "^av", "")
  role <- stringr::str_trim(role)
  role <- stringr::str_replace(role, pattern = "talma", "talman")
  
  # Clean details
  role <- stringr::str_replace(role, pattern = "ders- president", "derspresident")
  role <- stringr::str_replace(role, pattern = "statminister", "statsminister")
  role <- stringr::str_replace(role, pattern = "kulurminister", "kulturminister")
  role <- stringr::str_replace(role, pattern = "kommunkationsminister", "kommunikationsminister")
  role <- stringr::str_replace(role, pattern = "justititieminister", "justitieminister")
  role <- stringr::str_replace(role, pattern = "justititeminister", "justitieminister")
  role <- stringr::str_replace(role, pattern = "arbetsmarknadminister", "arbetsmarknadsminister")
  role <- stringr::str_replace(role, pattern = "arbetsmarkmarknadsminister", "arbetsmarknadsminister")
  role <- stringr::str_replace(role, pattern = "arbetsmarknadminister", "arbetsmarknadsminister")
  
  role <- as.factor(role)
  role
}