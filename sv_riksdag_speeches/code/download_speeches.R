
library(XML)
library(xml2)


#Download Speeches

years <- c(199394, 199495, 199596, 199697, 199798, 199899, 19992000, 200001, 200102, 200203, 
           200304, 200405, 200506, 200607, 200708, 200809, 200910, 201011, 201112, 201213, 
           201314, 201415, 201516, 201617, 201718, 201819, 201920, 202021, 202122, 202223,
           202324)

# Make a temporary file (tf) and a temporary folder (tdir)
tf <- tempfile(tmpdir = tdir <- tempdir())
xml_files <- list() #list to store speeches

# Download the zip file, unzip and store in a list
for (i in 1:length(years)) {
  print(paste("Downloading speeches for year:", years[i]))
  download.file(paste0("https://data.riksdagen.se/dataset/anforande/anforande-", years[i],".xml.zip"), tf)
  print(paste("Unzipping speeches for year:", years[i]))
  xml_files[[i]] <- unzip(tf, exdir = tdir) # Unzip it in the temp folder
}

df_j <- list()
df_i <- list()
for (i in 1:length(xml_files)) {
  for (j in 1:length(xml_files[[i]])) {
    print(paste0("Year: ", years[i], ", File: ", j,"/",length(xml_files[[i]])))
    doc <- xmlParse(xml_files[[i]][[j]]) #restructure the files
    df_j[[j]] <- xmlToDataFrame(nodes = getNodeSet(doc, "//anforande")) #transform to data frame and save in a list
  }
  print(paste("Merging all single row df into one, please wait..."))
  df_j <- lapply(df_j, function(x) x[!(names(x) %in% c("underrubrik"))]) #remove extra column present in some of the files
  df_i[[i]] <- do.call(rbind, df_j)
}

speech_data <- do.call(rbind, df_i) #merge data files from different years

speech_data <- transform(speech_data, anforandetext = as.character(anforandetext), #transform to character
                         avsnittsrubrik = as.character(avsnittsrubrik),
                         talare = as.character(talare),
                         dok_titel = as.character(dok_titel))

Encoding(speech_data[["anforandetext"]]) <- "UTF-8" #change encoding to UTF-8, for swedish letters
Encoding(speech_data[["avsnittsrubrik"]]) <- "UTF-8"
Encoding(speech_data[["talare"]]) <- "UTF-8"
Encoding(speech_data[["dok_titel"]]) <- "UTF-8"


#Save data
write.csv(speech_data,"sv_riksdag_speeches/data/speech_data.csv", row.names = FALSE)
