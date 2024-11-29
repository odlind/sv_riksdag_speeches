
library(tidyverse)
library(data.table)
library(stopwords)
library(tm)
library(stm)

setwd("C:/Users/Alexander/Documents/Militarisering projekt/Anföranden riksdag/")
source("code/source/anforandetext_clean.R")
source("code/source/speeches_preprocessing.R")
source("code/source/create_debate_ids.R")
source("code/source/create_debate_type.R")
source("code/source/create_role.R")
source("code/source/parti_clean.R")
source("code/source/talare_clean.R")
source("code/source/tokenize_speeches.R")



#Load data
speeches <- fread("data/speech_data.csv")

#Using cleaning functions from Måns Magnusson ("Mansmeg") https://github.com/MansMeg/rcrpsriksdag 

#Makes the variables into the correct R-format, and remove observations that are empty. #Magnusson r-file named "speeches_preprocessing"
speeches <- speeches_parse_variables(speeches) #I removed "underrubrik" from the function, as I removed it previously.
speeches <- speeches_remove_observations(speeches)

#Clean texts using the functions in the r-file called "anforandetext_clean" from Magnusson
speeches$anforandetext_cleaned <- anforandetext_clean(speeches$anforandetext)

#Create dabate IDs
speeches <- tibble::as_tibble(speeches) # Make into a tbl_df structure
speeches <- create_debate_ids(speeches) #Function created by Magnusson, using r-file called "create_debate_ids"

#Create debate type
speeches$avsnittsrubrik <- as.character(speeches$avsnittsrubrik)
speeches$kammaraktivitet <- as.character(speeches$kammaraktivitet)
speeches$debate_type <- create_debate_type(speeches$avsnittsrubrik,speeches$kammaraktivitet) #Function created by Magnusson, using r-file called "create_debate_type"

#Create dabate role
speeches$debate_role <- create_role(speeches$talare)#Function created by Magnusson, using r-file called "create_role"

#Clean parties
speeches$parti <- parti_clean(speeches$parti, speeches$talare, "data/party_errors.csv")#Function created by Magnusson, using r-file called "parti_clean" (Appendix A) and csv "party_errors" (Appendix B)

#Clean speakers (talare)
speeches$talare <- talare_clean(speeches$talare) #Function created by Magnusson, using r-file called "talare_clean"

#save data
write.csv(speeches,"speeches.csv", row.names = FALSE)



#additional cleaning ----

speeches_clean <- fread("data/speeches.csv")
speeches_clean <- as.data.frame(dplyr::select(speeches_clean[(
  speeches_clean$dok_rm == "2023/24" |
  speeches_clean$dok_rm == "2022/23" |
  speeches_clean$dok_rm == "2021/22" |
  speeches_clean$dok_rm == "2020/21" |
  speeches_clean$dok_rm == "2019/20" |
  speeches_clean$dok_rm == "2018/19" |
  speeches_clean$dok_rm == "2017/18" |
  speeches_clean$dok_rm == "2016/17" |
  speeches_clean$dok_rm == "2015/16" |
  speeches_clean$dok_rm == "2014/15" |
  speeches_clean$dok_rm == "2013/14" |
  speeches_clean$dok_rm == "2012/13" |
  speeches_clean$dok_rm == "2011/12" |
  speeches_clean$dok_rm == "2010/11" |
  speeches_clean$dok_rm == "2009/10" |
  speeches_clean$dok_rm == "2008/09" |
  speeches_clean$dok_rm == "2007/08" |
  speeches_clean$dok_rm == "2006/07") & (
    speeches_clean$parti != "" &
      speeches_clean$parti != "null")
], "anforandetext_cleaned", "anforande_id", "dok_datum", "parti"))


#Remove other irrelevant words that come up in the analysis from speeches
stopwords_se <- data_stopwords_stopwordsiso$sv
unnecessary_words <- c(stopwords_se,"vänsterpartiet", "socialdemokraterna", "miljöpartiet", "centerpartiet", "liberalerna", "folkpartiet", "moderaterna", "kristdemokraterna", "sverigedemokraterna", "fru", "herr", "nej", "ja", "tack", "tacka", "tackar", "tänker", "tänka", "tänk", "svaret", "svar", "fortfarande", "håller", "interpellation", "interpellationen", "ofta", "sällan", "antal", "antalet", "minister", "ministern", "alltid", "aldrig", "oerhört", "kammaren", "diskutera", "yrkar", "bakom", "förslag", "förslaget", "motion", "motionen", "betenkandet", "betänkande", "betänkandet", "utskott", "utskottet", "utredning", "utreda", "tycker", "år", "måste", "regeringen", "mats", "yrka", "lars", "rikta", "vice", "bosse", "ringholm", "mona", "sahlin", "åke", "karlsson", "gustav", "per", "svenerik", "johan", "göteborg", "stockholm", "malmö", "kinberg", "batra", "sten", "annie", "tobias", "ohlsson", "v", "s", "mp", "c", "l", "m", "kd", "sd")

unnecessary_stems <- c("frågan", "fråg", "regering", "sver", "sätt", "procent", "kommun", "tror", "riksdag", "sak", "möj", "problem", "debat", "väld", "se", "stefan", "stat", "valet", "talman", "statsminist",
                       "parti", "välkomn", "deb", "avslut", "uppdrag", "mynd", "karin", "anför", "göran", "persson", "fredrik", "reinfeldt", "svensk", "svensson", "carl", "reservation", "lagstiftning", "lag",
                       "proposition", "majoritet", "hel", "borg", "bifall", "samhället", "tid", "naturligtvis", "villk", "använd", "löfv", "därmed", "ber", "övr", "närvar", "medverk","andersson", "jan",
                       "björklund", "exempel", "odell", "johansson", "åsa", "mari", "larsson", "maud", "olofsson", "ser", "annik", "désiré", "thom", "veckan", "ull", "skap", "vet", "cecili", "diskussion", "försök",
                       "holm", "jen", "delt", "härmed", "bör", "rapport", "mikael", "lägg", "prat", "tal", "hör", "undr", "jul", "igång", "gläd", "överläggning", "slut", "valrör", "hillevi", "tom", "fas", "ans",
                       "deltag", "kamm", "listan", "hunnit", "deltagit", "vilj", "intressant", "ändå", "faktisk", "ställ", "noter", "information", "fall", "grund", "gansk", "välj", "varandr", "talarstol", "påmin", "ledamot",
                       "reinfeld", "minut", "företräd", "dag", "förklar", "öppn", "svar", "pet", "precis", "sänk", "lyssn", "tyd", "kritik", "synpunkt", "uppfattning", "minsk", "å", "opposition", "allians", "borger",
                       "ander", "politik", "står", "gång", "konstater", "socialdemokratisk", "finansminist", "besked", "synpunk", "ärendet", "ståndpunk", "ställningstag", "ledamöt", "ordför", "granskning", "gransk",
                       "framför", "kolleg", "plat", "vel", "hinn", "vidt", "åtgärd", "titt", "känn", "funger", "behöv", "motion", "behandl", "politisk", "avvägning", "effek", "oro", "uttal", "ställd", "statsrådet", "statsråd",
                       "förtyd", "meddel", "informer", "spänn", "satsning", "tog", "igenom", "utredning", "signal", "taget", "huvud", "händ", "såg", "tyck", "b", "änd", "eriksson", "pass", "ställt", "slutet", "ärend",
                       "uppgift", "bedömning", "ager", "landsting", "verksam", "över", "lagt", "bered", "förändring", "återkomm", "kommentar", "jörg", "historisk", "hägglund", "gunill", "besvar", "ulf", "tyvärr", "punk",
                       "aktiv", "frågeställning", "riksdagsledamöt", "garanter", "veck", "men", "tving", "lång", "lös", "lösning", "hitt", "kort", "socialdemokrat", "moderat", "applåd", "berät", "folket", "länsstyr", "enskild",
                       "lyft", "typ", "handl", "förbjud", "historisk", "överenskomm", "bred", "långsikt", "samarbet", "samtal", "innebär", "jon", "green", "tillgäng", "frågestund", "ministr", "varmt", "frågeställ",
                       "hann", "erik", "text", "tillkännagiv", "bord", "nöjd", "start", "för", "interpellant", "inlägg", "följ", "via", "sven", "börj", "riksdagsledamot", "be", "sänd", "inbland", "timm", "artikel", "däremot",
                       "långsikt", "mandatperiod", "hopp", "system", "fel", "omfat", "histori", "hultqvist", "sats", "interpellation", "hjärt", "erik", "kvalitet", "områd", "styr", "avs", "fatt", "regeringskanslier",
                       "riksrevision", "förstår", "näm", "regelverk", "part", "krav", "regl", "g", "ericson", "be", "sänd", "inbland", "vor", "framåt", "sitt", "vis", "beatric", "ask", "röst", "rödgrön", "replik", "monic", "nilsson",
                       "staffan", "emm", "björn", "söd", "oscarsson", "båd", "namn", "lindholm", "välkomm", "välkommet", "littorin", "andre", "carlsson", "person", "röd", "egent", "vor", "klart", "verk", "forum", "samhäll", "process",
                       "fusk", "ansvar", "regeringskansliet", "konstitutionsutskottet", "ku", "ann", "övergrip", "slag", "socialminist", "fysisk", "genomför", "viss", "nya", "betyd", "del", "rikt", "läs", "tänk", "stå", "låt",
                       "tullverket", "justitieminist", "systemet", "uppfat", "hamn", "brist", "skäl", "anledning", "skriv", "lämn", "försvarsminist", "frivil", "len", "ansvarsområd", "kulturminist", "utrikesminist", "åter",
                       "problemet", "hanter", "spel", "roll", "ämn", "parlamentarisk", "följdfråg", "människ", "diskuter", "runt", "rör", "ge", "fortsät","stället", "klar", "hjälp", "kall", "led", "ger", "samtid", "förutsättning",
                       "vikt", "tillbak", "öka", "trot", "end", "åren", "möts", "sker", "vikt", "stället", "stärk", "ny", "svårt", "all", "sen", "hos", "inför", "reform", "höj", "utmaning", "steg", "alldel", "alliansregering",
                       "tillräck", "syn", "dål", "syft", "princip", "pågår", "uttryck", "förändr", "allr", "närm", "pek", "ord", "räkn", "var", "nämn", "vänt", "beslut", "låg", "tas", "direk", "debatter", "sakn", "ske", "bedriv",
                       "månad", "förhåll", "result", "intress", "van", "glad", "nå", "exempelvis", "rad", "såväl", "redovis", "positivt", "negativt", "ansvaret", "snabbt", "snabb", "visst", "snar", "framöv", "vill", "birgit",
                       "interpellationsdebat", "jimmie", "åkesson", "ödmjuk", "kristdemokrat", "s", "christ", "håll", "sett", "själv", "ihop", "funder", "lov", "makt", "driv", "val", "linj", "stopp", "stod", "samt", "allmän", "bestämm", "utred", "särskild", "översyn", "gäll", "lev", "stark", "framtid", "ansv", "sättet", "märk", "bild", "absolut", "ena", "rim", "sid", "tagit", "tank", "konkret", "säkert", "nämnd", "situation", "ytt", "säkerställ", "initiativ", "förbättr", "utgångspunk", "träff", "skick", "drabb", "läst", "upplev", "hem", "först", "skilln", "förstå", "satt", "sidan", "ändr", "hört", "kring", "svår", "fallet", "ambition", "påpek", "funnit", "sammanhang", "självklart", "utifrån", "önsk", "övertyg", "form", "leif", "pettersson", "ohly", "jansson")

TextsV <- speeches_clean[, "anforandetext_cleaned"]
TextsV <- removePunctuation(TextsV)
TextsV <- removeNumbers(TextsV)
TextsV <- tolower(TextsV)
TextsV <- stripWhitespace(TextsV)
TextsV <- removeWords(TextsV, unnecessary_words)

#if stemming is to be used
TextsV <- stemDocument(TextsV, language = "swedish")
TextsV <- stripWhitespace(TextsV) #again to avoid error when removing words after stemming
TextsV <- removeWords(TextsV, unnecessary_stems)
##

speeches_clean$anforandetext_cleaned <- TextsV

write.csv(speeches_clean,"data/speeches_cleaned.csv", row.names = FALSE)
