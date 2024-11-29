

#load packages
library(tidyverse)
library(data.table)
library(stm)
library(quanteda)
library(tidytext)
library(lubridate)
library(readxl)

#read speech data
speeches <- fread("data/speeches_cleaned.csv")

#Create DFM
speeches_corpus <- corpus(x = speeches, text_field = "anforandetext_cleaned")
speeches_dfm <- dfm(tokens(speeches_corpus))

set.seed(679234)
save.results=FALSE
rerun=FALSE
if(!file.exists("output/topic_model_k0.RData") | rerun) {
  topic_model_k0 <-stm(speeches_dfm, K = 0, init.type = "Spectral", data = docvars(speeches_corpus)) #This is the topic model
  if(save.results) { saveRDS(topic_model_k0,file="output/topic_model_k0.RData") }
} else {
  topic_model_k0 <- readRDS("output/topic_model_k0.RData")
}


#examine topics
#Topic definitions

topic_model <- topic_model_k0
labels<-labelTopics(topic_model, c(1:82))


#Calculate Socio-economic and Socio-cultural Salience in Speeches
#Gamma shows the probability that a document belongs to a topic.
td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(speeches_dfm))
td_gamma <- dcast(data = td_gamma, document ~ topic) #transform from long to wide format

td_gamma$militarism <- rowSums(x = td_gamma[,c("16", "45", "55")])

#Add together gammas for topics that are about socio-economic and socio-cultural issues, respectively.
td_gamma$socio_cultural <- rowSums(x = td_gamma[,c("2", "6", "8", "10", "11", "12", "16", "18", "26", "27", "31", "32", "34", "38", "40", "42", "43", "44", "45", "50", "52", "54", "55", "56", "67", "68", "69", "71", "73", "77", "80" )])
td_gamma$socio_economic <- rowSums(x = td_gamma[,c("1", "3", "4", "7", "9", "13", "14", "15", "17", "20", "21", "22", "24", "30", "35", "36", "37", "41", "46", "47", "49", "51", "53", "57", "60", "61", "62", "64", "65", "66", "72", "75", "76", "78", "81")])
td_gamma$other_issues <- rowSums(x = td_gamma[,c("5", "19", "23", "25", "28", "29", "33", "39", "48", "58", "59", "63", "70", "74", "79", "82")])

#Merge the new gamma variables with the meta_data into "data"
data <- td_gamma
meta_data <- docvars(speeches_dfm)
meta_data$document <- speeches_dfm@Dimnames$docs
data$parti <- meta_data$parti[match(data$document, meta_data$document)] #party 
data$dok_datum <- meta_data$dok_datum[match(data$document, meta_data$document)] #date
data$anforande_id <- meta_data$anforande_id[match(data$document, meta_data$document)] #ID

#Sort on date
data <- data %>%
  mutate(dok_datum = as.Date(dok_datum)) %>%
  arrange(dok_datum)


#Calculate the Salience of Socio-economic and Socio-cultural Issues per Month
#create a variable capturing time in months instead of days

# Convert the string to Date type first
data$dok_datum <- as.Date(data$dok_datum, format = "%Y-%m-%d")

# Floor the dates to the nearest month
data$time <- floor_date(data$dok_datum, unit = "month")

#make monthly salience of issue-variables for:
#total
data_monthly_total <- data %>%
  group_by(time) %>%
  summarise(socio_cultural_total = mean(socio_cultural),
            socio_economic_total = mean(socio_economic),
            other_issues_total = mean(other_issues),
            militarism = mean(militarism))

#established parties
data_monthly_established <- data %>%
  mutate(established_party = ifelse(parti != "sd", 1, 0)) %>%
  group_by(time) %>%
  filter(established_party == 1) %>%
  summarise(socio_cultural_established = mean(socio_cultural),
            socio_economic_established = mean(socio_economic),
            other_issues_established = mean(other_issues),
            militarism_established = mean(militarism))

#sd
data_monthly_sd <- data %>%
  mutate(established_party = ifelse(parti != "sd", 1, 0)) %>%
  group_by(time) %>%
  filter(established_party == 0) %>%
  summarise(socio_cultural_sd = mean(socio_cultural),
            socio_economic_sd = mean(socio_economic),
            other_issues_sd = mean(other_issues),
            militarism_sd = mean(militarism))

#join variables with time series variables
ts_data <- read_excel("data/variables.xlsx") %>% 
  rename(time = "Time")

ts_data$time <- as.Date(ts_data$time)
ts_data <- inner_join(ts_data, data_monthly_total, by = "time") #remove NA, months with no speeches
ts_data <- left_join(ts_data, data_monthly_established, by = "time")
ts_data <- left_join(ts_data, data_monthly_sd, by = "time")

saveRDS(ts_data,file="ts_data.RData")
