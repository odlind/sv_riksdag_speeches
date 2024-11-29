

library(slider)
install.packages("dplyr")
library(dplyr)


readRDS("ts_data.RData")
ts <- ts_data

###### SD OPINION ######

#smoothing
sd_opinion_sliding_3m <- slide_dfr(ts$sd_opinion, mean,.before = 3) #3 months 
ts$sd_opinion_sliding_3m <- sd_opinion_sliding_3m$...1
sd_opinion_sliding_6m <- slide_dfr(ts$sd_opinion, mean,.before = 6) #6 months
ts$sd_opinion_sliding_6m <- sd_opinion_sliding_6m$...1
sd_opinion_sliding_12m <- slide_dfr(ts$sd_opinion, mean,.before = 12) #12 months
ts$sd_opinion_sliding_12m <- sd_opinion_sliding_12m$...1

#plot
df <- ts %>%
  dplyr::select(time, "None" = sd_opinion, "Three Months" = sd_opinion_sliding_3m, "Six Months" = sd_opinion_sliding_6m, "Twelve Months" = sd_opinion_sliding_12m) %>%
  gather(key = "Sliding", value = "Percent", -time)
head(df, 3)

ggplot(df, aes(x = time, y = Percent)) + 
  geom_line(aes(color = Sliding), size = 1.1) +
  scale_color_manual(values = c("gray", "red", "blue", "Green"), breaks = c("None","Three Months","Six Months","Twelve Months"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = "bottom", panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), line = element_line(size = 0.5), legend.title = element_blank(), axis.text = element_text(size = 12), plot.title = element_text(size = 5),panel.background = element_rect(fill = "white"),panel.border = element_rect(colour = "black", fill=NA) ,axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), legend.text = element_text(size = 14)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")



###### SOCIO CULTURAL ESTABLISHED ######

ts$socio_cultural_established <- ts$socio_cultural_established*100 #make into %

socio_cultural_established_3m <- slide_dfr(ts$socio_cultural_established, mean,.before = 3) #3 months
ts$socio_cultural_established_3m <- socio_cultural_established_3m$...1
socio_cultural_established_6m <- slide_dfr(ts$socio_cultural_established, mean,.before = 6) #6 months
ts$socio_cultural_established_6m <- socio_cultural_established_6m$...1
socio_cultural_established_12m <- slide_dfr(ts$socio_cultural_established, mean,.before = 12) #12 months
ts$socio_cultural_established_12m <- socio_cultural_established_12m$...1

###### SOCIO ECONOMIC ESTABLISHED ######
ts$socio_economic_established <- ts$socio_economic_established*100 #make into %
socio_economic_established_6m <- slide_dfr(ts$socio_economic_established, mean,.before = 6) #6 months
ts$socio_economic_established_6m <- socio_economic_established_6m$...1
socio_economic_established_12m <- slide_dfr(ts$socio_economic_established, mean,.before = 12) #12 months
ts$socio_economic_established_12m <- socio_economic_established_12m$...1

###### OTHER ISSUES ESTABLISHED ######
ts$other_issues_established <- ts$other_issues_established*100 #make into %
other_issues_established_6m <- slide_dfr(ts$other_issues_established, mean,.before = 6) #6 months
ts$other_issues_established_6m <- other_issues_established_6m$...1
other_issues_established_12m <- slide_dfr(ts$other_issues_established, mean,.before = 12) #12 months
ts$other_issues_established_12m <- other_issues_established_12m$...1

###### OTHER ISSUES ESTABLISHED ######
ts$militarism_established <- ts$militarism_established*100 #make into %
militarism_established_3m <- slide_dfr(ts$militarism_established, mean,.before = 3) #3 months
ts$militarism_established_3m <- militarism_established_3m$...1
militarism_established_6m <- slide_dfr(ts$militarism_established, mean,.before = 6) #6 months
ts$militarism_established_6m <- militarism_established_6m$...1
militarism_established_12m <- slide_dfr(ts$militarism_established, mean,.before = 12) #12 months
ts$militarism_established_12m <- militarism_established_12m$...1

#plot
df <- ts %>%
  dplyr::select(time, "None" = militarism_established, "Three Months" = militarism_established_3m, "Six Months" = militarism_established_6m, "Twelve Months" = militarism_established_12m) %>%
  gather(key = "Sliding", value = "Percent", -time)
head(df, 3)

ggplot(df, aes(x = time, y = Percent)) + 
  geom_line(aes(color = Sliding), size = 1.1) +
  scale_color_manual(values = c("gray", "red", "blue", "Green"), breaks = c("None","Three Months","Six Months","Twelve Months"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = "bottom", panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), line = element_line(size = 0.5), legend.title = element_blank(), axis.text = element_text(size = 12), plot.title = element_text(size = 5),panel.background = element_rect(fill = "white"),panel.border = element_rect(colour = "black", fill=NA) ,axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), legend.text = element_text(size = 14)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


#plot the development over time for socio-cultural, socio-economic and other issues
df <- ts %>%
  dplyr::select(time, "Socio Cultural" = socio_cultural_established_12m, "Socio Economic" = socio_economic_established_12m, "Other Issues" = other_issues_established_12m) %>%
  gather(key = "Variable", value = "Percent", -time)
head(df, 3)


ggplot(df, aes(x = time, y = Percent)) + 
  geom_line(aes(color = Variable), size = 1.4) +
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = "bottom", panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), line = element_line(size = 0.5), legend.title = element_blank(), axis.text = element_text(size = 12), plot.title = element_text(size = 5),panel.background = element_rect(fill = "white"),panel.border = element_rect(colour = "black", fill=NA) ,axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), legend.text = element_text(size = 14)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_color_manual(values = c("red", "blue", "gray"), breaks = c("Socio Cultural","Socio Economic","Other Issues"))



df <- data %>%
  dplyr::select(time, militarism, `16`, `45`, `55`) %>%
  gather(key = "Sliding", value = "Percent", -time)
head(df, 3)

ggplot(df, aes(x = time, y = Percent)) + 
  geom_line(aes(color = Sliding), size = 1.1) +
  scale_color_manual(values = c("gray", "red", "blue", "green"), breaks = c("militarism","16","45","55"))+
  theme(axis.text.x=element_text(angle=60, hjust=1), legend.position = "bottom", panel.grid.major = element_line(colour = "gray"), panel.grid.minor = element_blank(), line = element_line(size = 0.5), legend.title = element_blank(), axis.text = element_text(size = 12), plot.title = element_text(size = 5),panel.background = element_rect(fill = "white"),panel.border = element_rect(colour = "black", fill=NA) ,axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), legend.text = element_text(size = 14)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

