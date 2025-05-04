library(tidyverse)
(who <- tidyr::who)
view(who)

dict_url <- "https://extranet.who.int/tme/generateCSV.asp?ds=dictionary"
if (!file.exists("dict.csv")) download.file(dict_url, "dict.csv")
(dict <- read_csv('dict.csv'))
view(dict)

(labels  <- data.frame(name = colnames(who)))
view(labels)

(explanations <- semi_join(dict, labels, by=c("variable_name" = "name")))
view(explanations)

temp <- who %>% 
  gather(key="notification", cases, 5:60)%>% 
  filter(cases != "")
temp

temp$notification <- str_replace(temp$notification, "newrel", "new_rel")      
temp

temp <- temp %>% 
  separate(notification, into = c("new", "type", "sex"), sep = "_") %>%
  separate(sex, into = c("sex", "age"), sep = 1)
temp

temp <- subset(temp, select = -c(new, iso2, iso3))
temp

ans5 <-temp %>% 
  group_by(country) %>% 
  summarise(count = sum(cases))
ans5

ans6 <- filter(temp, type == "sp") %>%
  group_by(year) %>%
  summarise(count = max(cases))
ans6
