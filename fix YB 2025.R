##fix year of births where missing
## NOT complete

rm(list=ls())


library(tidyverse)
library(lubridate)
library(stringr)


year <- 2025
max_score <- 6
min_events <- 4
min_S <- 2  #minimum number of AS events to be eligible

wallaringa_events <- c(5, 10)  #different format

keep_name = 11

folder<-paste0(paste0("../Data/",as.character(year)),"/")


events <- c("SprintChampsOY2025.csv"
            ,"NgaraltaOY2025.csv"
            ,"TatachillaOY2025.csv" #Tatachilla
            ,"PrelinnaOY2025.csv"
            ,"NarrinyeriHillsOY.csv"   #Narrinyerri
            ,"BriGlenOY2025.csv"  #Bri Glen
            ,"NightChampsOY2025.csv"
            ,"ZellerfeldMiddleChamps2025.csv" #Zellerfeld
            ,"MarneRocksLongChamps2025.csv"  #Marne Rocks
            ,"MTBO.csv" #MBTO
) 




fix_yr = c(wallaringa_events) #  wrong or missing year of births

sa_clubs = c("Onkaparinga Hills Orienteering Club","Tjuringa Orienteers",
             "Yalanga Orienteers","Tintookies Orienteers",
             "Wallaringa Orienteering Club", 
             "Lincoln Orienteers","Saltbush Orienteers",
             "Top End Orienteers",
             "OH S","TJ S","TT S","YA S","WA S",
             "SB S","LI S", "TE S","OSA",
             "S")

results0 <- data.frame()


for (i in seq(length(events))) { 
  fn <- paste0(folder, events[i])
  print(i)
  r <- read.csv(fn, stringsAsFactors = F, na.strings = "")   
  
  
  if (i == keep_name) {
    r <- r %>%
      filter( !is.na(Place) | Place !=0, !is.na(Time)) %>%
      mutate(YB = as.numeric(YOB)) %>%
      rename( City = Club) 
    
  } else {
    r <- r %>% 
      mutate(Name=paste(First.name,Surname,sep = ' ')) %>% 
      rename(Class = Short)  
    
  }
       
  
  
  if (i %in% fix_yr) {
    
    r <- r %>%
      mutate(YB=NA) 
    
  } 
  
  
  r <- r %>%
    mutate(comp_class = (substr(Class,1,1) %in% c("W","M") | Class %in% c("MOB","WOB")))  %>%
    select("Database.Id","Surname","First.name", "Name",comp_class,City,S, YB)
 
  results0 <- bind_rows(results0, r)
  
  
}

##fix age_class for event with missing year of birth

age_group <- results0 %>%
  group_by(Name) %>%
  filter(!all(is.na(YB))) %>%
  mutate(YB =  min(YB, na.rm = T)) %>%
  filter(comp_class, City %in% sa_clubs) %>%
  select("Database.Id","Surname","First.name",S, Name, YB) %>% #,
  arrange(Database.Id) %>%
  slice_head() %>%  
  ungroup() 

##add in missing DOB from previous year

load("YB_2024.rds")

yb = full_join(yb, age_group %>% select(!Database.Id))

save(yb, file="age_groups_2025.rds")
