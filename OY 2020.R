library(tidyverse)
library(lubridate)
library(stringr)

rm(list=ls())

year <- 2020
max_score <- 6
min_events <- 2  ##change to 4 later

folder<-paste0(paste0("../Data/",as.character(year)),"/")


events <- c("PewseyValeOY.csv","TundarriOY.csv","MulgaValleyOY.csv")
S_offered <- c(1,0,1)
B_offered <- c(1,1,1)

max_short <- sum(S_offered)

setter <- c("David George","","Phil Hazell")
controller <- c("Phil Hazell","","Ken Thompson")

fn1 <- paste0(folder, events[1])
fn2 <- paste0(folder, events[2])
fn3 <- paste0(folder, events[3])


varlist <- c("Database.Id","Surname","First.name", "Name", "YB","S", 
             "Class","km", "Place", "mins_per_km",
             "age_group","age_class","age_classS","count_Aclass","count_Sclass",
             "event","eventno", "sex_prefix",
             "classB","classS","S_offered","B_offered", "points")
results <- data.frame()


for (i in seq(length(events))) { 
  fn <- paste0(folder, events[i])
  
  r <- read.csv(fn, stringsAsFactors = F)  %>% 
    filter(!is.na(Start) & !is.na(Place)) %>% 
    rename(Class = Entry.class..long.)  %>%
    mutate(Name=paste(First.name,Surname,sep = ' '),
           event = substr(events[i],1, nchar(events[i])-4),
           eventno=i,
           S_offered = S_offered[i],
           B_offered = B_offered[i],
           classS = grepl('S', Class),
           classB = grepl("B", Class),
           sex_prefix=if_else(S=="F","W","M"),
           age=year-YB,
           age_group=case_when(age < 10 ~ 10,
                               age < 21 ~ 2*ceiling(age/2),
                               age >= 21 & age < 35 ~ 21,
                               age > 35 ~ 10*floor((age-5)/10) + 5),
           age_class = strtoi(if_else(classB, NA_character_,
                                          substr(Class, 2,3))),
           count_Aclass = case_when(!classS & !classB & age <21 ~ age <= age_class,
                                    !classS & !classB & age >=21 ~ age >= age_class,
                                    TRUE ~ F), #course counts towards A class
           age_classS=case_when(
              age >= 21 & age < 55  ~ paste0(sex_prefix, paste0(as.character(age_group),"AS")),
              age >= 55  ~ paste0(sex_prefix,"55+AS"),
                                  TRUE ~ NA_character_),
           count_Sclass = (age >= 21) & ((classS & Class == age_classS) |
                             (!classS & age_group == age_class)),  #course counts towards S class
           comp_class = substr(Class,1,1) %in% c("W","M")) %>%
    filter(comp_class) %>%
    mutate(start=parse_date_time(Start,
                                 c("%M:%S","%H:%M:%S"), truncated = 3,
                                 tz = "Australia/Adelaide"),
           finish=parse_date_time(Finish,
                                  c("%M:%S","%H:%M:%S"), truncated = 3,
                                  tz = "Australia/Adelaide"),
           time=as.numeric(difftime(finish, start, units="mins")),
           mins_per_km= time/km)  %>%
    group_by(Class) %>%
    mutate(best=min(mins_per_km, na.rm=T),
           short_offered = max(as.integer(classS))) %>%
    ungroup() %>%
    mutate(points=if_else((!classB | Class %in% c('WOB','MOB')) ,
                          round(1000*best/mins_per_km ),0)) %>%
    select(all_of(varlist)) 
  
 
  
  results <- bind_rows(results, r)
  
  
}

####short classes
results_short <- results %>%
  mutate(C21AS = as.integer(Class == "M21AS" | Class == "W21AS")) %>%
  group_by(Database.Id,Name) %>%
  mutate(C21AS= max(C21AS),
    age_classS = if_else(S_offered == 0 & C21AS == 1, 
                         paste0(sex_prefix, "21AS"),age_classS ) ,    
    Class2 = case_when(
    Class == "M21AS" | Class == "W21AS" ~ Class,
    classS ~ Class,
    count_Sclass ~ age_classS,
    TRUE ~ age_classS),
    max_S=sum(S_offered,na.rm=T)) %>%
  group_by(Class2,Database.Id,Name) %>%
  mutate(sum_S=sum(as.integer(classS),na.rm=T),
         num_events = n()) %>%
  filter(sum_S == max_S & sum_S > 0 & num_events >= min_events) %>%
  slice_max(points, n=max_score) %>%
  mutate(Total = sum(points, na.rm=T)) %>%
  pivot_wider(id_cols=c("Class2","Database.Id","Name", "Total"), 
              names_from = event,
              values_from = points) %>%
  group_by(Class2) %>%
  arrange(desc(Total), .by_group = TRUE ) %>%
  rename(Class=Class2)

##A classes without S
results_A <- results %>%
  filter(count_Aclass) %>%
  group_by(Database.Id, Class) %>%
  ungroup()


###B classes
results_B <- results %>%
  filter(Class %in% c("WOB","MOB") | (B_offered != 1 & age_group==age_class)) %>%
  group_by(Class, Database.Id,Name) %>%
  slice_max(points, n=max_score) %>%
  mutate(Total = sum(points, na.rm=T),
         num_events = n(),
         num_events = sum(num_events >= min_events)) %>%
  filter() %>%
  pivot_wider(id_cols=c("Class","Database.Id","Name", "Total"), 
                        names_from = event,
                        values_from = points) %>%
  group_by(Class) %>%
  arrange(desc(Total), .by_group = TRUE )



#calculate total points per class
results_total <- results %>%
  group_by(Database.Id,Name, Class) %>%
  summarise(points_total = sum(points, na.rm=T), .groups = 'drop') %>%
  ungroup()



###add points for officials
official_total <- data.frame(controller) %>%
  rename(official = controller) %>%
  bind_rows(data.frame(setter) %>%  rename(official = setter)) %>%
  group_by(official) %>%
  summarise(n=as.integer(n()), .groups = 'drop') %>%
  mutate(n_official=if_else(n <= 2L, n, 2L)) %>%
  select(-n)
  

results <- left_join(results, official_total, by=c("Name"="official"))


##find winners per class



