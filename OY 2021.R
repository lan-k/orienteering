rm(list=ls())


library(tidyverse)
library(lubridate)
library(stringr)



year <- 2021
max_score <- 6
min_events <- 4  ##change to 4 later

folder<-paste0(paste0("../Data/",as.character(year)),"/")


events <- c("SprintChamps.csv","WeilaBunyip.csv","LongChamps.csv",
            "WilpenaSpurs.csv","ImmanuelSprint.csv","MTBO.csv","NightChamps.csv",
            "RockOyster.csv","Paradise.csv","NarrinyeriHills.csv")
S_offered <- c(0,0,1,1,0,0,1,1,0,1)  #short
B_offered <- c(0,1,1,1,0,0,1,1,1,1)  #B class
E_offered <- c(0,1,1,0,0,0,0,0,0,0)   #elite

MTBO = 6
night_champs = 7



##Rock Oyster had two courses for M55
max_short <- sum(S_offered)

##stilll need to add Bunyip-Weila
off1 <- c("Tony Roberts","Jenny Casanova","Aylwin Lim",NA_character_)
off2 <- c("Rob Tucker", "Steve Cooper","Phil Hazell",NA_character_)  
off3 <- c("Simon Uppill","Jenny Casanova",NA_character_,NA_character_)
off4 <- c("Phil Hazell","Robert Smith",NA_character_,NA_character_)
off5 <- c("Rob Tucker","Dante Afnan",NA_character_,NA_character_)
off6 <- c("Lewis Carter","Tyson Hillyard",NA_character_,NA_character_)
off7 <- c("James Lloyd","Julia Lloyd","Ian Grivell","Robert Smith")
off8 <- c("Julia Lloyd","Ian Grivell","Robert Smith",NA_character_)
off9 <- c("Aylwin Lim","Kym Barnett","Robin Uppill",NA_character_)
off10 <- c("Tyson Hillyard","Peter Ashforth",NA_character_,NA_character_)

# fn1 <- paste0(folder, events[1])
# fn2 <- paste0(folder, events[2])
# fn3 <- paste0(folder, events[3])


varlist <- c("Database.Id","Surname","First.name", "Name", "YB","S", 
             "Class","Course","km", "Place","time", "mins_per_km","best",
             "age_group","age_class","age_classS","count_Aclass","count_Sclass",
             "City","casual","event","eventno", "sex_prefix",
             "classB","classS","S_offered","B_offered","E_offered", "points")
results0 <- data.frame()

##NEED TO FIX AGE CLASSES IN MBTO
for (i in seq(length(events))) { 
  fn <- paste0(folder, events[i])
  print(i)
  r <- read.csv(fn, stringsAsFactors = F, na.strings = "")  %>% 
    filter(!is.na(Start), !is.na(Place), !is.na(Time)) %>% 
    rename(Class = Short)  %>%
    #rename(Class = Entry.class..long.)  %>%
    mutate(Name=paste(First.name,Surname,sep = ' '),
           Course = as.numeric(gsub("[^0-9]","", Course)),
           Place = as.numeric(gsub("[^0-9]","", Place)),
           event = substr(events[i],1, nchar(events[i])-4),
           eventno=i,
           casual = as.numeric(substr(City,1,3) == "CAS" | is.na(City)),
           S_offered = S_offered[i],
           B_offered = B_offered[i],
           E_offered = E_offered[i],
           classS = grepl('S', Class),
           classB = grepl("B", Class) & eventno != MTBO,
           classE = grepl("E", Class),
           sex_prefix=if_else(S=="F","W","M"),
           #YB=year(parse_date_time(YB,"y")),
           age=year-YB,
           age_group=case_when(age < 10 ~ 10,
                               age < 21 ~ 2*ceiling(age/2),
                               age >= 21 & age < 35 ~ 21,
                               age > 35 ~ 10*floor((age-5)/10) + 5),
           age_class = strtoi(if_else(classB | (eventno == MTBO & !grepl("MTBO", Class)) ,
                                      NA_character_, substr(Class, 2,3))),
           ##change classes for MTBO
           age_class = ifelse(eventno == MTBO & grepl("MTBO", Class), 
                              as.integer(substr(Class, 7,8)), age_class),
           age_class = ifelse(age_class %in% 17:18, 
                              20, age_group),  #fix classes for 17-20 and 18
           ##change classes for Elites & MTbo
           Class=case_when(E_offered == 1 & substr(Class, 4,5) == "E" ~ gsub("E","A",Class),
                           substr(Class, 2,3) == "18" ~ gsub("18","20",Class),
                           TRUE ~ Class),
           
           count_Aclass = case_when(!classS & !classB & age <21 ~ age <= age_class,
                                    !classS & !classB & age >=21 ~ age >= age_class,
                                    TRUE ~ F), #course counts towards A class
           age_classS=case_when(
              age_group >= 21 & age_group < 45  ~ paste0(sex_prefix, "21AS"),
              age_group >= 45 & age_group < 55  ~ paste0(sex_prefix, paste0(as.character(age_group),"AS")),
              age_group >= 55  ~ paste0(sex_prefix,"55+AS"),
                                  TRUE ~ NA_character_),
           
           count_Sclass = (age >= 21) & ((classS & Class == age_classS) |
                             (!classS & age_group == age_class)),  #course counts towards S class
           comp_class = substr(Class,1,1) %in% c("W","M") &
                                age_class >= 16 ) %>% #| substr(Class,1,1) == "1"
    filter(comp_class, !is.na(Place)) %>%
    # mutate(start=parse_date_time(Start,
    #                              c("%M:%S","%H:%M:%S"), truncated = 3,
    #                              tz = "Australia/Adelaide"),
    #        finish=parse_date_time(Finish,
    #                               c("%M:%S","%H:%M:%S"), truncated = 3,
    #                               tz = "Australia/Adelaide"),
    #        time=as.numeric(difftime(finish, start, units="mins")),
    #        mins_per_km= time/km)  %>%
    mutate(time=hms(Time),
           time2=as.numeric(hour(time)*60 + minute(time) + second(time)/60),
           mins_per_km= time2/km) %>%
    group_by(Class) %>%
    mutate(best=min(mins_per_km, na.rm=T),
           short_offered = max(as.integer(classS))) %>%
    ungroup() %>%
    mutate(points=ifelse((!classB | Class %in% c('WOB','MOB')) ,
                          round(1000*best/mins_per_km ),0),
          Class= case_when(eventno == MTBO & grepl("MTBO", Class) ~ paste0(sex_prefix, age_class,"A"),
                          grepl("55AS", Class) |  grepl("65AS", Class) ~ paste0(sex_prefix, "55+AS"),  #NOL events
                          TRUE ~ Class) ) %>%
    select(all_of(varlist)) 
  
 
  
  results0 <- bind_rows(results0, r)
  
  
}


table(results0$Class, results0$eventno, useNA="always")

##remove casual entries, non SA competitors & fix classes

results <- results0 %>%
  group_by(Database.Id) %>%
  mutate(casual = max(casual, na.rm = T),
         Class=ifelse(Class %in% c("M55A-3","M55A-4"),"M55A", Class)) %>%
   filter(casual != 1, !(City %in% c("A","N","Q","T","V","W")),
          !grepl("Easy",Class),
          !grepl("Non Comp",Name)) %>%  #| classB
  ungroup()


###add points for officials 
officials <- data.frame(rbind(off1, off2,off3, off4, off5, off6, 
                              off7, off8,off9,off10)) 
colnames(officials) <- c("Name1", "Name2","Name3","Name4")
officials <- officials %>%
  mutate(eventno=row_number()) %>%
  pivot_longer(!eventno, values_to="Name") %>%
  select(!name) %>%
  filter(!is.na(Name)) %>%
  group_by(Name) %>%
  slice_head(n=3) %>%
  ungroup()

e <- data.frame(events) %>%
  mutate(event = gsub(".csv","",events),
         eventno=row_number()) %>%
  select(!events)

e$S_offered=S_offered
e$B_offered=B_offered

officials <- officials %>%
  left_join(e)

official_names <- officials %>% select(Name) %>% unique()

official_means <- inner_join(official_names,results,  by="Name")  %>%
  group_by(Database.Id,Name) %>%
  summarise(points = round(mean(points, na.rm=T))) %>%
  ungroup() %>%
  inner_join(officials)

##copy over all the possible classes that officials have entered
official_classes <- inner_join(official_names,
                               results %>% 
                                 select(Database.Id, Name, Class, classS, classB))  %>%
  unique() %>%
  left_join(official_means) %>%
  # filter(!(S_offered ==0 & classS),
  #        !(B_offered ==0 & classB)) %>%
  mutate(official=1, count_Sclass = !classB,
         count_Aclass=!classS & !classB) %>%
  group_by(Name) %>%
  mutate(any_B = any(classB),
         any_S = any(classS),
         any_A = any(count_Aclass))

official_A <- official_classes %>%
  filter(any_A, !classS, !classB) #%>%
  # select(Name, Class, Database.Id, event, eventno, points) %>%
  # unique()


official_S <- official_classes %>%
  filter(any_S) %>%
  select(Name,  classS,S_offered, Database.Id, event, eventno, points) %>%
  unique()


official_B <- official_classes %>%
  filter(any_B, classB) #%>%
  # select(Name, Class, classB, Database.Id, event, eventno, points) %>%
  # unique()



####short classes
r_S <-  results %>% # bind_rows(results, official_S) %>%  #official_classes
  filter(!classB) %>%
  mutate(C21AS = as.integer(Class == "M21AS" | Class == "W21AS")) %>%
  group_by(Database.Id,Name) %>%
  mutate(C21AS= max(C21AS),
         ##highest age S class in night champs is 45AS
         age_classS = ifelse(S_offered == 0 & C21AS == 1, 
                             paste0(sex_prefix, "21AS"),age_classS ) ,    
         Class2 = case_when(
           Class == "M21AS" | Class == "W21AS" ~ Class,
           eventno == night_champs & age_group >= 55 &
             (Class == "M45AS" | Class == "W45AS") ~ age_classS,
           classS ~ Class,
           count_Sclass ~ age_classS,
           TRUE ~ age_classS),
         any_S = any(classS),
         nclass=n_distinct(Class2)
        ) %>%
  filter(any_S,
        nclass == 1
        ) %>%  #can only enter one S class
  group_by(Class2,Database.Id,Name)   %>%
  mutate( sum_S=sum(as.integer(classS),na.rm=T),
          max_S=sum(S_offered,na.rm=T)) %>%
   filter(sum_S == max_S & sum_S > 0) %>%
  ungroup()

off_S <- inner_join(r_S %>% select(Name, Class2) %>% unique(), official_S) %>%
  unique()


r_S <- bind_rows(r_S, off_S)

total_S <- r_S  %>%
  group_by(Class2,Database.Id,Name)   %>%
  mutate(
         num_events = n()) %>%
  filter(num_events >= min_events) %>%
  slice_max(points, n=max_score, with_ties = F) %>%
  summarise(
            sum_S=sum(as.integer(classS),na.rm=T),
            num_events = n(),
            Total = sum(points, na.rm=T))



results_short <- r_S %>%
  # mutate(sum_S=sum(as.integer(classS),na.rm=T),
  #        num_events = n()) %>%
  # filter(sum_S == max_S & sum_S > 0 & num_events >= min_events) %>%
  # slice_max(points, n=max_score, with_ties = F) %>%
  # mutate(Total = sum(points, na.rm=T)) %>%
  filter(!is.na(Class2)) %>%
  select("Class2","Database.Id","Name", event, points) %>%
  pivot_wider(id_cols=c("Class2","Database.Id","Name"), 
              names_from = event,
              values_from = points) %>%
  right_join(total_S) %>%
  group_by(Class2) %>%
  arrange(desc(Total), .by_group = TRUE ) %>%
  rename(Class=Class2)  %>%
  select(!sum_S)



###B classes
r_B <- 
  bind_rows(results, official_B) %>%
  filter(Class %in% c("WOB","MOB") | (B_offered != 1 )) %>% #& age_group==age_class
  group_by(Database.Id, Name) %>%
  mutate(sum_B = sum(as.numeric(classB))) %>% 
  filter(sum_B > 0) %>%
  ungroup()
 

total_B <- r_B %>%  
  group_by(Database.Id,Name, Class) %>%
  slice_max(points, n=max_score, with_ties = F) %>%
  summarise(
         num_events = n(),
         Total = sum(points, na.rm=T)) %>%
  filter(num_events >= min_events) %>%
  ungroup()


results_B <- r_B  %>%
  filter(classB) %>%
  arrange(Name, eventno) %>%
  pivot_wider(id_cols=c("Class","Database.Id","Name"), 
              names_from = event,
              values_from = points) %>%
  right_join(total_B) %>%
  group_by(Class) %>%
  arrange(desc(Total), .by_group = TRUE ) 
  


##A classes without S
r_A <- results %>%
  filter(count_Aclass) 

n_Aclass <- results %>%
  filter(count_Aclass)  %>%  #, is.na(official)
  pivot_wider(id_cols=c("Class","Database.Id","Name"), 
              names_from = event,
              values_from = points) %>%
  group_by(Database.Id,Name) %>%
  mutate(nclass=n(),
         maxclass=max(nclass)) %>%
  filter(maxclass > 1) %>%
  arrange(Database.Id,Name, Class) %>%
  ungroup()   %>%
  select(Database.Id,Name, Class) 



#copy points across classes if distance is same or longer

##all the events where competitors counted for 2 classes

extra_events <- inner_join(r_A, n_Aclass %>% select(Class)) %>%
  select(event, Class, km, best) %>%
  unique()

extra <- full_join(n_Aclass, extra_events, by=c("Class")) %>%
  left_join(r_A ) %>% #%>% filter(is.na(official))
  group_by(Database.Id,Name,event) %>%
  filter(!all(is.na(points))) %>%
  arrange(Name, event, Class) %>%
  mutate(kmcomp = min(km, na.rm=T),
         mins_km_comp = min(mins_per_km, na.rm=T),
         extra = is.na(points) & km <= kmcomp & !all(is.na(points))
         ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(points = ifelse(extra, round(min(1000, 1000*best/mins_km_comp)),
                          points)) %>%
  ungroup() %>%
  filter(!is.na(points)) %>%
  select(!c(kmcomp, mins_km_comp, extra))


n_Aclass_names <- n_Aclass %>%
  select(Database.Id, Name) %>% 
  unique()


res_A <- r_A %>%
  anti_join(n_Aclass_names) %>%
  bind_rows(extra) %>%
  bind_rows(official_A) %>%
  filter(!is.na(points)) %>%
  select(Database.Id,Name, Class, event, points) %>%
  unique() 



##find total points for A class taking into account longer distances and officials

##totals

total_A <-  res_A %>%
  group_by(Database.Id,Name, Class)   %>%
  slice_max(points, n=max_score, with_ties = F) %>%   
  summarise(
         num_events = n(),
         Total = sum(points, na.rm=T))  %>%
  filter(num_events >= min_events) %>%
  ungroup()


results_A <- res_A  %>%
  # slice_max(points, n=max_score, with_ties = F) %>%   
  # mutate(Total = sum(points, na.rm=T),
  #        num_events = n())  %>%
  # filter(num_events >= min_events) %>%
  #group_by(Class) %>%

  pivot_wider(id_cols=c("Class","Database.Id","Name"), #, "Total"
              names_from = event,
              values_from = points) %>%
  right_join(total_A) %>%
  group_by(Class)   %>%
  arrange(desc(Total), .by_group = TRUE ) 


##winners of each class
win_short <- results_short %>%
  group_by(Class) %>% 
  slice_max(Total, n=1, with_ties=T)

win_B <- results_B %>%
  group_by(Class) %>% 
  slice_max(Total, n=1, with_ties=T)


win_A <- results_A %>%
  group_by(Database.Id) %>% 
  slice_max(Total, n=1) %>% ##keep age class with max points 
  ungroup() %>%
  group_by(Class) %>%
  arrange(desc(Total), .by_group = TRUE ) %>%
  slice_max(Total, n=1, with_ties=T)




##save the results

results_all <- bind_rows(results_A, results_short, results_B)

winners_all <- bind_rows(win_A, win_short, win_B)  
#check that no one has won in more than two classes

##export 
write.csv(results_all, file="../results/2021/Totals.csv", na="", row.names=F)
write.csv(winners_all, file="../results/2021/Winners.csv", na="", row.names=F)





