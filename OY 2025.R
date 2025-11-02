##changes to AS course rules
## no need to always enter AS course, must have at least 2 during the year in correct age class 

##Use Place  = missing OR 0
#ADD 21AS for sub 20 year S class

rm(list=ls())


library(tidyverse)
library(lubridate)
library(stringr)


year <- 2025
max_score <- 6
min_events <- 4
min_S <- 2  #minimum number of AS events to be eligible

wallaringa_events <- c(5, 10)  #different format

folder<-paste0(paste0("../Data/",as.character(year)),"/")
results_folder = paste0(paste0("../Results/",as.character(year)))

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
load("age_groups_2025.rds") 


keep_name = 11 # event number of full names instead of first and last names, none in 2025

S_offered <- c(0,1,0,1,1,1,1,0,1,0)  #short 
B_offered <- c(0,1,0,1,1,1,1,1,1,0)  #B class 
E_offered <- c(0,0,0,0,0,0,0,0,0,0)   #elite 

night_champs = 7
MTBO = 10 ##need to fix riding in an older class in the MBTO

max_short <- sum(S_offered)

sa_clubs = c("Onkaparinga Hills Orienteering Club","Tjuringa Orienteers",
             "Yalanga Orienteers","Tintookies Orienteers",
             "Wallaringa Orienteering Club", 
             "Lincoln Orienteers","Saltbush Orienteers",
             "Top End Orienteers",
             "OH S","TJ S","TT S","YA S","WA S",
             "SB S","LI S", "TE S", "OSA",
             "S")

#officials
off1 <- c("Jack Marschall","Kym Barnett",NA_character_,NA_character_)
off2 <- c("Joanna George","Phil Hazell",NA_character_,NA_character_)  
off3 <- c("Lan Kelly","Ben Cazzolato","Toby Cazzolato",NA_character_) 
off4 <- c("Evelyn Colwell","",NA_character_,NA_character_)
off5 <- c("Jessica Jarvis","Jenny Casanova","Mark Overton",NA_character_)
off6 <- c("Ethan Penck","Nigel Dobson",NA_character_,NA_character_)
off7 <- c("Duncan Still","Robert Boekelaar","Natalie Still",NA_character_)  #
off8 <- c("Adrian Uppill","Tyson Hillyard",NA_character_,NA_character_)
off9 <- c("Steve Cooper","Phil Hazell",NA_character_,NA_character_)
off10 <- c("Ryan Farquharson","",NA_character_,NA_character_)


officials <- data.frame(rbind(off1, off2,off3,
                              off4, off5, 
                              off6, 
                              off7, 
                              off8,
                              off9
                              ,off10
                              )) 
                              


colnames(officials) <- c("Name1", "Name2","Name3", "Name4")


# fn1 <- paste0(folder, events[1])
# fn2 <- paste0(folder, events[2])
# fn3 <- paste0(folder, events[3])


varlist <- c("Surname","First.name", "Name", "YB","S", "Class", #"Database.Id","Course",
             "km", "Place","time", "mins_per_km","best",
             "age_group","age_class","age_classS","count_Aclass","count_Sclass",
             "City","casual","event","eventno", "sex_prefix",
             "classB","classS","S_offered","B_offered"#,"E_offered"
             , "points")

results0 <- data.frame()


for (i in seq(length(events))) { 
  fn <- paste0(folder, events[i])
  
  print(i)
  r <- read.csv(fn, stringsAsFactors = F, na.strings = "") 
  # create_name = any(names(r) == "Surname")
  
  
  r <- r %>% 
    mutate(time=hms(Time),
           time2=as.numeric(hour(time)*60 + minute(time) + second(time)/60),
            mins_per_km= time2/km)
  
     
  
  
  
  if (i %in% wallaringa_events) {
    r <- r %>%
      # mutate(YB=as.numeric(YB),
      #        YB=ifelse(YB > year-2000, YB+1900, YB+2000)) %>%
      select(!YB) %>%
      rename(Place=Pl)
  } 
  
  
  if (i == keep_name) {
    r <- r %>%
      filter( !is.na(Place) | Place !=0, !is.na(Time)) %>%
      mutate(YB = as.numeric(YOB),
             km=CourseLength.m./1000) %>%
      rename( City = Club) %>%
      select(!c(YOB, km=CourseLength.m.))
    
  } else {
    r <- r %>% 
      mutate(Name=paste(First.name,Surname,sep = ' ')) %>% 
      filter(!is.na(Start), !is.na(Place) | Place !=0, !is.na(Time)) %>% 
      rename(Class = Short)  
    
  }
  
  
  ##find best in Class
  r <- r %>%
    mutate(# Course = as.numeric(gsub("[^0-9]","", Course)),
      Place = as.numeric(gsub("[^0-9]","", Place)),
      event = substr(events[i],1, nchar(events[i])-4),
      eventno=i,
      City = str_replace_all(City, "[^[:alnum:]]", " "),
      casual = as.numeric(substr(City,1,3) == "CAS" | is.na(City)),
      S_offered = S_offered[i],
      B_offered = B_offered[i],
      E_offered = E_offered[i],
      classS = grepl('S', Class),
      classB = grepl("B", Class)  , #& eventno != MTBO,
      classE = grepl("E", Class),
      
      ##change classes for Elites 
      Class_orig = Class,
      Class=case_when(E_offered == 1 & substr(Class, 4,5) == "E" ~ gsub("E","A",Class),
                      substr(Class, 2,3) == "18" ~ gsub("18","20",Class),
                      grepl("JuniorB", Class) ~ gsub("JuniorB","OB",Class),
                      grepl("17-20", Class) ~ gsub("17-20","20",Class),
                      Class == "W45B" ~ "WOB",
                      TRUE ~ Class),,
      sex_prefix=if_else(S=="F","W","M"),
      Class= case_when(# grepl("55AS", Class) |  grepl("65AS", Class) ~ paste0(sex_prefix, "55+AS"),  #NOL events
        grepl("65AS", Class) |  grepl("65+AS", Class) ~ paste0(sex_prefix, "65+AS"),
        TRUE ~ Class)  ) %>%
    group_by(Class) %>%
    mutate(best=min(mins_per_km, na.rm=T),
           short_offered = max(as.integer(classS))) %>%
    ungroup() %>%
    mutate(points=ifelse((!classB | Class %in% c('WOB','MOB')) ,
                         round(1000*best/mins_per_km ),0))
  
  
  
  if (i %in% fix_yr) {
    
    r <- r %>%
   left_join(yb  ) #%>% select(!Database.Id)

  }
  
  
  
  r <- r %>%
    #rename(Class = Entry.class..long.)  %>%
    mutate(
          
           age=as.numeric(year-YB),
           age_group=case_when(age < 10 ~ 10,
                               age < 21 ~ 2*ceiling(age/2),
                               age >= 21 & age < 35 ~ 21,
                               age >= 35 ~ 10*floor((age-5)/10) + 5,
                               TRUE ~ NA),
           age_group = ifelse(age_group == 18, 20, age_group),
           age_class = strtoi(if_else(classB  , #& | (eventno == MTBO )!grepl("MTBO", Class))
                                      NA_character_, substr(Class, 2,3))),
           #change classes for MTBO
           age_class = ifelse(eventno == MTBO , age_group, age_class),
           age_class = ifelse(!is.na(age_group) & between(age_group, 17, 18),
                              20, age_class),  #fix classes for 17-20 and 18
          
           count_Aclass = case_when(!classS & !classB & age <21 ~ age <= age_class,
                                    !classS & !classB & age >=21 ~ age >= age_class,
                                    TRUE ~ F), #course counts towards A class
           age_classS=case_when(
              age_group <= 20 ~ paste0(sex_prefix, "21AS"),
              age_group >= 21 & age_group < 45  ~ paste0(sex_prefix, "21AS"),
              # age_group >= 45 & age_group < 55  ~ paste0(sex_prefix, paste0(as.character(age_group),"AS")),
              # age_group >= 55  ~ paste0(sex_prefix,"55+AS"),
              age_group >= 45 & age_group < 65  ~ paste0(sex_prefix, paste0(as.character(age_group),"AS")),
              age_group >= 65  ~ paste0(sex_prefix,"65+AS"),
                                  TRUE ~ NA_character_),
           
           count_Sclass =  ((classS & Class == age_classS) |   #(age >= 21) &
                             (!classS & age_group == age_class & S_offered == 0)),  #course counts towards S class
           count_Sclass = ifelse(eventno == night_champs & age_group >= 55,
                                 grepl('45AS', Class), count_Sclass),
           comp_class = (substr(Class,1,1) %in% c("W","M") &
                                age_class >= 16 ) | Class %in% c("MOB","WOB")) %>% #| substr(Class,1,1) == "1"
    filter( !is.na(Place) | Place !=0, comp_class)# %>%  , comp_class
    # mutate(start=parse_date_time(Start,
    #                              c("%M:%S","%H:%M:%S"), truncated = 3,
    #                              tz = "Australia/Adelaide"),
    #        finish=parse_date_time(Finish,
    #                               c("%M:%S","%H:%M:%S"), truncated = 3,
    #                               tz = "Australia/Adelaide"),
    #        time=as.numeric(difftime(finish, start, units="mins")),
    #        mins_per_km= time/km)  %>%
    # mutate(time=hms(Time),
    #        time2=as.numeric(hour(time)*60 + minute(time) + second(time)/60),
    #        mins_per_km= time2/km) 
  
  
  
  
  #fix MTBO Classes
  r <- r %>%
    mutate(MTBO_age_class = ifelse(eventno == MTBO,
                                   as.numeric(str_extract(Class_orig, "[0-9]+")), NA),
           MTBO_age_class = ifelse(!is.na(MTBO_age_class) & MTBO_age_class <= 21 , 
                                   MTBO_age_class, age_class),
           Class= ifelse(eventno == MTBO,
                              paste0(sex_prefix, MTBO_age_class,"A"), Class) ) %>%
    select(all_of(varlist)) 
  
  
 
  
  results0 <- bind_rows(results0, r)
  
  
}

# check missing year of birth - casuals?
summary(results0$YB)

miss_yb = results0 %>% filter(is.na(YB))



table(results0$age_group, results0$eventno, useNA="always")
table(results0$Class, results0$eventno, useNA="always")



##remove casual entries, non SA competitors & fix classes
table(results0$City, useNA="always")

results <- results0 %>%
  mutate(sa_club = as.numeric(City %in% sa_clubs)) %>%
  group_by(Name) %>%
  mutate(sa_club = max(sa_club, na.rm = T))%>% # for people who because members later
  ungroup() %>%
  filter(sa_club== 1,
     #, !(City %in% c("A","N","Q","T","V","W")
         !grepl("Easy",Class),
         !grepl("Non Comp",Name)) 

table(results$City, results$sa_club, useNA="always")
table(results$event, useNA="always")



check = results %>% filter(City == "CAS S") 
#check Casuals - all entered incorrectly 


###add points for officials 

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
  group_by(Name) %>% #Database.Id,
  summarise(points = round(mean(points, na.rm=T))) %>%
  ungroup() %>%
  inner_join(officials)

##copy over all the possible classes that officials have entered
official_classes <- inner_join(official_names,
                               results %>% 
                                 select(Name, Class, classS, classB), #Database.Id, 
                               relationship = "many-to-many")  %>%
  unique() %>%
  left_join(official_means,
            relationship = "many-to-many") %>%
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
  filter(any_S, classS) %>% #, S_offered == 1
  select(Name,  classS,S_offered,  event, eventno, points) %>% #Database.Id,
  unique()


official_B <- official_classes %>%
  filter(any_B, classB) #%>%
  # select(Name, Class, classB, Database.Id, event, eventno, points) %>%
  # unique()



####short classes
##2024 no longer need to enter every S class at every event
r_S <-  results %>% # bind_rows(results, official_S) %>%  #official_classes
  filter(count_Sclass) %>%
  mutate(C21AS = as.integer(Class == "M21AS" | Class == "W21AS")) %>%
  group_by(Name) %>% #Database.Id,
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
        nclass == 1,
        # count_Sclass
        ) %>%  #can only enter one S class
  group_by(Class2,Name)   %>% #Database.Id,
  mutate( sum_S=sum(as.integer(classS),na.rm=T),
          max_S=sum(S_offered,na.rm=T)) %>%
   # filter(sum_S == max_S & sum_S > 0) %>%
  filter(sum_S >= min_S ) %>%  #changed eligibility to at least 2AS events
  ungroup()


off_S <- inner_join(r_S %>% select(Name, Class2) %>% unique(), official_S) %>%
  unique()



r_S <- bind_rows(r_S, off_S)


total_S <- r_S  %>%
  group_by(Class2,Name)   %>% #Database.Id,
  mutate(
         num_events = n()) %>%
  # filter(num_events >= min_events) %>%
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
  select("Class2","Name", event, points) %>% #"Database.Id",
  pivot_wider(id_cols=c("Class2","Name"), #"Database.Id",
              names_from = event,
              values_from = points) %>%
  right_join(total_S) %>%
  group_by(Class2) %>%
  arrange(desc(Total), .by_group = TRUE ) %>%
  rename(Class=Class2)  %>%
  select(!sum_S) %>%
  mutate(type="AS")



###B classes
r_B <- 
  bind_rows(results, official_B) %>%
  filter(Class %in% c("WOB","MOB") | (B_offered != 1 )) %>% #& age_group==age_class
  group_by( Name) %>% #Database.Id,
  mutate(sum_B = sum(as.numeric(classB))) %>% 
  filter(sum_B > 0) %>%
  mutate(Class = paste0(substr(Class, 1,1),"OB")) %>%
  ungroup()
 

total_B <- r_B %>%  
  group_by(Name, Class) %>% #Database.Id,
  slice_max(points, n=max_score, with_ties = F) %>%
  summarise(
         num_events = n(),
         Total = sum(points, na.rm=T)) %>%
  ungroup()


#no one did at least 4 events in 2025
results_B <- r_B  %>%
  filter(classB | (B_offered != 1 )) %>%
  arrange(Name, eventno) %>%
  pivot_wider(id_cols=c("Class","Name"), #"Database.Id",
              names_from = event,
              values_from = points) %>%
  right_join(total_B) %>%
  # filter(num_events >= min_events) %>%
  group_by(Class) %>%
  arrange(desc(Total), .by_group = TRUE )  %>%
  mutate(type="B")
  


##A classes without S
##need to sort out MTBO who don't enter their class
r_A <- results %>%
  filter(count_Aclass) 


n_Aclass <- results %>%
  filter(count_Aclass)  %>%  #, is.na(official)
  pivot_wider(id_cols=c("Class","Name"),  #"Database.Id",
              names_from = event,
              values_from = points) %>%
  group_by(Name) %>% #Database.Id,
  mutate(nclass=n(),
         maxclass=max(nclass)) %>%
  filter(maxclass > 1) %>%
  arrange(Name, Class) %>% #Database.Id,
  ungroup()   %>%
  select(Name, Class) #Database.Id,



#copy points across classes if distance is same or longer

##all the events where competitors counted for 2 classes

extra_events <- inner_join(r_A, n_Aclass %>% select(Class),
                           relationship = "many-to-many") %>%
  select(event, Class, km, best) %>%
  unique()

##need to allow many to many relationship due to officals being in multiple age classes
extra <- full_join(n_Aclass, extra_events, 
                   # by=c("Class"),
                   relationship = "many-to-many"
                   ) %>%
  left_join(r_A ) %>% #%>% filter(is.na(official))
  group_by(Name,event) %>% #Database.Id,
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

##calculate extra points for classes where there were no competitors
extra_nocomp <- full_join(n_Aclass, extra_events, 
                          # by=c("Class"),
                          relationship = "many-to-many"
                          ) %>%
  left_join(r_A ) %>% #%>% filter(is.na(official))
  group_by(Name,event) %>%  #Database.Id,
  filter(!all(is.na(points))) %>%
  group_by(Name)  %>% #Database.Id,
  arrange(Class) %>%
  mutate(age_group=max(age_group, na.rm=T), 
         Class1=min(Class), Class2=max(Class),
         Class_extra = ifelse(Class == Class1, Class2, Class1)) %>%
  ungroup() %>%
  filter(!is.na(Class_extra)) %>%
  mutate(keep=ifelse(age_group < 21,  Class_extra < Class, 
                     Class_extra > Class)) %>%
  filter(keep) %>%
  mutate(Class=Class_extra, points=1000) 
#allocate 1000 points if there are no competitors in the other class 


extra_nocomp <- anti_join(extra_nocomp %>% 
                            select(!c(Class1, Class2, Class_extra, keep)),
                          extra %>% 
                            select( Name, Class, event)) #Database.Id,


n_Aclass_names <- n_Aclass %>%
  select( Name) %>% #Database.Id,
  unique()


res_A <- r_A %>%
  anti_join(n_Aclass_names) %>%
  bind_rows(extra) %>%
  bind_rows(extra_nocomp) %>%
  bind_rows(official_A) %>%
  filter(!is.na(points)) %>%
  select(Name, Class, event, points) %>% #Database.Id,
  unique() 



##find total points for A class taking into account longer distances and officials

##totals

total_A <-  res_A %>%
  group_by(Name, Class)   %>% #Database.Id,
  slice_max(points, n=max_score, with_ties = F) %>%   
  summarise(
         num_events = n(),
         Total = sum(points, na.rm=T))  %>%
  # filter(num_events >= min_events) %>%
  ungroup()



results_A <- res_A  %>%
  # slice_max(points, n=max_score, with_ties = F) %>%   
  # mutate(Total = sum(points, na.rm=T),
  #        num_events = n())  %>%
  # filter(num_events >= min_events) %>%
  #group_by(Class) %>%
  select("Class","Name", event, points) %>% #"Database.Id",
  pivot_wider(id_cols=c("Class","Name"), #, "Total", "Database.Id",
              names_from = event,
              values_from = points) %>%
  right_join(total_A) %>%
  group_by(Class)   %>%
  arrange(desc(Total), .by_group = TRUE ) %>%
  mutate(type="A") 


##combine the results

#no one can win in more than one class
results_all <- bind_rows(results_A, results_short, results_B) %>%
  mutate(Eligible = ifelse(num_events >= min_events, "Y","N"))


results_unique <- results_all %>%
  group_by(Name) %>% #Database.Id
  slice_max(Total, n=1) %>% ##keep age class with max points
  ungroup() %>%
  arrange(Class, desc(Total))


##winners of each class
win_short <- results_unique %>%
  filter(type=="AS",num_events >= min_events) %>%
  group_by(Class) %>% 
  slice_max(Total, n=1, with_ties=T)


win_B <- results_unique %>%
  filter(type=="B",num_events >= min_events) %>%
  group_by(Class) %>% 
  slice_max(Total, n=1, with_ties=T)


win_A <- results_unique %>%
  filter(type=="A",num_events >= min_events) %>%
  group_by(Name) %>% #Database.Id
  slice_max(Total, n=1) %>% ##keep age class with max points 
  ungroup() %>%
  group_by(Class) %>%
  arrange(desc(Total), .by_group = TRUE ) %>%
  slice_max(Total, n=1, with_ties=T)




##save

#check that no one has won twice
winners_all <- bind_rows(win_A, win_short, win_B) %>%
  rename(`Number of Events` = num_events)

##export 

write.csv(results_all %>% select(!type) %>%
            rename(`Number of Events` = num_events), 
          file=file.path(results_folder,"Totals 2025.csv"), na="", row.names=F)
write.csv(winners_all, file=file.path(results_folder,"Winners 2025.csv"), na="", row.names=F)



