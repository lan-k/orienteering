## OY.R - Orienteering SA Orienteer of the Year (OY) points
##
## Based on Lan Kelly's OY 2020-2025 code (github.com/lan-k/orienteering).
## Everything that changes each year is in the settings file (see settings_file below).
##
## How to run: open this file in RStudio, set the working directory to the OY folder
## (Session > Set Working Directory > To Source File Location) and click Source.
##
## Steps
##   1. Read the OY events from Eventor (results, course lengths, classes offered, officials)
##   2. Add year of birth and sex from the members list (and older data if needed)
##   3. Calculate points for each run (1000 x best run rate / your run rate)
##   4. Totals for A, AS and OB classes, officials' points, winners
##   5. Save results (csv, xlsx, pdf) and a list of checks to look at
##
## Changes from OY 2025.R
##   - results are read from Eventor (or an IOF XML file from OE) instead of OE csv files
##   - AS and OB classes offered at each event are read from the Eventor event page
##   - A results count for your AS class at any event where your AS class is not offered
##     (rules: "Where an AS class is not offered, points from a person's correct A age class
##     will count towards the AS class"). This was a manual correction in 2025.
##   - officials (course setters and controllers) are read from the Eventor event page
##   - a Checks sheet lists things to check before the results are published

rm(list=ls())

settings_file <- "OY settings 2026.R"

library(tidyverse)
library(lubridate)
library(stringr)
library(readxl)
library(rvest)
library(xml2)
library(openxlsx)
library(grid)

source(settings_file)

eventor_url <- "https://eventor.orienteering.asn.au"
folder <- file.path("Data", year)            #downloaded Eventor pages and cleaned event files
results_folder <- file.path("Results", year)
dir.create(folder, recursive = TRUE, showWarnings = FALSE)
dir.create(results_folder, recursive = TRUE, showWarnings = FALSE)

events <- events %>%
  mutate(eventno = row_number(),
         date = as.Date(date))
if (!"elite" %in% names(events)) events$elite <- 0   #elite classes counted as A (NOL events)

checks <- tibble(check = character(), event = character(), Name = character(),
                 Class = character(), detail = character())
add_check <- function(df, check) {
  if (nrow(df) == 0) return(invisible())
  df <- df %>% mutate(across(everything(), as.character))
  for (col in c("event", "Name", "Class", "detail")) if (!col %in% names(df)) df[[col]] <- NA_character_
  checks <<- bind_rows(checks, df %>% transmute(check = check, event, Name, Class, detail))
}



######################################################################
## 1. Read the events from Eventor
######################################################################

#download a page once and keep a copy in the Data folder
#(delete the copy to download it again, e.g. if results are corrected)
cached_page <- function(url, file) {
  if (!file.exists(file)) download.file(url, file, quiet = TRUE, mode = "wb")
  read_html(file)
}

#times "34:43" or "1:04:18" to seconds
to_secs <- function(x) {
  x <- ifelse(grepl("^[0-9:]+$", x), x, NA)
  p <- str_split(x, ":")
  map_dbl(p, function(v) {
    if (all(is.na(v))) return(NA_real_)
    v <- as.numeric(v)
    sum(v * 60^(rev(seq_along(v)) - 1))
  })
}

#class names as used in the OY rules, e.g. "M65 AS" -> "M65+AS", "WOpenB" -> "WOB"
clean_class <- function(x) {
  x <- gsub(" ", "", x)
  x <- case_when(substr(x, 2, 3) == "18" ~ sub("18", "20", x),
                 grepl("JuniorB", x) ~ sub("JuniorB", "OB", x),
                 grepl("OpenB", x) ~ sub("OpenB", "OB", x),
                 grepl("17-20", x) ~ sub("17-20", "20", x),
                 x == "W45B" ~ "WOB",
                 TRUE ~ x)
  ifelse(grepl("^[MW]65A?\\+?AS$", x), paste0(substr(x, 1, 1), "65+AS"), x)
}

#event page: classes offered and officials
read_event_page <- function(id) {
  page <- cached_page(paste0(eventor_url, "/Events/Show/", id),
                      file.path(folder, paste0("eventor_event_", id, ".html")))
  map_dfr(html_elements(page, "table.eventInfo"), function(t) {
    rows <- html_elements(t, "tr")
    tibble(caption = html_text2(html_element(t, "caption")),
           field = html_text2(html_element(rows, "th")),
           value = html_text2(html_element(rows, "td")))
  })
}

#Eventor result list: one row per competitor, with course length per class
read_eventor_results <- function(id) {
  page <- cached_page(paste0(eventor_url, "/Events/ResultList?eventId=", id, "&groupBy=EventClass"),
                      file.path(folder, paste0("eventor_results_", id, ".html")))
  map_dfr(html_elements(page, "div.eventClassHeader"), function(h) {
    cls <- html_text2(html_element(h, "h3"))
    txt <- sub(cls, "", html_text(h), fixed = TRUE)
    km <- as.numeric(gsub("[^0-9]", "", sub("m,.*", "", txt))) / 1000
    trs <- html_elements(html_element(h, xpath = "following-sibling::table[1]"), "tbody tr")
    tibble(Class_orig = cls, km = km,
           Place = suppressWarnings(as.numeric(html_text2(html_element(trs, "td.p")))),
           Id = as.numeric(str_remove(html_attr(html_element(trs, "td.n a"), "id"), "^p")),
           Name = str_squish(html_text2(html_element(trs, "td.n"))),
           Club = str_squish(html_text2(html_element(trs, "td.o"))),
           secs = to_secs(html_text2(html_element(trs, "td.t"))),
           file_YB = NA_real_)
  })
}

#IOF XML results file exported from OE (if an event is not on Eventor)
read_iof_xml <- function(f) {
  x <- xml_ns_strip(read_xml(f))
  map_dfr(xml_find_all(x, "//ClassResult"), function(cr) {
    prs <- xml_find_all(cr, "./PersonResult")
    get <- function(path) xml_text(xml_find_first(prs, path))
    ok <- get("./Result/Status") == "OK"
    tibble(Class_orig = xml_text(xml_find_first(cr, "./Class/Name")),
           km = as.numeric(xml_text(xml_find_first(cr, "./Course/Length"))) / 1000,
           Place = ifelse(ok, as.numeric(get("./Result/Position")), NA),
           Id = as.numeric(get("./Person/Id")),
           Name = str_squish(paste(get("./Person/Name/Given"), get("./Person/Name/Family"))),
           Club = coalesce(get("./Organisation/ShortName"), get("./Organisation/Name")),
           secs = ifelse(ok, as.numeric(get("./Result/Time")), NA),
           file_YB = as.numeric(substr(get("./Person/BirthDate"), 1, 4)))
  })
}


event_pages <- map(events$eventor_id, read_event_page)

runs0 <- map_dfr(seq_len(nrow(events)), function(i) {
  r <- if (!is.na(events$xml_file[i])) read_iof_xml(events$xml_file[i]) else
    read_eventor_results(events$eventor_id[i])
  r %>% mutate(eventno = i, event = events$event[i])
})


## classes offered at each event (from the Class information on the Eventor event page)
classes_offered <- map(seq_len(nrow(events)), function(i) {
  info <- event_pages[[i]] %>% filter(caption == "Class information")
  cl <- unlist(str_split(info$value, ","))
  if (length(cl) == 0) cl <- unique(runs0$Class_orig[runs0$eventno == i])  #no Eventor page
  clean_class(str_squish(cl))
})

events <- events %>%
  mutate(name = map_chr(event_pages, ~ c(.x$value[.x$field == "Event"], NA)[1]),
         as_classes = map_chr(classes_offered, ~ paste(sort(unique(grep("^[MW].*AS$", .x, value = TRUE))), collapse = ", ")),
         S_offered = as.numeric(as_classes != ""),
         B_offered = map_dbl(classes_offered, ~ as.numeric(any(.x %in% c("MOB", "WOB")))),
         link = paste0(eventor_url, "/Events/Show/", eventor_id),
         source = ifelse(is.na(xml_file), "Eventor results", paste("OE file:", xml_file)))

as_offered <- map(classes_offered, ~ unique(grep("^[MW].*AS$", .x, value = TRUE)))
S_offered <- events$S_offered
B_offered <- events$B_offered
E_offered <- events$elite
night_no <- which(events$event == night_champs)
mtbo_no <- which(events$event == mtbo)


######################################################################
## 2. Year of birth and sex
######################################################################

if (length(name_fix) > 0) {
  runs0 <- runs0 %>% mutate(Name = recode(Name, !!!name_fix))
  add_check(tibble(Name = names(name_fix), detail = paste("changed to", name_fix)),
            "Name changed to match members list (settings)")
}

members <- read_excel(members_file) %>%
  transmute(Id = as.numeric(ID),
            m_name = str_squish(paste(`First name`, `Last name`)),
            m_S = ifelse(Sex == "female", "F", "M"),
            m_YB = as.numeric(`Year of birth`))

members_by_name <- members %>%
  group_by(m_name) %>% filter(n() == 1) %>% ungroup() %>%   #skip duplicate names
  select(Name = m_name, nm_S = m_S, nm_YB = m_YB)

#optional Eventor results export with year of birth
if (!is.na(yb_table_file)) {
  tbl <- read_excel(yb_table_file) %>%
    transmute(Id, Name = str_squish(paste(FirstName, LastName)), YOB = as.numeric(YOB), Sex)
  tbl_id <- tbl %>% filter(!is.na(Id), !is.na(YOB)) %>%
    group_by(Id) %>% summarise(t_YB = min(YOB), t_S = first(Sex), .groups = "drop")
  tbl_name <- tbl %>% filter(!is.na(YOB)) %>%
    group_by(Name) %>% filter(n_distinct(YOB) == 1) %>%
    summarise(tn_YB = first(YOB), .groups = "drop")
} else {
  tbl_id <- tibble(Id = numeric(), t_YB = numeric(), t_S = character())
  tbl_name <- tibble(Name = character(), tn_YB = numeric())
}

#optional older data (OSA members list with dates of birth, OE csv files)
old_yb <- tibble(Name = character(), old_YB = numeric())
if (!is.na(old_yb_folder)) {
  old_files <- list.files(old_yb_folder, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  old_yb <- map_dfr(old_files, function(f) {
    d <- tryCatch(read.csv(f, stringsAsFactors = F, na.strings = "", check.names = F),
                  error = function(e) NULL)
    if (is.null(d)) return(NULL)
    names(d) <- gsub("[ .]", "", tolower(names(d)))
    if (!all(c("firstname", "surname") %in% names(d))) return(NULL)
    yb <- if ("dateofbirth" %in% names(d)) year(dmy(d$dateofbirth, quiet = TRUE)) else
      if ("yb" %in% names(d)) suppressWarnings(as.numeric(d$yb)) else NULL
    if (is.null(yb)) return(NULL)
    tibble(Name = str_squish(paste(d$firstname, d$surname)), YB = yb)
  }) %>%
    filter(!is.na(YB), YB > 1900) %>%
    group_by(Name) %>%
    summarise(old_YB = as.numeric(names(which.max(table(YB)))), .groups = "drop")
}

#order: members list (by Eventor Id), OE file, results export, members (by name), older data
runs <- runs0 %>%
  left_join(members %>% select(Id, m_name, m_S, m_YB), by = "Id") %>%
  left_join(tbl_id, by = "Id") %>%
  left_join(members_by_name, by = "Name") %>%
  left_join(tbl_name, by = "Name") %>%
  left_join(old_yb, by = "Name") %>%
  mutate(YB = coalesce(m_YB, file_YB, t_YB, nm_YB, tn_YB, old_YB),
         YB_source = case_when(!is.na(m_YB) ~ "members list",
                               !is.na(file_YB) ~ "OE file",
                               !is.na(t_YB) ~ "results export",
                               !is.na(nm_YB) ~ "members list (name)",
                               !is.na(tn_YB) ~ "results export (name)",
                               !is.na(old_YB) ~ "older data (name)"),
         S = coalesce(m_S, t_S, nm_S,
                      case_when(substr(Class_orig, 1, 1) == "W" ~ "F",
                                substr(Class_orig, 1, 1) == "M" ~ "M")),
         member = !is.na(m_name) | !is.na(nm_YB),
         Time = ifelse(is.na(secs), NA,
                       sprintf("%d:%02d:%02d", secs %/% 3600, (secs %% 3600) %/% 60, round(secs %% 60))))

#same Eventor Id with different names (OY age classes only)
add_check(runs %>% filter(!is.na(Id), !is.na(Place), grepl("^[MW][0-9]+|^[MW]OB$", clean_class(Class_orig))) %>%
            group_by(Id) %>% filter(n_distinct(Name) > 1) %>%
            group_by(Id, Name) %>% summarise(event = paste(unique(event), collapse = ", "), .groups = "drop") %>%
            mutate(detail = paste("Eventor Id", Id)),
          "Same Eventor Id with different names")

#cleaned results for each event (for the record)
for (i in seq_len(nrow(events))) {
  write.csv(runs %>% filter(eventno == i) %>%
              select(Id, Name, YB, YB_source, S, Class = Class_orig, Club, km, Time, Place),
            file.path(folder, paste0(events$event[i], ".csv")), row.names = F, na = "")
}



######################################################################
## 3. Points for each run (OY 2025.R)
######################################################################

varlist <- c("Id", "Name", "YB","S", "Class", "Class_orig",
             "km", "Place","Time", "mins_per_km","best",
             "age_group","age_class","age_classS","count_Aclass","count_Sclass",
             "City","casual","event","eventno", "sex_prefix",
             "classB","classS","S_offered","B_offered",
             "points")

results0 <- data.frame()

for (i in seq_len(nrow(events))) {

  r <- runs %>%
    filter(eventno == i, !is.na(Place), !is.na(secs)) %>%
    mutate(mins_per_km = secs / 60 / km)

  #MTBO courses at foot O events do not count (e.g. Simmonds Hill)
  if (i != mtbo_no) r <- r %>% filter(!grepl("mtbo|bike", Class_orig, ignore.case = TRUE))

  ##find best in Class
  r <- r %>%
    mutate(
      Class = clean_class(Class_orig),
      event = events$event[i],
      City = str_squish(str_replace_all(Club, "[^[:alnum:]]", " ")),
      casual = as.numeric(substr(City,1,3) == "CAS" | is.na(City)),
      S_offered = S_offered[i],
      B_offered = B_offered[i],
      E_offered = E_offered[i],
      classS = grepl("AS$", Class),
      classB = grepl("B", Class),
      ##change classes for Elites
      Class = ifelse(E_offered == 1 & substr(Class, 4,5) == "E", gsub("E","A",Class), Class),
      sex_prefix = if_else(S=="F","W","M")) %>%
    group_by(Class) %>%
    mutate(best=min(mins_per_km, na.rm=T)) %>%
    ungroup() %>%
    mutate(points=ifelse((!classB | Class %in% c('WOB','MOB')) ,
                         round(1000*best/mins_per_km ),0))

  r <- r %>%
    mutate(
           age=as.numeric(year-YB),
           age_group=case_when(age < 10 ~ 10,
                               age < 21 ~ 2*ceiling(age/2),
                               age >= 21 & age < 35 ~ 21,
                               age >= 35 ~ 10*floor((age-5)/10) + 5,
                               TRUE ~ NA),
           age_group = ifelse(age_group == 18, 20, age_group),
           age_class = suppressWarnings(strtoi(if_else(classB, NA_character_, substr(Class, 2,3)), base = 10)),
           #change classes for MTBO
           age_class = ifelse(eventno == mtbo_no , age_group, age_class),
           age_class = ifelse(!is.na(age_group) & between(age_group, 17, 18),
                              20, age_class),  #fix classes for 17-20 and 18

           count_Aclass = case_when(!classS & !classB & age <21 ~ age <= age_class,
                                    !classS & !classB & age >=21 ~ age >= age_class,
                                    TRUE ~ F), #course counts towards A class
           age_classS=case_when(
              age_group <= 20 ~ paste0(sex_prefix, "21AS"),
              age_group >= 21 & age_group < 45  ~ paste0(sex_prefix, "21AS"),
              age_group >= 45 & age_group < 65  ~ paste0(sex_prefix, paste0(as.character(age_group),"AS")),
              age_group >= 65  ~ paste0(sex_prefix,"65+AS"),
                                  TRUE ~ NA_character_),

           #course counts towards AS class: your AS class, or your A class if your AS class
           #is not offered at this event
           count_Sclass =  ((classS & Class == age_classS) |
                             (!classS & age_group == age_class & !(age_classS %in% as_offered[[i]]))),
           #night champs: highest AS class is 45AS, so 55+ can count 45AS
           count_Sclass = ifelse(eventno %in% night_no & age_group >= 55,
                                 grepl('45AS', Class) | count_Sclass, count_Sclass),
           comp_class = (substr(Class,1,1) %in% c("W","M") &
                                age_class >= 16 ) | Class %in% c("MOB","WOB")) %>%
    filter(comp_class)

  #fix MTBO Classes
  r <- r %>%
    mutate(MTBO_age_class = ifelse(eventno == mtbo_no,
                                   as.numeric(str_extract(Class_orig, "[0-9]+")), NA),
           MTBO_age_class = ifelse(!is.na(MTBO_age_class) & MTBO_age_class <= 21 ,
                                   MTBO_age_class, age_class),
           Class= ifelse(eventno == mtbo_no,
                              paste0(sex_prefix, MTBO_age_class,"A"), Class) ) %>%
    select(all_of(varlist))

  results0 <- bind_rows(results0, r)
}


##remove casual entries, non SA competitors
results <- results0 %>%
  mutate(sa_club = as.numeric(City %in% sa_clubs)) %>%
  group_by(Name) %>%
  mutate(sa_club = max(sa_club, na.rm = T))%>% # for people who became members later
  ungroup() %>%
  filter(sa_club== 1,
         !grepl("Easy",Class),
         !grepl("Non Comp",Name))

add_check(results0 %>% filter(is.na(YB), City %in% sa_clubs) %>%
            mutate(detail = paste("club", City, "- not scored without a year of birth")),
          "Missing year of birth (SA club)")

renamed <- results0 %>% filter(Class != Class_orig, eventno != mtbo_no) %>%
  count(event, Class_orig, Class) %>%
  mutate(detail = paste0(Class_orig, " -> ", Class, " (", n, " runners)"))
add_check(renamed, "Class name changed")



######################################################################
## 4. Totals
######################################################################

###officials: course setters and controllers from Eventor, plus/minus settings
officials_eventor <- map_dfr(seq_len(nrow(events)), function(i) {
  event_pages[[i]] %>%
    filter(caption == "Contact details and main officials",
           str_detect(field, regex("setter|controller", ignore_case = TRUE))) %>%
    mutate(Name = str_split(value, "\n")) %>%
    unnest(Name) %>%
    transmute(event = events$event[i], Name = str_squish(Name),
              role = str_to_lower(sub("s$", "", field)), source = "Eventor")
})

finished <- runs %>% filter(!is.na(Place), !is.na(secs)) %>% distinct(event, Name)

if (exists("officials_from_eventor") && !officials_from_eventor) officials_eventor <- officials_eventor[0, ]

officials_all <- bind_rows(officials_eventor,
                           officials_add %>% mutate(source = "settings")) %>%
  filter(Name != "") %>%
  group_by(event, Name) %>%
  summarise(role = paste(unique(role), collapse = " & "), source = first(source), .groups = "drop") %>%
  left_join(events %>% select(event, eventno, S_offered, B_offered), by = "event") %>%
  left_join(officials_remove, by = c("event", "Name")) %>%
  left_join(finished %>% mutate(ran = TRUE), by = c("event", "Name")) %>%
  mutate(status = case_when(!is.na(reason) ~ paste("no points:", reason),
                            !is.na(ran) ~ "no points: ran at this event",
                            !Name %in% results$Name ~ "no points: no OY results to average",
                            TRUE ~ "official points")) %>%
  arrange(eventno) %>%
  group_by(Name, ok = status == "official points") %>%
  mutate(status = ifelse(ok & row_number() > max_official,
                         paste("no points: more than", max_official, "sets"), status)) %>%
  ungroup() %>%
  select(!c(ok, ran, reason))

officials <- officials_all %>%
  filter(status == "official points") %>%
  select(event, Name, eventno, S_offered, B_offered)

add_check(officials_all %>% filter(status != "official points") %>% mutate(detail = paste(role, "-", status)),
          "Official without official points")


official_names <- officials %>% select(Name) %>% unique()

official_means <- inner_join(official_names,results,  by="Name")  %>%
  group_by(Name) %>%
  summarise(points = round(mean(points, na.rm=T))) %>%
  ungroup() %>%
  inner_join(officials, by = "Name")

##copy over all the possible classes that officials have entered
official_classes <- inner_join(official_names,
                               results %>%
                                 select(Name, Class, classS, classB),
                               by = "Name", relationship = "many-to-many")  %>%
  unique() %>%
  left_join(official_means, by = "Name",
            relationship = "many-to-many") %>%
  mutate(official=1, count_Sclass = !classB,
         count_Aclass=!classS & !classB) %>%
  group_by(Name) %>%
  mutate(any_B = any(classB),
         any_S = any(classS),
         any_A = any(count_Aclass))

official_A <- official_classes %>%
  filter(any_A, !classS, !classB)

official_S <- official_classes %>%
  filter(any_S, classS) %>%
  select(Name,  classS,S_offered,  event, eventno, points) %>%
  unique()

official_B <- official_classes %>%
  filter(any_B, classB)



####short classes
##2024 no longer need to enter every S class at every event
r_S <-  results %>%
  filter(count_Sclass) %>%
  mutate(C21AS = as.integer(Class == "M21AS" | Class == "W21AS")) %>%
  group_by(Name) %>%
  mutate(C21AS= max(C21AS),
         age_classS = ifelse(S_offered == 0 & C21AS == 1,
                             paste0(sex_prefix, "21AS"),age_classS ) ,
         Class2 = case_when(
                           Class == "M21AS" | Class == "W21AS" ~ Class,
                           ##highest age S class in night champs is 45AS
                           eventno %in% night_no & age_group >= 55 &
                             (Class == "M45AS" | Class == "W45AS") ~ age_classS,
                           classS ~ Class,
                           count_Sclass ~ age_classS,
                           TRUE ~ age_classS),
         any_S = any(classS),
         nclass=n_distinct(Class2)
        ) %>%
  filter(any_S,
        nclass == 1) %>%  #can only enter one S class
  group_by(Class2,Name)   %>%
  mutate( sum_S=sum(as.integer(classS),na.rm=T),
          max_S=sum(S_offered,na.rm=T)) %>%
  filter(sum_S >= min_S ) %>%  #at least 2 AS events
  ungroup()


off_S <- inner_join(r_S %>% select(Name, Class2) %>% unique(), official_S, by = "Name") %>%
  unique()

r_S <- bind_rows(r_S, off_S)


total_S <- r_S  %>%
  group_by(Class2,Name)   %>%
  slice_max(points, n=max_score, with_ties = F) %>%
  summarise(
            sum_S=sum(as.integer(classS),na.rm=T),
            num_events = n(),
            Total = sum(points, na.rm=T), .groups = "drop")


results_short <- r_S %>%
  filter(!is.na(Class2)) %>%
  select("Class2","Name", event, points) %>%
  pivot_wider(id_cols=c("Class2","Name"),
              names_from = event,
              values_from = points) %>%
  right_join(total_S, by = c("Class2", "Name")) %>%
  group_by(Class2) %>%
  arrange(desc(Total), .by_group = TRUE ) %>%
  rename(Class=Class2)  %>%
  select(!sum_S) %>%
  mutate(type="AS")



###B classes
r_B <-
  bind_rows(results, official_B) %>%
  filter(Class %in% c("WOB","MOB") | (B_offered != 1 )) %>%
  group_by( Name) %>%
  mutate(sum_B = sum(as.numeric(classB))) %>%
  filter(sum_B > 0) %>%
  mutate(Class = paste0(substr(Class, 1,1),"OB")) %>%
  ungroup()


total_B <- r_B %>%
  group_by(Name, Class) %>%
  slice_max(points, n=max_score, with_ties = F) %>%
  summarise(
         num_events = n(),
         Total = sum(points, na.rm=T), .groups = "drop")


results_B <- r_B  %>%
  filter(classB | (B_offered != 1 )) %>%
  arrange(Name, eventno) %>%
  pivot_wider(id_cols=c("Class","Name"),
              names_from = event,
              values_from = points) %>%
  right_join(total_B, by = c("Class", "Name")) %>%
  group_by(Class) %>%
  arrange(desc(Total), .by_group = TRUE )  %>%
  mutate(type="B")



##A classes
r_A <- results %>%
  filter(count_Aclass)


n_Aclass <- results %>%
  filter(count_Aclass)  %>%
  pivot_wider(id_cols=c("Class","Name"),
              names_from = event,
              values_from = points) %>%
  group_by(Name) %>%
  mutate(nclass=n(),
         maxclass=max(nclass)) %>%
  filter(maxclass > 1) %>%
  arrange(Name, Class) %>%
  ungroup()   %>%
  select(Name, Class)


#copy points across classes if distance is same or longer

##all the events where competitors counted for 2 classes
extra_events <- inner_join(r_A, n_Aclass %>% select(Class), by = "Class",
                           relationship = "many-to-many") %>%
  select(event, Class, km, best) %>%
  unique()

extra <- full_join(n_Aclass, extra_events, by = "Class",
                   relationship = "many-to-many") %>%
  left_join(r_A, by = c("Name", "Class", "event", "km", "best")) %>%
  group_by(Name,event) %>%
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
extra_nocomp <- full_join(n_Aclass, extra_events, by = "Class",
                          relationship = "many-to-many") %>%
  left_join(r_A, by = c("Name", "Class", "event", "km", "best")) %>%
  group_by(Name,event) %>%
  filter(!all(is.na(points))) %>%
  group_by(Name)  %>%
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
                            select( Name, Class, event), by = c("Name", "Class", "event"))


n_Aclass_names <- n_Aclass %>%
  select( Name) %>%
  unique()


res_A <- r_A %>%
  anti_join(n_Aclass_names, by = "Name") %>%
  bind_rows(extra) %>%
  bind_rows(extra_nocomp) %>%
  bind_rows(official_A) %>%
  filter(!is.na(points)) %>%
  select(Name, Class, event, points) %>%
  unique()


total_A <-  res_A %>%
  group_by(Name, Class)   %>%
  slice_max(points, n=max_score, with_ties = F) %>%
  summarise(
         num_events = n(),
         Total = sum(points, na.rm=T), .groups = "drop")


results_A <- res_A  %>%
  select("Class","Name", event, points) %>%
  pivot_wider(id_cols=c("Class","Name"),
              names_from = event,
              values_from = points) %>%
  right_join(total_A, by = c("Class", "Name")) %>%
  group_by(Class)   %>%
  arrange(desc(Total), .by_group = TRUE ) %>%
  mutate(type="A")


##combine the results

#no one can win in more than one class
results_all <- bind_rows(results_A, results_short, results_B) %>%
  ungroup() %>%
  mutate(Eligible = ifelse(num_events >= min_events, "Y","N"))


results_unique <- results_all %>%
  group_by(Name) %>%
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
  group_by(Name) %>%
  slice_max(Total, n=1) %>% ##keep age class with max points
  ungroup() %>%
  group_by(Class) %>%
  arrange(desc(Total), .by_group = TRUE ) %>%
  slice_max(Total, n=1, with_ties=T)

winners_all <- bind_rows(win_A, win_short, win_B) %>% ungroup()


## checks on the winners
member_names <- unique(c(members$m_name, results$Name[results$Id %in% members$Id]))
add_check(winners_all %>% filter(!Name %in% member_names) %>%
            mutate(detail = "winner not found in the members list - check membership"),
          "Winner not in members list")

add_check(winners_all %>% group_by(Class) %>% filter(n() > 1) %>%
            mutate(detail = paste("tie on", Total)), "Tie for first")

margins <- results_unique %>%
  filter(num_events >= min_events) %>%
  group_by(Class) %>%
  arrange(desc(Total), .by_group = TRUE) %>%
  filter(n() > 1) %>%
  summarise(Name = first(Name), detail = paste0(first(Name), " ", first(Total), " vs ",
                                                nth(Name, 2), " ", nth(Total, 2),
                                                " (", first(Total) - nth(Total, 2), " points)"),
            margin = first(Total) - nth(Total, 2), .groups = "drop") %>%
  filter(margin < 150)
add_check(margins, "Close result (less than 150 points)")

all_classes <- unique(results_all$Class)
add_check(tibble(Class = setdiff(all_classes, winners_all$Class)) %>%
            mutate(detail = paste("no one with", min_events, "or more events")),
          "Class with no winner")



######################################################################
## 5. Save
######################################################################

event_cols <- intersect(events$event, names(results_all))

totals_out <- results_all %>%
  select(Class, Name, all_of(event_cols), num_events, Total, Eligible) %>%
  rename(`# Events` = num_events)

winners_out <- winners_all %>%
  mutate(Eligible = "Y") %>%
  select(Class, Name, all_of(event_cols), num_events, Total, type, Eligible) %>%
  rename(`# Events` = num_events)

runs_out <- results0 %>%
  mutate(SA = ifelse(City %in% sa_clubs | Name %in% results$Name, "Y", "N")) %>%
  left_join(events %>% select(event, date), by = "event") %>%
  arrange(eventno, Class_orig, Place) %>%
  transmute(Event = event, Date = date, Name, Club = City, `SA club` = SA, YB, `Age group` = age_group,
            `Class entered` = Class_orig, `OY class` = Class, km, Time, Place,
            `min/km` = round(mins_per_km, 2), `Best min/km in class` = round(best, 2), Points = points)

events_out <- events %>%
  left_join(runs %>% filter(!is.na(Place)) %>% count(eventno, name = "Finishers"), by = "eventno") %>%
  transmute(`#` = eventno, Event = event, Date = date, `Eventor name` = name,
            `AS classes offered` = as_classes, `OB offered` = ifelse(B_offered == 1, "Y", "N"),
            Finishers, Source = source, Link = link)

officials_out <- officials_all %>%
  left_join(official_means %>% select(Name, event, points), by = c("Name", "event")) %>%
  transmute(Event = event, Name, Role = role, `From` = source, Status = status,
            `Points (average)` = points)

checks_out <- checks %>% rename(Check = check, Event = event, Class = Class, Detail = detail)

write_excel_csv(totals_out, file.path(results_folder, paste0("Totals ", year, ".csv")), na = "")
write_excel_csv(winners_out, file.path(results_folder, paste0("Winners ", year, ".csv")), na = "")


## spreadsheet
wb <- createWorkbook()
head_style <- createStyle(textDecoration = "bold", fgFill = "#DDE5EE", border = "Bottom",
                          wrapText = TRUE, valign = "center")
add_sheet <- function(name, df, widths = "auto") {
  addWorksheet(wb, name)
  writeData(wb, name, df, headerStyle = head_style, keepNA = FALSE)
  freezePane(wb, name, firstRow = TRUE, firstCol = name %in% c("Winners", "Totals"))
  setColWidths(wb, name, cols = seq_along(df), widths = widths)
  pageSetup(wb, name, orientation = "landscape", paperSize = 9, fitToWidth = TRUE, fitToHeight = FALSE,
            printTitleRows = 1)
}
add_sheet("Winners", winners_out)
add_sheet("Totals", totals_out)
#line between classes on the totals sheet
class_change <- which(totals_out$Class != lag(totals_out$Class)) + 1
addStyle(wb, "Totals", createStyle(border = "Top"), rows = class_change,
         cols = seq_along(totals_out), gridExpand = TRUE)
add_sheet("All runs", runs_out)
add_sheet("Events", events_out)
add_sheet("Officials", officials_out)
add_sheet("Checks", checks_out)
saveWorkbook(wb, file.path(results_folder, paste0("OY ", year, " Results.xlsx")), overwrite = TRUE)


## pdf tables (A4 landscape), same layout as previous years
table_pdf <- function(df, file, title, fontsize = 8) {
  df <- df %>% mutate(across(everything(), ~ ifelse(is.na(.x), "", as.character(.x))))
  numeric_col <- map_lgl(df, ~ all(grepl("^[0-9.]*$", .x)))
  pdf(file, width = 11.69, height = 8.27)
  grid.newpage()
  gp <- gpar(fontsize = fontsize)
  gpb <- gpar(fontsize = fontsize, fontface = "bold")
  width_in <- function(x, g) max(map_dbl(x, ~ convertWidth(grobWidth(textGrob(.x, gp = g)), "inches", TRUE)))
  w <- map2_dbl(df, names(df), ~ max(width_in(.x, gp), width_in(.y, gpb))) + 0.12
  w <- w * min(1, 10.9 / sum(w))
  x0 <- 0.4 + c(0, cumsum(w)[-length(w)])
  row_h <- fontsize * 1.6 / 72
  per_page <- floor((8.27 - 1.3) / row_h) - 1
  pages <- split(seq_len(nrow(df)), ceiling(seq_len(nrow(df)) / per_page))
  for (p in seq_along(pages)) {
    if (p > 1) grid.newpage()
    pushViewport(viewport(xscale = c(0, 11.69), yscale = c(8.27, 0)))
    grid.text(title, x = unit(0.4, "native"), y = unit(0.45, "native"), just = "left",
              gp = gpar(fontsize = 13, fontface = "bold"))
    y <- 0.8
    grid.rect(x = unit(0.4, "native"), y = unit(y + row_h / 2, "native"), width = unit(sum(w), "native"),
              height = unit(row_h, "native"), just = c("left", "centre"),
              gp = gpar(fill = "#DDE5EE", col = NA))
    for (j in seq_along(df)) {
      grid.text(names(df)[j], x = unit(x0[j] + ifelse(numeric_col[j], w[j] - 0.06, 0.06), "native"),
                y = unit(y + row_h / 2, "native"), just = ifelse(numeric_col[j], "right", "left"), gp = gpb)
    }
    y <- y + row_h
    rows <- pages[[p]]
    for (k in seq_along(rows)) {
      i <- rows[k]
      if (k > 1 && df$Class[i] != df$Class[rows[k - 1]]) {
        grid.lines(x = unit(c(0.4, 0.4 + sum(w)), "native"), y = unit(c(y, y), "native"),
                   gp = gpar(col = "grey40", lwd = 0.6))
      }
      for (j in seq_along(df)) {
        grid.text(df[[j]][i], x = unit(x0[j] + ifelse(numeric_col[j], w[j] - 0.06, 0.06), "native"),
                  y = unit(y + row_h / 2, "native"), just = ifelse(numeric_col[j], "right", "left"), gp = gp)
      }
      y <- y + row_h
    }
    grid.text(paste("Page", p, "of", length(pages)), x = unit(11.29, "native"), y = unit(8.0, "native"),
              just = "right", gp = gpar(fontsize = 7, col = "grey40"))
    popViewport()
  }
  invisible(dev.off())
}

table_pdf(winners_out, file.path(results_folder, paste0("OY ", year, " Winners.pdf")),
          paste("Orienteer of the Year", year, "- Winners"))
table_pdf(totals_out, file.path(results_folder, paste0("OY ", year, ".pdf")),
          paste("Orienteer of the Year", year, "- All results"))

print(winners_out %>% select(Class, Name, `# Events`, Total), n = Inf)
cat("\nChecks to look at:", nrow(checks_out), "(see the Checks sheet)\n")
