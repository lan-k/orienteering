## OY settings 2026 - everything that changes from year to year.
## Used by OY.R. For a new year: copy this file, rename it (e.g. "OY settings 2027.R"),
## edit it, and change settings_file at the top of OY.R.

year <- 2026

## scoring rules (OSA Policies & Guidelines 3.9, OY Scoring & Rules)
max_score <- 6      #number of best events that count
min_events <- 4     #events needed to win a class
min_S <- 2          #AS courses needed to win an AS class
max_official <- 2   #maximum sets of official points per person

## input files (in the OY folder)
members_file <- "Members in Orienteering SA Aug 2026.xlsx"   #Eventor membership list: year of birth
yb_table_file <- "table_30268886_1793869208.xlsx"            #optional Eventor results export with YOB, NA if none
old_yb_folder <- "2024-5"                                    #optional older data with year of birth, NA if none


## OY events in date order, as decided by the Program Committee.
## eventor_id is the number at the end of the Eventor link, e.g.
##   https://eventor.orienteering.asn.au/Events/Show/22981
## event is the short name used as the column heading in the results.
## xml_file (optional) is an IOF XML results file from OE, used instead of the Eventor results.
## AS and OB classes offered and the officials are read from each event's Eventor page.
events <- tribble(
  ~eventor_id, ~event,          ~date,        ~xml_file,
  22981,       "SprintChamps",  "2026-03-01", NA,   #SA Sprint Championships - Scotch College
  23524,       "Wirreanda",     "2026-03-29", NA,   #Wirreanda Farm OY
  22983,       "LongChamps",    "2026-06-07", NA,   #SA Long Championships - Mannawarra
  22982,       "MiddleChamps",  "2026-06-08", NA,   #SA Middle Championships - Rawnsley
  23375,       "SimmondsHill",  "2026-07-26", NA,   #Simmonds Hill foot O & MTBO (foot O only)
  23886,       "Briglen",       "2026-08-09", NA,   #Briglen-Tundarri OY
  24273,       "NightChamps",   "2026-08-22", NA,   #SA Night Championships - Cobblers Creek
  23356,       "MtBarker",      "2026-08-30", NA,   #Mt Barker Sprint OY
  24311,       "KeynesGap",     "2026-09-20", NA,   #Keynes Gap OY
  22985,       "MTBO",          "2026-10-24", NA    #SA MTBO Championships - Mt Crawford Goldfields
)
#not OY events in 2026: EP Champs (23795, Sandmine 30 Aug)

night_champs <- "NightChamps"  #only 21AS and 45AS offered: 55+ can count 45AS
mtbo <- "MTBO"                 #MTBO champs: riders are placed in their age group


## officials: course setters and controllers are read from the Eventor event pages.
## Add anyone missing from Eventor here, and remove anyone who should not get points.
#(no comma after the last row)
officials_add <- tribble(
  ~event,         ~Name,           ~role,
  #"MtBarker",    "First Last",    "course setter"
)
#Mt Barker course setters (Rhys Fogarty, Lucinda Fogarty, Sandra McKean) are not on Eventor
#and were not added: it does not change any winner.

officials_remove <- tribble(
  ~event,         ~Name,           ~reason,
  "SimmondsHill", "Phil Hazell",   "set the MTBO courses; only foot O results are used"
)


## names in the results that differ from the members list (no Eventor Id in the results)
## e.g. c("Greg Hancock" = "Gregory Hancock")
name_fix <- c()


## clubs affiliated with Orienteering SA (full names and Eventor short names)
sa_clubs <- c("Onkaparinga Hills Orienteering Club","Tjuringa Orienteers",
              "Yalanga Orienteers","Tintookies Orienteers",
              "Wallaringa Orienteering Club",
              "Lincoln Orienteers","Saltbush Orienteers",
              "Top End Orienteers", "Orienteering SA",
              "OH S","TJ S","TT S","YA S","WA S",
              "SB S","LI S", "TE S", "OSA",
              "S")
