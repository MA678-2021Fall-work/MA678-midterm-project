library(tidyr)
library(tidyverse)
library(dplyr)
library(viridis)
library(hrbrthemes)
library(mosaic)
library(readr)
library(lubridate)
library(tibble)

detail_playerstats <- read.csv("detail_player_data.csv", header = T)

# Subset playerstats from league NBA from season 2010-2011 to season 2019-2020
nba_detail_playerstats <- subset(detail_playerstats, League == "NBA") 
nba_detail_playerstats <- subset(nba_detail_playerstats, nba_detail_playerstats$Season == "2010 - 2011" |
                             nba_detail_playerstats$Season == "2011 - 2012" | 
                             nba_detail_playerstats$Season == "2012 - 2013" | 
                             nba_detail_playerstats$Season == "2013 - 2014" |
                             nba_detail_playerstats$Season == "2014 - 2015" | 
                             nba_detail_playerstats$Season == "2015 - 2016" |
                             nba_detail_playerstats$Season == "2016 - 2017" |
                             nba_detail_playerstats$Season == "2017 - 2018" |
                             nba_detail_playerstats$Season == "2018 - 2019" | 
                             nba_detail_playerstats$Season == "2019 - 2020")


# Create a new variable age to store every player's age at that time:
for (i in 1:nrow(nba_detail_playerstats)) {
  if (nba_detail_playerstats$Season[i] == "2010 - 2011") {
    nba_detail_playerstats$Age[i] = 2011 - nba_detail_playerstats$birth_year[i]
  }
  if (nba_detail_playerstats$Season[i] == "2011 - 2012") {
    nba_detail_playerstats$Age[i] = 2012 - nba_detail_playerstats$birth_year[i]
  }
  if (nba_detail_playerstats$Season[i] == "2012 - 2013") {
    nba_detail_playerstats$Age[i] = 2013 - nba_detail_playerstats$birth_year[i]
  }
  if (nba_detail_playerstats$Season[i] == "2013 - 2014") {
    nba_detail_playerstats$Age[i] = 2014 - nba_detail_playerstats$birth_year[i]
  }
  if (nba_detail_playerstats$Season[i] == "2014 - 2015") {
    nba_detail_playerstats$Age[i] = 2015 - nba_detail_playerstats$birth_year[i]
  }
  if (nba_detail_playerstats$Season[i] == "2015 - 2016") {
    nba_detail_playerstats$Age[i] = 2016 - nba_detail_playerstats$birth_year[i]
  }
  if (nba_detail_playerstats$Season[i] == "2016 - 2017") {
    nba_detail_playerstats$Age[i] = 2017 - nba_detail_playerstats$birth_year[i]
  }
  if (nba_detail_playerstats$Season[i] == "2017 - 2018") {
    nba_detail_playerstats$Age[i] = 2018 - nba_detail_playerstats$birth_year[i]
  }
  if (nba_detail_playerstats$Season[i] == "2018 - 2019") {
    nba_detail_playerstats$Age[i] = 2019 - nba_detail_playerstats$birth_year[i]
  }
  if (nba_detail_playerstats$Season[i] == "2019 - 2020") {
    nba_detail_playerstats$Age[i] = 2020 - nba_detail_playerstats$birth_year[i]
  }
}


nba_detail_playerstats <- nba_detail_playerstats %>% 
  mutate(PER = (FGM * 85.910 + STL * 53.897 + X3PM * 51.757 + FTM * 46.845 + BLK * 39.190 + ORB * 39.190 + AST * 34.677 + DRB * 14.707 - PF * 17.174 - (FTA - FTM) * 20.091 - (FGA - FGM) * 39.190 - TOV * 53.897) * (1/MIN))


rs_playerstats_1020 <- subset(nba_detail_playerstats, Stage == "Regular_Season")
ps_playerstats_1020 <- subset(nba_detail_playerstats, Stage == "Playoffs")

rs_playerstats_1020 <- rs_playerstats_1020 %>% mutate(join_c = paste(Season, Player))
ps_playerstats_1020 <- ps_playerstats_1020 %>% mutate(join_c = paste(Season, Player))

average_nba_detail_playerstats <- nba_detail_playerstats %>%
  mutate(MPG = MIN/GP,
         PPG = PTS/GP,
         RPG = REB/GP,
         APG = AST/GP,
         TOVPG = TOV/GP,
         PFPG = PF/GP,
         STLPG = STL/GP,
         BLKPG = BLK/GP,
         FGMPG = FGM/GP,
         FGAPG = FGA/GP,
         SP = FGM/FGA,
         X3PMPG = X3PM/GP,
         X3PAPG = X3PA/GP,
         X3PSP = X3PM/X3PA,
         FTMPG = FTM/GP,
         FTAPG = FTA/GP,
         FTSP = FTM/FTA
         ) %>% 
  select(League, 
         Season, 
         Stage, 
         Player, 
         Team,
         Age,
         PER,
         GP,
         MPG, 
         PPG, 
         RPG, 
         APG,
         STLPG, 
         BLKPG, 
         TOVPG, 
         PFPG,
         FGMPG,
         FGAPG,
         SP,
         X3PMPG, 
         X3PAPG, 
         X3PSP, 
         FTMPG, 
         FTAPG, 
         FTSP,
         height_cm,
         weight_kg,
         nationality)

# select avaerage players data from season 2010-2011 to season 2019-2020:
rs_aver_playerstats_1020 <- subset(average_nba_detail_playerstats, average_nba_detail_playerstats$Stage == "Regular_Season") %>% mutate(join_c = paste(Season, Player))
ps_aver_playerstats_1020 <- subset(average_nba_detail_playerstats, average_nba_detail_playerstats$Stage == "Playoffs") %>% mutate(join_c = paste(Season, Player))



# clean for injuriesdf:


injuries <- read.csv("injuries_2010-2020.csv",header = T)
injuries$Notes<-tolower(injuries$Notes)

# Set injuries to NA
injuries$body_part<-NA
injuries$Team<-injuries$Team

# Reclassify data to reduce levels 
assign.injury.code <- function(injury.list, code){
  # initialize/rebuild list of rows where there is no injury 
  injury.assigned.idx <- which(!is.na(injuries$body_part))
  # find rows with injury in  injury list
  idx <- grep(injury.list,injuries$Notes)
  # remove rows that were already assigned an injury code. 
  trimmed.list <- idx[!(idx %in% injury.assigned.idx)]
  # assign code to remaining list
  injuries$body_part[trimmed.list] <- code
  return(injuries)
}

# Classify each injury by body part
injuries <- assign.injury.code(injury.list = "metatarsal|toe|surgery to repair ligament and bone", code="toe")
injuries <- assign.injury.code(injury.list = "foot|heel|plantar", code="foot")
injuries <- assign.injury.code(injury.list = "knee|patella", code="knee")
injuries <- assign.injury.code(injury.list = "ankle",code="ankle")
injuries <- assign.injury.code(injury.list = "achilles|acchilles",code="achilles")
injuries <- assign.injury.code(injury.list = "leg|hamstring|calf|thigh|quad|thigh|shin|fibula|tibia|tailbone",code="leg")
injuries <- assign.injury.code(injury.list = "groin",code="groin")
injuries <- assign.injury.code(injury.list = "hip|abductor|adductor|si|hernia|pelvis",code="hip")
injuries <- assign.injury.code(injury.list = "hand|wrist|metacarpal",code="hand")
injuries <- assign.injury.code(injury.list = "finger|thumb",code = "finger")
injuries <- assign.injury.code(injury.list = "back|spinal|nerve root irritation",code= "back")
injuries <- assign.injury.code(injury.list = "shoulder|rotator",code="shoulder")
injuries <- assign.injury.code(injury.list = "arm|elbow|triceps|biceps|bicep|tricep",code="arm")
injuries <- assign.injury.code(injury.list = "upper body|ribs|pectoral|chest|rib|pectoracl|sternum|lat|sc|clavicle|abdominal|abdomen|oblique|core",code="torso")
injuries <- assign.injury.code(injury.list = "neck|cervical",code="neck")
injuries <- assign.injury.code(injury.list = "head|concussion",code="head")
injuries <- assign.injury.code(injury.list = "face|cheekbone|nose|facial|orbital|mouth|jaw|oral|dental|cheek|tooth|eye|cornia",code="face")
injuries <- assign.injury.code(injury.list = "rest",code="rest")
injuries <- assign.injury.code(injury.list = "illness|flu|stomach virus|gastro|upset stomach|food poisoning|infection|bronchitis|cold|allergic reaction|strep|stomach ailment|virus|upper respiratory inflammation|chicken pox|vertigo|respiratory ailment|pneumonia|blood clots|dizziness|gasroenteritis|embolism|ill (dnp)|dermatitis|thrombocytopenia|placed on il",code="illness")



# Standardize dates
injuries$NewDate <- as.POSIXct(injuries$Date, "%Y-%m-%d",tz="UTC")

only_injuries <- injuries[complete.cases(injuries$body_part),] %>% select(NewDate, Team, Relinquished, Notes, body_part)


for (i in 1:nrow(only_injuries)) {
  if (only_injuries$NewDate[i] <= "2011-04-13 UTC") {
    only_injuries$Season[i] = "2010 - 2011"
    only_injuries$Stage[i] = "Regular_Season"
  }
  if (only_injuries$NewDate[i] > "2011-04-13 UTC" & only_injuries$NewDate[i] < "2011-06-16 UTC") {
    only_injuries$Season[i] = "2010 - 2011"
    only_injuries$Stage[i] = "Playoffs"
  }
  if (only_injuries$NewDate[i] > "2011-06-16 UTC" & only_injuries$NewDate[i] < "2012-04-27 UTC") {
    only_injuries$Season[i] = "2011 - 2012"
    only_injuries$Stage[i] = "Regular_Season"
  }
  if (only_injuries$NewDate[i] > "2012-04-27 UTC" & only_injuries$NewDate[i] < "2012-06-15 UTC") {
    only_injuries$Season[i] = "2011 - 2012"
    only_injuries$Stage[i] = "Playoffs"
  }
  if (only_injuries$NewDate[i] > "2012-07-09 UTC" & only_injuries$NewDate[i] < "2013-04-18 UTC") {
    only_injuries$Season[i] = "2012 - 2013"
    only_injuries$Stage[i] = "Regular_Season"
  }
  if (only_injuries$NewDate[i] > "2013-04-18 UTC" & only_injuries$NewDate[i] < "2013-06-26 UTC") {
    only_injuries$Season[i] = "2012 - 2013"
    only_injuries$Stage[i] = "Playoffs"
  }
  if (only_injuries$NewDate[i] > "2013-06-26 UTC" & only_injuries$NewDate[i] <  "2014-04-17 UTC") {
    only_injuries$Season[i] = "2013 - 2014"
    only_injuries$Stage[i] = "Regular_Season"
  }
  if (only_injuries$NewDate[i] > "2014-04-17 UTC" & only_injuries$NewDate[i] < "2014-06-16 UTC") {
    only_injuries$Season[i] = "2013 - 2014"
    only_injuries$Stage[i] = "Playoffs"
  }
  if (only_injuries$NewDate[i] > "2014-06-16 UTC" & only_injuries$NewDate[i] <  "2015-04-16 UTC") {
    only_injuries$Season[i] = "2014 - 2015"
    only_injuries$Stage[i] = "Regular_Season"
  }
  if (only_injuries$NewDate[i] > "2015-04-16 UTC" & only_injuries$NewDate[i] <  "2015-06-17 UTC") {
    only_injuries$Season[i] = "2014 - 2015"
    only_injuries$Stage[i] = "Playoffs"
  }
  if (only_injuries$NewDate[i] > "2015-06-17 UTC" & only_injuries$NewDate[i] <  "2016-04-15 UTC") {
    only_injuries$Season[i] = "2015 - 2016"
    only_injuries$Stage[i] = "Regular_Season"
  }
  if (only_injuries$NewDate[i] > "2016-04-15 UTC" & only_injuries$NewDate[i] <  "2016-06-17 UTC") {
    only_injuries$Season[i] = "2015 - 2016"
    only_injuries$Stage[i] = "Playoffs"
  }
  if (only_injuries$NewDate[i] > "2016-06-17 UTC" & only_injuries$NewDate[i] <  "2016-04-13 UTC") {
    only_injuries$Season[i] = "2016 - 2017"
    only_injuries$Stage[i] = "Regular_Season"
  }
  if (only_injuries$NewDate[i] >= "2017-04-13 UTC" & only_injuries$NewDate[i] <  "2017-06-02 UTC") {
    only_injuries$Season[i] = "2016 - 2017"
    only_injuries$Stage[i] = "Playoffs"
  }
  if (only_injuries$NewDate[i] > "2017-06-02 UTC" & only_injuries$NewDate[i] <  "2018-04-13 UTC") {
    only_injuries$Season[i] = "2017 - 2018"
    only_injuries$Stage[i] = "Regular_Season"
  }
  if (only_injuries$NewDate[i] > "2018-04-13 UTC" & only_injuries$NewDate[i] <  "2018-05-27 UTC") {
    only_injuries$Season[i] = "2017 - 2018"
    only_injuries$Stage[i] = "Playoffs"
  }
  if (only_injuries$NewDate[i] > "2018-05-27 UTC" & only_injuries$NewDate[i] <  "2019-04-11 UTC") {
    only_injuries$Season[i] = "2018 - 2019"
    only_injuries$Stage[i] = "Regular_Season"
  }
  if (only_injuries$NewDate[i] > "2019-04-11 UTC" & only_injuries$NewDate[i] <  "2019-06-15 UTC") {
    only_injuries$Season[i] = "2018 - 2019"
    only_injuries$Stage[i] = "Playoffs"
  }
  if (only_injuries$NewDate[i] > "2019-06-15 UTC" & only_injuries$NewDate[i] <  "2020-03-20 UTC") {
    only_injuries$Season[i] = "2019 - 2020"
    only_injuries$Stage[i] = "Regular_Season"
  }
  if (only_injuries$NewDate[i] > "2020-03-20 UTC" & only_injuries$NewDate[i] <  "2020-10-03 UTC") {
    only_injuries$Season[i] = "2019 - 2020"
    only_injuries$Stage[i] = "Playoffs"
  }
}


only_injuries <- only_injuries %>% select(Season, Stage,Team, Relinquished, Notes, body_part) %>% mutate(join_c = paste(Season, Relinquished))

rs_injuries <- subset(only_injuries, only_injuries$Stage == "Regular_Season")
ps_injuries <- subset(only_injuries, only_injuries$Stage == "Playoffs")

# join injury and playerstats for further analysis and clean:

df_rs <- left_join(rs_playerstats_1020, rs_injuries, by = "join_c") %>% select(1:22, 27,29,30,37,43)
df_rs$body_part[is.na(df_rs$body_part)] <- "none"

df_ps <- left_join(ps_playerstats_1020, ps_injuries, by = "join_c") %>% select(1:22, 27,29,30,37,43)
df_ps$body_part[is.na(df_ps$body_part)] <- "none"


df_rs$body_part <- factor(df_rs$body_part, levels = c("none", 
                                                      "rest", 
                                                      "illness",
                                                      "finger",
                                                      "hand",
                                                      "face",
                                                      "arm",
                                                      "shoulder",
                                                      "neck",
                                                      "head",
                                                      "torso",
                                                      "back",
                                                      "leg",
                                                      "foot",
                                                      "toe",
                                                      "ankle",
                                                      "groin",
                                                      "hip",
                                                      "knee",
                                                      "achilles"), ordered = TRUE )

df_ps$body_part <- factor(df_ps$body_part, levels = c("none", 
                                                      "rest", 
                                                      "illness",
                                                      "finger",
                                                      "hand",
                                                      "face",
                                                      "arm",
                                                      "shoulder",
                                                      "neck",
                                                      "head",
                                                      "torso",
                                                      "back",
                                                      "leg",
                                                      "foot",
                                                      "toe",
                                                      "ankle",
                                                      "groin",
                                                      "hip",
                                                      "knee",
                                                      "achilles"), ordered = TRUE )


df_rs$injury_level <- as.numeric(factor(df_rs$body_part))
df_ps$injury_level <- as.numeric(factor(df_ps$body_part))

df_rs_combine <- aggregate(injury_level ~ join_c, FUN = mean, df_rs)
df_rs_combine$injury_level <- round(df_rs_combine$injury_level, digits = 2) 

df_ps_combine <- aggregate(injury_level ~ join_c, FUN = mean, df_ps)
df_ps_combine$injury_level <- round(df_ps_combine$injury_level, digits = 2)

# create the dataframes that I will use for further analysis:
df_rs_final <- left_join(rs_playerstats_1020, df_rs_combine, by = "join_c") %>% select(1:22,27,29,30,35,38)
df_ps_final <- left_join(ps_playerstats_1020, df_ps_combine, by = "join_c") %>% select(1:22,27,29,30,35,38)
df_rs_aver_final <- left_join(rs_aver_playerstats_1020, df_rs_combine, by = "join_c") 
df_ps_aver_final <- left_join(ps_aver_playerstats_1020, df_ps_combine, by = "join_c")

df_final <- rbind(df_rs_final, df_ps_final)
df_aver_final <- rbind(df_rs_aver_final,df_ps_aver_final)

df_final$Season <- as.factor(df_final$Season)
df_final$Stage <- as.factor(df_final$Stage)
 
df_aver_final$Season <- as.factor(df_aver_final$Season)
df_aver_final$Stage <- as.factor(df_aver_final$Stage)


df_rs_aver_final$Season <- as.factor(df_rs_aver_final$Season)
df_rs_aver_final$Stage <- as.factor(df_rs_aver_final$Stage)

df_ps_aver_final$Season <- as.factor(df_ps_aver_final$Season)
df_ps_aver_final$Stage <- as.factor(df_ps_aver_final$Stage)





