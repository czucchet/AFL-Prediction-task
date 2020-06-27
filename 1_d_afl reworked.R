library(tidyverse);library(magrittr);library(readr);library(readxl);library(lubridate)

afl <- read_excel("C:/Users/Chris Zucchet/Downloads/afl.xlsx", col_types = c("date", "date", "text", "text", "text", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
  "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
  "numeric", "numeric", "numeric", "numeric", "text"), skip = 1) %>% data.frame() %>% select(Date, Kick.Off..local., Home.Team, Away.Team, Venue, Home.Score, Away.Score, Home.Odds,
    Away.Odds, Home.Line.Open,Home.Line.Close, Total.Score.Close)  %>% mutate(Season = year(Date),Key = paste0(Date,"_",Kick.Off..local.,"_",Venue))

Team = c("Adelaide","Brisbane", "Carlton", "Collingwood", "Essendon", "Fremantle", "Geelong", "Gold Coast", "GWS Giants", "Hawthorn", "Melbourne", "North Melbourne", 
  "Port Adelaide", "Richmond", "St Kilda", "Sydney", "West Coast", "Western Bulldogs");State = c("SA", "QLD", "VIC", "VIC", "VIC", "WA", "VIC", "QLD", "NSW", "VIC", "VIC", "VIC", "SA", "VIC", "VIC", "NSW", "WA", "VIC")
Home_Location = data.frame(Home.Team = Team,Home_State = State);Away_Location = data.frame(Away.Team = Team,Away_State = State);
Home_Location$Home.Team = as.character(Home_Location$Home.Team);Home_Location$Home_State = as.character(Home_Location$Home_State);Away_Location$Away.Team = as.character(Away_Location$Away.Team);Away_Location$Away_State = as.character(Away_Location$Away_State)

  
afl_t = afl %>% rename(Team = Home.Team, Opponent = Away.Team, Team_Score = Home.Score, Opp_Score = Away.Score) %>% mutate(Result = Team_Score - Opp_Score,Home_Away = "Home", Line_Diff = Home.Line.Close +Result) %>% select(-Home.Odds,-Away.Odds, -Home.Line.Open, -Total.Score.Close) 
afl_t2 = afl %>% rename(Team = Away.Team, Opponent = Home.Team, Opp_Score = Home.Score, Team_Score = Away.Score) %>% mutate(Result = Team_Score - Opp_Score,Home_Away = "Away", Line_Diff = Result - Home.Line.Close) %>% select(-Home.Odds,-Away.Odds, -Home.Line.Open, -Total.Score.Close) 
head(afl_t2)
afl_t3 = bind_rows(afl_t, afl_t2) %>% arrange(desc(Date)) 
head(afl_t3)

afl_t4 = afl_t3 %>% mutate(Team_Season = paste0(Team,"_",Season)) %>% split(.$Team_Season)
for(i in 1:length(afl_t4)){afl_t4[[i]]$Game = nrow(afl_t4[[i]]):1};afl_t5 = bind_rows(afl_t4)

afl_t6 = afl_t5 %>% arrange(Date) %>% split(.$Team)
for(i in 1:length(afl_t6)){
  afl_t6[[i]]$LST_1 = lag(afl_t6[[i]]$Result,1);afl_t6[[i]]$LST_2 = lag(afl_t6[[i]]$Result,2);afl_t6[[i]]$LST_3 = lag(afl_t6[[i]]$Result,3);   
  afl_t6[[i]]$LST_4 = lag(afl_t6[[i]]$Result,4);afl_t6[[i]]$LST_5 = lag(afl_t6[[i]]$Result,5);afl_t6[[i]]$LST_6 = lag(afl_t6[[i]]$Result,6);afl_t6[[i]]$LST_7 = lag(afl_t6[[i]]$Result,7)
  afl_t6[[i]]$LN_1 = lag(afl_t6[[i]]$Line_Diff,1);afl_t6[[i]]$LN_2 = lag(afl_t6[[i]]$Line_Diff,2);afl_t6[[i]]$LN_3 = lag(afl_t6[[i]]$Line_Diff,3);   
  afl_t6[[i]]$Same_Season =ifelse(lag(afl_t6[[i]]$Season,1) == afl_t6[[i]]$Season, "1","0") 
  }

afl_t7 = bind_rows(afl_t6)  %>% filter(Season >= 2010) %>% mutate(Season = year(Date),Key = paste0(Date,"_",Kick.Off..local.,"_",Venue)) %>% 
  select(Key, Team, LST_1, LST_2, LST_3, LST_4, LST_5, LST_6, LST_7,LN_1,LN_2,LN_3,Home_Away,Game)
    
afl_home = afl_t7 %>% filter(Home_Away == "Home") %>% rename(HM_LST_1 = LST_1,HM_LST_2 = LST_2,HM_LST_3 = LST_3,HM_LST_4 = LST_4,HM_LST_5 = LST_5,HM_LST_6 = LST_6,HM_LST_7 = LST_7,HM_LN1 = LN_1,HM_LN_2 = LN_2,HM_LN_3 = LN_3,HM_Game = Game) %>% select(-Home_Away, -Team)
afl_away = afl_t7 %>% filter(Home_Away == "Away") %>% rename(AW_LST_1 = LST_1,AW_LST_2 = LST_2,AW_LST_3 = LST_3,AW_LST_4 = LST_4,AW_LST_5 = LST_5,AW_LST_6 = LST_6,AW_LST_7 = LST_7,AW_LN1 = LN_1,AW_LN_2 = LN_2,AW_LN_3 = LN_3,AW_Game = Game) %>% select(-Home_Away, -Team)
head(afl_home)
head(afl_away)


odds = c( seq(1,1.99,0.25),seq(2,5,0.5),20)
line = c(-300, seq(-90,-31,30), seq(-30,30,10), seq(60,90,30),300)

afl_1 = left_join(afl, afl_home)
afl_odds = left_join(afl_1, afl_away) %>% left_join(Home_Location) %>% left_join(Away_Location) %>% 
  mutate(GM_3_DF = ifelse(HM_LST_1 > 0,1,0)+ifelse(HM_LST_2 > 0,1,0)+ifelse(HM_LST_3 > 0,1,0)-ifelse(AW_LST_1 > 0,1,0)-ifelse(AW_LST_2 > 0,1,0)-ifelse(AW_LST_3 > 0,1,0),
         GM_5_DF = ifelse(HM_LST_1 > 0,1,0)+ifelse(HM_LST_2 > 0,1,0)+ifelse(HM_LST_3 > 0,1,0)+ifelse(HM_LST_4 > 0,1,0)+ifelse(HM_LST_5 > 0,1,0)-ifelse(AW_LST_1 > 0,1,0)-ifelse(AW_LST_2 > 0,1,0)-ifelse(AW_LST_3 > 0,1,0)-ifelse(AW_LST_4 > 0,1,0)-ifelse(AW_LST_5 > 0,1,0),
         LN_3_DF = ifelse(HM_LN1 > 0,1,0)+ifelse(HM_LN_2 > 0,1,0)+ifelse(HM_LN_3 > 0,1,0)-ifelse(AW_LN1 > 0,1,0)-ifelse(AW_LN_2 > 0,1,0)-ifelse(AW_LN_3 > 0,1,0),
        Line_Diff = Home.Line.Close + (Home.Score - Away.Score), Line_Winner = ifelse(Line_Diff > 0, "Home", "Away"),
        PG_Line_Diff = HM_LN1 - AW_LN1,PG_Result = HM_LST_1 - AW_LST_1,
        bands_odds = cut(Home.Odds, breaks = odds),bands_PG_Line = cut(PG_Line_Diff, breaks = line),bands_PG_Result = cut(PG_Result, breaks = line),
#        HM_GM = ifelse(Home.Score > Away.Score, 1,-1), CUR_GM_3 = ifelse(HM_LST_1 > 0, 1,0)+ifelse(HM_LST_2 > 0, 1,0)- ifelse(AW_LST_1 > 0, 1,0)-ifelse(AW_LST_2 > 0, 1,0)+CUR_GM,
#        CUR_LN = ifelse((Home.Score - Away.Score) +Home.Line.Close >0 , 1,-1),CUR_LN_3 = ifelse(HM_LN1 > 0, 1,0)+ifelse(HM_LN_2 > 0, 1,0)- ifelse(AW_LN1 > 0, 1,0)-ifelse(AW_LN_2 > 0, 1,0)+CUR_GM
        key = paste0(substr(as.character(Date),1,10),"_",Home.Team,"_",Away.Team)
        )
  
head(afl_odds)
afl_odds %>% filter(Season == 2020) %>% tail()

afl_home = afl_odds %>% mutate(PG_GAME_DF = Home.Score - Away.Score, Home_Away = "Home") %>% mutate(new_key = paste0(Home.Team,"_",key)) %>%
  select(new_key,Line_Diff, HM_LN1, HM_LN_2,HM_LST_1,HM_LST_2,PG_GAME_DF) %>% rename(HM_LST_Key = new_key) %>% 
  rename(LN_1 = HM_LN1, LN_2 = HM_LN_2,LST_1 = HM_LST_1,LST_2 = HM_LST_2,HM_LN_1 = Line_Diff,  HM_LST_1 = PG_GAME_DF)
afl_away = afl_odds %>% mutate(PG_GAME_DF = Away.Score-Home.Score, Home_Away = "Away") %>% mutate(new_key = paste0(Away.Team,"_",key), New_Line_Diff = Line_Diff*-1) %>%
  select(new_key,New_Line_Diff, AW_LN1, AW_LN_2,AW_LST_1,AW_LST_2,PG_GAME_DF) %>% rename(HM_LST_Key = new_key, HM_LN_1 = New_Line_Diff, HM_LST_1 = PG_GAME_DF) %>% 
  rename(LN_1 = AW_LN1, LN_2 = AW_LN_2,LST_1 = AW_LST_1,LST_2 = AW_LST_2)
afl_HM = bind_rows(afl_home, afl_away) %>% rename(HM_LN_2 = LN_1,HM_LN_3 = LN_2, HM_LST_2 = LST_1,HM_LST_3 = LST_2)
head(afl_HM)

afl_home = afl_odds %>% mutate(PG_GAME_DF = Home.Score - Away.Score, Home_Away = "Home") %>% mutate(new_key = paste0(Home.Team,"_",key)) %>%
  select(new_key,Line_Diff, HM_LN1, HM_LN_2,HM_LST_1,HM_LST_2,PG_GAME_DF) %>% rename(AW_LST_Key = new_key) %>% 
  rename(LN_1 = HM_LN1, LN_2 = HM_LN_2,LST_1 = HM_LST_1,LST_2 = HM_LST_2 , AW_LN_1 = Line_Diff, AW_LST_1 = PG_GAME_DF)
afl_away = afl_odds %>% mutate(PG_GAME_DF = Away.Score-Home.Score, Home_Away = "Away")  %>% mutate(new_key = paste0(Away.Team,"_",key), New_Line_Diff = Line_Diff*-1) %>%
  select(new_key,New_Line_Diff, AW_LN1, AW_LN_2,AW_LST_1,AW_LST_2,PG_GAME_DF) %>% rename(AW_LST_Key = new_key) %>% 
  rename(LN_1 = AW_LN1, LN_2 = AW_LN_2,LST_1 = AW_LST_1,LST_2 = AW_LST_2, AW_LN_1 = New_Line_Diff, AW_LST_1 = PG_GAME_DF)
afl_AW = bind_rows(afl_home, afl_away) %>% rename(AW_LN_2 = LN_1,AW_LN_3 = LN_2, AW_LST_2 = LST_1,AW_LST_3 = LST_2)
head(afl_AW)


test_t = read_excel("afl-2020-AUSEasternStandardTime.xlsx") %>% data.frame() 
test_t$Home.Team[test_t$Home.Team == "Adelaide Crows"] <- "Adelaide" ;test_t$Away.Team[test_t$Away.Team == "Adelaide Crows"] <- "Adelaide";test_t$Home.Team[test_t$Home.Team == "Brisbane Lions"] <- "Brisbane" ;test_t$Away.Team[test_t$Away.Team == "Brisbane Lions"] <- "Brisbane"
test_t$Home.Team[test_t$Home.Team == "Geelong Cats"] <- "Geelong" ;test_t$Away.Team[test_t$Away.Team == "Geelong Cats"] <- "Geelong";test_t$Home.Team[test_t$Home.Team == "Gold Coast Suns"] <- "Gold Coast" ;test_t$Away.Team[test_t$Away.Team == "Gold Coast Suns"] <- "Gold Coast"
test_t$Home.Team[test_t$Home.Team == "Sydney Swans"] <- "Sydney" ;test_t$Away.Team[test_t$Away.Team == "Sydney Swans"] <- "Sydney";test_t$Home.Team[test_t$Home.Team == "West Coast Eagles"] <- "West Coast" ;test_t$Away.Team[test_t$Away.Team == "West Coast Eagles"] <- "West Coast"

test = test_t2 %>% mutate(key = paste0(substr(as.character(Date),1,10),"_",Home.Team,"_",Away.Team))
head(test)

Team_Home = test %>% select(Home.Team, key,Date) %>% rename(Team = Home.Team) %>% 
  mutate(Home_Away = "Home") %>% mutate(key = paste0(Team,"_",key))
Team_Away = test %>% select(Away.Team, key,Date) %>% rename(Team = Away.Team) %>% 
  mutate(Home_Away = "Away") %>% mutate(key = paste0(Team,"_",key))
Team_Long = bind_rows(Team_Home, Team_Away) %>% arrange(Date) %>% split(.$Team)

for(i in 1:length(Team_Long)) {
  Team_Long[[i]]$last_key = lag(Team_Long[[i]]$key, 1)
  }
Team_All = bind_rows(Team_Long) 
Team_HM = Team_All %>% select(key, last_key) %>% rename(HM_Key = key, HM_LST_Key = last_key)
Team_AW = Team_All %>% select(key, last_key) %>% rename(AW_Key = key, AW_LST_Key = last_key)

test_all = test %>% mutate(HM_Key = paste0(Home.Team,"_",key),AW_Key = paste0(Away.Team,"_",key))  %>% select(-key) %>% left_join(Team_HM) %>% left_join(Team_AW) %>% 
  left_join(afl_HM) %>% left_join(afl_AW)
write.csv(test_all, "test_data.csv",row.names = F)
write.csv(afl_odds, "afl_odds.csv", row.names = F)


# Strat 1: Where Games > 3 and GM_DF_3 <= -2 and LINE_DF_3 < 0 # 93/139 - 67% hit rate
# Strat 2: Where Games > 3 and LN_DF_3 <= -1 and PG_GAME_DF <-20 & PG_GAME_DF >-60 # 94/152 - 61.8% hit rate
# Strat 3: Where Games > 3 and bands_PG_Result < -10  and LN_3_DF <= -1 - 171/290 - 59% hit rate
# Strat 4: Where Games > 3 and bands_PG_Result < -20  and LN_3_DF <= -1 and bands_PG_Line = (-60,30] - 64/93 - 69% hit rate






