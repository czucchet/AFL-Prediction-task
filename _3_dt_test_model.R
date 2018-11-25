setwd("C:/Users/Chris Zucchet/Documents/AFL-Prediction-task")
library(DMwR);library(tidyverse);library(DBI);library(xgboost);library(recipes);library(rsample);library(RSQLite);library(purrr);library(e1071);library(digest);library(mlr);library(parallelMap);options(dplyr.width = Inf)
dt_model = xgb.load('dt_xgb_model')
find_fixture = function(year){
  library(readr);library(RSQLite);library(XML);library(httr);library(data.table);library(dplyr)
#  setwd(db_dir)
  b_url = paste0("http://probabilistic-footy.monash.edu/~footy/data/fixture.",year,".txt")
  b = read_delim(b_url, delim = " ",skip = 18,col_names =F)
  b$X1 = sapply(strsplit(as.character(b$X1), "\\."), "[[", 1)
  b$date = sapply(strsplit(as.character(b$X5), "\\:"), "[[", 1)
  b$time = sapply(strsplit(as.character(b$X5), "\\:"), "[[", 2)
  b$home_score = paste(substr(sapply(strsplit(as.character(b$X6), '\\#'), '[', 2),2,10),".",b$X7,".",sapply(strsplit(as.character(b$X8), ")"), "[[", 1),sep ="")
  b$away_score = paste(substr(as.character(b$X9),2,10),".",b$X10,".",sapply(strsplit(as.character(b$X11), ")"), "[[", 1),sep ="")
  b$Year = rep(year,nrow(b))
  names(b) = c("Round","Home_team","Away_team","Venue","X5","X6","X7","X8","X9","X10","X11","date","time","home_score","away_score","Year")
  b_filter = b[,c("Round","Home_team","Away_team","Venue","date","time","home_score","away_score","Year")]
  b_filter$winner = ifelse(as.numeric(sapply(strsplit(as.character(b$home_score), "\\."), "[[", 3)) >
                             as.numeric(sapply(strsplit(as.character(b$away_score), "\\."), "[[", 3)),b$Home_team,b$Away_team)
  b_filter$winner_location =   ifelse(as.numeric(sapply(strsplit(as.character(b$home_score), "\\."), "[[", 3)) > 
                                        as.numeric(sapply(strsplit(as.character(b$away_score), "\\."), "[[", 3)),"Home","Away")
  
  b_filter[b_filter =="G_W_Sydney"] <- "GWS Giants";b_filter[b_filter =="W_Bulldogs"] <- "Western Bulldogs";b_filter[b_filter =="W_Coast"] <- "West Coast";b_filter[b_filter =="Gold_Coast"] <- "Gold Coast"
  b_filter[b_filter =="Kangaroos"] <- "North Melbourne";b_filter[b_filter =="P_Adelaide"] <- "Port Adelaide";b_filter[b_filter =="St_Kilda"] <- "St Kilda"
  
  team_list =c("Adelaide","Carlton","Collingwood","Western Bulldogs","Melbourne","St Kilda" ,"Port Adelaide","Sydney","Essendon","Hawthorn","Brisbane","Gold Coast","West Coast","Geelong","Fremantle" ,"Richmond","North Melbourne" ,"GWS Giants")
  team_ids = c("01","03","04","07","11","15","13","16","05","10","19","20","18","09","08","14","12", "21")
  home_df = data.frame(Home_team  = team_list, home_id = team_ids);home_df$Home_team = as.character(home_df$Home_team)
  away_df = data.frame(Away_team  = team_list, away_id = team_ids);away_df$Away_team = as.character(away_df$Away_team)
  b_filter_temp = left_join(b_filter,home_df);b_filter_temp$home_id = as.character(b_filter_temp$home_id)
  b_filter_temp2 = left_join(b_filter_temp,away_df);b_filter_temp2$away_id = as.character(b_filter_temp2$away_id)
  b_filter_temp2$teams = ifelse(b_filter_temp2$home_id>b_filter_temp2$away_id, paste(b_filter_temp2$away_id,b_filter_temp2$home_id,sep = ""),paste(b_filter_temp2$ home_id,b_filter_temp2$away_id,sep = ""))
  b_filter_temp2$web_id = paste("https://afltables.com/afl/stats/games/",b_filter_temp2$Year,"/",b_filter_temp2$teams,b_filter_temp2$date,".html", sep ="")
  b_filter_temp2$ID = paste(b_filter_temp2$Year,"_",b_filter_temp2$Round,"_",b_filter_temp2$date,"_",b_filter_temp2$time,"_",b_filter_temp2$Venue, sep="")
  b_filter_temp2$Diff = as.numeric(sapply(strsplit(as.character(b_filter_temp2$home_score), '\\.'), '[', 3)) - as.numeric(sapply(strsplit(as.character(b_filter_temp2$away_score), '\\.'), '[', 3))
  b_filter_temp2$Diff_Home = ifelse(b_filter_temp2$winner == b_filter_temp2$Home_team,b_filter_temp2$Diff*1,
                                    ifelse(b_filter_temp2$winner != b_filter_temp2$Home_team, b_filter_temp2$Diff*1,b_filter_temp2$Diff*-1))
  
  b_filter_temp2$Diff_Away = ifelse(b_filter_temp2$winner == b_filter_temp2$Away_team,b_filter_temp2$Diff*-1,
                                    ifelse(b_filter_temp2$winner != b_filter_temp2$Away_team, b_filter_temp2$Diff*-1,b_filter_temp2$Diff*-1))
  b_filter_temp2 = b_filter_temp2[b_filter_temp2$web_id != "https://afltables.com/afl/stats/games/2015/010920150705.html",]
  fixture <<- data.frame(b_filter_temp2)
#  b_filter_temp2 = b_filter_temp2[as.numeric(b_filter_temp2$Round) <= 5, ]
}


find_fixture(2018) 
head(fixture)


num_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","BR","CP","UP","CM","MI","one_pc","BO","GA","game_played","Year","Round","Diff","Age_Years","Games","PercentWon");scale_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","CP","UP","CM","MI","one_pc","BO","GA","game_played","Age_Years","Games","PercentWon","dt_score","error_rate","fairness","impact_plays")
teams = unique(dbGetQuery(con, "SELECT Team, GL FROM Player_Detail"));teams = sort(unique(teams[,"Team"]));state = c("SA","QLD","VIC","VIC","VIC","WA","VIC","QLD","NSW","VIC","VIC","VIC","SA","VIC","VIC","NSW","WA","VIC");home_team = data.frame(Team = teams,home_state = state) %>% mutate_if(is.factor, as.character);away_team = data.frame(Opponent = teams,away_state = state) %>% mutate_if(is.factor, as.character)

con = dbConnect(SQLite(), "PlayerRecords.sqlite");records = dbGetQuery(con, "SELECT * FROM Player_Detail") %>% rename(number = "#", one_pc = "1%", game_played = "%P");records[records == "Â"] <- 0
records_t2 = records %>% mutate_if(names(records) %in% num_cols, as.numeric) %>% distinct() %>% data.frame()
records_t2 = records_t2 %>% mutate(dream_team =(KI*3)+(HB*2)+(MK*3)+(GL*6)+(BO*1)+(HO*1)+(TK*4)+(FF*1)+(FA*-3))
records_t3 = records_t2 %>% group_by(Player, Team) %>% arrange(Games) %>% 
  mutate(win_loss = ifelse(Diff > 0, 1,0),prior_dt  = lag(dream_team),prior_dt_2  = lag(dream_team,2),prior_dt_3  = lag(dream_team,3),prior_dt_4  = lag(dream_team,4),prior_dt_5  = lag(dream_team,5),
         no_wins_last_3_games = lag(win_loss,1)+lag(win_loss,2)+lag(win_loss,3),no_wins_last_5_games = lag(win_loss,1)+lag(win_loss,2)+lag(win_loss,3)+lag(win_loss,4)+lag(win_loss,5),
         Age_Years = ifelse(is.na(Age_Years), lag(Age_Years),Age_Years)) %>% ungroup() %>% 
         mutate(rolling_last_3 = ifelse(is.na(prior_dt) | is.na(prior_dt_2) | is.na(prior_dt_3),NA, round(((prior_dt+prior_dt_2+prior_dt_3)/3),2)),
                rolling_last_5 = ifelse(is.na(prior_dt) | is.na(prior_dt_2) | is.na(prior_dt_3) | is.na(prior_dt_4) | is.na(prior_dt_5),NA, (prior_dt+prior_dt_2+prior_dt_3+prior_dt_4+prior_dt_5)/5)) %>% 
  select(-KI,-MK,-HB,-DI,-GL,-BH,-HO,-TK,-RB,-IF,-CL,-CG,-FF,-FA,-BR,-CP,-UP,-CM,-MI,-one_pc,-BO,-GA,-game_played,-number) %>%  distinct() %>% na.omit %>%  data.frame()
records_t4 = records_t3 %>% left_join(home_team) %>% left_join(away_team);rm(records_t3);rm(home_team);rm(away_team)
records_t5 = records_t4 %>% mutate(interstate = ifelse(home_state != away_state, 1,0)) %>% arrange(desc(Year), desc(Round)) ;rm(records_t4) 

students$score1 <- scores$score1[match(students$team, scores$team)]



