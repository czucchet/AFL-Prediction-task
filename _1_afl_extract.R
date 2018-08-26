setwd("C:/Users/Chris Zucchet/Documents/AFL-Prediction-task")

fixture_create = function(year, db_dir){
  library(readr);library(RSQLite);library(XML);library(httr);library(data.table);library(dplyr)
  setwd(db_dir)
  con = dbConnect(SQLite(), "PlayerRecords.sqlite")
  
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
#  b_filter_temp2 = b_filter_temp2[as.numeric(b_filter_temp2$Round) <= 5, ]
  
  for(i in 1:length(b_filter_temp2$web_id)){
    v_get = GET(b_filter_temp2$web_id[i])
    v_results = readHTMLTable(rawToChar(v_get$content), stringsAsFactors = F)
    cond <- sapply(v_results, function(x) class(x)== "data.frame")
    v_df = v_results[cond]
    cond2 <- sapply(v_df, function(x) nrow(x) == 22 &ncol(x) == 25)
    v_df2 = v_df[cond2]
    v_df2[[1]]$Team = rep(b_filter_temp2$Home_team[[i]], nrow(v_df2[[1]]));v_df2[[1]]$ID = rep(b_filter_temp2$ID[[i]], nrow(v_df2[[1]]));v_df2[[1]]$Home_Away = rep("Home", nrow(v_df2[[1]]))
    v_df2[[2]]$Team = rep(b_filter_temp2$Away_team[[i]], nrow(v_df2[[2]]));v_df2[[2]]$ID = rep(b_filter_temp2$ID[[i]], nrow(v_df2[[2]]));v_df2[[2]]$Home_Away = rep("Away", nrow(v_df2[[2]]))
    cond3 <- sapply(v_df, function(x) nrow(x) == 22 &ncol(x) == 7)
    v_df3 = v_df[cond3]
    v_df3[[1]] =  v_df3[[1]][,c("Player","Age","Career Games (W-D-L W%)") ]
    v_df3[[2]] =  v_df3[[2]][,c("Player","Age","Career Games (W-D-L W%)") ]
    v_df3 = rbindlist(v_df3)
    v_df3$Years = sapply(strsplit(as.character(v_df3$Age), '\\y'), '[', 1)
    v_df3$Days = sapply(strsplit(sapply(strsplit(as.character(v_df3$Age), 'd'), '[', 1), '\\y'), '[', 2)
    v_df3$PercentWon = sapply(strsplit(sapply(strsplit(as.character(v_df3$`Career Games (W-D-L W%)`), '\\ '), '[', 3), '\\%'), '[', 1)
    v_df3$Games = sapply(strsplit(as.character(v_df3$`Career Games (W-D-L W%)`), '\\ '), '[', 1)
    v_df3$Age_Years = round(as.numeric(v_df3$Years)+(as.numeric(v_df3$Days)/365),2)
    v_df4 = v_df3[,c("Player","Age_Years","Games","PercentWon")]
    home_data =  b_filter_temp2[i,c("ID","Year","Round","Away_team","Venue","Diff_Home")];names(home_data) = c("ID","Year","Round","Opponent","Venue","Diff")
    away_data =  b_filter_temp2[i,c("ID","Year","Round","Home_team","Venue","Diff_Away")];names(away_data) = c("ID","Year","Round","Opponent","Venue","Diff")
    v_df2[[1]] = left_join(v_df2[[1]],home_data)
    v_df2[[2]] = left_join(v_df2[[2]],away_data)
    v_df5 = rbindlist(v_df2)
    v_all = left_join(v_df5,v_df4);v_all[v_all == "?"] <- 0
    db_check_afl = try(dbGetQuery(con, "SELECT Player FROM Player_Detail"),silent = TRUE)
    if(class(db_check_afl) == "try-error"){
      dbWriteTable(con, "Player_Detail", v_all,overwrite = T)
      }
    if(class(db_check_afl) != "try-error"){
      dbWriteTable(con, "Player_Detail", v_all,append = T)      
      }
    }
  db_clean_afl = try(dbGetQuery(con, "SELECT * FROM Player_Detail"),silent = TRUE)
  if(class(db_clean_afl) != "try-error"){
  all_records_temp =  dbGetQuery(con, "SELECT * FROM Player_Detail")
  all_records = unique(all_records_temp)
  dbWriteTable(con, "Player_Detail", all_records,overwrite = T)
    }
  }
  
fixture_create(2013,"C:/Users/Chris Zucchet/Documents/AFL") #Done
fixture_create(2014,"C:/Users/Chris Zucchet/Documents/AFL") #Done
fixture_create(2015,"C:/Users/Chris Zucchet/Documents/AFL") #Done
fixture_create(2016,"C:/Users/Chris Zucchet/Documents/AFL") #Done
fixture_create(2017,"C:/Users/Chris Zucchet/Documents/AFL") #Done
fixture_create(2018,"C:/Users/Chris Zucchet/Documents/AFL") #Done

con = dbConnect(SQLite(), "PlayerRecords.sqlite")
records = unique(dbGetQuery(con, "SELECT * FROM Player_Detail"))
#dbDisconnect(SQLite(), "PlayerRecords.sqlite")

