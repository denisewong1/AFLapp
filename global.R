library(tidyr)
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)
library(colorspace)

# run this before serving the app to save app data locally
# compile_app_data()

# run this separately
compile_app_data <- function() {
  
  setwd("~/DataScience/AFL")
  source('~/DataScience/AFL/src/useful_collate.R')
  source('~/DataScience/AFL/src/useful_features.R')
  
  Season <- 2019
  x <- collate_data(Season)
  lstCol <- x[1][[1]]
  df <- as.data.frame(x[2][[1]])

  # remove players 
  df <- df[!is.na(df$Season),] # didn't play all season
  df <- df %>% filter(!is.na(Venue)) # bye rounds

  # convert categorical variables
  df$posML <- as.factor(substr(df$posALL,1,2))
  df$HomeAway <- as.factor(df$HomeAway)
  df$Opp <- as.factor(df$Opp)
  df$WtScore[is.na(df$WtScore)] <- 0
  df$nAgeMth[is.na(df$nAgeMth)] <- 240

  # add features
  df <- add_feature_StatusMatch(df)
  df <- add_feature_StatusCore(df)
  lstCol$StatType <- c(lstCol$StatType,"PR",
                     "StatusMatch","StatusMatchnext",
                     "StatusField","StatusFieldnext",
                     "StatusCore","StatusCorenext","ProbCore")
  df <- as.data.frame(df)

  # backfill missing data
  df$FtsyPx[df$FtsyPx==0] <- NA
  df <- df %>%
    group_by(PlayerID) %>%
    fill(FtsyPx,.direction="up") %>%
    ungroup()
  df <- df[df$RoundID<=23,] # only look at regular rounds
  df <- as.data.frame(df)
  
  xdir <- "~/DataScience/AFL/src/app/AFLapp"
  xfilename <- paste0(xdir,"/","data.csv")
  write.csv(df,xfilename,row.names=FALSE)
  
}


xfilename <- "data.csv"
df <- read.csv(xfilename,header=TRUE,stringsAsFactors=FALSE)
lstTeamID <- unique(df$TeamID)
lstStatID <- c("DI","MG","SC","AF")


#xTeamID <- c("ES","CW")
get_team_chart <- function(df,xTeamID,xTeamStatID,xAdjust) {
  
  Q1 <- 9.5
  Q2 <- 18.5
  
  df$xStat <- df[,c(xTeamStatID)]
  df$xStatTG <- ifelse(df$TG,100*df$xStat/df$TG,0)
  if (xAdjust) { df$xStatShow <- df$xStatTG 
  } else { df$xStatShow <- df$xStat }
  
  # get data
  chart6 <- df %>%
    filter(TeamID %in% xTeamID) %>%
    mutate(PosPlayer = paste0(posALL," ",Player)) %>%
    select(TeamID,RoundID,PlayerID,PosPlayer,posALL,
           StatusMatch,StatusCore,Played,TG,xStatShow) %>%
    #group_by(TeamID,RoundID) %>%
    group_by(RoundID) %>%
    mutate(rankStat = dense_rank(-xStatShow)) %>%
    ungroup()
    
  # order player names
  lstPlayers <- chart6 %>%
    group_by(TeamID,PlayerID,PosPlayer,posALL) %>%
    summarise(Field=sum(StatusMatch=="Field"),
              Bench=sum(StatusMatch=="Bench"),
              Dropped=sum(StatusMatch=="Dropped"),
              sumTG=sum(TG)) %>%
    ungroup() %>%
    mutate(PI = Field + Bench) %>%
    filter(!((posALL == "") & (Dropped!=22))) %>%
    arrange(-PI,-Field,-sumTG)
  chart6$PosPlayer <- factor(chart6$PosPlayer,levels=c(lstPlayers$PosPlayer))
    
  
  # categorical chart
  g6a <- ggplot(chart6 %>% filter(Played==1),
                aes(x=PosPlayer,y=as.factor(RoundID),fill=StatusCore)) +
    geom_tile(alpha=0.7) + coord_flip() + 
    geom_vline(xintercept=Q1,color="gold",size=1.5) +
    geom_vline(xintercept=Q2,color="gold",size=1.5) +
    theme_minimal() + theme(legend.position="none") + 
    xlab("") + ylab("") + labs(fill="") + ggtitle("Time on Ground Rank") +
    theme(plot.title = element_text(size=10)) +
    scale_fill_manual(values=diverge_hcl(3))
  
  
  # continuous chart
  g6b <- ggplot(chart6 %>% filter(Played==1),
                aes(x=PosPlayer,y=as.factor(RoundID),fill=rankStat)) +
    geom_tile() + coord_flip() + 
    geom_vline(xintercept=Q1,color="gold",size=1.5) +
    geom_vline(xintercept=Q2,color="gold",size=1.5) +
    theme_minimal() +  theme(legend.position="none") + 
    xlab("") + ylab("") + labs(fill="") + ggtitle(paste0(xTeamStatID," Rank")) +
    theme(plot.title = element_text(size=10)) +
    scale_fill_gradientn(colors=diverge_hcl(3))
  
  
  # combine charts into one
  gg <- grid.arrange(g6a, g6b, ncol=2,
               top =textGrob(xTeamID,gp=gpar(fontsize=12)))
  return(gg)
}

# Team Avg Score + Rank
# Show PPM per player
# Top/Bottom 5 - Cash Cows vs Premiums
# Price Change
# Breakevens 
# Value - Buy/Hold/Sell
# Scores by Position
# Trade In/Out
# Captains


# Rolling 22 by Position


