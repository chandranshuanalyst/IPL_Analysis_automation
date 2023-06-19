# Package names
packages <- c("ggplot2", "dplyr", "tidyverse", "lubridate","tidyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

ipl_m=read.csv("C:/Users/chandranshu/Desktop/ipl dataset/IPL Matches 2008-2020.csv")
ipl_b=read.csv("C:/Users/chandranshu/Desktop/ipl dataset/IPL Ball-by-Ball 2008-2020.csv")
View(ipl_b)
View(ipl_m)

dim(ipl_m)
dim(ipl_b)

# creating a new field season by formatting year from date
ipl_m$season <- format(as.Date(ipl_m$date), "%Y")
summary(ipl_m)
str(ipl_m)


ipl=merge(ipl_m,ipl_b,by="id")
dim(ipl)
str(ipl)
#There are NA values but we cannot drop them
ipl_s1=ipl[,c("id","team1","team2","venue","toss_winner","winner")]
head(ipl_s1)
ipl_s1=ipl_s1[!duplicated(ipl_s1[,c('id')]),]
head(ipl_s1)
dim(ipl_s1)
unique(ipl_s1$venue)
sum(is.na(ipl_s1$winner))
#So there are 4 null values and they need to be dropped as match was not completed
ipl_s1=drop_na(ipl_s1)

ipl_s2=ipl_s1[ipl_s1$venue=="Holkar Cricket Stadium" ,]
head(ipl_s2)
toss_impact=c()
for(i in 1:length(ipl_s2$id)){
  if(ipl_s2[i,5]==ipl_s2[i,6]){
    toss_impact=append(1,toss_impact)
  }else{
    toss_impact=append(0,toss_impact)
  }
}
if(length(ipl_s2$id)>30){
  t=prop.test(x=sum(toss_impact),n=length(toss_impact), p = 0.5)
}else{
  t=binom.test(x=sum(toss_impact),n=length(toss_impact), p = 0.5)
}
#Thus at M Chinnaswamy Stadium toss is not significant factor in win
if(t$p.value>0.05){
  print(paste0("Thus toss is not significant factor at ","M Chinnaswamy Stadium"))
}


stadium=unique(ipl_s1$venue)

toss=function(venue){
  ipl_s2=ipl_s1[ipl_s1$venue==venue ,]
  toss_impact=c()
  for(i in 1:length(ipl_s2$id)){
    if(ipl_s2[i,5]==ipl_s2[i,6]){
      toss_impact=append(1,toss_impact)
    }else{
      toss_impact=append(0,toss_impact)
    }
  }
  if(length(ipl_s2$id)>30){
    t=prop.test(x=sum(toss_impact),n=length(toss_impact), p = 0.5)
  }else{
    t=binom.test(x=sum(toss_impact),n=length(toss_impact), p = 0.5)
  }
  if(t$p.value>0.05){
    print(paste0("Thus toss is not significant factor at ",venue))
  }
}

for(i in (stadium)){
  toss(i)
}
# Thus toss is not significant in respect of the match result
# The best team on the day wins.


View(ipl)
#Most successful teams
ipl_m2=ipl_m%>%drop_na(result)
loser=c()
for(i in c(1:length(ipl_m2$id))){
  if(ipl_m2$winner[i]==ipl_m2$team1[i]){
    loser=append(ipl_m2$team2[i],loser)
  }else{
    loser=append(ipl_m2$team1[i],loser)
  }
}
ipl_m2['loser']=rev(loser)



w=ipl_m2 %>% 
  group_by(winner) %>%
  summarize(wins = n(), .groups = 'drop')

l=ipl_m2 %>% 
  group_by(loser) %>%
  summarize(loss = n(), .groups = 'drop')
loss=l$loss

w$team=w$winner
l$team=l$loser
team_success=merge(w,l,by="team")[,c(1,3,5)]
team_success["Percent_wins"]=(team_success$wins/(team_success$wins+team_success$loss))*100
team_success[order(team_success$Percent_wins,decreasing=TRUE),]


#Team batter analysis

colnames(ipl_b)
ipl_b2=ipl_b[,-c(3,4,6,9,10,11,12,13,15,18)]
View(ipl_b2)
ipl_b3=ipl_b2[ipl_b2$batting_team=="Rajasthan Royals",-4]

ipl_b4=ipl_b3%>%
  filter(extras_type!="wides"|is.na(extras_type))%>%
  filter(extras_type!="noballs"|is.na(extras_type))
  

df4<- ipl_b4 %>% group_by(batsman) %>%
  summarise(sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),
  innings= sum(table(unique(id))),.groups = 'drop')%>%
  as.data.frame()
noa=ipl_b3%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed)%>%summarise(outs=n())
colnames(noa)[1]="batsman"
df4=merge(df4,noa,by="batsman")
df4["average"]=df4$sum_runs/df4$outs


df3<- ipl_b4 %>% group_by(batsman,inning) %>% 
  summarise(innings= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
  as.data.frame()
noa=ipl_b3%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed,inning)%>%summarise(outs=n())
colnames(noa)[1]="batsman"
df3=merge(df3,noa,by=c("batsman","inning"))
df3["average"]=df3$sum_runs/df3$outs

df3=df3[order(df3$sum_runs,decreasing = TRUE),]

top_inn1=head(df3[df3$inning==1,],5)
top_inn2=head(df3[df3$inning==2,],5)

#4's
df10<- ipl_b3 %>% group_by(batsman) %>% 
  summarise(sum_runs=sum(batsman_runs),inn= sum(table(unique(id))),fours=sum(batsman_runs==4),.groups = 'drop')%>%
  as.data.frame()
#6's
df11<- ipl_b3 %>% group_by(batsman) %>% 
  summarise(sum_runs=sum(batsman_runs),inn= sum(table(unique(id))),sixes=sum(batsman_runs==6),.groups = 'drop')%>%
  as.data.frame()
#Destructive batters

df5<- ipl_b3 %>% group_by(batsman) %>% 
  summarise(sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
  as.data.frame()

df6<- ipl_b3 %>% group_by(batsman,inning) %>% 
  summarise(sum_runs=sum(batsman_runs),sr=mean(batsman_runs)*100,.groups = 'drop')%>%
  as.data.frame()
df6=df6[df6$sum_runs>200,][order(df6[df6$sum_runs>150,4],decreasing = TRUE),]

top_inn1=head(df6[df6$inning==1,],5)
top_inn2=head(df6[df6$inning==2,],5)

#Creating a in general function for innings wise top batters
#For all matches enter any number except 1 and 2 in innings parameter

destructive_bat=function(team,inn,limit){
  ipl_b2=ipl_b[,-c(3,4,6,9,10,11,12,13,15,18)]
  ipl_b3=ipl_b2[ipl_b2$batting_team==team,-4]
  ipl_b4=ipl_b3%>%
    filter(extras_type!="wides"|is.na(extras_type))%>%
    filter(extras_type!="noballs"|is.na(extras_type))
  
  if(inn ==1||inn==2){
    df3<- ipl_b3 %>% group_by(batsman,inning) %>% 
      summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
      as.data.frame()
    
    noa=ipl_b3%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed,inning)%>%summarise(outs=n())
    colnames(noa)[1]="batsman"
    df3=merge(df3,noa,by=c("batsman","inning"))
    df3["average"]=df3$sum_runs/df3$outs
    df3=df3[df3$sum_runs>200,][order(df3[df3$sum_runs>200,5],decreasing = TRUE),]
    top_inn=head(df3[df3$inning==inn,],limit)
  }else{
    df3<- ipl_b3 %>% group_by(batsman) %>% 
      summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,.groups = 'drop')%>%
      as.data.frame()
    
    noa=ipl_b3%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed)%>%summarise(outs=n())
    colnames(noa)[1]="batsman"
    df3=merge(df3,noa,by="batsman")
    df3["average"]=df3$sum_runs/df3$outs
    df3=df3[df3$sum_runs>200,][order(df3[df3$sum_runs>200,4],decreasing = TRUE),]
    top_inn=head(df3,limit)
  }
  return(top_inn)
}
destructive_bat( "Kolkata Knight Riders",2,6)
destructive_bat( "Kolkata Knight Riders",1,6)
destructive_bat( "Kolkata Knight Riders",0,6)
destructive_bat("Rajasthan Royals",2,6)

#top
top_bat=function(team,inn,limit){
  ipl_b2=ipl_b[,-c(3,4,6,9,10,11,12,13,15,18)]
  ipl_b3=ipl_b2[ipl_b2$batting_team==team,]
  
  if(inn ==1||inn==2){
    df3<- ipl_b3 %>% group_by(batsman,inning) %>% 
      summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
      as.data.frame()
   
    noa=ipl_b3%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed,inning)%>%summarise(outs=n())
    colnames(noa)[1]="batsman"
    df3=merge(df3,noa,by=c("batsman","inning"))
    df3["average"]=df3$sum_runs/df3$outs
    df3=df3[order(df3$sum_runs,decreasing = TRUE),]
    top_inn=head(df3[df3$inning==inn,],limit)
  }else{
    df3<- ipl_b3 %>% group_by(batsman) %>% 
      summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,.groups = 'drop')%>%
      as.data.frame()
    
    noa=ipl_b3%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed)%>%summarise(outs=n())
    colnames(noa)[1]="batsman"
    df3=merge(df3,noa,by="batsman")
    df3["average"]=df3$sum_runs/df3$outs
    df3=df3[order(df3$sum_runs,decreasing = TRUE),]
    top_inn=head(df3,limit)
  }
  return(top_inn)
}
top_bat( "Kolkata Knight Riders",2,6)
top_bat( "Kolkata Knight Riders",1,6)
top_bat( "Kolkata Knight Riders",0,6)
top_bat("Rajasthan Royals",2,6)

four_bat=function(team,inn,limit){
  ipl_b2=ipl_b[,-c(3,4,6,9,10,11,12,13,15,16,18)]
  ipl_b3=ipl_b2[ipl_b2$batting_team==team,-4]
  
  if(inn ==1||inn==2){
    df10<- ipl_b3 %>% group_by(batsman,inning) %>% 
      summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),fours=sum(batsman_runs==4),.groups = 'drop')%>%
      as.data.frame()
    df3=df10[order(df10$fours,decreasing = TRUE),]
    top_4=head(df3[df3$inning==inn,],limit)
  }else{
    df10<- ipl_b3 %>% group_by(batsman) %>% 
      summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),fours=sum(batsman_runs==4),.groups = 'drop')%>%
      as.data.frame()
    df3=df10[order(df10$fours,decreasing = TRUE),]
    top_4=head(df3,limit)
  }
  return(top_4)
}
four_bat( "Kolkata Knight Riders",2,6)
four_bat( "Kolkata Knight Riders",1,6)
four_bat( "Kolkata Knight Riders",0,6)

six_bat=function(team,inn,limit){
  ipl_b2=ipl_b[,-c(3,4,6,9,10,11,12,13,15,16,18)]
  ipl_b3=ipl_b2[ipl_b2$batting_team==team,-4]
  
  if(inn ==1||inn==2){
    df10<- ipl_b3 %>% group_by(batsman,inning) %>% 
      summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sixes=sum(batsman_runs==6),.groups = 'drop')%>%
      as.data.frame()
    df3=df10[order(df10$sixes,decreasing = TRUE),]
    top_4=head(df3[df3$inning==inn,],limit)
  }else{
    df10<- ipl_b3 %>% group_by(batsman) %>% 
      summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sixes=sum(batsman_runs==6),.groups = 'drop')%>%
      as.data.frame()
    df3=df10[order(df10$sixes,decreasing = TRUE),]
    top_4=head(df3,limit)
  }
  return(top_4)
}
six_bat( "Kolkata Knight Riders",2,6)
six_bat( "Kolkata Knight Riders",1,6)
six_bat( "Kolkata Knight Riders",0,6)



bat=function(name,team_name,inn,limit){
  x=name(team_name,inn,limit)
  print(x)
}
bat(top_bat,"Mumbai Indians",2,6)
bat(destructive_bat,"Mumbai Indians",2,6)
bat(four_bat,"Mumbai Indians",2,6)
bat(six_bat,"Mumbai Indians",2,6)


#Team bowler analysis
ipl_b5=ipl_b[,-c(3,4,5,6,8,9,11,14,15,17)]
View(ipl_b5)
ipl_b6=ipl_b5[ipl_b5$bowling_team=="Kolkata Knight Riders",]
ipl_b6$is_wicket=replace_na(ipl_b6$is_wicket,0)


sr=ipl_b6 %>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))%>%
  group_by(bowler)%>%
  summarise(runs=sum(total_runs))
balls=ipl_b6 %>%
  filter(extras_type!="wides"|is.na(extras_type))%>%
  filter(extras_type!="noballs"|is.na(extras_type))%>%
  group_by(bowler)%>%
  summarise(balls=n())
df6<- ipl_b6 %>%
  filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))
df6=df6 %>%
 group_by(bowler) %>% 
  summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
  as.data.frame()
df6=merge(df6,sr,by="bowler")
df6=merge(df6,balls,by="bowler")
df6['sr']=df6$balls/df6$sum_wickets
df6['avg']=df6$runs/df6$sum_wickets
df6['eco']=df6$runs/(df6$balls/6)

sr=ipl_b6 %>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))%>%
  group_by(bowler,inning)%>%
  summarise(runs=sum(total_runs))
balls=ipl_b6 %>%
  filter(extras_type!="wides"|is.na(extras_type))%>%
  filter(extras_type!="noballs"|is.na(extras_type))%>%
  group_by(bowler,inning)%>%
  summarise(balls=n())



df6<- ipl_b6 %>% 
  filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))
df7<- df6 %>% 
  group_by(bowler,inning) %>% 
  summarise(matches= sum(table(unique(id))),sum_wkts=sum(is_wicket),.groups = 'drop')%>%#,sr=mean(batsman_runs)*100,.groups = 'drop')%>%
  as.data.frame()
df7=merge(df7,sr,by=c("bowler","inning"))
df7=merge(df7,balls,by=c("bowler","inning"))
df7['sr']=df7$balls/df7$sum_wkts
df7['avg']=df7$runs/df7$sum_wkts
df7['eco']=df7$runs/(df7$balls/6)
df7=df7[order(df7$sum_wkts,decreasing = TRUE),]

top_inn1=head(df7[df7$inning==1,],5)
top_inn2=head(df7[df7$inning==2,],5)

#Creating a in general function for innings wise top bowlers
#For all matches enter any number except 1 and 2 in innings parameter

top_bowl=function(team,inn,limit){
  ipl_b5=ipl_b[,-c(3,4,5,6,8,9,11,14,15,17)]
  ipl_b6=ipl_b5[ipl_b5$bowling_team==team,]
  ipl_b6$is_wicket=replace_na(ipl_b6$is_wicket,0)
  if(inn ==1||inn==2){
    sr=ipl_b6 %>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))%>%
      group_by(bowler,inning)%>%
      summarise(runs=sum(total_runs))
    balls=ipl_b6 %>%
      filter(extras_type!="wides"|is.na(extras_type))%>%
      filter(extras_type!="noballs"|is.na(extras_type))%>%
      group_by(bowler,inning)%>%
      summarise(balls=n())
    df6<- ipl_b6 %>% 
      filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))
    df7<- df6 %>% 
      group_by(bowler,inning) %>% 
      summarise(matches= sum(table(unique(id))),sum_wkts=sum(is_wicket),.groups = 'drop')%>%#,sr=mean(batsman_runs)*100,.groups = 'drop')%>%
      as.data.frame()
    df7=merge(df7,sr,by=c("bowler","inning"))
    df7=merge(df7,balls,by=c("bowler","inning"))
    df7['sr']=df7$balls/df7$sum_wkts
    df7['avg']=df7$runs/df7$sum_wkts
    df7['eco']=df7$runs/(df7$balls/6)
    df7=df7[order(df7$sum_wkts,decreasing = TRUE),]
    df7=df7[order(df7$sum_wkts,decreasing = TRUE),]
    top_inn=head(df7[df7$inning==inn,],limit)
  }else{
    sr=ipl_b6 %>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))%>%
      group_by(bowler)%>%
      summarise(runs=sum(total_runs))
    balls=ipl_b6 %>%
      filter(extras_type!="wides"|is.na(extras_type))%>%
      filter(extras_type!="noballs"|is.na(extras_type))%>%
      group_by(bowler)%>%
      summarise(balls=n())
    df6<- ipl_b6 %>%
      filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))
    df6=df6 %>%
      group_by(bowler) %>% 
      summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
      as.data.frame()
    df6=merge(df6,sr,by="bowler")
    df6=merge(df6,balls,by="bowler")
    df6['sr']=df6$balls/df6$sum_wickets
    df6['avg']=df6$runs/df6$sum_wickets
    df6['eco']=df6$runs/(df6$balls/6)
    df6=df6[order(df6$sum_wickets,decreasing = TRUE),]
    top_inn=head(df6,limit)
  }
  
  
  return(top_inn)
}
top_bowl( "Kolkata Knight Riders",2,6)
top_bowl( "Kolkata Knight Riders",1,6)
top_bowl( "Kolkata Knight Riders",0,6)

#Most economical bowler with atleast 25 wkts

top_1=top_inn1[top_inn1$sum_wkts>25,][order(top_inn1[top_inn1$sum_wkts>25,5]),]
top_2=top_inn2[top_inn2$sum_wkts>25,][order(top_inn2[top_inn2$sum_wkts>25,5]),]

#Poor bowlers
df77=(df7[df7$inning==1,])
poor_1=head(df77[df77$sum_wkts>1,][order(df77[df77$sum_wkts>1,5],decreasing = TRUE),],5)

df88=(df7[df7$inning==2,])
poor_2=head(df88[df88$sum_wkts>1,][order(df88[df88$sum_wkts>1,5],decreasing = TRUE),],5)

#strike bowlers
strike_1=head(df77[df77$sum_wkts>25,][order(df77[df77$sum_wkts>25,6]),],5)
strike_2=head(df88[df88$sum_wkts>25,][order(df88[df88$sum_wkts>25,6]),],5)

strike_bowl=function(team,inn,limit){
  ipl_b5=ipl_b[,-c(3,4,5,6,8,9,11,14,15,17)]
  ipl_b6=ipl_b5[ipl_b5$bowling_team==team,]
  ipl_b6$is_wicket=replace_na(ipl_b6$is_wicket,0)
  if(inn ==1||inn==2){
    sr=ipl_b6 %>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))%>%
      group_by(bowler,inning)%>%
      summarise(runs=sum(total_runs))
    balls=ipl_b6 %>%
      filter(extras_type!="wides"|is.na(extras_type))%>%
      filter(extras_type!="noballs"|is.na(extras_type))%>%
      group_by(bowler,inning)%>%
      summarise(balls=n())
    df6<- ipl_b6 %>% 
      filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))
    df7<- df6 %>% 
      group_by(bowler,inning) %>% 
      summarise(matches= sum(table(unique(id))),sum_wkts=sum(is_wicket),.groups = 'drop')%>%#,sr=mean(batsman_runs)*100,.groups = 'drop')%>%
      as.data.frame()
    df7=merge(df7,sr,by=c("bowler","inning"))
    df7=merge(df7,balls,by=c("bowler","inning"))
    df7['sr']=df7$balls/df7$sum_wkts
    df7['avg']=df7$runs/df7$sum_wkts
    df7['eco']=df7$runs/(df7$balls/6)
    df7=df7[df7$inning==inn,]
    strike=head(df7[df7$sum_wkts>25,][order(df7[df7$sum_wkts>25,7]),],limit)
  }else{
    sr=ipl_b6 %>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))%>%
      group_by(bowler)%>%
      summarise(runs=sum(total_runs))
    balls=ipl_b6 %>%
      filter(extras_type!="wides"|is.na(extras_type))%>%
      filter(extras_type!="noballs"|is.na(extras_type))%>%
      group_by(bowler)%>%
      summarise(balls=n())
    df6<- ipl_b6 %>%
      filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))
    df6=df6 %>%
      group_by(bowler) %>% 
      summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
      as.data.frame()
    df6=merge(df6,sr,by="bowler")
    df6=merge(df6,balls,by="bowler")
    df6['sr']=df6$balls/df6$sum_wickets
    df6['avg']=df6$runs/df6$sum_wickets
    df6['eco']=df6$runs/(df6$balls/6)
    strike=head(df6[df6$sum_wickets>25,][order(df6[df6$sum_wickets>25,6]),],limit)
  }
  return(strike)
}
strike_bowl( "Kolkata Knight Riders",2,6)
strike_bowl( "Kolkata Knight Riders",1,6)
strike_bowl( "Kolkata Knight Riders",0,6)


poor_bowl=function(team,inn,limit){
  ipl_b5=ipl_b[,-c(3,4,5,6,8,9,11,14,15,17)]
  ipl_b6=ipl_b5[ipl_b5$bowling_team==team,]
  ipl_b6$is_wicket=replace_na(ipl_b6$is_wicket,0)
  if(inn ==1||inn==2){
    sr=ipl_b6 %>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))%>%
      group_by(bowler,inning)%>%
      summarise(runs=sum(total_runs))
    balls=ipl_b6 %>%
      filter(extras_type!="wides"|is.na(extras_type))%>%
      filter(extras_type!="noballs"|is.na(extras_type))%>%
      group_by(bowler,inning)%>%
      summarise(balls=n())
    df6<- ipl_b6 %>% 
      filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))
    df7<- df6 %>% 
      group_by(bowler,inning) %>% 
      summarise(matches= sum(table(unique(id))),sum_wkts=sum(is_wicket),.groups = 'drop')%>%#,sr=mean(batsman_runs)*100,.groups = 'drop')%>%
      as.data.frame()
    df7=merge(df7,sr,by=c("bowler","inning"))
    df7=merge(df7,balls,by=c("bowler","inning"))
    df7['sr']=df7$balls/df7$sum_wkts
    df7['avg']=df7$runs/df7$sum_wkts
    df7['eco']=df7$runs/(df7$balls/6)
    df7=df7[df7$inning==inn,]
    poor=head(df7[df7$sum_wkts>1,][order(df7[df7$sum_wkts>1,7],decreasing = TRUE),],5)
  }else{
    sr=ipl_b6 %>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))%>%
      group_by(bowler)%>%
      summarise(runs=sum(total_runs))
    balls=ipl_b6 %>%
      filter(extras_type!="wides"|is.na(extras_type))%>%
      filter(extras_type!="noballs"|is.na(extras_type))%>%
      group_by(bowler)%>%
      summarise(balls=n())
    df6<- ipl_b6 %>%
      filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))
    df6=df6 %>%
      group_by(bowler) %>% 
      summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
      as.data.frame()
    df6=merge(df6,sr,by="bowler")
    df6=merge(df6,balls,by="bowler")
    df6['sr']=df6$balls/df6$sum_wickets
    df6['avg']=df6$runs/df6$sum_wickets
    df6['eco']=df6$runs/(df6$balls/6)
    poor=head(df6[df6$sum_wickets>1,][order(df6[df6$sum_wickets>1,6],decreasing = TRUE),],limit)
  }
  return(poor)
}
poor_bowl( "Kolkata Knight Riders",2,6)
poor_bowl( "Kolkata Knight Riders",1,6)
poor_bowl( "Kolkata Knight Riders",0,6)

eco_bowl=function(team,inn,limit){
  ipl_b5=ipl_b[,-c(3,4,5,6,8,9,11,14,15,17)]
  ipl_b6=ipl_b5[ipl_b5$bowling_team==team,]
  ipl_b6$is_wicket=replace_na(ipl_b6$is_wicket,0)
  if(inn ==1||inn==2){
    sr=ipl_b6 %>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))%>%
      group_by(bowler,inning)%>%
      summarise(runs=sum(total_runs))
    balls=ipl_b6 %>%
      filter(extras_type!="wides"|is.na(extras_type))%>%
      filter(extras_type!="noballs"|is.na(extras_type))%>%
      group_by(bowler,inning)%>%
      summarise(balls=n())
    df6<- ipl_b6 %>% 
      filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))
    df7<- df6 %>% 
      group_by(bowler,inning) %>% 
      summarise(matches= sum(table(unique(id))),sum_wkts=sum(is_wicket),.groups = 'drop')%>%#,sr=mean(batsman_runs)*100,.groups = 'drop')%>%
      as.data.frame()
    df7=merge(df7,sr,by=c("bowler","inning"))
    df7=merge(df7,balls,by=c("bowler","inning"))
    df7['sr']=df7$balls/df7$sum_wkts
    df7['avg']=df7$runs/df7$sum_wkts
    df7['eco']=df7$runs/(df7$balls/6)
    df7=df7[df7$inning==inn,]
    eco=head(df7[df7$sum_wkts>25,][order(df7[df7$sum_wkts>25,9]),],limit)
  }else{
    sr=ipl_b6 %>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))%>%
      group_by(bowler)%>%
      summarise(runs=sum(total_runs))
    balls=ipl_b6 %>%
      filter(extras_type!="wides"|is.na(extras_type))%>%
      filter(extras_type!="noballs"|is.na(extras_type))%>%
      group_by(bowler)%>%
      summarise(balls=n())
    df6<- ipl_b6 %>%
      filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
      filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
      filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
      filter(extras_type!="byes"|is.na(extras_type))%>%
      filter(extras_type!="legbyes"|is.na(extras_type))
    df6=df6 %>%
      group_by(bowler) %>% 
      summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
      as.data.frame()
    df6=merge(df6,sr,by="bowler")
    df6=merge(df6,balls,by="bowler")
    df6['sr']=df6$balls/df6$sum_wickets
    df6['avg']=df6$runs/df6$sum_wickets
    df6['eco']=df6$runs/(df6$balls/6)
    eco=head(df6[df6$sum_wickets>25,][order(df6[df6$sum_wickets>25,4]),],limit)
  }
  return(eco)
}
eco_bowl( "Kolkata Knight Riders",2,6)
eco_bowl( "Kolkata Knight Riders",1,6)
eco_bowl( "Kolkata Knight Riders",0,6)

bowl=function(name,team_name,inn,limit){
  x=name(team_name,inn,limit)
  print(x)
}
bowl(top_bowl,"Mumbai Indians",0,6)
bowl(eco_bowl,"Mumbai Indians",0,6)
bowl(poor_bowl,"Mumbai Indians",0,6)
bowl(strike_bowl,"Mumbai Indians",0,6)

impact=function(name){
  df=ipl[ipl$bowling_team==name&ipl$winner==name,-c(2,3,5,6,7,8,9,10,12:35)]
  tt3=df%>%
    distinct(id,player_of_match)%>%
    group_by(player_of_match)%>%
    summarise(pom=n())%>%
    arrange(desc(pom))%>%
    head(3)%>%
    as.data.frame()
  return(tt3)
}
impact("Mumbai Indians")

top_bat_season=function(name){
  df=ipl[ipl$batting_team==name,]
  tt=df%>%
    group_by(season,batsman)%>%
    summarise(matches=sum(table(unique(id))),runs=sum(batsman_runs),.groups = 'drop')
  tt=tt%>%
    group_by(season)%>%
    filter(runs==max(runs))%>%
    as.data.frame()
  return(tt)
}

top_bowl_season=function(name){
  df=ipl[ipl$bowling_team==name,]
  tt2=df%>%
    filter(dismissal_kind!="run out")%>%
    group_by(season,bowler)%>%
    summarise(matches=sum(table(unique(id))),wkts=sum(is_wicket),.groups = 'drop')
  tt2=tt2%>%
    group_by(season)%>%
    filter(wkts==max(wkts))%>%
    as.data.frame()
  return(tt2)
}
print(top_bowl_season("Mumbai Indians"))


team=function(name){
  print(paste0("The key batters and bowlers of the franchise ",name," are:"))
  print(paste0("The top 5 run scorers of ",name," are :"))
  print(top_bat(name,0,5))
  print(paste0("The top 5 hitters of ",name," are :"))
  print(destructive_bat(name,0,5))
  print(paste0("The top 5 boundary hitters of ",name," are :"))
  print(four_bat(name,0,5))
  print(paste0("The top 5 6 hitters of ",name," are :"))
  print(six_bat(name,0,5))
  print(paste0("The top 5 bowlers of ",name," are :"))
  print(top_bowl(name,0,5))
  print(paste0("The top 5 strike bowlers of ",name," are :"))
  print(strike_bowl(name,0,5))
  print(paste0("The top 5 economical bowlers of ",name," are :"))
  print(eco_bowl(name,0,5))
  print(paste0("The poor bowling resources of ",name," are :"))
  print(poor_bowl(name,0,5))
  print(paste0("Innings wise analysis of players"))
  print("1st Innings")
  print(paste0("The top 5 run scorers of ",name," are :"))
  print(top_bat(name,1,5))
  print(paste0("The top 5 hitters of ",name," are :"))
  print(destructive_bat(name,1,5))
  print(paste0("The top 5 boundary hitters of ",name," are :"))
  print(four_bat(name,1,5))
  print(paste0("The top 5 6 hitters of ",name," are :"))
  print(six_bat(name,1,5))
  print(paste0("The top 5 bowlers of ",name," are :"))
  print(top_bowl(name,1,5))
  print(paste0("The top 5 strike bowlers of ",name," are :"))
  print(strike_bowl(name,1,5))
  print(paste0("The top 5 economical bowlers of ",name," are :"))
  print(eco_bowl(name,1,5))
  print(paste0("The poor bowling resources of ",name," are :"))
  print(poor_bowl(name,1,5))
  print("IInd Innings")
  print(paste0("The top 5 run scorers of ",name," are :"))
  print(top_bat(name,2,5))
  print(paste0("The top 5 hitters of ",name,"are :"))
  print(destructive_bat(name,2,5))
  print(paste0("The top 5 boundary hitters of ",name," are :"))
  print(four_bat(name,2,5))
  print(paste0("The top 5 6 hitters of ",name," are :"))
  print(six_bat(name,2,5))
  print(paste0("The top 5 bowlers of ",name," are :"))
  print(top_bowl(name,2,5))
  print(paste0("The top 5 strike bowlers of ",name," are :"))
  print(strike_bowl(name,2,5))
  print(paste0("The top 5 economical bowlers of ",name," are :"))
  print(eco_bowl(name,2,5))
  print(paste0("The poor bowling resources of ",name," are :"))
  print(poor_bowl(name,2,5))
  print("Season Wise analysis")
  print(paste0("The top batters of ",name," each season are:"))
  print(top_bat_season(name))
  print(paste0("The top bowlers of ",name," each season are:"))
  print(top_bowl_season(name))
  print("Impact players over years")
  print(impact(name))
}
team("Mumbai Indians")
name_pick=function(option){
  if(option==1){
    return("Kolkata Knight Riders")
  }else if(option ==2){
    return("Royal Challengers Bangalore")
  }else if(option==3){
    return("Kings XI Punjab")
  }else if(option ==4){
    return("Chennai Super Kings")
  }else if(option==5){
    return("Rajasthan Royals")
  }else if(option ==6){
    return("Delhi Daredevils")
  }else if(option ==7){
    return("Mumbai Indians")
  }else if(option ==8){
    return("Deccan Chargers")
  }else if(option == 9){
    return("Kochi Tuskers Kerala")
  }else if(option ==10){
    return("Pune Warriors")
  }else if(option == 11){
    return("Sunrisers Hyderabad")
  }else if(option == 12){
    return("Rising Pune Supergiants")
  }else if(option ==13){
    return("Gujarat Lions")
  }else if(option ==14){
    return("Rising Pune Supergiant")
  }else{
    return("Delhi Capitals")
  }
}
print("Choose team number")
print(cat("1 Kolkata Knight Riders,\n
      2 Royal Challengers Bangalore,\n
      3 Kings XI Punjab, \n
      4 Chennai Super Kings,\n
      5 Rajasthan Royals,\n
      6 Delhi Daredevils,  \n
      7 Mumbai Indians,\n
      8 Deccan Chargers,\n
      9 Kochi Tuskers Kerala , \n  
      10 Pune Warriors ,\n
      11 Sunrisers Hyderabad ,\n
      12 Rising Pune Supergiants,\n

      13 Gujarat Lions,\n
      14 Rising Pune Supergiant,\n
      15 Delhi Capitals"))
team(name_pick(5))

####################################################################

#Now we look at top grounds for a team and poor ground for a team
csk_m=ipl_m[ipl_m$team1=="Chennai Super Kings"|ipl_m$team2=="Chennai Super Kings",]
t=csk_m%>%
  group_by(venue)%>%
  summarise(matches=n(),wins=sum(winner%in%"Chennai Super Kings"),percent=wins/matches*100)%>%
  as.data.frame()
View(t)
#season wise
csk=ipl[ipl$batting_team=="Chennai Super Kings",]
#Season wise csk max runs
tt=csk%>%
  group_by(season,batsman)%>%
  summarise(matches=sum(table(unique(id))),runs=sum(batsman_runs),.groups = 'drop')
tt=tt%>%
  group_by(season)%>%
  filter(runs==max(runs))
#Season wise csk max wickets
csk=ipl[ipl$bowling_team=="Mumbai Indians",]
tt2=csk%>%
  filter(dismissal_kind!="run out")%>%
  group_by(season,bowler)%>%
  summarise(matches=sum(table(unique(id))),wkts=sum(is_wicket),.groups = 'drop')
tt2=tt2%>%
  group_by(season)%>%
  filter(wkts==max(wkts))
#Impactful players
csk=ipl[ipl$bowling_team=="Chennai Super Kings"&ipl$winner=="Chennai Super Kings",-c(2,3,5,6,7,8,9,10,12:35)]
tt3=csk%>%
  distinct(id,player_of_match)%>%
  group_by(player_of_match)%>%
  summarise(pom=n())%>%
  arrange(desc(pom))%>%
  head(3)%>%
  as.data.frame()

impact=function(name){
  df=ipl[ipl$bowling_team==name&ipl$winner==name,-c(2,3,5,6,7,8,9,10,12:35)]
  tt3=df%>%
    distinct(id,player_of_match)%>%
    group_by(player_of_match)%>%
    summarise(pom=n())%>%
    arrange(desc(pom))%>%
    head(3)%>%
    as.data.frame()
  return(tt3)
}
impact("Mumbai Indians")

top_bat_season=function(name){
  df=ipl[ipl$batting_team==name,]
  tt=df%>%
    group_by(season,batsman)%>%
    summarise(matches=sum(table(unique(id))),runs=sum(batsman_runs),.groups = 'drop')
  tt=tt%>%
    group_by(season)%>%
    filter(runs==max(runs))%>%
    as.data.frame()
  return(tt)
}

top_bowl_season=function(name){
  df=ipl[ipl$bowling_team==name,]
  tt2=df%>%
    group_by(season,bowler)%>%
    summarise(matches=sum(table(unique(id))),wkts=sum(is_wicket),.groups = 'drop')
  tt2=tt2%>%
    group_by(season)%>%
    filter(wkts==max(wkts))%>%
    as.data.frame()
  return(tt2)
}
print(top_bowl_season("Chennai Super Kings"))

#Team best over by over player
csk=ipl[ipl$batting_team=="Chennai Super Kings",]
df=csk%>%
  group_by(batsman,over)%>%
  summarise(runs=sum(batsman_runs))
df=df%>%
  group_by(over)%>%
  filter(runs==max(runs))%>%
  arrange(over)%>%
  as.data.frame()

over_analysis=function(team){
  team=name_pick(team)
  df=ipl[ipl$batting_team==team,]
  df1=df%>%
    group_by(batsman,over)%>%
    summarise(runs=sum(batsman_runs))
  df1=df1%>%
    group_by(over)%>%
    filter(runs==max(runs))%>%
    arrange(over)%>%
    as.data.frame()
  return(df1)
}

over_analysis(4)

#Position analysis

csk=ipl[ipl$batting_team=="Chennai Super Kings",]
csk=csk[order(csk$date,csk$inning,csk$over,csk$ball),]
position=c()
for(i in unique(csk$date)){
  df=csk[csk$date==i,]
  b=unique(df$batsman)
  osr=df$batsman[1]
  onsr=df$non_striker[1]
  batsman=c(c(osr,onsr), b[!b %in% c(onsr,osr)])
  bat_order=c(1:length(batsman))
  for(j in 1:((dim(df)[1]))){
    l=length(position)
    for(k in bat_order)
    if(df$batsman[j]==batsman[k])
      position[l+1]=bat_order[k]
    
  }
}
length(position)
csk['position']=position




ipl_b4=csk%>%
  filter(extras_type!="wides"|is.na(extras_type))%>%
  filter(extras_type!="noballs"|is.na(extras_type))


df4<- ipl_b4 %>% group_by(position,batsman) %>%
  summarise(sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),
            innings= sum(table(unique(id))),.groups = 'drop')%>%
  as.data.frame()
noa=csk%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed)%>%summarise(outs=n())
colnames(noa)[1]="batsman"
df4=merge(df4,noa,by="batsman")
df4["average"]=df4$sum_runs/df4$outs


df3<- ipl_b4 %>% group_by(batsman,inning) %>% 
  summarise(innings= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
  as.data.frame()
noa=csk%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed,inning)%>%summarise(outs=n())
colnames(noa)[1]="batsman"
df3=merge(df3,noa,by=c("batsman","inning"))
df3["average"]=df3$sum_runs/df3$outs

df3=df3[order(df3$sum_runs,decreasing = TRUE),]

df=csk%>%
  group_by(position,batsman)%>%
  summarise(matches=sum(table(unique(id))),runs=sum(batsman_runs),sr=runs/n()*100,avg=runs/sum(table(player_dismissed)))
df=df%>%
  group_by(position)%>%
  filter(runs==max(runs))
#Position analysis function

position_runs=function(team){
  team=name_pick(team)
  csk=ipl[ipl$batting_team==team,]
  csk=csk[order(csk$date,csk$inning,csk$over,csk$ball),]
  position=c()
  for(i in unique(csk$date)){
    df=csk[csk$date==i,]
    b=unique(df$batsman)
    osr=df$batsman[1]
    onsr=df$non_striker[1]
    batsman=c(c(osr,onsr), b[!b %in% c(onsr,osr)])
    bat_order=c(1:length(batsman))
    for(j in 1:((dim(df)[1]))){
      l=length(position)
      for(k in bat_order)
        if(df$batsman[j]==batsman[k])
          position[l+1]=bat_order[k]
      
    }
  }
  length(position)
  csk['position']=position
  
  
  
  
  ipl_b4=csk%>%
    filter(extras_type!="wides"|is.na(extras_type))%>%
    filter(extras_type!="noballs"|is.na(extras_type))
  
  
  df4<- ipl_b4 %>% group_by(position,batsman) %>%
    summarise(sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),
              innings= sum(table(unique(id))),.groups = 'drop')%>%
    as.data.frame()
  noa=csk%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed)%>%summarise(outs=n())
  colnames(noa)[1]="batsman"
  df4=merge(df4,noa,by="batsman")
  df4["average"]=df4$sum_runs/df4$outs
  
  
  df3<- ipl_b4 %>% group_by(batsman,inning) %>% 
    summarise(innings= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
    as.data.frame()
  noa=csk%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed,inning)%>%summarise(outs=n())
  colnames(noa)[1]="batsman"
  df3=merge(df3,noa,by=c("batsman","inning"))
  df3["average"]=df3$sum_runs/df3$outs
  
  df3=df3[order(df3$sum_runs,decreasing = TRUE),]
  
  df=csk%>%
    group_by(position,batsman)%>%
    summarise(matches=sum(table(unique(id))),runs=sum(batsman_runs),sr=runs/n()*100,avg=runs/sum(table(player_dismissed)))
  df=df%>%
    group_by(position)%>%
    filter(runs==max(runs))
  return(df)
}
position_runs(4)


#####################################################################
#Part 2 : TEAM VS TEAM
#####################################################################

csk=ipl[ipl$batting_team=="Chennai Super Kings",]
mi=ipl[ipl$bowling_team=="Mumbai Indians",]
csk_mi=ipl[ipl$batting_team=="Chennai Super Kings"&ipl$bowling_team=="Mumbai Indians",]
df=csk_mi%>%group_by(batsman)%>%
  summarise(matches=sum(table(unique(id))),runs=sum(batsman_runs),sr=runs/n()*100)%>%
  arrange(desc(runs))%>%
  head(3)%>%
  as.data.frame()
df2=csk_mi%>%
  filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  group_by(bowler)%>%
  summarise(matches=sum(table(unique(id))),wickets=sum(is_wicket))%>%
  arrange(desc(wickets))%>%
  head(3)%>%
  as.data.frame()

batting_vsteam=function(bat,bowl){
  bat=name_pick(bat)
  bowl=name_pick(bowl)
  df1=ipl[ipl$batting_team==bat&ipl$bowling_team==bowl,]
  df2=df1%>%group_by(batsman)%>%
    summarise(matches=sum(table(unique(id))),runs=sum(batsman_runs),sr=runs/n()*100)%>%
    arrange(desc(runs))%>%
    head(5)%>%
    as.data.frame()
  return (df2)
}
bowling_vsteam=function(bat,bowl){
  bat=name_pick(bat)
  bowl=name_pick(bowl)
  df1=ipl[ipl$batting_team==bat&ipl$bowling_team==bowl,]
  df2=df1%>%
    filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    group_by(bowler)%>%
    summarise(matches=sum(table(unique(id))),wickets=sum(is_wicket))%>%
    arrange(desc(wickets))%>%
    head(5)%>%
    as.data.frame()
  return (df2)
}
print("Choose team number")
print(cat("1 Kolkata Knight Riders,\n
      2 Royal Challengers Bangalore,\n
      3 Kings XI Punjab, \n
      4 Chennai Super Kings,\n
      5 Rajasthan Royals,\n
      6 Delhi Daredevils,  \n
      7 Mumbai Indians,\n
      8 Deccan Chargers,\n
      9 Kochi Tuskers Kerala , \n  
      10 Pune Warriors ,\n
      11 Sunrisers Hyderabad ,\n
      12 Rising Pune Supergiants,\n
      13 Gujarat Lions,\n
      14 Rising Pune Supergiant,\n
      15 Delhi Capitals"))
tvt=function(t1,t2){
  print(paste0("Top performers of ",name_pick(t1)," VS ",name_pick(t2)," are:"))
  print(paste0("Top ",name_pick(t1)," performers are:"))
  print(batting_vsteam(t1,t2))
  print(bowling_vsteam(t2,t1))
  print(paste0("Top ",name_pick(t2)," performers are:"))
  print(batting_vsteam(t2,t1))
  print(bowling_vsteam(t1,t2))
}
tvt(10,11)

#Common toss decision by ground
toss_result=ipl%>%
  group_by(venue,toss_decision)%>%
  summarise(count=sum(table(unique(id))))
toss_result=toss_result%>%
  group_by(venue)%>%
  filter(count==max(count))%>%
  as.data.frame()

library(ggplot2)

# Stacked + percent
plot=ggplot(toss_result, aes(fill=toss_decision, y=count, x=venue)) + 
  geom_bar(position="fill", stat="identity")
plot=plot+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
(plot)

fifty=ipl%>%
  group_by(batsman,date)%>%
  summarise(runs_scored=sum(batsman_runs))
fifty=fifty%>%
  group_by(batsman,runs_scored)%>%
  filter(runs_scored>=50)
fifty=fifty%>%
  group_by(batsman)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%
  head(10)%>%
  as.data.frame()

#Plot
plot=ggplot(fifty, aes(y=count, x=batsman)) + 
  geom_bar( stat="identity",fill="#69b3a2")
plot=plot+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ ggtitle("Top 10 batsman scored 50 in ipl")
(plot)

hundred=ipl%>%
  group_by(batsman,date)%>%
  summarise(runs_scored=sum(batsman_runs))
hundred=hundred%>%
  group_by(batsman,runs_scored)%>%
  filter(runs_scored>=100)
hundred=hundred%>%
  group_by(batsman)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%
  head(10)%>%
  as.data.frame()
#Plot
plot=ggplot(hundred, aes(y=count, x=batsman)) + 
  geom_bar( stat="identity",fill="#69b3a2",)
plot=plot+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ ggtitle("Top 10 batsman scored a 100 in ipl")
(plot)

extras=ipl%>%
  group_by(bowling_team,extras_type)%>%
  summarise(count=sum(table(unique(id))))
wides=extras%>%
  group_by(bowling_team,extras_type)%>%
  filter(extras_type=="wides")%>%
  arrange(desc(count))%>%
  as.data.frame()
plot=ggplot(wides, aes(y=count, x=bowling_team)) + 
  geom_bar( stat="identity",fill="#69b3a2",)
plot=plot+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ ggtitle("Number of wides conceded by bowling team")
(plot)

noballs=extras%>%
  group_by(bowling_team,extras_type)%>%
  filter(extras_type=="noballs")%>%
  arrange(desc(count))%>%
  as.data.frame()
plot=ggplot(noballs, aes(y=count, x=bowling_team)) + 
  geom_bar( stat="identity",fill="#69b3a2",)
plot=plot+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ ggtitle("Number of wides conceded by bowling team")
(plot)
#######################################################################################
# Package names
packages <- c("ggplot2", "dplyr", "tidyverse", "lubridate","tidyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

ipl_m=read.csv("C:/Users/chandranshu/Desktop/ipl dataset/IPL Matches 2008-2020.csv")
ipl_b=read.csv("C:/Users/chandranshu/Desktop/ipl dataset/IPL Ball-by-Ball 2008-2020.csv")
# creating a new field season by formatting year from date
ipl_m$season <- format(as.Date(ipl_m$date), "%Y")
ipl=merge(ipl_m,ipl_b,by="id")

#Individual Analysis
bat2=data.frame(matrix(NA,nrow=1,ncol=length(colnames(ipl))+1))
bat2=bat2[-1,]
names(bat2)=c(colnames(ipl),"position")
for(k in unique(ipl$batting_team)){
  bats=ipl[ipl$batting_team==k,]
  bats=bats[order(bats$date,bats$inning,bats$over,bats$ball),]
  position=c()
  for(i in unique(bats$date)){
    df=bats[bats$date==i,]
    b=unique(df$batsman)
    osr=df$batsman[1]
    onsr=df$non_striker[1]
    batsman=c(c(osr,onsr), b[!b %in% c(onsr,osr)])
    bat_order=c(1:length(batsman))
    for(j in 1:((dim(df)[1]))){
      l=length(position)
      for(k in bat_order)
        if(df$batsman[j]==batsman[k])
          position[l+1]=bat_order[k]
      
    }
  }
  bats['position']=position
  bat2=rbind(bat2,bats)
  
}
bat=bat2[bat2$batsman=="SK Raina" ,]
df3<- bat %>% group_by(batsman) %>% 
  summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
  as.data.frame()

noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed)%>%summarise(outs=n())
colnames(noa)[1]="batsman"
df3=merge(df3,noa,by="batsman")
df3["average"]=df3$sum_runs/df3$outs
df3['not_outs']=df3$matches-df3$outs
f<- bat %>% group_by(batsman) %>% 
  summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
  as.data.frame()
#6's
s<- bat %>% group_by(batsman) %>% 
  summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
  as.data.frame()
list_df = list(df3,f,s)
df3 <- list_df %>% reduce(right_join, by='batsman')
df3
ducks= bat %>% group_by(date) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs==0)
hundred<- bat %>% group_by(date) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs>=100)
fifty<- bat %>% group_by(date) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs>=50&runs<100)
df3['fifty']=max(length(fifty$runs),0)
df3['hundred']=max(length(hundred$runs),0)
df3['hs']=max(max(hundred$runs),max(fifty$runs))
df3['ducks']=length(ducks$runs)
df3["boundary_percent"]=((df3$fours+df3$sixes)/df3$balls_faced)*100
mom=bat%>%
  group_by(player_of_match)%>%
  filter(player_of_match=="SK Raina")%>%
  summarise(mom=sum(table(unique(id))))%>%
  as.data.frame()
df3["mom"]=mom$mom
overall_perf=function(df3){
  print(paste0("Matches     -   ",df3$matches))
  print(paste0("Not Outs    -   ",df3$not_outs))
  print(paste0("Runs        -   ",df3$sum_runs))
  print(paste0("Ball Faced  -   ",df3$balls_faced))
  print(paste0("High Score  -   ",df3$hs))
  print(paste0("Average     -   ",df3$average))
  print(paste0("Strike Rate -   ",df3$sr))
  print(paste0("50*         -   ",df3$fifty))
  print(paste0("100*        -   ",df3$hundred))
  print(paste0("4s          -   ",df3$fours))
  print(paste0("6s          -   ",df3$sixes))
  print(paste0("Man of Match-   ",df3$mom))
  print(paste0("Boundary %  -   ",df3$boundary_percent))
  print(paste0("Ducks       -   ",df3$ducks))
}


batsman_performance=function(b){
  bat=bat2[bat2$batsman==b ,]
  df3<- bat %>% group_by(batsman) %>% 
    summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
    as.data.frame()
  
  noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed)%>%summarise(outs=n())
  colnames(noa)[1]="batsman"
  df3=merge(df3,noa,by="batsman")
  df3["average"]=df3$sum_runs/df3$outs
  df3['not_outs']=df3$matches-df3$outs
  f<- bat %>% group_by(batsman) %>% 
    summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
    as.data.frame()
  #6's
  s<- bat %>% group_by(batsman) %>% 
    summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
    as.data.frame()
  list_df = list(df3,f,s)
  df3 <- list_df %>% reduce(right_join, by='batsman')
  ducks= bat %>% group_by(date) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs==0)
  hundred<- bat %>% group_by(date) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs>=100)
  fifty<- bat %>% group_by(date) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs>=50&runs<100)
  df3['fifty']=max(length(fifty$runs),0)
  df3['hundred']=max(length(hundred$runs),0)
  df3['hs']=max(max(hundred$runs,0),max(fifty$runs,0))
  df3['ducks']=length(ducks$runs)
  df3["boundary_percent"]=((df3$fours+df3$sixes)/df3$balls_faced)*100
  mom=bat%>%
    group_by(player_of_match)%>%
    filter(player_of_match==b)%>%
    summarise(mom=sum(table(unique(id))))%>%
    as.data.frame()
  df3["mom"]=max(mom$mom,0)
  overall_perf(df3 = df3)
}
batsman_performance("SK Raina")

position_wise=function(b){
  bat=bat2[bat2$batsman==b ,]
  df4=bat%>%
    group_by(position)%>%
    summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
    as.data.frame()
  
  noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(position,player_dismissed)%>%summarise(outs=n())
  colnames(noa)[2]="batsman"
  noa=noa%>%filter(!is.na(batsman))
  noa=noa[noa$batsman=="SK Raina",]
  df4=merge(df4,noa,by=c("position"),all.x = TRUE)
  df4["average"]=df4$sum_runs/df4$outs
  df4['not_outs']=df4$matches-df4$outs
  f<- bat %>% group_by(position,batsman) %>% 
    summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
    as.data.frame()
  #6's
  s<- bat %>% group_by(position,batsman) %>% 
    summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
    as.data.frame()
  list_df = list(df4,f,s)
  df4 <- list_df %>% reduce(left_join, by=c('position','batsman'))

  hundred<- bat %>% group_by(date,position) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs>=100)
  hundred<-hundred%>%
    group_by(position)%>%
    summarise(hundred=n())
  if(dim(hundred)[1]==0){
    hundred=matrix(c(df4$position,rep(0,length(df4$position))),nrow = length(df4$position),ncol=2)
    colnames(hundred)=c("position","hundred")
  }
  df4=merge(df4,hundred,by="position",all.x=TRUE)

  fifty<- bat %>% group_by(date,position) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs>=50&runs<100)
  fifty<-fifty%>%
    group_by(position)%>%
    summarise(fifty=n())
  
  df4=merge(df4,fifty,by="position",all.x = TRUE)

  hs=bat%>%
    group_by(position,id)%>%
    summarise(runs=sum(batsman_runs))
  hs=hs%>%
    group_by(position)%>%
    filter(runs==max(runs))
  hs=hs[,-2]
  colnames(hs)[2]="hs"
  df4=merge(df4,hs,by="position",all.x = TRUE)

  ducks=bat%>%
    group_by(position,date)%>%
    summarise(runs=sum(batsman_runs))
  ducks=ducks[,-2]
  ducks=ducks%>%
    group_by(position)%>%
    filter(runs==0)
  ducks=ducks%>%
    group_by(position)%>%
    summarise(ducks=n())
  if(dim(ducks)[1]==0){
    ducks=data.frame(matrix(c(df4$position,rep(0,length(df4$position))),ncol=2,nrow=length(df4$position)))
    colnames(ducks)=c("position","ducks")
  }
  df4=merge(df4,ducks,by="position",all.x = TRUE)

  df4["boundary_percent"]=((df4$fours+df4$sixes)/df4$balls_faced)*100

  mom=bat%>%
    group_by(position,player_of_match)%>%
    filter(player_of_match==b)%>%
    summarise(mom=sum(table(unique(id))))%>%
    as.data.frame()
  
  mom=mom[,-2]
  if(dim(mom)[1]==0){
    mom=data.frame(matrix(c(df4$position,rep(0,length(df4$position))),ncol=2,nrow=length(df4$position)))
    colnames(mom)=c("position","mom")
  }
  df4=merge(df4,mom,by="position",all.x = TRUE)
  df4[is.na(df4)]=0
  df4=select(df4,-"batsman")
  return(df4)
}
print(position_wise("SK Raina"))


bat=bat2[bat2$batsman=="SK Raina" ,]
df4=bat%>%
  group_by(position)%>%
  summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
  as.data.frame()

noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(position,player_dismissed)%>%summarise(outs=n())
colnames(noa)[2]="batsman"
noa=noa%>%filter(!is.na(batsman))
noa=noa[noa$batsman=="SK Raina",]
df4=merge(df4,noa,by=c("position"),all.x = TRUE)
df4["average"]=df4$sum_runs/df4$outs
df4['not_outs']=df4$matches-df4$outs
f<- bat %>% group_by(position,batsman) %>% 
  summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
  as.data.frame()
#6's
s<- bat %>% group_by(position,batsman) %>% 
  summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
  as.data.frame()
list_df = list(df4,f,s)
df4 <- list_df %>% reduce(left_join, by=c('position','batsman'))
df4
hundred<- bat %>% group_by(date,position) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs>=100)
hundred<-hundred%>%
  group_by(position)%>%
  summarise(hundred=n())
if(dim(hundred)[1]==0){
  hundred=matrix(c(df4$position,rep(0,length(df4$position))),nrow = length(df4$position),ncol=2)
  colnames(hundred)=c("position","hundred")
}
df4=merge(df4,hundred,by="position",all.x=TRUE)
df4
fifty<- bat %>% group_by(date,position) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs>=50&runs<100)
fifty<-fifty%>%
  group_by(position)%>%
  summarise(fifty=n())

df4=merge(df4,fifty,by="position",all.x = TRUE)
df4
hs=bat%>%
  group_by(position,id)%>%
  summarise(runs=sum(batsman_runs))
hs=hs%>%
  group_by(position)%>%
  filter(runs==max(runs))
hs=hs[,-2]
colnames(hs)[2]="hs"
df4=merge(df4,hs,by="position",all.x = TRUE)
df4
ducks=bat%>%
  group_by(position,date)%>%
  summarise(runs=sum(batsman_runs))
ducks=ducks[,-2]
ducks=ducks%>%
  group_by(position)%>%
  filter(runs==0)
ducks=ducks%>%
  group_by(position)%>%
  summarise(ducks=n())
if(dim(ducks)[1]==0){
  ducks=data.frame(matrix(c(df4$position,rep(0,length(df4$position))),ncol=2,nrow=length(df4$position)))
  colnames(ducks)=c("position","ducks")
}
df4=merge(df4,ducks,by="position",all.x = TRUE)
df4
df4["boundary_percent"]=((df4$fours+df4$sixes)/df4$balls_faced)*100
df4
mom=bat%>%
  group_by(position,player_of_match)%>%
  filter(player_of_match=="SK Raina")%>%
  summarise(mom=sum(table(unique(id))))%>%
  as.data.frame()

mom=mom[,-2]
if(dim(mom)[1]==0){
  mom=data.frame(matrix(c(df4$position,rep(0,length(df4$position))),ncol=2,nrow=length(df4$position)))
  colnames(mom)=c("position","mom")
}
df4=merge(df4,mom,by="position",all.x = TRUE)
df4[is.na(df4)]=0
df4
df4=select(df4,-"batsman")
df4
#Venue wise
venue_wise=function(b){
  bat=bat2[bat2$batsman==b ,]
  ven=bat%>%
    group_by(venue,date)%>%
    summarise(matches= sum(table(unique(id))),runs=sum(batsman_runs),sr=runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
    as.data.frame()
  
  f<- bat %>% group_by(venue,date) %>% 
    summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
    as.data.frame()
  #6's
  s<- bat %>% group_by(venue,date) %>% 
    summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
    as.data.frame()
  list_df = list(ven,f,s)
  ven <- list_df %>% reduce(left_join, by=c('venue','date'))
  ven
  mom=bat%>%
    group_by(venue,date,player_of_match)%>%
    filter(player_of_match==b)%>%
    summarise(mom=sum(table(unique(id))))%>%
    as.data.frame()
  
  ven=merge(ven,mom,by=c("venue","date"),all.x = TRUE)
  ven[is.na(ven)]=0
  ven=select(ven,-"player_of_match")
  ven=ven%>%
    arrange(desc(runs))%>%
    head(10)
}
print(venue_wise("D Padikkal"))

ven=bat%>%
  group_by(venue,date)%>%
  summarise(matches= sum(table(unique(id))),runs=sum(batsman_runs),sr=runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
  as.data.frame()

f<- bat %>% group_by(venue,date) %>% 
  summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
  as.data.frame()
#6's
s<- bat %>% group_by(venue,date) %>% 
  summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
  as.data.frame()
list_df = list(ven,f,s)
ven <- list_df %>% reduce(left_join, by=c('venue','date'))
ven
mom=bat%>%
  group_by(venue,date,player_of_match)%>%
  filter(player_of_match=="SK Raina")%>%
  summarise(mom=sum(table(unique(id))))%>%
  as.data.frame()

ven=merge(ven,mom,by=c("venue","date"),all.x = TRUE)
ven[is.na(ven)]=0
ven=select(ven,-"player_of_match")
ven=ven%>%
  arrange(desc(runs))%>%
  head(10)
#Innings wise
inning_wise=function(b){
  bat=bat2[bat2$batsman==b ,]
  df3<- bat %>% group_by(inning,batsman) %>% 
    summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
    as.data.frame()
  
  noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(inning,player_dismissed)%>%summarise(outs=n())
  colnames(noa)[2]="batsman"
  df3=merge(df3,noa,by=c("batsman","inning"))
  df3["average"]=df3$sum_runs/df3$outs
  df3['not_outs']=df3$matches-df3$outs
  f<- bat %>% group_by(batsman,inning) %>% 
    summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
    as.data.frame()
  #6's
  s<- bat %>% group_by(batsman,inning) %>% 
    summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
    as.data.frame()
  list_df = list(df3,f,s)
  df3 <- list_df %>% reduce(right_join, by=c('batsman','inning'))
  df3
  ducks= bat %>% group_by(date,inning) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs==0)
  hundred<- bat %>% group_by(date,inning,batsman) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs>=100)
  fifty<- bat %>% group_by(date,inning,batsman) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs>=50&runs<100)
  fifty<-fifty%>%
    group_by(batsman,inning)%>%
    summarise(fifty=n())
  hundred<-hundred%>%
    group_by(batsman,inning)%>%
    summarise(hundred=n())
  list_df = list(df3,fifty,hundred)
  df3 <- list_df %>% reduce(left_join, by=c('batsman','inning'))
  
  hs=bat%>%
    group_by(date,batsman,inning)%>%
    summarise(runs=sum(batsman_runs))
  hs<-hs%>%
    group_by(inning,batsman)%>%
    summarise(hs=max(runs))
  list_df = list(df3,hs)
  df3 <- list_df %>% reduce(left_join, by=c('batsman','inning'))
  
  ducks<- bat %>% group_by(date,inning,batsman) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs==0)
  ducks<-ducks%>%
    group_by(batsman,inning)%>%
    summarise(ducks=n())
  list_df = list(df3,ducks)
  df3 <- list_df %>% reduce(left_join, by=c('batsman','inning'))
  df3["boundary_percent"]=((df3$fours+df3$sixes)/df3$balls_faced)*100
  mom=bat%>%
    group_by(inning,player_of_match)%>%
    filter(player_of_match==b)%>%
    summarise(mom=sum(table(unique(id))))%>%
    as.data.frame()
  list_df = list(df3,mom)
  df3 <- list_df %>% reduce(left_join, by=c('inning'))
  df3=select(df3,-c("batsman","player_of_match"))
  return(df3)
}
inning_wise("SK Raina")

df3<- bat %>% group_by(inning,batsman) %>% 
  summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
  as.data.frame()

noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(inning,player_dismissed)%>%summarise(outs=n())
colnames(noa)[2]="batsman"
df3=merge(df3,noa,by=c("batsman","inning"))
df3["average"]=df3$sum_runs/df3$outs
df3['not_outs']=df3$matches-df3$outs
f<- bat %>% group_by(batsman,inning) %>% 
  summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
  as.data.frame()
#6's
s<- bat %>% group_by(batsman,inning) %>% 
  summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
  as.data.frame()
list_df = list(df3,f,s)
df3 <- list_df %>% reduce(right_join, by=c('batsman','inning'))
df3
ducks= bat %>% group_by(date,inning) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs==0)
hundred<- bat %>% group_by(date,inning,batsman) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs>=100)
fifty<- bat %>% group_by(date,inning,batsman) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs>=50&runs<100)
fifty<-fifty%>%
  group_by(batsman,inning)%>%
  summarise(fifty=n())
hundred<-hundred%>%
  group_by(batsman,inning)%>%
  summarise(hundred=n())
list_df = list(df3,fifty,hundred)
df3 <- list_df %>% reduce(left_join, by=c('batsman','inning'))

hs=bat%>%
  group_by(date,batsman,inning)%>%
  summarise(runs=sum(batsman_runs))
hs<-hs%>%
  group_by(inning,batsman)%>%
  summarise(hs=max(runs))
list_df = list(df3,hs)
df3 <- list_df %>% reduce(left_join, by=c('batsman','inning'))

ducks<- bat %>% group_by(date,inning,batsman) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs==0)
ducks<-ducks%>%
  group_by(batsman,inning)%>%
  summarise(ducks=n())
list_df = list(df3,ducks)
df3 <- list_df %>% reduce(left_join, by=c('batsman','inning'))
df3["boundary_percent"]=((df3$fours+df3$sixes)/df3$balls_faced)*100
mom=bat%>%
  group_by(inning,player_of_match)%>%
  filter(player_of_match=="SK Raina")%>%
  summarise(mom=sum(table(unique(id))))%>%
  as.data.frame()
list_df = list(df3,mom)
df3 <- list_df %>% reduce(left_join, by=c('inning'))
df3=select(df3,-c("batsman","player_of_match"))

#Season wise
season_wise=function(b){
  bat=bat2[bat2$batsman==b ,]
  df3<- bat %>% group_by(season,batsman) %>% 
    summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
    as.data.frame()
  
  noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(season,player_dismissed)%>%summarise(outs=n())
  colnames(noa)[2]="batsman"
  df3=merge(df3,noa,by=c("batsman","season"))
  df3["average"]=df3$sum_runs/df3$outs
  df3['not_outs']=df3$matches-df3$outs
  f<- bat %>% group_by(batsman,season) %>% 
    summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
    as.data.frame()
  #6's
  s<- bat %>% group_by(batsman,season) %>% 
    summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
    as.data.frame()
  list_df = list(df3,f,s)
  df3 <- list_df %>% reduce(right_join, by=c('batsman','season'))
  df3
  ducks= bat %>% group_by(date,season) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs==0)
  hundred<- bat %>% group_by(date,season,batsman) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs>=100)
  fifty<- bat %>% group_by(date,season,batsman) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs>=50&runs<100)
  fifty<-fifty%>%
    group_by(batsman,season)%>%
    summarise(fifty=n())
  hundred<-hundred%>%
    group_by(batsman,season)%>%
    summarise(hundred=n())
  list_df = list(df3,fifty,hundred)
  df3 <- list_df %>% reduce(left_join, by=c('batsman','season'))
  
  hs=bat%>%
    group_by(date,batsman,season)%>%
    summarise(runs=sum(batsman_runs))
  hs<-hs%>%
    group_by(season,batsman)%>%
    summarise(hs=max(runs))
  list_df = list(df3,hs)
  df3 <- list_df %>% reduce(left_join, by=c('batsman','season'))
  
  ducks<- bat %>% group_by(date,season,batsman) %>% 
    summarise(runs=sum(batsman_runs))%>%
    filter(runs==0)
  ducks<-ducks%>%
    group_by(batsman,season)%>%
    summarise(ducks=n())
  list_df = list(df3,ducks)
  df3 <- list_df %>% reduce(left_join, by=c('batsman','season'))
  df3["boundary_percent"]=((df3$fours+df3$sixes)/df3$balls_faced)*100
  mom=bat%>%
    group_by(season,player_of_match)%>%
    filter(player_of_match==b)%>%
    summarise(mom=sum(table(unique(id))))%>%
    as.data.frame()
  list_df = list(df3,mom)
  df3 <- list_df %>% reduce(left_join, by=c('season'))
  df3=select(df3,-c("batsman","player_of_match"))
  df3[is.na(df3)]=0
  return(df3)
}
season_wise("RG Sharma")

df3<- bat %>% group_by(season,batsman) %>% 
  summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
  as.data.frame()

noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(season,player_dismissed)%>%summarise(outs=n())
colnames(noa)[2]="batsman"
df3=merge(df3,noa,by=c("batsman","season"))
df3["average"]=df3$sum_runs/df3$outs
df3['not_outs']=df3$matches-df3$outs
f<- bat %>% group_by(batsman,season) %>% 
  summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
  as.data.frame()
#6's
s<- bat %>% group_by(batsman,season) %>% 
  summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
  as.data.frame()
list_df = list(df3,f,s)
df3 <- list_df %>% reduce(right_join, by=c('batsman','season'))
df3
ducks= bat %>% group_by(date,season) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs==0)
hundred<- bat %>% group_by(date,season,batsman) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs>=100)
fifty<- bat %>% group_by(date,season,batsman) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs>=50&runs<100)
fifty<-fifty%>%
  group_by(batsman,season)%>%
  summarise(fifty=n())
hundred<-hundred%>%
  group_by(batsman,season)%>%
  summarise(hundred=n())
list_df = list(df3,fifty,hundred)
df3 <- list_df %>% reduce(left_join, by=c('batsman','season'))

hs=bat%>%
  group_by(date,batsman,season)%>%
  summarise(runs=sum(batsman_runs))
hs<-hs%>%
  group_by(season,batsman)%>%
  summarise(hs=max(runs))
list_df = list(df3,hs)
df3 <- list_df %>% reduce(left_join, by=c('batsman','season'))

ducks<- bat %>% group_by(date,season,batsman) %>% 
  summarise(runs=sum(batsman_runs))%>%
  filter(runs==0)
ducks<-ducks%>%
  group_by(batsman,season)%>%
  summarise(ducks=n())
list_df = list(df3,ducks)
df3 <- list_df %>% reduce(left_join, by=c('batsman','season'))
df3["boundary_percent"]=((df3$fours+df3$sixes)/df3$balls_faced)*100
mom=bat%>%
  group_by(season,player_of_match)%>%
  filter(player_of_match=="SK Raina")%>%
  summarise(mom=sum(table(unique(id))))%>%
  as.data.frame()
list_df = list(df3,mom)
df3 <- list_df %>% reduce(left_join, by=c('season'))
df3=select(df3,-c("batsman","player_of_match"))
df3[is.na(df3)]=0

#High risk bowlwers
high_risk=function(b){
  bat=bat2[bat2$batsman==b ,]
  high<-bat%>%
    group_by(bowler)%>%
    summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
    as.data.frame()
  
  noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(bowler,player_dismissed)%>%summarise(outs=n())
  noa=noa%>%
    filter(player_dismissed==b)
  high=merge(high,noa,by=c("bowler"))
  high["average"]=high$sum_runs/high$outs
  high['not_outs']=high$matches-high$outs
  f<- bat %>% group_by(bowler) %>% 
    summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
    as.data.frame()
  #6's
  s<- bat %>% group_by(bowler) %>% 
    summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
    as.data.frame()
  list_df = list(high,f,s)
  high<- list_df %>% reduce(right_join, by=c('bowler'))
  
  high["boundary_percent"]=((high$fours+high$sixes)/high$balls_faced)*100
  
  high=select(high,-c("player_dismissed"))
  
  high[is.na(high)]=0
  high=high%>%
    arrange(desc(outs))%>%
    head(10)
  
}
print(high_risk("SK Raina"))

high<-bat%>%
  group_by(bowler)%>%
  summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
  as.data.frame()

noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(bowler,player_dismissed)%>%summarise(outs=n())
noa=noa%>%
  filter(player_dismissed=="SK Raina")
high=merge(high,noa,by=c("bowler"))
high["average"]=high$sum_runs/high$outs
high['not_outs']=high$matches-high$outs
f<- bat %>% group_by(bowler) %>% 
  summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
  as.data.frame()
#6's
s<- bat %>% group_by(bowler) %>% 
  summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
  as.data.frame()
list_df = list(high,f,s)
high<- list_df %>% reduce(right_join, by=c('bowler'))

high["boundary_percent"]=((high$fours+high$sixes)/high$balls_faced)*100

high=select(high,-c("player_dismissed"))

high[is.na(high)]=0
high=high%>%
  arrange(desc(outs))%>%
  head(10)
#LOW risk bowlwers
low_risk=function(b){
  bat=bat2[bat2$batsman==b ,]
  low<-bat%>%
    group_by(bowler)%>%
    summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
    as.data.frame()
  
  noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(bowler,player_dismissed)%>%summarise(outs=n())
  noa=noa%>%
    filter(player_dismissed=="SK Raina")
  low=merge(low,noa,by=c("bowler"))
  low["average"]=low$sum_runs/low$outs
  low['not_outs']=low$matches-low$outs
  f<- bat %>% group_by(bowler) %>% 
    summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
    as.data.frame()
  #6's
  s<- bat %>% group_by(bowler) %>% 
    summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
    as.data.frame()
  list_df = list(low,f,s)
  low<- list_df %>% reduce(right_join, by=c('bowler'))
  
  low["boundary_percent"]=((low$fours+low$sixes)/low$balls_faced)*100
  
  low=select(low,-c("player_dismissed"))
  
  low[is.na(low)]=0
  low=low%>%
    arrange(desc(average))%>%
    head(10)
}
print(low_risk("SK Raina"))

low<-bat%>%
  group_by(bowler)%>%
  summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
  as.data.frame()

noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(bowler,player_dismissed)%>%summarise(outs=n())
noa=noa%>%
  filter(player_dismissed=="SK Raina")
low=merge(low,noa,by=c("bowler"))
low["average"]=low$sum_runs/low$outs
low['not_outs']=low$matches-low$outs
f<- bat %>% group_by(bowler) %>% 
  summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
  as.data.frame()
#6's
s<- bat %>% group_by(bowler) %>% 
  summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
  as.data.frame()
list_df = list(low,f,s)
low<- list_df %>% reduce(right_join, by=c('bowler'))

low["boundary_percent"]=((low$fours+low$sixes)/low$balls_faced)*100

low=select(low,-c("player_dismissed"))

low[is.na(low)]=0
low=low%>%
  arrange(desc(average))%>%
  head(10)
#############################################################################

bowler=ipl[ipl$bowler=="Kartik Tyagi",]
sr=bowler %>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))%>%
  group_by(bowler)%>%
  summarise(runs=sum(total_runs))
balls=bowler %>%
  filter(extras_type!="wides"|is.na(extras_type))%>%
  filter(extras_type!="noballs"|is.na(extras_type))%>%
  group_by(bowler)%>%
  summarise(balls=n())
df6<- bowler %>%
  filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))
df6=df6 %>%
  group_by(bowler) %>% 
  summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
  as.data.frame()
df6=merge(df6,sr,by="bowler")
df6=merge(df6,balls,by="bowler")
df6['sr']=df6$balls/df6$sum_wickets
df6['avg']=df6$runs/df6$sum_wickets
df6['eco']=df6$runs/(df6$balls/6)
hs=bowler%>%
  filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))
hs=hs%>%
  group_by(date)%>%
  summarise(wkts=sum(is_wicket),runs=sum(total_runs))%>%
  arrange(desc(wkts))%>%
  head(1)
df6['bbf']=paste0(hs$wkts,"/",hs$runs)
overall_perf_bowler=function(df6){
  print(paste0("Innings               - ",df6$matches))
  print(paste0("Total Wickets         - ",df6$sum_wickets))
  print(paste0("Runs conceeded        - ",df6$runs))
  print(paste0("Balls Bowled          - ",df6$balls))
  print(paste0("Strike Rate           - ",df6$sr))
  print(paste0("Bowling Average       - ",df6$avg))
  print(paste0("Economy               - ",df6$eco))
  print(paste0("Best Bowling figures  - ",df6$bbf))
}
overall_perf_bowler(df6)

bowling_perf=function(b){
  bowler=ipl[ipl$bowler==b,]
  sr=bowler %>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))%>%
    group_by(bowler)%>%
    summarise(runs=sum(total_runs))
  balls=bowler %>%
    filter(extras_type!="wides"|is.na(extras_type))%>%
    filter(extras_type!="noballs"|is.na(extras_type))%>%
    group_by(bowler)%>%
    summarise(balls=n())
  df6<- bowler %>%
    filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))
  df6=df6 %>%
    group_by(bowler) %>% 
    summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
    as.data.frame()
  df6=merge(df6,sr,by="bowler")
  df6=merge(df6,balls,by="bowler")
  df6['sr']=df6$balls/df6$sum_wickets
  df6['avg']=df6$runs/df6$sum_wickets
  df6['eco']=df6$runs/(df6$balls/6)
  hs=bowler%>%
    filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))
  hs=hs%>%
    group_by(date)%>%
    summarise(wkts=sum(is_wicket),runs=sum(total_runs))%>%
    arrange(desc(wkts))%>%
    head(1)
  df6['bbf']=paste0(hs$wkts,"/",hs$runs)
  overall_perf_bowler(df6)
}

#Season
sr=bowler %>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))%>%
  group_by(season,bowler)%>%
  summarise(runs=sum(total_runs))
balls=bowler %>%
  filter(extras_type!="wides"|is.na(extras_type))%>%
  filter(extras_type!="noballs"|is.na(extras_type))%>%
  group_by(season,bowler)%>%
  summarise(balls=n())
df6<- bowler %>%
  filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))
df6=df6 %>%
  group_by(season,bowler) %>% 
  summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
  as.data.frame()
df6=merge(df6,sr,by=c("season","bowler"))
df6=merge(df6,balls,by=c("season","bowler"))
df6['sr']=df6$balls/df6$sum_wickets
df6['avg']=df6$runs/df6$sum_wickets
df6['eco']=df6$runs/(df6$balls/6)
hs=bowler%>%
  filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))
hs=hs%>%
  group_by(season,date)%>%
  summarise(wkts=sum(is_wicket),runs=sum(total_runs))%>%
  arrange(desc(wkts))
hs=hs%>%
  group_by(season)%>%
  filter(wkts==max(wkts))
hs=hs%>%
  group_by(season)%>%
  filter(runs==min(runs))%>%
  arrange(season)
df6['bbf']=paste0(hs$wkts,"/",hs$runs)

bowler_season=function(b){
  bowler=ipl[ipl$bowler==b,]
  sr=bowler %>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))%>%
    group_by(season,bowler)%>%
    summarise(runs=sum(total_runs))
  balls=bowler %>%
    filter(extras_type!="wides"|is.na(extras_type))%>%
    filter(extras_type!="noballs"|is.na(extras_type))%>%
    group_by(season,bowler)%>%
    summarise(balls=n())
  df6<- bowler %>%
    filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))
  df6=df6 %>%
    group_by(season,bowler) %>% 
    summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
    as.data.frame()
  df6=merge(df6,sr,by=c("season","bowler"))
  df6=merge(df6,balls,by=c("season","bowler"))
  df6['sr']=df6$balls/df6$sum_wickets
  df6['avg']=df6$runs/df6$sum_wickets
  df6['eco']=df6$runs/(df6$balls/6)
  hs=bowler%>%
    filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))
  hs=hs%>%
    group_by(season,date)%>%
    summarise(wkts=sum(is_wicket),runs=sum(total_runs))%>%
    arrange(desc(wkts))
  hs=hs%>%
    group_by(season)%>%
    filter(wkts==max(wkts))
  hs=hs%>%
    group_by(season)%>%
    filter(runs==min(runs))%>%
    arrange(season)
  df6['bbf']=paste0(hs$wkts,"/",hs$runs)
  return(df6)
  
}
print(bowler_season("Kartik Tyagi"))
#Innings
sr=bowler %>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))%>%
  group_by(inning,bowler)%>%
  summarise(runs=sum(total_runs))
balls=bowler %>%
  filter(extras_type!="wides"|is.na(extras_type))%>%
  filter(extras_type!="noballs"|is.na(extras_type))%>%
  group_by(inning,bowler)%>%
  summarise(balls=n())
df6<- bowler %>%
  filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))
df6=df6 %>%
  group_by(inning,bowler) %>% 
  summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
  as.data.frame()
df6=merge(df6,sr,by=c("inning","bowler"))
df6=merge(df6,balls,by=c("inning","bowler"))
df6['sr']=df6$balls/df6$sum_wickets
df6['avg']=df6$runs/df6$sum_wickets
df6['eco']=df6$runs/(df6$balls/6)
hs=bowler%>%
  filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))
hs=hs%>%
  group_by(inning,date)%>%
  summarise(wkts=sum(is_wicket),runs=sum(total_runs))%>%
  arrange(desc(wkts))
hs=hs%>%
  group_by(inning)%>%
  filter(wkts==max(wkts))
hs=hs%>%
  group_by(inning)%>%
  filter(runs==min(runs))%>%
  arrange(inning)
df6['bbf']=paste0(hs$wkts,"/",hs$runs)

bowler_inning=function(b){
  bowler=ipl[ipl$bowler==b,]
  sr=bowler %>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))%>%
    group_by(inning,bowler)%>%
    summarise(runs=sum(total_runs))
  balls=bowler %>%
    filter(extras_type!="wides"|is.na(extras_type))%>%
    filter(extras_type!="noballs"|is.na(extras_type))%>%
    group_by(inning,bowler)%>%
    summarise(balls=n())
  df6<- bowler %>%
    filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))
  df6=df6 %>%
    group_by(inning,bowler) %>% 
    summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
    as.data.frame()
  df6=merge(df6,sr,by=c("inning","bowler"))
  df6=merge(df6,balls,by=c("inning","bowler"))
  df6['sr']=df6$balls/df6$sum_wickets
  df6['avg']=df6$runs/df6$sum_wickets
  df6['eco']=df6$runs/(df6$balls/6)
  hs=bowler%>%
    filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))
  hs=hs%>%
    group_by(inning,date)%>%
    summarise(wkts=sum(is_wicket),runs=sum(total_runs))%>%
    arrange(desc(wkts))
  hs=hs%>%
    group_by(inning)%>%
    filter(wkts==max(wkts))
  hs=hs%>%
    group_by(inning)%>%
    filter(runs==min(runs))%>%
    arrange(inning)
  df6['bbf']=paste0(hs$wkts,"/",hs$runs)
}
#############
#Danger Batter(atleast 10 balls and 3 matches)
sr=bowler %>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))%>%
  group_by(bowler,batsman)%>%
  summarise(runs=sum(total_runs))
balls=bowler %>%
  filter(extras_type!="wides"|is.na(extras_type))%>%
  filter(extras_type!="noballs"|is.na(extras_type))%>%
  group_by(bowler,batsman)%>%
  summarise(balls=n())
df6<- bowler %>%
  filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))
df6=df6 %>%
  group_by(bowler,batsman) %>% 
  summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
  as.data.frame()
df6=merge(df6,sr,by=c("bowler","batsman"))
df6=merge(df6,balls,by=c("bowler","batsman"))
df6['sr']=df6$balls/df6$sum_wickets
df6['avg']=df6$runs/df6$sum_wickets
df6['eco']=df6$runs/(df6$balls/6)
df6=df6%>%
  filter(matches>=3&balls>10)%>%
  arrange(desc(eco))%>%
  head(10)
danger_batter=function(b){
  bowler=ipl[ipl$bowler==b,]
  sr=bowler %>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))%>%
    group_by(bowler,batsman)%>%
    summarise(runs=sum(total_runs))
  balls=bowler %>%
    filter(extras_type!="wides"|is.na(extras_type))%>%
    filter(extras_type!="noballs"|is.na(extras_type))%>%
    group_by(bowler,batsman)%>%
    summarise(balls=n())
  df6<- bowler %>%
    filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))
  df6=df6 %>%
    group_by(bowler,batsman) %>% 
    summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
    as.data.frame()
  df6=merge(df6,sr,by=c("bowler","batsman"))
  df6=merge(df6,balls,by=c("bowler","batsman"))
  df6['sr']=df6$balls/df6$sum_wickets
  df6['avg']=df6$runs/df6$sum_wickets
  df6['eco']=df6$runs/(df6$balls/6)
  df6=df6%>%
    filter(matches>=3&balls>10)%>%
    arrange(desc(eco))%>%
    head(10)
}
#############
#Easy Batter(atleast 10 balls and 3 matches)
sr=bowler %>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))%>%
  group_by(bowler,batsman)%>%
  summarise(runs=sum(total_runs))
balls=bowler %>%
  filter(extras_type!="wides"|is.na(extras_type))%>%
  filter(extras_type!="noballs"|is.na(extras_type))%>%
  group_by(bowler,batsman)%>%
  summarise(balls=n())
df6<- bowler %>%
  filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))
df6=df6 %>%
  group_by(bowler,batsman) %>% 
  summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
  as.data.frame()
df6=merge(df6,sr,by=c("bowler","batsman"))
df6=merge(df6,balls,by=c("bowler","batsman"))
df6['sr']=df6$balls/df6$sum_wickets
df6['avg']=df6$runs/df6$sum_wickets
df6['eco']=df6$runs/(df6$balls/6)
df6=df6%>%
  filter(matches>=3&balls>10)%>%
  arrange((eco))%>%
  head(10)
easy_batter=function(b){
  bowler=ipl[ipl$bowler==b,]
  sr=bowler %>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))%>%
    group_by(bowler,batsman)%>%
    summarise(runs=sum(total_runs))
  balls=bowler %>%
    filter(extras_type!="wides"|is.na(extras_type))%>%
    filter(extras_type!="noballs"|is.na(extras_type))%>%
    group_by(bowler,batsman)%>%
    summarise(balls=n())
  df6<- bowler %>%
    filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))
  df6=df6 %>%
    group_by(bowler,batsman) %>% 
    summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
    as.data.frame()
  df6=merge(df6,sr,by=c("bowler","batsman"))
  df6=merge(df6,balls,by=c("bowler","batsman"))
  df6['sr']=df6$balls/df6$sum_wickets
  df6['avg'] =df6$runs/df6$sum_wickets
  df6['eco']=df6$runs/(df6$balls/6)
  df6=df6%>%
    filter(matches>=3&balls>10)%>%
    arrange((eco))%>%
    head(10)
}
###################
sr=bowler %>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))%>%
  group_by(bowler,date,venue)%>%
  summarise(runs=sum(total_runs))
balls=bowler %>%
  filter(extras_type!="wides"|is.na(extras_type))%>%
  filter(extras_type!="noballs"|is.na(extras_type))%>%
  group_by(bowler,date,venue)%>%
  summarise(balls=n())
df6<- bowler %>%
  filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
  filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
  filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
  filter(extras_type!="byes"|is.na(extras_type))%>%
  filter(extras_type!="legbyes"|is.na(extras_type))
df6=df6 %>%
  group_by(bowler,date,venue) %>% 
  summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
  as.data.frame()
df6=merge(df6,sr,by=c("bowler","date","venue"))
df6=merge(df6,balls,by=c("bowler","date","venue"))
df6['sr']=df6$balls/df6$sum_wickets
df6['avg']=df6$runs/df6$sum_wickets
df6['eco']=df6$runs/(df6$balls/6)
df6=df6%>%
  arrange(desc(sum_wickets))%>%
  head(10)
best_venue=function(b){
  bowler=ipl[ipl$bowler==b,]
  sr=bowler %>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))%>%
    group_by(bowler,date,venue)%>%
    summarise(runs=sum(total_runs))
  balls=bowler %>%
    filter(extras_type!="wides"|is.na(extras_type))%>%
    filter(extras_type!="noballs"|is.na(extras_type))%>%
    group_by(bowler,date,venue)%>%
    summarise(balls=n())
  df6<- bowler %>%
    filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
    filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
    filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
    filter(extras_type!="byes"|is.na(extras_type))%>%
    filter(extras_type!="legbyes"|is.na(extras_type))
  df6=df6 %>%
    group_by(bowler,date,venue) %>% 
    summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
    as.data.frame()
  df6=merge(df6,sr,by=c("bowler","date","venue"))
  df6=merge(df6,balls,by=c("bowler","date","venue"))
  df6['sr']=df6$balls/df6$sum_wickets
  df6['avg']=df6$runs/df6$sum_wickets
  df6['eco']=df6$runs/(df6$balls/6)
  df6=df6%>%
    arrange(desc(sum_wickets))%>%
    head(10)
  
}
print(best_venue("Harbhajan Singh"))
#####################
bowler_perf=function(b){
  print(bowling_perf(b))
  print(bowler_inning(b))
  print(bowler_season(b))
  print(danger_batter(b))
  print(easy_batter(b))
  print(best_venue(b))
}
bowler_perf("Harbhajan Singh")
