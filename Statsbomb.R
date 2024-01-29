### Free Statsbomb Data - StatsBombR library

install.packages("devtools")
install.packages("remotes")
remotes::install_version("SDMTools", "1.1-221")
library(remotes)
library(devtools)

devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)

library(tidyverse)

# Shows Available Competitions 
CompList <- FreeCompetitions()

# Shows Available Matches in those Competitions
MatchList <- FreeMatches(CompList)

# Pick by Competitiion and Season
D1 <- CompList %>%
  filter(competition_id==16 & season_name=="2018/2019")

#16- UCL
#43- FWC

# Pick by Match
D2 <- MatchList %>%
  filter(match_id==22912)

#22912- 2019 UCL Final
#3869685- 2022 FWC Final

# Pull Matches (not necessary if pick by match first)
Matches <- FreeMatches(D1)

# Create Data Frame
StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)
StatsBombData2 <- free_allevents(MatchesDF = D2, Parallel = T)

# Extract Relevant Info
StatsBombData = allclean(StatsBombData)
StatsBombData2 = allclean(StatsBombData2)

devtools::install_github("FCrSTATS/SBpitch")
library(SBpitch)

# Seeing what teams are in data
unique(StatsBombData[,'team.name'])

# Seeing how many matches are available for free in data 
unique(StatsBombData[,'match_id'])

# Plotting Passes into Box
passes = StatsBombData %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name) &
           player.id==5487) %>% #filter to only completed passes
  filter(pass.end_location.x>=102 & pass.end_location.y<=62 &
           pass.end_location.y>=18) #filter to only passes into box

create_Pitch() +
  geom_segment(data = passes, aes(x = location.x, y = location.y,
               xend = pass.end_location.x, yend = pass.end_location.y), 
               lineend = "round", linewidth = 0.5, colour = "#000000", arrow =
arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + #3 
  labs(title = "Antoine Griezmann, Completed Passes to the Box", subtitle = "FIFA World Cup,
  2022") + #4
  scale_y_reverse() + #5
  coord_fixed(ratio = 105/100) #6

# Plotting Carries
carries = StatsBombData %>%
    filter(type.name=="Carry" & player.id==5503) 
  
create_Pitch() +
    geom_segment(data = carries, aes(x = location.x, y = location.y,
                  xend = carry.end_location.x, yend = carry.end_location.y), 
                 lineend = "round", linewidth = 0.5, colour = "#000000", arrow =
arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + #3 
  labs(title = "Lionel Messi, Carries", subtitle = "FIFA World Cup,
  2022") + #4
  scale_y_reverse() + #5 
  coord_fixed(ratio = 105/100) #6
  
# Plotting Passes 
library(SBpitch)
passes = StatsBombData %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name) &
             player.id==36430) #filter to only completed passes
  
create_Pitch() +
  geom_segment(data = passes, aes(x = location.x, y = location.y,
                xend = pass.end_location.x, yend = pass.end_location.y), 
                lineend = "round", size = 0.5, colour = "orange", arrow =
arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + #3 
labs(title = "Sergio Busquets, Completed Passes", subtitle = "MLS,
2023") #4
scale_y_reverse() + #5 
coord_fixed(ratio = 105/100) #6

# Defensive Heat Maps
#
library(tidyverse)

heatmap = StatsBombData %>%mutate(location.x = ifelse(location.x>120, 120, location.x), 
location.y = ifelse(location.y>80, 80, location.y),
location.x = ifelse(location.x<0, 0, location.x),
location.y = ifelse(location.y<0, 0, location.y)) #1

heatmap$xbin <- cut(heatmap$location.x, breaks = seq(from=0, to=120, by = 20),include.lowest=TRUE ) 
heatmap$ybin <- cut(heatmap$location.y, breaks = seq(from=0, to=80, by = 20),include.lowest=TRUE) #2

# 
heatmap = heatmap%>%
  filter(type.name=="Pressure" | duel.type.name=="Tackle" | 
  type.name=="Foul Committed" | type.name=="Interception" | 
  type.name=="Block" ) %>%
  group_by(team.name) %>%
  mutate(total_DA = n()) %>%
  group_by(team.name, xbin, ybin) %>%
  summarise(total_DA = max(total_DA),
            bin_DA = n(),
            bin_pct = bin_DA/total_DA,
            location.x = median(location.x),
            location.y = median(location.y)) %>%
group_by(xbin, ybin) %>%
mutate(league_ave = mean(bin_pct)) %>% 
group_by(team.name, xbin, ybin) %>%
mutate(diff_vs_ave = bin_pct - league_ave) #3

#
library(grid)
defensiveactivitycolors <- c("#dc2429", "#dc2329", "#df272d", "#df3238", "#e14348", "#e44d51", "#e35256", 
                             "#e76266", "#e9777b", "#ec8589", "#ec898d", "#ef9195",
                             "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2",
                             "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", "#c0c7cd",
                             "#b9c0c8", "#b5bcc3", "#909ba5", "#8f9aa5", "#818c98", "#798590", "#697785", 
                             "#526173", "#435367", "#3a4b60", "#2e4257", "#1d3048", "#11263e", "#11273e", "#0d233a", "#020c16") #1

# 
ggplot(data= heatmap, aes(x = location.x, y = location.y, fill = diff_vs_ave, group =diff_vs_ave)) + 
  geom_bin2d(binwidth = c(20, 20), position = "identity", alpha = 0.9) + #2
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) + 
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) + 
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) + 
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) + 
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) + 
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", size = 0.6)+ 
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+ 
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(), 
        line = element_blank()) +
  annotate("point", x = 12 , y = 40, colour = "white", size = 1.05) + 
  # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "white", size = 1.05) + 
  annotate("path", colour = "white", size = 0.6,
    x=60+10*cos(seq(0,2*pi,length.out=2000)),
    y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  # add center spot
  annotate("point", x = 60 , y = 40, colour = "white", size = 1.05) + 
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") + 
  annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
  theme(axis.text.x=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.caption=element_text(size=13,family="Calibri", hjust=0.5, vjust=0.5), 
      plot.subtitle = element_text(size = 18, family="Calibri", hjust = 0.5), 
      axis.text.y=element_blank(),
      legend.title = element_blank(), 
      legend.text=element_text(size=22,family="Calibri"),
      legend.key.size = unit(1.5, "cm"),
      plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5,family="Calibri", colour = "black", hjust = 0.5), 
      legend.direction = "vertical",
      axis.ticks=element_blank(),
      plot.background = element_rect(fill = "white"),
      strip.text.x = element_text(size=13,family="Calibri")) + #4
  scale_y_reverse() + #5
  scale_fill_gradientn(colours = defensiveactivitycolors, trans = "reverse", labels = scales::percent_format(accuracy = 1), limits = c(0.03, -0.03)) + #6
  labs(title = "Where Do Teams Defend vs Cup Average?", subtitle = "FA Women's Super League, 2020/21") + #7
  coord_fixed(ratio = 95/100) + #8
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last",
                                               length=unit(2.55,"mm")), gp=gpar(col="black", fill=NA, lwd=2.2)),
                  xmin=25, xmax = 95, ymin = -83, ymax = -83) + #9 
  facet_wrap(~team.name)+ #10
  guides(fill = guide_legend(reverse = TRUE)) #11
  
warnings()
  
ggplot(data= heatmap, aes(x = location.x, y = location.y, fill = diff_vs_ave, group =diff_vs_ave)) + 
  geom_bin2d(binwidth = c(20, 20), position = "identity", alpha = 0.9) + #2
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", linewidth = 0.3) + 
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", linewidth = 0.3) + 
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", linewidth = 0.3) + 
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", linewidth = 0.3) + 
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", linewidth = 0.3) + 
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", linewidth = 0.3) + 
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", linewidth = 0.3) + 
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", linewidth = 0.3) + 
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", linewidth = 0.3)+ 
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", linewidth = 0.3)+ 
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", linewidth = 0.3)+
  theme(rect = element_blank(), 
        line = element_blank()) +
  annotate("point", x = 12 , y = 40, colour = "white", size = .5) + 
  # add penalty spot
  annotate("point", x = 108 , y = 40, colour = "white", size = .5) + 
  annotate("path", colour = "white", linewidth = 0.3,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  # add center spot
  annotate("point", x = 60 , y = 40, colour = "white", size = .5) + 
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.3,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") + 
  annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.3,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size = 13,family="Calibri", hjust=0.5, vjust=0.5), 
        plot.subtitle = element_text(size = 18, family="Calibri", hjust = 0.5), 
        axis.text.y=element_blank(),
        legend.title = element_blank(), 
        legend.text=element_text(size = 22,family="Calibri"),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5,family="Calibri", colour = "black", hjust = 0.5), 
        legend.direction = "vertical",
        axis.ticks=element_blank(),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 13,family="Calibri")) + #4
  scale_y_reverse() + #5
  scale_fill_gradientn(colours = defensiveactivitycolors, trans = "reverse", labels = scales::percent_format(accuracy = 1), limits = c(0.03, -0.03)) + #6
  labs(title = "Defensive Actions vs Average", subtitle = "2022 FIFA World Cup") + #7
  coord_fixed(ratio = 95/100) + #8
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last",
                                                 length=unit(1,"mm")), gp=gpar(col="black", fill=NA, lwd=.8)),
                    xmin=25, xmax = 95, ymin = -83, ymax = -83) + #9 
  facet_wrap(~team.name)+ #10
  guides(fill = guide_legend(reverse = TRUE)) #11

# Creating xG+xGA Rankings
xGA = StatsBombData %>%
  filter(type.name=="Shot") %>% #1 
  select(shot.key_pass_id, xGA = shot.statsbomb_xg) #2

  shot_assists = left_join(StatsBombData, xGA, by = c("id" = "shot.key_pass_id")) %>% #3
  select(team.name, player.name, player.id, type.name, pass.shot_assist, 
          pass.goal_assist, xGA ) %>% #4
  filter(pass.shot_assist==TRUE | pass.goal_assist==TRUE) #5
  
player_xGA = shot_assists %>% 
  group_by(player.name, player.id, team.name) %>% 
  summarise(xGA = sum(xGA, na.rm = TRUE)) #1
  
player_xG = StatsBombData %>%
  filter(type.name=="Shot") %>% 
  filter(shot.type.name!="Penalty" | is.na(shot.type.name)) %>% 
  group_by(player.name, player.id, team.name) %>% 
  summarise(xG = sum(shot.statsbomb_xg, na.rm = TRUE)) %>% 
  left_join(player_xGA) %>%
  mutate(xG_xGA = sum(xG+xGA, na.rm =TRUE) ) #2

player_minutes = get.minutesplayed(StatsBombData)

player_minutes = player_minutes %>% 
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed)) #3

player_xG_xGA = left_join(player_xG, player_minutes) %>% 
  mutate(nineties = minutes/90,
  xG_90 = round(xG/nineties, 2),
  xGA_90 = round(xGA/nineties,2),
  xG_xGA90 = round(xG_xGA/nineties,2) ) #4

chart = player_xG_xGA %>% 
  ungroup() %>% 
  filter(minutes>=360) %>% 
  top_n(n = 15, w = xG_xGA90) #5

chart<-chart %>%
  select(1, 9:10)%>%
  pivot_longer(-player.name, names_to = "variable", values_to = "value") %>% 
  filter(variable=="xG_90" | variable=="xGA_90") #6

ggplot(chart, aes(x =reorder(player.name, value), y = value, fill=fct_rev(variable))) + #1 
  geom_bar(stat="identity", colour="white")+
  labs(title = "Expected Goal Contribution", subtitle = "FIFA World Cup, 2022",
       x="", y="Per 90", caption ="Minimum 360 minutes\nNPxG = Value of shots taken (no penalties)\nxG assisted = Value of shots assisted")+ 
  theme(axis.text.y = element_text(size=14, color="#333333", family="Calibri"),
       axis.title = element_text(size=14, color="#333333", family="Calibri"),
       axis.text.x = element_text(size=14, color="#333333", family="Calibri"),
       axis.ticks = element_blank(),
       panel.background = element_rect(fill = "white", colour = "white"),
       plot.background = element_rect(fill = "white", colour ="white"),
       panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
       plot.title=element_text(size=24, color="#333333", family="Calibri" , face="bold"), 
       plot.subtitle=element_text(size=18, color="#333333", family="Calibri", face="bold"), 
       plot.caption=element_text(color="#333333", family="Calibri", size =10), 
       text=element_text(family="Calibri"),
       legend.title=element_blank(),
       legend.text = element_text(size=14, color="#333333", family="Calibri"), 
       legend.position = "bottom") + #2
  scale_fill_manual(values=c("#3371AC", "#DC2228"), labels = c( "xG Assisted","NPxG")) + #3 
  scale_y_continuous(expand = c(0, 0), limits= c(0,max(chart$value) + 0.3)) + #4 
  coord_flip()+ #5
  guides(fill = guide_legend(reverse = TRUE)) #6

warnings()

# xG_Shot Map
shots = StatsBombData %>%
  filter(type.name=="Shot" & (shot.type.name!="Penalty" | is.na(shot.type.name)) & player.name=="Lionel Andrés Messi Cuccittini") #1

shotmapxgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960", 
                     "#FCDC5F", "#F5B94D", "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", 
                     "#BF0000", "#7F0000", "#5F0000") #2

ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) + 
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) + 
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+ 
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+ 
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) + 
  annotate("path", colour = "black", size = 0.6,
            x=60+10*cos(seq(0,2*pi,length.out=2000)),
            y=40+10*sin(seq(0,2*pi,length.out=2000)))+ 
  # add centre spot
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) + 
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") + 
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_point(data = shots, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg, shape = shot.body_part.name),
             size = 6, alpha = 0.8) + #3   
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Calibri", hjust=0.5, vjust=0.5), 
        plot.subtitle = element_text(size = 18, family="Calibri", hjust = 0.5), 
        axis.text.y=element_blank(),
        legend.position = "top",
        legend.title=element_text(size=22,family="Calibri"), 
        legend.text=element_text(size=20,family="Calibri"),
        legend.margin = margin(c(20, 10, -85, 50)),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5, family="Calibri", colour = "black", hjust = 0.5),
        legend.direction = "horizontal",
        axis.ticks=element_blank(),
        aspect.ratio = c(65/100),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Calibri")) +
  labs(title = "Lionel Messi, Shot Map", subtitle = "FIFA World Cup, 2022") + #4
  scale_fill_gradientn(colours = shotmapxgcolors, limit = c(0,0.8), oob=scales::squish, name = "Expected Goals Value") + #5
  scale_shape_manual(values = c("Head" = 21, "Right Foot" = 23, "Left Foot" = 24), name ="") + #6 
  guides(fill = guide_colourbar(title.position = "top"),
         shape = guide_legend(override.aes = list(size = 7, fill = "black"))) + #7
  coord_flip(xlim = c(85, 125)) #8

warnings()

# Where Actions Orcured in a Game
unique(StatsBombData[,'type.name'])

DefAct <- StatsBombData %>% 
  filter(type.name=="Pressure" | type.name== "Duel" |
         type.name=="Ball Recovery" | type.name=="Interception" | 
         type.name=="50/50", team.name=="Liverpool")

create_Pitch() +
  geom_point(data = DefAct, aes(x = location.x, y = location.y,
             fill=`type.name`, colour=`type.name`)) +
  labs(title = "Liverpool (vs Spurs), 2019 UEFA Champions League Final", 
       subtitle = "Defensive Actions") + #4
  scale_y_reverse() #5 

DA %>%
  soccerHeatmap(x = "location.x", y = "location.y", xBins = 21, yBins = 14,
                title = "Liverpool (vs Spurs) 2019 UCL Final", 
                subtitle = "Defensive Actions Heatmap") + #this used soccermatics heatmap function
  scale_y_reverse()

Rec <- StatsBombData %>% 
  filter(type.name=="Ball Receipt*", player.id=="5487")

Rec %>%
  soccerHeatmap(x = "location.x", y = "location.y", xBins = 21, yBins = 14,
                title = "Antoine Griezmann Pass Receptions Heatmap", 
                subtitle = "FIFA World Cup, 2022") + #this used soccermatics heatmap function
  scale_y_reverse()

# Plotting Passes into Final Third 
passes = StatsBombData %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name) &
           team.name=="Liverpool") %>% #filter to only completed passes
  filter(location.x<=80,pass.end_location.x>=80) #filter to only passes to final third

create_Pitch() +
  geom_segment(data = passes, aes(x = location.x, y = location.y,
                                  xend = pass.end_location.x, yend = pass.end_location.y), 
               lineend = "round", linewidth = 0.5, colour = "#000000", arrow =
                 arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + #3 
  labs(title = "Liverpool (vs Spurs), 2019 UEFA Champions Leagye Final", subtitle = "Completed Passes to the Final Third") + #4
  scale_y_reverse() + #5
  coord_fixed(ratio = 105/100) #6

### Soccermatics library

if (!require("devtools")) install.packages("devtools")
devtools::install_github("jogall/soccermatics")

library(soccermatics)

#StatsBombData2 <- StatsBombData2 %>% mutate(location.y=abs(80-location.y))

StatsBombData2 %>%
  filter(team.name == "Liverpool") %>%
  soccerShotmap(theme = "dark")

statsbomb %>%
  filter(team.name == "Argentina") %>%
  soccerShotmap(theme = "grass", colGoal = "yellow", 
                colMiss = "blue", legend = T)

StatsBombData2 %>%
  filter(team.name == "Liverpool") %>%
  soccerPassmap(fill = "lightblue", arrow = "r",
                title = "Liverpool (vs Spurs) 2019 UCL Final",) +
  scale_y_reverse()

StatsBombData2 %>%
  filter(team.name == "Liverpool") %>%
  soccerPassmap(fill = "blue", minPass = 3,
                edge_max_width = 30, edge_col = "grey40", edge_alpha = 1,
                title = "Liverpool vs Spurs 2019 UCL Final") +
  scale_y_reverse()

StatsBombData2 %>%
  filter(type.name == "Pass" & team.name == "Liverpool") %>% 
  soccerHeatmap(x = "location.x", y = "location.y",
                title = "Liverpool vs Spurs", 
                subtitle = "Passing heatmap") +
  scale_y_reverse()

StatsBombData2 %>%
  filter(type.name == "Pressure" & team.name == "Liverpool") %>% 
  soccerHeatmap(x = "location.x", y = "location.y", xBins = 21, yBins = 14,
                title = "Liverpool (vs Spurs) 2019 UCL Final", 
                subtitle = "Defensive pressure heatmap") +
  scale_y_reverse()

StatsBombData %>% 
  filter(type.name == "Pass" & team.name == "Liverpool") %>% 
  soccerPositionMap(id = "player.name", x = "location.x", y = "location.y", 
                    fill1 = "blue",
                    arrow = "r", 
                    title = "2019 UCl Final", 
                    subtitle = "Average pass position")

d3 <- StatsBombData2 %>% 
  filter(type.name == "Pass" & player.id == "3532") %>% 
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), 1, 0)))

soccerPitch(arrow = "r",
            title = "Hendo in the 2019 UCL Final", 
            subtitle = "Pass map") +
  geom_segment(data = d3, aes(x = location.x, xend = pass.end_location.x, y = location.y, yend = pass.end_location.y, col = pass.outcome), alpha = 0.75) +
  geom_point(data = d3, aes(x = location.x, y = location.y, col = pass.outcome), alpha = 0.5) +
  guides(colour = FALSE) 
  scale_y_reverse()

d2 <- StatsBombData2 %>% 
  filter(type.name %in% c("Pressure", "Interception", "Block", "Dispossessed", "Ball Recovery") & team.name == "Liverpool")

soccerPitch(arrow = "r", 
            title = "Liverpool in 2019 UCL Final", 
            subtitle = "Defensive actions") +
  geom_point(data = d2, aes(x = location.x, y = location.y, col = type.name), size = 3, alpha = 0.5)



### ggsoccer library

