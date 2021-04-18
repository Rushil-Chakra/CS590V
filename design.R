library(tidyverse)
library(maps)
library(scatterpie)
library(reshape)

df <- read.csv("countypres_2000-2016.csv", stringsAsFactors = F)


state_votes <- df %>% group_by(year, state, party) %>%
  summarise(count = sum(candidatevotes), proportion=sum(candidatevotes)/sum(totalvotes), state=tolower(state)) %>%
  replace_na(list(party = "other", proportion=0, count=0)) %>% distinct()

county_votes <- df %>% group_by(year, county, party) %>%
  summarise(count = sum(candidatevotes), proportion=sum(candidatevotes)/sum(totalvotes), state=tolower(state)) %>%
  replace_na(list(party = "other", proportion=0, count=0))

# Proportion of votes each year for each party in California bar chart
state_votes %>% filter(state=="california") %>% ggplot(aes(x=year, y=proportion, fill=party)) + geom_col() +
  theme_bw() + scale_x_continuous(breaks=c(2000, 2004, 2008, 2012, 2016), labels=c(2000, 2004, 2008, 2012, 2016)) +
  ggtitle("Proportion of Votes for each Party in California")




state <- map_data("state")
votes_16 <- state_votes %>% filter(year == 2016, state!="alaska")
dem_16 <- votes_16 %>% group_by(state) %>% filter(party=="democrat")
rep_16 <- votes_16 %>% group_by(state) %>% filter(party=="republican")
other_16 <- votes_16 %>% group_by(state) %>% filter(party=="other")
pie_df <- data.frame(dem=dem_16$count, rep=rep_16$count, other=other_16$count, state=dem_16$state)  %>% 
  left_join(county, by = c("state"="region")) %>% mutate(total = dem + rep + other)

pie_df$group <- NULL
pie_df$subregion <- NULL
pie_df$order <- NULL
pie_df <- pie_df %>% group_by(state) %>% 
  summarise(democrat=dem, republican=rep, other=other, total=total, long=mean(long), lat=mean(lat)) %>% distinct()

county_votes %>% filter(county=="Clark", state=="nevada") %>% ggplot(aes(x=factor(1), y=proportion, fill=party)) +
  geom_bar(stat="identity", position="fill") + facet_wrap(~year) + coord_polar(theta='y') + theme_bw() + 
  ggtitle("Proportion of votes in Clark County, Nevada") + scale_x_discrete(NULL, expand = c(0,0)) +
  scale_y_continuous(NULL, expand = c(0,0)) 

 ggplot(state, aes(long, lat)) + geom_map(map=state, aes(map_id=region), fill=NA, color="black") + coord_quickmap() + 
   geom_scatterpie(aes(x=long, y=lat, r=1.5), data=pie_df, cols=c("democrat", "republican", "other"), color=NA, alpha=.8) + 
   geom_scatterpie_legend(1.5, x=-160, y=30) + theme_void() + ggtitle("Proportion of votes in 2016")

 county_votes %>% filter(state=="arizona") %>% group_by(county, year) %>% summarise(count=sum(count)) %>% ggplot(aes(x=year, y=log(count), color=county)) + geom_line() + ggtitle("Voter Turnout by County for Arizona") + theme_bw()
 county_votes %>% filter(state=="arizona") %>% group_by(county, year, party) %>% summarise(count=sum(count))  %>% 
   filter(county=="Maricopa") %>% ggplot(aes(x=year, y=count, color=party)) + geom_line() + ggtitle("Voter Turnout by County for Maricopa County, Arizona") + theme_bw()
 