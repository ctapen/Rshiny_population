#### Libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(gapminder)
library(googleVis)
library(RColorBrewer)
library(scales)
library(shiny)
library(shinydashboard)
library(tidyr)
library(tidyverse) 


#### Data ETL

# Dataset 9:(ref)
df_9_ISOref_0 <- readr::read_csv("./ISO_countries_region.csv")

# Change column name of ISO 3166-1 numeric code
colnames(df_9_ISOref_0)[names(df_9_ISOref_0)=="country-code"] = "ISO_numeric_code"
colnames(df_9_ISOref_0)[names(df_9_ISOref_0)=="sub-region"] = "subregion"


#------

# Dataset 1: TotalPopSex_Thousands
df_1_TotalPopSex_0 <- readr::read_csv("./TotalPopSex_thousands.csv")
# Note: don't forget to multiply by 1000

# Change column name of ISO 3166-1 numeric code
colnames(df_1_TotalPopSex_0)[names(df_1_TotalPopSex_0)=="ISO 3166-1 numeric code"] = "ISO_numeric_code"

# Gather data to make a row per year instead of a column
df_1_TotalPopSex_1 = df_1_TotalPopSex_0 %>% 
  gather(., key="year", value="pop", `1950`:`2100`, na.rm=F)

# Classes vectors
world = 900
continent = c(935, 904, 909, 908, 905, 903)
subcontinent = c(5501, 5500, 957, 954, 947, 928, 927, 926 ,925, 924, 923, 922, 921, 920, 916, 915, 914, 913, 912, 911, 910, 906, 931)
development = c(1517, 1503, 1502, 1501, 1500, 948, 941, 934, 902, 901)
# Countries < 900

# Classes ifelse statement to categorize into continent, subcontinent, etc.
classes_function = function(x) {
  ifelse(x==world,"world"
         , ifelse(x %in% continent, "continent"
                  , ifelse(x %in% subcontinent,"subcontinent"
                           , ifelse(x %in% development, "development"
                                    , ifelse(x<900, "country_territory", NA
        )))))
}

# Turn population into a number and mult by 1000, flag countries
df_1_TotalPopSex_1 = df_1_TotalPopSex_1 %>% 
  mutate(., numeric_pop = as.numeric(gsub(" ", "", pop))*1000
         , category = classes_function(df_1_TotalPopSex_1$ISO_numeric_code)
         , projection_flag = ifelse(year > 2018, T, F)
        )

# Graph World population over time
df_1_TotalPopSex_2_Filtered1 = df_1_TotalPopSex_1 %>% 
  group_by(year, Location, projection_flag, category) %>% 
  summarise(max=max(numeric_pop))

g1 = ggplot(filter(df_1_TotalPopSex_2_Filtered1,category=="continent")
            , aes(x=year, y=max, group=Location, colour=Location))
x_axis_ticks = scale_x_discrete(breaks=seq(1950, 2100, 10))
y_axis_billions = scale_y_continuous(labels = comma)
line_to_date = geom_line(data = filter(filter(df_1_TotalPopSex_2_Filtered1,category=="continent"), !projection_flag), aes(colour=Location))
line_projection = geom_line(data = filter(filter(df_1_TotalPopSex_2_Filtered1,category=="continent"), projection_flag), aes(colour=Location), linetype="dashed")

## Plot --> actually done in server.R for app w/interactivity
 g1 + geom_point(aes(colour=Location)) + y_axis_billions + x_axis_ticks
 g1 + line_to_date + line_projection + y_axis_billions + x_axis_ticks + 
  labs(x='Year', y='Population', title='Population Over Time') +
  theme(plot.title = element_text(face="bold", hjust = 0.55), axis.title=element_text(size=10, face="bold"), axis.title.x=element_text(hjust = 0.53)) #+
#  ggsave("pop_projections_by_continent.png", width = 11)

# Africa's population growth (figure for PPT)
# df_1_TotalPopSex_1_Filtered1 %>% 
# filter(category=='continent' &year %in% c(2015, 2100)) %>% 
# select(Location, year, numeric_pop)



#------

# Dataset 2: PopulationAgeSex_Thousands
df_2_PopAgeSex_0 <- readr::read_csv("./PopulationAgeSex_thousands.csv")
# Note: don't forget to multiply by 1000

# Change column name of ISO 3166-1 numeric code
colnames(df_2_PopAgeSex_0)[names(df_2_PopAgeSex_0)=="ISO 3166-1 numeric code"] = "ISO_numeric_code"

# Gather data to make a row per year instead of a column
df_2_PopAgeSex_1 = df_2_PopAgeSex_0 %>% 
  gather(., key="year", value="pop", `1950`:`2100`, na.rm=F)

# Age Grouping
age_grouping_function = function(x) {
  ifelse(x %in% c('0-4', '5-9', '10-14', '15-19', '20-24'), "0-24",
         ifelse(x %in% c('25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64'), "25-64",
                  ifelse(x %in% c('65-69', '70-74', '75-79', '80-84', '85-89', '90-94', '95-99', '100+'), "65+", NA)))
}

# Turn population into a number and mult by 1000, flag countries
df_2_PopAgeSex_1 = df_2_PopAgeSex_1 %>% 
  mutate(., numeric_pop = as.numeric(gsub(" ", "", pop))*1000
         , category = classes_function(df_2_PopAgeSex_1$ISO_numeric_code)
         , projection_flag = ifelse(year > 2018, T, F)
         , age_grouping = age_grouping_function(df_2_PopAgeSex_1$Age)
  )

# Filter dataset on years and columns and sum population by age grouping
filter_rose = "continent"
years_rose = c(1990, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
df_2_PopAgeSex_2 = df_2_PopAgeSex_1 %>% 
  filter(category==filter_rose&year %in% years_rose) %>% 
  group_by(year, Location, age_grouping) %>% 
  summarise(numeric_pop=sum(numeric_pop))

# Rose chart --> actually plotted in server.R
rose1 = ggplot(df_2_PopAgeSex_2, aes(x=year, y=numeric_pop, fill=age_grouping, group=Location)) +
  geom_bar(stat="identity", position = "fill") + # position fill to make 100% stacked bar
  coord_polar() + # to make circular
  scale_fill_manual(values = c("#FFFFAA", "#deebf7", "#3182bd"), # brewer.pal(4,"Blues"))
                    name = "Age Grouping") + # to change legend title 
  facet_wrap( ~ Location) + 
  theme(strip.text = element_text(face="bold"), # to bold facet titles
        axis.title = element_text(face="bold"),
        plot.title = element_text(face="bold", hjust=.6),
        legend.title = element_text(face="bold")) + 
  labs(x='Year', y='Population Proportion'
       , title="Age Groups' Population Proportion Across Time") #+
  #ggsave("aging_pop.png", width = 9.5)
rose1


#------
# Youth Plot

# Data on Youth
df_2_PopAgeSex_3 = df_2_PopAgeSex_1 %>% 
  filter(category %in% c("world", "continent")&year %in% years_rose) %>% 
  group_by(year, Location, age_grouping, category) %>% 
  summarise(numeric_pop=sum(numeric_pop)) %>% 
  filter(age_grouping == '0-24')

# Shorten Latin America and the Caribbean to fit on graph
df_2_PopAgeSex_3$Location <- replace(df_2_PopAgeSex_3$Location
                                     , df_2_PopAgeSex_3$Location=="Latin America and the Caribbean"
                                     , "Latin America")

# Reorder the data so world is on top within each year and add row_nbr column
df_2_PopAgeSex_3$Location <- factor(df_2_PopAgeSex_3$Location, ordered = TRUE
                                    , levels = c("World", "Africa", "Asia", "Europe"
                                                 , "Latin America" # and the Caribbean
                                                 , "Northern America", "Oceania")
                                    ) # Order Location so World comes first in the factor

# Order rows, add column for numeric population for waterfall (countries negative), add row number grouped by year
df_2_PopAgeSex_3 = arrange(df_2_PopAgeSex_3, year, Location) %>% 
  mutate(numeric_pop_waterfall = ifelse(Location!="World",numeric_pop*-1/1000000000,numeric_pop/1000000000)) %>% # change continents to neg. so will cascade down from world in waterfall
  group_by(year) %>% # so can row_number separately for each year
  mutate(id = row_number()) # add row nbr based on arrange (on year and location) + group_by parameters so start over each year to create 1-7s

# Create end and start value columns
df_2_PopAgeSex_3$end <- cumsum(df_2_PopAgeSex_3$numeric_pop_waterfall)
df_2_PopAgeSex_3$end <- c(head(df_2_PopAgeSex_3$end, -1), 0)
df_2_PopAgeSex_3$start <- c(0, head(df_2_PopAgeSex_3$end, -1)) # sets start to end value of prior line

# Plot - Waterfall --> actually plotted in server.R
year_waterfall = 2020
ggplot(df_2_PopAgeSex_3 %>% filter(year==year_waterfall), aes(Location, fill = category)) + 
  geom_rect(aes(x = Location, xmin = id - 0.35, xmax = id + 0.35, ymin = end, ymax = start)) +
  labs(y='Population Under 25 (in billions)', title="Youth Population (under 25), by Location") +
  theme(axis.title = element_text(face="bold"),
        plot.title = element_text(face="bold", hjust=.6),
        legend.title = element_text(face="bold")) +
  scale_fill_manual(values = c("#add8e6", "#3182bd")) +
  #as number, not %: geom_text(aes(id, end, label = comma(round(numeric_pop/1000000000,1))), vjust = -0.3, size = 3)
  geom_text(aes(id, end, label = percent(round((numeric_pop/1000000000)/(df_2_PopAgeSex_3 %>% filter(year==year_waterfall,Location=="World"))[["numeric_pop_waterfall"]], 2))), vjust = -0.3, size = 3) #+
  #ggsave("youth_waterfall.png", width=9)
  
# Referenced to build waterfall: https://learnr.wordpress.com/2010/05/10/ggplot2-waterfall-charts/


#------

# Dataset 10: Nbr Births per 5Yr, in Thousands
df_10_NbrBirths_0 <- readr::read_csv("./NbrBirths_Thousands.csv")

# Change column name of ISO 3166-1 numeric code
colnames(df_10_NbrBirths_0)[names(df_10_NbrBirths_0)=="ISO 3166-1 numeric code"] = "ISO_numeric_code"

# Gather data to make a row per year instead of a column
df_10_NbrBirths_1 = df_10_NbrBirths_0 %>% 
  gather(., key="year_span", value="pop", `1950 - 1955`:`2095 - 2100`, na.rm=F)

# Turn population into a number and mult by 1000, flag countries
df_10_NbrBirths_1 = df_10_NbrBirths_1 %>% 
  mutate(., numeric_pop = as.numeric(gsub(" ", "", pop))*1000/5 # divide by 5 yrs
         , category = classes_function(df_10_NbrBirths_1$ISO_numeric_code)
         , projection_flag = ifelse(year_span > 2018, T, F)
         , year = substr(year_span, 1, 4)
  )

# Join data to ISO continent/region lookup table
df_10_NbrBirths_1 = left_join(df_10_NbrBirths_1, df_9_ISOref_0[,7:10]
                              , by=("ISO_numeric_code")) %>% 
  mutate(continent = ifelse(((region=='Americas'&subregion=='Latin America and the Caribbean')) ,'Latin America and the Caribbean', 
                            ifelse(((region=='Americas'&subregion=='Northern America')), 'Northern America', region)))

# Fill in continents missing for continents themselves with their Location name
df_10_NbrBirths_1[is.na(df_10_NbrBirths_1$region)&df_10_NbrBirths_1$category=="continent","continent"] = df_10_NbrBirths_1[is.na(df_10_NbrBirths_1$region)&df_10_NbrBirths_1$category=="continent","Location"]# "world test"

# Graph Babies Born by location --> actually graphed (with input values) in server.R
babies_born_year = 2020
filter2 = c("continent", "country_territory") # "country_territory"
df_10_NbrBirths_1_Filtered1 = df_10_NbrBirths_1 %>% 
  filter(category %in% filter2 & year==babies_born_year) #, projection_flag==0

# Group by, arrnage, remove NA
df_10_NbrBirths_1_Filtered1 = df_10_NbrBirths_1_Filtered1 %>% 
  group_by(year, Location, projection_flag, category, continent) %>% 
  summarise(max=max(numeric_pop)) %>% 
  arrange(continent, -max) %>% 
  filter(., !is.na(continent))# to remove Chanel Islands which creating an NA continent in graph legend

# Add column for row order to sort ggplot by
df_10_NbrBirths_1_Filtered1$row_order = seq.int(nrow(df_10_NbrBirths_1_Filtered1))

# How Many Born in Given Year in Nigeria
births_Nigeria = (df_10_NbrBirths_1_Filtered1 %>% filter(Location=="Nigeria"))[["max"]]

# Create ggplot
g3 = ggplot(df_10_NbrBirths_1_Filtered1, 
            aes(x=reorder(Location,row_order), y=max, shape = category))

# Plot as Bar Chart --> actually plotted in server.R
babies_born_per_year_bar_graph = g3 + 
  geom_hline(yintercept=births_Nigeria, linetype="dashed", color = "#ffbe42", size=0.5) +
  geom_bar(aes(fill=continent),stat="identity") + y_axis_billions + 
  scale_shape_manual(values = c(17, 16)) + 
  scale_fill_manual(values = rev(brewer.pal(8,"YlGnBu"))) + 
  theme(panel.border = element_rect(colour = "black", fill=NA), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(face="bold"),
        plot.title = element_text(face="bold", hjust=.6),
        legend.title = element_text(face="bold")) + 
  labs(y='Babies Born in a Year', x='Continents & Countries', title="Babies Born In a Year, by Location") + 
  geom_text(data=subset(df_10_NbrBirths_1_Filtered1, category =="continent"), aes(x=reorder(continent, -max), y=max,label=Location), position = position_nudge(x = 7, y = -2), size=3) + # facet_grid(. ~ continent)
  geom_text(data=subset(df_10_NbrBirths_1_Filtered1, Location %in% c("Nigeria")), aes(x=reorder(continent, -max), y=max,label=Location,fontface=2), position = position_nudge(x = 8, y = 0), size=3) #+
#ggsave("babies_born_per_year_nigeria.png", width = 9.5)

babies_born_per_year_bar_graph



#------

# Dataset 5: TotalFertilityBabiesPerWoman
df_5_TotalFertility_0 <- readr::read_csv("./TotalFertilityBabiesPerWoman.csv")

# Change column name of ISO 3166-1 numeric code
colnames(df_5_TotalFertility_0)[names(df_5_TotalFertility_0)=="ISO 3166-1 numeric code"] = "ISO_numeric_code"

# Gather data to make a row per year instead of a column
df_5_TotalFertility_1 = df_5_TotalFertility_0 %>% 
  gather(., key="year_span", value="pop", `1950 - 1955`:`2095 - 2100`, na.rm=F)

# Turn population into a number and mult by 1000, flag countries
df_5_TotalFertility_1 = df_5_TotalFertility_1 %>% 
  mutate(., birth_rate = as.numeric(gsub(" ", "", pop)) # don't truly need gsub here
         , category = classes_function(df_10_NbrBirths_1$ISO_numeric_code)
         , projection_flag = ifelse(year_span > 2018, T, F)
         , year = substr(year_span, 1, 4) # year portrayed is first year of the span
  )

# Join data to ISO continent/region lookup table
df_5_TotalFertility_1 = left_join(df_5_TotalFertility_1, df_9_ISOref_0[,7:10]
                              , by=("ISO_numeric_code")) %>% 
  mutate(continent = ifelse(((region=='Americas'&subregion=='Latin America and the Caribbean'))
                              ,'Latin America and the Caribbean', 
                            ifelse(((region=='Americas'&subregion=='Northern America'))
                                   , 'Northern America', region)))

# Filter on only countries, to make map
df_5_TotalFertility_1 = df_5_TotalFertility_1 %>% 
  filter(category=="country_territory", year==2020) %>% 
  select(Location, birth_rate)

# Remove the names with "bad" characters in their strings (gvis won't intake)
# Remove R<82>union (searching just on "R<82>union" didn't work)
df_5_TotalFertility_1 = df_5_TotalFertility_1[-12,] 
# Remove Cote d'Ivoire
df_5_TotalFertility_1 = df_5_TotalFertility_1[-44,] 
# Remove Curacao
df_5_TotalFertility_1 = df_5_TotalFertility_1[-152,] 

# Plot the map --> plotted for real in server.R
gvisfertility = gvisGeoChart(df_5_TotalFertility_1
                           , "Location", "birth_rate"
                           , options=list(width=500, height=400
                                          , colorAxis="{colors:['#ffe9ec', 'green']}"))
# Output plot
# plot(gvisfertility) # commented out so won't run in browser separately when run app


#---------

# Life Expectancy, from Gapminder
ggplot(gapminder %>% filter(year == 2007), aes(x = continent, y = lifeExp, fill = continent)) + 
  geom_boxplot() + 
  labs(x="Continent", y="Life Expectancy (years)", fill="Continent", title="Life Expectancy by Continent") +
  scale_fill_brewer(palette="YlGnBu") +
  theme(plot.title = element_text(face="bold", hjust = 0.55), axis.title=element_text(size=10, face="bold"), legend.position="bottom") #+
  #ggsave("life_expectancy.png")

# Some Statistics
# res = boxplot(lifeExp ~ continent, data = gapminder %>% filter(year == 2007))
# res

