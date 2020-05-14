#install.packages("treemapify")
#install.packages("shinydashboard")
#install.packages("sf")
#install.packages("tmap")
#install.packages("stringdist")
#install.packages("leaflet")
#install.packages("funnelR")
#install.packages("dplyr")
#install.packages("geofacet")
#install.packages("GGally")
#install.packages("viris")
#install.packages("directlabels")
library(ggplot2)
library(tidyverse)
library(shiny)
library(plotly)
library(treemapify)
library(shinydashboard)
library(sf)
library(tmap)
library(stringdist)
library(leaflet)
library(funnelR)
library(plyr)
library(dplyr)
library(geofacet)
library(viridis)
library(ggalluvial)
library(tidyr)
library(networkD3)
library(tidygraph)
library(stringr)
library(fuzzyjoin)
library(corrplot)
library(reshape2)
library(directlabels)

#detach("package:GGally", unload=TRUE)
#library(GGally)


#data preprocessing!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
district_stats <- read_csv("data/Consolidated_District_CAW.csv")
total_crimes = 0
for (row in 1:nrow(district_stats)) {
  crimes <- district_stats[row, "Total.Crimes.against.Women"]
  total_crimes = total_crimes + crimes
}
average = total_crimes / 5

years_total_df <- district_stats[,5:12] %>% group_by(district_stats$`Year`) %>% summarize_all(funs(sum))

colnames(years_total_df) <- c("Year", "Rape","Kidnap","Dowry","Assault", "Insult", "Cruelty","Importation", "Crime")

#used for treemap
yearly_crime_df <- data.frame(Year=integer(), Crime=integer(), Number=integer())
for (row in 1:nrow(years_total_df)){
  year <- years_total_df[row, "Year"]
  rape <- years_total_df[row, "Rape"]
  kidnap <- years_total_df[row, "Kidnap"]
  dowry <- years_total_df[row, "Dowry"]
  assault <- years_total_df[row, "Assault"]
  insult <- years_total_df[row, "Insult"]
  cruelty <- years_total_df[row, "Cruelty"]
  importation <- years_total_df[row, "Importation"]
  
  
  
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Rape", rape)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Kidnap", kidnap)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Dowry", dowry)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Assault", assault)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Insult", insult)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Cruelty", cruelty)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Importation", importation)
}
rape_total = 0
kidnap_total = 0 
dowry_total =0
assault_total = 0
insult_total = 0
cruelty_total = 0
importation_total = 0
total_year_crimes_df <- data.frame(year = integer(),Crime= integer(),Number=integer())

for (row in 1:nrow(yearly_crime_df)) {
  crime <- yearly_crime_df[row,"Crime"]
  if (crime == "Rape") {
    rape_total = rape_total + yearly_crime_df[row, "Number"]
  }
  if (crime == "Kidnap") {
    kidnap_total = kidnap_total + yearly_crime_df[row, "Number"]
  }
  if (crime == "Dowry") {
    dowry_total = dowry_total + yearly_crime_df[row, "Number"]
  }
  if (crime == "Assault") {
    assault_total = assault_total + yearly_crime_df[row, "Number"]
  }
  if (crime == "Insult") {
    insult_total = insult_total + yearly_crime_df[row, "Number"]
  }
  if (crime == "Cruelty") {
    cruelty_total = cruelty_total + yearly_crime_df[row, "Number"]
  }
  if (crime == "Importation") {
    importation_total = importation_total + yearly_crime_df[row, "Number"]
  }
}

total_year_crimes_df[nrow(total_year_crimes_df) + 1,] = c(2020,"Rape", rape_total)
total_year_crimes_df[nrow(total_year_crimes_df) + 1,] = c(2020,"Kidnap", kidnap_total)
total_year_crimes_df[nrow(total_year_crimes_df) + 1,] = c(2020,"Dowry", dowry_total)
total_year_crimes_df[nrow(total_year_crimes_df) + 1,] = c(2020,"Assault", assault_total)
total_year_crimes_df[nrow(total_year_crimes_df) + 1,] = c(2020,"Insult", insult_total)
total_year_crimes_df[nrow(total_year_crimes_df) + 1,] = c(2020,"Cruelty", cruelty_total)
total_year_crimes_df[nrow(total_year_crimes_df) + 1,] = c(2020,"Importation", importation_total)

#CHENG PART!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
states <- st_read(dsn = "data/geospatial", 
                  layer = "gadm36_IND_1")

# For State Level Data, sum up columns for each state
state_stats <- district_stats %>%
  group_by(Year, STATE.UT) %>%
  summarise_if(is.numeric, funs(sum))

# Drop X column
state_stats <- as.data.frame(state_stats)  
state_stats <- dplyr::select(state_stats, -c(X1)) 

# Change state names to uppercase
states <- mutate_at(states, .vars = vars(NAME_1), .funs=toupper)

# Create match column to suit the shapefile
i <- amatch(state_stats$STATE.UT, states$NAME_1, maxDist = 3)
state_stats$match[!is.na(i)] <- states$NAME_1[i[!is.na(i)]]

# Manually imputing values which are not replaced
state_stats <- state_stats %>% 
  mutate(match = replace(match, STATE.UT == "A & N ISLANDS", "ANDABAR & NICOMAN ISLANDS")) %>%
  mutate(match = replace(match, STATE.UT == "D & N HAVELI", "DADRA AND NAGAR HAVELI")) %>%
  mutate(match = replace(match, STATE.UT == "DELHI", "NCT OF DELHI")) %>% 
  mutate(match = replace(match, STATE.UT == "A&N ISLANDS", "ANDABAR & NICOMAN ISLANDS")) %>%
  mutate(match = replace(match, STATE.UT == "D&N HAVELI", "DADRA AND NAGAR HAVELI")) %>%
  mutate(match = replace(match, STATE.UT == "DELHI UT", "NCT OF DELHI"))
# merge spatial and attribute data on "match" column
state_stats$STATE.UT <- state_stats$match

# For Spatial Data, drop irrelevant columns
#states <- dplyr::select(states, -c(GID_0, NAME_0, GID_1, VARNAME_1, NL_NAME_1, TYPE_1, ENGTYPE_1, CC_1, HASC_1))

choro_data <- left_join(states, state_stats, 
                        by = c("NAME_1" = "match"))

### Prepare Data for Funnel Plot ###
# read population data and select relevant columns
popn_data <- read.csv("data/india-districts-census-2011.csv")
popn_data <- tbl_df(popn_data)
popn_data <- popn_data %>%
  dplyr::select(c(2:6)) %>%
  group_by(State.Name) %>%
  summarise_if(is.numeric, funs(sum))

popn_data <- popn_data[order(popn_data$State.Name),]

# crime data
funnel_states <- dplyr::select(state_stats, -c(match))

# merge popn and crime data
j <- amatch(funnel_states$STATE.UT, popn_data$State.Name, maxDist = 3)
funnel_states$match <- popn_data$State.Name[j]
funnel_states <- transform(funnel_states, match = as.character(match))

funnel_states <- funnel_states %>% 
  mutate(match = replace(match, which(is.na(match) & STATE.UT == "ANDABAR & NICOMAN ISLANDS"), "ANDAMAN AND NICOBAR ISLANDS")) 

funnel_data <- left_join(funnel_states, popn_data, by = c("match" = "State.Name"))    

#CHENG PART GEOFACET!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
state_stats_facet <- district_stats %>%
  group_by(Year, STATE.UT) %>%
  summarise_if(is.numeric, funs(sum))

test_df <- data.frame(Year= integer(), state= integer(), Crime= integer(),Number=integer())
for (row in 1:nrow(state_stats_facet)){
  
  year <- state_stats_facet[row, "Year"]

    state <- state_stats_facet[row,'STATE.UT']
    if (state == "ANDHRA PRADESH") {
      state = "AP"
    } else if (state == "ARUNACHAL PRADESH") {
      state = "AR"
    } else if (state == "ASSAM") {
      state = "AS"
    } else if (state == "BIHAR") {
      state = "BR"
    } else if (state == "CHANDIGARH") {
      state = "CH"
    } else if (state == "CHHATTISGARH") {
      state = "CG" #was CT
    } else if (state == "D & N HAVELI" || state == "D&N HAVELI") {
      state = "DH"
    } else if (state == "DAMAN & DIU") {
      state = "DD"
    } else if (state == "DELHI UT" || state == "DELHI") {
      state = "DL"
    } else if (state == "GOA") {
      state = "GA"
    } else if (state == "GUJARAT") {
      state = "GJ"
    } else if (state == "HARYANA") {
      state = "HR"
    } else if (state == "HIMACHAL PRADESH") {
      state = "HP"
    } else if (state == "JAMMU & KASHMIR") {
      state = "JK"
    } else if (state == "JHARKHAND") {
      state = "JH"
    } else if (state == "KARNATAKA") {
      state = "KA"
    } else if (state == "KERALA") {
      state = "KL"
    } else if (state == "LAKSHADWEEP") {
      state = "LD"
    } else if (state == "MADHYA PRADESH") {
      state = "MP"
    } else if (state == "MAHARASHTRA") {
      state = "MH"
    } else if (state == "MANIPUR") {
      state = "MN"
    } else if (state == "MEGHALAYA") {
      state = "ML"
    } else if (state == "MIZORAM") {
      state = "MZ"
    } else if (state == "NAGALAND") {
      state = "NL"
    } else if (state == "ODISHA") {
      state = "OD" #WAS OR
    } else if (state == "PUDUCHERRY") {
      state = "PY"
    } else if (state == "PUNJAB") {
      state = "PB"
    } else if (state == "RAJASTHAN") {
      state = "RJ"
    } else if (state == "SIKKIM") {
      state = "SK"
    } else if (state == "TAMIL NADU") {
      state = "TN"
    } else if (state == "TRIPURA") {
      state = "TR"
    } else if (state == "UTTAR PRADESH") {
      state = "UP"
    } else if (state == "UTTARAKHAND") {
      state = "UK"
    } else if (state == "WEST BENGAL") {
      state = "WB"
    } else if (state == "A & N ISLANDS") {
      state = "AN"
    } else if (state == "TELANGANA") {
      state = "TS" #was tg
    }
    
    year <- state_stats_facet[row, "Year"]
    rape <- state_stats_facet[row,"Rape"]
    kidnap <- state_stats_facet[row,"Kidnapping.and.Abduction"]
    dowry <- state_stats_facet[row,"Dowry.Deaths"]
    assault <- state_stats_facet[row,"Assault.on.women.with.intent.to.outrage.her.modesty"]
    insult <- state_stats_facet[row,"Insult.to.modesty.of.Women"]
    cruelty <- state_stats_facet[row,"Cruelty.by.Husband.or.his.Relatives"]
    importation <- state_stats_facet[row,"Importation.of.Girls"]
    
    test_df[nrow(test_df) + 1,] = c(year, state, "Rape", rape)
    test_df[nrow(test_df) + 1,] = c(year, state, "Kidnap", kidnap)
    test_df[nrow(test_df) + 1,] = c(year, state, "Dowry", dowry)
    test_df[nrow(test_df) + 1,] = c(year, state, "Assault", assault)
    test_df[nrow(test_df) + 1,] = c(year, state, "Insult", insult)
    test_df[nrow(test_df) + 1,] = c(year, state, "Cruelty", cruelty)
    test_df[nrow(test_df) + 1,] = c(year, state, "Importation", importation)
  
  
}




#SEAN CHAI PART!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

###################################################################################
# DATA PREP FOR DASHBOARD 3# 
###################################################################################
data_2001_2012 <- read_csv("data/dstrCAW_1.csv")
data_2013 <- read_csv("data/dstrCAW_2013.csv")

# create total field for 2013 data
total <- data_2013$Rape + data_2013$`Kidnapping and Abduction` + data_2013$`Dowry Deaths` + data_2013$`Assault on women with intent to outrage her modesty` + data_2013$`Insult to modesty of Women` + data_2013$`Cruelty by Husband or his Relatives` + data_2013$`Importation of Girls`
data_2013$`Total Crimes against Women` = total

# union: years 2001-2012 and 2013 data
data_2001_2013 <- rbind(data_2013, data_2001_2012)

# dropping rows for total in each district
data_2001_2013 <-data_2001_2013[!(data_2001_2013$DISTRICT=="ZZ TOTAL" | data_2001_2013$DISTRICT=="TOTAL"),]

# change all state data to uppercase to resolve differences between both files
data_2001_2013 <- apply(data_2001_2013,2,toupper)

data_2001_2013 <- as.data.frame(data_2001_2013, stringsAsFactors = FALSE)

# import 2014 and 2015 data and union
data_2014 <- read_csv("data/District-wise_Crimes_committed_against_Women_2014.csv")
data_2015 <- read_csv("data/District-wise_Crimes_committed_against_Women_2015_1.csv")
data_2014_2015 <- rbind(data_2014, data_2015)

# Dropping columns which are not found in 2001-2013 dataset
drop <- c("Sl. No.","Dowry Prohibition Act, 1961", "Indecent Representation of Women (P) Act, 1986", "Protection of Children from Sexual Offences Act", "Protection of Women from Domestic Violence Act, 2005", "Immoral Traffic Prevention Act", "Attempt to commit Rape", "Abetment of Suicides of Women", "Total Crimes against Women")
data_2014_2015 = data_2014_2015[,!(names(data_2014_2015) %in% drop)]

# Rename columns to match those in 2001-13 data
names(data_2014_2015)[1]<-"STATE/UT"
names(data_2014_2015)[2]<-"DISTRICT"
names(data_2014_2015)[5]<-"Kidnapping and Abduction"
names(data_2014_2015)[7]<-"Assault on women with intent to outrage her modesty"
names(data_2014_2015)[8]<-"Insult to modesty of Women"
names(data_2014_2015)[10]<-"Importation of Girls"

# Create Total field for data
total <- data_2014_2015$Rape + data_2014_2015$`Kidnapping and Abduction` + data_2014_2015$`Dowry Deaths` + data_2014_2015$`Assault on women with intent to outrage her modesty` + data_2014_2015$`Insult to modesty of Women` + data_2014_2015$`Cruelty by Husband or his Relatives` + data_2014_2015$`Importation of Girls`
data_2014_2015$`Total Crimes against Women` = total

# Drop rows for total in each district
data_2014_2015 <-data_2014_2015[!(data_2014_2015$DISTRICT=="Total District(s)"),]

# Convert State and District names to Uppercase
data_2014_2015 <- apply(data_2014_2015,2,toupper)

# Combine 2001-15 data
combined_data <- rbind(data_2001_2013, data_2014_2015)

# Creating new data frame for 2011 data set 
district_2011 <- filter(combined_data, Year == 2011)

# Renaming names with "&" to prevent errors in plot
district_2011$`STATE/UT` <- revalue(district_2011$`STATE/UT`, c("A & N ISLANDS"="ANDAMAN AND NICOBAR ISLANDS"))
district_2011$`STATE/UT` <- revalue(district_2011$`STATE/UT`, c("D & N HAVELI"="DADRA AND NAGAR HAVELI"))
district_2011$`STATE/UT` <- revalue(district_2011$`STATE/UT`, c("DAMAN & DIU"="DAMAN AND DIU"))
district_2011$`STATE/UT` <- revalue(district_2011$`STATE/UT`, c("JAMMU & KASHMIR"="JAMMU AND KASHMIR"))

# Convert chr to numeric 
district_2011$`Year` <- as.numeric(as.character(district_2011$`Year`))
district_2011$`Rape` <- as.numeric(as.character(district_2011$`Rape`))
district_2011$`Kidnapping and Abduction` <- as.numeric(as.character(district_2011$`Kidnapping and Abduction`))
district_2011$`Dowry Deaths` <- as.numeric(as.character(district_2011$`Dowry Deaths`))
district_2011$`Assault on women with intent to outrage her modesty` <- as.numeric(as.character(district_2011$`Assault on women with intent to outrage her modesty`))
district_2011$`Insult to modesty of Women` <- as.numeric(as.character(district_2011$`Insult to modesty of Women`))
district_2011$`Cruelty by Husband or his Relatives` <- as.numeric(as.character(district_2011$`Cruelty by Husband or his Relatives`))
district_2011$`Importation of Girls` <- as.numeric(as.character(district_2011$`Importation of Girls`))
district_2011$`Total Crimes against Women` <- as.numeric(as.character(district_2011$`Total Crimes against Women`))

# Remove "Year" column
district_2011 = subset(district_2011, select = -c(3) )

# Read dataset for socio-economic factors
district_socio <- read_csv("data/all.csv")
district_socio = subset(district_socio, select = -c(1,5,6,7,8,9,10,11,12,15,16,17,18,19,20,22,23,24,
                                                    25,26,27,28,29,31,32,34,35,36,37,39,
                                                    40,41,42,43,44,45,46,47,48,49,50,51,52,53,
                                                    54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,
                                                    69,70,71,72,73,74,75,76,77,78,79,80,81,82)
)

# Getting data as percentage of population
district_socio[,5] <- district_socio[,5]/1000
district_socio[,7] <- district_socio[,7]/ district_socio[,3]*100
district_socio[,8] <- district_socio[,8]/ district_socio[,3]*100
district_socio[,9] <- district_socio[,9]/ district_socio[,3]*100

# Rename column names for socio_economic data
colnames(district_socio) <- c("STATE/UT", "DISTRICT", "Population", "Average Household Size", "Females per Male",
                              "Literacy Rates (%)","Higher Secondary Graduates (%)", "Persons aged 15-59 (%)", "Percent of Non-Workers (%)")

for (i in 1:length(district_socio$`STATE/UT`)){
  name_list <- (strsplit(district_socio$`DISTRICT`[i], ", "))
  name <- sapply(name_list, "[", 2)
  name <- str_replace(name, " \\(.*\\)", "")
  district <- sapply(name_list, "[", 1)
  district <- str_replace(district, "  *\\(.*\\)", "")
  district <- gsub("[*].*$","",district)
  district <- gsub("District ","",district)
  district_socio$`DISTRICT`[i] <- toupper(district)
  if (is.na(name)){
    if ( district_socio$`STATE/UT`[i] == "Delhi"){
      district_socio$`STATE/UT`[i] <- "DELHI"
    } else if (district_socio$`STATE/UT`[i] == "Goa"){
      district_socio$`STATE/UT`[i] <- "GOA"
    } else if (district_socio$`STATE/UT`[i] == "Gujarat"){
      district_socio$`STATE/UT`[i] <- "GUJARAT"
    } else if (district_socio$`STATE/UT`[i] == "Karnataka"){
      district_socio$`STATE/UT`[i] <- "KARNATAKA"
    } else if (district_socio$`STATE/UT`[i] == "Meghalya"){
      district_socio$`STATE/UT`[i] <- "MEGHALYA"
    } else if (district_socio$`STATE/UT`[i] == "Orrisa"){
      district_socio$`STATE/UT`[i] <- "ORISSA"
    } else if (district_socio$`STATE/UT`[i] == "Sikkim"){
      district_socio$`STATE/UT`[i] <- "SIKKIM"
    } else if (district_socio$`STATE/UT`[i] == "TN"){
      district_socio$`STATE/UT`[i] <- "TAMIL NADU"
    } else if (district_socio$`STATE/UT`[i] == "Tripura"){
      district_socio$`STATE/UT`[i] <- "TRIPURA"
    } else if (district_socio$`STATE/UT`[i] == "UP"){
      district_socio$`STATE/UT`[i] <- "UTTAR PRADESH"
    } else if (district_socio$`STATE/UT`[i] == "WB"){
      district_socio$`STATE/UT`[i] <- "WEST BENGAL"
    } 
  } else {
    district_socio$`STATE/UT`[i] <- toupper(name)
  } 
}
rm(name_list)

district_socio$`STATE/UT` <- revalue(district_socio$`STATE/UT`, c("ANDAMAN & NICOBAR ISLANDS"="ANDAMAN AND NICOBAR ISLANDS"))
district_socio$`STATE/UT` <- revalue(district_socio$`STATE/UT`, c("DADRA & NAGAR HAVELI"="DADRA AND NAGAR HAVELI"))
district_socio$`STATE/UT` <- revalue(district_socio$`STATE/UT`, c("DAMAN & DIU"="DAMAN AND DIU"))
district_socio$`STATE/UT` <- revalue(district_socio$`STATE/UT`, c("JAMMU & KASHMIR"="JAMMU AND KASHMIR"))
district_socio$`STATE/UT` <- revalue(district_socio$`STATE/UT`, c("ORISSA"="ODISHA"))
district_socio$`STATE/UT` <- revalue(district_socio$`STATE/UT`, c("Bihar"="BIHAR"))
district_socio$`STATE/UT` <- revalue(district_socio$`STATE/UT`, c("MEGHALYA"="MEGHALAYA"))
district_socio$`STATE/UT` <- revalue(district_socio$`STATE/UT`, c("PONDICHERRY"="PUDUCHERRY"))
district_socio$`STATE/UT` <- revalue(district_socio$`STATE/UT`, c("UTTARANCHAL"="UTTARAKHAND"))



district_2011 <- district_2011 %>% stringdist_left_join(district_socio, by=c(`STATE/UT` = "STATE/UT", DISTRICT = "DISTRICT"), method="lcs", max_dist="1")
district_2011 <- district_2011[c(1,11,2,12,3,4,5,6,7,8,9,10,13,14,15,16,17,18,19)]
district_2011 <- na.omit(district_2011)
district_2011 = subset(district_2011, select = -c(2,4) )
names(district_2011)[1]<-"STATE/UT"
names(district_2011)[2]<-"DISTRICT"

###################################################################################
# Create State-Level Data # 
###################################################################################

# State Level Data for Crimes
state_2011 <- subset(district_2011, select = -c(2,11,12,13,14,15,16,17) )
state_2011 <- state_2011 %>% group_by(`STATE/UT`) %>% summarize_all(funs(sum))

# State Level Data for Socio-Economic Factors
state_socio <- subset(district_2011, select = c(1,11,12,13,14,15,16,17) )
population <- aggregate(state_socio["Population"], by=state_socio["STATE/UT"], sum,na.rm=TRUE)
state_socio = subset(state_socio, select = -c(2) )
state_socio <- state_socio %>% group_by(`STATE/UT`) %>% summarize_all(funs(mean), na.rm=TRUE)
state_socio <- merge(x = state_socio, y = population, by = "STATE/UT", all.x = TRUE)
state_socio <- state_socio[c(1,8,2,3,4,5,6,7)]

# Creating combined dataset for state level socio + crime
state_2011 <- merge(x = state_2011, y = state_socio, by = "STATE/UT", all.x = TRUE)

###################################################################################
# Create a new row that calculates the mean of all columns #
###################################################################################

# STATE #
temp <- data.frame(t(colMeans(state_2011[,2:length(state_2011)], na.rm="True")))
temp <- cbind(`STATE/UT`="INDIA AVERAGE", temp)
colnames(temp) <- colnames(state_2011)
state_2011 <- rbind(state_2011,temp)

# DISTRICT #
temp <- data.frame(t(colMeans(district_2011[,3:length(district_2011)], na.rm="True")))
temp <- cbind(`DISTRICT`="INDIA AVERAGE", temp)
temp <- cbind(`STATE/UT`="INDIA AVERAGE", temp)
colnames(temp) <- colnames(district_2011)
district_2011 <- rbind(district_2011,temp)

###################################################################################
#TIDY GLOBAL ENV#
###################################################################################

rm(population, state_socio, district_socio,temp, data_2001_2012, data_2001_2013, data_2013, data_2014, data_2014_2015, data_2015,district,drop,i,name,total)
district_2011[,-c(1,2)] <- round(district_2011[,-c(1,2)],2)
state_2011[,-1] <- round(state_2011[,-1], 2)


# This function will help us format our labels for our graph 
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

# Create a column for Percentile of Crime 
state_2011$Percentile.of.Total.Crime <- with(state_2011, factor(                   
  findInterval( `Total Crimes against Women`, c(-Inf,
                                                quantile(`Total Crimes against Women`, probs=c(0.25, .5, .75)), Inf)),                 
  labels=c("0-25th Percentile","25th-50th Percentile","50th-75th Percentile", "75th-100th Percentile")
))

# Create a column with names of each state 
for (state in state_2011$`STATE/UT`){
  state_2011[, state] <- ifelse(grepl(state, state_2011$`STATE/UT`), state, "OTHERS")
}

# Remove whitespace with '.' for column 18 onwards
for (i in 18:length(names(state_2011))){
  names(state_2011)[i] <- gsub(" ",".",names(state_2011)[i])
}

# Ensure all State values for row = "INDIA AVERAGE" shows "INDIA AVERAGE"
temp <- tail(state_2011,1)
temp[, 18:length(temp)] <- "INDIA AVERAGE"
state_2011 <- head(state_2011, -1)
state_2011 <- rbind(state_2011,temp)

# Create function to return STATE/UT Name
state_ut <- function(input, state_2011){
  for(i in 1:length(input)){
    #print(input[i])
    if (as.numeric(input[i]) < 14){
      #print(input[i])
      input[i] <- state_2011$`STATE/UT`[as.numeric(input[i])]
      #print(input[i])
      #print(combined_2011$`STATE/UT`[input[i]])
    } else if (as.numeric(input[i]) == 15){
      input[i] <- "JAMMU AND KASHMIR"
      #print(input[i])
    } else if (as.numeric(input[i]) == 14){
      input[i] <- "INDIA AVERAGE"
      #print(input[i])
    } else {
      input[i] <- state_2011$`STATE/UT`[as.numeric(input[i])-1]
      #print(input[i])
    } 
  }
  return (input)
}  

to_numeric <- function(input, state_2011){
  input_var <- input 
  index <- which(state_2011$`STATE/UT` == input_var)
  if (index >= 14){
    index <- index + 1
  }
  return (index)
}

# Create function to return State Name for color mapping
color_map <- function(input, state_2011){
  for (i in 1:length(input)){
    if (as.numeric(input[i]) == 3){
      input[i] <- "OTHERS"
    } else if (as.numeric(input[i]) == 1){
      input[i] <- "DELHI"
    } else {
      input[i] <- "INDIA AVERAGE"
    }
  } 
  return (input)
}

# Create function to return Percentile
r_percentile <- function(input, state_2011){
  for (i in 1:length(input)){
    if (as.numeric(input[i]) == 1){
      input[i] <- "0-25th Percentile"
    } else if (as.numeric(input[i]) == 2){
      input[i] <- "25th-50th Percentile"
    } else if (as.numeric(input[i]) == 3){
      input[i] <- "50th-75th Percentile"
    } else if (as.numeric(input[i]) == 4){
      input[i] <- "75th-100th Percentile"
    } else {
      return (input)
    }
  }
  return (input)
}

# Rescale values
rescale <- function(x){(x-min(x))/(max(x)-min(x))}

#https://www.rdocumentation.org/packages/viridis/versions/0.5.1/topics/scale_color_viridis  (iridis)
#######################################################################

# Correlation Heatmap #


# Function for reordering correlation matrix
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Rename y axis variables for correlation heatmap
rename_y <- function(y_list){
  for (i in 1:length(y_list)){
    if (y_list[i]== "Kidnapping and Abduction"){
      y_list[i] <- "kidnap.abduct"
    } else if (y_list[i] == "Importation of Girls"){
      y_list[i] <- "import.girls"
    } else if (y_list[i] == "Rape"){
      y_list[i] <- "rape"
    } else if (y_list[i] == "Dowry Deaths"){
      y_list[i] <- "dowry.deaths"
    } else if (y_list[i] == "Assault on women with intent to outrage her modesty"){
      y_list[i] <- "assault.women"
    } else if (y_list[i] == "Insult to modesty of Women"){
      y_list[i] <- "insult.modesty"
    } else if (y_list[i] == "Cruelty by Husband or his Relatives"){
      y_list[i] <- "cruelty.husband"
    }
  }
  return (y_list)
}

# Rename x axis variables for correlation heatmap
rename_x <- function(x_list){
  for (i in 1:length(x_list)){
    if (x_list[i] == "Average Household Size"){
      x_list[i] <- "household.size"
    } else if (x_list[i] == "Percent of Non-Workers (%)"){
      x_list[i] <- "non.workers"
    } else if (x_list[i] == "Population"){
      x_list[i] <- "population"
    } else if (x_list[i] == "Females per Male"){
      x_list[i] <- "females.per.male"
    } else if (x_list[i] == "Literacy Rates (%)"){
      x_list[i] <- "literacy.rates"
    } else if (x_list[i] == "Higher Secondary Graduates (%)"){
      x_list[i] <- "higher.sec.grads"
    } else if (x_list[i] == "Persons aged 15-59 (%)"){
      x_list[i] <- "person.aged.15-59"
    }
  }
  return (x_list)
}

# Convert x and y axis variable names to a list
to_strlist <- function(list){
  output <- list()
  for (i in 1:length(list)){
    #list[i] <- paste(list[i], ",", sep="'") 
    #list[i] <- paste("'", list[i], sep="")
    #print(i)
    output[[i]] <- list[i]
    #print(output)
  }
  #output <- substr(output, 1, nchar(output)-1)
  #output <- paste(output, ")", sep="")
  #output <- paste("(", output,sep="")
  #output <- gsub("'",'"', output)
  #print(output)
  return (output)
}

#plotting Charts
#years_total_df$Year <- as.factor(years_total_df$Year)

bar_chart_total_crimes <- ggplot(data=years_total_df, aes(x=Year, y=Crime, fill=factor(Year))) + 
      geom_bar(stat="identity") +
      geom_text(aes(label=Crime), vjust=2.6, color="white", size=3.5) +
      geom_hline(yintercept=276423.6, color="red", linetype="dotted") +
  scale_fill_manual(values=c("2011" = "darkgoldenrod1", "2012" = "orange", "2013" = "darkorange3", "2014" = "cornflowerblue", "2015" = "darkorchid"))+
  labs(fill = "Year")
#years_total_df$Year <-as.numeric(years_total_df$Year)

options(scipen = 999)


#V4 DATA PREP CHENG!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
police <- read.csv("data/Police Disposal.csv")
police <- as.data.frame(police)


#convert data to required format
police <- police %>%
  filter(Category != "SLL Cases") %>%
  filter(Crime.Head != "Abetment of Suicides of Women") %>%
  filter(Crime.Head != "Causing Miscarriage Without Womens Consent") %>%
  filter(Crime.Head != "Deaths Caused by Act Done with Intent to Cause Miscarriage") %>%
  filter(Crime.Head != "Acid Attack & Attempt to Acid Attack") %>%
  filter(Crime.Head != "Attempt to Commit Rape" ) %>%
  filter(Crime.Head != "Unnatural Offences") %>%
  dplyr::select(-c(S..No., Category, Chargesheeting.Rate, Pendency.Percentage)) %>%
  dplyr::rename(Cases.Transferred = Cases.Transferred.to.other.PS..Magistrate) %>%
  rename(Cases.Not.Investigated = Cases.not.Investigated.U.S.157.1...b..of.Cr.PC) %>%
  rename(Insufficient.Evidence = Final.Report...True.but.Insufficient.Evidence) %>%
  rename(False = Final.Report...False) %>%
  rename(Mistake.of.Fact = Final.Report...Mistake.of.Fact) %>%
  rename(Non.Cognizable = Final.Report...Non.Cognizable) %>%
  rename(Final.Report = Final.Report...Total) %>%
  rename(Chargesheets.Submitted = Cases.in.which.Chargesheets.were.Submitted) %>%
  filter(Crime.Head != "Total Crime Against Women")

court <- read.csv("data/Court Disposal.csv")
court <- as.data.frame(court)

court <- court %>%
  filter(Category != "SLL Cases") %>%
  filter(Crime.Head != "Abetment of Suicides of Women") %>%
  filter(Crime.Head != "Causing Miscarriage Without Womens Consent") %>%
  filter(Crime.Head != "Deaths Caused by Act Done with Intent to Cause Miscarriage") %>%
  filter(Crime.Head != "Acid Attack & Attempt to Acid Attack") %>%
  filter(Crime.Head != "Attempt to Commit Rape" ) %>%
  filter(Crime.Head != "Unnatural Offences") %>%
  dplyr::select(-c(S..No., Category, Conviction.Rate, Pendency.Percentage)) %>%
  rename(Trial.Cases.Withdrawn.by.Govt = Number.of.Cases.Withdrawn.by.the.Govt.) %>%
  rename(Disposed.by.Plea.Bargaining = Number.of.Cases.Disposed.by.Plea.Bargaining) %>%
  rename(Trials.Completed = Cases.in.which.Trials.were.Completed) %>%
  rename(Conviction = Cases.Convicted) %>%
  rename(Acquittion.or.Discharge = Cases.Acquitted.or.Discharged) %>%
  rename(Disposed.by.Court = Total.Cases.Disposed.off.by.Courts) %>%
  rename(Pending.Trial = Total.Cases.Pending.Trial.at.the.End.of.the.Year) %>%
  filter(Crime.Head != "Total Crime Against Women")


court_police = inner_join(court, police, by = "Crime.Head")

create_freq <- function(data) {
  output_data = data.frame(Source = character(),
                           Dest = character(),
                           Freq = integer())
  
  crime_types <- police %>% pull(Crime.Head)
  
  for (type_crime in crime_types) {
    filtered_df <- data %>%
      dplyr::select(everything()) %>%
      filter (Crime.Head == type_crime)
    
    phase0_1 = "Crime.Head, Cases.Reported.during.the.Year"
    phase0_2 = "Crime.Head, Cases.Pending.Investigation.from.Previous.Year"
    
    steps_list <- list(phase0_1, phase0_2)
    
    for (steps in steps_list) {
      # split the column names
      step <- as.list(strsplit(steps, ', ')[[1]])        
      
      # get the data in those columns
      col_1 = unlist(step[1])
      source_val <- filtered_df[, col_1]
      
      col_2 = unlist(step[2])
      dest_val <- filtered_df[, col_2]
      
      # store into the dataframe in the correct format
      new_data <- data.frame(Source = source_val, Dest = col_2, Freq = dest_val)
      
      output_data <- rbind(output_data, new_data)
    }
  }
  # sum the crimes tgt after phase 1
  consolidated_df = data %>%
    summarise_if(is.numeric, funs(sum))
  
  phase1_1 = "Cases.Reported.during.the.Year, Cases.Withdrawn.by.Govt."
  phase1_2 = "Cases.Reported.during.the.Year, Cases.Transferred"
  phase1_3 = "Cases.Reported.during.the.Year, Cases.Not.Investigated"
  phase1_4 = "Cases.Reported.during.the.Year, Final.Report"
  phase1_5 = "Cases.Pending.Investigation.from.Previous.Year, Cases.Withdrawn.by.Govt."
  phase1_6 = "Cases.Pending.Investigation.from.Previous.Year, Cases.Transferred"
  phase1_7 = "Cases.Pending.Investigation.from.Previous.Year, Cases.Not.Investigated"
  phase1_8 = "Cases.Pending.Investigation.from.Previous.Year, Final.Report"
  phase1_9 = "Cases.Reported.during.the.Year, Cases.Pending.Investigation.at.the.End.of.the.Year" 
  
  steps_list <- list(phase1_1, phase1_2, phase1_3, phase1_4, phase1_5, 
                     phase1_6, phase1_7, phase1_8, phase1_9)
  
  for (steps in steps_list) {
    # split the column names
    step <- as.list(strsplit(steps, ', ')[[1]])        
    
    # get the data in those columns
    col_1 = unlist(step[1])
    source_val <- consolidated_df[, col_1]
    
    col_2 = unlist(step[2])
    dest_val <- consolidated_df[, col_2]
    
    # store into the dataframe in the correct format
    new_data <- data.frame(Source = col_1, Dest = col_2, Freq = dest_val)
    
    output_data <- rbind(output_data, new_data)
  }
  
  
  phase2_1 = "Final.Report, Insufficient.Evidence"
  phase2_2 = "Final.Report, False"
  phase2_3 = "Final.Report, Mistake.of.Fact"
  phase2_4 = "Final.Report, Non.Cognizable"
  phase2_5 = "Final.Report, Chargesheets.Submitted"
  
  steps_list <- list(phase2_1, phase2_2, phase2_3, phase2_4, phase2_5)
  
  for (steps in steps_list) {
    # split the column names
    step <- as.list(strsplit(steps, ', ')[[1]])        
    
    # get the data in those columns
    col_1 = unlist(step[1])
    source_val <- consolidated_df[, col_1]
    
    col_2 = unlist(step[2])
    dest_val <- consolidated_df[, col_2]
    
    # store into the dataframe in the correct format
    new_data <- data.frame(Source = col_1, Dest = col_2, Freq = dest_val)
    
    output_data <- rbind(output_data, new_data)
  }
  
  output_data <- output_data %>%
    mutate(Source = str_replace_all(Source, "\\.", " ")) %>%
    mutate(Dest = str_replace_all(Dest, "\\.", " "))
  
  return(output_data)
}

####################### GEO FACET ####################################
# df_geofacet_states <- data.frame(Year=character(),
#                                  state=character(),
#                                  Crime=character(),
#                                  Number=character())
# 
# selections <- c("Andhra Pradesh" = "AP", "Jammu and Kashmir" = "JK", "Punjab" = "PB",
#                 "Himachal Pradesh" = "HP", "Haryana"= "HR", "Uttarakhand" = "UK",
#                 "Sikkim" = "SK", "Arunachal Pradesh" = "AR", "Rajasthan" = "RJ",
#                 "Uttar Pradesh" = "UP", "Bihar" = "BR", "Assam" = "AS", "Nagaland" = "NL", "Gujarat" = "GJ",
#                 "Madhya Pradesh" = "MP", "Chhattisgarh" = "CT", "Jharkhand" = "JH", "West Bengal" = "WB",
#                 "Meghalaya" = "ML", "Manipur" = "MN","Telangana" = "TG",
#                 "Odisha" = "OD", "Tripura" = "TR", "Mizoram" = "MZ", "Maharashtra" = "MH", "Goa" = "GA",
#                 "Andhra Pradesh" = "AP", "Karnataka" = "KA", "Puducherry" = "PY","Kerala" = "KL",
#                 "Tamil Nadu" = "TN"
# )
# 
# for (x in selections) {
#   indices <- which(test_df$state == x)
#   data_subset <- test_df[indices,]
# 
#   df_geofacet_states <- rbind(df_geofacet_states, data_subset)
# }
# 
# 
# gfacet <- ggplot(df_geofacet_states, aes(Crime, Number, fill=factor(Crime))) +
#   geom_col() +
#   coord_flip()
# 
# gfacet <- gfacet + geofacet::facet_geo(~state, grid="india_grid2", label=NULL)
# 
# gfacet <- gfacet + theme_bw()
#################################################################################

####################################### LAYOUT LIST #################################
m <- list(
  l = 5,
  r = 5,
  b = 10,
  t = 10,
  pad = 2
)


#UI COMPONENT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



ui <- dashboardPage(skin= "yellow",
  
  
  #HEADER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  dashboardHeader(title = "Crimes Against Women Analysis In India",
                  titleWidth = 450),
  
  #DASHBOARD SIDEBAR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName="home", icon =icon("home")),
      menuItem("Overview", tabName = "V1", icon = icon("play")),
      menuItem("State Level Analysis of Crimes", tabName = "V2", icon = icon("dice-one")),
      menuItem("Socioeconomic Factors", tabName = "V3", icon = icon("dice-two")),
      menuItem("Police Disposal of Crimes", tabName = "V4", icon = icon("dice-three"))
      #menuItem("Analysis Brief", tabName = "V5", icon = icon("dice-four"))
    )
  ),
  
  #DASHBOARD BODY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  dashboardBody(
        tabItems(
          tabItem(tabName="home",
                  fluidRow(
                    column(12,
                           
                           box(align="center", width=12,
                               
                               img(src="G9TeamLogo.png", align="center", height="13%", width="20%"),
                                
                               tags$br(),
                               h2("Description:", align="center"),
                               h3("According to the Thomson Reuters Foundation Annual Poll, India is ranked as the world's most dangerous country for women. This is not 
                            surprising, as India has had a long-standing history of violence against 
                               women, which is deeply rooted in certain cultural practices such as female infanticide and acid attacks. "),
                               
                               tags$br(),
                               
                               h2("Objectives:"),
                               h3("Our project aims to look into the trends of crimes against women in India at various geographic levels to accomplish the following"),
                               
                               tags$br(),
                               
                               h3("1) Provide an overview of the issue of crimes against women in India"),
                               
                               h3("2) Draw comparisons to study the differences in crime rates between different states"),
                               
                               h3("3) Study the effect of various socioeconomic factors on the number of crimes committed against women"),                            
                               
                               
                           ))
                  )
          ),  
            tabItem(tabName = "V1",
                    fluidRow(

                      box(
                        width = 6, height = "428px",
                        
                        valueBox(29, "States", icon = icon("landmark"), color="red"),
                        
                        valueBox("122", "Districts", icon = icon("gopuram"),color = "blue"),
                        
                        valueBox("7", "Crime Types", icon = icon("bell"), color = "yellow"),    
                        
                        tags$br(),
                        
                        h2("Brief"),
                        
                        h4("Despite India's rapid growth and development, Women in India still suffer from long-standing inequality and are victims to inhumane crimes."),
                        
                        h4('The Overview, at a glance, showcases the distribution of crimes against women in India over the years.'),
        
                        h4("The State Level Analysis dives deeper into the distribution of crimes of the various States in India."),

                        h4("The Socioeconomic Factors study the relationship between socioeconomic factors and the different types of crimes."),
                        h4("The Police Disposal of Crimes takes a look at how reported crimes have been handled by the police in 2016."),

                      ),
                      
                      box(
                        title = paste("Individual Crime Type over the Years") , status ="success", background="black", width = 6,
                        plotlyOutput(("linegraph"), height="365px")
                      )
                    ),
                    
                     fluidRow(
                       box(
                         title = "Crime over the Years", status ="success", width= 6,
                         plotOutput(outputId = "barchart", height = "350px", hover="plot_hover")
                       ),
                       box(
                         title = paste("Crime Breakdown") , status ="success", width =6,
                         plotOutput(outputId = "treemap", height = "350px")
                       )
                     )
                    
            ),
            
            tabItem(tabName ="V2",
                    fluidRow(
                        tabBox(
                          width= 12,
                          id = "tabset1", height = "700px",
                          tabPanel("Choropleth", 
                                  p("The relative crime rates of each state are shown on the choropleth map. Selecting a point on the chloropeth map will highlight the state's position on the funnel plot."), 
                                  p("Try out our filters while you're at it!"),
                                  # box(width=12, height=35, solidHeader = TRUE, title="The choropleth map shows the state level comparison of crimes against women in India. ",
                                         box( height= 80, status = "success", 
                                            h4(radioButtons( 
                                              "year", h4(" "),
                                              c("Total" = "Total", 
                                                "2015" = "2015",
                                                "2014" = "2014",
                                                "2013" = "2013",
                                                "2012" = "2012",
                                                "2011" = "2011"),
                                              "Total"
                                              ,inline= T))) ,
                                       box( height= 80, status = "success",
                                            selectInput("crimetype", h4(" "), 
                                                        choices = list(
                                                          "All" = "Total.Crimes.against.Women", 
                                                          "Rape" = "Rape", 
                                                          "Kidnapping and Abduction" = "Kidnapping.and.Abduction",
                                                          "Dowry Deaths" = "Dowry.Deaths",
                                                          "Assault on Women with Intent to Outrage Her Modesty" = "Assault.on.women.with.intent.to.outrage.her.modesty",
                                                          "Insult to Modesty of Women" = "Insult.to.modesty.of.Women",
                                                          "Cruelty by Husband or His Relatives" = "Cruelty.by.Husband.or.his.Relatives",
                                                          "Importation of Girls" = "Importation.of.Girls"
                                                        ), 
                                                        selected = "Total.Crimes.against.Women"
                                            )),

                                   #),
                                   
                                   fluidRow(
                                     box(title="State Level View of Crimes Against Women in India", status="success", 
                                         leafletOutput("choropleth", height = 350)
                                     ),
                                     
                                     box(title="Number of Crimes Against Women plotted against Population per State in 2011", status="success",
                                         plotlyOutput("funnelPlot", height = 350)
                                     )                                                #box
                                   ),                                             #fluid row
                                ),                                            #tab panel
                          
                          
                          
                          tabPanel("GeoFacet",
                                   p("The Geofacet plot will show you the breakdown of crimes in each state at a glance."),
                                  fluidRow(
                                    # box( width = 12,
                                    #   checkboxGroupInput("geofacet_country", "States to Show",  
                                    #      c("Andhra Pradesh" = "AP", "Jammu and Kashmir" = "JK", "Punjab" = "PB",
                                    #        "Himachal Pradesh" = "HP", "Haryana"= "HR", "Uttarakhand" = "UK",
                                    #        "Sikkim" = "SK", "Arunachal Pradesh" = "AR", "Rajasthan" = "RJ", 
                                    #        "Uttar Pradesh" = "UP", "Bihar" = "BR", "Assam" = "AS", "Nagaland" = "NL", "Gujarat" = "GJ",
                                    #        "Madhya Pradesh" = "MP", "Chhattisgarh" = "CT", "Jharkhand" = "JH", "West Bengal" = "WB",
                                    #        "Meghalaya" = "ML", "Manipur" = "MN","Telangana" = "TG",
                                    #        "Odisha" = "OD", "Tripura" = "TR", "Mizoram" = "MZ", "Maharashtra" = "MH", "Goa" = "GA",
                                    #        "Andhra Pradesh" = "AP", "Karnataka" = "KA", "Puducherry" = "PY","Kerala" = "KL",
                                    #        "Tamil Nadu" = "TN"
                                    #        
                                    #        ), inline = TRUE, selected = c("AP","JK", "PB","HP","HR","UK","SK","AR","RJ","UP","BR","AS","NL","GJ",
                                    #                                       "MP","CT","JH","WB","ML","MN","TG","OD","TR","MZ","MH","GA","AP","KA"
                                    #                                       ,"PY","KL","TN")
                                    # )),
                                     
                                     box(status="success", height=500, width=12,title="Geofacet by States",
                                        plotOutput(outputId = "geofacet", height = 600)
                                    ),
                                    
                                  )
              
                           )                                    #tabpanel
                        
                    )
                    
            )
            ),                           #end of V2
            
            tabItem(tabName ="V3",
              fluidRow(
                column(6, align="center",
                       selectInput("boxplot", label = "Display boxplot?",
                                   choices = c("YES","NO"),
                                   selected = "YES")),
                column(6, align="center",
                       selectInput("var",label = "Choose a State to display",
                                   choices = c("VIEW ALL","ANDAMAN AND NICOBAR ISLANDS","ANDHRA PRADESH","ARUNACHAL PRADESH",
                                               "ASSAM","BIHAR","CHANDIGARH","CHHATTISGARH","DAMAN AND DIU",
                                               "DELHI","GOA","GUJARAT","HARYANA","HIMACHAL PRADESH","JAMMU AND KASHMIR","JHARKHAND",
                                               "KARNATAKA","KERALA","LAKSHADWEEP","MADHYA PRADESH","MAHARASHTRA","MANIPUR","MEGHALAYA",
                                               "MIZORAM","NAGALAND","ODISHA","PUDUCHERRY","PUNJAB","RAJASTHAN","SIKKIM","TAMIL NADU",
                                               "TRIPURA","UTTAR PRADESH","UTTARAKHAND","WEST BENGAL"),
                                   selected = "VIEW ALL"))
              ),
              p("The Parallel Coordinate Plot shows the relationships between the various socioeconomic factors across states."),
              p("Toggle the boxplot filter to remove the boxplot, and toggle the filter for states to highlight states that you are intereted in."),
              fluidRow(
                box( width = 12, title="Socioeconomic factors for States in India based on 2011 Census Data",
                  plotlyOutput("plot1"),
                  p("Tip: Toggle the legends to hide/show points corresponding to the percentile of total crimes!")
                )
              ),
              
              fluidRow(
                box( width = 6, 
                     title="Correlation Heatmap between Crime Types and Socioeconomic Factors",
                     
                  plotlyOutput("plot2"),
                  p("Tip: Click on the correlation heatmap to populate the scatterplot!")
                ),
                box( width = 6, height="494px",
                     title="Scatterplot showing variables selected from Correlation Heatmap",
                  plotlyOutput("plot3")
                )
              )
              

            ),
            
            tabItem(tabName ="V4",
                    
              # fluidRow(
              #   box(
              #     position="right",
              #     selectInput("crimetype", h4("Crime Type"),
              #                 choices = list(
              #                   "All" = "Total.Crimes.against.Women",
              #                   "Rape" = "Rape",
              #                   "Kidnapping and Abduction" = "Kidnapping.and.Abduction",
              #                   "Dowry Deaths" = "Dowry.Deaths",
              #                   "Assault on Women with Intent to Outrage Her Modesty" = "Assault.on.women.with.intent.to.outrage.her.modesty",
              #                   "Insult to Modesty of Women" = "Insult.to.modesty.of.Women",
              #                   "Cruelty by Husband or His Relatives" = "Cruelty.by.Husband.or.his.Relatives",
              #                   "Importation of Girls" = "Importation.of.Girls"),
              #                 selected = "Total.Crimes.against.Women")
              #   )
              # ),
              
              p("The Sankey Diagram visualises how reported crimes against women have been handled by the police."),
              fluidRow(
                box( title="Police Disposal of Crimes Against Women", width = 12,
                     sankeyNetworkOutput("sankeyPlot")
                )
              ))
            # ),
            # tabItem(tabName ="V5",
            #         h2("Analysis Brief is here")
            # )
        )
  )
  
)




l <- list(
  font = list(
    family = "sans-serif",
    size = 12,
    color = "#000"),
  bgcolor = "#E2E2E2",
  bordercolor = "#FFFFFF",
  borderwidth = 2)



server <- function(input, output) {
  
  
  
  id <- reactive({
    p <- input$choropleth_shape_click    
    p$id
  })
  
  #Limzy !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  output$barchart <- renderPlot(bar_chart_total_crimes)

  output$linegraph <- renderPlotly({
   
    plot_ly(years_total_df, x=~Year, y=~Crime, type="scatter", mode="lines" , name='Total Crimes') %>%
      add_trace(y = ~Rape, name = 'Rape') %>%
      add_trace(y = ~Kidnap, name = 'Kidnap') %>%
      add_trace(y = ~Dowry, name = 'Dowry Death') %>%
      add_trace(y = ~Assault, name = 'Assault') %>%
      add_trace(y = ~Insult, name = 'Insult to Modesty') %>%
      add_trace(y = ~Cruelty, name = 'Cruelty to Women') %>%
      add_trace(y = ~Importation, name = 'Importation of Girls') %>%
      layout(margin = list(l=25, r=20, b=5, t=10), legend = list(orientation = 'h', 
                                                                 xanchor="center",
                                                                 x=0.5,
                                                                 y=-0.2,
                                                                 font = list(
        family = "sans-serif",
        size = 9.55,
        color = "#000")))
  })
  
  output$treemap <- renderPlot({
    hover <- input$plot_hover
    
      if (is.null(hover)) {
          #print("here")
          total_year <- yearly_crime_df %>% group_by(`Crime`) %>% summarize_all(funs(sum), na.rm=TRUE)
          #print(crime_total_df)
          print(total_year)
          ggplot(total_year, aes(fill= `Crime`, area=`Number`)) + 
            geom_treemap() +
            geom_treemap_text(colour = "white", place="centre", label=paste(total_year$Crime,": ",total_year$Number)) +
            labs(title=paste("Aggregated Crime Distribution for 2011-2015")) +
            theme(legend.position="right")  +
            scale_fill_brewer(palette="Dark2")
        }
    
      else {
          hover <- hover[[1]][1]
          if (hover < 2015.5) {
            year <- round(hover)
          } else {
            year <- 2015
          }
          yearly_subset <- filter(yearly_crime_df, Year == year)
          ggplot(yearly_subset, aes(fill= Crime, area=Number)) +
              geom_treemap() +
              geom_treemap_text(colour = "white", place="centre", label=paste(yearly_subset$Crime,": ",yearly_subset$Number)) +
              labs(title=paste("Crime Distribution for ", year)) +
              theme(legend.position="right") + 
            scale_fill_brewer(palette="Dark2")
      }
  })


  
#cheng!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
  tmap_mode("view")
  
  output$choropleth = renderLeaflet({
    year_chosen <- input$year
    # Filter data by year
    if (year_chosen != "Total") {
      choro_data <- choro_data %>%
        select(everything()) %>%
        dplyr::filter(Year == year_chosen)
    } else {
      choro_data <- choro_data %>%
        group_by(NAME_1) %>%
        summarise_if(is.numeric, funs(sum))
    }
    
    # Display data according to selected Crime Type
    data <- input$crimetype
    
    tm <- tm_shape(choro_data)+
      tm_fill(data,
              n = 6,
              style = "quantile",
              palette = "Reds",
              id="NAME_1",
              title="Number of Cases",
              popup.vars=c("Number of Cases" = data)) +
      tm_borders(alpha = 0.5)
    tmap_leaflet(tm)
  })
  
  ### Funnel Plot
  output$funnelPlot <- renderPlotly({
    crime_type <- input$crimetype
    year_chosen <- input$year
    
    #filter funnel_data by year
    if (year_chosen != "Total") {
      funnel_data <- funnel_data %>%
        select(everything()) %>%
        dplyr::filter(Year == year_chosen)
    } else {
      funnel_data <- funnel_data %>%
        group_by(match) %>%
        summarise_if(is.numeric, funs(sum))
    }
    
    funnel_data <- as.data.frame(funnel_data)
    
    y <- funnel_data[, crime_type]
    x <- funnel_data$Population
    
    df <- data.frame(x=x, y=y)
    df <- na.omit(df)
    m <- lm(y ~ x, data=df) 
    
    fit95 <- predict(m, interval="conf", level=.95)
    fit99 <- predict(m, interval="conf", level=.999)
    
    df <- cbind.data.frame(df, 
                           lwr95=fit95[,"lwr"],  upr95=fit95[,"upr"],     
                           lwr99=fit99[,"lwr"],  upr99=fit99[,"upr"])
    
    # merge df states with the state stats on x and y column
     df <- inner_join(df, funnel_data, by = c("x" = "Population", "y" = crime_type))
    
     if (!is.null(id())) {
       output_data <- gsub('\\.', " ", id())
       
       print(output_data)
       #print(df)
       df_one_point <- df %>% dplyr::filter(match ==output_data)
       print("test")
       print(df_one_point)
       #print(df_one_point)
      
       validate(
         need(nrow(df_one_point) != 0, "No crime data available for selected state. Please select another state. Thank you!")
       )
       
       p <- ggplotly(ggplot(df, aes(x, y)) +
                       geom_point(aes(text=sprintf("State: %s<br>Population: %s<br>Cases: %s", df$match, df$x, df$y))) + 
                       geom_point(aes(df_one_point$x, df_one_point$y, text=sprintf("State: %s<br>Population: %s<br>Cases: %s", df_one_point$match, df_one_point$x, df_one_point$y)),color="orange") + 
                       geom_text(aes(label=ifelse(df$`match` == output_data,as.character(`match`),''), fontface=2),position=position_nudge(y=4000), size=2, color="orange")+
                       geom_smooth(method="lm", colour="black", lwd=1.1, se=FALSE) + 
                       geom_line(aes(y = upr95), color="black", linetype=2) + 
                       geom_line(aes(y = lwr95), color="black", linetype=2) +
                       geom_line(aes(y = upr99), color="red", linetype=3) + 
                       geom_line(aes(y = lwr99), color="red", linetype=3)  +
                       geom_text(aes(x = 950000000, y= 240000, label="99.9% C.I."),size=2.5,colour="red") +
                       geom_text(aes(x = 950000000, y = 190000, label = "95% C.I."),size=2.5, colour="black") +
                       #scale_color_manual(values=c("upr95"="black", "lwr99"="red")) +
                       #scale_x_continuous(expand = c(.2, .2)) +
                       #geom_dl(aes(label=upr99), method="last.points") +
                       #geom_dl(aes(label=upr95), method="last.points") +
                       labs(x="Population per State", y="Number of Cases") +    
                       theme_bw(), tooltip="text"
       )
       
     } else {
       
       p <- ggplotly(ggplot(df, aes(x, y)) +
                       geom_point(aes(text=sprintf("State: %s<br>Population: %s<br>Cases: %s", df$match, df$x, df$y))) + 
                       geom_smooth(method="lm", colour="black", lwd=1.1, se=FALSE) + 
                       geom_line(aes(y = upr95), color="black", linetype=2) + 
                       geom_line(aes(y = lwr95), color="black", linetype=2) +
                       geom_line(aes(y = upr99), color="red", linetype=3) + 
                       geom_line(aes(y = lwr99), color="red", linetype=3)  + 
                       geom_text(aes(x = 950000000, y= 240000, label="99.9% C.I."),size=2.5,colour="red") +
                       geom_text(aes(x = 950000000, y = 190000, label = "95% C.I."),size=2.5, colour="black") +
                       #geom_dl(aes(label=y), method="last.points") +
                       #geom_dl(aes(label=upr95), method="last.points") +
                       #scale_color_manual(values=c("upr95"="black", "lwr99"="red")) +
                       #scale_x_continuous(expand = c(.2, .2)) +
                       labs(x="Population per State", y="Number of Cases") +    
                       theme_bw(), tooltip="text"
       )
       
     }
  })
  

  
  output$geofacet <- renderPlot({
    
    gfacet
    
  })
  
  
  
  #SEAN CHAI PART!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  color_filter <- reactive({
    if (input$var == "VIEW ALL"){
      return ("c('orange','blue','brown','red')")
    } else {
      return ("c('0-25th Percentile'='grey',
                    '25th-50th Percentile'='grey',
                    '50th-75th Percentile'='grey',
                    '75th-100th Percentile'='grey',
                    '1'='darkblue',
                    '2'='black',
                    '3'='grey')")
    }
  })
  
  filtered_data <- reactive({
    if (input$var == "VIEW ALL"){
      return ("Percentile.of.Total.Crime")
    } else {
      input_var <- input$var
      input_var <- gsub(" ",".",input_var)
      #print(input_var)
      return (input_var)
      #dplyr::filter(combined_2011, `STATE/UT` == input$var) #| `STATE/UT` == "INDIA AVERAGE")
    }
  })
  
  filtered_data_scatter <- reactive({
    if (input$var == "VIEW ALL"){
      #print(input$var)
      return ()
    } else {
      input_var <- input$var
      input_var <- gsub(" ",".",input_var)
      #print(input_var)
      return (input_var)
      #dplyr::filter(combined_2011, `STATE/UT` == input$var) #| `STATE/UT` == "INDIA AVERAGE")
    }
  })
  
  scatter_label <- reactive({
    if (input$var == "VIEW ALL"){
      #print(input$var)
      return (FALSE)
    } else {
      return (TRUE)
    }
  })
  
  box_plot <- reactive({
    if (input$boxplot == "YES"){
      return (TRUE)
    } else {
      return (FALSE)
    }
  })
  
  corr_matrix <- reactive({
    #colnames(state_2011_rename) <- c("STATE/UT", "rape", "kidnap.abduct", "dowry.deaths", "assault.on.women", "insult.to.modesty", "cruelty.by.husband",
    #                                 "importation.girls","Total Crimes against Women","population", "avg.household.size", "females.per.males", "literacy rates", "higher.sec.grads", "persons.aged.15-59", "non-workers")
    if (input$var == "VIEW ALL"){
      df2 <- state_2011[, 2:8]
      df1 <- state_2011[, 10:16]
      cor_mat <- cor(df1,df2, use="na.or.complete", method="pearson")
      cor_mat <- reorder_cormat(cor_mat)
      melted_cormat <- melt(cor_mat)
      
      y_list <- unique(as.character(melted_cormat[["Var2"]]))
      x_list <- unique(as.character(melted_cormat[["Var1"]]))
      #y_list <- rename_y(y_list)
      #x_list <- rename_x(x_list)
      y_list <- to_strlist(y_list)
      x_list <- to_strlist(x_list)
      
      corr_matrix <- list(melted_cormat = melted_cormat, y_list = y_list, x_list = x_list)
      
      return (corr_matrix)
    } else {
     #colnames(district_2011_rename) <- c("STATE/UT","DISTRICT","rape", "kidnap.abduct", "dowry.deaths", "assault.on.women", "insult.to.modesty", "cruelty.by.husband",
     #                                     "importation.girls","Total Crimes against Women","population", "avg.household.size", "females.per.males", "literacy rates", "higher.sec.grads", "persons.aged.15-59", "non-workers")
      filter <- filter(district_2011, district_2011$`STATE/UT` == input$var)
      if (nrow(filter) <= 1){
        return (FALSE)
      }
      df2 <- filter[, 3:9]
      df1 <- filter[, 11:17]
      cor_mat <- cor(df1,df2, use="na.or.complete", method="pearson")
      cor_mat[is.na(cor_mat)] = 0
      cor_mat <- reorder_cormat(cor_mat)
      melted_cormat <- melt(cor_mat)
      y_list <- unique(as.character(melted_cormat[["Var2"]]))
      x_list <- unique(as.character(melted_cormat[["Var1"]]))
      #y_list <- rename_y(y_list)
      #x_list <- rename_x(x_list)
      y_list <- to_strlist(y_list)
      x_list <- to_strlist(x_list)
      
      corr_matrix <- list(melted_cormat = melted_cormat, y_list = y_list, x_list = x_list)
      
      return (corr_matrix)
    }
  })
  
  output$plot1 <- renderPlotly({
    g <- GGally::ggparcoord(state_2011,
                    columns = 10:16, 
                    groupColumn = "Percentile.of.Total.Crime",
                    scale="uniminmax",
                    scaleSummary="NULL",
                    missing = "exclude",
                    centerObsID = 1,
                    showPoints = FALSE, 
                    splineFactor = FALSE,
                    alphaLines = 0.2,
                    boxplot = box_plot(), 
                    shadeBox = NULL,
                    mapping = aes( text = paste('Crimes against women in State:', `Total Crimes against Women`,
                                                '<br>Percentile of Total Crime: ', r_percentile(Percentile.of.Total.Crime, state_2011),
                                                '<br>Variable:', variable, 
                                                '<br>State/UT:', state_ut(`STATE/UT`, state_2011)))) 
    g <- g + theme_bw() + 
      theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
            legend.title=element_blank())+
      scale_color_manual(values=(eval(parse(text=color_filter())))) 
    #scale_color_viridis(discrete=TRUE)
    g <- g + scale_x_discrete(
      labels = addline_format(c("Population Size",
                                #"Police Strength",
                                "Average Household Size",
                                "Females per Male",
                                "Literacy Rates (%)", 
                                "Percent of Higher Sec Graduates (%)",
                                "Percent of Persons aged 15-59 (%)",
                                "Percent of Non-Workers (%)"
      ))) 
    g <- g + geom_point(size = 1) 
    
    if(input$var != "VIEW ALL"){
      g <- g + 
        geom_point(data=filter(g[["data"]], g[["data"]]$`STATE/UT`==to_numeric(input$var,state_2011)), aes(x=variable, y=value), colour="darkblue")
      g <- g + geom_line(data=filter(g[["data"]], g[["data"]]$`STATE/UT`==to_numeric(input$var,state_2011)), aes(x=variable, y=value), colour="darkblue")
    }
    #g <- g + geom_point(data = g[["data"]], aes(x=1), )
    #+ geom_point(size = 1, colour=color_map(eval(parse(text=filtered_data())))))
    
    g <- ggplotly(g , tooltip="text") %>%
      layout(margin = m, xaxis = list(autorange=TRUE), yaxis = list(autorange=TRUE),
        legend = list( font = list(
          family = "sans-serif",
          size = 15,
          color = "#000"),
          orientation = "h",
          x = 3.5,
          y = -0.3
        ))
    g
  })
  
  output$plot2 <- renderPlotly({
    
    validate(
      need(input$var != "CHANDIGARH", "Correlation plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "LAKSHADWEEP", "Correlation plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "MEGHALAYA", "Correlation plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "PUDUCHERRY", "Correlation plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "TRIPURA", "Correlation plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      
      need(input$var != "ANDAMAN AND NICOBAR ISLANDS", "Correlation plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "DAMAN AND DIU", "Correlation plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "GOA", "Correlation plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!")
    )
    
    corr_matrix <- corr_matrix()
    
    cp <- ggplot(data = corr_matrix$melted_cormat, aes(x=Var1,y=Var2,fill=value, 
                                           text = paste('Socioeconomic Factor(x-axis):', Var1, 
                                                        '<br>Crime Type(y-axis):', Var2,
                                                        '<br>Correlation Value:', round(value,2))))
    cp <- cp + geom_tile() + 
      scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") 
    cp <- cp + theme_minimal()+ 
      theme(axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 1, size = 8.8, hjust = 1))+
      theme(axis.title.y=element_blank(),axis.text.y = element_text(vjust = 1, size = 8.5, hjust = 1))+
      coord_fixed() + 
      geom_text(aes(Var1, Var2, label = round(value,2)), size = 2.5) 
    
    #y_list <- unique(as.character(cp[["data"]][["Var2"]]))
    #x_list <- unique(as.character(cp[["data"]][["Var1"]]))
    #y_list <- rename_y(y_list)
    #x_list <- rename_x(x_list)
    #y_list <- to_strlist(y_list)
    #x_list <- to_strlist(x_list)
    
    y_list <- corr_matrix$y_list
    x_list <- corr_matrix$x_list
    y_list <- rename_y(y_list)
    x_list <- rename_x(x_list)
    
    cp <- cp + scale_y_discrete( 
      labels = addline_format(c(y_list)))
    
    cp <- cp + scale_x_discrete(
      labels = addline_format(c(x_list)))
    
    cp <- ggplotly(cp, tooltip="text", source="corr",) %>% 
      layout(margin = m, xaxis = list(autorange=TRUE), yaxis = list(autorange=TRUE))
    
    cp
  })
  
  
  
  output$plot3 <- renderPlotly({
    #x_list <- list("Average Household Size", "Percent of Non-Workers (%)", "Population", "Females per Male", "Literacy Rates (%)", "Higher Secondary Graduates (%)","Persons aged 15-59 (%)")
    #y_list <- list("Kidnapping and Abduction","Importation of Girls", "Rape", "Dowry Deaths", "Assault on women with intent to outrage her modesty","Insult to modesty of Women", "Cruelty by Husband or his Relatives")
    corr_matrix <- corr_matrix()
    x_list <- corr_matrix$x_list
    y_list <- corr_matrix$y_list
    
    #print((x_list[1]))
    #print(y_list[1])
    
    #x_list <- as.list(levels(x_list))
    #y_list <- as.list(levels(y_list))
    
    
    
    x1 <- as.numeric(event_data("plotly_click", source="corr")[3])
    y1 <- as.numeric(event_data("plotly_click", source="corr")[4])

    x1 <- as.character(x_list[x1])
    y1 <- as.character(y_list[y1])

    
    #filter_color <- filtered_data_scatter()
    validate(
      #need(input$var != "VIEW ALL", "Please select a state to display scatter plot."),
      need(x1 != 0, "Please select a set of variables by clicking on the correlation heatmap"),
      need(input$var != "CHANDIGARH", "Scatter plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "LAKSHADWEEP", "Scatter plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "MEGHALAYA", "Scatter plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "PUDUCHERRY", "Scatter plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "TRIPURA", "Scatter plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "ANDAMAN AND NICOBAR ISLANDS", "Scatter plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "DAMAN AND DIU", "Scatter plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!"),
      need(input$var != "GOA", "Scatter plot could not be generated due to limited data collected from districts. Please select another state that has sufficient data. Thank you!")
    )
    
    india <- filter(state_2011, `STATE/UT` == "INDIA AVERAGE")
    
    if (input$var == "VIEW ALL"){
      cor_data <- state_2011
      ggplotly(ggplot(cor_data, aes(x=cor_data[,x1], y=cor_data[,y1], 
                                    color=`STATE/UT`, 
                                    text = paste( 'Socioeconomic Factor:', x1,
                                                  '<br>Crime Type:', y1,
                                                  '<br>State/UT:', `STATE/UT`))) + 
                 geom_point() + scale_fill_hue(l=40,c=35) +
                 #geom_smooth(method="auto", color="black", fill="white",fullrange=TRUE, se=TRUE) +
                 theme( legend.title = element_blank()) + 
                 xlab(x1) + ylab(y1) 
               + geom_point(data=india, aes(x=india[,x1], y=india[,y1]), color="black") 
               + geom_hline(aes(yintercept = mean(india[,y1])))+
                 geom_vline(aes(xintercept = mean(india[,x1])))
               , 
               tooltip="text") %>%
        layout (margin = m, xaxis = list(autorange=TRUE), yaxis = list(autorange=TRUE),
          showlegend=FALSE, legend = list(font = list(size = 3), orientation = "h")
        )
      
    } else {
      cor_data <- filter(district_2011, `STATE/UT` == input$var)
      input_var <- input$var
      state_name <-  paste(input_var, "AVERAGE", sep=" ")
      temp <- data.frame(t(colMeans(cor_data[,3:length(cor_data)], na.rm="True")))
      temp <- cbind(`DISTRICT`=state_name, temp)
      temp <- cbind(`STATE/UT`=input$var, temp)
      colnames(temp) <- colnames(cor_data)
      ggplotly(ggplot(cor_data, aes(x=cor_data[,x1], y=cor_data[,y1], 
                                    color=`DISTRICT`, 
                                    text = paste( 'Socioeconomic Factor:', x1, 
                                                  '<br>Crime Type:', y1,
                                                  '<br>State/UT:', `STATE/UT`,
                                                  '<br>District:',`DISTRICT`))) + 
                 geom_point() + scale_fill_hue(l=40,c=35) +
                 #geom_smooth(method="auto", color="black", fill="white",fullrange=TRUE, se=TRUE) +
                 theme( legend.title = element_blank() ) + 
                 xlab(x1) + ylab(y1)
               + geom_point(data=temp, aes(x=temp[,x1], y=temp[,y1]), color="black") 
               + geom_hline(aes(yintercept = mean(temp[,y1])))+
                 geom_vline(aes(xintercept = mean(temp[,x1]))), 
               tooltip="text") %>%
        layout (margin = m, xaxis = list(autorange=TRUE), yaxis = list(autorange=TRUE),
          showlegend=FALSE, legend = list(font = list(size = 3), orientation = "h")
        )
    }
    
  }) 
  
  #DASHBOARD 4 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!!!
  output$sankeyPlot <- renderSankeyNetwork({
    output_data <- create_freq(court_police)
    sankey_from_data_frame <- function(data, val_col) {
      weight <- enquo(val_col)
      
      df <- 1:(ncol(data)-2) %>% 
        # collapse dataframe into 3 columns: from, to, weight
        map_df(~ dplyr::select(data, 
                        from = !! quo(names(data)[.x]), 
                        to = !! quo(names(data)[.x + 1]), 
                        !! weight)) %>% 
        drop_na() %>% 
        group_by(from, to) %>% 
        dplyr::summarise(weight = sum(!! weight)) %>% 
        mutate(colour = from) # used to colour edges in the sankeyNetwork function (optional)
      
      # create tidygraph from data frame
      ig <- tidygraph::as_tbl_graph(df)
      
      # extract node data and reduce ids by 1 (required for D3 plotting)
      nodes <- as_tibble(ig) %>% 
        rowid_to_column("id") %>% 
        mutate(id = id -1) %>% 
        as.data.frame
      
      # do the same for the edges
      edges <- ig %>% 
        activate(edges) %>% 
        as_tibble() %>% 
        mutate(from = from - 1, to = to - 1) %>% 
        as.data.frame
      
      
      sankeyNetwork(Links = edges, Nodes = nodes, Source = "from", Target = "to", 
                    NodeID = "name", Value = "weight", LinkGroup = "colour", #(use if you want to colour edges)
                    fontSize = 12, fontFamily = "sans-serif", width = "100%")
    }
    sankey_from_data_frame(data = output_data, val_col = Freq)
  })
  
  
  
} #end of server


shinyApp(ui = ui, server = server)