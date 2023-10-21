
df <- read.csv("healthy lifestyle city 2021.csv")

#Changing the attributes names

colnames(df)[1] <- "City" #Cities
colnames(df)[2] <- "Position" # Posição de cada uma
colnames(df)[3] <- "Sun" #Annual average sun exposure 
colnames(df)[4] <- "Water" #Price of a water bottle 300ml 
colnames(df)[5] <- "Obesity" #Average by country
colnames(df)[6] <- "Life" # Life Expectancy in years
colnames(df)[7] <- "Pollution" #Index of pollution by city
colnames(df)[8] <- "HoursWorked" #Average hours worked by year
colnames(df)[9] <- "Happiness" #Index by country 
colnames(df)[10] <- "Activities" #By cities 
colnames(df)[11] <- "Restaurants"#By cities
colnames(df)[12] <- "Gym" #By cities 

#Changing the order of the columns - "Position","City" 
df <- df[c("Position","City","Sun", "Water", "Obesity","Life",
           "Pollution","HoursWorked","Happiness","Activities",
           "Restaurants","Gym")]

#Removing pound symbol out of the attributes

df$Water<- gsub("£","", df$Water)
df$Gym <- gsub("£","", df$Gym)

#Removing symbol % Obesity
df$Obesity <- gsub("%","", df$Obesity)

#Populating missing values

#1° Fukuoka pollution index - according to the source, the closest place
# is Osaka 53.24 - but https://aqicn.org/city/fukuoka/ shows somewhere between 54 and 57
# so I'll use the source data of osaka 53.24

#2° Geneva Hours of sunshine- in the database reference I found the value 1.887
#https://en.wikipedia.org/wiki/List_of_cities_by_sunshine_duration

#3° for the variable Average hours worked per year - there are 11 missing values
# I couldn't find the data in the database source
# changed cities - Johannesburg - Sao Paulo - Shanghai - Hong Kong - Mumbai - Taipei
# Cairo - Jakarta - Buenos Aires - Bangkok 

# Average hours worked per year per country. 
#https://worldpopulationreview.com/country-rankings/average-work-week-by-country 
#52 weeks in the year
#52 * 41.70 = 2168 china = beijing = Shanghai
#52 * 40.2 = 2090 thailand = Bangkok  
#52 * 30.90 = 1606 argentina = Buenos Aires
#52 * 38.80 = 2018 indonesia = Jakarta 
#egito = cairo = missing copying data from south africa
#52 * 40.10 = 2085 = Taipei = taiwan''''''
#52 * 40.80 = 2122 = Mumbai - india
#52 * 41.3 = 2148 = hong kong
#52 * 41.70 = 2168 china = beijing = shanghai
#52 * 32.8 = 1706 = brazil = sp
#52 * 42.1 = 2189 = Johannesburg = south africa

df$Pollution[7] = 53.24
df$Sun[20] = 1887

df$HoursWorked[12] = 2168
df$HoursWorked[13] = 2090
df$HoursWorked[14] = 1606
df$HoursWorked[17] = 2018
df$HoursWorked[23] = 2189
df$HoursWorked[24] = 2085
df$HoursWorked[26] = 2122
df$HoursWorked[31] = 2148
df$HoursWorked[32] = 2168
df$HoursWorked[36] = 1706
df$HoursWorked[39] = 2189

#Changing attributes to numeric 

df$Water <- as.numeric(df$Water)
df$Gym <- as.numeric(df$Gym)
df$Pollution <- as.numeric(df$Pollution)
df$Sun <- as.numeric(df$Sun)
df$Obesity <- as.numeric(df$Obesity)
df$HoursWorked <- as.numeric(df$HoursWorked)

#changing from Pound to US Dollar  
df$Water <- round(df$Water * 1.3, digits = 2)
df$Gym <- round(df$Gym * 1.3, digits = 2)

library(dplyr)
head(df, 5)
glimpse(df, 5)
