library(janitor)
library(tidyverse)
library(haven)
library(leaps)
library(lme4)

# Load raw data
raw_data <- read_dta(file = "C:/Users/zzpen/Documents/OneDrive - University of Toronto/UofT/Study Material/2021/Fall/STA304/Final Project/Input/data/2019 Canadian Election Study - Online Survey v1.0.dta")

# Remove post-election data
cps_data <- select(raw_data, -contains("pes19_"))
cps_data <- select(cps_data, -contains("split"))

# Remove over 50% missing value variable
cps <- cps_data[, colSums(is.na(cps_data)) <= 37822/2]

# Create age group
# Keep only eligible voters
cps <- subset(cps, cps19_age>17)
cps$age_under_30 <- NA
cps<-
  cps %>%
  mutate(age_under_30 = 
           ifelse(cps$cps19_age < 31, 1, 0)
         )
cps <- subset(cps, select = -c(cps19_age))

# Remove Don't know and another party
# hot-encode the result
cps <- subset(cps, cps19_votechoice!=7)
cps <- subset(cps, cps19_votechoice!=9)
cps$party_liberal <- NA
cps$party_conservative <- NA
cps$party_ndp <- NA
cps$party_bloc <- NA
cps$party_green <- NA
cps$party_people <- NA
cps<-
  cps %>%
  mutate(party_liberal = 
           ifelse(cps19_votechoice == 1, 1, 0),
         party_conservative = 
           ifelse(cps19_votechoice == 2, 1, 0),
         party_ndp = 
           ifelse(cps19_votechoice == 3, 1, 0),
         party_bloc = 
           ifelse(cps19_votechoice == 4, 1, 0),
         party_green = 
           ifelse(cps19_votechoice == 5, 1, 0),
         party_people = 
           ifelse(cps19_votechoice == 6, 1, 0))
cps <- subset(cps, select = -c(cps19_votechoice))

# Remove all Don't know
cps <- subset(cps, cps19_gender!=3)
cps <- subset(cps, cps19_bornin_canada!=3)
cps <- subset(cps, cps19_children!=3)
cps <- subset(cps, cps19_education!=12)

# Remove all territories
cps <- subset(cps, cps19_province!=19)
cps <- subset(cps, cps19_province!=21)
cps <- subset(cps, cps19_province!=26)

# Hot-encode variables
cps$male <- NA
cps$have_children <- NA
cps$bornin_canada <- NA
cps<-
  cps %>%
  mutate(male = 
           ifelse(cps19_gender == 1, 1, 0),
         have_children = 
           ifelse(cps19_children == 1, 1, 0),
         bornin_canada = 
           ifelse(cps19_bornin_canada == 1, 1, 0)
  )
cps <- subset(cps, select = -c(cps19_gender, cps19_children, cps19_bornin_canada))

# Split the education into 2 groups
cps$bachelor <- NA
cps<-
  cps %>%
  mutate(bachelor = 
           ifelse(cps19_education == 9 | cps19_education == 10 | cps19_education == 11, 1, 0))
cps <- subset(cps, select = -c(cps19_education))

# Only keep the interested variables
# regfit_vote=regsubsets(party_liberal ~ ., cps, really.big=T)
# summary(regfit_vote)
cps <- cps %>%
  select(male,
         cps19_province,
         bachelor,
         bornin_canada,
         have_children,
         age_under_30,
         party_liberal,
         party_conservative,
         party_ndp,
         party_bloc,
         party_green,
         party_people
  )

cps <- na.omit(cps)

# Keep the province name the same as census data
cps$cps19_province <- as.character(cps$cps19_province)
cps[cps == 14] <- "Alberta"
cps[cps == 15] <- "British Columbia"
cps[cps == 16] <- "Manitoba"
cps[cps == 17] <- "New Brunswick"
cps[cps == 18] <- "Newfoundland and Labrador"
cps[cps == 20] <- "Nova Scotia"
cps[cps == 22] <- "Ontario"
cps[cps == 23] <- "Prince Edward Island"
cps[cps == 24] <- "Quebec"
cps[cps == 25] <- "Saskatchewan"

cps <- cps %>%
  rename(
    province = cps19_province
  )

write_csv(cps, "cps.csv")
