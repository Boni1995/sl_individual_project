# Libraries
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
library(ggplot2)
library(rnaturalearth)
library(gridExtra)
library(hrbrthemes)
library(viridis)
library(factoextra)
library(ggthemes)
library(proxy)
library(cluster)
library(clValid)
library(corrplot)
library(ggcorrplot)
library(car)
library(ggpubr)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(outliers)
library(ranger)

#Load datasets
death_cause <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\annual-number-of-deaths-by-cause.csv")
population <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\population.csv")
n_cancer <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\number-of-people-with-cancer.csv")
d_cancer_type <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\total-cancer-deaths-by-type.csv")
d_cancer_age <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\cancer-deaths-by-age.csv")
health_spe <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\public-healthcare-spending-share-gdp.csv")
hd_index <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\human-development-index.csv")
ideology <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\global_leader_ideologies.csv")

# Data cleaning and manipulation

#Change column names
colnames(death_cause) <- c("entity", "code", "year", "meningitis", "dementia", "parkinson", "nutri_deficiencies",
                        "malaria", "drowning", "homicide", "maternal_disorders", "hiv", "drug_use_disorders",
                        "tuberculosis", "cardiovascular_diseases", "respiratory_infections", "neonatal_disorders",
                        "alcohol_use_disorders", "suicide", "natural_disasters", "diarrheal_diseases",
                        "heat", "cancer", "terrorism", "diabetes", "kidney_diseases", "poisonings",
                        "protein_energy_malnutri", "road_injuries", "respiratory_diseases", "liver_diseases",
                        "digestive_diseases", "fire", "hepatitis", "measles")

colnames(population) <- c("entity", "code", "year", "population")

colnames(n_cancer) <- c("entity", "code", "year", "n_people")

colnames(d_cancer_type) <- c("entity", "code", "year", "liver", "kidney", "lip_oral_cavity",
                             "tracheal_bronch_lung", "larynx", "gallblad_biliary", "skin", "leukemia",
                             "hodgkin", "myeloma", "others", "breast", "prostate", "thyroid", "stomach",
                             "bladder", "uterine", "ovarian", "cervical", "brain_cns", "non_hodgkin",
                             "pancreatic", "esophageal", "testicular", "nasopharynx", "other_pharynx",
                             "colon_rectum", "non_melanoma_skin", "mesothelioma")

colnames(d_cancer_age) <- c("entity", "code", "year", "age70_over", "age50_69", "age15_49", "age5_14", "under_5")

colnames(health_spe) <- c("entity", "code", "year", "public_health_spe")

colnames(hd_index) <- c("entity", "code", "year", "hdi")

ideology <- ideology %>% 
  rename(entity = country_name)

# Define years to use
define_year <- data.frame(
  Dataset = c("death_cause","population","n_cancer", "d_cancer_type", "d_cancer_age", "health_spe", "hd_index", "ideology"),
  Min = c(
    min(death_cause$year),
    min(population$year),
    min(n_cancer$year),
    min(d_cancer_type$year),
    min(d_cancer_age$year),
    min(health_spe$year),
    min(hd_index$year),
    min(ideology$year)
  ),
  Max = c(
    max(death_cause$year),
    max(population$year),
    max(n_cancer$year),
    max(d_cancer_type$year),
    max(d_cancer_age$year),
    max(health_spe$year),
    max(hd_index$year),
    max(ideology$year)
  )
)

define_year # I will use data from 2000 to 2017

#Check country name and code
country_name <- union(union(union(union(union(union(union(select(death_cause, entity), select(population, entity)),
                                                    select(n_cancer, entity)), select(d_cancer_type, entity)),
                                        select(d_cancer_age, entity)), select(health_spe, entity)),
                            select(hd_index, entity)), select(ideology, entity))

distinct(country_name)

country_name_code <- left_join(country_name, d_cancer_type, by = "entity") %>%
  mutate(code = ifelse(is.na(code), 0, code)) %>%
  select(entity, code)

country_name_fv <- distinct(country_name_code) # This is the final index

code_na <-  subset(country_name_fv, code == 0)
code_na # Checked in excel which are errors and need to be adjusted

# Correction of countries

country_corrections <- c("Czechia"="Czech Republic", "Czechoslovakia"="Czech Republic", "Burma/Myanmar"="Myanmar",
                         "Democratic Republic of the Congo"="Democratic Republic of Congo",
                         "Republic of Vietnam"="Vietnam", "Republic of the Congo"="Congo", "The Gambia"="Gambia",
                         "Timor-Leste"="East Timor", "United States of America"="United States",
                         "Ethiopia (former)"="Ethiopia", "Yemen Arab Republic"="Yemen",
                         "Yemen People's Republic"="Yemen")

death_cause$entity <- ifelse(death_cause$entity %in% names(country_corrections),
                          country_corrections[death_cause$entity], death_cause$entity)

population$entity <- ifelse(population$entity %in% names(country_corrections),
                          country_corrections[population$entity], population$entity)

n_cancer$entity <- ifelse(n_cancer$entity %in% names(country_corrections),
                          country_corrections[n_cancer$entity], n_cancer$entity)

d_cancer_type$entity <- ifelse(d_cancer_type$entity %in% names(country_corrections),
                        country_corrections[d_cancer_type$entity], d_cancer_type$entity)

d_cancer_age$entity <- ifelse(d_cancer_age$entity %in% names(country_corrections),
                       country_corrections[d_cancer_age$entity], d_cancer_age$entity)

health_spe$entity <- ifelse(health_spe$entity %in% names(country_corrections),
                     country_corrections[health_spe$entity], health_spe$entity)

hd_index$entity <- ifelse(hd_index$entity %in% names(country_corrections),
                   country_corrections[hd_index$entity], hd_index$entity)

ideology$entity <- ifelse(ideology$entity %in% names(country_corrections),
                  country_corrections[ideology$entity], ideology$entity)

# Create again the country index
country_list <- union(union(union(union(union(union(union(select(death_cause, entity), select(population, entity)),
                                                    select(n_cancer, entity)), select(d_cancer_type, entity)),
                                        select(d_cancer_age, entity)), select(health_spe, entity)),
                            select(hd_index, entity)), select(ideology, entity))
distinct(country_list)

country_index_base <- left_join(country_list, d_cancer_type, by = "entity") %>%
  mutate(code = ifelse(is.na(code), 0, code)) %>%
  select(entity, code)

country_index <- distinct(country_index_base) # This is the final index

# Delete from index those countries with no code because are not countries or countries that lack data
country_index <- subset(country_index, !(country_index$code == 0 | country_index$code == ""))

country_index

# Create final dataset for the project
years <- 2000:2017 # Vector with years to analyse

df_project <- expand.grid(entity = country_index$entity, year = years) # Create a row of country by year

df_project <- merge(df_project, country_index, by.x = "entity", by.y = "entity") # Add again the country code

df_project <- df_project[, c("entity", "code", "year")] # Reorder columns

summary(df_project)

# Calculate the rate of people with cancer by year
n_cancer <- merge(n_cancer, population, by = c("entity", "code", "year"), all.x = TRUE) %>%
  select("entity", "code", "year", "n_people", "population")

n_cancer$cancer_affection_rate <- n_cancer$n_people/n_cancer$population

# Add the rate of cancer as death cause
dc_numeric <- death_cause[, 4:ncol(death_cause)]
dcause_sum <- rowSums(dc_numeric)
death_cause$total_sum <- dcause_sum
death_cause$cancer_death_rate <- death_cause$cancer/death_cause$total_sum

# Add the rate of cancer deaths by age
dage_numeric <- d_cancer_age[, 4:ncol(d_cancer_age)]
sum_by_age <- rowSums(dage_numeric)
d_cancer_age$total_sum <- sum_by_age
d_cancer_age$rate_70_over <- d_cancer_age$age70_over/d_cancer_age$total_sum
d_cancer_age$rate_50_69 <- d_cancer_age$age50_69/d_cancer_age$total_sum
d_cancer_age$rate_15_49 <- d_cancer_age$age15_49/d_cancer_age$total_sum
d_cancer_age$rate_5_14 <- d_cancer_age$age5_14/d_cancer_age$total_sum
d_cancer_age$rate_under_5 <- d_cancer_age$under_5/d_cancer_age$total_sum

# Add the rate of cancer death by type
dtype_numeric <- d_cancer_type[, 4:ncol(d_cancer_type)]
sum_by_type <- rowSums(dtype_numeric)
d_cancer_type$total_sum <- sum_by_type
d_cancer_type$rate_liver <- d_cancer_type$liver/d_cancer_type$total_sum
d_cancer_type$rate_kidney <- d_cancer_type$kidney/d_cancer_type$total_sum
d_cancer_type$rate_lip_oral_cavity <- d_cancer_type$lip_oral_cavity/d_cancer_type$total_sum
d_cancer_type$rate_tracheal_bronch_lung <- d_cancer_type$tracheal_bronch_lung/d_cancer_type$total_sum
d_cancer_type$rate_larynx <- d_cancer_type$larynx/d_cancer_type$total_sum
d_cancer_type$rate_gallblad_biliary <- d_cancer_type$gallblad_biliary/d_cancer_type$total_sum
d_cancer_type$rate_skin <- d_cancer_type$skin/d_cancer_type$total_sum
d_cancer_type$rate_leukemia <- d_cancer_type$leukemia/d_cancer_type$total_sum
d_cancer_type$rate_hodgkin <- d_cancer_type$hodgkin/d_cancer_type$total_sum
d_cancer_type$rate_myeloma <- d_cancer_type$myeloma/d_cancer_type$total_sum
d_cancer_type$rate_others <- d_cancer_type$others/d_cancer_type$total_sum
d_cancer_type$rate_breast <- d_cancer_type$breast/d_cancer_type$total_sum
d_cancer_type$rate_prostate <- d_cancer_type$prostate/d_cancer_type$total_sum
d_cancer_type$rate_thyroid <- d_cancer_type$thyroid/d_cancer_type$total_sum
d_cancer_type$rate_stomach <- d_cancer_type$stomach/d_cancer_type$total_sum
d_cancer_type$rate_bladder <- d_cancer_type$bladder/d_cancer_type$total_sum
d_cancer_type$rate_uterine <- d_cancer_type$uterine/d_cancer_type$total_sum
d_cancer_type$rate_ovarian <- d_cancer_type$ovarian/d_cancer_type$total_sum
d_cancer_type$rate_cervical <- d_cancer_type$cervical/d_cancer_type$total_sum
d_cancer_type$rate_brain_cns <- d_cancer_type$brain_cns/d_cancer_type$total_sum
d_cancer_type$rate_non_hodgkin <- d_cancer_type$non_hodgkin/d_cancer_type$total_sum
d_cancer_type$rate_pancreatic <- d_cancer_type$pancreatic/d_cancer_type$total_sum
d_cancer_type$rate_esophageal <- d_cancer_type$esophageal/d_cancer_type$total_sum
d_cancer_type$rate_testicular <- d_cancer_type$testicular/d_cancer_type$total_sum
d_cancer_type$rate_nasopharynx <- d_cancer_type$nasopharynx/d_cancer_type$total_sum
d_cancer_type$rate_other_pharynx <- d_cancer_type$other_pharynx/d_cancer_type$total_sum
d_cancer_type$rate_colon_rectum <- d_cancer_type$colon_rectum/d_cancer_type$total_sum
d_cancer_type$rate_non_melanoma_skin <- d_cancer_type$non_melanoma_skin/d_cancer_type$total_sum
d_cancer_type$rate_mesothelioma <- d_cancer_type$mesothelioma/d_cancer_type$total_sum

# Add all the variables calculated to the final dataset (ds_project)

df_project <- merge(df_project, ideology, by = c("entity", "year"), all.x = TRUE) %>%
  select("entity", "code", "year", "hog_ideology") # Add ideology

df_project <- merge(df_project, n_cancer, by = c("entity", "code", "year"), all.x = TRUE) %>%
  select("entity", "code", "year", "hog_ideology", "cancer_affection_rate") # Add the affection rate

df_project <- merge(df_project, death_cause, by = c("entity", "code", "year"), all.x = TRUE) %>%
  select("entity", "code", "year", "hog_ideology", "cancer_affection_rate", "cancer_death_rate") # Add the rate of cancer deaths

df_project <- merge(df_project, d_cancer_age, by = c("entity", "code", "year"), all.x = TRUE) %>%
  select("entity", "code", "year", "hog_ideology", "cancer_affection_rate", "cancer_death_rate",
         "rate_under_5", "rate_5_14", "rate_15_49", "rate_50_69", "rate_70_over") # Add the rate of cancer deaths by age

df_project <- merge(df_project, d_cancer_type, by = c("entity", "code", "year"), all.x = TRUE) %>%
  select("entity", "code", "year", "hog_ideology", "cancer_affection_rate", "cancer_death_rate",
         "rate_under_5", "rate_5_14", "rate_15_49", "rate_50_69", "rate_70_over", "rate_liver",
         "rate_kidney", "rate_lip_oral_cavity", "rate_tracheal_bronch_lung", "rate_larynx",
         "rate_gallblad_biliary", "rate_skin", "rate_leukemia", "rate_hodgkin", "rate_myeloma",
         "rate_others", "rate_breast", "rate_prostate", "rate_thyroid", "rate_stomach", "rate_bladder",
         "rate_uterine", "rate_ovarian", "rate_cervical", "rate_brain_cns", "rate_non_hodgkin",
         "rate_pancreatic", "rate_esophageal", "rate_testicular", "rate_nasopharynx", "rate_other_pharynx",
         "rate_colon_rectum", "rate_non_melanoma_skin", "rate_mesothelioma") # Add the rate of cancer deaths by type

df_project <- merge(df_project, health_spe, by = c("entity", "code", "year"), all.x = TRUE) %>%
  select("entity", "code", "year", "hog_ideology", "cancer_affection_rate", "cancer_death_rate",
         "rate_under_5", "rate_5_14", "rate_15_49", "rate_50_69", "rate_70_over", "rate_liver",
         "rate_kidney", "rate_lip_oral_cavity", "rate_tracheal_bronch_lung", "rate_larynx",
         "rate_gallblad_biliary", "rate_skin", "rate_leukemia", "rate_hodgkin", "rate_myeloma",
         "rate_others", "rate_breast", "rate_prostate", "rate_thyroid", "rate_stomach", "rate_bladder",
         "rate_uterine", "rate_ovarian", "rate_cervical", "rate_brain_cns", "rate_non_hodgkin",
         "rate_pancreatic", "rate_esophageal", "rate_testicular", "rate_nasopharynx", "rate_other_pharynx",
         "rate_colon_rectum", "rate_non_melanoma_skin", "rate_mesothelioma",
         "public_health_spe") # Add the public health expenditure

df_project <- merge(df_project, hd_index, by = c("entity", "code", "year"), all.x = TRUE) %>%
  select("entity", "code", "year", "hog_ideology", "cancer_affection_rate", "cancer_death_rate",
         "rate_under_5", "rate_5_14", "rate_15_49", "rate_50_69", "rate_70_over", "rate_liver",
         "rate_kidney", "rate_lip_oral_cavity", "rate_tracheal_bronch_lung", "rate_larynx",
         "rate_gallblad_biliary", "rate_skin", "rate_leukemia", "rate_hodgkin", "rate_myeloma",
         "rate_others", "rate_breast", "rate_prostate", "rate_thyroid", "rate_stomach", "rate_bladder",
         "rate_uterine", "rate_ovarian", "rate_cervical", "rate_brain_cns", "rate_non_hodgkin",
         "rate_pancreatic", "rate_esophageal", "rate_testicular", "rate_nasopharynx", "rate_other_pharynx",
         "rate_colon_rectum", "rate_non_melanoma_skin", "rate_mesothelioma",
         "public_health_spe", "hdi") # Add the human development index

summary(df_project)


# Check and clean missing data

# NAs in ideology are because there is no information available, so are deleted from final df
# All cases are countries which have no information in any year
df_project <- df_project[!is.na(df_project$hog_ideology),]

# Check number of "no information", "none", or "not applicable" that each country has
check_ideology <- df_project[,c("entity", "hog_ideology")]
  
check2_ideology <- check_ideology %>%
    group_by(entity) %>%
    summarise(no_info = sum(hog_ideology == "no information" | hog_ideology == "none" | 
                              hog_ideology =="not applicable"))

check2_ideology[check2_ideology$no_info == 18, ]

# Delete those with no information all years (Bahamas, Belize, Brunei, Jordan, Libya, Qatar)
df_project <- subset(df_project, !(entity %in% c("Bahamas", "Belize", "Brunei", "Jordan", "Libya", "Qatar")))

# As there are NAs in the last 2 columns, a short count will be done to understand if there is no information
# in any year of a given country, or are some years without information
check_spe_hdi <- df_project[,c("entity", "public_health_spe", "hdi")]

check2_spe_hdi <- check_spe_hdi %>%
  group_by(entity) %>%
  summarise(nas_spe = sum(is.na(public_health_spe)),
            nas_hdi = sum(is.na(hdi)))

check2_spe_hdi[check2_spe_hdi$nas_spe == 18 | check2_spe_hdi$nas_hdi == 18,]

# Delete from df_project North Korea, Somalia and Taiwan because there is no information of public spending and
# hdi variables
df_project <- subset(df_project, !(entity %in% c("North Korea", "Somalia", "Taiwan")))

# Check only spe NAs
check_spe <- df_project[,c("entity", "public_health_spe")]

check2_spe <- check_spe %>%
  group_by(entity) %>%
  summarise(nas_spe = sum(is.na(public_health_spe)))

check2_spe[check2_spe$nas_spe > 0,] # Just 8 countries have NAs in some years, and only Zimbawe has more years
# without information about public spending 

# Check only hdi NAs
check_hdi <- df_project[,c("entity", "hdi")]

check2_hdi <- check_hdi %>%
  group_by(entity) %>%
  summarise(nas_hdi = sum(is.na(hdi)))

check2_hdi[check2_hdi$nas_hdi > 0,] # Just 8 countries have NAs in some years, and only Bhutan has more years without information about hdi

# Although the countries are important, the idea of the project is to analyze ideology and public spending/cancer/HDI
# Also, the NAs in public_health_spe and hdi is of around 1%, so is preferable to delete them rather than replacing with mean or other value
df_project <- df_project[!is.na(df_project$public_health_spe),]
df_project <- df_project[!is.na(df_project$hdi),]

# Codes for some personal check
# Check new dataset
summary(df_project)
nas <- colSums(is.na(df_project))
zeros <- colSums(df_project == 0, na.rm = TRUE)
total_rows <- nrow(df_project)
table_resume <- data.frame(NAs = nas, weight_nas = round((nas/total_rows)*100,2), Zeros = zeros, weight_zeros = round((zeros/total_rows)*100,2))
table_resume

# Check the rest of cases with no ideology information
number_ideologies <- df_project[,c("entity", "hog_ideology")]

number_ideologies2 <- number_ideologies %>%
  group_by(entity) %>%
  summarise(no_info = sum(hog_ideology == "no information" | hog_ideology == "none" | 
                            hog_ideology =="not applicable"))

length(number_ideologies2$entity) # 163 records with no information about ideology

# Those "no information" are not so representative from the whole dataset, sothey are deleted.

# Deleting all "no information", "none", or "not applicable" as it won't affect the project objective, and after some
# investigations, there were a lot of wars, doubts, independent governors, etc, that makes impossible to assume the
# ideology of those years.
# There are also cases of countries that were founded later or get the independence.
# The dataset is really detailed and makes no sense to assume ideologies in these cases.

df_project <- df_project[!(df_project$hog_ideology == "no information"),]
df_project <- df_project[!(df_project$hog_ideology == "none"),]
df_project <- df_project[!(df_project$hog_ideology == "not applicable"),]


# Save the files for manual checking
# ruta_archivo <- "C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\Dataset check\\outliers_check.xlsx"
# write.xlsx(df_project, file = ruta_archivo)

# ruta_archivo <- "C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\Dataset check\\check_country_index.xlsx"
# write.xlsx(country_index, file = ruta_archivo)

# EDA
str(df_project)

length(unique(df_project$entity))

summary(df_project)

# Number of cases by ideology
number_by_ideology <- data.frame(
  Left = sum(df_project$hog_ideology == "leftist"),
  Center = sum(df_project$hog_ideology == "centrist"),
  Right = sum(df_project$hog_ideology == "rightist"))

ideology_numbers <- barplot(as.numeric(unlist(number_by_ideology)), col = c("coral", "#FFFFE0", "cornflowerblue") , width = c(50, 50), las=1, space = 1,
        names.arg= c("Left", "Center", "Right"), cex.names=0.75,
        main = "Number of records by ideology", ylab = "N° of records",
        ylim = c(0, 1370))
text(ideology_numbers, y = number_by_ideology-100, paste(unlist(number_by_ideology), sep = ""), cex = 1, col = "black")
plot_limits <- par("usr")
rect(plot_limits[1], plot_limits[3], plot_limits[2], plot_limits[4], border = "black", lwd = 2)

# Average investment in public health by ideology
spe_by_ideology <- data.frame(
  Left = mean(df_project$public_health_spe[df_project$hog_ideology == "leftist"]),
  Center = mean(df_project$public_health_spe[df_project$hog_ideology == "centrist"]),
  Right = sum(mean(df_project$public_health_spe[df_project$hog_ideology == "rightist"])))

ideology_spe <- barplot(as.numeric(unlist(spe_by_ideology)), col = c("coral", "#FFFFE0", "cornflowerblue") , width = c(50, 50), las=1, space = 1,
                            names.arg= c("Left", "Center", "Right"), cex.names=0.75,
                            main = "Avg. Spending in Public Health by Ideology", ylab = "Public Health Spending (% from GPD)",
                        ylim = c(0, 3.75))
text(ideology_spe, y = unlist(spe_by_ideology) - 0.15, labels = round(unlist(spe_by_ideology), 2), col = "black")
plot_limits <- par("usr")
rect(plot_limits[1], plot_limits[3], plot_limits[2], plot_limits[4], border = "black", lwd = 2)

# Average death by cancer by ideology
cdeath_by_ideology <- data.frame(
  Left = mean(df_project$cancer_death_rate[df_project$hog_ideology == "leftist"]),
  Center = mean(df_project$cancer_death_rate[df_project$hog_ideology == "centrist"]),
  Right = sum(mean(df_project$cancer_death_rate[df_project$hog_ideology == "rightist"])))

ideology_cdeath <- barplot(as.numeric(unlist(cdeath_by_ideology)), col = c("coral", "#FFFFE0", "cornflowerblue") , width = c(50, 50), las=1, space = 1,
                        names.arg= c("Left", "Center", "Right"), cex.names=0.75,
                        main = "Avg. Cancer Death Rate by Ideology", ylab = "Cancer Death Rate",
                        ylim = c(0, 0.2))
text(ideology_cdeath, y = unlist(cdeath_by_ideology) - 0.01, labels = round(unlist(cdeath_by_ideology), 2),
          col = "black")
plot_limits <- par("usr")
rect(plot_limits[1], plot_limits[3], plot_limits[2], plot_limits[4], border = "black", lwd = 2)

# Average HDI by ideology
hdi_by_ideology <- data.frame(
  Left = mean(df_project$hdi[df_project$hog_ideology == "leftist"]),
  Center = mean(df_project$hdi[df_project$hog_ideology == "centrist"]),
  Right = sum(mean(df_project$hdi[df_project$hog_ideology == "rightist"])))

ideology_hdi <- barplot(as.numeric(unlist(hdi_by_ideology)), col = c("coral", "#FFFFE0", "cornflowerblue") , width = c(50, 50), las=1, space = 1,
                           names.arg= c("Left", "Center", "Right"), cex.names=0.75,
                           main = "Avg. HDI by Ideology", ylab = "HDI",
                        ylim = c (0, 0.8))
text(ideology_hdi, y = unlist(hdi_by_ideology) - 0.05, labels = round(unlist(hdi_by_ideology), 2),
     col = "black")
plot_limits <- par("usr")
rect(plot_limits[1], plot_limits[3], plot_limits[2], plot_limits[4], border = "black", lwd = 2)

# Map
# World map base
world <- ne_countries(scale = "medium", returnclass = "sf")

map_corrections <- c("Cabo Verde"="Cape Verde", "Republic of the Congo"="Congo", "Czechia"="Czech Republic",
                     "Democratic Republic of the Congo"="Democratic Republic of Congo",
                     "SÃ£o TomÃ© and Principe"="Sao Tome and Principe", "Republic of Serbia"="Serbia",
                     "United Republic of Tanzania"="Tanzania", "United States of America"="United States")

world$sovereignt <- ifelse(world$sovereignt %in% names(map_corrections),
                           map_corrections[world$sovereignt], world$sovereignt)

# Final df for map
df_map <- df_project[,c(1,4:6,41:42)]
df_map$hog_ideology_numeric <- ifelse(df_map$hog_ideology == "leftist", -1,
                                          ifelse(df_map$hog_ideology == "rightist", 1, 0))

# Ideology map
ideology_map <- df_map[,c(1,7)]
ideology_map <- aggregate(hog_ideology_numeric ~ entity, data = ideology_map, FUN = mean)

final_map_ideology <- merge(world, ideology_map, by.x = "sovereignt", by.y = "entity", all.x = TRUE)

ggplot(final_map_ideology, aes(fill = hog_ideology_numeric)) +
  geom_sf() +
  scale_fill_gradient(low = "coral", high = "cornflowerblue", name = "Avg. ideology", limits = c(-1, 1)) +
  labs(title = "Anual average ideology by country")

# Cancer death map
cancerdeath_map <- df_map[,c(1,4)]
cancerdeath_map <- aggregate(cancer_death_rate ~ entity, data = cancerdeath_map, FUN = mean)

death_cancer_map <- merge(world, cancerdeath_map, by.x = "sovereignt", by.y = "entity", all.x = TRUE)

ggplot(death_cancer_map, aes(fill = cancer_death_rate)) +
  geom_sf() +
  scale_fill_gradient(low = "green", high = "darkred", name = "Percentage") +
  labs(title = "Anual average death by cancer (% from total deaths)")

# Public Health Spending map
public_spe_map <- df_map[,c(1,5)]
public_spe_map <- aggregate(public_health_spe ~ entity, data = public_spe_map, FUN = mean)

final_pspe_map <- merge(world, public_spe_map, by.x = "sovereignt", by.y = "entity", all.x = TRUE)

ggplot(final_pspe_map, aes(fill = public_health_spe)) +
  geom_sf() +
  scale_fill_gradient(low = "darkred", high = "green", name = "Percentage") +
  labs(title = "Anual average public health spending (% from GDP)")

# HDI
hdi_map <- df_map[,c(1,6)]
hdi_map <- aggregate(hdi ~ entity, data = hdi_map, FUN = mean)

final_hdi_map <- merge(world, hdi_map, by.x = "sovereignt", by.y = "entity", all.x = TRUE)

ggplot(final_hdi_map, aes(fill = hdi)) +
  geom_sf() +
  scale_fill_gradient(low = "darkred", high = "green", name = "Index (0-1)") +
  labs(title = "Anual average HDI")

# df_project$hog_ideology <- factor(df_project$hog_ideology, levels = c("rightist", "centrist", "leftist"))
# df_project$hog_ideology_numeric <- as.numeric(df_project$hog_ideology)
df_project$hog_ideology_numeric <- ifelse(df_project$hog_ideology == "leftist", -1,
                                          ifelse(df_project$hog_ideology == "rightist", 1, 0))

# Check distribution of variables
# Main variables
car_dist <- ggplot(df_project, aes(x = "", y = df_project[,5])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Cancer Affection Rate",
       y = "Distribution") +
  theme_minimal()

cdr_dist <- ggplot(df_project, aes(x = "", y = df_project[,6])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Cancer Death Rate",
       y = "Distribution") +
  theme_minimal()

phs_dist <- ggplot(df_project, aes(x = "", y = df_project[,41])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Public Health Spending",
       y = "Distribution") +
  theme_minimal()

hdi_dist <- ggplot(df_project, aes(x = "", y = df_project[,42])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Human Development Index",
       y = "Distribution") +
  theme_minimal()

ideo_dist <- ggplot(df_project, aes(x = "", y = df_project[,43])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Ideology",
       y = "Distribution") +
  theme_minimal()

grid.arrange(car_dist, cdr_dist, phs_dist, hdi_dist, ideo_dist, ncol = 2)


# Type of cancer
par(mar = c(8, 5, 2, 2)) # Adjusting plot view

boxplot(df_project[,12:40],
        main = "Type of cancer",
        ylab = "Percentage of deaths by cancer",
        las = 2,
        col=rainbow(29),
        cex.axis = 0.6,
        border = "black")

par(mar = c(5, 4, 4, 2)) # Original size

# Age rate
ggplot(data = df_project) +
  geom_density(aes(x = rate_under_5, fill = "rate_under_5"), alpha = 0.4) +
  geom_density(aes(x = `rate_5_14`, fill = "rate_5_14"), alpha = 0.4) +
  geom_density(aes(x = `rate_15_49`, fill = "rate_15_49"), alpha = 0.4) +
  geom_density(aes(x = `rate_50_69`, fill = "rate_50_69"), alpha = 0.4) +
  geom_density(aes(x = `rate_70_over`, fill = "rate_70_over"), alpha = 0.4) +
  scale_fill_manual(values = c("red", "aquamarine", "chartreuse", "purple", "black")) +
  labs(title = "Death from cancer by range of age",
       x = "Sample percentage",
       y = "Density",
       fill = "Age ranges")

# Check outliers limits
get_outlier_limits <- function(column) {
  q1 <- quantile(column, 0.25)
  q3 <- quantile(column, 0.75)
  iqr <- q3 - q1
  lower_limit <- q1 - 1.5 * iqr
  upper_limit <- q3 + 1.5 * iqr
  return(list(lower = lower_limit, upper = upper_limit))
}

outlier_data <- data.frame(Column = character(), Lower_Limit = numeric(), Upper_Limit = numeric(), Num_outliers = numeric())

for (col in colnames(df_project[,5:43])) {
  limits <- get_outlier_limits(df_project[[col]])
  outliers <- df_project[[col]][df_project[[col]] < limits$lower | df_project[[col]] > limits$upper]
  num_outliers <- length(outliers)
  outlier_data <- rbind(outlier_data, data.frame(Column = col, Lower_Limit = limits$lower, Upper_Limit = limits$upper, Num_outliers = num_outliers))
}

df_wo_outliers <- df_project[df_project$cancer_affection_rate >= -0.0211906690 &
                               df_project$cancer_affection_rate <= 0.042611618, ]

df_wo_outliers <- df_wo_outliers[df_wo_outliers$public_health_spe >= -3.1306780125 &
                                   df_wo_outliers$public_health_spe <= 9.108248087, ]


new_car_dist <- ggplot(df_wo_outliers, aes(x = "", y = df_wo_outliers[,5])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Cancer Affection Rate",
       y = "Distribution") +
  theme_minimal()

new_phs_dist <- ggplot(df_wo_outliers, aes(x = "", y = df_wo_outliers[,41])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Public Health Spending",
       y = "Distribution") +
  theme_minimal()

grid.arrange(new_car_dist, new_phs_dist, ncol = 1)

new_corr_matrix <- cor(df_wo_outliers[, c(5:6, 41:43)] %>% dplyr::select_if(is.numeric))

ggcorrplot(new_corr_matrix, type = "lower", outline.color = "white", lab = TRUE,
           colors = c("darkred","#FFFFE0","darkblue")) +
  labs(title = "Correlation Heatmap")


# UNSUPERVISED LEARNING
# Only use variables: cancer affection, cancer deaths, public health spending, hdi and ideology

summary (df_project)

df_cluster <- df_project[ , c("code", "cancer_affection_rate", "cancer_death_rate",
                              "public_health_spe", "hdi", "hog_ideology_numeric")]

df_cluster <- aggregate(. ~ code, data = df_cluster, FUN = mean)

df_cluster_stand <- scale(df_cluster[,c(2:6)])  # To standarize the variables

summary(df_cluster_stand)


# Correlation matrix

corr_matrix <- cor(df_cluster %>% dplyr::select_if(is.numeric))

ggcorrplot(corr_matrix, type = "lower", outline.color = "white", lab = TRUE,
           colors = c("darkred","#FFFFE0","darkblue")) +
  labs(title = "Correlation Heatmap")


# PCA
# Principal Components
# Eigenvalues
autoval <- eigen(corr_matrix)

plot(autoval$values, type="b", main="Principal Components", xlab="Number of Component", ylab="Eigenvalues")
abline(h=1, lwd=3, col="darkred")

# PCA
pca_base<-princomp(df_cluster[, c(2:6)], cor=TRUE)
summary(pca_base)

pca_var <- pca_base$sdev^2
prop_var <- pca_var/sum(pca_var)

screeplot(pca_base, main = "Principal Components", col = "lightblue")
lines(cumsum(prop_var), col = "darkred", lwd = 2, type = 'b')
text(seq_along(prop_var), cumsum(prop_var), labels = paste(round((cumsum(prop_var)*100),2), "%"),
     pos = 1, col = "black")

loadings <- pca_base$loadings
print(loadings[, 1:2])

pca_plot <- prcomp(df_cluster[, c("cancer_affection_rate", "cancer_death_rate", "public_health_spe", "hdi", "hog_ideology_numeric")], scale. = TRUE)
plot(pca_plot$x[, 1], pca_plot$x[, 2], type = "n", xlab = "Comp. 1", ylab = "Comp. 2", main = "Principal Components Analysis")
text(pca_plot$x[, 1], pca_plot$x[, 2], labels = df_cluster[,1], col = "black", cex = 0.5, pos = 4)
abline(h=0, v=0, col="black")

arrows(0, 0, loadings['cancer_affection_rate',1], loadings['cancer_affection_rate',2], col='darkred', length=0.1)
arrows(0, 0, loadings['cancer_death_rate',1], loadings['cancer_death_rate',2], col='darkred', length=0.1)
arrows(0, 0, loadings['public_health_spe',1], loadings['public_health_spe',2], col='darkred', length=0.1)
arrows(0, 0, loadings['hdi',1], loadings['hdi',2], col='darkred', length=0.1)
arrows(0, 0, loadings['hog_ideology_numeric',1], loadings['hog_ideology_numeric',2], col='darkred', length=0.1)

text(loadings[, 1], loadings[, 2], labels = rownames(loadings), col = "darkred", cex = 0.8, pos = 4)

# Variables only
fviz_pca_var(pca_base, col.var = "black", repel = -5)


# Create new df with first 2 components of the PCA
pca_results <- as.data.frame(pca_base$scores)
pca_results <- cbind(df_cluster[, 1, drop = FALSE], pca_results)
pca_results <- pca_results[,c(1:3)]
# Component 1 represents cancer_affection_rate, cancer_death rate, public_health_spe, hdi
# Component 2 represents hod_ideology_numeric


# Check again correlation matrix
pca_corr_matrix <- cor(pca_results[,c(2:3)])

df_pca_corr <- as.data.frame(as.table(pca_corr_matrix))
colnames(df_pca_corr) <- c("Var1", "Var2", "Correlation")

# Plot again a heatmap to check the 0 correlation between components
ggplot(data = df_pca_corr, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", direction = 1, limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  coord_fixed()



# Clustering with original dataset

# Decision of clusters
# Original data best clustering number
rownames(df_cluster_stand) <- 1:nrow(pca_results)

clmethods <- c("hierarchical", "kmeans", "pam")
validation_original <- clValid(df_cluster_stand, 2:6, clMethods = clmethods, validation="internal")

summary(validation_original) # Hierarchical 3 and K-Means 2

# PCA best clustering number
rownames(pca_results) <- 1:nrow(pca_results)

clmethods <- c("hierarchical", "kmeans", "pam")
validation_pca <- clValid(pca_results[c(2:3)], 2:6, clMethods = clmethods, validation="internal")

summary(validation_pca) # Hierarchical 2 and 6, and K-Means 2

# Elbow method check
fviz_nbclust(df_cluster_stand, FUNcluster = hcut, method = "wss") # Original standarized dataset
fviz_nbclust(pca_results[,c(2:3)], FUNcluster = hcut, method = "wss") # New dataframe with PCA

# Silhouette method check
fviz_nbclust(df_cluster_stand, FUNcluster = hcut, method = "silhouette") # Original standarized dataset
fviz_nbclust(pca_results[,c(2:3)], FUNcluster = hcut, method = "silhouette") # New dataframe with PCA


# Hierarchical Clustering
# Number of clusters
k_2 <- 2
k_4 <- 5
k_6 <- 6

# Original
# Complete
H.fit_complete <- hclust(dist(df_cluster_stand, method = "euclidean"), method = "complete")
complete_cluster <- cutree(H.fit_complete, k_4)
mean(silhouette(complete_cluster, dist(df_cluster_stand))[, "sil_width"]) # Best option

# Single
H.fit_single <- hclust(dist(df_cluster_stand, method = "euclidean"), method = "single")
single_cluster <- cutree(H.fit_single, k_4)
mean(silhouette(single_cluster, dist(df_cluster_stand))[, "sil_width"])

# Average
H.fit_average <- hclust(dist(df_cluster_stand, method = "euclidean"), method = "average")
average_cluster <- cutree(H.fit_average, k_4)
mean(silhouette(average_cluster, dist(df_cluster_stand))[, "sil_width"])

# PCA
# Complete
H.fit_complete_pca <- hclust(dist(pca_results[,c(2:3)], method = "euclidean"), method = "complete")
complete_cluster_pca <- cutree(H.fit_complete_pca, k_6)
mean(silhouette(complete_cluster_pca, dist(pca_results[,c(2:3)]))[, "sil_width"])


# Single
H.fit_single_pca <- hclust(dist(pca_results[,c(2:3)], method = "euclidean"), method = "single")
single_cluster_pca <- cutree(H.fit_single_pca, k_6)
mean(silhouette(single_cluster_pca, dist(pca_results[,c(2:3)]))[, "sil_width"])


# Average
H.fit_average_pca <- hclust(dist(pca_results[,c(2:3)], method = "euclidean"), method = "average")
average_cluster_pca <- cutree(H.fit_average_pca, k_6)
mean(silhouette(average_cluster_pca, dist(pca_results[,c(2:3)]))[, "sil_width"]) # Best with both Ks (2 and 6)

average_2cluster_pca <- cutree(H.fit_average_pca, k_2)
mean(silhouette(average_2cluster_pca, dist(pca_results[,c(2:3)]))[, "sil_width"]) # Best with both Ks (2 and 6)


# Dendogram of PCA (not doing from original dataset)
plot(H.fit_average, labels = df_cluster$code, cex = 0.6) # display dendogram

# draw dendogram with borders around the clusters
rect.hclust(H.fit_average_pca, k=k_2, border="purple")
rect.hclust(H.fit_average_pca, k=k_4, border="green")
rect.hclust(H.fit_average_pca, k=k_6, border="darkblue") # From elbow method, k=3 was also an interesting option

plot(H.fit_complete_pca, labels = pca_results$code, cex = 0.6)
rect.hclust(H.fit_complete_pca, k=k_2, border="purple")
rect.hclust(H.fit_complete_pca, k=k_4, border="green")
rect.hclust(H.fit_complete_pca, k=k_6, border="darkblue")

# Add number of hierarchical clusters to a new dataFrame
hierachical_cluster_result <- pca_results
hierachical_cluster_result$hierarchical_cluster_k2 <- as.factor(average_2cluster_pca) # Add the cluster to the dataframe
hierachical_cluster_result$hierarchical_cluster_k6 <- as.factor(average_cluster_pca) # Add the cluster to the dataframe


# K-Means Clustering
set.seed(123) # Set Seed for Reproducibility

kmean_k <- 4

# Original dataset (df_cluster_stand)
k_mean <- kmeans(df_cluster_stand, centers=kmean_k, nstart=25)
mean(silhouette(k_mean$cluster, dist(df_cluster_stand))[, "sil_width"])

# Plot
df_kmean <- df_cluster

rownames(df_kmean) <- df_kmean$code

df_kmean[,c(2:6)] <- scale(df_kmean[,c(2:6)])

df_kmean$cluster <- as.factor(k_mean$cluster)

fviz_cluster(list(data=df_kmean[,c(2:6)],cluster=df_kmean$cluster), repel = TRUE, labelsize = 6) +
  theme_fivethirtyeight() +
  ggtitle("K-Means Cluster")


# Using first 2 components of PCA (pca_results)
k_mean_pca <- kmeans(pca_results[,c(2:3)], centers=kmean_k, nstart=25) # Pca dataset
mean(silhouette(k_mean_pca$cluster, dist(pca_results[,c(2:3)]))[, "sil_width"]) # This is better

# Plot
df_kmean_pca <- pca_results

rownames(df_kmean_pca) <- df_kmean_pca$code

df_kmean_pca$cluster <- as.factor(k_mean_pca$cluster)

fviz_cluster(list(data=df_kmean_pca[,c(2:3)],cluster=df_kmean_pca$cluster), repel = TRUE, labelsize = 6) +
  theme_fivethirtyeight() +
  ggtitle("K-Means Cluster with PCA")
# The plot is almost identical to the one using the df_cluster_stand


# Variables by hierarchical cluster (K=2)
# Cluster 1
cluster_1 <- hierachical_cluster_result[hierachical_cluster_result$hierarchical_cluster_k2 == 1,]

k2_cluster1 <- ggplot(data=cluster_1) +
  geom_density(aes(x = Comp.1, fill = "PCA_1"), alpha = 0.4) +
  geom_density(aes(x = Comp.2, fill = "PCA_2"), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid1", "darkgoldenrod1"), name = "Components") +
  labs(title = "Cluster 1",
       x = "Components values",
       y = "Density") +
  theme_minimal()

# Cluster 2
cluster_2 <- hierachical_cluster_result[hierachical_cluster_result$hierarchical_cluster_k2 == 2,]

k2_cluster2 <- ggplot(data=cluster_2) +
  geom_density(aes(x = Comp.1, fill = "PCA_1"), alpha = 0.4) +
  geom_density(aes(x = Comp.2, fill = "PCA_2"), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid1", "darkgoldenrod1"), name = "Components") +
  labs(title = "Cluster 2",
       x = "Components values",
       y = "Density") +
  theme_minimal()

grid.arrange(k2_cluster1, k2_cluster2, nrow = 2)


# Variables by hierarchical cluster (K=6)
# Cluster 1
clusterk6_1 <- hierachical_cluster_result[hierachical_cluster_result$hierarchical_cluster_k6 == 1,]

k6_cluster1 <- ggplot(data=clusterk6_1) +
  geom_density(aes(x = Comp.1, fill = "PCA_1"), alpha = 0.4) +
  geom_density(aes(x = Comp.2, fill = "PCA_2"), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid1", "darkgoldenrod1"), name = "Components") +
  labs(title = "Cluster 1",
       x = "Components values",
       y = "Density") +
  theme_minimal()

# Cluster 2
clusterk6_2 <- hierachical_cluster_result[hierachical_cluster_result$hierarchical_cluster_k6 == 2,]

k6_cluster2 <- ggplot(data=clusterk6_2) +
  geom_density(aes(x = Comp.1, fill = "PCA_1"), alpha = 0.4) +
  geom_density(aes(x = Comp.2, fill = "PCA_2"), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid1", "darkgoldenrod1"), name = "Components") +
  labs(title = "Cluster 2",
       x = "Components values",
       y = "Density") +
  theme_minimal()

# Cluster 3
clusterk6_3 <- hierachical_cluster_result[hierachical_cluster_result$hierarchical_cluster_k6 == 3,]

k6_cluster3 <- ggplot(data=clusterk6_3) +
  geom_density(aes(x = Comp.1, fill = "PCA_1"), alpha = 0.4) +
  geom_density(aes(x = Comp.2, fill = "PCA_2"), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid1", "darkgoldenrod1"), name = "Components") +
  labs(title = "Cluster 3",
       x = "Components values",
       y = "Density") +
  theme_minimal()

# Cluster 4
clusterk6_4 <- hierachical_cluster_result[hierachical_cluster_result$hierarchical_cluster_k6 == 4,]

k6_cluster4 <- ggplot(data=clusterk6_4) +
  geom_density(aes(x = Comp.1, fill = "PCA_1"), alpha = 0.4) +
  geom_density(aes(x = Comp.2, fill = "PCA_2"), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid1", "darkgoldenrod1"), name = "Components") +
  labs(title = "Cluster 4",
       x = "Components values",
       y = "Density") +
  theme_minimal()

# Cluster 5
clusterk6_5 <- hierachical_cluster_result[hierachical_cluster_result$hierarchical_cluster_k6 == 5,]

k6_cluster5 <- ggplot(data=clusterk6_5) +
  geom_density(aes(x = Comp.1, fill = "PCA_1"), alpha = 0.4) +
  geom_density(aes(x = Comp.2, fill = "PCA_2"), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid1", "darkgoldenrod1"), name = "Components") +
  labs(title = "Cluster 5",
       x = "Components values",
       y = "Density") +
  theme_minimal()

# Cluster 6
clusterk6_6 <- hierachical_cluster_result[hierachical_cluster_result$hierarchical_cluster_k6 == 6,]

k6_cluster6 <- ggplot(data=clusterk6_6) +
  geom_density(aes(x = Comp.1, fill = "PCA_1"), alpha = 0.4) +
  geom_density(aes(x = Comp.2, fill = "PCA_2"), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid1", "darkgoldenrod1"), name = "Components") +
  labs(title = "Cluster 6",
       x = "Components values",
       y = "Density") +
  theme_minimal()

grid.arrange(k6_cluster1, k6_cluster2, k6_cluster3, nrow = 3)
grid.arrange(k6_cluster4, k6_cluster5, k6_cluster6, nrow = 3)


# Variables by K-Means cluster (K=2)
df_final_kmean <- merge(pca_results, df_kmean, by = "code")
df_final_kmean <- df_final_kmean[,c(1:3,9)]

# Cluster 1
km_cluster_1 <- df_final_kmean[df_final_kmean$cluster == 1,]

kmean_cluster1 <- ggplot(data=km_cluster_1) +
  geom_density(aes(x = Comp.1, fill = "PCA_1"), alpha = 0.4) +
  geom_density(aes(x = Comp.2, fill = "PCA_2"), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid1", "darkgoldenrod1"), name = "Components") +
  labs(title = "Cluster 1",
       x = "Components values",
       y = "Density") +
  theme_minimal()

# Cluster 2
km_cluster_2 <- df_final_kmean[df_final_kmean$cluster == 2,]

kmean_cluster2 <- ggplot(data=km_cluster_2) +
  geom_density(aes(x = Comp.1, fill = "PCA_1"), alpha = 0.4) +
  geom_density(aes(x = Comp.2, fill = "PCA_2"), alpha = 0.4) +
  scale_fill_manual(values = c("darkorchid1", "darkgoldenrod1"), name = "Components") +
  labs(title = "Cluster 2",
       x = "Components values",
       y = "Density") +
  theme_minimal()

grid.arrange(kmean_cluster1, kmean_cluster2, nrow = 2)



# SUPERVISED LEARNING

# Corrleation of all the model
superv_corr <- cor(df_project[,c(5:43)])

corrplot(superv_corr, type = "upper", 
         tl.col = "black", tl.srt = 90, tl.cex = 0.5) # High correlation among some variables (multicolinearity)


# Reduction of variables (age and type of cancer)
# New variable 50 and over for age.
d_cancer_age$rate_50_over <- (d_cancer_age$age50_69+d_cancer_age$age70_over)/d_cancer_age$total_sum

# Just use the main cancers (according to WHO): lungs, breast, colorectal, prostate and stomach.
d_cancer_type$main_cancers <- (d_cancer_type$tracheal_bronch_lung + d_cancer_type$breast + d_cancer_type$colon_rectum +
                                 d_cancer_type$prostate + d_cancer_type$stomach)/d_cancer_type$total_sum

df_supervised <- merge(df_project, d_cancer_age, by = c("entity", "code", "year"), all.x = TRUE) %>%
  select("entity", "code", "year", "cancer_affection_rate", "cancer_death_rate", "rate_50_over",
         "public_health_spe", "hdi", "hog_ideology", "hog_ideology_numeric")

df_supervised <- merge(df_supervised, d_cancer_type, by = c("entity", "code", "year"), all.x = TRUE) %>%
  select("entity", "code", "year", "cancer_affection_rate", "cancer_death_rate", "rate_50_over", "main_cancers",
         "public_health_spe", "hdi", "hog_ideology_numeric")

summary(df_supervised)

# Check again corrleation

corr_sup_matrix <- cor(df_supervised[,c(4, 6:10)] %>% dplyr::select_if(is.numeric))

ggcorrplot(corr_sup_matrix, type = "lower", outline.color = "white", lab = TRUE,
           colors = c("darkred","#FFFFE0","darkblue")) +
  labs(title = "Correlation Heatmap")

# Check distribution of variables
new_car_dist <- ggplot(df_project, aes(x = "", y = df_supervised[,4])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Cancer Affection Rate",
       y = "Distribution") +
  theme_minimal()

new_cdr_dist <- ggplot(df_project, aes(x = "", y = df_supervised[,5])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Cancer Death Rate",
       y = "Distribution") +
  theme_minimal()

new_phs_dist <- ggplot(df_project, aes(x = "", y = df_supervised[,8])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Public Health Spending",
       y = "Distribution") +
  theme_minimal()

new_hdi_dist <- ggplot(df_project, aes(x = "", y = df_supervised[,9])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Human Development Index",
       y = "Distribution") +
  theme_minimal()

new_ideo_dist <- ggplot(df_project, aes(x = "", y = df_supervised[,10])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Ideology",
       y = "Distribution") +
  theme_minimal()

over_50_dist <- ggplot(df_project, aes(x = "", y = df_supervised[,6])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Over 50",
       y = "Distribution") +
  theme_minimal()

main_cancers_dist <- ggplot(df_project, aes(x = "", y = df_supervised[,7])) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Main Cancers",
       y = "Distribution") +
  theme_minimal()

grid.arrange(new_car_dist, new_cdr_dist, new_phs_dist, new_hdi_dist, new_ideo_dist, over_50_dist,
             main_cancers_dist, ncol = 2)


# Check outliers limits
new_outlier_limits <- function(column) {
  q1 <- quantile(column, 0.25)
  q3 <- quantile(column, 0.75)
  iqr <- q3 - q1
  lower_limit <- q1 - 1.5 * iqr
  upper_limit <- q3 + 1.5 * iqr
  return(list(lower = lower_limit, upper = upper_limit))
}

new_outlier_data <- data.frame(Column = character(), Lower_Limit = numeric(), Upper_Limit = numeric(), Num_outliers = numeric())

for (col in colnames(df_supervised[,4:10])) {
  limits <- new_outlier_limits(df_supervised[[col]])
  outliers <- df_supervised[[col]][df_supervised[[col]] < limits$lower | df_supervised[[col]] > limits$upper]
  num_outliers <- length(outliers)
  new_outlier_data <- rbind(new_outlier_data, data.frame(Column = col, Lower_Limit = limits$lower, Upper_Limit = limits$upper, Num_outliers = num_outliers))
}

df_sup_wo_outliers <- df_supervised[df_supervised$cancer_affection_rate >= -0.0211906690 &
                                      df_supervised$cancer_affection_rate <= 0.042611618, ]

df_sup_wo_outliers <- df_sup_wo_outliers[df_sup_wo_outliers$public_health_spe >= -3.1306780125 &
                                           df_sup_wo_outliers$public_health_spe <= 9.108248087, ]

df_sup_wo_outliers <- df_sup_wo_outliers[df_sup_wo_outliers$main_cancers >= 0.32510275 &
                                           df_sup_wo_outliers$main_cancers <= 0.72415382, ]

# Check new correlation
corr_new_sup_matrix <- cor(df_sup_wo_outliers[,c(4, 6:10)] %>% dplyr::select_if(is.numeric))

ggcorrplot(corr_new_sup_matrix, type = "lower", outline.color = "white", lab = TRUE,
           colors = c("darkred","#FFFFE0","darkblue")) +
  labs(title = "Correlation Heatmap")


# PCA for supervised, excluding the dependant variable
sup_autoval <- eigen(corr_sup_matrix)

plot(sup_autoval$values, type="b", main="Principal Components", xlab="Number of Component", ylab="Eigenvalues")
abline(h=1, lwd=3, col="darkred")

sup_pca <-princomp(df_supervised[,c(4, 6:10)], cor=TRUE)
summary(sup_pca)

sup_pca$loadings

first_two_components <- predict(sup_pca, newdata = df_supervised[, c(4, 6:10)])[, 1:2]

df_sup_pca <- cbind(df_supervised, first_two_components)


#Regresión model (linear)
linear_reg_model<- lm(cancer_death_rate ~ Comp.1 + Comp.2, data = df_sup_pca)

summary(linear_reg_model)

# Plot dependent variable with each independent variable
ggplot(data=df_sup_pca, aes(x=Comp.1, y=cancer_death_rate)) + geom_point() +
  geom_abline(intercept=0.1615433, slope=0.0448551, color="red")

ggplot(data=df_sup_pca, aes(x=Comp.2, y=cancer_death_rate)) + geom_point() +
  geom_abline(intercept=0.1615433, slope=-0.0009496, color="red")

# Predictions and residuals
predictions <- predict(linear_reg_model, newdata=df_sup_pca)

residuals <- df_sup_pca$cancer_death_rate-predictions

qqnorm(residuals)
qqline(residuals)


R2 = R2(predictions, df_sup_pca$cancer_death_rate)
RMSE = RMSE(predictions, df_sup_pca$cancer_death_rate)  
MAE = MAE(predictions, df_sup_pca$cancer_death_rate)

cat("R^2 =", R2, "\n")
cat("RMSE =", RMSE, "\n")
cat("MAE =", MAE, "\n")


# Check the same regression model but with anual average

df_supervised_avg <- aggregate(. ~ code, data = df_supervised, FUN = mean)

df_supervised_avg <- df_supervised_avg[c(1, 4:10)]


# Check again corrleation

corr_sup_avg_matrix <- cor(df_supervised_avg[,c(2, 4:8)] %>% dplyr::select_if(is.numeric))

ggcorrplot(corr_sup_avg_matrix, type = "lower", outline.color = "white", lab = TRUE,
           colors = c("darkred","#FFFFE0","darkblue")) +
  labs(title = "Correlation Heatmap")


# PCA for supervised, excluding the dependant variable
sup_avg_autoval <- eigen(corr_sup_avg_matrix)

plot(sup_avg_autoval$values, type="b", main="Principal Components", xlab="Number of Component", ylab="Eigenvalues")
abline(h=1, lwd=3, col="darkred")

sup_avg_pca <-princomp(df_supervised_avg[,c(2, 4:8)], cor=TRUE)
summary(sup_pca)

sup_avg_pca$loadings

avg_first_two_components <- predict(sup_avg_pca, newdata = df_supervised_avg[, c(2, 4:8)])[, 1:2]

df_sup_avg_pca <- cbind(df_supervised_avg, avg_first_two_components)


#Regresión model (linear)
avg_linear_reg_model<- lm(cancer_death_rate ~ Comp.1 + Comp.2, data = df_sup_avg_pca)

summary(avg_linear_reg_model)

# Plot depdendant variable with each independant variable
ggplot(data=df_sup_avg_pca, aes(x=Comp.1, y=cancer_death_rate)) + geom_point() +
  geom_abline(intercept=0.157333, slope=0.043852, color="red")

ggplot(data=df_sup_avg_pca, aes(x=Comp.2, y=cancer_death_rate)) + geom_point() +
  geom_abline(intercept=0.157333, slope=-0.001925, color="red")

# Predictions and residuals
avg_predictions <- predict(avg_linear_reg_model, newdata=df_sup_avg_pca)

avg_residuals <- df_sup_avg_pca$cancer_death_rate-avg_predictions

qqnorm(avg_residuals)
qqline(avg_residuals)

R2_avg = R2(avg_predictions, df_sup_avg_pca$cancer_death_rate)
RMSE_avg = RMSE(avg_predictions, df_sup_avg_pca$cancer_death_rate)  
MAE_avg = MAE(avg_predictions, df_sup_avg_pca$cancer_death_rate)

cat("R^2 =", R2_avg, "\n")
cat("RMSE =", RMSE_avg, "\n")
cat("MAE =", MAE_avg, "\n")


# Final check, eliminate not significant variables
check_avg_pca <- lm(cancer_death_rate ~ cancer_affection_rate + rate_50_over + main_cancers + public_health_spe + 
                        hdi + hog_ideology_numeric, data = df_sup_avg_pca)

summary(check_avg_pca) # Eliminate main_cancers and ideology

df_new_avg_pca <- df_sup_avg_pca[c(1:4, 6,7)]

corr_sup_new_avg_matrix <- cor(df_new_avg_pca[,c(2, 4:6)] %>% dplyr::select_if(is.numeric))

ggcorrplot(corr_sup_new_avg_matrix, type = "lower", outline.color = "white", lab = TRUE,
           colors = c("darkred","#FFFFE0","darkblue")) +
  labs(title = "Correlation Heatmap")

# PCA for supervised, excluding the dependant variable
sup_new_avg_autoval <- eigen(corr_sup_new_avg_matrix)

plot(sup_new_avg_autoval$values, type="b", main="Principal Components", xlab="Number of Component", ylab="Eigenvalues")
abline(h=1, lwd=3, col="darkred")

sup_new_avg_pca <-princomp(df_new_avg_pca[,c(2, 4:6)], cor=TRUE)
summary(sup_new_avg_pca)

sup_new_avg_pca$loadings

new_avg_first_two_components <- predict(sup_new_avg_pca, newdata = df_new_avg_pca[, c(2, 4:6)])[, 1:2]

df_new_avg_pca <- cbind(df_new_avg_pca, new_avg_first_two_components)

#Regresión model (linear)
new_avg_linear_reg_model<- lm(cancer_death_rate ~ Comp.1, data = df_new_avg_pca)

summary(new_avg_linear_reg_model)

# Predictions and residuals
new_avg_predictions <- predict(new_avg_linear_reg_model, newdata=df_new_avg_pca)

new_avg_residuals <- df_new_avg_pca$cancer_death_rate-new_avg_predictions

qqnorm(new_avg_residuals)
qqline(new_avg_residuals)

RMSE(new_avg_residuals, df_new_avg_pca$cancer_death_rate)


# Decision Tree original dataset
#Training and Testing Data split
set.seed(123)
partition = createDataPartition(df_supervised$cancer_death_rate, p = 0.70, list = FALSE)
train_set = df_supervised[partition, ]  
test_set = df_supervised[-partition, ]

train_tree = rpart(cancer_death_rate ~ hog_ideology_numeric+cancer_affection_rate+main_cancers+public_health_spe+hdi,
                   data = train_set)

printcp(train_tree)

prediction = predict(train_tree, test_set)

r_2 <- (prediction - test_set$cancer_death_rate)^2

mse <- mean(r_2)

RMSE(prediction, test_set$cancer_death_rate) # RMSE

1 - mse / var(test_set$cancer_death_rate) # Accuracy

# Plot
tree = rpart(cancer_death_rate ~ hog_ideology_numeric+cancer_affection_rate+main_cancers+public_health_spe+hdi,
             cp=0.01, data = df_supervised)

rpart.plot(tree)

# Cross validation
train_control <- trainControl(method = "repeatedcv",   # Use cross validation
                              number = 10,             # Use 10 partitions
                              repeats = 10)

tune_grid = expand.grid(cp=c(0.01))

validated_tree <- train(cancer_death_rate ~ hog_ideology_numeric+cancer_affection_rate+main_cancers+public_health_spe+hdi,
                        data=df_supervised,                 # Data set
                        method="rpart",                     # Model type(decision tree)
                        trControl= train_control,           # Model control options
                        tuneGrid = tune_grid) 

validated_tree

# Decision Tree average dataset
#Training and Testing Data split
set.seed(123)
partition_2 = createDataPartition(df_supervised_avg$cancer_death_rate, p = 0.70, list = FALSE)
train_set_2 = df_supervised_avg[partition_2, ]  
test_set_2 = df_supervised_avg[-partition_2, ]

train_tree_2 = rpart(cancer_death_rate ~ hog_ideology_numeric+cancer_affection_rate+main_cancers+public_health_spe+hdi,
                   data = train_set_2)

printcp(train_tree_2)

prediction_2 = predict(train_tree_2, test_set_2)

r_2_2 <- (prediction_2 - test_set_2$cancer_death_rate)^2

mse_2 <- mean(r_2_2)

RMSE(prediction_2, test_set_2$cancer_death_rate) # RMSE

1 - mse_2 / var(test_set_2$cancer_death_rate) # Accuracy


# Plot
tree_2 = rpart(cancer_death_rate ~ hog_ideology_numeric+cancer_affection_rate+main_cancers+public_health_spe+hdi,
                   cp=0.01, data = df_supervised_avg)

rpart.plot(tree_2)

# Cross validation
train_control_2 <- trainControl(method = "repeatedcv", number = 10)

tune_grid_2 = expand.grid(cp=c(0.01))

validated_tree_2 <- train(cancer_death_rate ~ hog_ideology_numeric+cancer_affection_rate+main_cancers+public_health_spe+hdi,
                        data=df_supervised_avg,                 # Data set
                        method="rpart",                     # Model type(decision tree)
                        trControl= train_control_2,           # Model control options
                        tuneGrid = tune_grid) 

validated_tree_2


# RANDOM FOREST
# Random forest (original df)
rf_cancer_data <- randomForest(cancer_death_rate ~ cancer_affection_rate+main_cancers+public_health_spe+hdi+hog_ideology_numeric,
                        data=train_set, ntree=1000, importance = TRUE)

rf_cancer_data

varImpPlot(rf_cancer_data)

rf_cancer_data$importance

# Random forest (original df without ideology)
rf_cancer_data_woideo <- randomForest(cancer_death_rate ~ cancer_affection_rate+main_cancers+public_health_spe+hdi,
                               data=train_set, ntree=1000, importance = TRUE)

rf_cancer_data_woideo # Improves, but I'll continue with ideology to understand results and it's impact

varImpPlot(rf_cancer_data_woideo)

rf_cancer_data_woideo$importance

# Random forest (average)
rf_cancer_data_avg <- randomForest(cancer_death_rate ~ cancer_affection_rate+main_cancers+public_health_spe+hdi+hog_ideology_numeric,
                               data=train_set_2, ntree=1000, importance = TRUE)

rf_cancer_data_avg # Without ideology doesn't improve too much, but it's better

varImpPlot(rf_cancer_data_avg)

rf_cancer_data_avg$importance

# Continue with original dataset
#Training random forest with 10 fold cross validation
rf_fit<-train(cancer_death_rate ~ cancer_affection_rate+main_cancers+public_health_spe+hdi+hog_ideology_numeric,
              data=train_set, method="rf",n_estimators=1000, 
              importance=TRUE, trControl=trainControl(method = "cv", number=10))

varImp(rf_fit)
varImpPlot(rf_fit$finalModel)

#Preticion and evaluation of random forest with cv
rf_predict <- predict(rf_fit$finalModel, test_set)
compare_rf <- data.frame(test_set$cancer_death_rate,rf_predict)
rf_RMSE <- sqrt(mean((test_set$cancer_death_rate - rf_predict)^2))
rf_RMSE 


## Ranger library
ranger_fit<-train(cancer_death_rate ~ cancer_affection_rate+main_cancers+public_health_spe+hdi+hog_ideology_numeric,
                  data=train_set, method="ranger", num.trees = 1000, importance="impurity",
                  trControl=trainControl(method = "cv", number=10))
ranger_fit

varImp(ranger_fit)

#Prediction and evaluation of ranger
ranger_predict <- predict(ranger_fit, test_set)
ranger_RMSE <- sqrt(mean((test_set$cancer_death_rate - ranger_predict)^2))
ranger_RMSE #Similar to random forest

