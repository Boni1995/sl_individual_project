# Libraries
library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)

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
  Minimo = c(
    min(death_cause$year),
    min(population$year),
    min(n_cancer$year),
    min(d_cancer_type$year),
    min(d_cancer_age$year),
    min(health_spe$year),
    min(hd_index$year),
    min(ideology$year)
  ),
  Maximo = c(
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

df_project <- df_project[, c("entity", "code", "year")] # Reorder columnas

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
# ruta_archivo <- "C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\Dataset check\\check_final_ds.xlsx"
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

barplot(as.numeric(unlist(number_by_ideology)), col = c("#8B0000", "#FFFFE0", "#008000") , width = c(50, 50), las=2, space = 1,
        names.arg= c("Left", "Center", "Right"), cex.names=0.75,
        main = "Number of records by ideology", ylab = "N° of records", xlab = "Ideology",
        ylim = c(0, 1350))

# Check evolution of ideology by year
