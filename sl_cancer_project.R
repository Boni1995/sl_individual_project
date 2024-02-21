#Load datasets
n_cancer <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\number-of-people-with-cancer.csv")
d_cancer_type <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\total-cancer-deaths-by-type.csv")
d_cancer_age <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\cancer-deaths-by-age.csv")
health_exp <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\public-health-expenditure-share-gdp.csv")
hd_index <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\sl_individual_project\\Datasets\\human-development-index.csv")

#Change column names
colnames(n_cancer)
colnames(n_cancer) <- c("entity", "code", "year", "n_people")

colnames(d_cancer_type)
colnames(d_cancer_type) <- c("entity", "code", "year", "liver", "kidney", "lip_oral_cavity",
                             "tracheal_bronch_lung", "larynx", "gallblad_biliary", "skin", "leukemia",
                             "hodgkin", "myeloma", "others", "breast", "prostate", "thyroid", "stomach",
                             "bladder", "uterine", "ovarian", "cervical", "brain_cns", "non_hodgkin",
                             "pancreatic", "esophageal", "testicular", "nasopharynx", "other_pharynx",
                             "colon_rectum", "non_melanoma_skin", "mesothelioma")

colnames(d_cancer_age)
colnames(d_cancer_age) <- c("entity", "code", "year", "70_over", "50_69", "15_49", "5_14", "under_5")

colnames(health_exp)
colnames(health_exp) <- c("entity", "code", "year", "public_health_exp")

colnames(hd_index)
colnames(hd_index) <- c("entity", "code", "year", "hdi")

#Join all datasets


#Check pacients dataset

