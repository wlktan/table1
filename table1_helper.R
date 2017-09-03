# Load packages
library(data.table)
library(compareGroups)
library(RCurl)
library(dplyr)

ReadData <- function(url){
  # Read in data from an URL
  file_name <- getURL(url, 
               ssl.verifyhost = FALSE, 
               ssl.verifypeer = FALSE) 
  df <- read.csv(textConnection(file_name), header=T)
  return(df)
}

cerv.df <- fread('kag_risk_factors_cervical_cancer.csv')
cerv.df <- apply(cerv.df, c(1,2), function(x) 
  ifelse(x == '?', NA, as.numeric(x))) %>% # standardize NA
  as.data.frame(.)
  
names(cerv.df) <- tolower(names(cerv.df)) %>%
  gsub("\\(|\\)|\\/|\\-", "", ., perl = TRUE) %>%
  gsub("\\s|\\:", "_", ., perl = TRUE)

names(cerv.df)
View(cerv.df)  

# Descriptive Tables ------------------------------------------------------

# Overall table w/o outcome (biopsy)
compareGroups(~ .-biopsy, data = cerv.df) %>%
  createTable()

# Method = 1 (normal) by default; specify factor variables by method = 3
compareGroups(~ .-biopsy, data = cerv.df,
              method = c(number_of_sexual_partners = 3,
                         num_of_pregnancies = 3,
                         smokes = 3,
                         hormonal_contraceptives = 3,
                         iud = 3,
                         stds = 3,
                         stds_number = 3,
                         stds_condylomatosis = 3,
                         stds_cervical_condylomatosis = 3,
                         stds_vaginal_condylomatosis = 3,
                         stds_vulvoperineal_condylomatosis = 3,
                         stds_syphilis = 3,
                         stds_pelvic_inflammatory_disease = 3,
                         stds_genital_herpes = 3,
                         stds_molluscum_contagiosum = 3,
                         stds_aids = 3,
                         stds_hiv = 3,
                         stds_hepatitis_b = 3,
                         stds_hpv = 3,
                         stds__number_of_diagnosis = 3,
                         dx_cancer = 3,
                         dx_cin = 3,
                         dx_hpv = 3,
                         dx = 3,
                         hinselmann = 3,
                         schiller = 3,
                         citology = 3)) %>%
  createTable()

# Table by biopsy status
compareGroups(biopsy ~ .-biopsy, data = cerv.df,
              method = c(number_of_sexual_partners = 3,
                         num_of_pregnancies = 3,
                         smokes = 3,
                         hormonal_contraceptives = 3,
                         iud = 3,
                         stds = 3,
                         stds_number = 3,
                         stds_condylomatosis = 3,
                         stds_cervical_condylomatosis = 3,
                         stds_vaginal_condylomatosis = 3,
                         stds_vulvoperineal_condylomatosis = 3,
                         stds_syphilis = 3,
                         stds_pelvic_inflammatory_disease = 3,
                         stds_genital_herpes = 3,
                         stds_molluscum_contagiosum = 3,
                         stds_aids = 3,
                         stds_hiv = 3,
                         stds_hepatitis_b = 3,
                         stds_hpv = 3,
                         stds__number_of_diagnosis = 3,
                         dx_cancer = 3,
                         dx_cin = 3,
                         dx_hpv = 3,
                         dx = 3,
                         hinselmann = 3,
                         schiller = 3,
                         citology = 3)) %>%
  createTable()

