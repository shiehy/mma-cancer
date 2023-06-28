#### Clean NHANES MMA data ####

library(tidyverse)
library(lubridate)
library(gtsummary)
library(haven)
library(labelled)


mma_2001_o <- read_xpt("./data/MMA_B12_B_2001.XPT") # this data contains both mma and b12
mma_2003 <- read_xpt("./data/MMA_C_2003.XPT")
mma_2011 <- read_xpt("./data/MMA_G_2011.XPT")
mma_2013 <- read_xpt("./data/MMA_H_2013.XPT")
# mma_second_2001 <- read_xpt("./data/MMA_second_2001.XPT")
b12_2003 <- read_xpt("./data/B12_C_2003.XPT")
b12_2011 <- read_xpt("./data/B12_G_2011.XPT")
b12_2013 <- read_xpt("./data/B12_H_2013.XPT")
# b12_second_2001 <- read_xpt("./data/B12_second_2001.XPT")
creatinine_2001 <- read_xpt("./data/L40_B_2001.XPT")
creatinine_2003 <- read_xpt("./data/L40_C_2003.XPT")
creatinine_2011 <- read_xpt("./data/BIOPRO_G_2011.XPT")
creatinine_2013 <- read_xpt("./data/BIOPRO_H_2013.XPT")
# creatinine_second_2001 <- read_xpt("./data/CREATININE_second_2001.XPT")
demo_2001 <- read_xpt("./data/DEMO_B_2001.XPT")
demo_2003 <- read_xpt("./data/DEMO_C_2003.XPT")
demo_2011 <- read_xpt("./data/DEMO_G_2011.XPT")
demo_2013 <- read_xpt("./data/DEMO_H_2013.XPT")
cancer_2001 <- read_xpt("./data/MEDICAL_CONDITIONS_2001.XPT")
cancer_2003 <- read_xpt("./data/MEDICAL_CONDITIONS_2003.XPT")
cancer_2011 <- read_xpt("./data/MEDICAL_CONDITIONS_2011.XPT")
cancer_2013 <- read_xpt("./data/MEDICAL_CONDITIONS_2013.XPT")


mma_2001 <- mma_2001_o %>% 
  select(SEQN, LB2MMA) %>% 
  rename(MMA = LB2MMA) %>% 
  mutate(MMA = MMA*1000) %>%  # convert to nmol/L
  set_variable_labels(MMA = "Methylmalonic acid (nmol/L)")

mma_2003 <- mma_2003 %>% 
  select(SEQN, LBXMMA) %>% 
  rename(MMA = LBXMMA) %>% 
  mutate(MMA = MMA*1000) %>% # convert to nmol/L from umol/L
  set_variable_labels(MMA = "Methylmalonic acid (nmol/L)")

mma_2011 <- mma_2011 %>% 
  select(SEQN, LBXMMASI) %>% 
  rename(MMA = LBXMMASI) %>% 
  set_variable_labels(MMA = "Methylmalonic acid (nmol/L)")

mma_2013 <- mma_2013 %>% 
  select(SEQN, LBXMMASI) %>% 
  rename(MMA = LBXMMASI) %>% 
  set_variable_labels(MMA = "Methylmalonic acid (nmol/L)")


b12_2001 <- mma_2001_o %>% 
  select(SEQN, LB2B12) %>% 
  rename(B12 = LB2B12) %>% 
  set_variable_labels(B12 = "Serum B12 (pg/mL)")

b12_2003 <- b12_2003 %>% 
  select(SEQN, LBXB12) %>% 
  rename(B12 = LBXB12) %>% 
  set_variable_labels(B12 = "Serum B12 (pg/mL)")

b12_2011 <- b12_2011 %>% 
  select(SEQN, LBXB12) %>% 
  rename(B12 = LBXB12) %>% 
  set_variable_labels(B12 = "Serum B12 (pg/mL)") 

b12_2013 <- b12_2013 %>% 
  select(SEQN, LBDB12) %>% 
  rename(B12 = LBDB12) %>% 
  set_variable_labels(B12 = "Serum B12 (pg/mL)")

creatinine_2001 <- creatinine_2001 %>% 
  select(SEQN, LBDSCR) %>% 
  rename(CREATININE = LBDSCR) %>% 
  set_variable_labels(CREATININE = "Creatinine (mg/dL)")

creatinine_2003 <- creatinine_2003 %>% 
  select(SEQN, LBXSCR) %>% 
  rename(CREATININE = LBXSCR) %>% 
  set_variable_labels(CREATININE = "Creatinine (mg/dL)")

creatinine_2011 <- creatinine_2011 %>% 
  select(SEQN, LBXSCR) %>% 
  rename(CREATININE = LBXSCR) %>% 
  set_variable_labels(CREATININE = "Creatinine (mg/dL)")

creatinine_2013 <- creatinine_2013 %>% 
  select(SEQN, LBXSCR) %>% 
  rename(CREATININE = LBXSCR) %>% 
  set_variable_labels(CREATININE = "Creatinine (mg/dL)")

demo_2001 <- demo_2001 %>% 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1) %>% 
  mutate(year = "2001-2002")
  
demo_2003 <- demo_2003 %>% 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1) %>% 
  mutate(year = "2003-2004")
  
demo_2011 <- demo_2011 %>% 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1) %>% 
  mutate(year = "2011-2012")

demo_2013 <- demo_2013 %>% 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1) %>% 
  mutate(year = "2013-2014")


demo <- rbind(demo_2001, demo_2003, demo_2011, demo_2013)

demo <- demo %>% 
  rename(age = RIDAGEYR,
         race_ethnicity = RIDRETH1,
         gender = RIAGENDR) %>%
  mutate(race_ethnicity = case_when(race_ethnicity == 1 ~ "Mexican American",
                                    race_ethnicity == 2 ~ "Other Hispanic",
                                    race_ethnicity == 3 ~ "Non-Hispanic White",
                                    race_ethnicity == 4 ~ "Non-Hispanic Black",
                                    race_ethnicity == 5 ~ "Other race, including multi-racial"),
         gender = case_when(gender == 1 ~ "Male",
                            gender == 2 ~ "Female"),
         age_cat = case_when(age < 10 ~ "<10",
                             age >= 10 & age < 20 ~ "10-19",
                             age >= 20 & age < 30 ~ "20-29",
                             age >= 30 & age < 40 ~ "30-39",
                             age >= 40 & age < 50 ~ "40-49",
                             age >= 50 & age < 60 ~ "50-59",
                             age >= 60 & age < 70 ~ "60-69",
                             age >= 70 & age < 80 ~ "70-79",
                             age >= 80 ~ "80+"))


b12 <- rbind(b12_2001, b12_2003, b12_2011, b12_2013)
creatinine <- rbind(creatinine_2001, creatinine_2003, creatinine_2011,
                    creatinine_2013)  
mma <- rbind(mma_2001, mma_2003, mma_2011, mma_2013) 


# Clean cancer data

cancer_2001 <- cancer_2001 %>% 
  select(SEQN, MCQ220, MCQ230A, MCQ230B, MCQ230C, MCQ230D, MCQ240N)
cancer_2003 <- cancer_2003 %>% 
  select(SEQN, MCQ220, MCQ230A, MCQ230B, MCQ230C, MCQ230D, MCQ240N)
cancer_2011 <- cancer_2011 %>% 
  select(SEQN, MCQ220, MCQ230A, MCQ230B, MCQ230C, MCQ230D, MCQ240N)
cancer_2013 <- cancer_2013 %>% 
  select(SEQN, MCQ220, MCQ230A, MCQ230B, MCQ230C, MCQ230D, MCQ240N)

cancer <- rbind(cancer_2001, cancer_2003, cancer_2011, cancer_2013)

cancer <- cancer %>% 
  mutate(cancer_malignancy = case_when(MCQ220 == 1 ~ "Yes",
                                       MCQ220 == 2 ~ "No",
                                       MCQ220 == 7 ~ "Refused",
                                       MCQ220 == 9 ~ "Don't know"),
         lung_cancer = if_else(MCQ230A == 23 | MCQ230B == 23 | MCQ230C == 23 |
                                 MCQ230D == 23, 1, 0),
         ) %>% 
  rename(age_lung_cancer_diag = MCQ240N) %>% 
  select(SEQN, cancer_malignancy, lung_cancer, age_lung_cancer_diag)


nhanes_dat <- demo %>% 
  full_join(b12, by = "SEQN") %>% 
  full_join(creatinine, by = "SEQN") %>% 
  full_join(mma, by = "SEQN") %>% 
  filter(!is.na(B12) | !is.na(CREATININE) | !is.na(MMA)) %>% # remove data points without mma, b12, or creatinine data
  left_join(cancer, by = "SEQN") %>% 
  mutate(complete_data = if_else(!is.na(B12) & !is.na(CREATININE) & !is.na(MMA), 1, 0))
  
save(nhanes_dat, file = "./output/NHANES_dat.RData")


# Clean lung cancer patient data

cancer_pts <- read_xlsx("./data/MMA Treatment Naive_June 20_2023.xlsx")

cancer_pts <- cancer_pts %>% 
  rename(SEQN = Label,
         MMA = `MMA (umol/L)`,
         age = Age,
         CREATININE = creatinine) %>% 
  select(SEQN, age, MMA, B12, CREATININE) %>% 
  mutate(cancer = 1)


save(cancer_pts, file = "./output/cancer_pts.RData")
