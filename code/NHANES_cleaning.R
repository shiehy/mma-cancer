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
creatinine_2001 <- read_xpt("./data/CREATININE_B_2001.XPT")
creatinine_2003 <- read_xpt("./data/CREATININE_C_2003.XPT")
creatinine_2011 <- read_xpt("./data/CREATININE_G_2011.XPT")
creatinine_2013 <- read_xpt("./data/CREATININE_H_2013.XPT")
# creatinine_second_2001 <- read_xpt("./data/CREATININE_second_2001.XPT")
demo_2001 <- read_xpt("./data/DEMO_B_2001.XPT")
demo_2003 <- read_xpt("./data/DEMO_C_2003.XPT")
demo_2011 <- read_xpt("./data/DEMO_G_2011.XPT")
demo_2013 <- read_xpt("./data/DEMO_H_2013.XPT")

mma_2001 <- mma_2001_o %>% 
  select(SEQN, LB2MMA) %>% 
  rename(MMA = LB2MMA) %>% 
  mutate(MMA = MMA*1000) %>%  # convert to nmol/L
  set_variable_labels(MMA = "Methylmalonic acid (nmol/L)") %>% 
  mutate(year = "2001-2002")
  
mma_2003 <- mma_2003 %>% 
  select(SEQN, LBXMMA) %>% 
  rename(MMA = LBXMMA) %>% 
  mutate(MMA = MMA*1000) %>% # convert to nmol/L from umol/L
  set_variable_labels(MMA = "Methylmalonic acid (nmol/L)") %>% 
  mutate(year = "2003-2004")

mma_2011 <- mma_2011 %>% 
  select(SEQN, LBXMMASI) %>% 
  rename(MMA = LBXMMASI) %>% 
  set_variable_labels(MMA = "Methylmalonic acid (nmol/L)") %>% 
  mutate(year = "2011-2012")

mma_2013 <- mma_2013 %>% 
  select(SEQN, LBXMMASI) %>% 
  rename(MMA = LBXMMASI) %>% 
  set_variable_labels(MMA = "Methylmalonic acid (nmol/L)") %>% 
  mutate(year = "2013-2014")


b12_2001 <- mma_2001_o %>% 
  select(SEQN, LB2B12) %>% 
  rename(B12 = LB2B12) %>% 
  set_variable_labels(B12 = "Serum B12 (pg/mL)") %>% 
  mutate(year = "2001-2002")

b12_2003 <- b12_2003 %>% 
  select(SEQN, LBXB12) %>% 
  rename(B12 = LBXB12) %>% 
  set_variable_labels(B12 = "Serum B12 (pg/mL)") %>% 
  mutate(year = "2003-2004")

b12_2011 <- b12_2011 %>% 
  select(SEQN, LBXB12) %>% 
  rename(B12 = LBXB12) %>% 
  set_variable_labels(B12 = "Serum B12 (pg/mL)") %>% 
  mutate(year = "2011-2012")

b12_2013 <- b12_2013 %>% 
  select(SEQN, LBDB12) %>% 
  rename(B12 = LBDB12) %>% 
  set_variable_labels(B12 = "Serum B12 (pg/mL)") %>% 
  mutate(year = "2013-2014")

creatinine_2001 <- creatinine_2001 %>% 
  select(SEQN, URXUCR) %>% 
  rename(CREATININE = URXUCR) %>% 
  set_variable_labels(CREATININE = "Creatinine (mg/dL)") %>% 
  mutate(year = "2001-2002")

creatinine_2003 <- creatinine_2003 %>% 
  select(SEQN, URXUCR) %>% 
  rename(CREATININE = URXUCR) %>% 
  set_variable_labels(CREATININE = "Creatinine (mg/dL)") %>% 
  mutate(year = "2003-2004")

creatinine_2011 <- creatinine_2011 %>% 
  select(SEQN, URXUCR) %>% 
  rename(CREATININE = URXUCR) %>% 
  set_variable_labels(CREATININE = "Creatinine (mg/dL)") %>% 
  mutate(year = "2011-2012")

creatinine_2013 <- creatinine_2013 %>% 
  select(SEQN, URXUCR) %>% 
  rename(CREATININE = URXUCR) %>% 
  set_variable_labels(CREATININE = "Creatinine (mg/dL)") %>% 
  mutate(year = "2013-2014")

demo_2001 <- demo_2001 %>% 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1) %>% 
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
         age = case_when(age < 85 ~ as.character(age),
                         age == 85 ~ ">= 85 years"),
         year = "2001-2002")
  
demo_2003 <- demo_2003 %>% 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1) %>% 
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
         age = case_when(age < 85 ~ as.character(age),
                         age == 85 ~ ">= 85 years"),
         year = "2003-2004")
  
demo_2011 <- demo_2011 %>% 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1) %>% 
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
         age = case_when(age < 85 ~ as.character(age),
                         age == 85 ~ ">= 85 years"),
         year = "2011-2012")

demo_2013 <- demo_2013 %>% 
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1) %>% 
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
         age = case_when(age < 85 ~ as.character(age),
                         age == 85 ~ ">= 85 years"),
         year = "2013-2014")

demo <- rbind(demo_2001, demo_2003, demo_2011, demo_2013)
b12 <- rbind(b12_2001, b12_2003, b12_2011, b12_2013)
creatinine <- rbind(creatinine_2001, creatinine_2003, creatinine_2011,
                    creatinine_2013)  
mma <- rbind(mma_2001, mma_2003, mma_2011, mma_2013) 


save(demo, b12, creatinine, mma, file = "./output/NHANES_dat.RData")

