library(tidyverse)

# in unix, use this command to find colnum for breast: WORD="breast"; head -n1 ukPheno_2021-09-15.csv | tr "\"*,\"*" "\n" | grep -n $WORD
# then this command to subset: 
# head -n1 ukPheno_2021-09-15.csv > ukPheno_2021-09-15_breast.csv
# awk -F "\"*,\"*" '{if($327==1) {print}}' ukPheno_2021-09-15.csv >> ukPheno_2021-09-15_breast.csv
# can change out "breast" for preferred cancer site

### Clean phenotype file
datapath="/Volumes/Shieh-share$/MMA_Cancer/data/"
lung_all <- read.table(paste0(datapath, "lung/ukPheno_2022-03-09_lung.csv"), sep=",", header=T)
lung_nonsmall <- read.table(paste0(datapath, "lung/ukPheno_2022-03-09_lungnonsmall.csv"), sep=",", header=T)
lung_small <- read.table(paste0(datapath, "lung/ukPheno_2022-03-09_lungsmall.csv"), sep=",", header=T)

length(intersect(lung_all$eid, lung_nonsmall$eid)) # all nonsmall cell cases are contained in lung
length(intersect(lung_all$eid, lung_small$eid)) # all small cell cases are contained in lung
length(intersect(lung_small$eid, lung_nonsmall$eid)) # there is overlap of 19 participants with both nonsmall and small cell

phenos_nonsmall_all <- lung_nonsmall %>%
  select(eid, dxdate.1, dxage.1, behavior.1, 
         lungnonsmall_malig, lungnonsmall_dxdate_any, lungnonsmall_dxage_any, lungnonsmall_incid_any,lungnonsmall_first_malig, 
         lungnonsmall_dxdate_first, lungnonsmall_dxage_first,
         lungnonsmall_incid_first, lungnonsmall_first,
         cancer_death, lung_death, death_date, death_age, ethnicity) %>% 
  mutate_at(vars(eid), as.character) %>%
  mutate(lung_death=replace_na(lung_death, 0)) %>%
  mutate_at(vars(contains("date")), ymd) %>%
  mutate(end_fu = case_when(!is.na(death_date) ~ death_date,
                            TRUE ~ ymd("2021-3-15"))) %>%
  mutate(fu_time = end_fu - ymd(lungnonsmall_dxdate_any))

phenos_nonsmall_incident <- subset(phenos_nonsmall_all, lungnonsmall_incid_any == 1)


### Clean genotype file
raw.geno <- read.table(paste0(datapath, "merged_recode.raw"), header = T, check.names = F)[,-c(3:6)]

ukb.geno.mma <- raw.geno %>% 
  select(-IID) %>%
  rename(eid = FID) %>%
  mutate_at(vars(eid), as.character)

# quick QC for missingness of genotypes
library(naniar)
mv <- miss_var_summary(ukb.geno.mma)

### Clean PCs
load(paste0(datapath, "pc_ukbb.Rdata"))

pc_ukbb <- pc_ukbb %>% 
  mutate_at(vars(eid), as.character)

### Merge
merged_nonsmall_all <- phenos_nonsmall_all %>%
  inner_join(ukb.geno.mma, by="eid") %>%
  inner_join(pc_ukbb, by="eid")


### Save file for analysis in R notebook
save(merged_nonsmall_all, file=paste0(datapath, "UKB_nonsmall_MMAsnps.RData"))

