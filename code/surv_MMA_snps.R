# generates cleaned genotype file from genotype data (.raw)

library(tidyverse)
library(survival)
library(survminer)

### Read genotype file
datapath = "/Volumes/Shieh-share$/MMA_Cancer/"
raw.geno <- read.table(paste0(datapath, "data/merged_recode.raw"), header = T, check.names = F)[,-c(3:6)]

ukb.geno.mma <- raw.geno %>% 
  select(-IID) %>%
  rename(eid = FID) %>%
  mutate_at(vars(eid), as.character)


### Check allele frequencies
af.func <- function(x){
  sum(x, na.rm=T)/(2*nrow(ukb.geno.mma))
}

af.func(ukb.geno.mma$rs291466_A) # AFs: A=0.41, G=0.59; this matches up w/ GWAS and 1000 Genomes

### Load & merge cleaned UKB breast data
load("/Volumes/Shieh-share$/SNPTumor/data/processed/UKB_cleaned.RData")

length(intersect(ukb.noNA.combined_incident$eid, ukb.geno.mma$eid)) #confirm overlap

breast_mma <- inner_join(ukb.geno.mma, ukb.noNA.combined_incident, by="eid")

### Subset UKB to malignant BC, filter by necessary covar/SNPs
dat <- breast_mma %>%
  subset(behavior.1 == 3) %>%
  select(eid, breast_death, fu_time, breast_dxage_any, any_of(starts_with("PC")), any_of(starts_with("rs291466"))) %>%
  relocate(starts_with("rs"), .after=last_col()) %>%
  mutate(fu_yrs = fu_time/365)

### KM plot for rs291466 (G is effect allele -> higher MMA)
km <- with(dat, Surv(fu_yrs, breast_death))
km_pc_fit <- survfit(Surv(fu_yrs, breast_death)~rs291466_A, data=dat)

ggsurvplot(km_pc_fit, data = dat, 
           pval=TRUE, pval.method=T, conf.int=F,
           ylim=c(0.5,1), xlim=c(0,10), break.time.by=2, xlab = "Time (years)",
           pval.coord = c(8, 0.835), pval.method.coord = c(8, 0.85),
           legend = c(0.15, 0.25), legend.labs=c("GG", "AG", "AA"), legend.title="Genotype",
           risk.table = T, risk.table.height = 0.3, tables.theme=theme_survminer(), risk.table.fontsize=4,
           ggtheme=theme_classic(), palette=c("#482677FF", "#1F968BFF", "#FDE725FF"),
           font.x = c(14), font.y = c(14),
           font.tickslab = c(12))

### Sensitivity analyses
table(dat$breast_death) #544 deaths d/t breast cancer


