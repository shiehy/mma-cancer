library(dplyr)
path <- "/Users/yiweyshieh/OneDrive - med.cornell.edu/UKB_MMA/"

ukb_ucsf <- read.table(paste0(path, "ukb.qc.chr1.03292018.psam"))
colnames(ukb_ucsf) <- c("FID", "IID", "sex")

ukb_stan <- read.table(paste0(path, "ukb55891_cal_chr1_v2_s488128.fam"))
