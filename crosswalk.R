library(dplyr)

# read all UCSF files together
dir <- "/Users/yiweyshieh/OneDrive - med.cornell.edu/UKB_MMA/psam"
fnames <- list.files(dir, full.names=T)
ukb_ucsf <- do.call(rbind, lapply(fnames, read.table))
colnames(ukb_ucsf) <- c("FID", "IID", "sex")

# read individual files
#ukb_ucsf_mix <- read.table(paste0(dir, "/ukb.qc.chr1.03292018.mixed.psam"))

# read Stanford files
ukb_stan <- read.table(paste0(path, "ukb55891_cal_chr1_v2_s488128.fam"))

