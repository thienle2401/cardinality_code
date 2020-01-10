library(readxl)
library(data.table)
library(janitor)
library(scales)
library(radiant.data)
library(ggplot2)

source("funs.R")

DATA_PATH <- "data/RFM_Analysis.xlsx"
file_sheets <- excel_sheets(DATA_PATH)


data <- lapply(file_sheets, function(sheet, data_path){
  
  sheet_data <- read_xlsx(data_path, sheet)
  setDT(sheet_data)
  setnames(sheet_data, make_clean_names(names(sheet_data), case = "snake"))
  
}, data_path = DATA_PATH)

names(data) <- file_sheets


# remove monetary_score and add prefix for corresponding dataset
# these operations are by references, so minimal cost

data_keys <- c("msisdn", "date_most_recent", "recency_score", "frequency_score", "transaction_count")

check_all_equal(data, data_keys)

data_keys <- c("msisdn", "date_most_recent", "recency_score", "frequency_score", "transaction_count")

setkeyv(data[["monetary_uplink"]], data_keys)
setkeyv(data[["monetary_downlink"]], data_keys)
setkeyv(data[["monetary_uplinkRTT"]], data_keys)
setkeyv(data[["monetary_downlinkRTT"]], data_keys)


combined_ds <- Reduce(merge, data)


combined_ds[, a := obj_fun_5G_profile(.SD), .SDcols = c("ul_vl_amount", "dl_vl_amount", "ul_rtt_amount", "dl_rtt_amount")]
combined_ds[, monetary_score := xtile(a, 5)]
combined_ds[order(-recency_score, -frequency_score, -monetary_score, -a)]


summary(combine_ds[, .(ul_vl_amount, dl_vl_amount, ul_rtt_amount, dl_rtt_amount)])


