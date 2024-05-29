library(data.table)
library(readxl)
library(tidyr)

#setwd("") / set relative working directory

party_2020 <- fread("Data/2020_party.csv", na.strings = c(NA_character_, ""))
names(party_2020) <- c("District", "Panchayat", "Ruling_Party")
party_2020 <- party_2020[! is.na(Ruling_Party)]

party_2015 <- fread("Data/2015_party.csv", na.strings = c(NA_character_, ""))
names(party_2015) <- c("District", "Panchayat", "Ruling_Party")
party_2015 <- party_2015[! is.na(Ruling_Party)]

coalitions <- read_excel("Data/Kerala Coalitions.xlsx")

coa_2020 <- merge(party_2020, coalitions, by.x = "Ruling_Party", by.y = "Party")
coa_2020[, Ruling_Party := NULL]
names(coa_2020) <- c("District", "Panchayat", "Coalition_2020")
coa_2015 <- merge(party_2015, coalitions, by.x = "Ruling_Party", by.y = "Party")
coa_2015[, Ruling_Party := NULL]
names(coa_2015) <- c("District", "Panchayat", "Coalition_2015")

party_dt <- merge(coa_2015, coa_2020, by = c("District", "Panchayat"))
party_dt <- party_dt[!(Coalition_2015 == "INDEPENDENT" | Coalition_2015 == "NDA" |
                         Coalition_2020 == "INDEPENDENT" | Coalition_2020 == "NDA")]


files <- c(list.files("Data/Kannur", full.names = TRUE),
           list.files("Data/Palakkad", full.names = TRUE),
           list.files("Data/Wayanad", full.names = TRUE),
           list.files("Data/Alappuzha", full.names = TRUE),
           list.files("Data/Ernakulam", full.names = TRUE),
           list.files("Data/Idukki", full.names = TRUE),
           list.files("Data/Kasargod", full.names = TRUE),
           list.files("Data/Kollam", full.names = TRUE),
           list.files("Data/Kottayam", full.names = TRUE),
           list.files("Data/Kozhikode", full.names = TRUE),
           list.files("Data/Malappuram", full.names = TRUE),
           list.files("Data/Pathanamthitta", full.names = TRUE),
           list.files("Data/Thiruvananthapuram", full.names = TRUE),
           list.files("Data/Thrissur", full.names = TRUE))
names(files) <- basename(files)
tables <- lapply(files, read_xlsx)
dist_exp_dt <- rbindlist(tables, idcol = "filename", fill = TRUE)
dist_exp_dt <- separate(dist_exp_dt, col = "filename",
                        into = c("Expend", "District", "Year"))
dist_exp_dt <- as.data.table(dist_exp_dt)
dist_exp_dt <- dist_exp_dt[!is.na(LB_Name) & LB_Type == "Grama Panchayat"]
dist_exp_dt <- dist_exp_dt[,Expend := NULL]

dist_exp_dt[, Marg_Spend_Perc := ((as.numeric(SCP) + as.numeric(TSP))*100) / as.numeric(Total) ]

pop_dt <- read_xlsx("Data/Panchayat Population.xlsx")
pop_dt <- as.data.table(pop_dt)

data_temp <- merge(dist_exp_dt, party_dt,
                   by.x = c("District", "LB_Name"),
                   by.y = c("District", "Panchayat"))
data_temp <- merge(data_temp, pop_dt, by.x = "LB_Name", by.y = "Panchayat")
data_temp <- data_temp[, Rul_Coa := ifelse(Year < 2020, Coalition_2015,Coalition_2020),]
data_temp <- data_temp[, is_LDF := ifelse(Rul_Coa == "LDF", TRUE, FALSE)]
data_temp <- data_temp[, cov_yr := ifelse(Year == 2020 | Year == 2021, TRUE, FALSE),]
reg_data <- data_temp[,c("Year", "District.x", "LB_Name", "Marg_Spend_Perc", "is_LDF", "Marg_Pop","cov_yr")]
names(reg_data) <- c("Year", "District","LB_Name", "Marg_Spend_Perc", "is_LDF", "Tot_Marg_Pop","cov_yr")

model <- lm(Marg_Spend_Perc ~ is_LDF + cov_yr + Tot_Marg_Pop, reg_data)

summary(model)

saveRDS(reg_data, file = "full_reg_data.rds")
