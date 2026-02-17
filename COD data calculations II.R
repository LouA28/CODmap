#Load libraries

library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(countrycode)
library(tidyr)
library(data.table)
library(readr)

## convert the original files from .csv to .xlsx [ONLY RUN ONCE]

csv_files <- list.files("Raw Data/", pattern = "\\.csv$", full.names = TRUE)

for (file in csv_files) {
  data <- read_csv(file)
  out_file <- sub("\\.csv$", ".xlsx", file)
  writexl::write_xlsx(data, out_file)
}

## Dont forget to delete the original csv files before continuing with the rest of the code


## Importing data and combining all monthly individual files into one data frame - (note files are in .xlsx format)
files_excel <- list.files(path = "Raw Data/", pattern = '.xlsx', full.names = TRUE)

df_list <- lapply(files_excel,  read_excel)

data <- rbindlist(df_list)


## read in class and PUR_elig files

#Class file is combination of 2022 and 2023 CN systems.

class <- read_excel("classifications2022to26.xlsx")

PUR_elig <- read_excel("PUR eligibility.xlsx")


## Create new column in the PUR dataframe called "Combo code" to merge the eligibility codes for each line together

data$combocode <- paste(data$eligibility,data$use, sep = "")

## Extract Year and month
data$Year <- substr(data$perref, start = 1, stop = 4)
data$Month <- substr(data$perref, start = 5, stop = 6)

## Join the files together 
PUR_import <- inner_join(data, PUR_elig, by = c("eligibility", "use"))

#Manually check if there are any lost records
#Katie added e5uzz to the list based on a) it appears in the data but not in DBT's table b) all other uzzs are "No" and "No" in the table

## Add zeros and creating HS2/HS6 codes + filter out statreg >1
PUR_import <- PUR_import %>% 
  mutate(CN8 = ifelse(str_length(PUR_import$comcode) == 7, 
                      str_c("0", PUR_import$comcode), PUR_import$comcode))

## get rid of all the columns we don't need e.g. stat reg, eligibility, use, netmass and suppunit

PUR_import <- PUR_import %>%
  mutate(HS2_code = substr(CN8,1 ,2)) %>%
  filter(HS2_code <= 24 & statreg <= 1)

#Write an excel file for manual QA
writexl::write_xlsx(PUR_import, paste0("CODdata/UK import data for manual QA",Sys.Date(),".xlsx"))

# Final PUR calculations

final_importPUR <- PUR_import %>%
  mutate(cooalpha = ifelse(is.na(cooalpha), "BK", cooalpha)) %>%
  group_by(
    cooalpha,        # Country of origin
    codalpha,        # Country of dispatch
    CN8,             # Product code
    Year,            # Year only (no Month for annual)
    combocode,       # Keep tariff/eligibility code
    eligibility_name,
    use_name
  ) %>%
  summarise(
    Pref_Trade = sum(statvalue[PUR_numerator == "Yes"], na.rm = TRUE),
    Eligible_Trade = sum(statvalue[PUR_denominator == "Yes"], na.rm = TRUE),
    PUR = ifelse(Eligible_Trade > 0,
                 (Pref_Trade / Eligible_Trade) * 100,
                 NA_real_),
    Total_imp = sum(statvalue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, cooalpha, codalpha, CN8)


#Replacing NA values with "Not Eligible"
#This means that there were imports, but that they got filtered out on the basis of the PUR methodology table
#e.g. they were MFN or otherwise ineligible.

final_importPUR$PUR <- ifelse(is.na(final_importPUR$PUR),"Not Eligible", final_importPUR$PUR)

#Add in the HS descriptions
final_importPUR <- inner_join(final_importPUR, class, by = c("CN8"))

#Check if any CN8s in the data aren't in our classifications file
# lost_records2 <- final_importPUR %>%
#   filter(!CN8 %in% class$CN8)


## Add in the country names for country of origin

final_importPUR$country_origin <- countrycode(final_importPUR$cooalpha, origin = "genc2c", destination = "country.name")
final_importPUR$country_origin <- if_else(final_importPUR$cooalpha == "PS", "Palestine", final_importPUR$country_origin)
final_importPUR$country_origin <- if_else(final_importPUR$cooalpha == "XS", "Serbia", final_importPUR$country_origin)
final_importPUR$country_origin <- if_else(final_importPUR$cooalpha == "UM", "United States Minor Outlying Islands", final_importPUR$country_origin)
final_importPUR$country_origin <- if_else(final_importPUR$cooalpha == "XC", "Ceuta", final_importPUR$country_origin)


## Add in the country names for country of dispatch

final_importPUR$country_dispatch <- countrycode(final_importPUR$codalpha, origin = "genc2c", destination = "country.name")
final_importPUR$country_dispatch <- if_else(final_importPUR$codalpha == "PS", "Palestine", final_importPUR$country_dispatch)
final_importPUR$country_dispatch <- if_else(final_importPUR$codalpha == "XS", "Serbia", final_importPUR$country_dispatch)
final_importPUR$country_dispatch <- if_else(final_importPUR$codalpha == "UM", "United States Minor Outlying Islands", final_importPUR$country_dispatch)
final_importPUR$country_dispatch <- if_else(final_importPUR$codalpha == "XC", "Ceuta", final_importPUR$country_dispatch)


final_importPUR <- final_importPUR[order(final_importPUR$CN8), ]

col_order <- c("Year", "cooalpha","codalpha", "country_origin","country_dispatch","combocode",
               "eligibility_name","use_name","HS2","HS2_desc","HS4", 
               "HS4_desc", "HS6", "HS6_desc", "CN8", "CN8_desc",
               "ffd_desc", "ffdplus_desc", "Agri", "HS_Section", "Sector", "DIV", 
               "DIV description", "HS2 combined", "HS4 combined", "HS6 combined", 
               "SITC combined","Total_imp", "Pref_Trade", "Eligible_Trade","PUR")

final_importPUR <- final_importPUR[, col_order]


################################ SAVE FILES ######################################


# #Write the output file to be uploaded into the app (Excel & RDS)
writexl::write_xlsx(final_importPUR, paste0("CODdata/Cod_data",Sys.Date(),".xlsx"))

saveRDS(final_importPUR, paste0("CODdata/Cod_data", Sys.Date(),".RDS"))


#Write an output file for analysing type of preference used. (Excel & RDS)
# writexl::write_xlsx(PUR_import, paste0("CODdata/PUR_type of preference",Sys.Date(),".xlsx"))
# 
# saveRDS(PUR_import, paste0("CODdata/PUR_type of preference", Sys.Date(),".RDS"))



