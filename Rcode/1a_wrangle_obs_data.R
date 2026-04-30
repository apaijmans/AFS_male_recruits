# -----------------------------------------------------------
# Author: A.J. Paijmans
#
# Script Name: 1a_wrangle_obs_data.R
#
# Purpose: This script is written to import all observational 
# location data of the adult males. This contains all males 
# (observed only as well as observed and tissue sampled). This 
# is then used to calculate the number of observed males per year.
# It also adds the SAM index to the observed counts.
# The saved df "n_observed_males.xlsx" is further used in 
# scripts "2_explore_data_n_males.R" and "3_stats_n_males".
#
# NB Start script at 335 if you want to skip loading the raw data!!!
# or 458 if you want to skip cleaning the data
#
# -----------------------------------------------------------


library(here)
library(readxl)
library(tidyverse)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Raw male location data 1994-2006  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# 1994-2006
read_excel_allsheets <- function(filename) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_names = F, col_type = "text"))
  #if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

raw_pos94 <- read_excel_allsheets(here("Data", "raw", "MaleLocations", "males & paternities 06_07_06.xls"))

# Change column names...
colnames(raw_pos94[[1]]) <- raw_pos94[[1]][2,]
colnames(raw_pos94[[1]])[5] <- "V1"
colnames(raw_pos94[[1]])[6] <- "V2"
colnames(raw_pos94[[2]]) <- raw_pos94[[2]][2,]
colnames(raw_pos94[[2]])[3] <- "V1"
colnames(raw_pos94[[2]])[4] <- "V2"
colnames(raw_pos94[[3]]) <- raw_pos94[[3]][2,]
colnames(raw_pos94[[3]])[3] <- "V1"
colnames(raw_pos94[[3]])[4] <- "V2"
colnames(raw_pos94[[4]]) <- raw_pos94[[4]][2,]
colnames(raw_pos94[[4]])[3] <- "V1"
colnames(raw_pos94[[4]])[4] <- "V2"
colnames(raw_pos94[[5]]) <- raw_pos94[[5]][2,]
colnames(raw_pos94[[5]])[3] <- "V1"
colnames(raw_pos94[[5]])[4] <- "V2"
colnames(raw_pos94[[6]]) <- raw_pos94[[6]][3,]
colnames(raw_pos94[[6]])[3] <- "V1"
colnames(raw_pos94[[6]])[4] <- "V2"

colnames(raw_pos94[[7]]) <- raw_pos94[[7]][1,]
colnames(raw_pos94[[7]])[2] <- "SampleID"

colnames(raw_pos94[[8]]) <- raw_pos94[[8]][8,]
#colnames(raw_pos94[[8]])[4] <- "SampleID"
colnames(raw_pos94[[8]])[2] <- "V1"
colnames(raw_pos94[[8]])[3] <- "V2"

colnames(raw_pos94[[9]]) <- raw_pos94[[9]][2,]
#colnames(raw_pos94[[9]])[6] <- "SampleID"
colnames(raw_pos94[[9]])[2] <- "V1"
colnames(raw_pos94[[9]])[3] <- "V2"

colnames(raw_pos94[[10]]) <- raw_pos94[[10]][1,]
colnames(raw_pos94[[10]])[4] <- "SampleID1"
colnames(raw_pos94[[10]])[3] <- "SampleID2"

colnames(raw_pos94[[11]]) <- raw_pos94[[11]][1,]
colnames(raw_pos94[[11]])[4] <- "SampleID1"
colnames(raw_pos94[[11]])[3] <- "SampleID2"

colnames(raw_pos94[[12]]) <- raw_pos94[[12]][1,]
colnames(raw_pos94[[12]])[4] <- "SampleID1"
colnames(raw_pos94[[12]])[3] <- "SampleID2"

colnames(raw_pos94[[13]]) <- raw_pos94[[13]][1,]
colnames(raw_pos94[[13]])[4] <- "SampleID1"
colnames(raw_pos94[[13]])[3] <- "SampleID2"

# ... and remove top rows
raw_pos94[[1]] <- raw_pos94[[1]][-c(1,2),]
raw_pos94[[2]] <- raw_pos94[[2]][-c(1,2),]
raw_pos94[[3]] <- raw_pos94[[3]][-c(1,2),]
raw_pos94[[4]] <- raw_pos94[[4]][-c(1,2),]
raw_pos94[[5]] <- raw_pos94[[5]][-c(1,2),]
raw_pos94[[6]] <- raw_pos94[[6]][-c(1:3),]
raw_pos94[[7]] <- raw_pos94[[7]][-1,]
raw_pos94[[8]] <- raw_pos94[[8]][-c(1:8),-c(69:91)]
raw_pos94[[9]] <- raw_pos94[[9]][-c(1,2),]
raw_pos94[[10]] <- raw_pos94[[10]][-1,]
raw_pos94[[11]] <- raw_pos94[[11]][-1,]
raw_pos94[[12]] <- raw_pos94[[12]][-1,]
raw_pos94[[13]] <- raw_pos94[[13]][-1,]


#~~ Loop through data frames to unite two columns with ID info and remove extra cols and rows
for (i in c(1:6, 8, 9)) {
  
  # Remove columns with empty headers
  keep.cols <- names(raw_pos94[[i]]) %in% c(NA)
  raw_pos94[[i]] <- raw_pos94[[i]][! keep.cols]
  
  raw_pos94[[i]] <- raw_pos94[[i]] %>%
    #.[colSums(!is.na(.)) > 0] %>% # remove empty columns - this occasionally removes column in the beginning or end of season
    .[rowSums(!is.na(.)) > 0, ] # remove empty rows
  
  # Unite columns with ID
  raw_pos94[[i]] <- raw_pos94[[i]] %>% unite("SampleID", V1:V2, sep = "")
  
  # Fix duplicate AGM95076
  # The one the rocks has a tag ID (G460) that fits with an individual from '97 and is also a genetic match with that ID. So the DNA must belong to that individual
  # The other one has a flipper tag that fits with another individual from '94, but that ID from 94 is not a genetic match with any other sample
  if(i == 2) {raw_pos94[[i]][which(raw_pos94[[i]]$SampleID == "AGM95076" & raw_pos94[[i]]$`35040` == "1907"), "SampleID"] <- "NOTAGM95076"}
  
  # Remove extra columns in 2001/02
  if(i == 8) {raw_pos94[[i]] <- raw_pos94[[i]] %>% select(-c(`2001 sample`, `tussock only`, `TW only`))}
  
  # Select only columns of interest
  first_col <- grep("^[[:digit:]]", colnames(raw_pos94[[i]]))[1]
  last_c <- tail(grep("^[[:digit:]]", colnames(raw_pos94[[i]])), n = 1)
  
  raw_pos94[[i]] <- raw_pos94[[i]] %>%
    select(SampleID, all_of(first_col):all_of(last_c))
  
}


#~~ Loop through data frames and remove extra cols and rows
for (i in 7:7) {
  # Remove columns with empty headers
  keep.cols <- names(raw_pos94[[i]]) %in% c(NA)
  raw_pos94[[i]] <- raw_pos94[[i]][! keep.cols]
  
  raw_pos94[[i]] <- raw_pos94[[i]] %>%
    #.[colSums(!is.na(.)) > 0] %>% # remove empty columns
    .[rowSums(!is.na(.)) > 0, ] # remove empty rows
  
  # Select only columns of interest
  first_col <- grep("^[[:digit:]]", colnames(raw_pos94[[i]]))[1]
  last_c <- tail(grep("^[[:digit:]]", colnames(raw_pos94[[i]])), n = 1)
  
  raw_pos94[[i]] <- raw_pos94[[i]] %>%
    select(SampleID, all_of(first_col):all_of(last_c))
  
}

for (i in 10:13) {
  # Remove columns with empty headers
  keep.cols <- names(raw_pos94[[i]]) %in% c(NA)
  raw_pos94[[i]] <- raw_pos94[[i]][! keep.cols]
  
  raw_pos94[[i]] <- raw_pos94[[i]] %>%
    #.[colSums(!is.na(.)) > 0] %>% # remove empty columns
    .[rowSums(!is.na(.)) > 0, ] # remove empty rows
  
  # Unite columns with ID
  raw_pos94[[i]] <- raw_pos94[[i]] %>% mutate(SampleID = ifelse(SampleID1 == "NO SAMPLE", SampleID2, SampleID1))
  
  # Select only columns of interest
  first_col <- grep("^[[:digit:]]", colnames(raw_pos94[[i]]))[1]
  last_c <- tail(grep("^[[:digit:]]", colnames(raw_pos94[[i]])), n = 1)
  
  raw_pos94[[i]] <- raw_pos94[[i]] %>%
    select(SampleID, all_of(first_col):all_of(last_c))
  
}


#~~ Pivot longer
pos_long_total94 <- NULL

for (i in 1:13) {
  pos_long94 <- raw_pos94[[i]] %>%
    filter(!is.na(SampleID)) %>%
    pivot_longer(!SampleID, names_to = "Date", values_to = "Location", values_transform = list(Location = as.character))
  
  pos_long_total94 <- rbind(pos_long_total94, pos_long94)
  
} 


#~~ Check when converting variables if and what data is lost
pos_long_total94num <- pos_long_total94 %>%
  mutate(rown = row_number()) %>%
  filter(grepl("^[0-9\\.]+$", Date))

pos_long_total94rest <- pos_long_total94 %>%
  mutate(rown = row_number()) %>%
  filter(!grepl("^[0-9\\.]+$", Date))

pos_long_total94num[which(is.na(
  as_date(as_date(as.numeric(pos_long_total94num$Date), origin = "1899-12-30")))!= is.na(pos_long_total94num$Date)), "Date"]

pos_long_total94rest[which(is.na(
  as_date(pos_long_total94rest$Date, format = "%d-%b-%y"))!= is.na(pos_long_total94rest$Date)), "Date"]
# No warnings left after adjusting reading in of data 2001/2002


#~~ Convert dates

# Check above did not give warnings, however code below still does
# pos_long_total94 <- pos_long_total94 %>% 
#   mutate(DateClean = as_date(ifelse(grepl("^[0-9\\.]+$", Date), 
#                                   as.numeric(Date) %>% as_date(origin = "1899-12-30") %>% as_date(), 
#                                   as_date(Date, format = "%d-%b-%y"))))
# could be to do with ifelse trying to evaluate both branches,
# however case_when also gave warnings. I therefore decided to split the df and do the conversion separatly

pos_long_total94a <-rbind(pos_long_total94num %>% 
                            mutate(DateClean = as_date(as_date(as.numeric(Date), origin = "1899-12-30"))),
                          pos_long_total94rest %>% 
                            mutate(DateClean = as_date(Date, format = "%d-%b-%y"))) %>%
  arrange(rown) %>%
  select(-rown)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Raw male location data 2007-2020  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

files <- list.files(path = here("Data", "raw", "MaleLocations"), full.names = T)

files <- files[grepl(paste0("^", here("Data", "raw", "MaleLocations"), "/M"), files)]

# 2006/2007 already included from the other file
files <- files[!grepl(paste0("^", here("Data", "raw", "MaleLocations"), "/Males06"), files)]

# 2010/2011 twice in there. Take most recent one
files <- files[!grepl(paste0("^", here("Data", "raw", "MaleLocations"), "/Males10"), files)]

pos_long_total07 <- NULL
counter = 0

for (i in files){
  
  raw_pos <- read_excel(i, col_names = F, col_type = "text")
  
  colnames(raw_pos) <- raw_pos %>% filter(if_any(everything(), ~ grepl("Serial Number", .)))
  
  raw_pos <- raw_pos[!grepl("Total", raw_pos[[1]]),]
  
  # Remove columns with empty headers
  keep.cols <- names(raw_pos) %in% c(NA)
  raw_pos <- raw_pos[! keep.cols]
  
  raw_pos <- raw_pos %>%
    #select(-`NA`) %>%
    filter(row_number() > which(`Serial Number`=="Serial Number")) %>%
    #filter(!grepl("Total", 1)) %>%
    #`colnames<-`(.[1, ]) %>%
    #.[-1, ] %>% # removes first row (containing headers)
    #.[colSums(!is.na(.)) > 0] %>% # remove empty columns
    .[rowSums(!is.na(.)) > 0, ] %>% # remove empty rows
    rename(SampleID = `Serial Number`)
  
  # Select only columns of interest, convert to date and pivot longer
  first_col <- grep("^[[:digit:]]", colnames(raw_pos))[1]
  last_c <- tail(grep("^[[:digit:]]", colnames(raw_pos)), n = 1)
  
  counter = counter + 1
  
  # Fix mistake in 2017: AGM17015 twice (once as AGM17009 + AGM17015), the second one must be 16
  # AGM17009 and AGM17015 are indeed a genetic match and also a genetic match with AGM18011 which has the same PIT tag ID
  # AGM17016 is missing in the raw location data, but we have a genetic sample for this individual (which is different from AGM17015)
  # Therefore I assume that the second AGM17015 must be AGM17016
  if(grepl("Males_17_18.xlsx", i)) {
    raw_pos[which(raw_pos$SampleID == "AGM17015"), "SampleID"] <- "AGM17016"
  }
  
  # Fix mistake in 2020: AGM20022 twice, the first one must be 21 based on PIT tag info
  if(grepl("Males_20_21.xlsx", i)) {
    raw_pos[which(raw_pos$SampleID == "AGM20022" & is.na(raw_pos$`PIT tag`)), "SampleID"] <- "AGM20021"
  }
  
  pos_long <- raw_pos %>%
    mutate(SampleID = ifelse(is.na(SampleID) | SampleID == "-", paste0("dummyID", counter, "_", row_number()), SampleID)) %>%
    select(SampleID, all_of(first_col):all_of(last_c)) %>%
    pivot_longer(!SampleID, names_to = "Date", values_to = "Location", values_transform = list(Location = as.character)) 
  
  pos_long_total07 <- rbind(pos_long_total07, pos_long)
  
}


#~~ Check when converting variables if and what data is lost
pos_long_total07num <- pos_long_total07 %>%
  mutate(rown = row_number()) %>%
  filter(grepl("^[0-9\\.]+$", Date))

pos_long_total07rest <- pos_long_total07 %>%
  mutate(rown = row_number()) %>%
  filter(!grepl("^[0-9\\.]+$", Date))

pos_long_total07num[which(is.na(
  as_date(as_date(as.numeric(pos_long_total07num$Date), origin = "1899-12-30")))!= is.na(pos_long_total07num$Date)), "Date"]

pos_long_total07rest[which(is.na(
  as_date(pos_long_total07rest$Date, format = "%d-%b-%Y"))!= is.na(pos_long_total07rest$Date)), "Date"]
# No warnings left after adjusting reading in of data 2001/2002


#~~ Convert dates
pos_long_total07a <-rbind(pos_long_total07num %>% 
                            mutate(DateClean = as_date(as_date(as.numeric(Date), origin = "1899-12-30"))),
                          pos_long_total07rest %>% 
                            mutate(DateClean = as_date(Date, format = "%d-%b-%Y"))) %>%
  arrange(rown) %>%
  select(-rown) 


#~~ Bind all data together
pos_long_total <- rbind(pos_long_total94a, pos_long_total07a)


#~~ Save raw data to avoid rerunning loop above
saveRDS(pos_long_total, file = here("data", "Processed", "raw_pos_data_obs.Rds"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Post import clean up  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Load data
pos_long_total <- readRDS(file = here("data", "Processed", "raw_pos_data_obs.Rds"))

# Any dummyIDs without locations at all?
nrow(pos_long_total %>% filter(grepl("^dummy", SampleID)) %>% group_by(SampleID) %>% filter(all(is.na(Location))))
# none

# Any sampleIDs without locations at all?
to_remove <- pos_long_total %>% 
  filter(grepl("AGM", SampleID)) %>% 
  group_by(SampleID) %>% 
  filter(all(is.na(Location))) %>% 
  distinct(SampleID)
# Remove all of them, except the '98 ones. For only 35 males locations were recorded, however, 
# the field ledgers seem to indicate that although they weren't tissue sampled or location recorded, 
# more individuals were there (quite a few have a AGM of the year before or tag noted, for example)
# NB for the other years only males with at least one recorded location were included as those would be
# considered territorial males. This includes locations outside the main beach eg rocks, tussock etc.

# Check IDs that do not match expected number of characters
# Possibly these IDs do not have location data because it has been merged with the other ID?
to_remove %>% 
  mutate(id_nchar = str_count(SampleID)) %>%
  filter(id_nchar != 8) %>%
  select(SampleID) 
# all except 1 are anyway from 1998 and thus maintained and for the last one we have no genetic info for either of the IDs

to_remove <- to_remove %>% 
  filter(!grepl("^AGM98", SampleID))

pos_long_total <- pos_long_total[!pos_long_total$SampleID %in% to_remove$SampleID,]

# Cleanup and fixing mistakes in dates
pos_long_total2 <- pos_long_total %>%
  # there is no info about individuals with missing sample ID (ie "dummy")
  # keep to include in observation counts (there was a male present)
  #filter(!grepl("^dummyID", SampleID)) %>%
  filter(!grepl("S", SampleID)) %>%
  filter(!grepl("NANA", SampleID)) %>%
  mutate(SampleID = sub("AMG", "AGM", SampleID)) %>%
  mutate(Year = as.numeric(format(as.Date(DateClean, format="%d/%m/%y"),"%Y"))) %>%
  mutate(Mo = as.numeric(format(as.Date(DateClean, format="%d/%m/%y"),"%m"))) %>%
  mutate(Season = ifelse(Mo == 1 | Mo == 2, Year - 1, Year)) %>% # Should be the same as SamplingYear from tissue ID (below)
  mutate(Season = ifelse(is.na(Season), 2001, Season)) %>% # all missing seasons are 2001
  separate(SampleID , c("AG", "SamplingYear"), sep = "(?<=[A-Za-z])(?=[0-9])", remove = F) %>%
  mutate(pre = ifelse(grepl("^9", .$SamplingYear), 19, 20)) %>%
  mutate(SamplingYear = paste(pre, SamplingYear, sep = "")) %>%
  mutate(SamplingYear = substr(SamplingYear, 1, 4)) %>%
  mutate(SamplingYear = as.numeric(SamplingYear)) %>%
  mutate(SamplingYear = ifelse(grepl("^dummyID", SampleID), Season, SamplingYear)) %>% 
  # Check
  mutate(CheckYear = ifelse(SamplingYear != Season, "WRONG", NA)) %>%
  # pos_long_total2 %>% filter(CheckYear=="WRONG") %>% group_by(Season, SamplingYear) %>% tally()
  # Fix mistakes
  mutate(SeasonNew = ifelse(Season == 1995 & SamplingYear == 1996, 1996, Season)) %>% # some dates for 1996 were wrong in the raw data file
  mutate(SeasonNew = ifelse(Season == 1995 & SamplingYear == 1997, 1997, SeasonNew)) %>% # some dates for 1997 were wrong in the raw data file
  mutate(SeasonNew = ifelse(Season == 1996 & SamplingYear == 1997, 1997, SeasonNew)) %>% # some dates for 1997 were wrong in the raw data file
  mutate(SeasonNew = ifelse(Season == 2001 & SamplingYear == 2002, 2002, SeasonNew)) %>% # some dates for 1997 were wrong in the raw data file
  mutate(SeasonNew = ifelse(Season == 2006 & SamplingYear == 2005, 2005, SeasonNew)) %>% # some dates for 1997 were wrong in the raw data file
  mutate(SeasonNew = ifelse(Season == 2005 & SamplingYear == 2006, 2006, SeasonNew)) %>% # some dates for 2006 were wrong in the raw data file
  mutate(SeasonNew = ifelse(Season == 2008 & SamplingYear == 2009, 2009, SeasonNew)) %>% # dates for 2009 were all wrong in the raw data file
  mutate(SeasonNew = ifelse(Season == 2012 & SamplingYear == 2013, 2013, SeasonNew)) %>% # dates for 2013 were all wrong in the raw data file
  mutate(SeasonNew = ifelse(Season == 2018 & SamplingYear == 2019, 2019, SeasonNew)) %>% # dates for 2019 were all wrong in the raw data file
  #mutate(FinalDate = as.Date(ifelse(grepl("^dummyID", SampleID), DateClean, NA), origin = "1970-01-01")) %>%
  mutate(FinalDate = as.Date(ifelse(Season == 1995 & SamplingYear == 1996, DateClean %m+% years(1), DateClean), origin = "1970-01-01")) %>%
  mutate(FinalDate = as.Date(ifelse(Season == 1995 & SamplingYear == 1997, DateClean %m+% years(2), FinalDate), origin = "1970-01-01")) %>%
  mutate(FinalDate = as.Date(ifelse(Season == 1996 & SamplingYear == 1997, DateClean %m+% years(1), FinalDate), origin = "1970-01-01")) %>%
  mutate(FinalDate = as.Date(ifelse(Season == 2001 & SamplingYear == 2002, DateClean %m+% years(1), FinalDate), origin = "1970-01-01")) %>%
  mutate(FinalDate = as.Date(ifelse(Season == 2006 & SamplingYear == 2005, DateClean %m-% years(1), FinalDate), origin = "1970-01-01")) %>%
  mutate(FinalDate = as.Date(ifelse(Season == 2005 & SamplingYear == 2006, DateClean %m+% years(1), FinalDate), origin = "1970-01-01")) %>%
  mutate(FinalDate = as.Date(ifelse(Season == 2008 & SamplingYear == 2009, DateClean %m+% years(1), FinalDate), origin = "1970-01-01")) %>%
  mutate(FinalDate = as.Date(ifelse(Season == 2012 & SamplingYear == 2013, DateClean %m+% years(1), FinalDate), origin = "1970-01-01")) %>%
  mutate(FinalDate = as.Date(ifelse(Season == 2018 & SamplingYear == 2019, DateClean %m+% years(1), FinalDate), origin = "1970-01-01")) %>%
  # # Check
  mutate(CheckYear2 = ifelse(SamplingYear != SeasonNew, "WRONG", NA)) %>%
  # Checks
  mutate(NewYear = as.numeric(format(as.Date(FinalDate, format="%d/%m/%y"),"%Y"))) %>%
  mutate(CheckYear3 = ifelse((Mo == 10 | Mo == 11 | Mo == 12) & NewYear != SeasonNew, "WRONG", 
                             ifelse((Mo == 1 | Mo == 2) & NewYear != SeasonNew + 1, "WRONG", NA)))

# Column cleanup
pos_long_total3 <- pos_long_total2 %>%
  select(SampleID, Date=FinalDate, Location) %>%
  mutate(Location = sub("x", "", Location)) %>% # season 20/21 had locations with x at the end, remove x 
  mutate(Location = sub("11111", "1111", Location)) %>% # fix obvious typo
  mutate(Location = sub("21116", "2116", Location)) %>% # fix obvious typo
  mutate(Location = sub("21123", "2123", Location)) %>% # fix obvious typo
  mutate(Location = ifelse(!is.na(SampleID) & SampleID == "AGM03034" & !is.na(Location) & Location == "9001", "0901", Location)) %>%
  mutate(Year = as.numeric(format(as.Date(Date, format="%d/%m/%y"),"%Y"))) %>%
  mutate(Mo = as.numeric(format(as.Date(Date, format="%d/%m/%y"),"%m"))) %>%
  mutate(Season = ifelse(Mo == 1 | Mo == 2, Year - 1, Year)) %>%
  select(-Mo, -Year)

# SampleID cleanup
pos_long_total4 <- pos_long_total3 %>%
  mutate(SampleID = toupper(SampleID)) %>%
  mutate(SampleID = gsub("AND ", " AND ", SampleID)) %>% 
  mutate(SampleID = gsub(" \\= ", " AND ", SampleID)) %>%
  mutate(SampleID = gsub("\\+", " AND ", SampleID)) %>% 
  mutate(SampleID = gsub(" \\+ ", " AND ", SampleID)) %>% 
  mutate(SampleID = gsub(" \\(", " AND ", SampleID)) %>% 
  mutate(SampleID = gsub("\\)", "", SampleID)) %>% 
  mutate(SampleID = gsub("  ", " ", SampleID)) %>% 
  separate(SampleID, c("SampleID", "SampleID2"), sep = " AND ", remove = F) %>%
  mutate(SampleID = ifelse(SampleID == "AGM03046 PATX1", "AGM03046", SampleID)) %>%
  mutate(SampleID2 = ifelse(SampleID2 == "AGM03056 PATX1", "AGM03056", SampleID2)) %>%
  mutate(SampleID2 = ifelse(SampleID2 == "034", "AGM98034", SampleID2)) %>%
  mutate(SampleID = ifelse(SampleID == "AGM01147", "AGM01147/8", SampleID)) #%>% # The tissue is called also 7/8
#mutate(SampleID2 = ifelse(SampleID == "AGM01147", NA, SampleID2)) 

#pos_long_total3[grepl("\\=", pos_long_total3$SampleID),]


#~~ Save cleaned data to avoid rerunning loop above and for use in other project
saveRDS(pos_long_total4, file = here("data", "Processed", "clean_pos_data_obs.Rds"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Check duplicate IDs within seasons  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Load data
pos_long_total4 <- readRDS(file = here("data", "Processed", "clean_pos_data_obs.Rds"))

# Some individuals were sampled twice (or more) within one season
# Here I remove those duplicates, while keeping all observed (non-sampled) IDs. 
# Because without genetic checking, it is unknown whether these are different individuals
# or not. So I assume that non-sampled individuals with different IDs are different individuals

# Load all_genotypes dataframe, selecting unique IDs
unique_ID <- read_excel(here("Data", "raw", "all_msat_genotypes_uniqueID.xlsx"),
                        guess_max = 15000) %>%
  distinct(uniqueID, dummyID, Matches)

# Merge uniqueID into location data
pos_long_total5 <- left_join(pos_long_total4, unique_ID, by = c("SampleID" = "dummyID")) %>%
  rename(uniqueID1 = uniqueID) %>%
  left_join(unique_ID %>% select(-Matches), by = c("SampleID2" = "dummyID")) %>%
  rename(uniqueID2 = uniqueID, uniqueID = uniqueID1)

# Check mismatching uniqueIDs
# View(pos_long_total5 %>% filter(!is.na(uniqueID) & !is.na(uniqueID2) & uniqueID != uniqueID2))

# Mistake found: AGM06079 is not the same individual as AGM06085, separate data based on raw location data file
pos_long_total5 <- pos_long_total5 %>%
  mutate(SampleID = ifelse((SampleID == "AGM06079" & Date!="2006-12-02") & (SampleID == "AGM06079" & Date!="2006-12-03"), "AGM06085", SampleID)) %>%
  mutate(uniqueID = ifelse(SampleID == "AGM06085", uniqueID2, uniqueID)) %>%
  mutate(SampleID2 = ifelse(SampleID == "AGM06079", NA, SampleID2)) %>%
  mutate(uniqueID2 = ifelse(SampleID == "AGM06079", NA, uniqueID2)) %>%
  mutate(SampleID2 = ifelse(SampleID == "AGM06085", NA, SampleID2)) %>%
  mutate(uniqueID2 = ifelse(SampleID == "AGM06085", NA, uniqueID2))

# Annual male counts
n_males <- pos_long_total5 %>%
  distinct(SampleID, uniqueID, Season)

#View(n_males %>% filter(!is.na(uniqueID)) %>% filter(uniqueID %in% unique(.[["uniqueID"]][duplicated(.[["uniqueID"]])])) %>% arrange(uniqueID))
#View(n_males %>% filter(!is.na(SampleID)) %>% filter(SampleID %in% unique(.[["SampleID"]][duplicated(.[["SampleID"]])])) %>% arrange(SampleID))

# Any males in data with unknown season?
n_males %>% filter(is.na(Season)) # none

# Check: does the same sample ID occur twice within a season?
n_males %>% group_by(SampleID, Season) %>% tally() %>% filter(n!=1)  
# no

# Check: any unique IDs occur twice within a season?
n_males %>%
  mutate(uniqueID = ifelse(is.na(uniqueID), SampleID, uniqueID)) %>%
  group_by(uniqueID, Season) %>% tally() %>% filter(n!=1) 
# yes

# Keep only unique males
n_males <- n_males %>%
  mutate(uniqueID = ifelse(is.na(uniqueID), SampleID, uniqueID)) %>%
  distinct(uniqueID, Season)

# n unique observed and genotyped males 
# For observed only males (ie not tissue sampled), different IDs are considered to be different individuals
nrow(n_males %>% distinct(uniqueID)) # 1955

n_obs_males <- n_males %>%
  group_by(Season) %>% 
  tally()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Add SAM index to male annual counts  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~ Add SAM index to the df with male counts per year
sam_index <- read.table(here("data", "raw", "newsam.1957.2007.seas.txt"), header = T) %>%
  rownames_to_column("year") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(annual_mean = as.numeric(ANN)) %>%
  select(year, annual_mean)

# Simple LM
summary(lm(sam_index$annual_mean ~ sam_index$year))

# Year and SAM are correlated. Create detrended SAM variable (= residuals from the lm on SAM)
detrend <- function(x) resid(lm(x ~ seq(x)))

sam_index$detrended_annual_mean <- detrend(sam_index$annual_mean) # where you pass annual SAM values from the entire time series

n_obs_males <- n_obs_males %>%
  left_join(sam_index, by = c("Season" = "year")) %>%
  rename(SamplingYear = Season, SAMSamplingYear = annual_mean, dtSAMSamplingYear = detrended_annual_mean)

# Breeding seasons are referred to by the year in which they ended
n_obs_males2 <- n_obs_males %>%
  mutate(SamplingYear = SamplingYear + 1) 


#~~ Save df
openxlsx::write.xlsx(n_obs_males2, here("Data", "Processed", "n_observed_males.xlsx"))         

