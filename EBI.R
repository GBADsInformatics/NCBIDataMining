##############################################################################
#script to analyze EBI data from NCBI 
#written by Cassandre Pyne for GBADs, 2022-2023
##############################################################################

#load necessary packages 
library(stringr)
library(ggplot2)
library(visdat)
library(dplyr)
library(nanier)

#load data in 
setwd("/Users/cassandrepyne/Documents/GBADs")
data <- read.csv("MASTER_DATA_coordinates_FINAL_AUG_5.csv", sep = ",", header = T)
spp_list <- read.csv("gbads_spp.csv", sep = ",", header = T)
spp_list <- tolower(spp_list$species)

#choose the correct species (i.e. economically important species)
data$spp_combined <- tolower(paste(data$species, data$host.species))
data$spp_combined <- gsub("salmonella", "", data$spp_combined)
data$spp_combined <- str_extract(data$spp_combined, paste(spp_list, collapse="|"))

#choose entries that have been submitted by EDI
EBI <- data[which(data$organization == "EBI" | data$spp_combined == "European Bioinformatics Institute"),]

#all other entries (those that are not from EBI)
other <- data[-which(data$organization == "EBI" | data$spp_combined == "European Bioinformatics Institute"),]

#data = all entries
#EBI = just EBI entries
#other = everything that isn't EBI 

#replace all empty entries with NA (so vis_miss knows they are missing)
data$location[data$location==""]<-NA
data$breed[data$breed==""]<-NA
data$sex[data$sex==""]<-NA
data$age[data$age==""]<-NA
data$weight[data$weight==""]<-NA
data$disease[data$disease==""]<-NA
data$latitude[data$latitude==""] <-NA 
data$longitude[data$longitude==""]<-NA

#select the specific columns of interest 
x <- select(data, c(9, 5, 10, 11, 12, 13, 16,14, 10))

#missingness visualization, pre-cleaning 
s <- vis_miss(x, warn_large_data = FALSE)

#clean location data (remove unnecessary phrases)
x$location <- gsub("0", "" , x$location)
x$location <- gsub('.*[0-9]+.*', '', x$location)
x$location <- gsub("N/A", "" , x$location)
x$location <- gsub("-", "" , x$location)
x$location <- gsub("NULL", "" , x$location)
x$location <- gsub("na", "" , x$location)
x$location <- gsub("not available", "" , x$location)
x$location <- gsub("     </Comment", "" , x$location)
x$location <- gsub("highland", "" , x$location)
x$location <- gsub("lowland", "" , x$location)
x$location <- gsub("caudal_pectoralis major", "" , x$location)
x$location <- gsub("cranial_pectoralis major", "" , x$location)
x$location <- gsub("Rumen", "" , x$location)
x$location <- gsub(" geographic information", "" , x$location)
x$location <- gsub("not collect", "" , x$location)
x$location <- gsub("Unknown", "" , x$location)
x$location <- gsub("none", "" , x$location)
x$location <- gsub("Right.*", "" , x$location)
x$location <- gsub("Left.*", "" , x$location)
x$location <- gsub("Proximal trachea", "" , x$location)
x$location <- gsub("Bottom of the mouth", "" , x$location)
x$location <- gsub("Oropharynx", "" , x$location)
x$location <- gsub("Distal trachea", "" , x$location)
x$location <- gsub("Not applicable", "" , x$location)
x$location <- gsub("apical", "" , x$location)
x$location <- gsub("lateral", "" , x$location)
x$location <- gsub("In Vitro", "" , x$location)
x$location <- gsub("Remaining Stream", "" , x$location)
x$location <- gsub("Invasive Front", "" , x$location)
x$location <- gsub("Trailer", "" , x$location)
x$location <- gsub("Leader", "" , x$location)
x$location[x$location==""]<-NA

#clean breed data (remove unnecessary phrases)
x$breed <- gsub("isolate.*", "" , x$breed)
x$breed <- gsub("strain.*", "" , x$breed)
x$breed <- gsub("label.*", "" , x$breed)
x$breed <- gsub('[0-9]+', '', x$breed)
x$breed <- gsub("breed: ", "", x$breed)
x$breed <- gsub("NAG", "", x$breed)
x$breed <- gsub("F", "", x$breed)
x$breed <- gsub(":i:-", "", x$breed)
x$breed <- gsub("COMPOSITE", "", x$breed)
x$breed <- gsub("Commercial", "", x$breed)
x$breed <- gsub("Y x L", "", x$breed)
x$breed <- gsub('.*[0-9]+.*', '', x$breed)
x$breed <- gsub("-", "", x$breed)
x$breed <- gsub(":g", "", x$breed)
x$breed <- gsub(":i", "", x$breed)
x$breed[x$breed==""]<-NA

#clean sex data (remove unnecessary phrases)
x$sex <- gsub("Missing", "", x$sex)
x$sex <- gsub("-", "", x$sex)
x$sex <- gsub("male and female", "", x$sex)
x$sex <- gsub("to be completed by Elena", "", x$sex)
x$sex <- gsub("intersex", "", x$sex)
x$sex <- gsub("Not.*", "", x$sex)
x$sex <- gsub('.*[0-9]+.*', '', x$sex)
x$sex <- gsub("mixed", "", x$sex)
x$sex <- gsub("neuter", "", x$sex)
x$sex <- gsub("hermaphrodite", "", x$sex)
x$sex <- gsub("bull", "", x$sex)
x$sex <- gsub("Unknown", "", x$sex)
x$sex <- gsub("unknown", "", x$sex)
x$sex <- gsub("Steer", "", x$sex)
x$sex <- gsub("MISSING", "", x$sex)
x$sex <- gsub(" pool of equal ", "", x$sex)
x$sex <- gsub("Male and female", "", x$sex)
x$sex <- gsub("Male and Female", "", x$sex)
x$sex <- gsub("male/female", "", x$sex)
x$sex <- gsub("female/male", "", x$sex)
x$sex <- gsub("_sex", "", x$sex)
x$sex <- gsub("N/A", "", x$sex)
x$sex <- gsub("n/a", "", x$sex)
x$sex <- gsub("unkonwn", "", x$sex)
x$sex <- gsub("na", "", x$sex)
x$sex <- gsub("both", "", x$sex)
x$sex <- gsub("unk", "", x$sex)
x$sex <- gsub("two.*", "", x$sex)
x$sex <- gsub("Two.*", "", x$sex)
x$sex <- gsub("six.*", "", x$sex)
x$sex <- gsub("four.*", "", x$sex)
x$sex <- gsub("Four.*", "", x$sex)
x$sex <- gsub("Australia", "", x$sex)
x$sex <- gsub("#NAME?", "", x$sex)
x$sex <- gsub("female and male", "", x$sex)
x$sex <- gsub("Female and male combined", "", x$sex)
x$sex <- gsub("USA:SC", "", x$sex)
x$sex <- gsub("?", "", x$sex)
x$sex[x$sex==""]<-NA

#clean age data (remove unnecessary phrases)
x$age <- gsub("Missing", "", x$age)
x$age <- gsub("Not application", "", x$age)
x$age <- gsub("Passage.*", "", x$age)
x$age <- gsub("Samples after.*", "", x$age)
x$age <- gsub("&gt;1", "", x$age)
x$age <- gsub("Eimeria tenella", "", x$age)
x$age <- gsub("None", "", x$age)
x$age[x$age==""]<-NA

#clean disease data (remove unnecessary phrases)
x$disease <- gsub("Week.*", "", x$disease)
x$disease <- gsub("No disease", "", x$disease)
x$disease <- gsub("N/A", "", x$disease)
x$disease <- gsub("None", "", x$disease)
x$disease <- gsub("null", "", x$disease)
x$disease <- gsub("Missing", "", x$disease)
x$disease <- gsub("No", "", x$disease)
x$disease <- gsub("['' '']", "", x$disease)
x$disease <- gsub("na", "", x$disease)
x$disease <- gsub("Not applicable", "", x$disease)
x$disease <- gsub("72 h", "", x$disease)
x$disease[x$disease==""]<-NA

#clean weight data (remove unnecessary phrases)
x$weight <- gsub("NORM", "", x$weight)
x$weight <- gsub("IUGR", "", x$weight)
x$weight[x$weight==""]<-NA

#clean coordinate data (remove unnecessary phrases)
x$latitude <- gsub("#NAME?", "", x$latitude)
x$latitude <- gsub("d", "", x$latitude)
x$latitude <- gsub("2021-04-08", "", x$latitude)
x$latitude <- gsub("USA:TN", "", x$latitude)
x$latitude <- gsub(" ", "", x$latitude)
x$latitude <- gsub("2009", "", x$latitude)
x$latitude <- grep("-", x$latitude, invert=TRUE, value = TRUE) 
x$latitude[x$latitude ==""]<-NA
x$longitude <- gsub("A", "", x$longitude)
x$longitude <- gsub("/A", "", x$longitude)
x$longitude <- gsub("otapplicable", "", x$longitude)
x$longitude <- gsub("China", "", x$longitude)
x$longitude <- gsub("OTCOLLECTED", "", x$longitude)
x$longitude <- gsub("Netherlands", "", x$longitude)
x$longitude <- gsub("Germany", "", x$longitude)
x$longitude <- gsub("Canada: British Columbia", "", x$longitude)
x$longitude <- gsub("USA", "", x$longitude)
x$longitude <- gsub("Germany: Thuringia", "", x$longitude)
x$longitude <- gsub("Canada", "", x$longitude)
x$longitude <- gsub(":British Columbia Fraser Valley", "", x$longitude)
x$longitude <- gsub("Denmark", "", x$longitude)
x$longitude <- gsub("USA:TX", "", x$longitude)
x$longitude <- gsub("USA:SD", "", x$longitude)
x$longitude <- gsub("USA:OK", "", x$longitude)
x$longitude <- gsub("2022-02-21", "", x$longitude)
x$longitude <- gsub("USA:WY", "", x$longitude)
x$longitude <- gsub("USA:TN", "", x$longitude)
x$longitude <- gsub("otCollected", "", x$longitude)
x$longitude <- gsub("otAvailable", "", x$longitude)
x$t <- NA 
no_years <- x$latitude[!grepl("-", x$latitude)]
x$latitude <- no_years
x$t <- no_years

#missingness visualization, posts-cleaning 
t <- vis_miss(x, warn_large_data = FALSE)

#panelled plot (pre and post cleaning)
opts(plot.margin = unit(c(2, 2), "cm"))
ggarrange(s, t, ncol=1, nrow=2)
dev.off()