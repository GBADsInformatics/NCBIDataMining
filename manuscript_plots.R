##############################################################
#Script for making manuscript plot
#Script written by Cassandre Pyne for GBADs
##############################################################

#load necessary packages
library(stringr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(highcharter) 
library(tidyverse)
library(ggpubr)
library(heatmaply)

#load all necessary data
setwd("/Users/cassandrepyne/Documents/GBADs")
data <- read.csv("MASTER_DATA_coordinates_FINAL_AUG_5.csv", sep = ",", header = T)
spp_list <- read.csv("gbads_spp.csv", sep = ",", header = T)
spp_list <- tolower(spp_list$species)
#choose the correct species 
data$spp_combined <- tolower(paste(data$species, data$host.species))
data$spp_combined <- gsub("salmonella", "", data$spp_combined)
data$spp_combined <- str_extract(data$spp_combined, paste(spp_list, collapse="|"))
WOAH_agents <- read.csv("WOAH_disease_agents.csv", sep = ",", header=T)
disease <- read.csv("WOAH_disease.csv", sep = ",", header=T)


########################################
#Entries over time 
########################################
x <- count(data, "date")
x$date <- as.Date(x$date)
# plot
x %>% 
  ggplot( aes(x=date, y=freq)) +
  geom_line() + scale_x_date(limits = as.Date(c("2005-01-01","2022-08-1"))) + labs(x = "Date", y = "Number of entries")

########################################
#Entries per species 
########################################
#create data frames for each species
ovis_aries <- data[which(data$spp_combined == "ovis aries" | data$spp_combined == "sheep"),]
bos_taurus <- data[which(data$spp_combined == "bos taurus" | data$spp_combined == "bovine" | data$spp_combined == "cow" | data$spp_combined == "cattle" | data$spp_combined == "livestock"),]
gallus <- data[which(data$spp_combined == "gallus gallus" | data$spp_combined == "chicken" | data$spp_combined == "hen" | data$spp_combined == "poultry"),]
sus_scrofa <- data[which(data$spp_combined == "sus scrofa" | data$spp_combined == "pig"),]
capra_hircus <- data[which(data$spp_combined == "capra hircus" | data$spp_combined == "goat"),]
llama <- data[which(data$spp_combined == "lama glama" | data$spp_combined == "llama"),]
camel <- data[which(data$spp_combined == "camel"),]
equid <- data[which(data$spp_combined == "equid" | data$spp_combined == "equine" | data$spp_combined == "equus ferus caballus" | data$spp_combined == "equus ferus"| data$spp_combined == "equus caballus" | data$spp_combined == "equus kiang" | data$spp_combined == "equus zebra" | data$spp_combined == "equus asinus x equus caballus" | data$spp_combined == "equus przewalskii" | data$spp_combined == "equus hemionus kulan" | data$spp_combined == "equus asinus" | data$spp_combined == "donkey"),]

#calculate number of entries per species
cattle_num <- nrow(bos_taurus)
sheep_num <- nrow(ovis_aries)
cow_num <- nrow(bos_taurus)
chicken_num <- nrow(gallus)
pig_num <- nrow(sus_scrofa)
goat_num <- nrow(capra_hircus)
equid_num <- nrow(equid)
llama_num <- nrow(llama)
camel_num <- nrow(camel)

#create dataframe with species and number of entries for all species 
df <- data.frame(species=c("cattle", "chicken", "pig", "sheep", "goat", "equid", "camel", "llama"),
                 count=c(cattle_num, chicken_num, pig_num, sheep_num, goat_num, equid_num, camel_num, llama_num))

#plot number of entries per species 
p<-ggplot(data=df, aes(x= reorder(species, -count), y=count)) +
  geom_bar(stat="identity", fill="#E84C3D")+
  labs(x = "Species", y = "Number of entries")+ ylim(0,100000)

p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 15))

########################################
#Entries per country 
########################################
#combine all species data
sdf <- rbind(ovis_aries, bos_taurus, gallus, sus_scrofa, capra_hircus, llama, camel, equid)

#load country data in 
country_list <- read.csv("countries.csv", sep = ",", header = F)
country_list <- tolower(country_list$V1)

#convert NCBI country data to the same phrasing (based on countries.csv)
sdf$country = sdf$location
sdf$country <- tolower(sdf$country)
sdf$country <- str_extract(sdf$country, paste(country_list, collapse="|"))

#calculate the number of species per country 
d <- sdf %>% group_by(spp_combined, country) %>% summarise(COUNT = n())
d <- data.frame(d)
d <- na.omit(d)

#clean species name
d$spp_combined <- gsub("bovine", "cattle", d$spp_combined)
d$spp_combined <- gsub("bos taurus", "cattle", d$spp_combined)
d$spp_combined <- gsub("cow", "cattle", d$spp_combined)
d$spp_combined <- gsub("livestock", "cattle", d$spp_combined)
d$spp_combined <- gsub("gallus gallus", "chicken", d$spp_combined)
d$spp_combined <- gsub("poultry", "chicken", d$spp_combined)
d$spp_combined <- gsub("lama glama", "llama", d$spp_combined)
d$spp_combined <- gsub("hen", "chicken", d$spp_combined)
d$spp_combined <- gsub("ovis aries", "sheep", d$spp_combined)
d$spp_combined <- gsub("sus scrofa", "pig", d$spp_combined)
d$spp_combined <- gsub("capra hircus", "goat", d$spp_combined)
d$spp_combined <- gsub("equid", "equine", d$spp_combined)
d$spp_combined <- gsub("equus asinus", "equine", d$spp_combined)
d$spp_combined <- gsub("equus caballus", "equine", d$spp_combined)
d$spp_combined <- gsub("equine caballus", "equine", d$spp_combined)
d$spp_combined <- gsub("equus ferus", "equine", d$spp_combined)
d$spp_combined <- gsub("equus ferus caballus", "equine", d$spp_combined)
d$spp_combined <- gsub("equus hemionus kulan", "equine", d$spp_combined)
d$spp_combined <- gsub("equus kiang", "equine", d$spp_combined)
d$spp_combined <- gsub("equus przewalskii", "equine", d$spp_combined)
d$spp_combined <- gsub("equus zebra", "equine", d$spp_combined)
d$spp_combined <- gsub("donkey", "equine", d$spp_combined)

#clean country name
d$country <- gsub("\\busa\\b", "united states", d$country)
d$country <- gsub("\\buk\\b", "united kingdom", d$country)
d$country <- gsub("england", "united kingdom", d$country)

#order dataframe by decreasing number of entries per country 
d <- d[order(d$country, -d$COUNT),]
t <- aggregate(d$COUNT, by=list(Category=d$country), FUN=sum)

#remove countries with less than 500 entries 
r <- t[which(t$x < 500),]
remove <- unique(r$Category)
rdf <- d[!d$country %in% remove, ]

#new dataframe with only countries with more than 500 entries 
colnames(rdf) = c("species", "country", "COUNT")
rdf$country <- str_to_title(rdf$country)

#plot: number of entries per country 
p <- ggplot(rdf, aes(x = reorder(country,-COUNT), y = COUNT, fill = species)) +  labs(x = "Country", y = "Number of Entries") + 
  geom_bar(stat = "identity") 
p + theme(axis.text.x = element_text(angle = 90, vjust=0.5,hjust=1)) 

#stats
#create dataframe for just cattle entries 
t <- rdf[which(rdf$species == "cattle"),]
#create dataframe for just cattle entries in the US & Canada 
other <- t[which(t$country == "United States" | t$country == "Canada"),]
#create dataframe for just cattle entries in European countries 
europe <- t[which(t$country == "United Kingdom" | t$country == "Germany" | t$country == "France" | t$country == "Norway" | t$country == "Spain" | t$country == "Sweden" | t$country == "Netherlands" | t$country == "Denmark" | t$country == "Austria" | t$country == "Czeck Republic" | t$country == "Italy" | t$country== "Ireland" | t$country=="Switzerland"),]

########################################
#number of breeds / species 
########################################

setwd("/Users/cassandrepyne/Documents/GBADs")
data <- read.csv("MASTER_DATA_coordinates_FINAL_AUG_5.csv", sep = ",", header = T)
spp_list <- read.csv("gbads_spp.csv", sep = ",", header = T)
spp_list <- tolower(spp_list$species)
#choose the correct species 
data$spp_combined <- tolower(paste(data$species, data$host.species))
data$spp_combined <- str_extract(data$spp_combined, paste(spp_list, collapse="|"))

#sheep dataset 
ovis_aries <- data[which(data$spp_combined == "ovis aries" | data$spp_combined == "sheep"),]
ovis_aries$breed <- tolower(ovis_aries$breed)
breed <- read.csv("sheep_breeds.csv", header =F)
breed <- tolower(breed$V1)
#select entries with breeds on the breed list 
ovis_aries$breed_combined <- str_extract(ovis_aries$breed, paste(breed, collapse="|")) 
sheep <- ovis_aries[c("species", "breed_combined")]
sheep$species <- "Sheep"

#cattle dataset
bos_taurus <- data[which(data$spp_combined == "bos taurus" | data$spp_combined == "bovine" | data$spp_combined == "cow" | data$spp_combined == "cattle"),]
bos_taurus$breed <- tolower(bos_taurus$breed)
breed <- read.csv("cattle_breeds.csv", header =F)
breed <- tolower(breed$V1)
#select entries with breeds on the breed list 
bos_taurus$breed_combined <- str_extract(bos_taurus$breed, paste(breed, collapse="|")) 
cattle <- bos_taurus[c("species", "breed_combined")]
cattle$species <- "Cattle"

#chicken dataset
gallus <- data[which(data$spp_combined == "gallus gallus" | data$spp_combined == "chicken" | data$spp_combined == "hen" | data$spp_combined == "poultry"),]
gallus$breed <- tolower(gallus$breed)
breed <- read.csv("chicken_breeds.csv", header =F)
breed <- tolower(breed$V1)
#select entries with breeds on the breed list 
gallus$breed_combined <- str_extract(gallus$breed, paste(breed, collapse="|")) 
chicken <- gallus[c("species", "breed_combined")]
chicken$species <- "Chicken"

#pig dataset
sus_scrofa <- data[which(data$spp_combined == "sus scrofa" | data$spp_combined == "pig"),]
sus_scrofa$breed <- tolower(sus_scrofa$breed)
breed <- read.csv("pig_breeds.csv", header =F)
breed <- tolower(breed$V1)
#select entries with breeds on the breed list 
sus_scrofa$breed_combined <- str_extract(sus_scrofa$breed, paste(breed, collapse="|")) 
pig <- sus_scrofa[c("species", "breed_combined")]
pig$species <- "Pig"

#goat dataset 
capra_hircus <- data[which(data$spp_combined == "capra hircus" | data$spp_combined == "goat"),]
capra_hircus$breed <- tolower(capra_hircus$breed)
breed <- read.csv("goat_breeds.csv", header =F)
breed <- tolower(breed$V1)
#select entries with breeds on the breed list 
capra_hircus$breed_combined <- str_extract(capra_hircus$breed, paste(breed, collapse="|")) 
goat <- capra_hircus[c("species", "breed_combined")]
goat$species <- "Goat"

#equine dataset 
equid <- data[which(data$spp_combined == "equid" | data$spp_combined == "equine" | data$spp_combined == "equus ferus caballus" | data$spp_combined == "equus ferus"| data$spp_combined == "equus caballus" | data$spp_combined == "equus kiang" | data$spp_combined == "equus zebra" | data$spp_combined == "equus asinus x equus caballus" | data$spp_combined == "equus przewalskii" | data$spp_combined == "equus hemionus kulan" | data$spp_combined == "equus asinus" | data$spp_combined == "donkey"),]
equid$breed <- tolower(equid$breed)
breed <- read.csv("gbads_spp.csv", header =F)
breed <- tolower(breed$V1)
#select entries with breeds on the breed list 
equid$breed_combined <- str_extract(equid$breed, paste(breed, collapse="|")) 
equid <- equid[c("species", "breed_combined")]
equid$species <- "Equine"

#combine all species datasets 
df <- rbind(sheep, cattle, chicken, pig, goat, equid)
df$text <- df$breed_combined
#if certain species do not have breed information, add that as the plot text 
df[is.na(df)] <- "Zero breed information"
df$text <- str_to_title(df$text)
df$breed_combined <- str_to_title(df$breed_combined)

cols <- brewer.pal(11, "Paired")

#count number of entries per species 
d <- df %>% group_by_all() %>% summarise(COUNT = n())
d <- data.frame(d)

#plot
p <- ggplot(data=d, aes(x=species, y=COUNT, fill=breed_combined)) +
  geom_bar(stat="identity", fill="#E84C3D")+
  labs(x = "Species", y = "Number of entries")+ ylim(0,100000)
p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 10)) + labs(tag = "A")

#faceted breed plot 
d %>%
  filter(COUNT > 50) %>%
  ggplot(aes(x=breed_combined, y=COUNT)) +
  geom_col() +
  scale_y_log10(labels = scales::comma, name = "Number of Entries") + labs(x="Breeds") + 
  facet_wrap(.~species, scales = "free_x") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        strip.text = element_text(face = "bold"),
        panel.border = element_rect(fill = NA, color = "gray90"))



############################################################
#disease plots 
############################################################

############################################################
#combining disease and host disease in original dataset
############################################################
data$comb_disease <- paste0(data$disease, " ", data$host.disease)
data$comb_disease <- gsub("  ", "", data$comb_disease)
data$comb_disease <- trimws(data$comb_disease)
data$comb_disease <- tolower(data$comb_disease)

############################################################
#editing format of phrases in WOAH disease dataset
############################################################
disease <- disease$Name_english
disease <- c(disease,WOAH_agents$name_eng) 
disease <- tolower(disease)
disease <- gsub("infection with ", "", disease)
disease <- gsub("infestation of ", "", disease)
disease <- gsub("infection of ", "", disease)
disease <- gsub("brucella suis", "brucellosis", disease)
disease <- disease[-which(disease =="")]

############################################################
#editing format of phrases in original dataset
############################################################
data$comb_disease <- paste0(data$disease, " ", data$host.disease)
data$comb_disease <- gsub("  ", "", data$comb_disease)
data$comb_disease <- trimws(data$comb_disease)
data$comb_disease <- tolower(data$comb_disease)
data$comb_disease <- gsub("necrotizing enteritis", "transmissible gastroenteritis virus", data$comb_disease)
data$comb_disease <- gsub("enteritis/diarrhea", "transmissible gastroenteritis virus", data$comb_disease)
data$comb_disease <- gsub("enteritis / diarrhea", "transmissible gastroenteritis virus", data$comb_disease)
data$comb_disease <- gsub("necrotic enteritis", "transmissible gastroenteritis virus", data$comb_disease)
data$comb_disease <- gsub("mastitis metritis and agalactia", "mycoplasma agalactiae", data$comb_disease)
data$comb_disease <- gsub("bovine tuberculosis", "mycobacterium tuberculosis complex", data$comb_disease)
data$comb_disease <- gsub("lumpy skin disease", "lumpy skin disease virus", data$comb_disease)
data[4919,]$comb_disease = "bovine babesiosis"
data$comb_disease <- gsub("foot-and-mouth disease", "foot and mouth disease virus", data$comb_disease)
data$comb_disease <- gsub("infectious bovine keratoconjunctivits", "conjunctivitis", data$comb_disease)
data$comb_disease <- gsub("asf", "african swine fever", data$comb_disease)
data$comb_disease <- gsub("tb", "mycobacterium tuberculosis complex", data$comb_disease)
data$comb_disease <- gsub("johne's disease", "paratuberculosis", data$comb_disease)
data[275485,]$comb_disease = "contagious equine metritis"
data[275486,]$comb_disease = "contagious equine metritis"
data[276070,]$comb_disease = "contagious equine metritis"
data[276103,]$comb_disease = "contagious equine metritis"
data$comb_disease <- gsub("infectious synovitis", "mycoplasma synoviae", data$comb_disease)
data$comb_disease <- gsub("mac disease", "mycobacterium avium subsp. paratuberculosis", data$comb_disease)
data$comb_disease <- gsub("middle east respiratory syndrome", "middle east respiratory syndrome coronavirus (mers-cov)", data$comb_disease)
data$comb_disease <- gsub("salmonellosis", "salmonellosis (s. abortusovis)", data$comb_disease)
data$comb_disease <- gsub("paucibacillary paratuberculosis", "paratuberculosis", data$comb_disease)
data$comb_disease <- gsub("multibacillary paratuberculosis", "paratuberculosis", data$comb_disease)
data$comb_disease <- gsub("mycoplasma bovis pneumonia", "contagious bovine pleuropneumonia", data$comb_disease)
data$comb_disease <- gsub("synovitis", "mycoplasma synoviae", data$comb_disease)
data$comb_disease <- gsub("tuberculose", "mycobacterium tuberculosis complex", data$comb_disease)
data$comb_disease <- gsub("hemorrhagic bowel syndrome", "epizootic hemorrhagic disease virus", data$comb_disease)
data$comb_disease <- gsub("salmonella gastroenteritis", "transmissible gastroenteritis virus", data$comb_disease)
data$comb_disease <- gsub("infetious bursal disease", "infectious bursal disease", data$comb_disease)
data$comb_disease <- gsub("bovine viral diarrhea virus", "bovine viral diarrhoea", data$comb_disease)
data$comb_disease <- gsub("cystic hydatid disease", "echinococcosis", data$comb_disease)
data$comb_disease <- gsub("venerealis diseases", "campylobacter fetus subsp. venerealis", data$comb_disease)
data$comb_disease <- gsub("infectious bronchitis", "avian infectious bronchitis", data$comb_disease)
data$comb_disease <- gsub("pi - bvdv1a", "bovine viral diarrhoea", data$comb_disease)
data$comb_disease <- gsub("pi - bvdv1b", "bovine viral diarrhoea", data$comb_disease)
data$comb_disease <- gsub("pi - bvdv2", "bovine viral diarrhoea", data$comb_disease)
data$comb_disease <- gsub("brucelosis", "brucellosis", data$comb_disease)
data$comb_disease <- gsub("meningitis;pneumonia;intestinal serosal hemorrage", "contagious bovine pleuropneumonia", data$comb_disease)
data$comb_disease <- gsub("avian influenza", "high pathogenicity avian influenza viruses", data$comb_disease)
data$comb_disease <- gsub("animal african trypanosomiasis", "trypanosomiases", data$comb_disease)
data$comb_disease <- gsub("campylobacteriosis", "campylobacter fetus subsp. venerealis", data$comb_disease)
data$comb_disease <- gsub("cbpp", "mycoplasmamycoides subsp. mycoides sc (contagious bovine pleuropneumonia)", data$comb_disease)
data$comb_disease <- gsub("swine respiratory disease", "porcine reproductive and respiratory syndrome virus", data$comb_disease)
data$comb_disease <- gsub("brucellios", "brucellosis", data$comb_disease)
data$comb_disease <- gsub("prrs", "porcine reproductive and respiratory syndrome virus", data$comb_disease)
data$comb_disease <- gsub("slight or mild diarrhea mild enteritis", "transmissible gastroenteritis", data$comb_disease)
data$comb_disease <- gsub("h5n1 high pathogenicity avian influenza viruses infection", "high pathogenicity avian influenza viruses", data$comb_disease)
data$comb_disease <- gsub("bse", "bovine spongiform encephalopathy", data$comb_disease)
data$comb_disease <- gsub("infected with ibdv strain f52/70", "infectious bursal disease", data$comb_disease)
data$comb_disease <- gsub("induce transmissible gastroenteritis virus in chickens", "transmissible gastroenteritis virus", data$comb_disease)
data$comb_disease <- gsub("h9n2 infected", "high pathogenicity avian influenza viruses", data$comb_disease)
data$comb_disease <- gsub("resipiratory disease", "porcine reproductive and respiratory syndrome virus", data$comb_disease)
data$comb_disease <- gsub("johne's", "paratuberculosis", data$comb_disease)
data$comb_disease <- gsub("infected with hp-porcine reproductive and respiratory syndrome virusv", "porcine reproductive and respiratory syndrome virus", data$comb_disease)
data$comb_disease <- gsub("glanders", "burkholderia mallei (glanders)", data$comb_disease)
data$comb_disease <- gsub("west nile neuroinvasive disease", "west nile fever", data$comb_disease)
data$comb_disease <- gsub("african horse sickness", "african horse sickness virus", data$comb_disease)
data$comb_disease <- gsub("venezuelan equine encephalitis", "venezuelan equine encephalomyelitis virus", data$comb_disease)
data$comb_disease <- gsub("eastern equine encephalitis", "equine encephalomyelitis (eastern)", data$comb_disease)
data$comb_disease <- gsub("western equine encephalomyelitis", "western equine encephalomyelitis virus", data$comb_disease)
data$comb_disease <- gsub("suspected metritis", "contagious equine metritis", data$comb_disease)
data$comb_disease <- gsub("bacterial enteritis", "transmissible gastroenteritis", data$comb_disease)
data$comb_disease <- gsub("ehm", "equid herpesvirus-1  (ehv-1)", data$comb_disease)
data$comb_disease <- gsub("bacterial septicemia ulcerative and suppurative bacterial enterocolitis", "transmissible gastroenteritis", data$comb_disease)
data$comb_disease <- gsub("african_horse_sickness_disease", "african horse sickness virus", data$comb_disease)
data$comb_disease <- gsub("isav", "infectious salmon anemia virus", data$comb_disease)
data$comb_disease <- gsub("salmonid alpha virus", "salmonid alphavirus", data$comb_disease)
data$comb_disease <- gsub("infectious salmon anemia virus infected", "infectious salmon anemia virus", data$comb_disease)
data$comb_disease <- gsub("infected with infectious salmon anemia virus", "infectious salmon anemia virus", data$comb_disease)
data$comb_disease <- gsub("infectious salmon anaemia", "hpr-deleted or hpr0 infectious salmon anaemia virus", data$comb_disease)
data$comb_disease <- gsub("infectious salmon ananemia", "hpr-deleted or hpr0 infectious salmon anaemia virus", data$comb_disease)
data$comb_disease <- gsub("fish vibriosis", "vibrio parahaemolyticus", data$comb_disease)
data$comb_disease <- gsub("tilapia lake virus disease", "infection with tilapia lake virus (tilv)", data$comb_disease)
data$comb_disease <- gsub("aeromonas septicaemia", "viral haemorrhagic septicaemia virus", data$comb_disease)
data$comb_disease <- gsub("vibriosis", "vibrio parahaemolyticus", data$comb_disease)
data$comb_disease <- gsub("white spot disease", "white spot syndrome virus", data$comb_disease)
data$comb_disease <- gsub("ahpnd", "acute hepatopancreatic necrosis disease", data$comb_disease)
data$comb_disease <- gsub("standard scrapie sheep prion [()] infected", "scrapie", data$comb_disease)
data$comb_disease <- gsub("standard scrapie sheep prion ssbp/1 infected", "scrapie", data$comb_disease)
data[273351:273368,]$comb_disease <- "venezuelan equine encephalomyelitis virus"
data[41114:41117,]$comb_disease <- "mycobacterium tuberculosis complex"
data[147104:147106,]$comb_disease <- "mycobacterium tuberculosis complex"
data[244108,]$comb_disease <- "mycobacterium tuberculosis complex"
data[220511,]$comb_disease <- "mycobacterium tuberculosis complex"
data[24807,]$comb_disease <- "transmissible gastroenteritis virus"
data[24810:24811,]$comb_disease <- "transmissible gastroenteritis virus"
data[24894,]$comb_disease <- "transmissible gastroenteritis virus"
data[257862:257864,]$comb_disease <- "transmissible gastroenteritis virus"
data[257866,]$comb_disease <- "transmissible gastroenteritis virus"
data[276113:276114,]$comb_disease <- "transmissible gastroenteritis virus"
data[276203,]$comb_disease <- "transmissible gastroenteritis virus"
data[221313:221317,]$comb_disease <- "transmissible gastroenteritis virus"
#data[which(data$comb_disease == "equine encephalomyelitis"),]

############################################################
#determining which entries overlap with WOAH datasets
############################################################

disease_compared <- data[data$comb_disease %in% disease, ]
#disease agents 
spp <- WOAH_agents$name_eng

#which species or diseases are from NCBI are in WOAH disease agents 
y <- data[data$species %in% spp || data$comb_disease %in% spp, ]
y <- y[-which(y$species ==""),]
agents_compared <- y[-which(y$comb_disease ==""),]

#which disease agents are in WOAH disease list 
agents_compared <- agents_compared[agents_compared$comb_disease %in% disease, ]
#final overlapping disease list 
total_compared <- rbind(disease_compared, agents_compared)
total_compared = total_compared[!duplicated(total_compared$ID),]

#choose the correct species 
total_compared$spp_combined <- tolower(paste(total_compared$species, total_compared$host.species))
total_compared$spp_combined <- gsub("salmonella", "", total_compared$spp_combined)
total_compared$spp_combined <- str_extract(total_compared$spp_combined, paste(spp_list, collapse="|"))

#combine data into major species investigated (sheep, cattle, chicken, pig, goat, llama, equine)
total_compared$spp_combined <- gsub("ovis aries", "sheep", total_compared$spp_combined)
total_compared$spp_combined <- gsub("bos taurus", "cattle", total_compared$spp_combined)
total_compared$spp_combined <- gsub("bovine", "cattle", total_compared$spp_combined)
total_compared$spp_combined <- gsub("cow", "cattle", total_compared$spp_combined)
total_compared$spp_combined <- gsub("livestock", "cattle", total_compared$spp_combined)
total_compared$spp_combined <- gsub("gallus gallus", "chicken", total_compared$spp_combined)
total_compared$spp_combined <- gsub("hen", "chicken", total_compared$spp_combined)
total_compared$spp_combined <- gsub("poultry", "chicken", total_compared$spp_combined)
total_compared$spp_combined <- gsub("sus scrofa", "pig", total_compared$spp_combined)
total_compared$spp_combined <- gsub("capra hircus", "goat", total_compared$spp_combined)
total_compared$spp_combined <- gsub("llama glama", "llama", total_compared$spp_combined)
total_compared$spp_combined <- gsub("equus asinus", "equine", total_compared$spp_combined)
total_compared$spp_combined <- gsub("equus caballus", "equine", total_compared$spp_combined)
total_compared$spp_combined <- gsub("horse", "equine", total_compared$spp_combined)
total_compared$spp_combined <- gsub("equus ferus caballus", "equine", total_compared$spp_combined)
total_compared$spp_combined <- gsub("equid", "equine", total_compared$spp_combined)

#number of unique diseases per species 
p <- total_compared %>% 
  group_by(spp_combined) %>% 
  filter(!duplicated(comb_disease)) %>% 
  summarise(sum = length(unique(comb_disease)))

subset <- subset(p, subset = p$spp_combined %in% c("camel", "cattle", "chicken", "donkey", "equine", "goat", "pig", "goat"))

#plot: number of WOAH diseases per species 
plot<-ggplot(data=subset, aes(x= reorder(spp_combined, -sum), y=sum)) +
  geom_bar(stat="identity", fill="#E84C3D")+
  labs(x = "Species", y = "Number of WOAH emerging diseases") + labs(tag = "B")
plot <- plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 10)) 


#############################
#code from above to make barplot for number of entries per species 
data$spp_combined <- tolower(paste(data$species, data$host.species))
data$spp_combined <- gsub("salmonella", "", data$spp_combined)
data$spp_combined <- str_extract(data$spp_combined, paste(spp_list, collapse="|"))
ovis_aries <- data[which(data$spp_combined == "ovis aries" | data$spp_combined == "sheep"),]
bos_taurus <- data[which(data$spp_combined == "bos taurus" | data$spp_combined == "bovine" | data$spp_combined == "cow" | data$spp_combined == "cattle" | data$spp_combined == "livestock"),]
gallus <- data[which(data$spp_combined == "gallus gallus" | data$spp_combined == "chicken" | data$spp_combined == "hen" | data$spp_combined == "poultry"),]
sus_scrofa <- data[which(data$spp_combined == "sus scrofa" | data$spp_combined == "pig"),]
capra_hircus <- data[which(data$spp_combined == "capra hircus" | data$spp_combined == "goat"),]
llama <- data[which(data$spp_combined == "lama glama" | data$spp_combined == "llama"),]
camel <- data[which(data$spp_combined == "camel"),]
equid <- data[which(data$spp_combined == "equid" | data$spp_combined == "equine" | data$spp_combined == "equus ferus caballus" | data$spp_combined == "equus ferus"| data$spp_combined == "equus caballus" | data$spp_combined == "equus kiang" | data$spp_combined == "equus zebra" | data$spp_combined == "equus asinus x equus caballus" | data$spp_combined == "equus przewalskii" | data$spp_combined == "equus hemionus kulan" | data$spp_combined == "equus asinus" | data$spp_combined == "donkey"),]
cattle_num <- nrow(bos_taurus)
sheep_num <- nrow(ovis_aries)
cow_num <- nrow(bos_taurus)
chicken_num <- nrow(gallus)
pig_num <- nrow(sus_scrofa)
goat_num <- nrow(capra_hircus)
equid_num <- nrow(equid)
llama_num <- nrow(llama)
camel_num <- nrow(camel)
df <- data.frame(species=c("cattle", "chicken", "pig", "sheep", "goat", "equine", "camel", "llama"),
                 count=c(cattle_num, chicken_num, pig_num, sheep_num, goat_num, equid_num, camel_num, llama_num))
# Minimal theme + blue fill color
p<-ggplot(data=df, aes(x= reorder(species, -count), y=count)) +
  geom_bar(stat="identity", fill="#E84C3D")+
  labs(x = "Species", y = "Number of entries")+ ylim(0,100000)
p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 10)) + labs(tag = "A")

#panel plot 
png("panel.png", width = 375, height = 225, units='mm', res = 300)
ggarrange(p, plot, ncol=1, nrow = 2, common.legend = TRUE, legend="right", align="v")
dev.off()


########################################
#missingness heat map
########################################
##choose the correct species 
data$spp_combined <- tolower(paste(data$species, data$host.species))
data$spp_combined <- str_extract(data$spp_combined, paste(spp_list, collapse="|"))

data$sex_miss <- ""
data$disease_miss <- ""

x <- data %>% mutate(lat = case_when(
  working_file$location == coord_loc ~ coord_lat,
  TRUE ~ as.numeric(working_file$lat)
))

#assign binary values to missingness data for sex attribute (1 = present, 0 = absent)
data <- data %>% 
  mutate(sex_miss = case_when(sex == "" ~ 0, TRUE ~1))

#assign binary values to missingness data for disease attribute (1 = present, 0 = absent)
data <- data %>% 
  mutate(disease_miss = case_when(disease== "" ~ 0, TRUE ~1))

#combine missingness and species data 
x <- rbind(data$spp_combined, data$sex_miss, data$disease_miss)

t <- data %>%
  select(spp_combined, sex_miss, disease_miss)

heatmaply_na(
  airquality[1:30, ],
  showticklabels = c(TRUE, FALSE)
)
