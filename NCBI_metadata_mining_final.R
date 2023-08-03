##############################################################################
#updated NCBI metadata mining script for GBADS 
#written by Cassandre Pyne, 2022
##############################################################################

###load in packages
library(rentrez)
library(plyr)
library(stringr)
library(sjmisc)

#set working directory
setwd("/Users/cassandrepyne/Documents/GBADs")

#FUNCTION - ATTRIBUTE CLEANING
##############################################################################
att_clean <- function(attr){
  index_val = split[attr+1]
  index_val = gsub(",", "", index_val)
  index_val = gsub(",", "", index_val)
  index_val = gsub("not applicable", "", index_val)
  index_val = gsub("not collected", "", index_val)
  index_val = gsub("Not collected", "", index_val)
  index_val = gsub("missing", "", index_val)
  index_val = gsub("not provided", "", index_val)
  index_val = gsub("not available", "", index_val)
  index_val = gsub("not determined", "", index_val)
  index_val = gsub("unknown_sex", "", index_val)
  index_val = gsub("unknown", "", index_val)
}


#MAIN FUNCTION - QUERY NCBI DATABASE
##############################################################################

#query <- entrez_search(db = "biosample", term = "(sheep[ORGN] OR chicken[ORGN] OR cow[ORGN] OR pig[ORGN] OR cattle[ORGN] OR goat[ORGN] OR domestic[ORGN] OR livestock[ORGN]) NOT embryo NOT pooled NOT cell type[ATNM] NOT tissue[ATNM] NOT fetal NOT pregnant NOT human vaginal metagenome[ORGN] NOT human gut metagenome[ORGN] NOT homo sapiens[ORGN] NOT human skin metagenome[ORGN] NOT human oral metagenome[ORGN] NOT metagenome[ORGN] NOT gut metagenome[ORGN] NOT skin metagenome[ORGN] NOT mouse gut metagenome[ORGN] NOT bat metagenome[ORGN] NOT mus musculus[ORGN] NOT feces metagenome[ORGN] NOT bird metagenome[ORGN] NOT indoor metagenome[ORGN] NOT fish metagenome[ORGN] NOT rat metagenome[ORGN] NOT pig gut metagenome", retmax=70000)

##69,033
#query <- entrez_search(db = "biosample", term = "(sheep[ORGN] OR poultry[ORGN] OR camel[ORGN] OR donkey[ORGN] OR llama[ORGN] OR chicken[ORGN] OR cow[ORGN] OR pig[ORGN] OR cattle[ORGN] OR goat[ORGN] OR domestic[ORGN] OR livestock[ORGN]) NOT embryo NOT pooled NOT cell type[ATNM] NOT tissue[ATNM] NOT fetal NOT pregnant NOT human vaginal metagenome[ORGN] NOT human gut metagenome[ORGN] NOT homo sapiens[ORGN] NOT human skin metagenome[ORGN] NOT human oral metagenome[ORGN] NOT metagenome[ORGN] NOT gut metagenome[ORGN] NOT skin metagenome[ORGN] NOT mouse gut metagenome[ORGN] NOT bat metagenome[ORGN] NOT mus musculus[ORGN] NOT feces metagenome[ORGN] NOT bird metagenome[ORGN] NOT indoor metagenome[ORGN] NOT fish metagenome[ORGN] NOT rat metagenome[ORGN] NOT pig gut metagenome", retmax=70000)

##261,228
#query <- entrez_search(db = "biosample", term = "(sheep OR poultry OR camel OR donkey OR llama OR chicken OR cow OR pig OR cattle OR goat OR domestic OR livestock) NOT embryo NOT pooled NOT cell type[ATNM] NOT tissue[ATNM] NOT fetal NOT pregnant NOT human vaginal metagenome[ORGN] NOT human gut metagenome[ORGN] NOT homo sapiens[ORGN] NOT human skin metagenome[ORGN] NOT human oral metagenome[ORGN] NOT metagenome[ORGN] NOT gut metagenome[ORGN] NOT skin metagenome[ORGN] NOT mouse gut metagenome[ORGN] NOT bat metagenome[ORGN] NOT mus musculus[ORGN] NOT feces metagenome[ORGN] NOT bird metagenome[ORGN] NOT indoor metagenome[ORGN] NOT fish metagenome[ORGN] NOT rat metagenome[ORGN] NOT pig gut metagenome", retmax=70000)

#309302
query <- entrez_search(db = "biosample", term = "(sheep OR poultry OR camel OR donkey OR llama OR chicken OR cow
                       OR pig OR cattle OR goat OR domestic OR livestock) NOT embryo NOT pooled NOT human vaginal metagenome[ORGN] NOT pig manure NOT cow dung metagenome
                       NOT human gut metagenome[ORGN] NOT homo sapiens[ORGN] NOT human skin metagenome[ORGN] NOT human oral metagenome[ORGN] NOT animal stool NOT Man-made water systems
                       NOT metagenome[ORGN] NOT gut metagenome[ORGN] NOT skin metagenome[ORGN] NOT mouse gut metagenome[ORGN] NOT sludge metagenome NOT Vero cells NOT
                       bat metagenome[ORGN] NOT mus musculus[ORGN] NOT feces metagenome[ORGN] NOT bird metagenome[ORGN] NOT anser albifrons NOT airborne dust
                       NOT poultry rinse NOT milk NOT pollen metagenome NOT Drosophila melanogaster NOT manure metagenome NOT manure NOT pig metagenome NOT chicken feed
                       NOT meat NOT soil metagenome NOT wastewater metagenome NOT biofilter metagenome NOT bioreactor metagenome NOT seed tissue NOT chicken wing NOT chicken leg
                       NOT compost metagenome NOT quail NOT chicken breast NOT food NOT poultry bedding NOT dust metagenome NOT Felis catus NOT rock pigeon NOT chicken drums NOT chicken whole NOT chicken - mixed parts
                       NOT activated sludge metagenome NOT anaerobic digester metagenome NOT food metagenome NOT fungus metagenome NOT biogas fermenter metagenome NOT chicken wings NOT chicken thighs NOT chicken thigh NOT chicken breasts
                       NOT microbial fuel cell metagenome NOT USDA NOT indoor metagenome[ORGN] NOT fish metagenome[ORGN] NOT rat metagenome[ORGN] NOT human feces NOT feces NOT chicken rinse NOT chicken legs 
                       NOT roast chicken NOT frozen chicken NOT wastewater NOT mini-tank NOT silkworm NOT sheep blood NOT pig blood NOT pig feed NOT air metagenome
                       NOT poultry plant NOT green goat NOT Domestic shorthair NOT sheep-grazed NOT guinea pig NOT Homo sapiens NOT poultry litter NOT drinking water metagenome NOT poultry feed NOT duck NOT retail NOT cow dung NOT dung", retmax=310000)

#22419
query <- entrez_search(db = "biosample", term = "(horse OR equid OR equine OR Equus ferus caballus OR Equus ferus OR Equus) NOT (sheep OR poultry OR camel OR donkey OR llama OR chicken OR cow
                       OR pig OR cattle OR goat OR domestic OR livestock) NOT bovine gut metagenome NOT embryo NOT pooled NOT human vaginal metagenome[ORGN] NOT manure metagenome[ORGN]
                       NOT human gut metagenome[ORGN] NOT Horse Gram NOT soil metagenome[ORGN] NOT homo sapiens[ORGN] NOT feces metagenome[ORGN] NOT Faeces NOT human skin metagenome[ORGN] NOT human oral metagenome[ORGN] NOT animal stoolNOT metagenome[ORGN] NOT gut metagenome[ORGN] NOT skin metagenome[ORGN]", retmax = 23000)

##some manure metagenome is from a rectal swab so it is okay to include here 

#22690
query <- entrez_search(db = "biosample", term = "(salmon OR salmo salar) NOT (sheep OR poultry OR camel OR donkey OR llama OR chicken OR cow
                       OR pig OR cattle OR goat OR domestic OR livestock) NOT embryo NOT pooled NOT human vaginal metagenome[ORGN] NOT Salmon Bay NOT food metagenome
                       NOT human gut metagenome[ORGN] NOT homo sapiens[ORGN] NOT feces metagenome[ORGN] NOT Faeces NOT human skin metagenome[ORGN] NOT human oral metagenome[ORGN] NOT skin metagenome[ORGN]", retmax = 23000)


#8690
query <- entrez_search(db = "biosample", term = "(tilapia OR oreochromis) NOT (sheep OR poultry OR camel OR donkey OR llama OR chicken OR cow
                       OR pig OR cattle OR goat OR domestic OR livestock OR salmon) NOT embryo NOT pooled NOT human vaginal metagenome[ORGN] NOT Salmon Bay NOT food metagenome
                       NOT human gut metagenome[ORGN] NOT homo sapiens[ORGN] NOT feces metagenome[ORGN] NOT Faeces NOT human skin metagenome[ORGN] NOT human oral metagenome[ORGN] NOT skin metagenome[ORGN]", retmax = 9000)

#28017
query <- entrez_search(db = "biosample", term = "(clam OR shellfish OR mussel OR lobster OR scallop OR oyster OR shrimp OR crab OR crayfish) NOT (sheep OR poultry OR camel OR donkey OR llama OR chicken OR cow
                       OR pig OR cattle OR goat OR domestic OR livestock OR salmon) NOT embryo NOT pooled NOT human vaginal metagenome[ORGN] NOT Salmon Bay NOT food metagenome NOT sediment metagenome NOT aquaculture metagenome NOT seawater metagenome NOT tank NOT mushroom NOT cooked NOT cold seep NOT Malus NOT paste NOT aquatic metagenome
                       NOT human gut metagenome[ORGN] NOT homo sapiens[ORGN] NOT feces metagenome[ORGN] NOT Faeces NOT human skin metagenome[ORGN] NOT human oral metagenome[ORGN] NOT skin metagenome[ORGN]", retmax = 29000)


##consider running the above query for "host" rather than sheep - may not be picking up on everything (like with salmon - many pathogen listed as orgn but salmon is host)
#query <- entrez_search(db = "biosample", term = "poultry NOT embryo NOT pooled NOT cell type[ATNM] NOT tissue[ATNM] NOT fetal NOT pregnant NOT human vaginal metagenome[ORGN] NOT human gut metagenome[ORGN] NOT homo sapiens[ORGN] NOT human skin metagenome[ORGN] NOT human oral metagenome[ORGN] NOT metagenome[ORGN] NOT gut metagenome[ORGN] NOT skin metagenome[ORGN] NOT mouse gut metagenome[ORGN] NOT bat metagenome[ORGN] NOT mus musculus[ORGN] NOT feces metagenome[ORGN] NOT bird metagenome[ORGN] NOT indoor metagenome[ORGN] NOT fish metagenome[ORGN] NOT rat metagenome[ORGN] NOT pig gut metagenome", retmax=70000)
#query <- entrez_search(db = "biosample", term = "poultry", retmax=70000)

#not Japanese eel, not salmon bank ridge
#query <- entrez_search(db = "biosample", term = "salmon NOT capsicum NOT salmon pink birdeater NOT baker's NOT norovirus NOT glacier metagenome NOT salmon gums NOT salmon mdws NOT salmon processing NOT catfish NOT salmon arm NOT canis NOT salmon pens NOT salmon falls NOT insect metagenome NOT fillet NOT homo sapiens NOT canine NOT food metagenome NOT salmon creek NOT seafood NOT rainbow trout NOT pastrami NOT food NOT MEATCUT[ATNM] NOT Salmon Lake NOT Salmon River NOT caviar NOT warm roll NOT smoked NOT embryo NOT pooled NOT fetal NOT pregnant NOT human vaginal metagenome[ORGN] NOT human gut metagenome[ORGN] NOT homo sapiens[ORGN] NOT human skin metagenome[ORGN] NOT human oral metagenome[ORGN] NOT metagenome[ORGN] NOT skin metagenome[ORGN] NOT mouse gut metagenome[ORGN] NOT bat metagenome[ORGN] NOT mus musculus[ORGN] NOT feces metagenome[ORGN] NOT bird metagenome[ORGN] NOT indoor metagenome[ORGN] NOT fish metagenome[ORGN] NOT rat metagenome[ORGN] NOT pig gut metagenome", retmax=10000)
######################################################################################
#can look at local scale environmental context to look for "salmon farm"  23150584
#can parse some species for "wild" or "farmed" 
######################################################################################

#sheep <- entrez_search(db = "biosample", term = "sheep[ORGN] NOT pooled NOT embryo NOT cell type[ATNM] NOT tissue[ATNM] NOT fetal NOT pregnant", retmax=3305)
#weights <- entrez_search(db = "biosample", term = "weight[ATNM] AND (sheep[ORGN] OR chicken[ORGN] OR cow[ORGN] OR pig[ORGN] OR cattle[ORGN] OR goat[ORGN] OR domestic[ORGN] OR livestock[ORGN] OR horse[ORGN] OR donkey[ORGN] OR buffalo[ORGN] OR yak[ORGN] OR camel[ORGN] OR llama[ORGN] OR alpaca[ORGN] OR rabbit[ORGN])", retmax=162)
#all <- entrez_search(db = "biosample", term = "(sheep[ORGN] OR chicken[ORGN] OR cow[ORGN] OR pig[ORGN] OR cattle[ORGN] OR goat[ORGN] OR domestic[ORGN] OR livestock[ORGN]) NOT embryo NOT pooled NOT cell type[ATNM] NOT tissue[ATNM] NOT fetal NOT pregnant", retmax=20000)
#chicken <- entrez_search(db = "biosample", term = "chicken[ORGN] NOT pooled NOT embryo NOT cell type[ATNM] NOT tissue[ATNM] NOT fetal NOT pregnant", retmax=5000)
#cattle <- entrez_search(db = "biosample", term = "cattle[ORGN] NOT embryo NOT cell type[ATNM] NOT tissue[ATNM]", retmax = 250)

#add NCBI query to a data frame 
matrix <- as.data.frame(matrix(ncol=17))
colnames(matrix) <- c("ID", "GEO ID", "species","host species", "breed", "organization", "date", "collection date", "location","latitude", "longitude", "sex", "age", "weight", "condition", "disease", "host disease")

removed <- as.data.frame(matrix(ncol=17))
colnames(removed) <- c("ID", "GEO ID", "species","host species", "breed", "organization", "date", "collection date", "location","latitude", "longitude", "sex", "age", "weight", "condition", "disease", "host disease")

###ONLY CHANGE LOOP_INDEX AND END_INDEX AND KEEP MATRIX_INDEX THE SAME
##set up index values
##START WITH THESE VALUES
loop_index = 27001
end_index = 27050

#if not subsetting data:
#loop_index = 1 
#end_index = length(query$ids)

#index for loop 
start_index = loop_index

##add indexed values to a list (all IDs to be included)
ID_list <- query$ids[start_index:end_index]

spp_list <- read.csv("gbads_spp.csv", sep = ",", header = T)
spp_list <- tolower(spp_list$species)

matrix_index = 1
removed_index = 1
for(i in 1:nrow(plyr::count(ID_list))){
  #get first ID in the list 
  id = query$ids[start_index]
  #extract data from NCBI for that ID 
  info = entrez_summary(db = "biosample", id = id)

  data <- vector(mode="character", length=17)
  
  #parse sample data for the ID into different attributes 
  split = strsplit(info$sampledata, ">")[[1]]
  
  #add ID, organism, breed, organization and date to data frame 
  data[1] = info$uid
  
  #extract GEO ID from sample data 
  split_GEO = strsplit(info$identifiers, "; ")[[1]]
  attr_GEO = grep(pattern="GEO", split_GEO)
  if(!identical(attr_GEO, integer(0))){ GEO_ID = (strsplit(split_GEO[attr_GEO], "GEO: ")[[1]])[2]
  data[2] = GEO_ID  }
  
  org <- info$organism 
  
  data[3] = info$organism
  
  attr_host = grep(pattern = "=\"host\"", split)
  attr_iso = grep(pattern = "isolation source", split)
  attr_broad_env = grep(pattern = "broad-scale environmental context", split)
  attr_local_env = grep(pattern = "local-scale environmental context", split)
  attr_env_med = grep(pattern = "environmental medium", split)
  attr_isolate = grep(pattern = "isolate", split)
  
  if(!identical(split[attr_iso+1], integer(0)))  iso_source <- att_clean(attr_iso) 
  if(!identical(split[attr_host+1], integer(0))) host <- att_clean(attr_host)
  if(!identical(split[attr_broad_env+1], integer(0))) broad_env <- att_clean(attr_broad_env)
  if(!identical(split[attr_local_env+1], integer(0))) local_env <- att_clean(attr_local_env)
  if(!identical(split[attr_env_med+1], integer(0))) env_med <- att_clean(attr_env_med)
  if(!identical(split[attr_isolate+1], integer(0))) isolate <- att_clean(attr_isolate)
  
  smush <- tolower(paste(host, iso_source, broad_env, local_env, env_med, isolate))
  val <- str_extract(smush, paste(spp_list, collapse="|"))
  smush_2 <- tolower(paste(val, org))
  cor_org <- str_extract(smush_2, paste(spp_list, collapse="|"))
  
  data[4]= str_remove(cor_org[1], "</Attribute") 
  
  data[5] = strsplit(info$infraspecies, ",")[[1]][1]
  data[6] = gsub(",", "", info$organization)
  data[7] = info$date
       
  #parse date data, clean, then add to data frame 
  attr_coldate = grep(pattern = "collection date", split)
  if(!identical(split[attr_coldate+1], character(0)) && !identical(split[attr_coldate+1], integer(0))) { 
    coldate <- att_clean(attr_coldate)
  data[8]= str_remove(coldate, "</Attribute") }
  
  #parse location data, clean, then add to data frame
  attr_location = grep(pattern = "location", split)
  if(!identical(split[attr_location[1]+1], character(0))){ location <- att_clean(attr_location)
  data[9] = str_remove(location[1], "</Attribute") }
  
  attr_lat = grep(pattern = "latitude", split)
  if(!identical(split[attr_lat+1], character(0))){ lat <- att_clean(attr_lat)
  data[10]= str_remove(lat, "</Attribute") }
  
  attr_long = grep(pattern = "longitude", split)
  if(!identical(split[attr_long+1], character(0))){ long <- att_clean(attr_long)
  data[11]= str_remove(long, "</Attribute") }
  
  #parse coordinate data, clean, then add to data frame
  attr_coordinates = grep(pattern="latitude and longitude", split)
  if(!identical(split[attr_coordinates+1], character(0))) { 
  coordinates = att_clean(attr_coordinates)
  coordinates = str_remove(coordinates, "</Attribute")
  coordinates = gsub(" ", "", coordinates)
  if(!is_empty(coordinates)){
    if(grepl("N", coordinates)){
      coordinates = strsplit(coordinates, "N")
     if(grepl("W", coordinates[[1]][2])){
      coordinates[[1]][2] = paste0("-", coordinates[[1]][2])
     } }
    else{
      coordinates = strsplit(coordinates, "S")
      coordinates[[1]][1] = paste0("-", coordinates[[1]][1])
      if(grepl("W", coordinates[[1]][2])){
        coordinates[[1]][2] = paste0("-", coordinates[[1]][2])
      } }
  data[10]= str_remove(coordinates[[1]][1], "</Attribute")
  data[11]= str_remove(coordinates[[1]][2], "</Attribute") }}
  
  #parse sex data, clean, then add to data frame
  attr_sex = grep(pattern="=\"sex", split)
  if(!identical(split[attr_sex+1], character(0))) { sex = att_clean(attr_sex)
  data[12] = str_remove(sex[1], "</Attribute") }
  
  #parse age data, clean, then add to data frame
  attr_age = grep(pattern="attribute_name=\"age", split)
  if(!identical(split[attr_age+1], character(0))){ age = att_clean(attr_age)
  data[13] = str_remove(age[1], "</Attribute") }
  
  #parse weight data, clean, then add to data frame
  attr_weight = grep(pattern="attribute_name=\"weight", split) 
  if(!identical(split[attr_weight+1], character(0))){ 
  weight = att_clean(attr_weight)
  data[14] = str_remove(weight[1], "</Attribute") }
 
  #parse condition data, clean, then add to data frame
  attr_condition = grep(pattern="attribute_name=\"condition", split) 
  if(!identical(split[attr_condition+1], character(0))){ 
  condition = att_clean(attr_condition)
  data[15]= str_remove(condition, "</Attribute") }
  
  #parse disease data, clean, then add to data frame
  attr_disease = grep(pattern="attribute_name=\"disease", split) 
  if(!identical(split[attr_disease+1], character(0))){
  disease = att_clean(attr_disease)
  data[16]= str_remove(disease[1], "</Attribute")
  }
  attr_host_disease = grep(pattern="attribute_name=\"host_disease", split) 
  if(!identical(split[attr_host_disease+1], character(0))){
    host_disease = att_clean(attr_host_disease)
    data[17]= str_remove(host_disease, "</Attribute")
  }
  
  if(!is.null(cor_org) && !is.na(cor_org)){
    matrix[matrix_index,] <- data
    matrix_index <- matrix_index + 1
  }
  else{
    removed[removed_index, ] <- data
    removed_index <- removed_index + 1
  }
  start_index = start_index + 1 
  
  
}

write.table(matrix, "shellfish.csv", row.names = FALSE, append = TRUE, quote = F, sep = ",", col.names = FALSE)
write.table(removed, "shellfish_removed.csv", row.names = FALSE, append = TRUE, quote = F, sep = ",", col.names = FALSE)


write.table(matrix, "tilapia.csv", row.names = FALSE, append = TRUE, quote = F, sep = ",", col.names = FALSE)
write.table(removed, "tilapia_removed.csv", row.names = FALSE, append = TRUE, quote = F, sep = ",", col.names = TRUE)

#write.table(matrix, "salmon.csv", row.names = FALSE, append = TRUE, quote = F, sep = ",", col.names = FALSE)
#write.table(removed, "salmon_removed.csv", row.names = FALSE, append = TRUE, quote = F, sep = ",", col.names = FALSE)

#write.table(matrix[loop_index:nrow(matrix),], "new_file.csv", row.names = FALSE, append = TRUE, quote = F, sep = ",", col.names = FALSE)

#write.table(matrix, "xxx.csv", row.names = FALSE, append = TRUE, quote = F, sep = ",", col.names = FALSE)
#write.table(matrix, "equid.csv", row.names = FALSE, append = TRUE, quote = F, sep = ",", col.names = FALSE)
#write.table(removed, "equid_removed.csv", row.names = FALSE, append = TRUE, quote = F, sep = ",", col.names = FALSE)
