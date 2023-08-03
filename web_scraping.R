##############################################################
#Script for web scraping for breed lists 
#Script written by Cassandre Pyne for GBADs
##############################################################

#load necessary packages 
library(rvest)
library(dplyr)

#######SHEEP and CATTLE
url <- "http://afs.okstate.edu/breeds/sheep/#"
url <- "https://breeds.okstate.edu/cattle/index.html"

#choose specific areas of website to grab 
flat_html <- readLines(con=url)
flat_html <- flat_html[212:262]
flat_html <- flat_html[-c(1:900)]

#clean data 
t <- as.character(flat_html[flat_html!=""])
s <- grep("/breeds/sheep", t, value = TRUE)

x <- function(var){
 # var <- gsub(".*\"internal-link\">", "", var)
  var <- gsub("</a>.*", "", var)
  #var <- gsub(".*>", "", var)
  #var <- gsub(">.*", "", var)
}
var <- gsub("<div class=\"alphabetical-list__letter-group\">", "", t)
var <- gsub("<h3 class=\"alphabetical-list__letter\">S</h3><a href=\"/cattle/", "", var)
l <- lapply(t, x)

write.table(t(as.data.frame(l)), file = "t.csv", row.names=F, col.names=F, quote = F, sep = ",")

#######PIG
url <- "https://www.livestockoftheworld.com/pigs/?ScreenWidth=1295"
flat_html <- readLines(con=url)
pig <- flat_html[44]
p <- unlist(strsplit(pig, ", "))
p <- gsub(".*information on", "", p)
write.table(as.data.frame(p), file = "pig_breeds.csv", row.names=F, col.names=F, quote = F, sep = ",")

#######GOAT
url <- "https://www.livestockoftheworld.com/Goats/?Screenwidth=1200"
flat_html <- readLines(con=url)
goat <- flat_html[46]
g <- unlist(strsplit(goat, ", "))
g <- gsub(".*information on", "", g)
write.table(as.data.frame(g), file = "goat_breeds.csv", row.names=F, col.names=F, quote = F, sep = ",")

#######CHICKEN
url <- "https://www.livestockoftheworld.com/Chickens/?Screenwidth=1200"
flat_html <- readLines(con=url)
chicken <- flat_html[46]
c <- unlist(strsplit(chicken, ", "))
c <- gsub(".*information on", "", c)
write.table(as.data.frame(c), file = "chicken_breeds.csv", row.names=F, col.names=F, quote = F, sep = ",")

#######CATTLE
url <- "https://www.livestockoftheworld.com/Cattle/?Screenwidth=1200"
flat_html <- readLines(con=url)
cattle <- flat_html[44]
cattle <- unlist(strsplit(cattle, ", "))
cattle <- gsub(".*information on", "", cattle)
write.table(as.data.frame(cattle), file = "cattle_breeds.csv", row.names=F, col.names=F, quote = F, sep = ",")

#######DONKEY
url <- "https://www.livestockoftheworld.com/Donkeys/?Screenwidth=1200"
flat_html <- readLines(con=url)
donkey <- flat_html[44]
d <- unlist(strsplit(donkey, ", "))
d <- gsub(".*information on", "", d)
write.table(as.data.frame(d), file = "donkey_breeds.csv", row.names=F, col.names=F, quote = F, sep = ",")




