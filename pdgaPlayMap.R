###
# data prep for the 
#
#
###


install.packages("DT")
library(rvest)
library(dplyr)
library(sf)
library(tmap)
library(DT)
tmap::tmap_mode("view")

# pro, male, current
wp <- "https://www.pdga.com/players?FirstName=&LastName=&PDGANum=&Status=Current&Gender=M&Class=P&MemberType=All&City=&StateProv=All&Country=All&Country_1=All&UpdateDate=&order=Rating_1&sort=desc"

for(i in 1:20){
  # 25 per page so this return the top 500 rated players
  if(i == 1){
    webpage <- read_html(wp)
    t2 <- html_table(webpage, fill = TRUE)
    df <- t2[[1]]
  }else{
    webpage <- read_html(paste0(wp,"&page=",as.character(i)))
    t2 <- html_table(webpage, fill = TRUE)
    df <- dplyr::bind_rows(df,t2)
  }
  print(i)
}
View(df)
write.csv(df, file = paste0("D:/PDGA/data/top500mens",Sys.Date(),".csv"))

df <- read.csv(file = "D:/PDGA/data/top500mens2021-04-05.csv")

sum1 <- df %>%
  dplyr::group_by(State.Prov)%>%
  dplyr::summarise(total = n(), average_rating = mean(Rating))

# add link to player PDGA site 
df$link <- paste0("https://www.pdga.com/player/",df$PDGA..)

df$link2 <- paste0("<a href='",df$link,"'>",df$PDGA..,"</a>")
df$PDGA.. <- df$link2
df <- df[,c(1:4,6:8)]
names(df) <- c("Global Rank","Name", "PDGA Number", "Rating", "Home Town", "Home State", "Country")
# this to make a national map -- focusing on the us for now 
## read in data and join the summary file 
states <- sf::st_read("D:/genericSpatialData/US/states/tl_2017_us_state.shp")%>%
  dplyr::left_join(y = sum1, by = c("STUSPS" = "State.Prov")) %>%
  dplyr::select("STUSPS", "NAME", "total", "average_rating","geometry")



### need 

# tutorial on referencing cities 
  # https://rstudio-pubs-static.s3.amazonaws.com/489236_0259d8532a354ad6945d818bc4c052f1.html
install.packages("geonames")
library(geonames)
options(geonamesUsername=carverd)
GNsearch(name_equals = "Fargo")

