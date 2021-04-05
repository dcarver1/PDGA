install.packages("rvest")
library(rvest)
library(dplyr)
library(sf)

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
sum1 <- df %>%
  dplyr::group_by(`State/Prov`)%>%
  dplyr::summarise(total = n(), average_rating = mean(Rating))

# this to make a national map

# tutorial on referencing cities 
# https://rstudio-pubs-static.s3.amazonaws.com/489236_0259d8532a354ad6945d818bc4c052f1.html
