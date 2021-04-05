library(shiny)
library(tmap)
library(dplyr)
library(sf)
library(DT)
tmap::tmap_mode("view")


df <- read.csv(file = "./data/top500mens2021-04-05.csv")

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
states <- sf::st_read("./data/states/tl_2017_us_state.shp")%>%
  dplyr::left_join(y = sum1, by = c("STUSPS" = "State.Prov")) %>%
  dplyr::select("STUSPS", "NAME", "total", "average_rating","geometry")




t2 <- as.character(unique(df$`Home State`))


## user interface 
ui <- fluidPage(
    fluidRow(
      column(3, 
          helpText("Select a State to show the players"),
          selectInput("state",
                         label = "Choose your State",
                         choices = t2,
                         selected = "CO"),
      ),
      column(9,
             shiny::h2("The top 500 professional male disc golfers in the United States"),
             fluidRow(
               column(12,
                      shiny::h5("Number of Professionals Per State"),
                      #Output: map ----
                      tmap::tmapOutput("map", width = "100%", height = 400)
               ),
               fluidRow(
                 column(12, DTOutput('table'))
               )
               
      )
    )
  )
) 
  



## server
server <- function(input, output) {
  output$table <- renderDT({
    df %>%
      dplyr::filter(df$`Home State` == input$"state")
  }, escape = FALSE, options = list(pageLength = 5))
  
  output$map <- renderTmap(tm_shape(states)+
      tm_polygons(col = "total"),
    env = parent.frame(), quoted = FALSE)
  
}
shinyApp(ui, server)

