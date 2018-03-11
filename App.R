########################## LOADING LIBRARIES #######################

library(shinythemes)
library(shiny)
library(png)
library(shinydashboard)
library(dplyr)

########################### READING DATA ###########################

imdb<-read.csv('https://www.dropbox.com/s/znfnfpmlijc0bpk/imdb_shiny_final.csv?dl=1',header=T)
imdb$X<-NULL
set.seed(123)
sample <- sample.int(n = nrow(imdb), size = floor(.75*nrow(imdb)), replace = F)
train <- imdb[sample, ]
lmfit<-lm(imdb_score~.,data=train)

########################### RSHINY APP #############################

ui<-fluidPage(
  titlePanel(column(12, offset = 5, tags$img(src = "imdb2.png", width =200))),
  column(6, offset = 3 , tags$hr(style="border: solid 1.5px black")),
  tags$head(
            tags$style(HTML("body{
                background-image: url(https://deepfriedbits.files.wordpress.com/2014/04/iy23bpe.jpg);
  }"))),
  fluidRow(column(width = 4, offset = 2, 
         wellPanel(top = 75, style="border: solid 2px green;background-color:black",
           sliderInput("fb_likes",
                       h2("Movie Fb Likes", 
                          style = "color:gold;font-family:Courier;text-align:center",
                          icon("facebook-official",class="fa-align-right fa-3x")), 
                       value=43000,min=0,max=50000,step=1000),
           sliderInput("critics",
                       h2("Critics' Reviews", 
                          style = "color:gold;font-family:Courier;text-align:center",
                          icon("thumbs-up",class="fa-align-right fa-3x")), 
                       value=208,min=1,max=1000,step=50),
           selectizeInput("genres",
                          h2("Movie Genre(s)", 
                             style = "color:gold;font-family:Courier;text-align:center",
                             icon("list-ol",class="fa-align-right fa-3x")),
                          c("Action", "Animation", "Comedy", "Documentary",
                            "Family", "Horror", "Musical", "Romance", "Sport",
                            "War", "Adventure", "Biography","Crime","Drama",
                            "Fantasy","History","Music","Mystery","Sci-Fi","Thriller","Western"),
                          c("Crime","Drama"),
                          multiple = TRUE),
           sliderInput("user_votes",
                       h2("Users Voted", 
                          style = "color:gold;font-family:Courier;text-align:center",
                          icon("users",class="fa-align-right fa-3x")), 
                       value=1155770,min=10000,max=1300000,step=1000)
    )), 
     column(width = 4,
        wellPanel(style="border: solid 2px green;background-color: black",
           numericInput("duration",
                        h2("Movie Duration", 
                           style = "color:gold;font-family:Courier;text-align:center",
                           icon("hourglass",class="fa-align-right fa-3x")), 
                        value=175,min=60,max=300),
           radioButtons("faces",
                        h2("Poster Faces", 
                           style = "color:gold;font-family:Courier;text-align:center",
                           icon("image",class="fa-align-right fa-3x")), 
                        choiceNames= list(
                          HTML("<p style='color:gold;'>0</p>"),
                          HTML("<p style='color:gold;'>1</p>"), 
                          HTML("<p style='color:gold;'>2</p>"),
                          HTML("<p style='color:gold;'>3</p>"),
                          HTML("<p style='color:gold;'>4</p>"),
                          HTML("<p style='color:gold;'>5</p>"),
                          HTML("<p style='color:gold;'>6</p>"),
                          HTML("<p style='color:gold;'>7</p>")), inline = TRUE, selected = 1,
                        choiceValues= list(0,1,2,3,4,5,6,7)),
           numericInput("budget",
                        h2("Movie Budget", 
                           style = "color:gold;font-family:Courier;text-align:center",
                           icon("usd",class="fa-align-right fa-3x")), 
                        value=34436454,min=10000000,max=400000000),
           sliderInput("user_reviews",
                       h2("User Reviews", 
                          style = "color:gold;font-family:Courier;text-align:center",
                          icon("comments",class="fa-align-right fa-3x")), 
                       value=2238,min=50,max=5000,step=100)
           
  ))),
  column(4, offset=4, 
         wellPanel(style="border: solid 2px green;background-color:black",
                actionButton("go", "Check it out!", 
                                 style="display:inline-block;width:100%;text-align: center; font-size: 30px;background-color:gold",
                                 icon = icon("exclamation-sign",lib="glyphicon")),
                h2("The IMDB Rating", style="color:gold"),
                      verbatimTextOutput("value")
         ))
  )


server<-function(input,output){
  data1<-reactive({
    ifelse(input$genres=="Action",1,0)})
  data2<-reactive({
    ifelse(input$genres=="Animation",1,0)})
  data3<-reactive({
    ifelse(input$genres=="Comedy",1,0)})
  data4<-reactive({
    ifelse(input$genres=="Documentary",1,0)})
  data5<-reactive({
    ifelse(input$genres=="Family",1,0)})
  data6<-reactive({
    ifelse(input$genres=="Horror",1,0)})
  data7<-reactive({
    ifelse(input$genres=="Musical",1,0)})
  data8<-reactive({
    ifelse(input$genres=="Romance",1,0)})
  data9<-reactive({
    ifelse(input$genres=="Sport",1,0)})
  data10<-reactive({
    ifelse(input$genres=="War",1,0)})
  data11<-reactive({
    ifelse(input$genres=="Adventure",1,0)})
  data12<-reactive({
    ifelse(input$genres=="Biography",1,0)})
  data13<-reactive({
    ifelse(input$genres=="Crime",1,0)})
  data14<-reactive({
    ifelse(input$genres=="Drama",1,0)})
  data15<-reactive({
    ifelse(input$genres=="Fantasy",1,0)})
  data16<-reactive({
    ifelse(input$genres=="History",1,0)})
  data17<-reactive({
    ifelse(input$genres=="Music",1,0)})
  data18<-reactive({
    ifelse(input$genres=="Mystery",1,0)})
  data19<-reactive({
    ifelse(input$genres=="Sci-Fi",1,0)})
  data20<-reactive({
    ifelse(input$genres=="Thriller",1,0)})
  data21<-reactive({
    ifelse(input$genres=="Western",1,0)})
  
  output$value <- renderText({
    if (input$go > 0){
    pred <- predict(lmfit,
                    newdata = data.frame(movie_facebook_likes=as.numeric(input$fb_likes),
                                         num_critic_for_reviews=input$critics,
                                         duration=input$duration,
                                         num_voted_users=input$user_votes,
                                         facenumber_in_poster=as.numeric(input$faces),
                                         num_user_for_reviews=input$user_reviews,
                                         movie_facebook_likes=input$fb_likes,
                                         budget_net=input$budget,
                    
                    action=as.numeric(data1()),
                    animation=as.numeric(data2()),
                    comedy=as.numeric(data3()),
                    documentary=as.numeric(data4()),
                    family=as.numeric(data5()),
                    horror=as.numeric(data6()),
                    musical=as.numeric(data7()),
                    romance=as.numeric(data8()),
                    sport=as.numeric(data9()),
                    war=as.numeric(data10()),
                    adventure=as.numeric(data11()),
                    biography=as.numeric(data12()),
                    crime=as.numeric(data13()),
                    drama=as.numeric(data14()),
                    fantasy=as.numeric(data15()),
                    history=as.numeric(data16()),
                    music=as.numeric(data17()),
                    mystery=as.numeric(data18()),
                    sci_fi=as.numeric(data19()),
                    thriller=as.numeric(data20()),
                    western=as.numeric(data21())),
                                               interval="predict")
                    
    a<-pred
    paste(a[1])
    }}
  )}
shinyApp(ui=ui,server=server)

####################### THE END #############################