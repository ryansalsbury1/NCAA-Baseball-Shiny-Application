
library("shiny")

library("dplyr")

library(datasets)

library("DT")

library("shinythemes")

library("sqldf")

FilterData <- read.csv("Conference_School.csv", stringsAsFactors = FALSE)

NCAA_2018 <- read.csv("NCAA_Batting_2018_Season.csv", stringsAsFactors = FALSE)
NCAA_Car <- read.csv("NCAA_Batting_2018_Career.csv", stringsAsFactors = FALSE)
NCAA_2018R <- read.csv("NCAA_Batting_2018_Season_vr.csv", stringsAsFactors = FALSE)
NCAA_CarR <- read.csv("NCAA_Batting_2018_Career_vr.csv", stringsAsFactors = FALSE)
NCAA_2018L <- read.csv("NCAA_Batting_2018_Season_vl.csv", stringsAsFactors = FALSE)
NCAA_CarL <- read.csv("NCAA_Batting_2018_Career_vl.csv", stringsAsFactors = FALSE)


#Change Column Names
colnames(NCAA_2018) <- c("Player", "Conference", "School", "Yr", "Pos", "Draft", "Pick", "PA", "AVG", "OBP", 
                         "SLG","HR", "ISO", "SB", "SBA", "K%", "BB%", "BB%-K%", "BABIP", "wOBA", "wRAA", "wRC") 
colnames(NCAA_Car) <- c("Player", "Conference", "School", "Yr", "Pos", "Draft", "Pick", "PA", "AVG", "OBP", 
                         "SLG","HR", "ISO", "SB", "SBA", "K%", "BB%", "BB%-K%", "BABIP", "wOBA","wRAA", "wRC")
colnames(NCAA_2018R) <- c("Player", "Conference", "School", "Yr", "Pos", "Draft", "Pick", "PA", "AVG", "OBP", 
                         "SLG","HR", "ISO", "SB", "SBA", "K%", "BB%", "BB%-K%", "BABIP", "wOBA","wRAA", "wRC") 
colnames(NCAA_CarR) <- c("Player", "Conference", "School", "Yr", "Pos", "Draft", "Pick", "PA", "AVG", "OBP", 
                        "SLG","HR", "ISO", "SB", "SBA", "K%", "BB%", "BB%-K%", "BABIP", "wOBA","wRAA", "wRC")
colnames(NCAA_2018L) <- c("Player", "Conference", "School", "Yr", "Pos", "Draft", "Pick", "PA", "AVG", "OBP", 
                         "SLG","HR", "ISO", "SB", "SBA", "K%", "BB%", "BB%-K%", "BABIP", "wOBA","wRAA", "wRC") 
colnames(NCAA_CarL) <- c("Player", "Conference", "School", "Yr", "Pos", "Draft", "Pick", "PA", "AVG", "OBP", 
                        "SLG","HR", "ISO", "SB", "SBA", "K%", "BB%", "BB%-K%", "BABIP", "wOBA","wRAA", "wRC")
     

NCAA_P_2018 <- read.csv("NCAA_Pitching_2018_Season.csv", stringsAsFactors = FALSE)
NCAA_P_Car <- read.csv("NCAA_Pitching_2018_Career.csv", stringsAsFactors = FALSE)
NCAA_P_2018R <- read.csv("NCAA_Pitching_2018_Season_vr.csv", stringsAsFactors = FALSE)
NCAA_P_CarR <- read.csv("NCAA_Pitching_2018_Career_vr.csv", stringsAsFactors = FALSE)
NCAA_P_2018L <- read.csv("NCAA_Pitching_2018_Season_vl.csv", stringsAsFactors = FALSE)
NCAA_P_CarL <- read.csv("NCAA_Pitching_2018_Career_vl.csv", stringsAsFactors = FALSE)


#Change Column Names
colnames(NCAA_P_2018) <- c("Player","Conference", "School", "Yr", "Pos", "Draft", "Pick", "IP", "PA", "Pitches",
                                 "K%", "BB%", "K%-BB%", "AVG", "OBP", "SLG", "wOBA", "GO%", "FO%", "ERA", "FIP")
colnames(NCAA_P_Car) <- c("Player","Conference", "School", "Yr", "Pos", "Draft", "Pick", "IP", "PA", "Pitches",
                            "K%", "BB%", "K%-BB%", "AVG", "OBP", "SLG", "wOBA", "GO%", "FO%", "ERA", "FIP")
colnames(NCAA_P_2018R) <- c("Player","Conference", "School", "Yr", "Pos", "Draft", "Pick", "IP", "PA",
                           "K%", "BB%", "K%-BB%", "AVG", "OBP", "SLG", "wOBA", "GO%", "FO%", "FIP")
colnames(NCAA_P_CarR) <- c("Player","Conference", "School", "Yr", "Pos", "Draft", "Pick", "IP", "PA",
                          "K%", "BB%", "K%-BB%", "AVG", "OBP", "SLG", "wOBA", "GO%", "FO%", "FIP")
colnames(NCAA_P_2018L) <- c("Player","Conference", "School", "Yr", "Pos", "Draft", "Pick", "IP", "PA",
                           "K%", "BB%", "K%-BB%", "AVG", "OBP", "SLG", "wOBA", "GO%", "FO%", "FIP")
colnames(NCAA_P_CarL) <- c("Player","Conference", "School", "Yr", "Pos", "Draft", "Pick", "IP", "PA",
                          "K%", "BB%", "K%-BB%", "AVG", "OBP", "SLG", "wOBA", "GO%", "FO%", "FIP")




shinyApp(
  ui = tagList(
    navbarPage(inverse = TRUE, position = c("fixed-top"),
               # theme = "cerulean",  # <--- To use a theme, uncomment this
               theme = shinytheme("spacelab"),
               "NCAA Baseball 2018",
               tabPanel("Batting",
                        
                        sidebarPanel(width = 3,
                                     
                                     selectInput("allconferences", "Conference", c("All", NCAA_2018$Conference), width = "170px"),
                                  
                                     uiOutput("secondSelection"),
                                     sliderInput("paInput", "PA", min = 0, max = max(NCAA_Car$PA), value = c(0,max(NCAA_Car$PA))),
                                     sliderInput("isoInput", "ISO", min = 0, max = 1, value = c(0,max(1))),
                                     sliderInput("bbkInput", "BB%-K%", min = -50, max = 20, value = c(-50,max(20))),
                                     sliderInput("wobaInput", "wOBA", min = 0, max = .600, value = c(0,max(.600))),
                                     sliderInput("wrcInput", "wRC", min = 0, max = 220, value = c(-2,max(220)))
                                     
            
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Season Totals", DT::dataTableOutput("NCAABattingSeason")),
                            tabPanel("Career Totals", DT::dataTableOutput("NCAABattingCareer")),
                            tabPanel("Season vs. RHP", DT::dataTableOutput("NCAABattingSeasonR")),
                            tabPanel("Season vs. LHP", DT::dataTableOutput("NCAABattingSeasonL")),
                            tabPanel("Career vs. RHP", DT::dataTableOutput("NCAABattingCareerR")),
                            tabPanel("Career vs. LHP", DT::dataTableOutput("NCAABattingCareerL"))
                            
                          )
                        )
               ),
               tabPanel("Pitching",
                        
                        sidebarPanel(width = 3,
                                     
                                     
                                     
                                     selectInput("pitchingallconferences", "Conference", c("All", NCAA_P_2018$Conference), width = "170px"),
                                     
                                     uiOutput("pitchingsecondSelection"),
                                     
                                     sliderInput("ipInput", "Innings Pitched", min = 0, max = max(NCAA_P_Car$IP), value = c(0,max(NCAA_P_Car$IP))),
                                     sliderInput("kkbInput", "K%-BB%", min = -20, max = 50, value = c(-20,50)),
                                     sliderInput("fipInput", "FIP", min = 0, max = 10, value = c(0,10))
                  
                        ),
                        
                        mainPanel(tags$style(type="text/css", "body {padding-top: 60px;}"),
                          tabsetPanel(
                            tabPanel("Season Totals", DT::dataTableOutput("NCAAPitchingSeason")),
                            tabPanel("Career Totals", DT::dataTableOutput("NCAAPitchingCareer")),
                            tabPanel("Season vs. RHB", DT::dataTableOutput("NCAAPitchingSeasonR")),
                            tabPanel("Season vs. LHB", DT::dataTableOutput("NCAAPitchingSeasonL")),
                            tabPanel("Career vs. RHB", DT::dataTableOutput("NCAAPitchingCareerR")),
                            tabPanel("Career vs. LHB", DT::dataTableOutput("NCAAPitchingCareerL"))
                            
                          )
                        )
               )
    )
  ),
  
  
  #DT::dataTableOutput("resultstable")
  server <- shinyServer(function(input, output, session) {
    
  

    output$secondSelection <- renderUI({
    
      
      
     if (input$allconferences != "All") {selectInput("allschools", "School", choices = c("All", as.character(FilterData[FilterData$Conference==input$allconferences,"School"])))}
     else {selectInput("allschools", "School", choices = c("All", FilterData$School))}
    })
    
    output$pitchingsecondSelection <- renderUI({
      
      
      
      if (input$pitchingallconferences != "All") {selectInput("pitchingallschools", "School", choices = c("All", as.character(FilterData[FilterData$Conference==input$pitchingallconferences,"School"])))}
      else {selectInput("pitchingallschools", "School", choices = c("All", FilterData$School))}
    })
    
    
    #NCAA Batting Season Tab
    output$NCAABattingSeason <- DT::renderDataTable({
      
      if(is.null(input$allschools) || is.na(input$allschools))
      {
        conferences <- NULL
        schools <- NULL
      }
      
      else if (input$allconferences == "All" & input$allschools == "All" ){
        conferences <- NCAA_2018$Conference
        schools <- NCAA_2018$School
      } 
      
      else if (input$allconferences != "All" & input$allschools != "All"){
        conferences <- input$allconferences
        schools <- input$allschools
      } 
      else if (input$allconferences == "All" & input$allschools != "All"){
        conferences <- NCAA_2018$Conference
        schools <- input$allschools
      } 
      else {
        conferences <- input$allconferences
        schools <- NCAA_2018$School
      } 
      
      NCAA_2018 <- filter(NCAA_2018, NCAA_2018$Conference %in% conferences, NCAA_2018$School %in% schools,  PA >= input$paInput[1], PA <= input$paInput[2], wOBA >= input$wobaInput[1], wOBA <= input$wobaInput[2], wRC >= input$wrcInput[1], wRC <= input$wrcInput[2], ISO >= input$isoInput[1], ISO <= input$isoInput[2], NCAA_2018$"BB%-K%" >= input$bbkInput[1], NCAA_2018$"BB%-K%" <= input$bbkInput[2])
      
      
      brks <- quantile(NCAA_2018$wOBA, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
    
      brks2 <- quantile(NCAA_2018$ISO, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs2 <- round(seq(255, 40, length.out = length(brks2) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      brks3 <- quantile(NCAA_2018$wRC, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs3 <- round(seq(255, 40, length.out = length(brks3) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      brks4 <- quantile(NCAA_2018$"BB%-K%", probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs4 <- round(seq(255, 40, length.out = length(brks4) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      datatable(NCAA_2018, options = list(sDom  = '<"top">t<"bottom">flip', pageLength = 50)) %>% formatStyle('wOBA', backgroundColor = styleInterval(brks, clrs)) %>% formatStyle('ISO', backgroundColor = styleInterval(brks2, clrs2)) %>% formatStyle('wRC', backgroundColor = styleInterval(brks3, clrs3)) %>% formatStyle('BB%-K%', backgroundColor = styleInterval(brks4, clrs4))
                                                                                              
      
    }) 
    
    #NCAA Pitching Season Tab
    output$NCAAPitchingSeason <- DT::renderDataTable({
      
      
      
      if(is.null(input$pitchingallschools) || is.na(input$pitchingallschools))
      {
        pitchingconferences <- NULL
        pitchingschools <- NULL
      }
      
      
      else if (input$pitchingallconferences == "All" & input$pitchingallschools == "All" ){
        pitchingconferences <- NCAA_P_2018$Conference
        pitchingschools <- NCAA_P_2018$School
      } 
      
      else if (input$pitchingallconferences != "All" & input$pitchingallschools != "All"){
        pitchingconferences <- input$pitchingallconferences
        pitchingschools <- input$pitchingallschools
      } 
      else if (input$pitchingallconferences == "All" & input$pitchingallschools != "All"){
        pitchingconferences <- NCAA_P_2018$Conference
        pitchingschools <- input$pitchingallschools
      } 
      else {
        pitchingconferences <- input$pitchingallconferences
        pitchingschools <- NCAA_P_2018$School
      } 
      
      
      NCAA_P_2018 <- filter(NCAA_P_2018, NCAA_P_2018$Conference %in% pitchingconferences, NCAA_P_2018$School %in% pitchingschools, IP >= input$ipInput[1], IP <= input$ipInput[2], FIP >= input$fipInput[1], FIP <= input$fipInput[2], NCAA_P_2018$"K%-BB%" >= input$kkbInput[1], NCAA_P_2018$"K%-BB%" <= input$kkbInput[2])
      
      brks <- quantile(NCAA_P_2018$FIP, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(40, 255, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks1 <- quantile(NCAA_P_2018$"K%-BB%", probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs1 <- round(seq(255, 40, length.out = length(brks1) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      datatable(NCAA_P_2018, options = list(sDom  = '<"top">t<"bottom">flip', pageLength = 50)) %>% formatStyle('FIP', backgroundColor = styleInterval(brks, clrs)) %>% formatStyle('K%-BB%', backgroundColor = styleInterval(brks1, clrs1))
  }) 
    
    
    
    #NCAA Batting Career Tab
    output$NCAABattingCareer <- DT::renderDataTable({
      
      if (input$allconferences == "All" & input$allschools == "All" ){
        conferences <- NCAA_Car$Conference
        schools <- NCAA_Car$School
      } 
      
      else if (input$allconferences != "All" & input$allschools != "All"){
        conferences <- input$allconferences
        schools <- input$allschools
      } 
      else if (input$allconferences == "All" & input$allschools != "All"){
        conferences <- NCAA_Car$Conference
        schools <- input$allschools
      } 
      else {
        conferences <- input$allconferences
        schools <- NCAA_Car$School
      } 
      
      
      NCAA_Car <- filter(NCAA_Car, NCAA_Car$Conference %in% conferences, NCAA_Car$School %in% schools, PA >= input$paInput[1], PA <= input$paInput[2], wOBA >= input$wobaInput[1], wOBA <= input$wobaInput[2], wRC >= input$wrcInput[1], wRC <= input$wrcInput[2], ISO >= input$isoInput[1], ISO <= input$isoInput[2], NCAA_Car$"BB%-K%" >= input$bbkInput[1], NCAA_Car$"BB%-K%" <= input$bbkInput[2])
      
      brks <- quantile(NCAA_Car$wOBA, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks1 <- quantile(NCAA_Car$ISO, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs1 <- round(seq(255, 40, length.out = length(brks1) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks2 <- quantile(NCAA_Car$wRC, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs2 <- round(seq(255, 40, length.out = length(brks2) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks3<- quantile(NCAA_Car$"BB%-K%", probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs3 <- round(seq(255, 40, length.out = length(brks3) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      datatable(NCAA_Car, options = list(sDom  = '<"top">t<"bottom">flip', pageLength = 50)) %>% formatStyle('wOBA', backgroundColor = styleInterval(brks, clrs)) %>% formatStyle('ISO', backgroundColor = styleInterval(brks1, clrs1)) %>% formatStyle('wRC', backgroundColor = styleInterval(brks2, clrs2)) %>% formatStyle('BB%-K%', backgroundColor = styleInterval(brks3, clrs3))
      
    })
    
    #NCAA Pitching Career Tab
    output$NCAAPitchingCareer <- DT::renderDataTable({
      
      
      if (input$pitchingallconferences == "All" & input$pitchingallschools == "All" ){
        pitchingconferences <- NCAA_P_Car$Conference
        pitchingschools <- NCAA_P_Car$School
      } 
      
      else if (input$pitchingallconferences != "All" & input$pitchingallschools != "All"){
        pitchingconferences <- input$pitchingallconferences
        pitchingschools <- input$pitchingallschools
      } 
      else if (input$pitchingallconferences == "All" & input$pitchingallschools != "All"){
        pitchingconferences <- NCAA_P_Car$Conference
        pitchingschools <- input$pitchingallschools
      } 
      else {
        pitchingconferences <- input$pitchingallconferences
        pitchingschools <- NCAA_P_Car$School
      } 
      
      
      NCAA_P_Car <- filter(NCAA_P_Car, NCAA_P_Car$Conference %in% pitchingconferences, NCAA_P_Car$School %in% pitchingschools, IP >= input$ipInput[1], IP <= input$ipInput[2], FIP >= input$fipInput[1], FIP <= input$fipInput[2], NCAA_P_Car$"K%-BB%" >= input$kkbInput[1], NCAA_P_Car$"K%-BB%" <= input$kkbInput[2])
      
      brks <- quantile(NCAA_P_Car$FIP, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(40, 255, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks1 <- quantile(NCAA_P_Car$"K%-BB%", probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs1 <- round(seq(255, 40, length.out = length(brks1) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      datatable(NCAA_P_Car, options = list(sDom  = '<"top">t<"bottom">flip', pageLength = 50)) %>% formatStyle('FIP', backgroundColor = styleInterval(brks, clrs)) %>% formatStyle('K%-BB%', backgroundColor = styleInterval(brks1, clrs1))    })
    
    
    #NCAA Batting Season vs. RHP Tab
    output$NCAABattingSeasonR <- DT::renderDataTable({
      
      if (input$allconferences == "All" & input$allschools == "All" ){
        conferences <- NCAA_2018R$Conference
        schools <- NCAA_2018R$School
      } 
      
      else if (input$allconferences != "All" & input$allschools != "All"){
        conferences <- input$allconferences
        schools <- input$allschools
      } 
      else if (input$allconferences == "All" & input$allschools != "All"){
        conferences <- NCAA_2018R$Conference
        schools <- input$allschools
      } 
      else {
        conferences <- input$allconferences
        schools <- NCAA_2018R$School
      } 
      
      
      NCAA_2018R <- filter(NCAA_2018R, NCAA_2018R$Conference %in% conferences, NCAA_2018R$School %in% schools, PA >= input$paInput[1], PA <= input$paInput[2], wOBA >= input$wobaInput[1], wOBA <= input$wobaInput[2], wRC >= input$wrcInput[1], wRC <= input$wrcInput[2], ISO >= input$isoInput[1], ISO <= input$isoInput[2], NCAA_2018R$"BB%-K%" >= input$bbkInput[1], NCAA_2018R$"BB%-K%" <= input$bbkInput[2])
      
      brks <- quantile(NCAA_2018R$wOBA, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks1 <- quantile(NCAA_2018R$ISO, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs1 <- round(seq(255, 40, length.out = length(brks1) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks2 <- quantile(NCAA_2018R$wRC, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs2 <- round(seq(255, 40, length.out = length(brks2) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks3<- quantile(NCAA_2018R$"BB%-K%", probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs3 <- round(seq(255, 40, length.out = length(brks3) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      datatable(NCAA_2018R, options = list(sDom  = '<"top">t<"bottom">flip', pageLength = 50)) %>% formatStyle('wOBA', backgroundColor = styleInterval(brks, clrs)) %>% formatStyle('ISO', backgroundColor = styleInterval(brks1, clrs1)) %>% formatStyle('wRC', backgroundColor = styleInterval(brks2, clrs2)) %>% formatStyle('BB%-K%', backgroundColor = styleInterval(brks3, clrs3))
      
    })
    
    #NCAA Pitching Season vs. RHB Tab
    output$NCAAPitchingSeasonR <- DT::renderDataTable({
      
      if (input$pitchingallconferences == "All" & input$pitchingallschools == "All" ){
        pitchingconferences <- NCAA_P_2018R$Conference
        pitchingschools <- NCAA_P_2018R$School
      } 
      
      else if (input$pitchingallconferences != "All" & input$pitchingallschools != "All"){
        pitchingconferences <- input$pitchingallconferences
        pitchingschools <- input$pitchingallschools
      } 
      else if (input$pitchingallconferences == "All" & input$pitchingallschools != "All"){
        pitchingconferences <- NCAA_P_2018R$Conference
        pitchingschools <- input$pitchingallschools
      } 
      else {
        pitchingconferences <- input$pitchingallconferences
        pitchingschools <- NCAA_P_2018R$School
      } 
      
      
      NCAA_P_2018R <- filter(NCAA_P_2018R, NCAA_P_2018R$Conference %in% pitchingconferences, NCAA_P_2018R$School %in% pitchingschools, IP >= input$ipInput[1], IP <= input$ipInput[2], FIP >= input$fipInput[1], FIP <= input$fipInput[2], NCAA_P_2018R$"K%-BB%" >= input$kkbInput[1], NCAA_P_2018R$"K%-BB%" <= input$kkbInput[2])
      
      brks <- quantile(NCAA_P_2018R$FIP, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(40, 255, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks1 <- quantile(NCAA_P_2018R$"K%-BB%", probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs1 <- round(seq(255, 40, length.out = length(brks1) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      datatable(NCAA_P_2018R, options = list(sDom  = '<"top">t<"bottom">flip', pageLength = 50)) %>% formatStyle('FIP', backgroundColor = styleInterval(brks, clrs)) %>% formatStyle('K%-BB%', backgroundColor = styleInterval(brks1, clrs1))   })
    
    #NCAA Batting Season vs. LHP Tab
    output$NCAABattingSeasonL <- DT::renderDataTable({
      
      if (input$allconferences == "All" & input$allschools == "All" ){
        conferences <- NCAA_2018L$Conference
        schools <- NCAA_2018L$School
      } 
      
      else if (input$allconferences != "All" & input$allschools != "All"){
        conferences <- input$allconferences
        schools <- input$allschools
      } 
      else if (input$allconferences == "All" & input$allschools != "All"){
        conferences <- NCAA_2018L$Conference
        schools <- input$allschools
      } 
      else {
        conferences <- input$allconferences
        schools <- NCAA_2018L$School
      } 
      
      
      NCAA_2018L <- filter(NCAA_2018L, NCAA_2018L$Conference %in% conferences, NCAA_2018L$School %in% schools, PA >= input$paInput[1], PA <= input$paInput[2], wOBA >= input$wobaInput[1], wOBA <= input$wobaInput[2], wRC >= input$wrcInput[1], wRC <= input$wrcInput[2], ISO >= input$isoInput[1], ISO <= input$isoInput[2], NCAA_2018L$"BB%-K%" >= input$bbkInput[1], NCAA_2018L$"BB%-K%" <= input$bbkInput[2])
      
      brks <- quantile(NCAA_2018L$wOBA, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks1 <- quantile(NCAA_2018L$ISO, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs1 <- round(seq(255, 40, length.out = length(brks1) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks2 <- quantile(NCAA_2018L$wRC, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs2 <- round(seq(255, 40, length.out = length(brks2) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks3<- quantile(NCAA_2018L$"BB%-K%", probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs3 <- round(seq(255, 40, length.out = length(brks3) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      datatable(NCAA_2018L, options = list(sDom  = '<"top">t<"bottom">flip', pageLength = 50)) %>% formatStyle('wOBA', backgroundColor = styleInterval(brks, clrs)) %>% formatStyle('ISO', backgroundColor = styleInterval(brks1, clrs1)) %>% formatStyle('wRC', backgroundColor = styleInterval(brks2, clrs2)) %>% formatStyle('BB%-K%', backgroundColor = styleInterval(brks3, clrs3))
      
    })
    
    #NCAA Pitching Season vs. LHB Tab
    output$NCAAPitchingSeasonL <- DT::renderDataTable({
      
      if (input$pitchingallconferences == "All" & input$pitchingallschools == "All" ){
        pitchingconferences <- NCAA_P_2018L$Conference
        pitchingschools <- NCAA_P_2018L$School
      } 
      
      else if (input$pitchingallconferences != "All" & input$pitchingallschools != "All"){
        pitchingconferences <- input$pitchingallconferences
        pitchingschools <- input$pitchingallschools
      } 
      else if (input$pitchingallconferences == "All" & input$pitchingallschools != "All"){
        pitchingconferences <- NCAA_P_2018L$Conference
        pitchingschools <- input$pitchingallschools
      } 
      else {
        pitchingconferences <- input$pitchingallconferences
        pitchingschools <- NCAA_P_2018L$School
      } 
      
      
      NCAA_P_2018L <- filter(NCAA_P_2018L, NCAA_P_2018L$Conference %in% pitchingconferences, NCAA_P_2018L$School %in% pitchingschools, IP >= input$ipInput[1], IP <= input$ipInput[2], FIP >= input$fipInput[1], FIP <= input$fipInput[2], NCAA_P_2018L$"K%-BB%" >= input$kkbInput[1], NCAA_P_2018L$"K%-BB%" <= input$kkbInput[2])
      
      brks <- quantile(NCAA_P_2018L$FIP, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(40, 255, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks1 <- quantile(NCAA_P_2018L$"K%-BB%", probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs1 <- round(seq(255, 40, length.out = length(brks1) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      datatable(NCAA_P_2018L, options = list(sDom  = '<"top">t<"bottom">flip', pageLength = 50)) %>% formatStyle('FIP', backgroundColor = styleInterval(brks, clrs)) %>% formatStyle('K%-BB%', backgroundColor = styleInterval(brks1, clrs1))    })
    
    #NCAA Batting Career vs. RHP Tab
    output$NCAABattingCareerR <- DT::renderDataTable({
      
      if (input$allconferences == "All" & input$allschools == "All" ){
        conferences <- NCAA_CarR$Conference
        schools <- NCAA_CarR$School
      } 
      
      else if (input$allconferences != "All" & input$allschools != "All"){
        conferences <- input$allconferences
        schools <- input$allschools
      } 
      else if (input$allconferences == "All" & input$allschools != "All"){
        conferences <- NCAA_CarR$Conference
        schools <- input$allschools
      } 
      else {
        conferences <- input$allconferences
        schools <- NCAA_CarR$School
      } 
      
      
      NCAA_CarR <- filter(NCAA_CarR, NCAA_CarR$Conference %in% conferences, NCAA_CarR$School %in% schools, PA >= input$paInput[1], PA <= input$paInput[2], wOBA >= input$wobaInput[1], wOBA <= input$wobaInput[2], wRC >= input$wrcInput[1], wRC <= input$wrcInput[2], ISO >= input$isoInput[1], ISO <= input$isoInput[2], NCAA_CarR$"BB%-K%" >= input$bbkInput[1], NCAA_CarR$"BB%-K%" <= input$bbkInput[2])
      
      brks <- quantile(NCAA_CarR$wOBA, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks1 <- quantile(NCAA_CarR$ISO, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs1 <- round(seq(255, 40, length.out = length(brks1) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks2 <- quantile(NCAA_CarR$wRC, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs2 <- round(seq(255, 40, length.out = length(brks2) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks3<- quantile(NCAA_CarR$"BB%-K%", probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs3 <- round(seq(255, 40, length.out = length(brks3) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      datatable(NCAA_CarR, options = list(sDom  = '<"top">t<"bottom">flip', pageLength = 50)) %>% formatStyle('wOBA', backgroundColor = styleInterval(brks, clrs)) %>% formatStyle('ISO', backgroundColor = styleInterval(brks1, clrs1)) %>% formatStyle('wRC', backgroundColor = styleInterval(brks2, clrs2)) %>% formatStyle('BB%-K%', backgroundColor = styleInterval(brks3, clrs3))
      
    })
    
    #NCAA Pitching Career vs. RHB Tab
    output$NCAAPitchingCareerR <- DT::renderDataTable({
      
      if (input$pitchingallconferences == "All" & input$pitchingallschools == "All" ){
        pitchingconferences <- NCAA_P_CarR$Conference
        pitchingschools <- NCAA_P_CarR$School
      } 
      
      else if (input$pitchingallconferences != "All" & input$pitchingallschools != "All"){
        pitchingconferences <- input$pitchingallconferences
        pitchingschools <- input$pitchingallschools
      } 
      else if (input$pitchingallconferences == "All" & input$pitchingallschools != "All"){
        pitchingconferences <- NCAA_P_CarR$Conference
        pitchingschools <- input$pitchingallschools
      } 
      else {
        pitchingconferences <- input$pitchingallconferences
        pitchingschools <- NCAA_P_CarR$School
      } 
      
      
      NCAA_P_CarR <- filter(NCAA_P_CarR, NCAA_P_CarR$Conference %in% pitchingconferences, NCAA_P_CarR$School %in% pitchingschools, IP >= input$ipInput[1], IP <= input$ipInput[2], FIP >= input$fipInput[1], FIP <= input$fipInput[2], NCAA_P_CarR$"K%-BB%" >= input$kkbInput[1], NCAA_P_CarR$"K%-BB%" <= input$kkbInput[2])
      
      brks <- quantile(NCAA_P_CarR$FIP, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(40, 255, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks1 <- quantile(NCAA_P_CarR$"K%-BB%", probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs1 <- round(seq(255, 40, length.out = length(brks1) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      datatable(NCAA_P_CarR, options = list(sDom  = '<"top">t<"bottom">flip', pageLength = 50)) %>% formatStyle('FIP', backgroundColor = styleInterval(brks, clrs)) %>% formatStyle('K%-BB%', backgroundColor = styleInterval(brks1, clrs1))  })
    
    
    
    #NCAA Batting Career vs. LHP Tab
    output$NCAABattingCareerL <- DT::renderDataTable({
      
      if (input$allconferences == "All" & input$allschools == "All" ){
        conferences <- NCAA_CarL$Conference
        schools <- NCAA_CarL$School
      } 
      
      else if (input$allconferences != "All" & input$allschools != "All"){
        conferences <- input$allconferences
        schools <- input$allschools
      } 
      else if (input$allconferences == "All" & input$allschools != "All"){
        conferences <- NCAA_CarL$Conference
        schools <- input$allschools
      } 
      else {
        conferences <- input$allconferences
        schools <- NCAA_CarL$School
      } 
      
      
      NCAA_CarL <- filter(NCAA_CarL, NCAA_CarL$Conference %in% conferences, NCAA_CarL$School %in% schools, PA >= input$paInput[1], PA <= input$paInput[2], wOBA >= input$wobaInput[1], wOBA <= input$wobaInput[2], wRC >= input$wrcInput[1], wRC <= input$wrcInput[2], ISO >= input$isoInput[1], ISO <= input$isoInput[2], NCAA_CarL$"BB%-K%" >= input$bbkInput[1], NCAA_CarL$"BB%-K%" <= input$bbkInput[2])
      
      brks <- quantile(NCAA_CarL$wOBA, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks1 <- quantile(NCAA_CarL$ISO, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs1 <- round(seq(255, 40, length.out = length(brks1) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks2 <- quantile(NCAA_CarL$wRC, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs2 <- round(seq(255, 40, length.out = length(brks2) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks3<- quantile(NCAA_CarL$"BB%-K%", probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs3 <- round(seq(255, 40, length.out = length(brks3) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      datatable(NCAA_CarL, options = list(sDom  = '<"top">t<"bottom">flip', pageLength = 50)) %>% formatStyle('wOBA', backgroundColor = styleInterval(brks, clrs)) %>% formatStyle('ISO', backgroundColor = styleInterval(brks1, clrs1)) %>% formatStyle('wRC', backgroundColor = styleInterval(brks2, clrs2)) %>% formatStyle('BB%-K%', backgroundColor = styleInterval(brks3, clrs3))
      
    })
    
    
    #NCAA Pitching Career vs. LHB Tab
    output$NCAAPitchingCareerL <- DT::renderDataTable({
      
      if (input$pitchingallconferences == "All" & input$pitchingallschools == "All" ){
        pitchingconferences <- NCAA_P_CarL$Conference
        pitchingschools <- NCAA_P_CarL$School
      } 
      
      else if (input$pitchingallconferences != "All" & input$pitchingallschools != "All"){
        pitchingconferences <- input$pitchingallconferences
        pitchingschools <- input$pitchingallschools
      } 
      else if (input$pitchingallconferences == "All" & input$pitchingallschools != "All"){
        pitchingconferences <- NCAA_P_CarL$Conference
        pitchingschools <- input$pitchingallschools
      } 
      else {
        pitchingconferences <- input$pitchingallconferences
        pitchingschools <- NCAA_P_CarL$School
      } 
      
      
      NCAA_P_CarL <- filter(NCAA_P_CarL, NCAA_P_CarL$Conference %in% pitchingconferences, NCAA_P_CarL$School %in% pitchingschools, IP >= input$ipInput[1], IP <= input$ipInput[2], FIP >= input$fipInput[1], FIP <= input$fipInput[2], NCAA_P_CarL$"K%-BB%" >= input$kkbInput[1], NCAA_P_CarL$"K%-BB%" <= input$kkbInput[2])
      
      brks <- quantile(NCAA_P_CarL$FIP, probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs <- round(seq(40, 255, length.out = length(brks) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      
      brks1 <- quantile(NCAA_P_CarL$"K%-BB%", probs = seq(.05, .95, .05), na.rm = TRUE)
      clrs1 <- round(seq(255, 40, length.out = length(brks1) + 1), 0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}
      datatable(NCAA_P_CarL, options = list(sDom  = '<"top">t<"bottom">flip', pageLength = 50)) %>% formatStyle('FIP', backgroundColor = styleInterval(brks, clrs)) %>% formatStyle('K%-BB%', backgroundColor = styleInterval(brks1, clrs1))    })
    
    
  }))










