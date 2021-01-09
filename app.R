#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(httr)
library(tidyverse)
#library(devtools)
library(DT)

source("toConnectToKobo.R")
# Define UI for application that draws a histogram

kobohr_getdata_csv<-function(url,u,pw){
    #supply url for the data
    rawdata<-GET(url,authenticate(u,pw),progress())
    d_content <- read_csv(content(rawdata,"raw",encoding = "UTF-8"), na = "n/a")
}

url_midline <-"https://kc.humanitarianresponse.info/api/v1/data/691626.csv"
url_backcheck <-"https://kc.humanitarianresponse.info/api/v1/data/701627.csv"


backcheck_keep <- c("index", "survey_agency",
                    "district_bl", "name_call_back")

backcheck_var <- c("Telephone_Number",
                   "head_hh_yn", "displacement_status",
                   "move_plan_12mths", "MealsEatPerDay",
                   "shelter_type", "electric_hh_yn", "HH_size", 
                   "males_gte_18", "females_gte_18", "edu_level_hh",
                   "meet_basic_needs", 
                   "main_source_income", "land_owned_by","csi_score")
backcheck_keep <- paste0(backcheck_keep,".s")
backcheck_var <- paste0(backcheck_var,".s")

midline_keep <- c("index", "survey_agency", "State",
                  "District", "username",
                  "gps_location", "_gps_location_latitude", "_gps_location_longitude",
                  "_gps_location_altitude", "_gps_location_precision", "Respondent_name", "start", "CompletionDateTime", "interviewDuration",
                  "interviewDuringDay", "reasonableDuration", "nbDontknow")
midline_var <- c( "PhoneNumber",
                  "QuHeadOfHH", "DisplacementStatus_Full",
                  "MovePlan", "MealsEatPerDay",
                  "ShelterType", "ElectricityAccess", "hh_size", 
                  "Male_18_plus_years_old", "Female_18_plus_years_old", "Highest_Head_Secular_Education",
                  "SelfReliance", 
                  "MainIncomeSource", "LandOwnership",
                  "csi_score")
midline_keep <- paste0(midline_keep,".m")
midline_var <- paste0(midline_var,".m")
varshown <- c("percentMatch", "index.m", "survey_agency.m","username.m", "start.m", "CompletionDateTime.m","interviewDuration.m", "nbDontknow.m",
              "State.m","District.m", c(rbind(midline_var, backcheck_var)))
allvars <- c("percentMatch",midline_keep, midline_var, backcheck_keep, backcheck_var)

listData <- function(){
    d <- data.frame(matrix(ncol=length(varshown), nrow = 0))
    colnames(d)<-varshown
    return(d)
}


prepareData <- function(midline, backcheck){
    
    backcheck$csi_score <- as.numeric(backcheck$csiLess)+ as.numeric(backcheck$csiBorrow)+ as.numeric(backcheck$csiReduce)+ as.numeric(backcheck$csiAdult)+ as.numeric(backcheck$csiFewer)
    midline$csi_score <- as.numeric(midline$csiLess)+ as.numeric(midline$csiBorrow)+ as.numeric(midline$csiReduce)+ as.numeric(midline$csiAdult)+ as.numeric(midline$csiFewer)
    midline$interviewDuration <- difftime(midline$CompletionDateTime, midline$start, units='mins')
    midline$interviewDuringDay <- between(format(midline$start, format="%H%M%S"),40000, 160000)
    midline$reasonableDuration <- between(midline$interviewDuration, 30, 90)
    midline$nbDontknow <- apply(midline,1,function(x) sum(x=="dontknow", na.rm=T))
    
    
    colnames(midline) <- paste0(colnames(midline),".m")
    colnames(backcheck) <- paste0(colnames(backcheck),".s")
    
    data_check <- left_join(midline[,c(midline_keep,midline_var)], backcheck[,c(backcheck_keep,backcheck_var)], by=c("index.m"="index.s"), keep=TRUE)
    
    data_check$qualScore <-0
    
    for(i in 1:length(backcheck_var)){
        isItDifferent <- ifelse(data_check[,backcheck_var[i]]!=data_check[,midline_var[i]], 1, 0)
        if(backcheck_var[i] %in% c("age_years", "HH_size")){
            isItDifferent <- (abs(data_check[,backcheck_var[i]]-data_check[,midline_var[i]])) > 1
        }else if(backcheck_var[i]=="csi_score"){
            isItDifferent <- (abs(data_check[,backcheck_var[i]]-data_check[,midline_var[i]])) > 3
        }
        
        data_check$qualScore <- data_check$qualScore + ifelse(is.na(isItDifferent), .5, isItDifferent)
    }
    
    data_check$percentMatch <- ifelse(is.na(data_check$index.s), NA, 100-data_check$qualScore/length(backcheck_var)*100)
    
    
    return(data_check)
}



get_data <- function(login, password){
    d_midline <- tryCatch(kobohr_getdata_csv(url_midline,login,password), error=function(e){message("can't access data")})
    d_backcheck <- tryCatch(kobohr_getdata_csv(url_backcheck,login,password), error=function(e){message("can't access data")})
    if(length(d_midline)>1 & length(d_midline)>1){
        midline <- as.data.frame(d_midline)
        colnames(midline) <-gsub(".*/","",colnames(midline))
        midline$survey_agency[is.na(midline$survey_agency)] <- "-"
        midline$username[is.na(midline$username)] <- "-"
        backcheck <- as.data.frame(d_backcheck)
        colnames(backcheck) <-gsub(".*/","",colnames(backcheck))
        return(prepareData(midline, backcheck))
    }
}

library(shiny)

ui <- fluidPage(
    
    titlePanel("LORA data collection monitoring dashboard"),
    
    sidebarLayout(
        sidebarPanel(
            actionButton("load_data", "Load data"),
            #selectInput("summary_by", "Choose focus of quality summary table", c("survey_agency.m", "username.m"), multiple = TRUE),
            pickerInput("summary_by", "Summary by (top table)", c("survey_agency.m", "username.m"), selected = c("survey_agency.m"), multiple = TRUE),
            pickerInput("filter_agency", "Filter agency partner",sort(unique(listData()$survey_agency.m), na.last=TRUE),selected = unique(listData()$survey_agency.m),options = list(`actions-box` = TRUE), multiple = T),
            pickerInput("filter_username", "Filter username",sort(unique(listData()$username.m), na.last=TRUE),selected=unique(listData()$username.m),options = list(`actions-box` = TRUE), multiple = T),
            h5("For bottom table:"),
            sliderInput("dontknow_threshold",
                        "Show when nomber of dont know is greater than... ",
                        min=0,
                        max=50,
                        value=0),
            #sliderInput("duration_threshold",
            #            "Show when interview durations is less than... ",
            #            min=0,
            #            max=999900,
            #            value=200),
            sliderInput("data_quality_threshold",
                        "Show when back-check percent match is less than... ",
                        min=0,
                        max=100,
                        value=90),
            downloadButton("downloadtable1", "Download top table"),
            downloadButton("downloadtable2", "Download bottom table")
        ),
        
        mainPanel(
            dataTableOutput("summary_table"),
            br(),br(),
            dataTableOutput("data")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    data <- reactiveValues()
    data$check <- data.frame(matrix(ncol=length(allvars), nrow = 0, dimnames=list(NULL, allvars)) )
    # observe({showModal(modalDialog(
    #   title = "start",
    #   paste(colnames(data$check)[1:5], collapse=","),
    #   easyClose = TRUE,
    #   footer = NULL
    # ))
    # })
    observeEvent(input$load_data, {
        showModal(modalDialog(
            textInput('login', 'Please enter login to access data'),
            textInput('password', 'Please enter password to access data'),
            footer=tagList(
                actionButton('submit', 'Submit'),
                modalButton('cancel')
            )
        ))
    })
    observeEvent(input$submit, {
        data$check <- get_data(isolate(input$login), isolate(input$password))
        removeModal()
        updatePickerInput(session, "filter_agency", choices = sort(unique((data$check)$survey_agency.m), na.last=TRUE),selected = unique((data$check)$survey_agency.m))
    })
    
    
    # update list of usernames
    updatedChoices = reactive({
        filtered_data <- isolate(data$check) %>%
            filter(survey_agency.m %in% input$filter_agency)
        enum_choices <- filtered_data %>%
            pull(username.m) %>% unique() %>% sort(na.last=TRUE)
        tmp<-list(enum_choices)
        return(tmp)
    })
    observe({
        updatePickerInput(session, "filter_username", choices = updatedChoices()[[1]], selected=updatedChoices()[[1]])
    })
    
    
    
    
    # Prepare top table (summary)
    summaryTable <- reactive({
        # showModal(modalDialog(
        #      title = "start",
        #      paste(colnames(data$check)[1:5], collapse=","),
        #      easyClose = TRUE,
        #      footer = NULL
        #    ))
        isolate(data$check) %>%
            filter(survey_agency.m%in%input$filter_agency,
                   username.m %in% input$filter_username)%>%
            group_by_at(vars(input$summary_by))%>%
            summarise(N=sum(!is.na(index.m), na.rm=T),
                      time_ok=mean(interviewDuringDay.m, na.rm=TRUE),
                      duration_ok = mean(reasonableDuration.m, na.rm=TRUE),
                      avg_dontknow = mean(nbDontknow.m),
                      N_backchecked=sum(!is.na(index.s), na.rm=T),
                      #prop_bc=sum(!is.na(index.s), na.rm=T)/sum(!is.na(index.m), na.rm=T),
                      avg_match_perc=mean(percentMatch, na.rm=T)/100,
                      min_match_perc=min(percentMatch, na.rm=T)/100,
                      #`25percMatch`=quantile(percentMatch, probs = .25, na.rm=T)/100
                      
            )
    })
    
    # Prepare bottom table
    filteredRawData <- reactive({
        data$check %>%
            
            # Apply filter
            #filter(!is.na(index.s))%>%
            filter(percentMatch<=input$data_quality_threshold | input$data_quality_threshold==100)%>%
            filter(nbDontknow.m>=input$dontknow_threshold)%>%
            #filter(reasonableDuration.m<=input$duration_threshold | is.na(input$duration_threshold))%>%
            
            filter(username.m %in% input$filter_username,
                   survey_agency.m%in%input$filter_agency)%>%
            .[,varshown]
    })
    
    
    # show the top table
    output$summary_table <- renderDataTable({
        datatable(summaryTable()) %>%
            formatPercentage(c("avg_match_perc", "min_match_perc", "duration_ok", "time_ok"), 0)%>%
            #formatRound(c("prop"), 3)%>%
            formatRound(c("avg_dontknow"), 1)
    })
    
    # show the bottom table
    output$data <- renderDataTable({
        datatable(filteredRawData()) %>%
            formatRound(c("percentMatch"), 2)%>%
            formatRound(c("interviewDuration.m"), 0)
    })
    
    # make the download top table button
    output$downloadtable1 <- downloadHandler(
        filename = function() "summaryTable.csv",
        content = function(file) {
            write.csv(summaryTable(), file, row.names = FALSE)
        }
    )
    
    # make the download bottom table button
    output$downloadtable2 <- downloadHandler(
        filename = function() "filteredRawData.csv",
        content = function(file) {
            write.csv(filteredRawData(), file, row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
