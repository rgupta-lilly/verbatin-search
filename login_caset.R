###########################Header##################################
#Developer : Ravi Gupta (c260984)
#Version : 1 (22May2019)
#This program allow you to :
#1- SEARCH
#2- UPDATE (ADD ROWS)
#3- EMAIL
#Database (CSV) with interactive features
############################Header_End###############################

rm(list = ls())
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyTree)
library(shinyAce)
library(sendmailR)
library(shinyjs)
library(shinyLP)

my_username <- "c260984"
my_password <- "abc"
my_role <- "admin"

###########################/ui.R/##################################
header <- dashboardHeader(title = "CaSET")
sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
              sidebarMenuOutput("menu")
  )
)
body <- dashboardBody(#shinythemes::themeSelector(),
  theme = shinytheme("lumen"),
  tags$head(tags$style("dataInfo{color: red")),
  htmlOutput("page")
)

ui <- dashboardPage(header, sidebar, body)

ui1 <- function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("userName", "Username", value =Sys.getenv("USER")),
                  passwordInput("passwd", "Password", value ="abc"),
                  br(),
                  actionButton("Login", "Log in"),
                  verbatimTextOutput("dataInfo")
        )
    ),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui2 <- function(){
    #####Option -1 Sidepanel (SEARCH)######
    tabItems( 
      tabItem(tabName = "m1", 
              tabBox(
                title = "LCCI STATS",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "550px" , width="600px",
                ###INSTRUCTIONS PAGE#####
                tabPanel("Instructions", value="s2",
                         tags$div(style="background-color:LemonChiffon",
                                  tags$br(),
                                  tags$h4("Here are the Instructions"),
                                  tags$p("STEP1: Input your search query"),
                                  tags$p("STEP2: Select column you want to look into"),
                                  tags$p("STEP3: Press SUBMIT button"),
                                  tags$br(),tags$br(),tags$br(),tags$br(),
                                  tags$br(),tags$br(),tags$br(),tags$br(),
                                  tags$br(),tags$br(),tags$br(),tags$br(),
                                  tags$br()
                                  
                         )),
                ####TOOL PAGE########
                tabPanel("Search Tool", 
                         
                         shiny::fluidPage(tags$div(style="background-color:LemonChiffon",
                                                   sidebarLayout(
                                                     useShinyjs(),
                                                     tags$div(id="form",
                                                              titlePanel(""),
                                                              sidebarPanel(
                                                                
                                                                #sliderInput("n", "N:", min = 10, max = 1000, value = 200, step = 10),
                                                                textInput("text", "Text:", "text here"),
                                                                selectInput('var',"Variable:",c('Verbatim'='1','To be code'='2','All'='3')),
                                                                actionButton("submit","Submit"),
                                                                actionButton("reset_input", "Reset inputs")
                                                              ),
                                                              mainPanel(
                                                                #               plotOutput("plot1", width = 400, height = 300),
                                                                verbatimTextOutput("text"),
                                                                tableOutput('table'),
                                                                uiOutput("ui")
                                                              )
                                                     )
                                                     
                                                   )
                         )
                         )
                         ,
                         tags$br(), tags$br(), tags$br(), tags$br(),
                         actionButton('jumpToS2', 'Instruction-Page')
                ),
                
                #####EMAIL PAGE######
                tabPanel("Contact Admin",
                         pageWithSidebar(
                           
                           headerPanel("Email sender"),
                           
                           sidebarPanel(
                             textInput("from", "From:", value="from@lilly.com"),
                             #textInput("to", "To:", value="to@lilly.com"),
                             selectInput("to","Admin:",c('Admin-1'='gupta_ravi3@lilly.com')),
                             textInput("subject", "Subject:", value="VST Query: "),
                             actionButton("send", "Send mail")
                           ),
                           
                           mainPanel(    
                             aceEditor("message", 
                                       value="write message here:
      For Example: Could you please remove a row which mistakenly got added- 
      Verbatim : XXXX , To be coded : NNN,
      
      Thank you!")
                           )
                           
                         ))
              ) 
      ),
      
      #####Option -2 Sidepanel (DEVELOPER)######      
      tabItem(tabName = "m2", p('Hello') , a("click on me",target="_blank",
                                             href="https://nam.delve.office.com/?u=e96b6f41-f5c3-43de-b552-8e0dc488bb7f&v=work"))
    )
}


###########################/server.R/##################################

server = (function(input, output,session) {
  
  Logged <- FALSE
  Security <- TRUE
  
  USER <- reactiveValues(Logged = Logged)
  SEC <- reactiveValues(Security = Security)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(my_username == Username & my_password == Password) {
            USER$Logged <- TRUE
          } else {SEC$Security <- FALSE}
        } 
      }
    }    
  })
  
  observe({
    if (USER$Logged == FALSE) {output$page <- renderUI({ui1()})}
    if (USER$Logged == TRUE) {
            output$page <- renderUI({ui2()}) 
            ####MENU######  
            output$menu <- renderMenu({
              sidebarMenu(
                menuItem("Search", tabName="m1", icon = icon("home"), badgeColor="green", selected = T),
                menuItem("Developer", tabName="m2", icon = icon("database"),badgeColor="blue")
              )
            })
    }
    outputOptions(output, "page", suspendWhenHidden = FALSE, priority = 1)
  })
  
  observe({
    output$dataInfo <- renderText({
      if (SEC$Security) {""}
      else {"Your username or password is not correct"}
    })
  })
  

  
  ### TEXT INPUT ########
  output$text <- renderText({
    if (input$var=='1'){paste0("Searching for: '", input$text, "'in VERBATIM column")}
    else if (input$var=='2'){paste0("Searching for: '", input$text, "'in TO BE CODED column")}
    else {paste0("Searching for: '", input$text, "'in --- column")}
  })
  
  ###TABSET JUMP###  
  observeEvent(input$jumpToS2, {
    updateTabsetPanel(session, "tabset1",
                      selected = "s2")
  })
  
  isolate({updateTabItems(session, "tabs", "m1")})
  
  
  ####READING DATA ######
  
  countries<-read.csv("Dummy.csv",sep=',')   
  
  
  ####  INSERT ROW #########
  inserted <- c()
  
  observeEvent(input$insertBtn, {
    btn <- input$insertBtn
    id <- paste0('txt', btn)
    insertUI(
      selector = '#placeholder',
      ## wrap element in a div with id for ease of removal
      ui = fluidRow(
        
        tags$div(
          #tags$p(paste('Element number', btn)), 
          tags$br(), box(width = 12, title = "Add Row to the database",
                         splitLayout(
                           textInput("verbatim", "Verbatim Term", value  = input$text),
                           textInput("to_be_coded", "To be coded", placeholder = "Enter term to be coded"),
                           textInput("llt", "LLT", placeholder = "LLT"),
                           textInput("status", "Status", placeholder = "ACTIVE", value="ACTIVE"),
                           #dateInput("stdate", "Start Date",value=Sys.Date()),
                           #dateInput("eddate","End date", value=Sys.Date()),
                           tags$br(),
                           actionButton("addRow", "Save"))
          ),
          id = id
        )
        
      ))
    inserted <<- c(id, inserted)
  })
  
  #### Append ROW ###
  
  observeEvent(input$addRow,{
    
    x =paste0(trimws(as.character(input$verbatim)),',',
              trimws(as.character(input$to_be_coded)),',',
              trimws(as.character(input$llt)),',',
              trimws(as.character(input$status)))
    command=paste("echo ",x," >>Dummy.csv")
    system(command)
    showModal(
      modalDialog(
        title="Success!!", tags$p("You have added new row sucessfully. Press REFRESH and check. Thanks!"),
        easyClose=TRUE,
        footer = tagList(
          #modalButton("Cancel" ),
          actionButton("refresh", "Refresh")
        )
      )
    )
  })
  
  ####  UNDO ROW #########
  
  observeEvent(input$removeBtn, {
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted[length(inserted)])
    )
    inserted <<- inserted[-length(inserted)]
  })
  
  
  ### RENDER TABLE #####        
  output$table <- renderTable({
    if(input$submit > 0) {
      #filter(countries, countries[1]==input$text)
      if (input$var=='1' || input$var=='2' ){countries[which(countries[as.integer(input$var)]==input$text),]}
      else if(input$var =='3'){
        
        #progress bar
        progress <- Progress$new(session, min=1, max=15)
        on.exit(progress$close())
        
        progress$set(message = 'Searching CasSET',
                     detail = 'This may take a while...')
        
        for (i in 1:15) {
          progress$set(value = i/2)
          Sys.sleep(0.5)
        }
        
        #Intializing data frames
        x <- c()
        y <- c()
        for (i in 1:length(countries)) {
          y <- rbind(y,x)
          x <- countries[with(countries,grep(input$text,countries[,i],perl=TRUE,ignore.case=TRUE)),]
          
        }
        
        
        if (is.null(y)){ print("No data found")}
        else print(unique(y))
      }
    }
  })
  
  ### RESET EVENT########      
  observeEvent(input$reset_input, {
    reset("form")
  })
  
  
  ### Refresh APP####
  observeEvent(input$refresh, {
    session$reload()
  })
  
  ###  OUTPUT UI #####
  
  output$ui <- renderUI({
    if(nrow(countries[which(countries[as.integer(input$var)]==input$text),]) == 0 && input$var!='3' && my_role =="admin")
      return(
        conditionalPanel(
          condition="input.submit >0",
          actionButton('insertBtn', 'Insert'), 
          actionButton('removeBtn', 'Undo'), 
          tags$div(id = 'placeholder')
        )
      )
    tableOutput("table")
  })
  
  
  ######EMAIL #######  
  observeEvent(input$send, {
    if(is.null(input$send) || input$send==0) return(NULL)
    from <- isolate(input$from)
    to <- isolate(input$to)
    subject <- isolate(input$subject)
    msg <- isolate(input$message)
    sendmail(from, to, subject, msg)
    showModal(
      modalDialog(
        title="Email Sent!!", "You will hear response soon! Thanks!",
        easyClose=TRUE,
        footer = tagList(
          #modalButton("Cancel" ),
          actionButton("refresh", "Refresh")
        )
      )
    )
  })
})

runApp(list(ui = ui, server = server))
