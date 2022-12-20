library(shiny)

#Whatever order things are written here, that is how they will appear in the app

shinyUI(fluidPage(
  
  #This is to include Google analytics connected to gethelptrla@gmail.com
  
  tags$head(includeHTML("analytics.html")),
  
#--------Application Title--------

  titlePanel(h2("H-2A and H-2B Job Posting Data"), windowTitle="H-2A and H-2B Job Postings"),

#--------Create application with two tabs-------
fluidPage(theme = shinytheme("flatly"),
              useShinyjs(),
              useShinydashboard(),
         
navbarPage("H2Data",
           
           #this ID is set for navigation purposes! 
           
            id = "navbar",
           

    #--------Tab 1: Home--------      
    
    tabPanel("Home", 
             value = "home",
             fluid = TRUE,
             
             #Create header panel for home page
             
             div(fluidRow(column(12, 
                                 
                                 #Note where R-version of html is used
                                 # 
                                 # wellPanel(
                                 #   
                                 #   tags$strong("January 5th, 2022: Due to recent changes to the DOL data portal, the data in this tool is incomplete for jobs posted after November 18th, 2021."),
                                 #   "We are working to update the data in this tool as soon as possible and will post updates to this page. 
                                 #           Complete data for jobs posted since November 18th, 2021 can still be accessed and downloaded from",
                                 #   
                                 #   tags$a(href = "https://spotlight.tcbmi.com/",
                                 #          "https://spotlight.tcbmi.com/.", target = "_blank"),
                                 #   
                                 #   
                                 #   align = "center"
                                 # ),
                                 
                                 wellPanel(h2("This is a tool designed for farmworker advocates .", align = "center"),
                                           br(),
                                           
                                           h4("We're making it easier to gather information about 
                                           jobs posted on the U.S. Department of Labor's (DOL) 
                                           Seasonal Jobs website.",
                                              
                                              br(),
                                              br(),
                                              
                                              "This tool integrates daily job postings 
                                              with DOL's Quarterly Disclosure Data releases."),

                                         
                                           align = "center")
             )
             ),
            
            #Source document with each collapsed panel -- easier to manage text this way
            #The local=T and [1] were necessary; can't entirely remember why
            
            source("home_text.R", local=T)[1]
            
             ), 
            
            #This is where css comes into play
            
            style = "max-width:750px; margin-left:auto; margin-right:auto"),
             
    
    #--------Tab 2: H-2A Worker Map by Month and County --------
    
    tabPanel("H-2A Worker Map",
             sidebarLayout(
               div(sidebarPanel(
                 
                 # add an option to toggle housing or worksite for the county assignment
                 
                 selectInput("toggle",
                             "Housing County or Worksite County",
                             choices = c("Worksite", "Housing"))
                 ,
                 selectInput("month",
                             "Select Month",
                             choices = c(months$month))
                 ,
                 selectInput("year",
                             "Select Year",
                             choices = c("2021","2022", "2023"))
                 ,
                 selectInput("state",
                             "Select State",
                             choices = c(STATES_matching$State))
                 
               ), style = "max-width:750px"),
               
               # Show the map
               mainPanel(
                 
                 #Map output
                 leafletOutput("map", width = "100%", height = "50vh"),
                 
                 br(),
                 
                 #DataTable output
                 DT::dataTableOutput("map_table")
                 
               )
             )),
    
    
    #--------Tab 3a: Choose Data--------
    
    navbarMenu("Search Clearance Orders",
    
    tabPanel("Search All Data", 
             value = "filter",
             fluid = TRUE,
             
      br(),
      
        div(wellPanel(
             
        #Select Worksite State
        selectInput(
          
                  #title of selectInput for later reference
          
                  "Worksite_ST",
          
                  #label on the selectInput
                  
                   "*Worksite State(s):",
                   
                   #Select state(s) = '' means that the text you see when nothing is selected will be "Select state(s)"
                   #states is the vector we created in global.R
                   #All allows us to select all states (see server.R)
                   
                   choices = c("Select state(s)" = '', "All", states),
                   multiple = TRUE),

        #Select Visa Type
        selectInput("Visa",
                    "Visa type(s):",
                    choices = c("All", "H-2A", "H-2B"),
                    multiple = FALSE,
                    
                    #you can set what is selected by default
                    selected = "All"),
        
        #Select Fifty Percent
        selectInput("fifty",
                    "50% mark:",
                    choices = c("Filter based on job status" = '',
                                "Reached 50% mark", "Has not yet reached 50% mark"),
                    multiple = FALSE,
                    selected = "All"),
        
        #Select Status
        selectInput("Status", 
                    "Job start status:",
                    choices = c("Select status(es)" = '', "Started", "Ended", "Not Yet Started"),
                    multiple = TRUE,
                    selected = c("Started","Ended","Not Yet Started")),
        
        
        
        #Select desired variables
        
        ##Create spacing for the treepop button
        ##Treepop (tree diagram) just allows users to visualize all variable options
        ##All of the treepop stuff will happen on the server side
        
        tags$head(
          tags$style(
            HTML("#treepop {
         display: inline;
         margin: 6px 5px 6px 15px;
         }")
          )
        ),
        
        ##Select method for picking variables - either in thematic groups or individually
        
        radioGroupButtons("method", 
                          label = HTML("How would you like to add additional variables?",
                                       as.character(actionLink(inputId = "treepop", 
                                                               label = "", 
                                                               icon = icon("question-circle")))),
                          choices = c("Groups","Individually")),
        
        
        ##Output depending on what they chose (the rest of this UI is actually in the server logic)
        ##The work on this is happening on the server side
        ##Any time you see "uiOutput," that means something is being created on the server side
        ##and here, we are telling the app WHERE to put that thing
        
        uiOutput("method_choice"),

        #Select Dates
        
        dateRangeInput("date_posted",
                       "Job posted between:",
                       # start = today_last_year,
                       # end = today,
                       start = as.Date(earliest_posted),
                       end = as.Date(latest_posted),
                       width = "50%",
                       format = "mm/dd/yy"),
        
        dateRangeInput("date_start",
                       "Job starts between:",
                       start = as.Date(earliest_start),
                       end = as.Date(latest_start),
                       width = "50%",
                       format = "mm/dd/yy"),
        
        dateRangeInput("date_end",
                       "Job ends between:",
                       start = earliest_end,
                       end = latest_end,
                       width = "50%",
                       format = "mm/dd/yy"),
        
       # checkboxInput("quarterly", "Exclude Scraped Data", value = FALSE),
       # 
       # checkboxInput("scraped", "Exclude DOL Data", value = FALSE),

        
        br(),
        
       #Submit button
       
        actionButton("view","View Data", icon = icon("arrow-right"))
        
        ), style = "max-width:750px; margin-left:auto; margin-right:auto")
    ),
    
    
#--------Tab 3.b: Show Resulting Data Table--------
      
    #This option will be hidden until someone presses "View" on the Search Clearance Orders tab
      tabPanel(title ="Data Table",
               value = "tab",
               fluid = TRUE,
               div(id = "results",
                   actionButton("filter", "Change Data Selection", icon = icon("arrow-left")),
                   
                   downloadButton("download","Download table as CSV"),
                   
                   hr(),
                   
                   uiOutput("count"),
                   
                   uiOutput("last"),
                   
                   DT::dataTableOutput("Data"),
               ) 
      ),


#--------Tab 3.c: Search by H-2A Employer (allows user to search employer name in both basic data and add'l employer addendum data)--------

  tabPanel("Search by Employer", 
           value = "emp",
           fluid = TRUE,
           div(wellPanel(
             h3("Search by Employer:"),
             selectInput("emp_states",
                         label = "Filter by worksite state (optional):",
                         choices = c("Select state(s)" = '', states),
                         multiple = TRUE),
             searchInput("emp",
                                 label = "Employer name:",
                                 placeholder = "Search partial or full employer name",
                                 btnSearch = icon("search"),
                                 btnReset = icon("remove"))

                         
           ),
           
           shinyjs::hidden(wellPanel(id = "basic_emp",
                                     
             h3("Basic Information"),
             
                     tags$i("Source: H-2A_Disclosure_Data"),
             
             br(),
             br(),
             
             selectInput("columns3",
                         "Select additional variables to display in data table:",
                         choices=list(
                           "Select variable(s)" = '', "All",
                           "Housing" = c(col_housing),
                           "Worksite" = c(col_worksite),
                           "Housing Compliance" = c(col_housing_compliance),
                           "Employer" = c(col_employer),
                           "Employer POC" = c(col_employer_poc),
                           "Attorney/Agent" = c(col_att_agent),
                           "Job Information" = c(col_job_info),
                           "Job Requirements" = c(col_job_req),
                           "Pay Information" = c(col_pay),
                           "Meals" = c(col_meals),
                           "Preparer" = c(col_preparer),
                           "Entity" = c(col_entity),
                           "Codes" = c(col_codes),
                           "Application" = c(col_apply),
                           "Other Filing Information" = c(col_other_filing_info),
                           "Addendums and Appendices" = c(col_add_app),
                           "Geographical Information" = c(col_geo),
                           "All other" = c(col_others)),
                         multiple = TRUE),
             
             actionButton("add_emp","Add", style='padding:1 px 4 px 4px 4px; font-size:90%; float: right;'),
                     
                      br(),
                      br(),
                     
                     DT::dataTableOutput("primary_emp"),
             
                     downloadButton("download4","Download table")
           )),
           
           shinyjs::hidden(wellPanel(id = "add_ws_emp",
             h3("Additional Worksites"),
                     tags$i("Source: H-2A_AddendumB_Employment"),
                     DT::dataTableOutput("add_worksites_emp"),
                     downloadButton("download5","Download table")
             
           ))), style = "max-width:750px; margin-left:auto; margin-right:auto"),

#--------Tab 3d: Search Additional Housing and Employers --------

tabPanel("Search Additional H-2A Housing and Employers", 
         value = "cn",
         fluid = TRUE,
         div(wellPanel(selectInput("case_num",
                                   h3("Select a case number:"),
                                   choices = c("Type or select case number" = '', case_nums),
                                   multiple = FALSE
         ),
         
         tags$i("Note: Case numbers are in this list only if they have additional housing or worksites attached to their application."),
         
         ),
         
         shinyjs::hidden(wellPanel(id = "basic_case_num",
                                   
                                   h3("Basic Information"),
                                   tags$i("Source: H-2A_Disclosure_Data"),
                                   br(),
                                   br(),
                                   
                                   selectInput("columns2",
                                               "Select additional variables to display in data table:",
                                               choices=list(
                                                 "Select variable(s)" = '', "All",
                                                 "Housing" = col_housing,
                                                 "Worksite" = c(col_worksite),
                                                 "Housing Compliance" = c(col_housing_compliance),
                                                 "Employer" = c(col_employer),
                                                 "Employer POC" = c(col_employer_poc),
                                                 "Attorney/Agent" = c(col_att_agent),
                                                 "Job Information" = c(col_job_info),
                                                 "Job Requirements" = c(col_job_req),
                                                 "Pay Information" = c(col_pay),
                                                 "Meals" = c(col_meals),
                                                 "Preparer" = c(col_preparer),
                                                 "Entity" = c(col_entity),
                                                 "Codes" = c(col_codes),
                                                 "Application" = c(col_apply),
                                                 "Other Filing Information" = c(col_other_filing_info),
                                                 "Addendums and Appendices" = c(col_add_app),
                                                 "Geographical Information" = c(col_geo),
                                                 "All other" = c(col_others)),
                                               multiple = TRUE),
                                   
                                   DT::dataTableOutput("primary"),
                                   downloadButton("download1","Download table")
                                   
         )),
         
         shinyjs::hidden(wellPanel(id = "add_hous_cn",
                                   h3("Additional Housing"),
                                   tags$i("Source: H-2A_AddendumB_Housing"),
                                   DT::dataTableOutput("add_housing"),
                                   downloadButton("download2","Download table")
         )),
         
         shinyjs::hidden(wellPanel(id = "add_ws_cn",
                                   h3("Additional Worksites"),
                                   tags$i("Source: H-2A_AddendumB_Employment"),
                                   DT::dataTableOutput("add_worksites"),
                                   downloadButton("download3","Download table")
                                   
         )), 
         
         style = "max-width:750px; margin-left:auto; margin-right:auto"),
         
         
         actionButton("cn_to_basic", "Back to Search Results") )
    ) 
)
)
)
)

