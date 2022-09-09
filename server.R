library(shiny)

#Order of things often does not matter unless one thing depends on another thing happening first


shinyServer(function(input, output, session) {
  
  
  
###--------Action buttons on home page--------###

  #Take user to "Basic" search
  
  #When button with value 'toBasic' is clicked, 
  #Jump to panel with value 'filter' under the 'navbar'
  #"session" is just required
  #observeEvent tells the app to wait until the event has happened (in this case, someone clicks the 'toBasic' button)
  
  observeEvent(input$toBasic, {
    updateTabsetPanel(session, "navbar", "filter")
  })
  
  #Take user to Case Number search
  
  observeEvent(input$toAdv1, {
    updateTabsetPanel(session, "navbar", "cn")
  })
  
  #Take user to Employer Search
  
  observeEvent(input$toAdv2, {
    updateTabsetPanel(session, "navbar", "emp")
  })
  
  
  
###--------Deal with filters on the Search All Data tab--------###
  
  ##--------Filter for worksite state--------##
  
  filtered_worksite_state <- reactive({
    
    #Validate to ensure a state is chosen
    validate(
      need(input$Worksite_ST != "", "Please select a worksite state.")
    )
    
    if(input$Worksite_ST %in% "All"){
      
      fwdata
      
    } 
    else {
      
      fwdata  %>%
        
        # %in% allows for us to filter for things in a whole vector of options, rather than just equal to one thing
        # for example, WORKSITE_STATE is IN this list: c(Pennsylvania, Texas, Louisiana, Alabama)
        # this allows for people to select multiple states
        
        filter(WORKSITE_STATE %in% local(input$Worksite_ST))
    }
  })
  
  
  ##--------Visa filter--------##
  
  filtered_visa <- reactive({
    
    if(input$Visa == "All"){
      
      filtered_worksite_state()
      
    }
    
    else {
      
      filtered_worksite_state() %>%
        
        #Can only pick one option, so no "%in%" required
        
        filter(`Visa type` == local(input$Visa))
      
    }
    
  })
  
  ##--------Halfway complete filter--------##
  
  filtered_fifty <- reactive({
    
    if(input$fifty==''){
      filtered_visa()
    }
    
    else if(input$fifty == "Reached 50% mark"){
      filtered_visa() %>%
        filter(is_halfway == TRUE)
    }
    
    else if(input$fifty == "Has not yet reached 50% mark"){
      filtered_visa() %>%
        filter(is_halfway == FALSE)
    }
  })
  
  
  ##--------Status filter--------##
  
  filtered_status <- reactive({
    
    if(is.null(input$Status)){
      filtered_fifty()
    }
    
    else{
      filtered_fifty() %>%
        filter(status %in% local(input$Status))
    }
  })
  
  
  ##--------Variable selection--------##

  #Variable selection method selection
  #Usually, this is something you'd do on the UI side
  #Because we have created options, we have to do it on the server side
  
  output$method_choice <- renderUI({
    
 if(input$method == "Individually") {
   
      selectInput("columns",
                  "Select additional individual variables to display in data table:",
                  
                  #all of this here is just creating labels to group the individual variables
                  #We created these groups in variable-lists.R
                  
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
                    "Hours" = c(col_hours),
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
                  
                  multiple = TRUE)
   
      } else if(input$method == "Groups"){
        
        selectInput("groups",
                    "Select additional groups of variables to display in data table:",
                    
                    choices = c("Select variable group(s)" = '', c(unique(Additional$name))),
                    
                    multiple = TRUE)
      }
    
    })

  
  #Filter based on method
  
  filtered_variables <-reactive({
    
    #For individual selection
    
    if(input$method == "Individually"){
      
      updateSelectInput(session, "groups", selected = NULL)
      
      
      #If they didn't add any variables, we just serve up the default columns we listed in variable-lists.R
      if(is.null(input$columns)){
        filtered_status() %>%
          select(c(all_of(default_columns)))
        
      #The data up to this point still has all columns. IF they want all variables, the abstract data table already has all the variables.   
      } else if (input$columns %in% "All"){
        filtered_status()
      
      #Finally, this option selects all the columns that the person selected under the 'Individually' method    
      } else {
        filtered_status() %>%
          select(c(all_of(default_columns), input$columns))
      }
      
      
    #For group selection  
      
    } else if (input$method == "Groups"){
      
      updateSelectInput(session, "columns",selected = NULL)
      
      if(is.null(input$groups)){
        filtered_status() %>%
          select(c(all_of(default_columns)))
      } 
      
      else {
        
        #Create a vector of variables based on selected groups
        
        additional_select <- Additional %>%
          filter(name %in% c(all_of(input$groups))) %>%
          select(vars)
        additional_select <- additional_select$vars
        
        filtered_status() %>%
          select(c(all_of(default_columns), all_of(additional_select)))
      }
    }
    
  })
  
  #Pop-up for tree diagram
  
  observeEvent(input$treepop, {
    
    #Show the dialogue box that has the tree in it when the little question mark is clicked
    
    showModal(modalDialog(
      title = "Variable Reference Tree",
      collapsibleTreeOutput("tree", width = "100%", height = "400px"),
      easyClose = TRUE,
      footer = modalButton("Return")))

  })
  
  addTooltip(session, id = "treepop", title = "Click to view clickable reference for all 
  variable options and their groups.",
             placement = "right", trigger = "hover")
  
  #This is referenced above in showModal ^
  output$tree <- renderCollapsibleTree(collapsibleTree(Additional,
                                                       hierarchy = c("name","vars"),
                                                       width = 500))
  
  ##--------Select all dates--------##
  
  filtered_posted <-reactive({
    filtered_variables() %>%
      filter(RECEIVED_DATE >= local(input$date_posted[1]) & RECEIVED_DATE <= local(input$date_posted[2]))
  })
  
  filtered_start <-reactive({
    filtered_posted() %>%
      filter(EMPLOYMENT_BEGIN_DATE >= local(input$date_start[1]) & EMPLOYMENT_BEGIN_DATE <= local(input$date_start[2]))
  })
  
  filtered_end <-reactive({
    filtered_start() %>%
      filter(EMPLOYMENT_END_DATE >= local(input$date_end[1]) & EMPLOYMENT_END_DATE <= local(input$date_end[2]))
  })
  
  ##--------DOL Data Only--------##
  
  filtered_check <- reactive({
    
    if(local(input$quarterly) == TRUE){
    filtered_end() %>%
        filter(Source == "DOL")
    }
    
    else{
      filtered_end()
    }
  
  })
  
  ##--------Scraped Data Only--------##
  
  filtered_check2 <- reactive({
    if(local(input$scraped) == TRUE){
      filtered_check() %>%
        filter(Source == "Apify")
    }
    else{
      filtered_check()
    }
  })
  

  ###--------Execute when 'view' button is pressed--------###
  
  #eventReactive is in the same family of functions as reactive and observeEvent
  
  fully_filtered <- eventReactive(input$view, {
    
    filtered_check2() %>%
      
      #This pulls the abstract data down into an actual dataframe
      collect() %>%
    
      #Convert to factor for easy filtering in data table
      mutate(across(where(is.character) & (starts_with("WORKSITE_COUNTY") | starts_with("HOUSING_COUNTY")), as.factor))
    
    })
  

  ###--------Make the "Data Table" dropdown in the navbar available when 'view' button is pressed--------###
  
  #this option is the default
  observe({
    hide(selector = "#navbar li a[data-value=tab]")
  })
  
  #this option happens when 'view' button is clicked
  observeEvent(input$view, {
    show(selector = "#navbar li a[data-value=tab]")
  })


  ###--------Jump to table page when 'view' button is pressed--------###
  
  observeEvent(input$view, {
    updateTabsetPanel(session, "navbar", "tab")
  })
  
  
  ###--------Jump to filter page when 'filter' button is pressed--------###
  
  observeEvent(input$filter, {
    updateTabsetPanel(session, "navbar", "filter")
  })
  
  
  ###--------Render the data table--------###
  
  output$Data <- DT::renderDataTable({
    DT::datatable(data = fully_filtered(), 
                  
                  #renaming the columns so it looks nicer
                  colnames = c("Worksite State" = "WORKSITE_STATE",
                               "Case Number" = "CASE_NUMBER",
                               "Employer Name" = "EMPLOYER_NAME",
                               "Job Title" = "JOB_TITLE",
                               "Date Received by DOL" = "RECEIVED_DATE",
                               "Employment Begin Date" = "EMPLOYMENT_BEGIN_DATE",
                               "Employment End Date" = "EMPLOYMENT_END_DATE",
                               "Started?" = "status",
                               "Job Order Link" = "Job Order Link2",
                               "Job Summary Link" = "Job Summary Link2",
                               "Workers Needed"="TOTAL_WORKERS_NEEDED",
                               "Workers Certified" = "TOTAL_WORKERS_H-2A_CERTIFIED",
                               "Days until 50% Point" = "days_until_halfway",
                               "50% Point Reached" = "is_halfway"),
                  
                selection = 'none', 
                
                filter = list(position = 'bottom', clear = FALSE),
                
                options = list (pageLength=10, scrollX=TRUE, search = list(regex = TRUE, caseInsensitive = TRUE),
                                
                                #This hides some columns -- ADDENDUM indicators (we NEED them in the table so we can them to turn some cells pink)
                                #Also hides the version of the job order links that is NOT hyperlinked
                          
                                columnDefs = list(list(targets = c(11,12,14,15), visible = FALSE))
                      
                                ),
                
                rownames = FALSE,
                class = 'display',
                
                #Allows us to view the html hyperlinks AS hyperlinks
                escape = c(-9, -10)
                
                ) %>%
      
      formatDate(c("Date Received by DOL","Employment Begin Date","Employment End Date"), method = 'toLocaleDateString') %>%
      
      formatStyle(1, cursor = 'pointer') %>%
      
      formatStyle(1, c("ADDENDUM_B_HOUSING_ATTACHED","ADDENDUM_B_WORKSITE_ATTACHED"),
                  backgroundColor = styleEqual(TRUE, 'pink')) %>%
      
      formatStyle('50% Point Reached', backgroundColor = styleEqual(c(1,0), c('lightgreen','lightblue')))
  })
  
  observeEvent(input$Data_cell_clicked, {
    
    info = input$Data_cell_clicked
    
    # do nothing if not clicked yet, or the clicked cell is not in the 1st column
    if (is.null(info$value) || info$col != 0) return()
    
    #if whatever is clicked is not in my list of case numbers with addendum info, show the 'nothing to see here' option
    if (!(info$value %in% case_nums$CASE_NUMBER)) return(showModal(modalDialog(
      title = "Nothing to see here!",
      "There are no additional housing or worksite records to display. Try selecting a case number highlighted in pink.",
      easyClose = TRUE,
      footer = NULL)))
    
    updateTabsetPanel(session, "navbar", selected = "cn")
    
    updateSelectInput(session, "case_num", selected = info$value)
    
  })
  
  
  ###--------Download Data--------###
  
  output$download <- downloadHandler(
    
    #Generate the file name
    filename = function() {
      paste0(Sys.Date()," - H2Data_Search - ", input$Worksite_ST[1], ".csv")
    },
    
    #ensure that any filters applied on the actual datatable are reflected in what is downloaded
    content = function(con) {
      write.csv(fully_filtered()[input[["Data_rows_all"]], ] %>%
                  select(-c("Job Order Link2", "Job Summary Link2"))
                , con)
    })
  
  
  ###--------Render the count--------###
  
  output$count <-renderUI({
    infoBox("Number of Results", nrow(fully_filtered()[input[["Data_rows_all"]], ]),
            fill = FALSE, color = "light-blue", width = 6, icon = icon("list"))
  })
  
  output$last <-renderUI({
    infoBox("last updated:", last_run, fill = FALSE, color = "light-blue",
            width = 6, icon = icon("calendar"))
  })
  
  
  ###--------Render Datatables for Case Number Search--------###
  
  #Keep results empty until the user has actually searched
  
  observeEvent(input$case_num, {
    shinyjs::showElement(id = "basic_case_num")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$case_num, {
    shinyjs::showElement(id = "add_hous_cn")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$case_num, {
    shinyjs::showElement(id = "add_ws_cn")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  #Allow user to add additional variables
  
  filtered_variables2 <-reactive({
    if(is.null(input$columns2)){
      fwdata %>%
        select(c(all_of(default_columns)))
    }
    else if(input$columns2 %in% "All"){
      fwdata
    }
    else {
      fwdata %>%
        select(c(all_of(default_columns), input$columns2))
    }
  })
  
  output$primary <- DT::renderDataTable({
    DT::datatable(data = 
                    filtered_variables2() %>%
                    filter(CASE_NUMBER == local(input$case_num))%>%
                    collect(),
    colnames = c("Worksite State" = "WORKSITE_STATE",
                 "Case Number" = "CASE_NUMBER",
                 "Employer Name" = "EMPLOYER_NAME",
                 "Job Title" = "JOB_TITLE",
                 "Date Received by DOL" = "RECEIVED_DATE",
                 "Employment Begin Date" = "EMPLOYMENT_BEGIN_DATE",
                 "Employment End Date" = "EMPLOYMENT_END_DATE",
                 "Started?" = "status",
                 "Job Order Link" = "Job Order Link2",
                 "Job Summary Link" = "Job Summary Link2"),
    options = list (dom='tpi', scrollX=TRUE, columnDefs = list(list(targets = c(12,13,15,16), visible = FALSE))),
    escape = c(-10, -11)
    )
  })
  
  output$add_housing <- DT::renderDataTable({
    DT::datatable(data = 
                    add_housing_incl_la %>%
                    filter(CASE_NUMBER == local(input$case_num))%>%
                    collect(),
                  options = list(dom='tpi', scrollX=TRUE)
    )
  })
  
  
  output$add_worksites <- DT::renderDataTable({
    DT::datatable(data = 
                    add_worksites %>%
                    filter(CASE_NUMBER == local(input$case_num))%>%
                    collect(),
                  options = list(dom='tpi', scrollX=TRUE)
    )
  })
  
  ##--------Action buttons for Case Num page -------##
  
  
  observe({
    hide("cn_to_basic")
  })
  observeEvent(input$view, {
    show("cn_to_basic")
  })
  
  observeEvent(input$cn_to_basic, {
    updateTabsetPanel(session, "navbar", "tab")
  
  })
  
  #Downloads for Case Number Search page
  
  output$download1 <-
    downloadHandler(
      filename = function() {
        paste0(input$case_num, "_basic.csv")
      },
      content = function(con) {
        write.csv(filtered_variables2() %>%
                    filter(CASE_NUMBER == local(input$case_num))%>%
                    collect()
                  , con)
      })
  
  output$download2 <-
    downloadHandler(
      filename = function() {
        paste0(input$case_num, "_additional_housing.csv")
      },
      content = function(con) {
        write.csv(add_housing_incl_la %>%
                    filter(CASE_NUMBER == local(input$case_num))%>%
                    collect()
                  , con)
      })
  
  output$download3 <-
    downloadHandler(
      filename = function() {
        paste0(input$case_num, "_additional_worksites.csv")
      },
      content = function(con) {
        write.csv(add_worksites %>%
                    filter(CASE_NUMBER == local(input$case_num))%>%
                    select(1:9) %>%
                    collect()
                  , con)
      })
  
  ###--------Render Datatables for Employer Search--------###
  
  #Setup so that if additional variables are selected, the table will update:

  primary_emp_data1 <- reactive({
    if(is.null(input$columns3)){
      fwdata %>%
        select(c(all_of(default_columns))) %>%
        filter(str_detect(tolower(EMPLOYER_NAME), tolower(!!input$emp)))
      
    } else if(input$columns3 %in% "All"){
      fwdata %>%
        filter(str_detect(tolower(EMPLOYER_NAME), tolower(!!input$emp)))
    
    } else {
      fwdata %>%
        select(c(all_of(default_columns), input$columns3)) %>%
        filter(str_detect(tolower(EMPLOYER_NAME), tolower(!!input$emp)))
    }
  })
  
  primary_emp_data2 <- reactive({
    if(is.null(input$emp_states)){
      primary_emp_data1()
    } else {
      primary_emp_data1() %>%
        filter(WORKSITE_STATE %in% local(input$emp_states))
    }
  })

  fully_filtered_emp <- eventReactive(c(input$emp_search, input$add_emp), {
    primary_emp_data2() %>%
      collect()
  })
  


  
  add_worksites_data1 <- reactive({
    add_worksites %>%
      filter(str_detect(tolower(NAME_OF_AGRICULTURAL_BUSINESS), tolower(!!input$emp)))
  })
  
  add_worksites_data2 <- reactive({
  if(is.null(input$emp_states)){
    add_worksites_data1()
  } else {
    add_worksites_data1() %>%
      filter(str_to_title(WORKSITE_STATE) %in% local(input$emp_states))
  }
  })
  
  fully_filtered_add_worksites <- eventReactive(c(input$emp_search, input$add_emp), {
    add_worksites_data2() %>%
      collect()
  })
  
  
  #Show tables when emp_search is pressed
  
  observeEvent(input$emp_search, {
    shinyjs::showElement(id = "basic_emp")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$emp_search, {
    shinyjs::showElement(id = "add_ws_emp")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  #Render employment data tables
  
  output$primary_emp <- DT::renderDataTable({
    DT::datatable(data = fully_filtered_emp(),
                  colnames = c("Worksite State" = "WORKSITE_STATE",
                               "Case Number" = "CASE_NUMBER",
                               "Employer Name" = "EMPLOYER_NAME",
                               "Job Title" = "JOB_TITLE",
                               "Date Received by DOL" = "RECEIVED_DATE",
                               "Employment Begin Date" = "EMPLOYMENT_BEGIN_DATE",
                               "Employment End Date" = "EMPLOYMENT_END_DATE",
                               "Started?" = "status",
                               "Job Order Link" = "Job Order Link2",
                               "Job Summary Link" = "Job Summary Link2"),
                  options = list (dom='tpi', scrollX=TRUE, columnDefs = list(list(targets = c(12,13,15,16), visible = FALSE))),
                  escape = c(-10, -11)
    )
  })
  
  
  output$add_worksites_emp <- DT::renderDataTable({
    DT::datatable(data = 
                    fully_filtered_add_worksites(),
                  options = list(dom='tpi', scrollX=TRUE)
    )
  })
  
  
  #Download functions for Employer Search page
  
  output$download4 <-
    downloadHandler(
      filename = function() {
        paste0(input$emp, "_basic.csv")
      },
      content = function(con) {
        write.csv(fully_filtered_emp()
                  , con)
      })
  
  output$download5 <-
    downloadHandler(
      filename = function() {
        paste0(input$emp, "_additional_worksites.csv")
      },
      content = function(con) {
        write.csv(fully_filtered_add_worksites()
                  , con)
      })
  
  

  ###--------Render Maps for Tab 2--------###

  
  #Data for table that is shown on screen, summary numbers
  
    fw_state1 <- reactive({
      
      if (local(input$toggle == "Worksite")) {
      
      
      fwdata %>%
        filter(WORKSITE_STATE == local(input$state) & `Visa type` == "H-2A") %>%
        collect() %>%
        left_join(STATES_matching, by = c("WORKSITE_STATE" = "State")) %>%
        
        #This function is defined in set-up.R; it is an indicator for whether the job is active in the selected month
        append_month(month_name = local(input$month), year = local(input$year), df = .)
      
      
      } else {
        
      
      fwdata %>% 
        filter(HOUSING_STATE == local(input$state) & `Visa type` == "H-2A") %>%
        collect() %>%
        left_join(STATES_matching, by = c("HOUSING_STATE" = "State")) %>%
        
        append_month(month_name = local(input$month), year = local(input$year), df = .)
        
      }
      
    })
    
    
    
    fw_state <- reactive({
      
      if (local(input$toggle == "Worksite")) {
        
      
    fwdata_state_counts <- fw_state1() %>%
      group_by(WORKSITE_COUNTY, WORKSITE_STATE) %>%
      summarise(Month_sum = sum(month_Active),
                Month_count = sum(month_Number)) %>%
      mutate(WORKSITE_COUNTY = str_to_upper(WORKSITE_COUNTY)) %>%
      left_join(STATES_matching, by = c("WORKSITE_STATE" = "State")) %>%
      rename("County" = "WORKSITE_COUNTY", "State" = "WORKSITE_STATE", "Total Workers for Jobs Active This Month" = "Month_sum",
             "Total Jobs Active This Month" = "Month_count") %>%
      filter(`Total Jobs Active This Month` > 0) %>%
      arrange(desc(`Total Workers for Jobs Active This Month`)) %>%
      select(-c("Abb"))
    
    
      } else {
        
        
        
        fwdata_state_counts <- fw_state1() %>%
          group_by(HOUSING_COUNTY, HOUSING_STATE) %>%
          summarise(Month_sum = sum(month_Active),
                    Month_count = sum(month_Number)) %>%
          mutate(HOUSING_COUNTY = str_to_upper(HOUSING_COUNTY)) %>%
          left_join(STATES_matching, by = c("HOUSING_STATE" = "State")) %>%
          rename("County" = "HOUSING_COUNTY", "State" = "HOUSING_STATE", "Total Workers for Jobs Active This Month" = "Month_sum",
                 "Total Jobs Active This Month" = "Month_count") %>%
          filter(`Total Jobs Active This Month` > 0) %>%
          arrange(desc(`Total Workers for Jobs Active This Month`)) %>%
          select(-c("Abb"))
        
        
      }
    
    })
    
    
  #Render data table with summary numbers
  
  output$map_table <- DT::renderDataTable({
    DT::datatable(data = fw_state() %>% select(-c("FIPS")),
                  rownames = FALSE,
                  selection = 'none') %>%
       formatStyle(1, cursor = 'pointer') %>%
       formatStyle(1, fontWeight = 'bold', color = 'blue', textDecoration = 'underline')
  })
  
  
  #Data for table shown in modal dialogue -- contains detailed job order info
  
  job_orders_filt <- reactive({
    
    if (local(input$toggle == "Worksite")) {
      
    
    info = input$map_table_cell_clicked
    fw_state1() %>%
      filter(WORKSITE_COUNTY == info$value & month_Number > 0 & WORKSITE_STATE == local(input$state)) %>%
      select(c(all_of(default_columns),all_of(col_housing),all_of(col_worksite),"TOTAL_WORKERS_NEEDED"))
    
    } else {
      
      
      info = input$map_table_cell_clicked
      fw_state1() %>%
        filter(HOUSING_COUNTY == info$value & month_Number > 0 & HOUSING_STATE == local(input$state)) %>%
        select(c(all_of(default_columns),all_of(col_housing),all_of(col_worksite),"TOTAL_WORKERS_NEEDED"))
      
      
  } 
  
  })

  #Button so that job order data can be downloaded

  output$download_map_table2 <-
    downloadHandler(
      filename = function() {
        paste0(input$map_table_cell_clicked, "_basic.csv")
      },
      content = function(con) {
        write.csv(job_orders_filt()
                  ,con)
      })


  #Render data table with job order data

  output$map_table2 <- DT::renderDataTable({
    DT::datatable(data = job_orders_filt(),
                  options = list(scrollX=TRUE),
                  escape = FALSE) 
  })


  #Show dialogue when county name is clicked

  observeEvent(input$map_table_cell_clicked, {
    info = input$map_table_cell_clicked
    if (is.null(info$value)|| info$col != 0) return()
    showModal(
      modalDialog(DT::dataTableOutput("map_table2"),
                  downloadButton("download_map_table2"),
                  easyClose = TRUE)
    )
  })


  #Render map
  
  output$map <- renderLeaflet({


    data_geo_all <- counties %>%
      right_join(fw_state(), by = c("NAME" = "County", "STATEFP" = "FIPS"))

    bins <- c(0, 15, 25, 50, 100, 200,300,400,500, Inf)

    content <- paste("<p><strong>", data_geo_all$NAME, "</strong></p>",
                     data_geo_all$`Total Workers for Jobs Active This Month`, "H-2A workers in",
                     input$month) %>%
      lapply(htmltools::HTML)

    pal <-  colorBin("YlOrRd", domain = data_geo_all$`Total Workers for Jobs Active This Month`, bins=bins)

    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = data_geo_all,
                  fillColor = ~pal(`Total Workers for Jobs Active This Month`),
                  weight = 0,
                  fillOpacity = .9,
                  smoothFactor = .8,
                  label = content)  %>%
      addLegend(position = "topright",
                pal = pal,
                values = data_geo_all$`Total Workers for Jobs Active This Month`,
                title = "Number of Workers")
  }
  )
  
  
})



