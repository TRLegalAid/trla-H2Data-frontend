#--------Install Dependencies--------

#for connecting to database
library(DBI)
library(RPostgres)

#for using dplyr verbs with database
library(dbplyr)

#for managing the database connections
library(pool)

library(tidyverse)
library(dplyr)
library(DT)
library(lubridate)
library(leaflet)
library(rsconnect)
library(shinydashboard)
library(stringr)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(collapsibleTree)
library(config)
library(tigris)
library(readxl)



#This is all for the H-2A worker map portion of the tool

STATES_matching <- read_excel("States_FIPS.xlsx")

counties <- counties(cb = FALSE, class = "sf", options(tigris_use_cache = TRUE)) %>%
  mutate(NAME = str_to_upper(NAME))

months <- tibble(month = c("January","February","March","April", "May", "June", "July", "August", 
                           "September", "October", "November","December"),
                 number = c(1:12))

#Function to determine whether a job is active in a given month

append_month <- function(month_name, year = "2021", df){
  
  get_month <- months$number
  names(get_month) <- months$month
  
  month_num <- get_month[month_name]
  
  mid_date <- paste0(year,"-", ifelse(month_num < 10, paste0(0,month_num), month_num), "-15")
  begin_date <- floor_date(ymd(mid_date), 'month')
  end_date <- ceiling_date(ymd(mid_date), 'month') %m-% days(1)
  
  mutate(df,
         
         month_Active = ifelse(month(EMPLOYMENT_BEGIN_DATE) == month_num & year(EMPLOYMENT_BEGIN_DATE) == year | 
                                 month(EMPLOYMENT_END_DATE) == month_num & year(EMPLOYMENT_END_DATE) == year |
                                 (EMPLOYMENT_BEGIN_DATE <= as.Date(begin_date) & EMPLOYMENT_END_DATE >= as.Date(end_date)),
                               TOTAL_WORKERS_NEEDED,0),
         
         month_Number = ifelse(month(EMPLOYMENT_BEGIN_DATE) == month_num & year(EMPLOYMENT_BEGIN_DATE) == year | 
                                 month(EMPLOYMENT_END_DATE) == month_num & year(EMPLOYMENT_END_DATE) == year |
                                 (EMPLOYMENT_BEGIN_DATE <= as.Date(begin_date) & EMPLOYMENT_END_DATE >= as.Date(end_date)),
                               1,0)
  )
}

