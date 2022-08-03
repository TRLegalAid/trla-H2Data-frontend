source("set-up.R")
source("variable-lists.R")

#--------Connect to Heroku--------

#pull database credentials from config.yml
dw <- config::get("db")

#create the connection to the database
con <- dbPool(RPostgres::Postgres(),
                dbname = 'dcjvt05vuja5li',
                 host = dw$server,
                 port = dw$port,
                 user = dw$uid,
                 password = dw$pwd)

#--------Create drop-down options for states --------

states <- dbGetQuery(con, 'SELECT DISTINCT UPPER("WORKSITE_STATE") AS "WORKSITE_STATE" FROM job_central;') %>%
  filter(WORKSITE_STATE != "Rhode Island")

states <- states$WORKSITE_STATE %>% 
  str_to_title(locale = "en") %>%
  sort(decreasing=FALSE) 


#--------Create list of case numbers for additional worksites and additional housing--------

#first place you'll see that we are merging job_central and low_accuracies

case_nums <- dbGetQuery(con, 'SELECT DISTINCT "CASE_NUMBER" 
FROM (SELECT "CASE_NUMBER" FROM job_central 
WHERE "Source" = \'DOL\' AND "Visa type" = \'H-2A\' AND ("ADDENDUM_B_WORKSITE_ATTACHED" = TRUE OR "ADDENDUM_B_HOUSING_ATTACHED" = TRUE) 
UNION ALL 
SELECT "CASE_NUMBER" 
FROM low_accuracies 
WHERE "Source" = \'DOL\' AND "Visa type" = \'H-2A\' AND ("ADDENDUM_B_WORKSITE_ATTACHED" = TRUE OR "ADDENDUM_B_HOUSING_ATTACHED" = TRUE))
"q01";')


#--------Create defaults for dates for the Search All Data section--------

earliest_posted <- dbGetQuery(con, 'SELECT MIN("RECEIVED_DATE") FROM (SELECT "RECEIVED_DATE" FROM job_central UNION ALL SELECT "RECEIVED_DATE" FROM low_accuracies) "q01";')
earliest_posted <- earliest_posted$min 

earliest_start <- dbGetQuery(con, 'SELECT MIN("EMPLOYMENT_BEGIN_DATE") FROM (SELECT "EMPLOYMENT_BEGIN_DATE" FROM job_central UNION ALL SELECT "EMPLOYMENT_BEGIN_DATE" FROM low_accuracies) "q01";')
earliest_start <- earliest_start$min

earliest_end <- dbGetQuery(con, 'SELECT MIN("EMPLOYMENT_END_DATE") FROM (SELECT "EMPLOYMENT_END_DATE" FROM job_central UNION ALL SELECT "EMPLOYMENT_END_DATE" FROM low_accuracies) "q01";')
earliest_end <- earliest_end$min

latest_posted <- dbGetQuery(con, 'SELECT MAX("RECEIVED_DATE") FROM (SELECT "RECEIVED_DATE" FROM job_central UNION ALL SELECT "RECEIVED_DATE" FROM low_accuracies) "q01";')
latest_posted <- latest_posted$max 

latest_start <- dbGetQuery(con, 'SELECT MAX("EMPLOYMENT_BEGIN_DATE") FROM (SELECT "EMPLOYMENT_BEGIN_DATE" FROM job_central UNION ALL SELECT "EMPLOYMENT_BEGIN_DATE" FROM low_accuracies) "q01";')
latest_start <- latest_start$max

latest_end <- dbGetQuery(con, 'SELECT MAX("EMPLOYMENT_END_DATE") FROM (SELECT "EMPLOYMENT_END_DATE" FROM job_central UNION ALL SELECT "EMPLOYMENT_END_DATE" FROM low_accuracies) "q01";')
latest_end <- latest_end$max

last_run <- dbGetQuery(con, 'SELECT MAX(CAST("Date of run" AS DATE)) FROM (SELECT "Date of run" FROM job_central UNION ALL SELECT "Date of run" FROM low_accuracies) "q01";') 
last_run <- format(last_run$max, "%m/%d/%Y")


#--------Create vector for selecting variable -- this section just reveals which variables are different between the two--------

all_variables <- dbListFields(con, "job_central")
all_variables_la <- dbListFields(con, "low_accuracies")
all_variables_minus_extra <- setdiff(all_variables, remove)
all_variables_minus_default <- setdiff(all_variables_minus_extra, default_columns)
diff <- setdiff(all_variables_la, all_variables)

#--------Prep job_central data --------

#Remove one subset of columns from job_central -- "remove" is generated in variable-lists

fwdata_clean2 <- tbl(con, "job_central") %>%
  select(-c(remove))

#Remove same subset of columns from low_accuracies; select only those that also belong in job_central

fwdata_la2 <- tbl(con, "low_accuracies") %>%
  select(-c(remove, all_of(diff))) %>%
  filter(table == "central")

#Merge job_central and low_accuracies so that each job is represented in table; turn job order links into hyperlinks; other formatting changes

fwdata <- union_all(fwdata_clean2, fwdata_la2) %>%
  mutate(WORKSITE_STATE = str_to_title(WORKSITE_STATE),
         
         #status variable = started/ended/not yet started
         status = str_to_title(status)) %>%
  
  #turn all dates into dates
  mutate_at(vars(RECEIVED_DATE, EMPLOYMENT_BEGIN_DATE, EMPLOYMENT_END_DATE, `Date of run`), as.Date) %>%
  
  #create hyperlinks with html -- makes them NEW variables in addition to Job Order Link and Job Summary Link
  mutate(`Job Order Link2` = paste0("<a href='",`Job Order Link`,"' target = \"\ _blank \"\ '>",`Job Order Link`, "</a>"),
         `Job Summary Link2` = paste0("<a href='",`Job Summary Link` ,"' target = \"\ _blank \"\ >",`Job Summary Link` ,"</a>"),
         
         #Other formatting changes
         WORKSITE_COUNTY = str_to_upper(WORKSITE_COUNTY),
         HOUSING_COUNTY = str_to_upper(HOUSING_COUNTY),
         WORKSITE_COUNTY = str_replace(WORKSITE_COUNTY, " PARISH", ""),
         WORKSITE_COUNTY = str_replace(WORKSITE_COUNTY, " COUNTY", "")) %>%
  
  #put the case number in the front
  relocate(CASE_NUMBER, .before = `790A_ADDENDUM_B_ATTACHED`) 


#--------Prep additional housing and additional worksites --------

add_housing <- tbl(con, "additional_housing")
add_worksites <- tbl(con, "additional_worksites")

#need to bring in the low_accuracies data, because some additional housing ends up in low_accuracies as well.
#this data can be identified by " table == 'dol_h' "

fwdata_la1 <- tbl(con, "low_accuracies") %>%
  filter(table == "dol_h") %>%
  select(c(all_of(housing_columns))) 

add_housing_incl_la <- union_all(add_housing, fwdata_la1)

#--------clean up GE--------

rm(all_variables,all_variables_la, all_variables_minus_extra,diff,remove)

onStop(function(){
  print("DB Closed")
  poolClose(con)})
