
#"default_columns" are the columns that will always show up in the table 

default_columns <- c("CASE_NUMBER", "Visa type", "EMPLOYER_NAME", "JOB_TITLE", "WORKSITE_STATE", 
                     "RECEIVED_DATE","EMPLOYMENT_BEGIN_DATE", "EMPLOYMENT_END_DATE", 
                     "Job Order Link2", "Job Summary Link2", "status", 
                     "ADDENDUM_B_HOUSING_ATTACHED",
                     "ADDENDUM_B_WORKSITE_ATTACHED", "Source", "Job Order Link", "Job Summary Link",
                     "TOTAL_WORKERS_NEEDED",
                     "TOTAL_WORKERS_H-2A_CERTIFIED",
                     "is_halfway", "days_until_halfway")

remove<- c("1st_PWD_CASE_NUMBER", "2nd_PWD_CASE_NUMBER", "3rd_PWD_CASE_NUMBER",
           "EMERGENCY_FILING_PWD_ATTACHED","MSA_NAME_OES_AREA_TITLE")


housing_columns <- c("CASE_NUMBER", "JO_ORDER_NUMBER", "TYPE_OF_HOUSING", "HOUSING_ADDRESS_LOCATION", "HOUSING_CITY", "HOUSING_STATE", "HOUSING_COUNTY", "HOUSING_POSTAL_CODE","HOUSING_STANDARDS_LOCAL",
                     "HOUSING_STANDARDS_STATE","HOUSING_STANDARDS_FEDERAL","TOTAL_UNITS","TOTAL_OCCUPANCY","table","Source","fixed","housing_fixed_by", "housing accuracy",
                     "housing_long", "housing_lat", "notes", "ADDITIONAL_HOUSING_INFORMATION")

col_housing <- c("HOUSING_ADDRESS_LOCATION",
                    "HOUSING_CITY",
                    "HOUSING_STATE",
                    "HOUSING_POSTAL_CODE",
                    "HOUSING_COUNTY",
                 "HOUSING_TRANSPORTATION",
                 "TOTAL_OCCUPANCY",
                 "TOTAL_UNITS",
                 "TYPE_OF_HOUSING",
                 "notes")

col_housing_compliance <- c("HOUSING_COMPLIANCE_FEDERAL",
                            "HOUSING_COMPLIANCE_LOCAL",
                            "HOUSING_COMPLIANCE_STATE"
                            )

col_worksite <- c("WORKSITE_ADDRESS",
                      "WORKSITE_ADDRESS2",
                      "WORKSITE_CITY",
                      "WORKSITE_STATE",
                      "WORKSITE_POSTAL_CODE",
                      "WORKSITE_COUNTY",
                  "OTHER_WORKSITE_LOCATION")

col_worksite_2 <- c( "Place of Employment Info/Address/Location",
                     "Place of Employment Info/City",            
                     "Place of Employment Info/Postal Code",      
                     "Place of Employment Info/State")

col_att_agent <- c("ATTORNEY_AGENT_ADDRESS_1",
                            "ATTORNEY_AGENT_ADDRESS_2",
                            "ATTORNEY_AGENT_CITY",
                            "ATTORNEY_AGENT_COUNTRY",
                            "ATTORNEY_AGENT_EMAIL",
                            "ATTORNEY_AGENT_EMAIL_ADDRESS",
                            "ATTORNEY_AGENT_FIRST_NAME",
                            "ATTORNEY_AGENT_LAST_NAME",
                            "ATTORNEY_AGENT_MIDDLE_NAME",
                            "ATTORNEY_AGENT_PHONE",
                            "ATTORNEY_AGENT_PHONE_EXT",
                            "ATTORNEY_AGENT_POSTAL_CODE",
                            "ATTORNEY_AGENT_PROVINCE",
                            "ATTORNEY_AGENT_STATE",
                   "TYPE_OF_REPRESENTATION"
                            )

col_entity <- c("FOREIGN_LABOR_RECRUITER",
                "H-2A_LABOR_CONTRACTOR",
                "LAWFIRM_NAME_BUSINESS_NAME")

col_employer <- c("TRADE_NAME_DBA", 
  "EMPLOYER_ADDRESS1",
                       "EMPLOYER_ADDRESS2",
                       "EMPLOYER_ADDRESS_1",
                       "EMPLOYER_ADDRESS_2",
                       "EMPLOYER_CITY" ,
                       "EMPLOYER_STATE",
                       "EMPLOYER_PROVINCE" ,
                       "EMPLOYER_POSTAL_CODE",
                       "EMPLOYER_COUNTRY",
                       "EMPLOYER_PHONE",
                       "EMPLOYER_PHONE_EXT"
                       )

col_employer_poc <- c("EMPLOYER_POC_ADDRESS1",
                          "EMPLOYER_POC_ADDRESS2" ,
                          "EMPLOYER_POC_CITY"  ,
                          "EMPLOYER_POC_STATE",
                          "EMPLOYER_POC_PROVINCE" ,
                          "EMPLOYER_POC_POSTAL_CODE" ,
                          "EMPLOYER_POC_COUNTRY"  ,
                          "EMPLOYER_POC_EMAIL" ,
                          "EMPLOYER_POC_JOB_TITLE",
                          "EMPLOYER_POC_FIRST_NAME"  ,
                          "EMPLOYER_POC_MIDDLE_NAME",
                          "EMPLOYER_POC_LAST_NAME",
                          "EMPLOYER_POC_PHONE",
                          "EMPLOYER_POC_PHONE_EXT" 
                          )

col_preparer <- c("PREPARER_BUSINESS_NAME",
                  "PREPARER_EMAIL",
                  "PREPARER_FIRST_NAME",
                  "PREPARER_MIDDLE_NAME",
                  "PREPARER_MIDDLE_INITIAL",
                  "PREPARER_LAST_NAME" 
                  )

col_codes <- c("NAICS_CODE",
              "SOC_CODE",
              "SOC_TITLE"
              )

col_job_req <- c("Experience Required",
                 "WORK_EXPERIENCE_MONTHS",
                              "CERTIFICATION_REQUIREMENTS",
                               "CRIMINAL_BACKGROUND_CHECK",
                               "DRIVER_REQUIREMENTS",
                               "DRUG_SCREEN",
                               "EDUCATION_LEVEL" ,
                               "ADDITIONAL_JOB_REQUIREMENTS")

col_hours <- c("ANTICIPATED_NUMBER_OF_HOURS",
               "HOURLY_SCHEDULE_BEGIN",
               "HOURLY_SCHEDULE_END",
                   "MONDAY_HOURS",
               "TUESDAY_HOURS",
                "WEDNESDAY_HOURS",
               "THURSDAY_HOURS",
                   "FRIDAY_HOURS",
               "SATURDAY_HOURS",
               "SUNDAY_HOURS"
                   )



col_job_info <- c("CASE_STATUS" ,
                  "Job duties",
                      "BOARD_LODGING_OTHER_FACILITIES",
                      "DAILY_TRANSPORTATION",
                      "EMP_PROVIDED_TOOLS_EQUIPMENT",
                      "EXPOSURE_TO_TEMPERATURES",
                      "EXTENSIVE_PUSHING_PULLING",
                      "EXTENSIVE_SITTING_WALKING",
                      "FREQUENT_STOOPING_BENDING_OVER",
                  "LIFTING_AMOUNT",
                  "REPETITIVE_MOVEMENTS",
                  "ON_CALL_REQUIREMENT",
                  "ON_THE_JOB_TRAINING_AVAILABLE",
                  "TRAINING_MONTHS" ,
                  "SUPERVISE_OTHER_EMP",
                  "SUPERVISE_HOW_MANY")

col_other_filing_info<- c(
                  "TOTAL_WORKERS_CERTIFIED" ,
                  "TOTAL_WORKERS_H-2A_CERTIFIED",
                  "TOTAL_WORKERS_H-2A_REQUESTED",
                  "TOTAL_WORKERS_NEEDED",
                  "Number of Workers Requested H-2B",
                  "JOB_ORDER_SUBMIT_DATE",
                      "DECISION_DATE",
                  "NATURE_OF_TEMPORARY_NEED",
                  "EMERGENCY_FILING",
                  "TYPE_OF_EMPLOYER_APPLICATION",
                  "TYPE_OF_EMPLOYER"
                      )

col_meals <- c("MEALS_PROVIDED",
               "MEALS_CHARGED",
               "MEAL_REIMBURSEMENT_MAXIMUM",
               "MEAL_REIMBURSEMENT_MINIMUM" 
               )


col_pay <- c("WAGE_OFFER",
  "BASIC_WAGE_RATE_FROM",
              "BASIC_WAGE_RATE_TO",
              "DEDUCTIONS_FROM_PAY",
             "Additional Wage Information",
             "OTHER_FREQUENCY_OF_PAY" ,
             "OVERTIME_AVAILABLE",
             "OVERTIME_RATE_FROM" ,
             "OVERTIME_RATE_TO",
             "PER",
             "PIECE_RATE_OFFER",
             "PIECE_RATE_UNIT"
             )

col_apply <- c("EMAIL_TO_APPLY",
               "PHONE_TO_APPLY",
               "WEBSITE_TO_APPLY"
                         )

col_geo <- c("fixed",
             "housing accuracy",
             "housing accuracy type",
             "housing_fixed_by",
             "housing_lat",                              
             "housing_long",
             "worksite accuracy",
             "worksite accuracy type",
             "worksite_fixed_by",                        
             "worksite_lat",
             "worksite_long" 
             )

col_add_app <- c("790A_ADDENDUM_B_ATTACHED",
                              "790A_addendum_a_attached",
                              "ADDENDUM_B_HOUSING_ATTACHED",
                              "ADDENDUM_B_WORKSITE_ATTACHED",
                              "APPENDIX_A_ATTACHED",
                              "APPENDIX_D_COMPLETED",
                              "EMPLOYER_APPENDIX_B_ATTACHED",
                              "EMPLOYER_MSPA_ATTACHED",
                              "EMP_CLIENT_APPENDIX_B_ATTACHED",
                 "JOINT_EMPLOYER_APPENDIX_A_ATTACHED",
                 "WORK_CONTRACTS_ATTACHED",
                 "SURETY_BOND_ATTACHED",
                 "TOTAL_ADDENDUM_A_RECORDS",
                 "TOTAL_HOUSING_RECORDS",
                 "TOTAL_WORKSITE_RECORDS" 
                 )

col_others <- c("CAP_EXEMPT",
                "JOB_ORDER_NUMBER",
                "NAME_OF_HIGHEST_STATE_COURT",
                "STATE_OF_HIGHEST_COURT",
                "Date of run",
                "Number of Pages",
                "REQUESTED_BEGIN_DATE",
                "REQUESTED_END_DATE",
                "SWA_STATE",
                "table",
                "W to H Ratio",
                "days_until_halfway",
                "is_halfway",
                "occupancy_minus_workers"
                   )


#For purposes of the tree/groupings -- to add a group, first add a new label, then add a group of variables under data = list(...)
#in the corresponding locations

var_lists <- tibble(
  
  #Labels for the groups
  name = c("Housing","Worksite","Addendums","Application",
           "Attorney/Agent", "Codes", "Employer","Employer Point of Contact",
           "Entity","Lat/Long","Hours"),
  
  #Variable lists that go with each label
  data = list(
    tibble("vars" = col_housing),
    tibble("vars" = col_worksite),
    tibble("vars" = col_add_app),
    tibble("vars" = col_apply),
    tibble("vars" = col_att_agent),
    tibble("vars" = col_codes),
    tibble("vars" = col_employer),
    tibble("vars" = col_employer_poc),
    tibble("vars" = col_entity),
    tibble("vars" = col_geo),
    tibble("vars" = col_hours)
  )
)

Additional <- var_lists %>%
  unnest(cols = c(data))
