

#Panel that stretches across the home page
#Come here to make any edits to the text on the home page
#Any time you're using quotes, use the \ to escape the characters

list(bsCollapse(
  bsCollapsePanel(title = "About the Data",

                  "This data is pulled from two sources.
                  We use a web scraper to pull new postings daily from",

                  tags$a(href = "https://seasonaljobs.dol.gov/", "seasonaljobs.dol.gov.",
                         target = "_blank"),

                  br(),
                  br(),

                  "For each quarter of the fiscal year, the Office of Foreign Labor Certification
                  (OFLC) releases a large dataset that includes many details about every job order
                  filed. We'll refer to this as \"quarterly disclosure data.\"",

                  br(),
                  br(),

                  "Each data source has its own advantages and disadvantages.
                  Scraping the data daily and compling it here is helpful, because
                  if you want to get information about multiple job postings and
                  organize it in a CSV or run any sort of analysis, the existing tools
                  either require manually copying information or waiting until
                  the quarterly disclosure data is released.",

                  br(),
                  br(),

                  "The quarterly disclosure data, on the other hand,
                  contains much more detailed information, like the locations
                  of additional housing sites and additional worksites.",

                  br(),
                  br(),

                  "When new quarterly disclosure data is released,
                  that data is integrated into this dataset as well.
                  We have made it easy to match information across different tables,
                  so that you can see the job order information,
                  additional housing information, and additional worksites
                  for a single case number all in one place.",

                  style = "success")
  ,

  bsCollapsePanel(title = "What kind of information can I get from this data?",

                  "Every piece of information in these data files can be called a \"variable.\"
                  The quarterly disclosure data has over 100 variables,
                  so we can't list them all here. But they include information
                  such as employer information, attorney/agent information,
                  information about the job, housing locations,
                  worksite locations, job requirements...the list goes on!",

                  br(),
                  br(),

                  "You can read the detailed definitions of each variable in
                  the quarterly disclosure data",

                  tags$a(href = "https://www.dol.gov/agencies/eta/foreign-labor/performance",
                         "here.", target = "_blank"),

                  "Scroll down to \"OFLC Programs and Disclosures\"
                  and under \"H-2A\" or \"H-2B,\" select a Record Layout to learn more.",

                  br(),
                  br(),

                  "The data that is scraped daily does not have as many variables
                  as the quarterly disclosure data, because we don't have access
                  to as much information. But, the scraped data does contain some
                  information that we cannot learn from the quarterly disclosure data.
                  This includes a detailed job description.",

                  br(),
                  br(),

                  "You can read a list of the variables that we are able to scrape
                  from the Seasonal Jobs website each day ",

                  tags$a(href="https://docs.google.com/document/d/e/2PACX-1vQZORwe_e2LGec93_9T-bB8QZbso1BFtW1FFZMc6FTJ2Xyl1FIYtzxTrOsWmU8Ekg/pub",
                         "here.", target = "_blank"),

                  "You can read more FAQs about the data ",

                  tags$a(href="https://docs.google.com/document/d/e/2PACX-1vR7-E58WX3bdD8_xTW0bSLEAiysIp6AE5ySG3X__a7ZgmAYaamagtn_hz_VO18jVw/pub",
                         "here.",target = "blank"),

                  style = "success"),

  bsCollapsePanel(title = "Search Clearance Orders > Search All Data",

  "Under the Search Clearance Orders menu, you first select options to filter the data.
  The only information you are required to enter is a Worksite State.
  You can always select \"All.\" ",

                  br(),
                  br(),

                  "Note that you can select whether you want to see jobs that have started,
                  have not started yet, or have ended. The default selection is all options.
                  These are based on the estimated date the work starts.",

                  br(),
                  br(),

                  "Click \"View Data\" to see the results of your selections.
                  Once you're at the data table, you can search,
                  sort, and download the data. If a case number is pink,
                  this means that it has additional housing and worksite records.",

                  tags$strong("Click on any of the pink case numbers to see
                  additional housing or worksites associated with that case number."),

                  br(),
                  br(),

                  "If you want to only see data that comes directly from DOL/OFLC,
                  check the box that says \"Exclude Scraped Data & See DOL Data Only.\"",

                  br(),
                  br(),

                  "Remember, the scraped data has a limited amount of information.
                  If you are doing research that requires detailed fields such as
                  \"Sunday Hours,\" then you may want to look only at
                  (not necessarily quite as recent) DOL data by checking that box.",

                  br(),
                  br(),

                  actionButton("toBasic", "Go to Basic Search"),

                  style = "success"),

  bsCollapsePanel(title = "H-2A Search Options",
                  "There are two additional search options. They both are only applicable to H-2A jobs.",

                  br(),
                  br(),

                  "The first option allows you to search all or part of an employer's name. Some employers come up under the \"Additional Worksites\"
                        table, so they are not always evident in a basic disclosure data search.",
                  
                  br(),
                  br(),
                  
                  "The first allows you to search by a case number to see and download additional housing and worksites associated with the case.",


                  br(),
                  br(),

                  actionButton("toAdv2", "Search by Employer"),

                  br(),
                  br(),

                  actionButton("toAdv1", "Search Additional H-2A Housing and Employers"),

                  style = "success"),

  bsCollapsePanel(title = "This sounds complicated...",

  "You're not wrong. There's a lot going on with this data.
                                      We suggest that you start with a basic search, look through the additional variables that you can add,
                                      and hover over a help button if you ever need some extra guidance.", style = "success"),

  #bsCollapsePanel(title = "Resources", "Coming soon!", style = "success"),

  bsCollapsePanel(title = "Contact", "If you have questions about this data,
                                      e-mail",
                tags$a(href="mailto: gethelp@trla.org","gethelp@trla.org", target = "_blank"),
                ".",
                style = "success")
))