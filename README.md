# trla-h2data-frontend-R

## background

Job postings are added to a PostgreSQL database daily. They are collected from a web scraper hosted on [Apify](https://my.apify.com/account), which gets latest postings from the DOL RSS feed [here](https://seasonaljobs.dol.gov/). Each quarter, the official dataset put out by the US Department of Labor is merged with the existing scraped data.

Backend scripts to pull and manage the data are [here](https://github.com/TRLegalAid/trla-H2Data-backend).

The deployed tool is [here](https://trla.shinyapps.io/H2Data/).

## things you need

All you'll really need is the 'config.yml' file with database credentials! Packages used throughout are all included in "set-up.R". You'll also need credentials for shinyapps.io for deployment - log in with the gethelptrla@gmail.com account.

All help documentation is housed with the gethelptrla@gmail.com Google drive.

Additional resources located in the Data&Maps SharePoint Notebook!

## How to test changes and deploy to Shinyapps.io

From RStudio, click "Run App" in the top right-hand corner to test changes locally. When you're ready to deploy, click the drop-down arrow of the "Republish" button and choose your deployment destination.

If it's the first time you are deploying, you'll be prompted to log in to shinyapps.io. Choose the "Sign in with Google" option and use the gethelptral@gmail.com account credentials in Dashlane.

If you're making a complicated change and want to make sure it works as expected when deployed, or want someone else to test it before deploying to trla.shinyapps.io/H2Data/, you can deploy to trla.shinyapps.io/trla-H2Data-frontend/ as a test site. Otherwise, set the destination to H2Data and replace the current app with your changes.


## Scripts

###set-up.R
Loads libraries, sets some variables and functions for the map.

###variable-lists.R


###global.R
Sources set-up.R and variable-lists.R. This script creates the connection pool to the Heroku Postgres database, and sets the dropdown options and defaults for the H2Data App's search filters.


###home_text.R
Text for the home page. Make any edits needed to that section here!


###server.R



###ui.R
