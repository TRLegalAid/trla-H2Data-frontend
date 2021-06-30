# trla-h2data-frontend-R

## background

Job postings are added to a postgresql database daily. They are collected from a web scraper hosted on [Apify](https://my.apify.com/account), which gets latest postings from [here](https://seasonaljobs.dol.gov/). Each quarter, the official dataset put out by the US Department of Labor is merged with the existing data. 

Backend scripts to pull and manage the data are [here](https://github.com/TRLegalAid/trla-H2Data-backend).

The deployed tool is [here](https://trla.shinyapps.io/H2Data/). 

## things you need

All you'll really need is the 'config.yml' file with database credentials! Packages used throughout are all included in "set-up.R". You'll also need login credentials for shinyapps.io for deployment.

All help documentation is housed with the gethelptrla@gmail.com Google drive.

Additional resources located in the Notebook!


