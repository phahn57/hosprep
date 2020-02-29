# HospRep
HospRep is an application for a monthly overview over the performance of the complete hospital (Vulpius Klinik) and their different departments. From 2017, the shiny app replaced the outdated Excel-based sheets. Before 2017 preparation of the report wasted many days. Now after export of the data, preparing and deploying takes about 10 minutes.  

### Prerequisites for using RadaR

I built HospRep in R, an open source programming language using the Shiny package, a web application framework for R. Users will need to download R to use RadaR and we suggest the use of RStudio. R is free to use. All required code is in this github repository.

### Data
We store the required data in two lists, y_prev and y_akt, each list containing the data for a specific period, y_prev (2018-2019), y_akt (2019-2020). The data are an export of our hospital EHR (electronic health records). Pre-processing with R removes patients identification and build tables and data summaries to enhance the performance of the shiny app. 

### Shinyapps.io and github
The application is published on shinyapps.io: https://kphahn.shinyapps.io/hosprep/. Github repo: https://github.com/phahn57/hosprep

### Privacy
HospRep works with sensitive patients data. During pre-processing, all patient identification are removed. For the running example, the original data are modified and do not reflect the real hospital. Names of referring doctors and the names of the intern doctors are anonymized.

### Language
I translated the main features into English, but some parts are in German because they are difficult to translate due to some strange characteristics of the German Health system.

### Author 
HospRep was created by Peter Hahn, Professor of Hand Surgery and Medical director at the Vulpius Klinik. After finishing Johns Hopkins Data Science Qualification 2015, I continuously developed my skills in R by several projects. HospRep is one of them.  
