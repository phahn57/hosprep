# Readme
###HospRep
HospRep is an application for a monthly overview over the achievments of the complete hospital (Vulpius Klinik) and the their different departments. Beginning in 2017 the shiny app replaced the outdated Excel-based sheets.

### Prerequisites for using RadaR

HospRep was built in R , an open source programming language using the Shiny package, a web application framework for R. Users will need to download R in order to use RadaR and we suggest the use of RStudio. R is completely free to use. All required code can be found in this github repository.

### Data
The required data are stored in lists, y_prev and y_akt, each list containing the data for a specific period. y_prev (2018-2019), y_akt (2019-2020). The data in the lists are preprocessed with R and are exported from our hospital EHR (electronic health records). 

### Privacy
HospRep works with sensitive patients data. During preprocessing all patients identification are removed. For the running example the original data are modified and do not reflect the real hospital. Names of reffering doctors and the names of the intern doctors are anonymized.

### Language
The main features are translated into English, but some parts are in German because they are difficult to translate due to some strange characteristics of the German Health system.

### Author 
HospRep was created by Peter Hahn, Professor of Hand Surgery and Medical director at the Vulpius Klinik. After finishing Johns Hopkins Data Science Qualification 2015, I continously developed my skills in R by several projects. HospRep is one of them.  
