library(leaflet)
library(RColorBrewer)
library(raster)
library(htmlwidgets)
library(gdata)
library(DT)
require(dendextend)
require(installr)
require(colorspace)
library(shiny)
library(d3heatmap)
library(gplots)
library(ggdendro)
library(dbConnect)
library(dplyr)
library(stringi)
library(ggplot2)
library(scales)
#library(mapview)
#library(webshot)
library(plotly)

source("../global_server.R")
source("../global_ui.R") # so we can see EXAMPLE_FILES
source("../config.R") # load DB connection details
`%then%` <- shiny:::`%OR%` #function to have several validation for error handling
# Data for region mapping----
region_names <- read.csv("../department_municipality_name.csv",
                         header = T,
                         sep = ",")
# Data for bar graph variable names mapping
bar_graphs_mappings <- read.csv("tools/bar_graphs_column_names_mapping.csv",
                                header = T,
                                sep = ",",
                                col.names = c("actual","predicted", "axis_label","best_worst_projection"),
                                colClasses = c("character","character","character", "character"))
# Data to map between country names in data files and map file names
maps_files_to_data_files <- read.csv("tools/map_name_to_data_name.csv",
                                     header = T,
                                     sep = ",",
                                     col.names = c("datafile","prefix"),
                                     colClasses = c("character","character"))

# Constants----
dimensions_msg <- "Input data can have up to 50 data columns."

#FUNCTIONS----
mround <- function(x,base){
 return(base*round(x/base))
}
#Idea from: https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  capitalized_string <- paste(toupper(substring(c, 1,1)), substring(c, 2),
                              sep="", collapse=" ")
  capitalized_string <- gsub(pattern = " And ", replacement = " and ",capitalized_string)
  capitalized_string <- gsub(pattern = " Of ", replacement = " of ",capitalized_string)
  capitalized_string <- gsub(pattern = " The ", replacement = " the ",capitalized_string)
  return(capitalized_string)
}
capitalize_str <- function(charcter_string){
  sapply(charcter_string, CapStr)
}
# Logging & debugging
log_filename = tryCatch({
  paste(system("hostname", intern = TRUE),
        'log.txt', sep = "_")
}, error = function(e) {
  'log.txt'
})
if (!exists('logDir') || is.na(logDir) || logDir == '') {
	logDir = '.'
}
log_filename = paste(logDir, log_filename, sep='/')
debug = FALSE

# reference: https://jcheng.shinyapps.io/choropleth3/
shinyServer(function(input, output, session) {
  values <- reactiveValues(
    file = NULL,
    inputFile = NULL,
    highlight = NULL,
    density = NULL,
    colours = NULL,
    to = NULL,
    from = NULL,
    palette = NULL,
    map = NULL
  )
  #################### OBSERVERS ####################
  observe({
    input$clearFile
    values$inputFile <- NULL
    log_activity('geomap', 'clearFile')
  })
  
  observe({
    values$inputFile <- input$file
    log_activity('geomap', 'input$file')
    debug = TRUE
    if (debug)
      write('observe input$file run',
            file = log_filename,
            append = TRUE)
  })
  
  # when a valid column is selected set values$density
  observe({
   # input$radio
    if (input$colSelect != 0) {
      tryCatch({
        if (debug) {
          write('observe input$colSelect triggered',
                file = log_filename,
                append = TRUE)
          write('value of input$colSelect:',
                file = log_filename,
                append = TRUE)
          write(input$colSelect,
                file = log_filename,
                append = TRUE)
        }
        log_activity('geomap', 'input$colSelect')
        
        values$density <- get_density()
        if (debug)
          write(
            'observe input$colSelect: get_density returned',
            file = log_filename,
            append = TRUE
          )
        
        # update the slider with the new max and min from that column
        if (debug) {
          write(paste('starting minmax', sep = "\t"),
                file = log_filename,
                append = TRUE)
          write(values$density, file = log_filename, append = TRUE)
          write(paste('applying min...', sep = "\t"),
                file = log_filename,
                append = TRUE)
        }
        min <- floor(min(values$density, na.rm = TRUE))
        if (is.infinite(min)){
          min <- 0
        }
        if (debug) {
          write(values$density, file = log_filename, append = TRUE)
          write(paste('applying max...', sep = "\t"),
                file = log_filename,
                append = TRUE)
        }
        max <- ceiling(max(values$density, na.rm = TRUE))
        #Setting values of larger countries as thresholds for max value
        col_names <- names(values$density)
        #World map
        if ("united states of america" %in% col_names &&
            "spain" %in% col_names &&
            "france" %in% col_names &&
            "italy" %in% col_names &&
            "south africa" %in% col_names &&
            "china" %in% col_names &&
            "brazil" %in% col_names &&
            "australia" %in% col_names ) {
          max <- ceiling(max(values$density[names(values$density) %in% c("united states of america",
                                                                         "spain",
                                                                         "france",
                                                                         "italy",
                                                                         "south africa",
                                                                         "china",
                                                                         "brazil",
                                                                         "australia")], 
                             na.rm = TRUE))
        } else if ("united states of america" %in% col_names){
          #Northern America
          max <- ceiling(values$density[names(values$density) == "united states of america"])
        } else if ("spain" %in% col_names &&
                   "france" %in% col_names &&
                   "italy" %in% col_names){
          #Europe
          max <- ceiling(max(values$density[names(values$density) %in% c("spain",
                                                                         "france",
                                                                         "italy")],
                             na.rm = TRUE))
        } else if ("china" %in% col_names &&
                   "iran" %in% col_names){
          #Asia
          max <- ceiling(max(values$density[names(values$density) %in% c("china",
                                                                         "iran")], 
                             na.rm = TRUE))
        } else if ("brazil" %in% col_names) {
          #South America
          max <- ceiling(values$density[names(values$density) == "brazil"])
        } else if ("australia" %in% col_names) {
          # Oceania
          max <- ceiling(values$density[names(values$density) == "australia"])
        } else if ("algeria" %in% col_names &&
                   "egypt" %in% col_names &&
                   "south africa" %in% col_names) {
          #Africa
          max <- ceiling(max(values$density[names(values$density) %in% c("algeria",
                                                                         "egypt",
                                                                         "south africa")], 
                             na.rm = TRUE))
        }

        if (is.infinite(max)){
          max <- 0
        }
        if (debug) {
          write(values$density, file = log_filename, append = TRUE)
          write(paste('done minmax', sep = "\t"),
                file = log_filename,
                append = TRUE)
          write(paste('min:', min, sep = "\t"),
                file = log_filename,
                append = TRUE)
          write(paste('max:', max, sep = "\t"),
                file = log_filename,
                append = TRUE)
        }
        
        # set number of digit to which to round
        num_digits = 0
        if (max < 10) {
          num_digits = 2
        } else if (max < 100) {
          num_digits = 1
        }
        #num_digits = -1*log10(mean(min,max))+1
        updateSliderInput(
          session,
          inputId = "range",
          min = min,
          max = max,
          value = c(min, max)
        )
        if (debug)
          write(
            'observe input$colSelect: updateSliderInput returned',
            file = log_filename,
            append = TRUE
          )
        isolate({
          if (is.finite(min) && is.finite(max) && min != max) {
            #update_colours(round(seq(min, max, length.out = 8 + 1), num_digits))#input$binNumber
            #Added logarithmic scale if max is 500 times higher than min
            if (max/max(abs(min),1) > 500){
              densityBreaks <- round(10^seq(log10(max(min,1)), log10(max), length.out = 8 + 1), num_digits)
            } else {
              densityBreaks <- round(seq(min, max, length.out = 8 + 1), num_digits)
            }
            
            if (min == 0){
              #Set lower limit to 0
              densityBreaks[1] <- 0
            } 
            update_colours(densityBreaks)#input$binNumber
    
            
            if (debug)
              write(
                'observe input$colSelect: update_colours returned',
                file = log_filename,
                append = TRUE
              )
          } else {
            update_colours(round(min, num_digits))
          }
        })
        
        # When user chooses an example file, automatically set the
        # appropriate map.
        map_to_select = ''
        # if (input$exampleFiles == EXAMPLE_FILES[1]) {
        # 	map_to_select = 'data/CAN_1.rds'
        # } else if (input$exampleFiles == EXAMPLE_FILES[2]) {
        # 	map_to_select = 'data/CAN_1.rds'
        # } else if (input$exampleFiles == EXAMPLE_FILES[3]) {
        # 	map_to_select = 'data/USA_1.rds'
        # } else if (input$exampleFiles == EXAMPLE_FILES[4]) {
        #     map_to_select = 'data/COL_2.rds'
        # }
        if (map_to_select != '') {
          updateSelectInput(
            session,
            inputId = "area",
            label = NULL,
            choices = NULL,
            selected = map_to_select
          )
        }
        
        if (debug)
          write(
            'observe input$colSelect: completed without exception',
            file = log_filename,
            append = TRUE
          )
      },
      # warning = function(warning_condition) {
      # 	write('observe input$colSelect: caught warning:', file=log_filename, append=TRUE)
      # 	write(paste0(warning_condition), file=log_filename, append=TRUE)
      # },
      error = function(error_condition) {
        write(
          'observe input$colSelect: caught error:',
          file = log_filename,
          append = TRUE
        )
        write(paste0(error_condition),
              file = log_filename,
              append = TRUE)
      })
    }
  })
  
  # update colours when range of interest is changed or new file is selected
  observe({
    #input$rangeSubmit
    #input$binNumber
    # input$lowColour
    # input$highColour
    # input$colourScheme
 #   input$radio
    log_activity('geomap',
                 'observe rangeSubmit binNumber lowColour highColour colourScheme')
    isolate({
      if (!is.null(values$density)) {
        rangeMin <- 0
        rangeMax <- 100
        
        min <- floor(min(values$density, na.rm = TRUE))
        max <- ceiling(max(values$density, na.rm = TRUE))
        
        # min_tester <- values$density[values$density != 0]
        # 
        # if (min(min_tester, na.rm = TRUE) < 0.9){
        #   min <- min(values$density, na.rm = TRUE)
        #   max <- max(values$density, na.rm = TRUE)
        # } else {
        #   min <- floor(min(values$density, na.rm = TRUE))
        #   max <- ceiling(max(values$density, na.rm = TRUE))
        # }
        bins <- 8 + 1 #input$binNumber
        
        # adjust selected range if difference between max and min is < # of bins
        if (rangeMax - rangeMin < bins) {
          if (rangeMax - bins > min) {
            rangeMin <- rangeMax - bins
          }
          else if (rangeMin + bins < max) {
            rangeMax <- rangeMin + bins
          }
          # error handling for when range is < #bins, this should be improved upon
          else{
            rangeMin <- min
            rangeMax <- max
            bins <- max - min
          }
          updateSliderInput(session,
                            inputId = "range",
                            value = c(rangeMin, rangeMax))
        }
        
        densityBreaks <- get_breaks(rangeMin, rangeMax, min, max, bins)
        update_colours(densityBreaks)  
      }
    })
  })
  
  # if input$area is updated change map
  observe({
    log_activity('geomap', 'observe values$map')
    values$map <- readRDS(input$area)
  })
  
  #If date is changed update dropdown menus
  observe({
    datepart <- input$date
    #reading file and updating based on file
    map_file_name <- input$area
    choice = input$tabSelections
    isolate({
      col_selected <- input$colSelect
        if (choice == "Plots" &&
            !(col_selected %in% bar_graphs_mappings$actual)
            ){
          col_selected <- "Confirmed"
        } 
      #Retrieve datafile mapping string
      datafile_mapping <- maps_files_to_data_files %>% 
        filter(datafile == input$area) %>% 
        pull(prefix)
      #Extract region name
      region_name <- datafile_mapping %>% 
        paste("_",sep = "")
      prefix <- paste("../filesystem/",region_name,sep = "")
      file_name <-
        paste(prefix, datepart,".txt", sep = "")
      if (file.exists(file_name)){
        #read file
        data_file <- read.csv(file = file_name,
                              sep = "\t",
                              stringsAsFactors = FALSE)
      } else {
        path_initial <- strsplit(datafile_mapping, split = "/") %>% unlist()
        path_corrected <- path_initial[-length(path_initial)]
        path_corrected <- paste(path_corrected, collapse = "/")
        file_full_path <- paste("../filesystem", path_corrected,"accumulated.txt", sep = "/")
        data_file <- read.csv(file = file_full_path,
                              sep = "\t",
                              stringsAsFactors = FALSE)
      }
      
      col_names <- colnames(data_file)
    })
    #TODO Hack for Alberta
    if (choice == "Plots" &&
        col_selected %in% bar_graphs_mappings$actual){
      updateSelectInput(session,
                        inputId = "colSelect",
                        label = "Select Data to Display:",
                        choices = c("Confirmed COVID-19 Cases" = 'Confirmed',
                                    "Confirmed COVID-19 Cases Daily" = 'Confirmed_daily',
                                    "Confirmed Deaths" = 'Deaths',
                                    "Confirmed Deaths Daily" = 'Deaths_daily',
                                    'Confirmed COVID-19 Cases Daily (Worst Case Scenario)' = 'Cases_worst_case',
                                    #'Confirmed COVID-19 Cases Daily per 100,000 (Worst Case Scenario)' = 'Cases_per_capita_worst_case',
                                    'Confirmed COVID-19 Cases (Worst Case Scenario)' = 'Total_Cases_worst_case',
                                    #'Confirmed COVID-19 Cases per 100,000 (Worst Case Scenario)' = 'Total_Cases_per_capita_worst_case',
                                    'Confirmed Deaths Daily (Worst Case Scenario)' = 'Deaths_worst_case',
                                    #'Confirmed Deaths Daily per 100,000 (Worst Case Scenario)' = 'Deaths_per_capita_worst_case',
                                    'Confirmed Deaths (Worst Case Scenario)' = 'Total_Deaths_worst_case',
                                    #'Confirmed Deaths per 100,000 (Worst Case Scenario)' = 'Total_Deaths_per_capita_worst_case',
                                    'Confirmed COVID-19 Cases Daily (Best Case Scenario)' = 'Cases_best_case',
                                    #'Confirmed COVID-19 Cases Daily per 100,000 (Best Case Scenario)' = 'Cases_per_capita_best_case',
                                    'Confirmed COVID-19 Cases (Best Case Scenario)' = 'Total_Cases_best_case',
                                    #'Confirmed COVID-19 Cases per 100,000 (Best Case Scenario)' = 'Total_Cases_per_capita_best_case',
                                    'Confirmed Deaths Daily (Best Case Scenario)' = 'Deaths_best_case',
                                    #'Confirmed Deaths Daily per 100,000 (Best Case Scenario)' = 'Deaths_per_capita_best_case',
                                    'Confirmed Deaths (Best Case Scenario)' = 'Total_Deaths_best_case'
                                    #'Confirmed Deaths per 100,000 (Best Case Scenario)' = 'Total_Deaths_per_capita_best_case'
                        ),
                        selected = col_selected
      )
    } 
    else if ("Tests" %in% col_names && 
             choice %in% c("Heatmap", "Table")){
      updateSelectInput(session,
                        inputId = "colSelect",
                        label = "Select Data to Display:",
                        choices = c("Confirmed COVID-19 Cases" = 'Confirmed',
                                    #"Recovered" = 'Recovered',
                                    "Confirmed Deaths" = 'Deaths',
                                    #"Active" = 'Active',
                                    "Confirmed COVID-19 Cases per 100,000" = "Confirmed_per_capita",
                                    "COVID-19 Deaths per 100,000" = "Deaths_per_capita",
                                    # "% Daily Change in Confirmed COVID-19 Cases" = "Confirmed_change",
                                    # "% Daily Change in COVID-19 Deaths" = "Deaths_change",
                                    "Likely COVID-19 Cases (IFR 0.30%)" = 'IFR_0.30_expected',
                                    "Likely COVID-19 Cases (IFR 0.65%)" = 'IFR_0.65_expected',
                                    "Likely COVID-19 Cases (IFR 1.00%)" = 'IFR_1.0_expected',
                                    "COVID-19 Tests Performed" = 'Tests',
                                    "COVID-19 Tests Performed per 100,000" = 'Tests_per_capita'
                        ),
                        selected = col_selected
      )
    } else if ("Predicted_New_Cases" %in% col_names && 
               choice %in% c("Heatmap", "Table")){
      updateSelectInput(session,
                        inputId = "colSelect",
                        label = "Select Data to Display:",
                        choices = c("Predicted Daily Cases" = 'Predicted_New_Cases',
                                    "Predicted Daily Cases per 100000" = 'Predicted_New_per_capita',
                                    "Predicted Total Cases" = 'Total_Predicted_Cases',
                                    "Predicted Total Cases per 100000" = 'Predicted_Total_Cases_per_capita',
                                    "Predicted Daily Deaths" = 'Predicted_New_Deaths',
                                    "Predicted Daily Deaths per 100000" = 'Predicted_New_Deaths_per_capita',
                                    "Predicted Total Deaths" = 'Total_Predicted_Deaths',
                                    "Predicted Total Deaths per 100000" = 'Predicted_Total_Deaths_per_capita'),
                        selected = col_selected)
    } 
    else {
      updateSelectInput(session,
                        inputId = "colSelect",
                        label = "Select Data to Display:",
                        choices = c("Confirmed COVID-19 Cases" = 'Confirmed',
                                    #"Recovered" = 'Recovered',
                                    "Confirmed Deaths" = 'Deaths',
                                    #"Active" = 'Active',
                                    "Confirmed COVID-19 Cases per 100,000" = "Confirmed_per_capita",
                                    "COVID-19 Deaths per 100,000" = "Deaths_per_capita",
                                    # "% Daily Change in Confirmed COVID-19 Cases" = "Confirmed_change",
                                    # "% Daily Change in COVID-19 Deaths" = "Deaths_change",
                                    "Likely COVID-19 Cases (IFR 0.30%)" = 'IFR_0.30_expected',
                                    "Likely COVID-19 Cases (IFR 0.65%)" = 'IFR_0.65_expected',
                                    "Likely COVID-19 Cases (IFR 1.00%)" = 'IFR_1.0_expected'
                                    ),
                        selected = col_selected
                        )
    }
  })
  
  observe({
    choice <- input$tabSelections
    isolate({
      area_selected <- input$area
      if (area_selected %in% c('data/World_Countries.rds',
                              'data/Africa.rds', 
                              'data/Asia.rds', 
                              'data/Europe.rds', 
                              'data/North_America.rds',
                              'data/Oceania.rds',
                              'data/South_America.rds') &&
          choice == "Plots"){
        area_selected <- 'data/CAN_1.rds'
      }
    })
    if (choice == "Plots"){
      updateSelectInput(session,
                        inputId = "area",
                        label = "Plot COVID bar graphs by:",
                        choices = c("Australia" = 'data/AUS_1.rds',
                                    "Canada" = 'data/CAN_1.rds',
                                    "China" = 'data/CHN_1.rds',
                                    "France" = 'data/FRA_1.rds',
                                    "Germany" = 'data/DEU_1.rds',
                                    "Iran" = 'data/IRN_1.rds',
                                    "Italy" = 'data/ITA_1.rds',
                                    "Spain" = 'data/ESP_1.rds',
                                    "United Kingdom" = 'data/GBR_1.rds',
                                    "United States" = 'data/USA_1.rds',
                                    #"Canada: Level 2" = 'data/CAN_2.rds',
                                    
                                    #"USA: Level 2" = 'data/USA_2.rds',
                                    #"United Kingdom: Level 2" = 'data/GBR_2.rds',
                                    #"Australia: Level 2" = 'data/AUS_2.rds',
                                    
                                    "Afghanistan" = 'data/AFG_1.rds',
                                    # "Akrotiri and Dhekelia" = 'data/XAD_1.rds',
                                    # "Ćland" = 'data/ALA_1.rds',
                                    "Albania" = 'data/ALB_1.rds',
                                    "Algeria" = 'data/DZA_1.rds',
                                    # "American Samoa" = 'data/ASM_1.rds',
                                    "Andorra" = 'data/AND_1.rds',
                                    "Angola" = 'data/AGO_1.rds',
                                    # "Antigua and Barbuda" = 'data/ATG_1.rds',
                                    "Argentina" = 'data/ARG_1.rds',
                                    "Armenia" = 'data/ARM_1.rds',
                                    
                                    "Austria" = 'data/AUT_1.rds',
                                    "Azerbaijan" = 'data/AZE_1.rds',
                                    "Bahamas" = 'data/BHS_1.rds',
                                    "Bahrain" = 'data/BHR_1.rds',
                                    "Bangladesh" = 'data/BGD_1.rds',
                                    "Barbados" = 'data/BRB_1.rds',
                                    "Belarus" = 'data/BLR_1.rds',
                                    "Belgium" = 'data/BEL_1.rds',
                                    "Belize" = 'data/BLZ_1.rds',
                                    "Benin" = 'data/BEN_1.rds',
                                    # "Bermuda" = 'data/BMU_1.rds',
                                    "Bhutan" = 'data/BTN_1.rds',
                                    "Bolivia" = 'data/BOL_1.rds',
                                    # "Bonaire, Saint Eustatius and Saba" = 'data/BES_1.rds',
                                    # "Bosnia and Herzegovina" = 'data/BIH_1.rds',
                                    "Botswana" = 'data/BWA_1.rds',
                                    "Brazil" = 'data/BRA_1.rds',
                                    # "British Virgin Islands" = 'data/VGB_1.rds',
                                    "Brunei" = 'data/BRN_1.rds',
                                    "Bulgaria" = 'data/BGR_1.rds',
                                    "Burkina Faso" = 'data/BFA_1.rds',
                                    "Burundi" = 'data/BDI_1.rds',
                                    "Cambodia" = 'data/KHM_1.rds',
                                    "Cameroon" = 'data/CMR_1.rds',
                                    "Cape Verde" = 'data/CPV_1.rds',
                                    # "Cayman Islands" = 'data/CYM_1.rds',
                                    # "Central African Republic" = 'data/CAF_1.rds',
                                    "Chad" = 'data/TCD_1.rds',
                                    "Chile" = 'data/CHL_1.rds',
                                    "Colombia" = 'data/COL_1.rds',
                                    # "Comoros" = 'data/COM_1.rds',
                                    "Costa Rica" = 'data/CRI_1.rds',
                                    # "CĆ“te d'Ivoire" = 'data/CIV_1.rds',
                                    "Croatia" = 'data/HRV_1.rds',
                                    "Cuba" = 'data/CUB_1.rds',
                                    "Cyprus" = 'data/CYP_1.rds',
                                    "Czech Republic" = 'data/CZE_1.rds',
                                    "Democratic Republic of the Congo" = 'data/COD_1.rds',
                                    "Denmark" = 'data/DNK_1.rds',
                                    "Djibouti" = 'data/DJI_1.rds',
                                    "Dominica" = 'data/DMA_1.rds',
                                    "Dominican Republic" = 'data/DOM_1.rds',
                                    "Ecuador" = 'data/ECU_1.rds',
                                    "Egypt" = 'data/EGY_1.rds',
                                    "El Salvador" = 'data/SLV_1.rds',
                                    "Equatorial Guinea" = 'data/GNQ_1.rds',
                                    "Eritrea" = 'data/ERI_1.rds',
                                    "Estonia" = 'data/EST_1.rds',
                                    "Ethiopia" = 'data/ETH_1.rds',
                                    # "Faroe Islands" = 'data/FRO_1.rds',
                                    "Fiji" = 'data/FJI_1.rds',
                                    "Finland" = 'data/FIN_1.rds',
                                    
                                    # "French Guiana" = 'data/GUF_1.rds',
                                    # "French Polynesia" = 'data/PYF_1.rds',
                                    # "French Southern Territories" = 'data/ATF_1.rds',
                                    "Gabon" = 'data/GAB_1.rds',
                                    "Gambia" = 'data/GMB_1.rds',
                                    "Georgia" = 'data/GEO_1.rds',
                                    
                                    "Ghana" = 'data/GHA_1.rds',
                                    "Greece" = 'data/GRC_1.rds',
                                    # "Greenland" = 'data/GRL_1.rds',
                                    "Grenada" = 'data/GRD_1.rds',
                                    # "Guadeloupe" = 'data/GLP_1.rds',
                                    # "Guam" = 'data/GUM_1.rds',
                                    "Guatemala" = 'data/GTM_1.rds',
                                    "Guinea-Bissau" = 'data/GNB_1.rds',
                                    "Guinea" = 'data/GIN_1.rds',
                                    "Guyana" = 'data/GUY_1.rds',
                                    "Haiti" = 'data/HTI_1.rds',
                                    "Honduras" = 'data/HND_1.rds',
                                    # "Hong Kong" = 'data/HKG_1.rds',
                                    "Hungary" = 'data/HUN_1.rds',
                                    "Iceland" = 'data/ISL_1.rds',
                                    "India" = 'data/IND_1.rds',
                                    "Indonesia" = 'data/IDN_1.rds',
                                    "Iraq" = 'data/IRQ_1.rds',
                                    "Ireland" = 'data/IRL_1.rds',
                                    "Israel" = 'data/ISR_1.rds',
                                    "Jamaica" = 'data/JAM_1.rds',
                                    "Japan" = 'data/JPN_1.rds',
                                    "Jordan" = 'data/JOR_1.rds',
                                    "Kazakhstan" = 'data/KAZ_1.rds',
                                    "Kenya" = 'data/KEN_1.rds',
                                    "Kosovo" = 'data/XKO_1.rds',
                                    "Kuwait" = 'data/KWT_1.rds',
                                    "Kyrgyzstan" = 'data/KGZ_1.rds',
                                    "Laos" = 'data/LAO_1.rds',
                                    "Latvia" = 'data/LVA_1.rds',
                                    "Lebanon" = 'data/LBN_1.rds',
                                    # "Lesotho" = 'data/LSO_1.rds',
                                    "Liberia" = 'data/LBR_1.rds',
                                    "Libya" = 'data/LBY_1.rds',
                                    "Liechtenstein" = 'data/LIE_1.rds',
                                    "Lithuania" = 'data/LTU_1.rds',
                                    "Luxembourg" = 'data/LUX_1.rds',
                                    # "Macao" = 'data/MAC_1.rds',
                                    "Macedonia" = 'data/MKD_1.rds',
                                    # "Madagascar" = 'data/MDG_1.rds',
                                    "Malawi" = 'data/MWI_1.rds',
                                    "Malaysia" = 'data/MYS_1.rds',
                                    "Mali" = 'data/MLI_1.rds',
                                    # "Martinique" = 'data/MTQ_1.rds',
                                    "Mauritania" = 'data/MRT_1.rds',
                                    "Mauritius" = 'data/MUS_1.rds',
                                    # "Mayotte" = 'data/MYT_1.rds',
                                    "Mexico" = 'data/MEX_1.rds',
                                    # "Micronesia" = 'data/FSM_1.rds',
                                    "Moldova" = 'data/MDA_1.rds',
                                    "Mongolia" = 'data/MNG_1.rds',
                                    "Montenegro" = 'data/MNE_1.rds',
                                    # "Montserrat" = 'data/MSR_1.rds',
                                    "Morocco" = 'data/MAR_1.rds',
                                    "Mozambique" = 'data/MOZ_1.rds',
                                    "Myanmar" = 'data/MMR_1.rds',
                                    "Namibia" = 'data/NAM_1.rds',
                                    # "Nauru" = 'data/NRU_1.rds',
                                    "Nepal" = 'data/NPL_1.rds',
                                    "Netherlands" = 'data/NLD_1.rds',
                                    # "New Caledonia" = 'data/NCL_1.rds',
                                    "New Zealand" = 'data/NZL_1.rds',
                                    "Nicaragua" = 'data/NIC_1.rds',
                                    "Niger" = 'data/NER_1.rds',
                                    "Nigeria" = 'data/NGA_1.rds',
                                    # "North Korea" = 'data/PRK_1.rds',
                                    # "Northern Cyprus" = 'data/XNC_1.rds',
                                    # "Northern Mariana Islands" = 'data/MNP_1.rds',
                                    "Norway" = 'data/NOR_1.rds',
                                    "Oman" = 'data/OMN_1.rds',
                                    "Pakistan" = 'data/PAK_1.rds',
                                    # "Palau" = 'data/PLW_1.rds',
                                    "West Bank and Gaza" = 'data/PSE_1.rds',
                                    "Panama" = 'data/PAN_1.rds',
                                    "Papua New Guinea" = 'data/PNG_1.rds',
                                    "Paraguay" = 'data/PRY_1.rds',
                                    "Peru" = 'data/PER_1.rds',
                                    "Philippines" = 'data/PHL_1.rds',
                                    "Poland" = 'data/POL_1.rds',
                                    "Portugal" = 'data/PRT_1.rds',
                                    # "Puerto Rico" = 'data/PRI_1.rds',
                                    "Qatar" = 'data/QAT_1.rds',
                                    "Republic of Congo" = 'data/COG_1.rds',
                                    # "Reunion" = 'data/REU_1.rds',
                                    "Romania" = 'data/ROU_1.rds',
                                    "Russia" = 'data/RUS_1.rds',
                                    "Rwanda" = 'data/RWA_1.rds',
                                    # "Saint Helena" = 'data/SHN_1.rds',
                                    "Saint Kitts and Nevis" = 'data/KNA_1.rds',
                                    "Saint Lucia" = 'data/LCA_1.rds',
                                    # "Saint Pierre and Miquelon" = 'data/SPM_1.rds',
                                    "Saint Vincent and the Grenadines" = 'data/VCT_1.rds',
                                    # "Samoa" = 'data/WSM_1.rds',
                                    # "San Marino" = 'data/SMR_1.rds',
                                    "Sao Tome and Principe" = 'data/STP_1.rds',
                                    "Saudi Arabia" = 'data/SAU_1.rds',
                                    "Senegal" = 'data/SEN_1.rds',
                                    "Serbia" = 'data/SRB_1.rds',
                                    "Sierra Leone" = 'data/SLE_1.rds',
                                    "Slovakia" = 'data/SVK_1.rds',
                                    "Slovenia" = 'data/SVN_1.rds',
                                    # "Solomon Islands" = 'data/SLB_1.rds',
                                    "Somalia" = 'data/SOM_1.rds',
                                    "South Africa" = 'data/ZAF_1.rds',
                                    "South Korea" = 'data/KOR_1.rds',
                                    "South Sudan" = 'data/SSD_1.rds',
                                    "Sri Lanka" = 'data/LKA_1.rds',
                                    "Sudan" = 'data/SDN_1.rds',
                                    "Suriname" = 'data/SUR_1.rds',
                                    # "Svalbard and Jan Mayen" = 'data/SJM_1.rds',
                                    "Swaziland" = 'data/SWZ_1.rds',
                                    "Sweden" = 'data/SWE_1.rds',
                                    "Switzerland" = 'data/CHE_1.rds',
                                    # "Syria" = 'data/SYR_1.rds',
                                    "Taiwan" = 'data/TWN_1.rds',
                                    # "Tajikistan" = 'data/TJK_1.rds',
                                    "Tanzania" = 'data/TZA_1.rds',
                                    "Thailand" = 'data/THA_1.rds',
                                    # "Timor-Leste" = 'data/TLS_1.rds',
                                    "Togo" = 'data/TGO_1.rds',
                                    # "Tokelau" = 'data/TKL_1.rds',
                                    # "Tonga" = 'data/TON_1.rds',
                                    "Trinidad and Tobago" = 'data/TTO_1.rds',
                                    "Tunisia" = 'data/TUN_1.rds',
                                    "Turkey" = 'data/TUR_1.rds',
                                    # "Turkmenistan" = 'data/TKM_1.rds',
                                    # "Turks and Caicos Islands" = 'data/TCA_1.rds',
                                    # "Tuvalu" = 'data/TUV_1.rds',
                                    "Uganda" = 'data/UGA_1.rds',
                                    "Ukraine" = 'data/UKR_1.rds',
                                    "United Arab Emirates" = 'data/ARE_1.rds',
                                    
                                    # "United States Minor Outlying Islands" = 'data/UMI_1.rds',
                                    "Uruguay" = 'data/URY_1.rds',
                                    "Uzbekistan" = 'data/UZB_1.rds',
                                    # "Vanuatu" = 'data/VUT_1.rds',
                                    "Venezuela" = 'data/VEN_1.rds',
                                    "Vietnam" = 'data/VNM_1.rds',
                                    # "Virgin Islands, U.S." = 'data/VIR_1.rds',
                                    # "Wallis and Futuna" = 'data/WLF_1.rds',
                                    # "Western Sahara" = 'data/ESH_1.rds',
                                    "Yemen" = 'data/YEM_1.rds',
                                    "Zambia" = 'data/ZMB_1.rds',
                                    "Zimbabwe" = 'data/ZWE_1.rds',
                                    'CA Alberta' = 'data/Alberta.rds',
                                    'CA British Columbia' = 'data/British_Columbia.rds',
                                    'CA Manitoba' = 'data/Manitoba.rds',
                                    'CA New Brunswick' = 'data/New_Brunswick.rds',
                                    'CA Newfoundland and Labrador' = 'data/Newfoundland_and_Labrador.rds',
                                    'CA Northwest Territories' = 'data/Northwest_Territories.rds',
                                    'CA Nova Scotia' = 'data/Nova_Scotia.rds',
                                    'CA Ontario' = 'data/Ontario.rds',
                                    'CA Prince Edward Island' = 'data/Prince_Edward_Island.rds',
                                    'CA Quebec' = 'data/Quebec.rds',
                                    'CA Saskatchewan' = 'data/Saskatchewan.rds',
                                    'CA Yukon' = 'data/Yukon.rds',
                                    "US Alabama" = 'data/Alabama.rds',
                                    "US Alaska" = 'data/Alaska.rds',
                                    "US Arizona" = 'data/Arizona.rds',
                                    "US Arkansas" = 'data/Arkansas.rds',
                                    "US California" = 'data/California.rds',
                                    "US Colorado" = 'data/Colorado.rds',
                                    "US Connecticut" = 'data/Connecticut.rds',
                                    "US Delaware" = 'data/Delaware.rds',
                                    "US District of Columbia" = 'data/District_of_Columbia.rds',
                                    "US Florida" = 'data/Florida.rds',
                                    "US Georgia" = 'data/Georgia.rds',
                                    "US Hawaii" = 'data/Hawaii.rds',
                                    "US Idaho" = 'data/Idaho.rds',
                                    "US Illinois" = 'data/Illinois.rds',
                                    "US Indiana" = 'data/Indiana.rds',
                                    "US Iowa" = 'data/Iowa.rds',
                                    "US Kansas" = 'data/Kansas.rds',
                                    "US Kentucky" = 'data/Kentucky.rds',
                                    "US Louisiana" = 'data/Louisiana.rds',
                                    "US Maine" = 'data/Maine.rds',
                                    "US Maryland" = 'data/Maryland.rds',
                                    "US Massachusetts" = 'data/Massachusetts.rds',
                                    "US Michigan" = 'data/Michigan.rds',
                                    "US Minnesota" = 'data/Minnesota.rds',
                                    "US Mississippi" = 'data/Mississippi.rds',
                                    "US Missouri" = 'data/Missouri.rds',
                                    "US Montana" = 'data/Montana.rds',
                                    "US Nebraska" = 'data/Nebraska.rds',
                                    "US Nevada" = 'data/Nevada.rds',
                                    "US New Hampshire" = 'data/New_Hampshire.rds',
                                    "US New Jersey" = 'data/New_Jersey.rds',
                                    "US New Mexico" = 'data/New_Mexico.rds',
                                    "US New York" = 'data/New_York.rds',
                                    "US North Carolina" = 'data/North_Carolina.rds',
                                    "US North Dakota" = 'data/North_Dakota.rds',
                                    "US Ohio" = 'data/Ohio.rds',
                                    "US Oklahoma" = 'data/Oklahoma.rds',
                                    "US Oregon" = 'data/Oregon.rds',
                                    "US Pennsylvania" = 'data/Pennsylvania.rds',
                                    "US Rhode Island" = 'data/Rhode_Island.rds',
                                    "US South Carolina" = 'data/South_Carolina.rds',
                                    "US South Dakota" = 'data/South_Dakota.rds',
                                    "US Tennessee" = 'data/Tennessee.rds',
                                    "US Texas" = 'data/Texas.rds',
                                    "US Utah" = 'data/Utah.rds',
                                    "US Vermont" = 'data/Vermont.rds',
                                    "US Virginia" = 'data/Virginia.rds',
                                    "US Washington" = 'data/Washington.rds',
                                    "US West Virginia" = 'data/West_Virginia.rds',
                                    "US Wisconsin" = 'data/Wisconsin.rds',
                                    "US Wyoming" = 'data/Wyoming.rds'
                        ),
                        selected = area_selected
      )
    } else {
      updateSelectInput(session,
                        inputId = "area",
                        label = "Plot COVID Heatmap by:", 
                        choices = c(
                          #"Municipios" = 'data/COL_2.rds',
                          #"Departamentos" = 'data/COL_1.rds'
                          "World (By Country)" = 'data/World_Countries.rds',
                          "Africa (By Country)" = 'data/Africa.rds', 
                          "Asia (By Country)" = 'data/Asia.rds', 
                          "Europe (By Country)" = 'data/Europe.rds', 
                          "North America (By Country)" = 'data/North_America.rds',
                          "Oceania (By Country)" = 'data/Oceania.rds',
                          "South America (By Country)" = 'data/South_America.rds', 
                          "Australia (By State)" = 'data/AUS_1.rds',
                          "Canada (By Province)" = 'data/CAN_1.rds',
                          "China (By Province)" = 'data/CHN_1.rds',
                          #"United Kingdom (By Country)" = 'data/GBR_1.rds',
                          "United States (By States)" = 'data/USA_1.rds',
                          #"Estonia (By County)" = 'data/EST_1.rds',
                          #"Canada: Level 2" = 'data/CAN_2.rds',
                          
                          #"USA: Level 2" = 'data/USA_2.rds',
                          #"United Kingdom: Level 2" = 'data/GBR_2.rds',
                          #"Australia: Level 2" = 'data/AUS_2.rds',
                          
                          # "Afghanistan" = 'data/AFG_1.rds',
                          # "Akrotiri and Dhekelia" = 'data/XAD_1.rds',
                          # "Ćland" = 'data/ALA_1.rds',
                          # "Albania" = 'data/ALB_1.rds',
                          # "Algeria" = 'data/DZA_1.rds',
                          # "American Samoa" = 'data/ASM_1.rds',
                          # "Andorra" = 'data/AND_1.rds',
                          # "Angola" = 'data/AGO_1.rds',
                          # "Antigua and Barbuda" = 'data/ATG_1.rds',
                          # "Argentina" = 'data/ARG_1.rds',
                          # "Armenia" = 'data/ARM_1.rds',
                          
                          # "Austria" = 'data/AUT_1.rds',
                          # "Azerbaijan" = 'data/AZE_1.rds',
                          # "Bahamas" = 'data/BHS_1.rds',
                          # "Bahrain" = 'data/BHR_1.rds',
                          # "Bangladesh" = 'data/BGD_1.rds',
                          # "Barbados" = 'data/BRB_1.rds',
                          # "Belarus" = 'data/BLR_1.rds',
                          # "Belgium" = 'data/BEL_1.rds',
                          # "Belize" = 'data/BLZ_1.rds',
                          # "Benin" = 'data/BEN_1.rds',
                          # "Bermuda" = 'data/BMU_1.rds',
                          # "Bhutan" = 'data/BTN_1.rds',
                          # "Bolivia" = 'data/BOL_1.rds',
                          # "Bonaire, Saint Eustatius and Saba" = 'data/BES_1.rds',
                          # "Bosnia and Herzegovina" = 'data/BIH_1.rds',
                          # "Botswana" = 'data/BWA_1.rds',
                          # "Brazil" = 'data/BRA_1.rds',
                          # "British Virgin Islands" = 'data/VGB_1.rds',
                          # "Brunei" = 'data/BRN_1.rds',
                          # "Bulgaria" = 'data/BGR_1.rds',
                          # "Burkina Faso" = 'data/BFA_1.rds',
                          # "Burundi" = 'data/BDI_1.rds',
                          # "Cambodia" = 'data/KHM_1.rds',
                          # "Cameroon" = 'data/CMR_1.rds',
                          
                          # "Cape Verde" = 'data/CPV_1.rds',
                          # "Cayman Islands" = 'data/CYM_1.rds',
                          # "Central African Republic" = 'data/CAF_1.rds',
                          # "Chad" = 'data/TCD_1.rds',
                          # "Chile" = 'data/CHL_1.rds',
                          
                          
                          # "Comoros" = 'data/COM_1.rds',
                          # "Costa Rica" = 'data/CRI_1.rds',
                          # "CĆ“te d'Ivoire" = 'data/CIV_1.rds',
                          # "Croatia" = 'data/HRV_1.rds',
                          # "Cuba" = 'data/CUB_1.rds',
                          # "Cyprus" = 'data/CYP_1.rds',
                          # "Czech Republic" = 'data/CZE_1.rds',
                          # "Democratic Republic of the Congo" = 'data/COD_1.rds',
                          # "Denmark" = 'data/DNK_1.rds',
                          # "Djibouti" = 'data/DJI_1.rds',
                          # "Dominica" = 'data/DMA_1.rds',
                          # "Dominican Republic" = 'data/DOM_1.rds',
                          # "Ecuador" = 'data/ECU_1.rds',
                          # "Egypt" = 'data/EGY_1.rds',
                          # "El Salvador" = 'data/SLV_1.rds',
                          # "Equatorial Guinea" = 'data/GNQ_1.rds',
                          # "Eritrea" = 'data/ERI_1.rds',
                          
                          # "Ethiopia" = 'data/ETH_1.rds',
                          # "Faroe Islands" = 'data/FRO_1.rds',
                          # "Fiji" = 'data/FJI_1.rds',
                          # "Finland" = 'data/FIN_1.rds',
                          # "France" = 'data/FRA_1.rds',
                          # "French Guiana" = 'data/GUF_1.rds',
                          # "French Polynesia" = 'data/PYF_1.rds',
                          # "French Southern Territories" = 'data/ATF_1.rds',
                          # "Gabon" = 'data/GAB_1.rds',
                          # "Gambia" = 'data/GMB_1.rds',
                          # "Georgia" = 'data/GEO_1.rds',
                          # "Germany" = 'data/DEU_1.rds',
                          # "Ghana" = 'data/GHA_1.rds',
                          # "Greece" = 'data/GRC_1.rds',
                          # "Greenland" = 'data/GRL_1.rds',
                          # "Grenada" = 'data/GRD_1.rds',
                          # "Guadeloupe" = 'data/GLP_1.rds',
                          # "Guam" = 'data/GUM_1.rds',
                          # "Guatemala" = 'data/GTM_1.rds',
                          # "Guinea-Bissau" = 'data/GNB_1.rds',
                          # "Guinea" = 'data/GIN_1.rds',
                          # "Guyana" = 'data/GUY_1.rds',
                          # "Haiti" = 'data/HTI_1.rds',
                          # "Honduras" = 'data/HND_1.rds',
                          # "Hong Kong" = 'data/HKG_1.rds',
                          # "Hungary" = 'data/HUN_1.rds',
                          # "Iceland" = 'data/ISL_1.rds',
                          # "India" = 'data/IND_1.rds',
                          # "Indonesia" = 'data/IDN_1.rds',
                          # "Iran" = 'data/IRN_1.rds',
                          # "Iraq" = 'data/IRQ_1.rds',
                          # "Ireland" = 'data/IRL_1.rds',
                          # "Israel" = 'data/ISR_1.rds',
                          # "Italy" = 'data/ITA_1.rds',
                          # "Jamaica" = 'data/JAM_1.rds',
                          # "Japan" = 'data/JPN_1.rds',
                          # "Jordan" = 'data/JOR_1.rds',
                          # "Kazakhstan" = 'data/KAZ_1.rds',
                          # "Kenya" = 'data/KEN_1.rds',
                          # "Kosovo" = 'data/XKO_1.rds',
                          # "Kuwait" = 'data/KWT_1.rds',
                          # "Kyrgyzstan" = 'data/KGZ_1.rds',
                          # "Laos" = 'data/LAO_1.rds',
                          # "Latvia" = 'data/LVA_1.rds',
                          # "Lebanon" = 'data/LBN_1.rds',
                          # "Lesotho" = 'data/LSO_1.rds',
                          # "Liberia" = 'data/LBR_1.rds',
                          # "Libya" = 'data/LBY_1.rds',
                          # "Liechtenstein" = 'data/LIE_1.rds',
                          # "Lithuania" = 'data/LTU_1.rds',
                          # "Luxembourg" = 'data/LUX_1.rds',
                          # "Macao" = 'data/MAC_1.rds',
                          # "Macedonia" = 'data/MKD_1.rds',
                          # "Madagascar" = 'data/MDG_1.rds',
                          # "Malawi" = 'data/MWI_1.rds',
                          # "Malaysia" = 'data/MYS_1.rds',
                          # "Mali" = 'data/MLI_1.rds',
                          # "Martinique" = 'data/MTQ_1.rds',
                          # "Mauritania" = 'data/MRT_1.rds',
                          # "Mauritius" = 'data/MUS_1.rds',
                          # "Mayotte" = 'data/MYT_1.rds',
                          # "Mexico" = 'data/MEX_1.rds',
                          # "Micronesia" = 'data/FSM_1.rds',
                          # "Moldova" = 'data/MDA_1.rds',
                          # "Mongolia" = 'data/MNG_1.rds',
                          # "Montenegro" = 'data/MNE_1.rds',
                          # "Montserrat" = 'data/MSR_1.rds',
                          # "Morocco" = 'data/MAR_1.rds',
                          # "Mozambique" = 'data/MOZ_1.rds',
                          # "Myanmar" = 'data/MMR_1.rds',
                          # "Namibia" = 'data/NAM_1.rds',
                          # "Nauru" = 'data/NRU_1.rds',
                          # "Nepal" = 'data/NPL_1.rds',
                          # "Netherlands" = 'data/NLD_1.rds',
                          # "New Caledonia" = 'data/NCL_1.rds',
                          # "New Zealand" = 'data/NZL_1.rds',
                          # "Nicaragua" = 'data/NIC_1.rds',
                          # "Niger" = 'data/NER_1.rds',
                          # "Nigeria" = 'data/NGA_1.rds',
                          # "North Korea" = 'data/PRK_1.rds',
                          # "Northern Cyprus" = 'data/XNC_1.rds',
                          # "Northern Mariana Islands" = 'data/MNP_1.rds',
                          # "Norway" = 'data/NOR_1.rds',
                          # "Oman" = 'data/OMN_1.rds',
                          # "Pakistan" = 'data/PAK_1.rds',
                          # "Palau" = 'data/PLW_1.rds',
                          # "Palestina" = 'data/PSE_1.rds',
                          # "Panama" = 'data/PAN_1.rds',
                          # "Papua New Guinea" = 'data/PNG_1.rds',
                          # "Paraguay" = 'data/PRY_1.rds',
                          # "Peru" = 'data/PER_1.rds',
                          # "Philippines" = 'data/PHL_1.rds',
                          # "Poland" = 'data/POL_1.rds',
                          # "Portugal" = 'data/PRT_1.rds',
                          # "Puerto Rico" = 'data/PRI_1.rds',
                          # "Qatar" = 'data/QAT_1.rds',
                          # "Republic of Congo" = 'data/COG_1.rds',
                          # "Reunion" = 'data/REU_1.rds',
                          # "Romania" = 'data/ROU_1.rds',
                          # "Russia" = 'data/RUS_1.rds',
                          # "Rwanda" = 'data/RWA_1.rds',
                          # "Saint Helena" = 'data/SHN_1.rds',
                          # "Saint Kitts and Nevis" = 'data/KNA_1.rds',
                          # "Saint Lucia" = 'data/LCA_1.rds',
                          # "Saint Pierre and Miquelon" = 'data/SPM_1.rds',
                          # "Saint Vincent and the Grenadines" = 'data/VCT_1.rds',
                          # "Samoa" = 'data/WSM_1.rds',
                          # "San Marino" = 'data/SMR_1.rds',
                          # "Sao Tome and Principe" = 'data/STP_1.rds',
                          # "Saudi Arabia" = 'data/SAU_1.rds',
                          # "Senegal" = 'data/SEN_1.rds',
                          # "Serbia" = 'data/SRB_1.rds',
                          # "Sierra Leone" = 'data/SLE_1.rds',
                          # "Slovakia" = 'data/SVK_1.rds',
                          # "Slovenia" = 'data/SVN_1.rds',
                          # "Solomon Islands" = 'data/SLB_1.rds',
                          # "Somalia" = 'data/SOM_1.rds',
                          # "South Africa" = 'data/ZAF_1.rds',
                          # "South Korea" = 'data/KOR_1.rds',
                          # "South Sudan" = 'data/SSD_1.rds',
                          # "Spain" = 'data/ESP_1.rds',
                          # "Sri Lanka" = 'data/LKA_1.rds',
                          # "Sudan" = 'data/SDN_1.rds',
                          # "Suriname" = 'data/SUR_1.rds',
                          # "Svalbard and Jan Mayen" = 'data/SJM_1.rds',
                          # "Swaziland" = 'data/SWZ_1.rds',
                          # "Sweden" = 'data/SWE_1.rds',
                          # "Switzerland" = 'data/CHE_1.rds',
                          # "Syria" = 'data/SYR_1.rds',
                          # "Taiwan" = 'data/TWN_1.rds',
                          # "Tajikistan" = 'data/TJK_1.rds',
                          # "Tanzania" = 'data/TZA_1.rds',
                          # "Thailand" = 'data/THA_1.rds',
                          # "Timor-Leste" = 'data/TLS_1.rds',
                          # "Togo" = 'data/TGO_1.rds',
                          # "Tokelau" = 'data/TKL_1.rds',
                          # "Tonga" = 'data/TON_1.rds',
                          # "Trinidad and Tobago" = 'data/TTO_1.rds',
                          # "Tunisia" = 'data/TUN_1.rds',
                          # "Turkey" = 'data/TUR_1.rds',
                          # "Turkmenistan" = 'data/TKM_1.rds',
                          # "Turks and Caicos Islands" = 'data/TCA_1.rds',
                          # "Tuvalu" = 'data/TUV_1.rds',
                          # "Uganda" = 'data/UGA_1.rds',
                          # "Ukraine" = 'data/UKR_1.rds',
                          # "United Arab Emirates" = 'data/ARE_1.rds',
                          
                          # "United States Minor Outlying Islands" = 'data/UMI_1.rds',
                          # "Uruguay" = 'data/URY_1.rds',
                          # "Uzbekistan" = 'data/UZB_1.rds',
                          # "Vanuatu" = 'data/VUT_1.rds',
                          # "Venezuela" = 'data/VEN_1.rds',
                          # "Vietnam" = 'data/VNM_1.rds',
                          # "Virgin Islands, U.S." = 'data/VIR_1.rds',
                          # "Wallis and Futuna" = 'data/WLF_1.rds',
                          # "Western Sahara" = 'data/ESH_1.rds',
                          # "Yemen" = 'data/YEM_1.rds',
                          # "Zambia" = 'data/ZMB_1.rds',
                          # "Zimbabwe" = 'data/ZWE_1.rds'
                          "US Alabama" = 'data/Alabama.rds',
                          "US Alaska" = 'data/Alaska.rds',
                          "US Arizona" = 'data/Arizona.rds',
                          "US Arkansas" = 'data/Arkansas.rds',
                          "US California" = 'data/California.rds',
                          "US Colorado" = 'data/Colorado.rds',
                          "US Connecticut" = 'data/Connecticut.rds',
                          "US Delaware" = 'data/Delaware.rds',
                          "US District of Columbia" = 'data/District_of_Columbia.rds',
                          "US Florida" = 'data/Florida.rds',
                          "US Georgia" = 'data/Georgia.rds',
                          "US Hawaii" = 'data/Hawaii.rds',
                          "US Idaho" = 'data/Idaho.rds',
                          "US Illinois" = 'data/Illinois.rds',
                          "US Indiana" = 'data/Indiana.rds',
                          "US Iowa" = 'data/Iowa.rds',
                          "US Kansas" = 'data/Kansas.rds',
                          "US Kentucky" = 'data/Kentucky.rds',
                          "US Louisiana" = 'data/Louisiana.rds',
                          "US Maine" = 'data/Maine.rds',
                          "US Maryland" = 'data/Maryland.rds',
                          "US Massachusetts" = 'data/Massachusetts.rds',
                          "US Michigan" = 'data/Michigan.rds',
                          "US Minnesota" = 'data/Minnesota.rds',
                          "US Mississippi" = 'data/Mississippi.rds',
                          "US Missouri" = 'data/Missouri.rds',
                          "US Montana" = 'data/Montana.rds',
                          "US Nebraska" = 'data/Nebraska.rds',
                          "US Nevada" = 'data/Nevada.rds',
                          "US New Hampshire" = 'data/New_Hampshire.rds',
                          "US New Jersey" = 'data/New_Jersey.rds',
                          "US New Mexico" = 'data/New_Mexico.rds',
                          "US New York" = 'data/New_York.rds',
                          "US North Carolina" = 'data/North_Carolina.rds',
                          "US North Dakota" = 'data/North_Dakota.rds',
                          "US Ohio" = 'data/Ohio.rds',
                          "US Oklahoma" = 'data/Oklahoma.rds',
                          "US Oregon" = 'data/Oregon.rds',
                          "US Pennsylvania" = 'data/Pennsylvania.rds',
                          "US Rhode Island" = 'data/Rhode_Island.rds',
                          "US South Carolina" = 'data/South_Carolina.rds',
                          "US South Dakota" = 'data/South_Dakota.rds',
                          "US Tennessee" = 'data/Tennessee.rds',
                          "US Texas" = 'data/Texas.rds',
                          "US Utah" = 'data/Utah.rds',
                          "US Vermont" = 'data/Vermont.rds',
                          "US Virginia" = 'data/Virginia.rds',
                          "US Washington" = 'data/Washington.rds',
                          "US West Virginia" = 'data/West_Virginia.rds',
                          "US Wisconsin" = 'data/Wisconsin.rds',
                          "US Wyoming" = 'data/Wyoming.rds'
                          
                          
                        ),
                        selected = area_selected)
    }
  })
  # if values$density, values$colours, or values$map is changed update the polygons
  observe({
    log_activity('geomap', 'layer/tab changes')
    get_file()
    if(input$tabSelections == "Heatmap"){
      if (is.null(values$density)) {
        # remove old shapes when map is changed
        leafletProxy("map") %>%
          clearShapes() %>%
          get_tiles() %>%
          get_view()
      }
      else{
        mapData <- get_map_data()
        leafletProxy("map", data =  mapData) %>%
          clearShapes() %>%
          get_shapes() %>%
          get_tiles() %>%
          get_view()
        
      }
    }
  })
  
  # update legend when needed
  observe({
    get_file()
    if (length(values$from) == 1){
      colours <- colorRampPalette(c("#ffffcc", "#b10026"))(length(values$from))
    } else {
      colours <- c(
        "#ffffcc",
        "#ffeda0",
        "#fed976",
        "#feb24c",
        "#fd8d3c",
        "#fc4e2a",
        "#e31a1c",
        "#b10026"
      )
    }
    if(!is.null(values$density) && input$tabSelections == "Heatmap"){
      leafletProxy("map", data = isolate({
        get_map_data()
      })) %>%
        addLegend(
          layerId = "legendLayer",
          position = "bottomright",
          opacity = 0.7,
          colors = colours,
          labels = paste(values$from, "-", values$to),
          title = legend_title
        )#input$legend
    }
  }) 
  
  # get hover location over region
  observe({
    values$highlight <- input$map_shape_mouseover$id
  })
  
  # use values$file to store file info instead of get_file() for editing table
  observe({
    values$file <- get_file()
    if (debug)
      write('observe values$file run',
            file = log_filename,
            append = TRUE)
  })
  
  # populate select input with region names for that map
  observe({
    updateSelectInput(session,
                      'tableNames',
                      choices = tolower(levels(values$map$NAME)),
                      selected = " ")
  })
  
  # if selected row has name that matches a region name make that the default selection for select input
  observe({
    updateSelectInput(session, 'tableNames', selected = values$file[input$table_cell_clicked$row, 1])
  })
  
  # update name in values$file when user clicks submit
  observe({
    input$submitName
    isolate({
      if (!is.null(values$file)) {
        values$file[input$table_cell_clicked$row, 1] <- input$tableNames
      }
    })
  })
  # Implement showing Download csv button only for Table tab
  # Idea from: https://stackoverflow.com/questions/41738927/show-hide-button-on-tab-select-r-shiny
  observeEvent(input$tabSelections, {
    choice = input$tabSelections
    write(paste('Observe tabSelection:',choice),
          file = log_filename,
          append = TRUE)
    if(choice == "Table") {
      write(paste('Observe tabSelection:',choice),
            file = log_filename,
            append = TRUE)
      runjs(
        'document.getElementById("tableDownload").style.visibility = "visible";'
      )
    }
    else {
      runjs(
        'document.getElementById("tableDownload").style.visibility = "hidden";'
      )
    }
  })
  
  #################### HELPER FUNCTIONS ####################
  
  # return the values from the selected column
  # get_nums_col <- function(data_file, col){
  # 	nums_col <- data_file[[col]]
  # 	if(is.null(nums_col)){
  # 		nums_col <- data_file[[2]]
  # 	}
  # 	return(nums_col)
  # }
  get_nums_col <- function(data_file, col) {
    tryCatch({
      if (debug) {
        write('    get_nums_col triggered',
              file = log_filename,
              append = TRUE)
        write(
          paste('    get_nums_col: col:', col, sep = "\t"),
          file = log_filename,
          append = TRUE
        )
        write('    get_nums_col: data_file:',
              file = log_filename,
              append = TRUE)
        write(paste0(data_file),
              file = log_filename,
              append = TRUE)
      }
      
      nums_col <- data_file[[col]]
      #Check if values have python "N/A" symbol and R NA-s, substitute with 0
      if (grepl(pattern = "N/A|NA",x = nums_col) %>% sum() >= 1){
        nums_col <- gsub(pattern = "N/A|NA", replacement = 0, x = nums_col) %>% as.numeric()
      }
      if (debug)
        write(
          paste(
            '    get_nums_col: nums_col after nums_col <- data_file[[col]]:',
            nums_col,
            sep = "\t"
          ),
          file = log_filename,
          append = TRUE
        )
      isolate({
        tab_selected <- input$tabSelections
      })
      if (is.null(nums_col) &&
          tab_selected != "Plots") {
        nums_col <- data_file[[2]]
        col_names <-colnames(data_file)
        #Update the selected column name, if we grab the second column to show instead of missing column
        updateSelectInput(session,
                          inputId = "colSelect",
                          selected = col_names[2])
        if (debug)
          write(
            paste(
              '    get_nums_col: nums_col after nums_col <- data_file[[2]]:',
              nums_col,
              sep = "\t"
            ),
            file = log_filename,
            append = TRUE
          )
      }
      if (debug)
        write(
          '    get_nums_col: completed without exception',
          file = log_filename,
          append = TRUE
        )
      if (debug)
        write(paste0('    nums_col: ', paste0(nums_col)),
              file = log_filename,
              append = TRUE)
      return(nums_col)
    },
    warning = function(warning_condition) {
      write('    get_nums_col: caught warning:',
            file = log_filename,
            append = TRUE)
      write(paste0(warning_condition),
            file = log_filename,
            append = TRUE)
    },
    error = function(err) {
      write('    get_nums_col: caught error:',
            file = log_filename,
            append = TRUE)
      write(paste0(err), file = log_filename, append = TRUE)
      validate(txt = paste(ERR_file_read, dimensions_msg))
    })
  }
  
  # assign density names and values based on the selected column
  get_density <- reactive({
    tryCatch({
      if (debug)
        write('  get_density triggered',
              file = log_filename,
              append = TRUE)
      
      data_file <- values$file
      
      if (debug) {
        write('  get_density: data_file:',
              file = log_filename,
              append = TRUE)
        write(paste0(data_file),
              file = log_filename,
              append = TRUE)
        write('  get_density: calling tolower...',
              file = log_filename,
              append = TRUE)
      }
      
      name_col <- tolower(data_file[[1]])
      
      if (debug) {
        write('  get_density: name_col:',
              file = log_filename,
              append = TRUE)
        write(name_col, file = log_filename, append = TRUE)
        write('  get_density: calling get_nums_col...',
              file = log_filename,
              append = TRUE)
      }
      
      # nums_col contains values in the selected column 
      nums_col <- get_nums_col(data_file, input$colSelect)
      # set legend title
      legend_title <<- "Person count"
      # indicate names of files that contain country-level data to specify per Million adjustment
      # keyNames <- c("Global-Country_", "North_America_", 
      #               "Asia_", "Europe_", "Africa_", "South_America_", "Oceania_", "Canada_", "China_", "United_States_of_America")
      # # adjust per capita number to per Million and adjsut legend title
      # if (grepl(paste(keyNames, collapse = "|"), file_name) & grepl("_per_capita", input$colSelect)){
      #    nums_col <- as.numeric(nums_col) * 1000000
      #    legend_title <<- "Person count (per 1M)"
      # } else if (grepl("_per_capita", input$colSelect)){
         # nums_col <- as.numeric(nums_col) * 1000
         # legend_title <<- "Person count (per 1000)"
      #  }
      if(grepl("_per_capita", input$colSelect)){
        legend_title <<- "Person count (per 100,000)"
        #Check if % change column to update the legend title
      } else if (grepl("_change", input$colSelect)){
        legend_title <<- "% Change"
      }
      
      if (debug)
        write(
          paste('  get_density: nums_col:', nums_col, sep = "\t"),
          file = log_filename,
          append = TRUE
        )
      
      names(nums_col) <- name_col
      
      if (debug)
        write(
          '  get_density: completed without exception',
          file = log_filename,
          append = TRUE
        )
      return(nums_col)
      
    },
    warning = function(warning_condition) {
      write('  get_density: caught warning:',
            file = log_filename,
            append = TRUE)
      write(paste0(warning_condition),
            file = log_filename,
            append = TRUE)
    },
    error = function(err) {
      write('  get_density: caught error:',
            file = log_filename,
            append = TRUE)
      write(paste0(err), file = log_filename, append = TRUE)
      validate(txt = paste(ERR_file_read, dimensions_msg))
    }) 
  }) # End of get_density, which get the number of person
  
  # read file if chooseInput is changed or file is uploaded
  get_file <- reactive({
    log_activity('geomap', 'begin get_file')
    write(
      'get_file part',
      file = log_filename,
      append = TRUE
    )
      date_checked <- input$date
      
    tryCatch({
      
      map_file_name <- input$area
      write(paste('  map-file_name:', map_file_name, sep = "\t"),
            file = log_filename,
            append = TRUE)
      #Retrieve datafile mapping string
      datafile_mapping <- maps_files_to_data_files %>% 
        filter(datafile == input$area) %>% 
        pull(prefix)
      #Extract region name
      region_name <- datafile_mapping %>% 
        paste("_",sep = "")
      write(paste('  region_name:', region_name, sep = "\t"),
            file = log_filename,
            append = TRUE)
      prefix <- paste("../filesystem/",region_name,sep = "")
      write(paste('  prefix:', prefix, sep = "\t"),
            file = log_filename,
            append = TRUE)
      # #Retrieving the datafiles for that region
      # datafile_prefix <- datafile_mapping %>% 
      #   stri_split(regex = "/") %>% 
      #   unlist()
      # path_to_dir <- paste("../filesystem",
      #                      paste(datafile_prefix[1:(length(datafile_prefix)-1)],collapse = "/"),
      #                      sep = "/")
      # 
      # filenames_list <- list.files(path_to_dir)
      # #Select only .txt files
      # filenames_list <- filenames_list[grep(pattern = ".txt", x = filenames_list)]
      # filenames_list <- filenames_list[!grepl(pattern = "accumulated.txt", x = filenames_list)]
      # dates_vec <- NULL
      # for (filename in filenames_list){
      #   file <- read.csv(paste(path_to_dir,filename, sep = "/"), sep = "\t")
      #   col_names <- colnames(file)
      #   #Assumption the file with confirmed data does not have columns with namepart predicted
      #   if ("Predicted_New_Cases" %in% col_names){
      #     next
      #   }
      #   date <- filename %>% 
      #     stri_extract_all(regex = "\\d{4}-\\d{2}-\\d{2}") %>%
      #     unlist() 
      #   dates_vec <- c(dates_vec,date)
      # }
      # oldest_date <- min(dates_vec, na.rm = T)
      # newest_date <- max(dates_vec, na.rm = T)
      # #TODO Hack for ALberta
      # if (date_checked <= as.Date(Sys.time()+21600)){
      #   #
      #   #Check if we do have file with that date
      #     date_checked <- case_when(
      #       date_checked < oldest_date ~ oldest_date %>% as.character(), 
      #       date_checked > newest_date ~ newest_date %>% as.character(), 
      #       TRUE ~ date_checked %>% as.character()
      #   )
      # } else {
      #   date_checked <- date_checked %>% as.character()
      # }
      # 
      # #Update input$date in UI if date has been corrected
      # isolate({
      #   if (date_checked != input$date){
      #     #Update input$date in UI
      #     updateDateInput(session, inputId="date", value = date_checked %>% as.Date())
      #   }
      # })
      
      #Extract date information from string this part maybe obsolete as we have now same format as default format
      date <- date_checked %>% stri_split(regex = "-") %>% unlist()
      write(paste('  date:', date, sep = "\t"),
            file = log_filename,
            append = TRUE)
      datepart <- paste0(date, collapse = "-")
      #Create file name
      file_name <-
        paste(prefix, datepart,".txt", sep = "")
      write(paste('  file_name:', file_name, sep = "\t"),
            file = log_filename,
            append = TRUE)
      #read file
      data_file <- read.csv(file = file_name,
                            sep = "\t",
                            stringsAsFactors = FALSE)
      
      if (grepl("Global-Country_", file_name)) {
        data_file <- data_file[-c((nrow(data_file)-1):nrow(data_file)),]
      }
      
      # region names should be in lower case
      data_file[[1]] <- tolower(data_file[[1]])
      # check if columns have pyhton N/A-s and substitute them with R NA-s
      nr_columns <- length(data_file)
      col_names <- colnames(data_file)
      for (i in 2:nr_columns){
        if (grepl(pattern = "N/A",x = data_file[[i]]) %>% sum() >= 1){
          data_file[[i]] <- gsub(pattern = "N/A", replacement = NA, x = data_file[[i]])
        }
        
        data_file[[i]] <- as.numeric(data_file[[i]])
        #Check if it is not per capita column and round to integers, as we cannot have fraction of people
        if (!grepl("_per_capita", tolower(col_names[i]))){
          data_file[[i]] <- round(data_file[[i]], digits = 0)
        } else {
          data_file[[i]] <- as.numeric(data_file[[i]]) * 100000
          #nums_col <- as.numeric(nums_col) * 100000
        }
      }
      #Sort the rows by region name  
      data_file <- data_file %>% 
        arrange(Name)
      return(data_file)
    },
    error = function(err) {
      # return message if we do not have data for this country
      write(paste("ERROR : ", conditionMessage(err), "\n"), sep = "\t",
            file = log_filename,
            append = TRUE)
      
      return(NULL)
    },
    finally = {
      log_activity('geomap', 'end get_file')
    })
  })
  get_file_for_plot <- function(file_name,
                                area_name,
                                type){
    #Retrieve datafile mapping string
    datafile_mapping <- maps_files_to_data_files %>% 
      filter(datafile == area_name) %>% 
      pull(prefix)
    path_initial <- strsplit(datafile_mapping, split = "/") %>% unlist()
    path_initial[1] <- case_when(
      type == "best_case" ~ "Global_best_case",
      type == "worst_case" ~ "Global_worst_case",
      TRUE ~ "Global",
    )
    path_corrected <- path_initial[-length(path_initial)]
    path_corrected <- paste(path_corrected, collapse = "/")
    file_full_path <- paste("../filesystem", path_corrected,file_name, sep = "/")
    validate(need(file.exists(file_full_path),"Unfortuantely we do not provide bar graphs for that region. Please select another region from menu."))
    data_file <- read.csv(file_full_path, sep = "\t")
    return(data_file)
  }
  
  get_dataframe_for_plotting <- reactive({
    
    actual_col_names <- bar_graphs_mappings$actual
    
    plotted_variable_actual <- input$colSelect
    
    if (!(plotted_variable_actual %in% actual_col_names)){
      plotted_variable_actual <- "Confirmed"
    }
    
    projection_variable <- bar_graphs_mappings %>% 
      filter(actual == plotted_variable_actual) %>% 
      pull(best_worst_projection) %>% 
      as.character()
    
    if (grepl(pattern = "_worst_case", plotted_variable_actual)){
      predicted_data <- get_file_for_plot(file_name = "accumulated.txt",
                                       area_name = input$area,
                                       type = "worst_case") %>% 
        mutate(Date = as.POSIXct(Date))
      
      plot_dataset <- predicted_data %>% 
        dplyr::rename(variable = all_of(projection_variable)) %>% 
        dplyr::mutate(type = "Predicted") %>% 
        dplyr::select(Date,variable, type)
    } else if (grepl(pattern = "_best_case", plotted_variable_actual)){
      predicted_data <- get_file_for_plot(file_name = "accumulated.txt",
                                       area_name = input$area,
                                       type = "best_case") %>% 
        mutate(Date = as.POSIXct(Date))
      plot_dataset <- predicted_data %>% 
        dplyr::rename(variable = all_of(projection_variable)) %>% 
        dplyr::mutate(type = "Predicted") %>% 
        dplyr::select(Date,variable, type)
    } else {
      
      actual_data <- get_file_for_plot(file_name = "accumulated.txt",
                                       area_name = input$area,
                                       type = "normal") %>% 
        mutate(Date = as.POSIXct(Date))
      
      #Generate Confirmed_daily and Deaths Daily
      dates_vec <- actual_data$Date
      daily_confirmed_vec <- c(NA)
      daily_deaths_vec <- c(NA)
      for (i in 2:length(dates_vec)){
        day_data <- actual_data %>% filter(Date == dates_vec[i])
        day_before_data <- actual_data %>% filter(Date == dates_vec[i-1])
        daily_confirmed <- day_data$Confirmed - day_before_data$Confirmed
        daily_confirmed <- max(daily_confirmed,0)
        daily_deaths <- day_data$Deaths - day_before_data$Deaths
        daily_deaths <- max(daily_deaths,0)
        daily_confirmed_vec <- c(daily_confirmed_vec,daily_confirmed)
        daily_deaths_vec <- c(daily_deaths_vec,daily_deaths)
      }
      actual_data <- actual_data %>% 
        dplyr::mutate(Confirmed_daily = daily_confirmed_vec,
                      Deaths_daily = daily_deaths_vec)
      
      predicted_data <- get_file_for_plot(file_name = "predicted.tsv",
                                          area_name = input$area,
                                          type = "normal") %>% 
        mutate(Date = as.POSIXct(Date))
      
      plotted_variable_predicted <- bar_graphs_mappings %>% 
        filter(actual == plotted_variable_actual) %>% 
        pull(predicted) %>% 
        as.character()
      
      time_lower_limit <- actual_data %>% 
        filter(Confirmed > 0) %>% 
        pull(Date) %>% 
        as.character() %>% 
        min()
      
      time_upper_limit <- predicted_data %>% 
        filter(Predicted_Daily_Cases > 0) %>% 
        pull(Date) %>% 
        as.character() %>% 
        max()
      
      plot_dataset <- actual_data %>% 
        dplyr::rename(variable = all_of(plotted_variable_actual)) %>% 
        dplyr::mutate(type = "Actual") %>% 
        bind_rows(predicted_data %>% 
                    dplyr::rename(variable = plotted_variable_predicted) %>% 
                    dplyr::mutate(type = "Predicted")) %>% 
        dplyr::select(Date,variable, type) %>% 
        filter(Date >= time_lower_limit,
               Date <= time_upper_limit)
    }
    plot_dataset <- plot_dataset %>% 
      dplyr::mutate(variable = round(variable, 0))
    return(plot_dataset)
  })
  
  # obtain data from database
  get_db_data <- reactive({
    log_activity('geomap', 'begin get_db_data')
    if (debug)
      write('get_db_data triggered',
            file = log_filename,
            append = TRUE)
    
    tryCatch({
      if (debug) {
        write('credentials:', file = log_filename, append = TRUE)
        write(paste('  dbName:', dbName, sep = "\t"),
              file = log_filename,
              append = TRUE)
        write(paste('  hostName:', hostName, sep = "\t"),
              file = log_filename,
              append = TRUE)
        write(paste('  userName:', userName, sep = "\t"),
              file = log_filename,
              append = TRUE)
        # write(paste('  password:', password, sep="\t"), file=log_filename, append=TRUE)
      }
      conn <- dbConnect(
        drv = RMySQL::MySQL(),
        dbname = dbName,
        host = hostName,
        username = userName,
        password = password
      )
      on.exit(dbDisconnect(conn), add = TRUE)
      data_file <- dbGetQuery(
        conn,
        paste0(
          "select
			   i.GeolocCiudad as region,
			   i.GeolocDepartamento as department,
			   high_score,
			   fever_count,
			   cough_count,
			   difficult_breath_count,
			   fever_cough_count,
			   fever_breath_count,
			   cough_breath_count,
			   fever_cough_breath_count
			   -- Distinct of all the regions available in DB
			   from (select
			         distinct(k.key_region),
			         k.GeolocCiudad,
			         k.GeolocDepartamento
			         from (select
			               GeolocCiudad,
			               GeolocDepartamento,
			               CONCAT(GeolocCiudad,'_',GeolocDepartamento) as key_region
			               from responses) as k
			         ) as i
			   -- Add Severity score counting
			   left join (select
                    key_region,
                    count(*) as high_score
                    from (select
                    			CONCAT(GeolocCiudad,'_',GeolocDepartamento) as key_region,
                    			case when inputFebre = 'febre' then 1 else 0 end as score_fever,
                    			case when inputTos = 'tos' then 1 else 0 end as score_cough,
                    			case when inputDigestivos = 'problemas digestivos' then 1 else 0 end as score_digest,
                    			case when inputRespirar = 'dificultad a respirar' then 3.5 else 0 end as score_breath,
                    			case when optionsContacto = 'si' then 0.5 else 0 end as score_contact
                    			from responses )si
                    where (si.score_fever+si.score_cough+si.score_breath+si.score_digest+si.score_contact) >= 3.5
                    group by key_region) hs on hs.key_region = i.key_region
			   -- Add fever cases
			   left join (select
			         CONCAT(GeolocCiudad,'_',GeolocDepartamento) as key_region,
			         count(inputFebre) as fever_count
			         from responses
			         where inputFebre  = 'febre'
			         group by key_region) f on f.key_region = i.key_region
			   -- Add Cough cases
			   left join (select
			         CONCAT(GeolocCiudad,'_',GeolocDepartamento) as key_region,
			         count(inputTos) as cough_count
			         from responses
          		 where inputTos  = 'tos'
          		 group by key_region) as c on c.key_region = i.key_region
         -- Add Breathing difficulties cases
			   left join (select
			         CONCAT(GeolocCiudad,'_',GeolocDepartamento) as key_region,
			         count(inputRespirar) as difficult_breath_count
			         from responses
          		 where inputRespirar  = 'dificultad a respirar'
          		 group by key_region) r on r.key_region = i.key_region
         -- Add Fever and Cough combination cases
			   left join (select
			         CONCAT(GeolocCiudad,'_',GeolocDepartamento) as key_region,
			         count(*) as fever_cough_count
			         from responses
          		 where inputFebre  = 'febre'
          		 and inputTos  = 'tos'
          		 group by key_region) fc on fc.key_region = i.key_region
         -- Add Fever and Breath difficulties combination cases
			   left join (select
			         CONCAT(GeolocCiudad,'_',GeolocDepartamento) as key_region,
			         count(*) as fever_breath_count
			         from responses
          		 where inputFebre  = 'febre'
          		 and inputRespirar  = 'dificultad a respirar'
          		 group by key_region) fb on fb.key_region = i.key_region
         -- Add Cough and Breath difficulties combination cases
			   left join (select
			         CONCAT(GeolocCiudad,'_',GeolocDepartamento) as key_region,
			         count(*) as cough_breath_count
			         from responses
          		 where inputTos  = 'tos'
          		 and inputRespirar  = 'dificultad a respirar'
          		 group by key_region) cb on cb.key_region = i.key_region
         -- Add Fever, Cough and Breath difficulties combination cases
			   left join (select
			         CONCAT(GeolocCiudad,'_',GeolocDepartamento) as key_region,
			         count(*) as fever_cough_breath_count
			         from responses
          		 where inputTos  = 'tos'
          		 and inputRespirar  = 'dificultad a respirar'
          		 and inputFebre  = 'febre'
          		 group by key_region) fcb on fcb.key_region = i.key_region
        -- Add everything on department level
			  union
			  select
			   i.GeolocDepartamento as region,
			   i.GeolocDepartamento as department,
			   high_score,
			   fever_count,
			   cough_count,
			   difficult_breath_count,
			   fever_cough_count,
			   fever_breath_count,
			   cough_breath_count,
			   fever_cough_breath_count
			   -- Distinct of all the regions available in DB
			   from (select
			         distinct(GeolocDepartamento)
			         from responses) as i
			   left join (select
                    GeolocDepartamento,
                    count(*) as high_score
                    from (select
                    			GeolocDepartamento,
                    			case when inputFebre = 'febre' then 1 else 0 end as score_fever,
                    			case when inputTos = 'tos' then 1 else 0 end as score_cough,
                    			case when inputDigestivos = 'problemas digestivos' then 1 else 0 end as score_digest,
                    			case when inputRespirar = 'dificultad a respirar' then 3.5 else 0 end as score_breath,
                    			case when optionsContacto = 'si' then 0.5 else 0 end as score_contact
                    			from responses )si
                    where (si.score_fever+si.score_cough+si.score_breath+si.score_digest+si.score_contact) >= 3.5
                    group by GeolocDepartamento) as hs on hs.GeolocDepartamento = i.GeolocDepartamento
         left join (select
			         GeolocDepartamento,
			         count(inputFebre) as fever_count
			         from responses
			         where inputFebre  = 'febre'
          		 group by GeolocDepartamento) as f on f.GeolocDepartamento = i.GeolocDepartamento
			   left join (select
			         GeolocDepartamento,
			         count(inputTos) as cough_count
			         from responses
          		 where inputTos  = 'tos'
          		 group by GeolocDepartamento) as c on c.GeolocDepartamento = i.GeolocDepartamento
			   left join (select
			         GeolocDepartamento,
			         count(inputRespirar) as difficult_breath_count
			         from responses
          		 where inputRespirar  = 'dificultad a respirar'
          		 group by GeolocDepartamento) r on r.GeolocDepartamento = i.GeolocDepartamento
         left join (select
			         GeolocDepartamento,
			         count(*) as fever_cough_count
			         from responses
          		 where inputFebre  = 'febre'
          		 and inputTos  = 'tos'
          		 group by GeolocDepartamento) fc on fc.GeolocDepartamento = i.GeolocDepartamento
         left join (select
			         GeolocDepartamento,
			         count(*) as fever_breath_count
			         from responses
          		 where inputFebre  = 'febre'
          		 and inputRespirar  = 'dificultad a respirar'
          		 group by GeolocDepartamento) fb on fb.GeolocDepartamento = i.GeolocDepartamento
         left join (select
			         GeolocDepartamento,
			         count(*) as cough_breath_count
			         from responses
          		 where inputTos  = 'tos'
          		 and inputRespirar  = 'dificultad a respirar'
          		 group by GeolocDepartamento) cb on cb.GeolocDepartamento = i.GeolocDepartamento
    		 left join (select
			         GeolocDepartamento,
			         count(*) as fever_cough_breath_count
			         from responses
          		 where inputTos  = 'tos'
          		 and inputRespirar  = 'dificultad a respirar'
          		 and inputFebre  = 'febre'
          		 group by GeolocDepartamento) fcb on fcb.GeolocDepartamento = i.GeolocDepartamento;
			  "
        )
      )
      #Ouput is "region", "department", "fever_count", "cough_count", "difficult_breath_count", "fever_cough_count",
      #"fever_breath_count", "cough_breath_count", fever_cough_breath_count
      
      if (debug) {
        write(
          'get_db_data: raw data_file after SQL query:',
          file = log_filename,
          append = TRUE
        )
        write(paste0(data_file),
              file = log_filename,
              append = TRUE)
      }
      
      data_file <- data_file %>%
        mutate(row_nr = row_number()) %>%
        group_by(row_nr) %>%
        mutate(department = stringi::stri_trans_general(department, id = "Latin-ASCII")) %>%
        ungroup() %>%
        dplyr::left_join(
          region_names %>%
            dplyr::mutate(department = as.character(department)) %>%
            dplyr::distinct(department, department_abbreviation),
          by = "department"
        ) %>%
        group_by(row_nr) %>%
        mutate(
          region = stringi::stri_trans_general(region, id = "Latin-ASCII"),
          region = dplyr::case_when(
            #the departments in the end of table Assumption no municipality has same name as department
            region == department ~ region,
            is.na(department_abbreviation) ~ paste0(region),
            TRUE ~ paste(region, department_abbreviation, sep = ", ")
          ),
          region = tolower(region),
          fever_count = dplyr::if_else(is.na(fever_count),
                                       0,
                                       fever_count),
          cough_count = dplyr::if_else(is.na(cough_count),
                                       0,
                                       cough_count),
          difficult_breath_count = dplyr::if_else(
            is.na(difficult_breath_count),
            0,
            difficult_breath_count
          ),
          fever_cough_count = dplyr::if_else(is.na(fever_cough_count),
                                             0,
                                             fever_cough_count),
          fever_breath_count = dplyr::if_else(is.na(fever_breath_count),
                                              0,
                                              fever_breath_count),
          cough_breath_count = dplyr::if_else(is.na(cough_breath_count),
                                              0,
                                              cough_breath_count),
          fever_cough_breath_count = dplyr::if_else(
            is.na(fever_cough_breath_count),
            0,
            fever_cough_breath_count
          )
        ) %>%
        ungroup() %>%
        dplyr::select(
          region,
          fever_count,
          cough_count,
          difficult_breath_count,
          fever_cough_count,
          fever_breath_count,
          cough_breath_count,
          fever_cough_breath_count
        )
      
      # region names should be in lower case
      #data_file[[1]] <- tolower(data_file[[1]])
      
      # # update the column selection options when new DB data is loaded
      # updateSelectInput(session, inputId="colSelect", choices = names(data_file)[-1])
      debug = TRUE
      if (debug) {
        write(
          'get_db_data: completed without exception',
          file = log_filename,
          append = TRUE
        )
        write('get_db_data: data_file:',
              file = log_filename,
              append = TRUE)
        write(paste0(data_file),
              file = log_filename,
              append = TRUE)
      }
      return(data_file)
    },
    warning = function(warning_condition) {
      write('get_db_data: caught warning:',
            file = log_filename,
            append = TRUE)
      write(paste0(warning_condition),
            file = log_filename,
            append = TRUE)
      if (debug) {
        write("get_db_data: data_file:",
              file = log_filename,
              append = TRUE)
        write(paste0(data_file),
              file = log_filename,
              append = TRUE)
      }
    },
    error = function(error_condition) {
      write('get_db_data: caught error:',
            file = log_filename,
            append = TRUE)
      write(paste0(error_condition),
            file = log_filename,
            append = TRUE)
      if (debug) {
        write("get_db_data: data_file:",
              file = log_filename,
              append = TRUE)
        write(paste0(data_file),
              file = log_filename,
              append = TRUE)
      }
    },
    finally = {
      log_activity('geomap', 'end get_db_data')
    })
  })
  
  # returns a list of break points given local min/max, global min/max, and # of bins
  get_breaks <- function(rangeMin, rangeMax, min, max, bins) {
    minadd <- FALSE
    maxadd <- FALSE
    
    if (min < rangeMin) {
      bins <- bins - 1
      minadd <- TRUE
    }
    if (max > rangeMax) {
      bins <- bins - 1
      maxadd <- TRUE
    }
    
    densityBreaks <-
      round(seq(rangeMin, rangeMax, length.out = bins), 0)
    
    if (minadd) {
      densityBreaks <- c(min, densityBreaks)
    }
    if (maxadd) {
      densityBreaks <- c(densityBreaks, max)
    }
    if (min == max) {
      densityBreaks <- rangeMin
    }
    densityBreaks
  } # End of get_breaks()
  
  # update colours based on density breaks when value changes
  update_colours <- function(densityBreaks) {
    # Construct break ranges for displaying in the legend
    values$from <- head(densityBreaks, length(densityBreaks) - 1)
    values$to <- tail(densityBreaks, length(densityBreaks) - 1)
    #Change empty elements to densityBreaks values
    if (length(values$from) == 0 && length(values$to) == 0){
      values$from <- densityBreaks
      values$to <- densityBreaks
    }
    
    #Rounding the bin limits values for legend
    mround_bases <- c(0.05,0.5,5,50,500,5000,50000)
    if (grepl("_change", input$colSelect)){
      mround_bases <- c(0.01,0.1,1,10,100,1000,10000)
    }
    for (i in 1:length(values$from)){
      values$from[i] <- case_when(
        # values$from[i] < 0.0000001 ~ mround(values$from[i], base = 0.00000001),
        # values$from[i] < 0.000001 ~ mround(values$from[i], base = 0.0000001),
        # values$from[i] < 0.00001 ~ mround(values$from[i], base = 0.000001),
        # values$from[i] < 0.0001 ~ mround(values$from[i], base = 0.00001),
        # values$from[i] < 0.001 ~ mround(values$from[i], base = 0.0001),
        # values$from[i] < 0.01 ~ mround(values$from[i], base = 0.001),
        abs(values$from[i]) < 0.3 ~ mround(values$from[i], base = mround_bases[1]),
        abs(values$from[i]) <= 3 ~ mround(values$from[i], base = mround_bases[2]),
        abs(values$from[i]) <= 30 ~ mround(values$from[i], base = mround_bases[3]),
        abs(values$from[i]) <= 300 ~ mround(values$from[i], base = mround_bases[4]),
        abs(values$from[i]) <= 3000 ~ mround(values$from[i], base = mround_bases[5]),
        abs(values$from[i]) <= 30000 ~ mround(values$from[i], base = mround_bases[6]),
        TRUE ~ mround(values$from[i], base = mround_bases[7])
        )
      values$to[i] <- case_when(
        # values$to[i] < 0.0000001 ~ mround(values$to[i], base = 0.00000001),
        # values$to[i] < 0.000001 ~ mround(values$to[i], base = 0.0000001),
        # values$to[i] < 0.00001 ~ mround(values$to[i], base = 0.000001),
        # values$to[i] < 0.0001 ~ mround(values$to[i], base = 0.00001),
        # values$to[i] < 0.001 ~ mround(values$to[i], base = 0.0001),
        # values$to[i] < 0.01 ~ mround(values$to[i], base = 0.001),
        abs(values$to[i]) < 0.3 ~ mround(values$to[i], base = mround_bases[1]),
        abs(values$to[i]) <= 3 ~ mround(values$to[i], base = mround_bases[2]),
        abs(values$to[i]) <= 30 ~ mround(values$to[i], base = mround_bases[3]),
        abs(values$to[i]) <= 300 ~ mround(values$to[i], base = mround_bases[4]),
        abs(values$to[i]) <= 3000 ~ mround(values$to[i], base = mround_bases[5]),
        abs(values$to[i]) <= 30000 ~ mround(values$to[i], base = mround_bases[6]),
        TRUE ~ mround(values$to[i], base = mround_bases[7])
      )
    }
    
    # Eight colors for eight buckets
    # if(input$colourScheme == 'red/green'){
    #   values$palette <- colorRampPalette(c("#FF0000", "#000000", "#33FF00"))(8)#input$binNumber
    # }else if(input$colourScheme == 'blue/yellow'){
    #   values$palette <- colorRampPalette(c("#0016DB", "#FFFFFF", "#FFFF00"))(8)#input$binNumber
    # }else if(input$colourScheme == 'grayscale'){
    #   values$palette <- colorRampPalette(c("#000000", "#bdbdbd", "#FFFFFF"))(8)#input$binNumber
    # }else if(input$colourScheme == 'piyg'){
    #   values$palette <- colorRampPalette(c("#C9438C", "#f7f7f7", "#7BC134"))(8)#input$binNumber
    # }else if(input$colourScheme == 'rainbow'){
    #   values$palette <- rainbow(8)#input$binNumber
    # }else if(input$colourScheme == 'topo'){
    #     values$palette <- topo.colors(8)#input$binNumber
    # }else if(input$colourScheme == 'custom'){
    # values$palette <- colorRampPalette(c(input$lowColour, input$highColour))(8)#input$binNumber
    # }
    #https://colorbrewer2.org/#type=sequential&scheme=YlOrRd&n=8
    #Colorblind friendly 8 bins
    values$palette <-
      c(
        "#ffffcc",
        "#ffeda0",
        "#fed976",
        "#feb24c",
        "#fd8d3c",
        "#fc4e2a",
        "#e31a1c",
        "#b10026"
      )
    
    # Assign colors to states
    if (length(values$from) == 1){
      values$palette <- colorRampPalette(c("#ffffcc","#b10026"))(length(values$from))
      values$colours <- structure(rep(values$palette,length(values$density)),names = names(values$density))
    } else {
      values$colours <- structure(values$palette[as.integer(cut(
        values$density,
        densityBreaks,
        include.lowest = TRUE,
        ordered = TRUE
      ))],
      names = names(values$density))
      #Adding the most red colour to countries with higher value than max restricted to large area country
      # Exclude the areas with NA in values$density set them gray
      for (i in 1:length(values$density)){
        if (is.na(values$colours[i])){
          if (is.na(values$density[i])){
            values$colours[i] <- "#d3d3d3"
          } else {
            values$colours[i] <- values$palette[length(values$palette)]
          }
        } else {
          next
        }
      }
    }
  } # End of update_colours() function
  
  # The state names that come back from the maps package's state database has
  # state:qualifier format. This function strips off the qualifier.
  parseRegionName <- function(id) {
    strsplit(id, ":")[[1]][1]
  }
  
  # add fillColour column to a map, depends on values$map and values$colours
  get_map_data <- reactive({
    mapData <- values$map
    
    i <- 1
    fillArray <- rep("#d3d3d3", length(mapData$NAME))
    
    for (region in mapData$NAME) {
      region <- tolower(parseRegionName(region))
      
      tryCatch({
        fillArray[[i]] <- values$colours[[region]]
      },
      error = function(e) {
        
      })
      i <- i + 1
    }
    
    mapData$fillColour <- fillArray
    return(mapData)
  })
  
  # default map
  output$map <- renderLeaflet({
    
    #Error handling
    #Retrieveing the oldest date for available datafile
    datafile_prefix <- maps_files_to_data_files %>%
      filter(datafile == input$area) %>%
      pull(prefix) %>%
      stri_split(regex = "/") %>%
      unlist()
    path_to_dir <- paste("../filesystem/",
                         paste(datafile_prefix[1:(length(datafile_prefix)-1)],collapse = "/"),
                         sep = "/")
    filenames_list <- list.files(path_to_dir)
    dates_vec <- NULL
    for (filename in filenames_list){
      date <- filename %>%
        stri_extract_all(regex = "\\d{4}-\\d{2}-\\d{2}") %>%
        unlist()
      dates_vec <- c(dates_vec,date)
    }
    oldest_date <- min(dates_vec, na.rm = T)
    newest_date <- max(dates_vec, na.rm = T)

    validate(
      #need(input$date <= Sys.Date(), "Your selected date is in the future. Please select correct date") %then% #Error message for dates in the future
      need(input$date >= oldest_date,
           paste("No data available for this region on that date.\nWe can provide data for that region starting from",oldest_date)) %then% #Error message for dates that are too early for particular region
      need(!is.null(get_file()),
           paste("No data available for this region on that date\nWe can provide data for that region starting from",
                 oldest_date,"to",newest_date)) #Error message for data not available
      )
    get_file()
    #Present map
    leaflet(
      #Restricting lowest zoom limit
      options = leafletOptions(minZoom = 2))
  })
  
  
  get_view <- function(m) {
    # calculate average lat and long for the selected area
    lat <- c()
    lon <- c()
    
    lapply(values$map@polygons, function(x) {
      lat <<- c(lat, x@labpt[[1]])
      lon <<- c(lon, x@labpt[[2]])
    })
    latitude_diff <- max(lat)-min(lat)
    longitude_diff <- max(lat)-min(lat)
    zoom <- case_when( # zoom is assigned based on the max of longitude or latitude diff
      max(latitude_diff,longitude_diff*1.5) < 1.8 ~ 9,
      max(latitude_diff,longitude_diff*1.5) < 6 ~ 8,
      max(latitude_diff,longitude_diff*1.5) < 10 ~ 7,
      max(latitude_diff,longitude_diff*1.5) < 12 ~ 6,
      max(latitude_diff,longitude_diff*1.5) < 15 ~ 5,
      max(latitude_diff,longitude_diff*1.5) < 40 ~ 4,
      max(latitude_diff,longitude_diff*1.5) < 250 ~ 3,
      TRUE ~ 2
    )
    setView(m, mean(lat), mean(lon), zoom = zoom)
  }
  
  get_shapes <- function(m) {
    addPolygons(
      m,
      layerId = ~ NAME,
      weight = get_lines(),
      color = "black",
      opacity = 1,
      fillColor = ~ fillColour,
      fillOpacity = get_opacity()
    )
  }
  
  get_lines <- reactive({
    # if(layer_selected("showContours")){
    # 	#input$lineSize
    #   1
    # }
    # else{
    # 	0
    # }
    1
  })
  get_opacity <- reactive({
    # if(layer_selected("showHeatmap")){
    # 	input$fillOpacity
    # }
    # else{
    # 	0
    # }
    input$fillOpacity
  })
  
  # see if a given layer name is shown or hidden by user
  layer_selected <- function(name) {
    if (length(grep(name, input$layers)) > 0) {
      TRUE
    }
    else{
      FALSE
    }
  }
  
  get_tiles <- function(m) {
    # if(layer_selected("showTiles")){
    # 	addTiles(m, options=tileOptions(detectRetina = input$detectRetina))
    # }
    # else{
    # 	clearTiles(m)
    # }
    addProviderTiles(map = m,
                     provider = "Stamen.Terrain",
             #REstrict to one copy of the map of Globe
             options=providerTileOptions(noWrap = TRUE))
  }
  
  ################# OUTPUT FUNCTIONS #################
  output$table <- DT::renderDataTable({
    #Correct region  names
    values$file[[1]] <- capitalize_str(values$file[[1]])
    #Cahnge column names to per 100000, original fiels have per capita, and numeric values are multiplied by 100000 in get_file()
    nr_columns <- length(values$file)
    col_names <- colnames(values$file)
    for (i in 1:nr_columns){
      if (grepl("_per_capita", tolower(col_names[i]))) {
        col_name <- gsub(pattern = "_per_capita", replacement = "_per_100000", x = col_names[i])
        names(values$file)[i] <- col_name
      } else {
        next
      }
    }
    x <- datatable(
      rownames = FALSE,
      values$file,
      selection = 'single',
      class = 'row-border strip hover'
    )
    # colour text red if name doesn't have a match
    if (!is.null(get_unmatched_names())) {
      x <- formatStyle(x, 1, color = get_unmatched_names())
    }
    x
  })
  
  # return list of names in values$file that don't match any region name
  get_unmatched_names <- function() {
    x <-
      values$file[[1]][!values$file[[1]] %in% tolower(levels(values$map$NAME))]
    if (length(x) > 0) {
      styleEqual(x, rep("red", length(x)))
    }
    else{
      NULL
    }
  }
  
  # Dynamically render the box in the upper-right
  output$stateInfo <- renderUI({
    log_activity('geomap', 'stateInfo')
    if (is.null(values$highlight)) {
      return(tags$div("Mouse over a region"))
    }
    else {
      # Get a properly formatted state name
      stateName <- values$highlight
      return(tags$div(tags$strong(stateName),
                      tags$div(
                        if_else(
                          is.na(values$density[tolower(parseRegionName(stateName))]),
                          "No data collected",
                          values$density[tolower(parseRegionName(stateName))] %>% as.character()
                        ),
                        HTML("")
                      )))
    }
  })
  output$plot <- renderPlotly({
    #Functions to enable reverse timeseries:
    c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
      a <- as.trans(a)
      b <- as.trans(b)
      
      name <- paste(a$name, b$name, sep = "-")
      
      trans <- function(x) a$trans(b$trans(x))
      inv <- function(x) b$inverse(a$inverse(x))
      
      trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
      
    }
    
    rev_date <- c_trans("reverse", "time")
    
    plot_dataset <- get_dataframe_for_plotting()
    
    if (grepl(pattern = "per_capita", input$colSelect)){
      plot_dataset <- plot_dataset %>% 
        dplyr::mutate(variable = 100000*variable)
    }
    
    time_lower_limit <- plot_dataset %>% 
      pull(Date) %>% 
      as.character() %>% 
      min()
    
    time_upper_limit <- plot_dataset %>% 
      pull(Date) %>% 
      as.character() %>% 
      max()
    
    label_plotted_variable <- bar_graphs_mappings %>% 
      filter(actual == input$colSelect) %>% 
      pull(axis_label) %>% 
      as.character()
    
    date_breaks <- seq(from =  as.POSIXct(time_lower_limit, tz = "GMT"),
                       to =  as.POSIXct(time_upper_limit, tz = "GMT"),
                       by = "1 day")
    
    date_labels <- seq(from =  as.Date(time_lower_limit),
                       to =  as.Date(time_upper_limit),
                       by = "1 day")
    
    len_data_types <- plot_dataset %>% 
      distinct(type) %>% 
      pull(type) %>% 
      length()
    if (len_data_types < 2){
      colours <- c("#b10026")
    } else {
      colours <- c("#0c2c84","#b10026")
    }

    gray_color <- "#808080"
    font_size <- 14
    axis_text_font_size <- 10
    
    bar_graph <- plot_dataset %>% 
      ggplot(aes(x = Date,
                 y = variable,
                 fill = type)) +
      geom_col(width = 60*60*24*0.5)+ 
      labs(x = "Date",
           y = label_plotted_variable)+
      scale_x_continuous(trans = rev_date,
                         breaks = date_breaks,
                         labels = date_labels)+
      scale_y_continuous(position = "right",
                         sec.axis = sec_axis(~ .,
                                             name = label_plotted_variable,
                                             labels = scales::comma),
                         labels = scales::comma)+
      scale_fill_manual(values = colours)+
      coord_flip()+
      theme(
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title.x = element_text(colour = gray_color,
                                    size = font_size,
                                    hjust = 1),
        axis.title.y = element_text(colour = gray_color,
                                    size = font_size),
        axis.text = element_text(colour = gray_color,
                                 size = axis_text_font_size),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = gray_color,
                                 size = 0.5),
        legend.title = element_blank(),
        legend.position = c(0.95,0.95),
        legend.text = element_text(colour = gray_color,
                                   size = font_size)
        )
    
    secondary_x_axis <- list(
      overlaying = "x",
      side = "top",
      title = label_plotted_variable,
      titlefont = list(family = "Arial",
                       size = font_size*1.4,
                       color = gray_color),
      tickfont = list(family = "Arial",
                      size = axis_text_font_size*1.4,
                      color = gray_color),
      tickformat = ",",
      rangemode = 'tozero',
      showgrid = FALSE,
      linecolor = toRGB(gray_color),
      linewidth = 0.5,
      automargin = TRUE
    )
    
    ggplotly(bar_graph,
             tooltip = c("Date","variable")) %>% 
      add_lines(y~Date, x=~variable, colors=NULL, xaxis="x2", 
                data=plot_dataset, showlegend=FALSE, inherit=FALSE) %>%
      layout(xaxis2 = secondary_x_axis)
    
    
  })
  # output$regionNames <- renderDataTable({
  # 	data.frame("Regions" = levels(values$map$NAME))
  # }, options = list(pageLength = 10))
  
  # save example file
  # output$tableDownload <- downloadHandler(
  #   filename = "table.txt",
  #   content = function(file) {
  #     log_activity('geomap', 'tableDownload')
  #     write.table(
  #       values$file,
  #       sep = "\t",
  #       quote = FALSE,
  #       file = file,
  #       row.names = FALSE
  #     )
  #   }
  # )
  #   }
  # )
  # 
  # # save leaflet png page
  # output$geomap <- downloadHandler(
  #   filename = paste0( Sys.Date()
  #                      , "_customGeomap"
  #                      , ".png"
  #   ),
  #   content = function(file) {
  #     # mapshot() from mapview package to save the image as png
  #     mapshot( x = get_shapes(leaflet(data = get_map_data())) %>% get_tiles()
  #              %>% get_view()
  #              %>% addLegend(layerId = "legendLayer", position = "bottomright", 
  #                            opacity = 0.7, colors = values$palette, labels = paste(values$from, "-", values$to),
  #                            title = legend_title)
  #              , file = file
  #              , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
  #              , selfcontained = TRUE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
  #     )# end of mapshot()
  #     log_activity('geomap', 'geomap')
  #   } # end of content function
  # )# end of downloadHandler
})