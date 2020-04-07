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
#library(mapview)
#library(webshot)

source("../global_server.R")
source("../global_ui.R") # so we can see EXAMPLE_FILES
source("../config.R") # load DB connection details
`%then%` <- shiny:::`%OR%` #function to have several validation for error handling
# Data for region mapping----
region_names <- read.csv("../department_municipality_name.csv",
                         header = T,
                         sep = ",")

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
# Logging & debugging
date_part <- Sys.time() %>% 
  as.character() %>% 
  strsplit(split = " ") %>% 
  unlist() %>% 
  paste(collapse = "_")
log_filename = tryCatch({
  paste(system("hostname", intern = TRUE),
        #Add date to log file
        #date_part,
        'log.txt', sep = "_")
}, error = function(e) {
  'log.txt'
  #paste(date_part,'log.txt', sep = "_")
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
    if(!is.null(values$density) && input$tabSelections == "Heatmap"){
      leafletProxy("map", data = isolate({
        get_map_data()
      })) %>%
        addLegend(
          layerId = "legendLayer",
          position = "bottomright",
          opacity = 0.7,
          colors = colorRampPalette(c("#ffffcc", "#b10026"))(length(values$from)),
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
      #Check if values have python "N/A" symbol, substitute with 0
      if (grepl(pattern = "N/A",x = nums_col) %>% sum() >= 1){
        nums_col <- gsub(pattern = "N/A", replacement = 0, x = nums_col)
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
      if (is.null(nums_col)) {
        nums_col <- data_file[[2]]
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
      # update the column name when "Per capita" radio button is selected 
      #col_name <- input$colSelect
       # if (input$radio == "per_capita"){
       # col_name <- paste(col_name,input$radio, sep = "_")
       # }
      
      
      # nums_col contains values in the selected column 
      nums_col <- get_nums_col(data_file, input$colSelect)
      #Check if it is not per capita column and round to integers, as we cannot have fraction of people
      if (!grepl("_per_capita", input$colSelect)){
        nums_col <- round(nums_col, digits = 0)
      }
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
        nums_col <- as.numeric(nums_col) * 100000
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
      #Retrieveing the the datafiles for that region
      datafile_prefix <- datafile_mapping %>% 
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
      
      #Check if we do have file with that date
      date_checked <- case_when(
        input$date < oldest_date ~ oldest_date %>% as.character(),
        input$date > newest_date ~ newest_date %>% as.character(),
        TRUE ~ input$date %>% as.character()
      )
      #Update input$date in UI if date has been corrected
      isolate({
        if (date_checked != input$date){
          #Update input$date in UI
          updateDateInput(session, inputId="date", value = date_checked %>% as.Date())
        }
      })
      
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
      
      # update the column selection options when a new file is uploaded
      #updateSelectInput(session, inputId="colSelect", choices = names(data_file)[-1])
      
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
    mround_bases <- c(0.1,5,50,500,5000,50000)
    if (grepl("_change", input$colSelect)){
      mround_bases <- c(0.1,1,10,100,1000,10000)
    }
    for (i in 1:length(values$from)){
      values$from[i] <- case_when(
        # values$from[i] < 0.0000001 ~ mround(values$from[i], base = 0.00000001),
        # values$from[i] < 0.000001 ~ mround(values$from[i], base = 0.0000001),
        # values$from[i] < 0.00001 ~ mround(values$from[i], base = 0.000001),
        # values$from[i] < 0.0001 ~ mround(values$from[i], base = 0.00001),
        # values$from[i] < 0.001 ~ mround(values$from[i], base = 0.0001),
        # values$from[i] < 0.01 ~ mround(values$from[i], base = 0.001),
        # values$from[i] < 0.1 ~ mround(values$from[i], base = 0.01),
        abs(values$from[i]) <= 1 ~ mround(values$from[i], base = mround_bases[1]),
        abs(values$from[i]) <= 30 ~ mround(values$from[i], base = mround_bases[2]),
        abs(values$from[i]) <= 300 ~ mround(values$from[i], base = mround_bases[3]),
        abs(values$from[i]) <= 3000 ~ mround(values$from[i], base = mround_bases[4]),
        abs(values$from[i]) <= 30000 ~ mround(values$from[i], base = mround_bases[5]),
        TRUE ~ mround(values$from[i], base = mround_bases[6])
        )
      values$to[i] <- case_when(
        # values$to[i] < 0.0000001 ~ mround(values$to[i], base = 0.00000001),
        # values$to[i] < 0.000001 ~ mround(values$to[i], base = 0.0000001),
        # values$to[i] < 0.00001 ~ mround(values$to[i], base = 0.000001),
        # values$to[i] < 0.0001 ~ mround(values$to[i], base = 0.00001),
        # values$to[i] < 0.001 ~ mround(values$to[i], base = 0.0001),
        # values$to[i] < 0.01 ~ mround(values$to[i], base = 0.001),
        # values$to[i] < 0.1 ~ mround(values$to[i], base = 0.01),
        abs(values$to[i]) <= 1 ~ mround(values$to[i], base = mround_bases[1]),
        abs(values$to[i]) <= 30 ~ mround(values$to[i], base = mround_bases[2]),
        abs(values$to[i]) <= 300 ~ mround(values$to[i], base = mround_bases[3]),
        abs(values$to[i]) <= 3000 ~ mround(values$to[i], base = mround_bases[4]),
        abs(values$to[i]) <= 30000 ~ mround(values$to[i], base = mround_bases[5]),
        TRUE ~ mround(values$to[i], base = mround_bases[6])
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
    }
  } # End of get_colours() function
  
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
    # datafile_prefix <- maps_files_to_data_files %>% 
    #   filter(datafile == input$area) %>% 
    #   pull(prefix) %>% 
    #   stri_split(regex = "/") %>% 
    #   unlist()
    # path_to_dir <- paste("../filesystem/",
    #                      paste(datafile_prefix[1:(length(datafile_prefix)-1)],collapse = "/"),
    #                      sep = "/")
    # filenames_list <- list.files(path_to_dir)
    # dates_vec <- NULL
    # for (filename in filenames_list){
    #   date <- filename %>% 
    #     stri_extract_all(regex = "\\d{4}-\\d{2}-\\d{2}") %>%
    #     unlist() 
    #   dates_vec <- c(dates_vec,date)
    # } 
    # oldest_date <- min(dates_vec, na.rm = T)
    # newest_date <- max(dates_vec, na.rm = T)

    # validate(
    #   need(input$date <= Sys.Date(), "Your selected date is in the future. Please select correct date") %then% #Error message for dates in the future
    #   need(input$date >= oldest_date, 
    #        paste("No data available for this region on that date.\nWe can provide data for that region starting from",oldest_date)) %then% #Error message for dates that are too early for particular region
    #   need(!is.null(get_file()), 
    #        paste("No data available for this region on that date\nWe can provide data for that region starting from",
    #              oldest_date,"to",newest_date)) #Error message for data not available
    #   )
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
    addTiles(m,
             #REstrict to one copy of the map of Globe
             options=providerTileOptions(noWrap = TRUE))
  }
  
  ################# OUTPUT FUNCTIONS #################
  output$table <- DT::renderDataTable({
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