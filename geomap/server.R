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

source("../global_server.R")
source("../global_ui.R") # so we can see EXAMPLE_FILES
source("../config.R") # load DB connection details
# Data for region mapping
region_names <- read.csv("../department_municipality_name.csv",
                         header = T,
                         sep = ",")

# Data to map between country names in data files and map file names
maps_files_to_data_files <- read.csv("tools/map_name_to_data_name.csv",
                                     header = T,
                                     sep = ",",
                                     col.names = c("datafile","prefix"),
                                     colClasses = c("character","character"))

# Constants
dimensions_msg <- "Input data can have up to 50 data columns."

# Logging & debugging
log_filename = tryCatch({
  paste(system("hostname", intern = TRUE), 'log.txt', sep = "_")
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
    if (debug)
      write('observe input$file run',
            file = log_filename,
            append = TRUE)
  })
  
  # when a valid column is selected set values$density
  observe({
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
        if (debug) {
          write(values$density, file = log_filename, append = TRUE)
          write(paste('applying max...', sep = "\t"),
                file = log_filename,
                append = TRUE)
        }
        max <- ceiling(max(values$density, na.rm = TRUE))
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
          if (is.finite(min) && is.finite(max)) {
            update_colours(round(seq(min, max, length.out = 8 + 1), num_digits))#input$binNumber
            if (debug)
              write(
                'observe input$colSelect: update_colours returned',
                file = log_filename,
                append = TRUE
              )
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
    log_activity('geomap',
                 'observe rangeSubmit binNumber lowColour highColour colourScheme')
    isolate({
      if (!is.null(values$density)) {
        rangeMin <- 0
        rangeMax <- 100
        
        min <- floor(min(values$density, na.rm = TRUE))
        max <- ceiling(max(values$density, na.rm = TRUE))
        
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
    if(input$tabSelections == "Interactive"){
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
    if(!is.null(values$density) && input$tabSelections == "Interactive"){
      leafletProxy("map", data = isolate({
        get_map_data()
      })) %>%
        addLegend(
          layerId = "legendLayer",
          position = "bottomright",
          opacity = 0.7,
          colors = colorRampPalette(c("#ffffcc", "#b10026"))(length(values$from)),
          labels = paste(values$from, "-", values$to),
          title = 'Legend'
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
      
      nums_col <- get_nums_col(data_file, input$colSelect)
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
  })
  
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
      region_name <- maps_files_to_data_files %>% 
        filter(datafile == map_file_name) %>% 
        pull(prefix) %>% 
        paste("_",sep = "")
      write(paste('  region_name:', region_name, sep = "\t"),
            file = log_filename,
            append = TRUE)
      prefix <- paste("data/",region_name,sep = "")
      write(paste('  prefix:', prefix, sep = "\t"),
            file = log_filename,
            append = TRUE)
      date <- input$date %>% stri_split(regex = "-") %>% unlist()
      write(paste('  date:', date, sep = "\t"),
            file = log_filename,
            append = TRUE)
      datepart <- paste(date[2], date[3], date[1], sep = "-")
      file_name <-
        paste(prefix, datepart,".txt", sep = "")
      write(paste('  file_name:', file_name, sep = "\t"),
            file = log_filename,
            append = TRUE)
      
      data_file <- read.csv(file = file_name,
                            sep = "\t")
      
      # region names should be in lower case
      data_file[[1]] <- tolower(data_file[[1]])
      
      # update the column selection options when a new file is uploaded
      #updateSelectInput(session, inputId="colSelect", choices = names(data_file)[-1])
      
      return(data_file)
    },
    error = function(err) {
      # return message if we do not have data for this country
      validate(need(
        !file.exists(file_name),
        "No data available for this country on that level"
      ))
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
    
    densityBreaks
  }
  
  # update colours based on density breaks when value changes
  update_colours <- function(densityBreaks) {
    # Construct break ranges for displaying in the legend
    values$from <- head(densityBreaks, length(densityBreaks) - 1)
    values$to <- tail(densityBreaks, length(densityBreaks) - 1)
    
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
    values$colours <- structure(values$palette[as.integer(cut(
      values$density,
      densityBreaks,
      include.lowest = TRUE,
      ordered = TRUE
    ))],
    names = names(values$density))
  }
  
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
    get_file()
    leaflet()
  })
  
  
  get_view <- function(m) {
    # calculate average lat and long for the selected area
    lat <- c()
    lon <- c()
    
    lapply(values$map@polygons, function(x) {
      lat <<- c(lat, x@labpt[[1]])
      lon <<- c(lon, x@labpt[[2]])
    })
    
    setView(m, mean(lat), mean(lon), zoom = 3)
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
    addTiles(m)
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
  output$tableDownload <- downloadHandler(
    filename = "table.txt",
    content = function(file) {
      log_activity('geomap', 'tableDownload')
      write.table(
        values$file,
        sep = "\t",
        quote = FALSE,
        file = file,
        row.names = FALSE
      )
    }
  )
  
  # save leaflet html page
  output$plotDownload <- downloadHandler(
    filename = function() {
      "geomap.html"
    },
    content = function(file) {
      log_activity('geomap', 'plotDownload')
      m <- get_shapes(leaflet(data = get_map_data())) %>% get_tiles()
      #m <- leaflet()
      saveWidget(m, file = file)
    }
  )
  
})