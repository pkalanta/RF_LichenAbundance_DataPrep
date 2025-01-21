## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "RF_LichenAbundance_DataPrep",
  description = "Preprare data from POE for Lichen Abundance model using RF algorithm",
  authors = person("Parvin Kalantari", email = "parvin.kalantari.1@ulaval.ca"),
  keywords = character(0),
  childModules = character(0),
  version = list(RF_LichenAbundance_DataPrep ="1.0.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "RF_LichenAbundance_DataPrep.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.0)", "bit64", "raster", "ggplot2", "googledrive", "reproducible"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    # expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA),
    expectsInput("lichen_data_csv", "data.frame", "Environmental data"),
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "Lichens", objectClass = "data.frame", desc = "Environmental data"),
    createsOutput(objectName = "Predictors", objectClass = "data.frame", desc = "covariates")
  )
))
#Event Function
doEvent.RF_LichenAbundance_DataPrep <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    # Initialization event
    init = {
      sim <- Init(sim)
     },
    
    # Error handling for unknown event types
    warning(paste("Undefined event type:", eventType))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
  browser()
  # lichen_data-gpkg= prepInputs(url = "https://drive.google.com/file/d/1kdlKO3H8K52mL1-d37FsIZcaJMKUpCn2/view?usp=drive_link",
  #fun = sf::st_read) |> Cache(),
  response <- "REC_LICHEN"  # We are predicting REC_LICHEN
  abundance <- response
  features <- c("LATITUDE", "LONGITUDE", "ALTITUDE", "PC_PENT","drainage", "ageori", "essence")
  predictors <- features # Covariates
  Lichens <- sim$lichen_data_csv
  Lichens$drainage <- as.factor(Lichens$drainage)
  Lichens$ageori <- as.factor(Lichens$ageori)
  Lichens$essence <- as.factor(Lichens$essence) #essence =type of forest
  Lichens$REC_LICHEN <- as.numeric(Lichens$REC_LICHEN)
  Lichens$LATITUDE <- as.numeric(Lichens$LATITUDE)
  Lichens$LONGITUDE <- as.numeric(Lichens$LONGITUDE)
  Lichens$ALTITUDE <- as.numeric(Lichens$ALTITUDE)
  Lichens$PC_PENT <- as.numeric(Lichens$PC_PENT)
  Lichens$Presence<- as.numeric(Lichens$Presence)
  sim$Lichens <- Lichens
  # # Split the data into train and test sets
  # set.seed(123)#,
  #Create a training and testing split 75/25
  split = {
    origSeed <- .Random.seed
    set.seed(123)
    split <- caTools::sample.split(Lichens$ID_POE, SplitRatio = 0.75)
    set.seed(origSeed)
    split
  }
  sim$train_data = Lichens[split, ]    # 75% for training
  sim$test_data  = Lichens[!split, ]   # 25% for testing
  
  lichen_data_sf = sf::st_as_sf(sim$Lichens,
                                coords = c("LONGITUDE", "LATITUDE"),
                                crs = 4326) # CRS set to WGS84
  sim$studyArea = sf::st_convex_hull(st_union(lichen_data_sf))
  
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(inputPath(sim), 1)
  
  if (!suppliedElsewhere("lichen_data_csv", sim)) {
    sim$lichen_data_csv <- prepInputs(url = "https://drive.google.com/file/d/1xBRTq0GYgD8CAJgsoDCV77JMUOXHZP5H/view?usp=drive_link",
                                      fun = data.table::fread, 
                                      destinationPath = dPath) |> Cache()
  }
  return(invisible(sim))
}



