
####### Standardising location names ###########

#########################################################################################

library(openxlsx)
library(data.table)


## example data sets:

setwd("C:/Hanno/Bioinvasion/GenericFunctions/R")
# setwd("/scratch/home/hseebens/Bioinvasion/InvAccu")

# dataset <- data.frame(regions=c("Alaska", "United States", "Gran Canaria", "Fiji island", "Hessen"), test=c(13,2,1,2,5))
# path_to_table <- file.path("..", "Data")
# column_name <- "regions"

# introdat <- read.table("Data/IntroData_raw_21Aug2023.csv",sep=";",stringsAsFactors=F,header=T)
dataset <- fread(file.path("..","..","InvAccu","Data","IntroData_raw_10Oct2024.csv"), encoding="Latin-1")#[1:10000,]
column_name <- "Country"


StandardiseLocationNames <- function(dataset=NULL,
                                     column_name=NULL,
                                     path_to_table=NULL){

  if (is.null(dataset)){
    stop("dataset missing without a default.")
  }
  if (is.null(path_to_location_table)){
    stop("path_to_table missing without a default.")
  }
  if (is.null(column_name)){
    stop("column_name missing without a default.")
  }
  
  ## load location tables #################################################
  
  # load table with countries (i.e. regions)
  regions <- read.xlsx(file.path(path_to_table, "AllLocations.xlsx"), sheet = 2, na.strings = "") # Sheet with first aggregation level (i.e. countries)
  regions <- regions[, c("locationID", "location", "location_var")]
  regions$location_var <- tolower(regions$location_var)  # Set all to lowercase for matching
  regions$location_lower <- tolower(regions$location)  # Set all to lowercase for matching
  
  # load table with state, provinces, departments, etc... (i.e. subregions)
  subregions <- read.xlsx(file.path(path_to_table, "AllLocations.xlsx"), sheet = 3, na.strings = "") # Sheet with second aggregation level (i.e. states, provinces...)
  subregions <- subregions[, c("locationID", "location", "location_var", "gadm1_name", "gadm1_var")]
  subregions$gadm1_var <- tolower(subregions$gadm1_var)  # Set all to lowercase for matching
  subregions$gadm1_lower <- tolower(subregions$gadm1_name)  # Set all to lowercase for matching

  
  ### WHY ?????????  
  # Get duplicated names of subregions  
  # dup <- unique(gsub("\\s*\\(.*?\\)", "", subregions$gadm1_name)[duplicated(gsub("\\s*\\(.*?\\)", "", subregions$gadm1_name))])
  

  # Prepare for matching with regions
  loc_match <- dataset ## use another dat set for region matching to keep the original names
  colnames(loc_match)[colnames(loc_match)==column_name] <- "verbatimLocation" # set focal column to standard name
  loc_match$location <- loc_match$verbatimLocation # column 'location' used for the final set of standardised names
  
  loc_match$order <- 1:nrow(loc_match) # order of rows for restoring original order later
  
  loc_match$location <- gsub("\\xa0|\\xc2", " ", loc_match$location)  # Replace special spaces
  loc_match$location <- gsub("^\\s+|\\s+$", "", loc_match$location)  # Trim leading/trailing whitespace
  loc_match$location <- gsub(" \\(the\\)", "", loc_match$location)  # Remove " (the)"
  
  
  ## some initial changes to address frequent and easy-to-fix issues
  ind_china <- grep("\\bChina\\b", loc_match$location) # \\b ensures matches of whole words only (to avoid e.g. Australian etc.)
  loc_match[ind_china,]$location <- regions$location[regions$location=="China"]

  ind_australia <- grep("\\bAustralia\\b", loc_match$location)
  loc_match[ind_australia,]$location <- regions$location[regions$location=="Australia"]

  ind_australia <- grep("\\bCanada\\b", loc_match$location)
  loc_match[ind_australia,]$location <- regions$location[regions$location=="Australia"]

  ## remove unnecessary additions to names
  loc_match$location <- gsub(" (mainland)", "", loc_match$location, fixed=T)
  loc_match$location <- gsub("??", "", loc_match$location, fixed=T)

  
  ## start matching with AllLocations.xlsx #########################################################
  
  loc_match$location_lower <- tolower(loc_match$location)  # lowercase names for internal matching

  loc_match_regions$location_lower[is.na(loc_match_regions$location_lower)] <- ""
  
  ## Step 1: Match names of 'loc_match' with location names of 'regions' 
  loc_match_regions <- merge(loc_match, regions[, c("location_lower", "locationID")], by = "location_lower", all.x = TRUE)


  ## Step 2: Match based on keywords in 'regions' - after "location_var" column
  ind_keys_regions <- which(!is.na(regions$location_var)) # check only entries with available variations of names
  for (j in ind_keys_regions) {  # loop over regions with available variations 
    location_var <- unlist(strsplit(regions$location_var[j], "; ")) # check if multiple country name variations provided
    for (k in location_var) { # loop over each variation of that name
      ind_match <- which(loc_match_regions$location_lower == k) # check whether a match can be found in the dataset file
      
      if (length(ind_match)==0) next # no match found -> move on
      
      # if (length(unique(regions$location[j])) > 1) { # match not unique
      #   cat(paste0("Warning: ", k, " matches multiple location names. Refine location_var!"))
      # }
      if (length(ind_match)>1){
        loc_match_regions$location[ind_match] <- regions$location[j]
        loc_match_regions$locationID[ind_match] <- regions$locationID[j]
      }
    }
  }

  ## code introduces NA and "" which should be set to "" to allow easier control of matching 
  loc_match_regions$location_lower[is.na(loc_match_regions$location_lower)] <- ""

    
  ## Step 3: Match based on 'subregions' - gadm1 names
  
  # still missing region matches
  ind_missing <- which(is.na(loc_match_regions$locationID) & loc_match_regions$location_lower!="") # check only still missing matches 
  mismatches <- unique(loc_match_regions[ind_missing,]$location_lower)
  
  ## exact match of GADM1 and original name
  for (j in 1:length(mismatches)){
    ind_match <- which(subregions$gadm1_lower==mismatches[j])
    
    if (length(ind_match)>0){
      loc_match_regions$location[loc_match_regions$location_lower==mismatches[j]] <- subregions[ind_match,]$location
      loc_match_regions$locationID[loc_match_regions$location_lower==mismatches[j]] <- subregions[ind_match,]$locationID
    }
  }


  
  ## Step 4: Check combinations of country and state names
  
  # still missing region matches
  ind_missing <- which(is.na(loc_match_regions$locationID) & loc_match_regions$location_lower!="") # check only still missing matches 
  mismatches <- unique(loc_match_regions[ind_missing,]$location_lower)
  
  country_states <- tolower(paste(subregions$location, subregions$gadm1_name))
  
  ind_match <- match(country_states, mismatches)
  ind_match <- ind_match[!is.na(ind_match)]
  if (length(ind_match)>0){
    for (j in 1:length(ind_match)){
      ind_state <- which(mismatches[ind_match[j]] == country_states)
      if (length(ind_state)>0){
        loc_match_regions$location[loc_match_regions$location_lower==mismatches[ind_match[j]]] <- subregions[country_states==mismatches[ind_match[j]],]$location
        loc_match_regions$locationID[loc_match_regions$location_lower==mismatches[ind_match[j]]] <- subregions[country_states==mismatches[ind_match[j]],]$locationID
      }
    }
  }
  

  
  table(is.na(loc_match_regions$locationID))
  sort(table(loc_match_regions$location_lower[is.na(loc_match_regions$locationID)]))
  
  
  


  #   
  #   ## final merging of both data sets with standardized region names to original data
  #   dat_match1 <- dat_match1[order(dat_match1$order),]
  #   if (!identical(dat_match1$taxon_orig,dat$taxon_orig)) stop("Data sets not sorted equally!")
  #   
  #   dat$locationID <- dat_match1$locationID
  #   dat$location <- dat_match1$location
  #   dat$stateProvince <- dat_match1$gadm1_name
  #   
  #   # dat_regnames <- dat
  #   
  #   # ## Remove duplicated entries
  #   # dat_regnames <- dat_regnames[!duplicated(dat_regnames), ]
  #   # write_regnames <- dat_regnames
  #   # 
  #   # # Clean locations and locationID 
  #   # write_regnames <- write_regnames |> 
  #   #   select(-c(stateProvince, locationID)) |> 
  #   #   left_join(regions |> select(location, locationID), by = "location")
  # 
  #   
  #   ## output ###############################################################################
  #   
  #   # Output: Save the file with standardized location names
  #   write.table(write_regnames,file.path("Output","Intermediate",paste0("Step3_StandardLocationNames_",FileInfo[i,"Dataset_brief_name"],".csv")),row.names=F)
  #   
  #   # Check and export missing locations
  #   missing <- dat_regnames$location_orig[is.na(dat_regnames$locationID)]
  #   if (length(missing) > 0) {
  #     write.table(sort(unique(missing)),file.path("Output","Check",paste0("Missing_Locations_",FileInfo[i,"Dataset_brief_name"],".csv")),row.names = F,col.names=F)
  #   }
  # }
  # 
  # ## Post-processing: Aggregate and export changed location names
  # if (nrow(dat_regnames)>0){ # avoid step when no region names have changed
  #   reg_names <- vector()
  #   for (i in 1:length(inputfiles)){
  #     dat <- read.table(file.path("Output","Intermediate",paste0("Step3_StandardLocationNames_",FileInfo[i,"Dataset_brief_name"],".csv")),stringsAsFactors = F,header=T)
  #     reg_names <- rbind(reg_names,cbind(dat[,c("location","location_orig")],FileInfo[i,1]))
  #   }
  #   reg_names <- reg_names[reg_names$location!=reg_names$location_orig,] # export only region names deviating from the original
  #   reg_names <- unique(reg_names[order(reg_names$location),])
  #   colnames(reg_names) <- c("location","location_orig","origDB")
  #   
  #   # Clean locations and locationID 
  #   reg_names  <- reg_names |> left_join(regions |> select(location, locationID), by = "location")
  #   
  #   write.table(reg_names,file.path("Output","Translated_LocationNames.csv"),row.names=F)
  # }
}
