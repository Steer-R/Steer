#' Get roads from a state of the US, from TIGER API
#'
#' `get_sp_data()` download a spatial dataset, using an API connection to TIGER/Line Shapefiles. This dataset include a
#' spatial feature object with the downloaded variables. The dataset may be stored locally in the R session, or saved
#' directly to a specified path.
#'
#' @param year The year of the spatial object, according to the US Census Bureau
#' @param state A character vector of the two-digit FIPS code of the state you'd like to download the roads for. Can also
#'  be state name or abbreviation (case-insensitive). Default to "PR"
#' @param unified (Logical) `TRUE` if the resultant spatial object must be a unique dataset with all road types (with a type
#' identifier column). `False` if a separate dataset should be returned for each road type.
#' @param path The path where the resultant dataset will be stored. If omitted, the dataframe will be available only
#'  locally
#' @param name The name of the resultant file. Default to "data_pr"
#' @param file_type (string) The extension used to save the dataset. It can be a spatial object (i.e., "shp", "gpkg", "gdb")
#' or a text object (i.e., "csv", "txt"). The string must not include the intial dot. Default to "gpkg".
#' @param county (Optional) The county to retrieve the spatial feature. If `NULL`, roads from all counties will be returned.
#'  Default to `NULL`
#'
#' @return
#' If unified is `TRUE`, the output will be a spatial dataframe with the roads of the county or counties of the desired
#' state, each road will be classified as Primary, Secondary, Minor, Ramp, or Other, in the column type. If unified is
#'  `FALSE`, the output will be five spatial dataframes, one for each road type.
#'
#' @seealso
#' * [tigris::roads()] which this function wraps
#' * \href{https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2020/TGRSHP2020_TechDoc.pdf}{TIGER/Line Shapefiles
#' Techinal Documentation (2020)}
#' * The classification by road type is made using the MAF/TIGER Feature Class Code (MTFCC, see
#'  \href{https://www.census.gov/library/reference/code-lists/mt-feature-class-codes.html}{MAF/TIGER Feature Class Code
#'  Definitions}). Each road type has an associated MTFCC, which for this case, is translated to a string indicating the
#'  road type associated.
#' @export
#'
#' @examples
#' vias <- get_sp_data()

get_sp_data <- function(year, state, unified, path, name, file_type, county) {
  require(tidyverse)
  require(tigris)
  require(sf)

  #   ____________________________________________________________________________
  #   Handling of missing variables                                           ####

  ## state: assumes as default Puerto Rico as "PR"
  if (missing(state)){
    state <- "PR"
  }

  ## unified: (logical) if T, the output is a dataframe with all roads in the state. Otherwise, the output will be
  ## a separate file for each road type
  if (missing(unified)){
    unified <- T
  }

  ## file_type: If no file_type is specified, the default option is assigned as geopackage (gpkg)
  if (missing(file_type)){
    file_type <- "gpkg"
  }

  ## name: If no name is specified, the default option is assigned as data_sp_pr
  if (missing(name)){
    name <- "data_sp_pr"
  }

  ## path: If path is provided but does not exists, maybe due to misspelling, the function stops and shows an error message
  if (!missing(path)) {
    if (!file.exists(path)) {
      stop(str_wrap("The path does not exists, please check if you misspelled the path and try again"))
    }
  }

  #   ____________________________________________________________________________
  #   Counties                                                                ####

  ## Search of the state counties based on the year input, to retrieve the roads for all the counties at once.
  ## If year is missing, uses as first input the actual year

  if (missing(year)) {

    ## Gets the actual year
    year <- lubridate::year(now())

    capture.output(
      suppressMessages(
        ## Tries to retrieve the information for all counties in the actual year. If it is not found, tries with the later
        ## year
        while (is(try(vias_año <- roads(state = state, county = "San Juan", year = year, progress_bar = F), silent = TRUE), "try-error")) {
          year <- year - 1
        }
      )
    )

    counties <- counties(state = state, year = year)

    ## Prints a message clarifying that year was not provided as input, and returns the last year with available
    ## information used as input for the next stages
    message(str_wrap(paste0("Since no year was provided, retrieving data for the year ", year, " which is the
                            latest year with available information.")))
  } else {
    suppressWarnings(
      ## If the year was provided, the counties for that year are retrieved
      if(is(try(counties <- counties(state = state, year = year, progress_bar = T), silent = T), "try-error")){
        stop(str_wrap(paste0("There is no available information for the selected year (", year, "), please try again with a previous year ")))
      }
    )
  }

  #   ____________________________________________________________________________
  #   Roads                                                                   ####

  ## Request the roads for the state or county using a connection to the TIGER API
  suppressMessages(
    ## If the county is provided, only the roads of the county are retrieved
    if(!missing(county)){
      vias <- invisible(roads(state = state, county = county, year = year, progress_bar = F)) ## Retrieve the roads
    } else {
      ## If the county was not provided, the roads for all the state are retrieved
      vias <- invisible(map_dfr(
        counties %>% pull(COUNTYFP),                                       ## Get the names of all the counties in the state
        ~ roads(state = state, county = .x, year = year, progress_bar = F) ## Retrieve the roads
      ))
    }
  )

  #   ____________________________________________________________________________
  #   Database cleaning                                                       ####

  ## Creation of a columng with the type of road according to the MTFCC code from the Census Bureau
  vias_df <- vias %>%
    mutate(type = case_when(
      MTFCC == "S1100" ~ "primary",             # Primary roads for the code MTFCC S1100
      MTFCC == "S1200" ~ "secondary",           # Secondary roads for the code MTFCC S1200
      MTFCC %in% c("S1400", "S1500") ~ "minor", # Minor roads for the codes MTFCC S1400 and MTFCC 1500
      MTFCC == "S1630" ~ "ramps",               # Ramps for the code MTFCC S1630
      T ~ "other"                               # Other roads for the remaining codes
    )) %>%
    relocate(geometry, .after = last_col())     # Assigning geometry column at last position

  ## If unified is false, the roads dataframe will be splited by categories, creating one dataframe for each road type
  if(unified == F){
    vias_list <- vias_df %>%
      group_by(type) %>%               # Grouping the dataframe by road type
      {
        group_keys(.) %>%
          pull() ->> list_names        # Definition of the road types names to be used as list element names
        .
      } %>%
      group_split() %>%                # Creation of the individual dataframe for each road type
      set_names(list_names)            # Assigning of list element name, according to the road type
    rm(list_names, envir = .GlobalEnv) # Removing of the object with the road types for naming the road list elements
  }

  #   ____________________________________________________________________________
  #   File saving                                                             ####

  if (missing(path)){
    ## If the path is missing as input, the output will only be available locally, and a message is printed
    message(str_wrap("Since no path was provided as input, the output will be available only locally. If you want to save
                       the output, please provide a path and run again the function"))
    if (unified ==  T){
      ## Return the dataframe with the roads for the geometry specified
      return(vias_df)
    } else {
      ## Return one dataframe per road type for the geometry specified, stored as a list
      return(vias_list)
    }
  } else {

    date <- str_sub(str_remove_all(ymd(today()), "-"), 3)
    if(!str_ends(path, "/")){
      path <- paste0(path, "/")
    }
    file_path <- paste0(path, date, "_", name, ".", file_type)
    if (unified == T){

      if (file.exists(file_path)) {
        message(str_wrap(paste0(
          "There was already a file with the same name in the path provided, the previous file will be replaced."
        )))
      }
      st_write(vias_df, file_path, delete_layer = T)
      message(str_wrap(paste0(
        "The dataset was succesfully exported to the specified path: ", path, name, ".", file_type
      )))
    } else {
      Map(function(df, road_type){
        file_path <- paste0(path, date, "_", name, "_", road_type, ".", file_type)
        if (file.exists(file_path)) {
          message(str_wrap(paste0(
            "There was already a file with the same name in the path provided, the previous file will be replaced."
          )))}
        # invisible(
        df %>%
          st_write(paste0(path, date, "_", name, "_", road_type, ".", file_type), delete_layer = T)
        # )
      },
      lista, lista %>% names
      )
      # not unified
    }
  }
}
