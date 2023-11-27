#' Get information from the American Community Survey API
#'
#' `us_census_data()` download a dataset of information from the American Community Survey, using an API connection to
#' the US Census Bureau. This dataset may include a geometry column, allowing to create a spatial feature object
#' with the downloaded variables.
#'
#' @param search (optional) The name of a county to search specific information. If omitted, the information will be for all
#'   the state.
#' @param survey The ACS survey to be searched. The ACS contains one-year, three-year, and five-year surveys expressed as
#'   "acs1", "acs3", and "acs5".Default to "acs5"
#' @param year The year, or endyear, of the ACS sample. As now, 5-year ACS data is available from 2009 through 2021; 1-year
#'   ACS data is available from 2005 through 2021, except for 2020. If no year is provided, the last year with available
#'   information will be used
#' @param acs_variables The code of the variables to be searched in the acs as strings. This input may accept a single value
#'  or a vector variable with multiple inputs (i.e., "DP05_0001E" or c("DP05_0001E", "S1701_C03_001E")). To see the available
#'  variables for the acs5 2021, refer to:
#'  \href{https://api.census.gov/data/2021/acs/acs5/variables.html}{List of variables for 2021}, for other years,
#'  please search in \hfer{https://www.census.gov/data/developers/data-sets.html}{US Census Bureau Available APÏs}.
#' @param geography The geography scale to retrieve the information. refer to
#'   \href{https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus}{Tidycensus geographies}
#'   to see the available gegraphies.
#' @param path The path where the resultant dataset will be stored. If omitted, the dataframe will be available only
#'  locally
#' @param file_type (string) The extension used to save the dataset. It can be a spatial object (i.e., "shp", "gpkg", "gdb")
#' or a text object (i.e., "csv", "txt"). The string must not include the intial dot. Default to "gpkg".
#' @param CRS (numeric) The four digit EPSG code for the Coordinate Reference System of the spatial object output.
#' If not sure about which is the desired EPSG code, refer to \href{https://epsg.org/search/by-name}{EPSG codes} to see
#' a list of all availabel codes an their description. Default to North American Datu of 1983 (NAD83), EPSG: 4269.
#' @param format One of "tidy" (the default) in which each row represents an enumeration unit-variable combination, or "wide"
#'  in which each row represents an enumeration unit and the variables are in the columns.
#' @param moe_level The confidence level of the returned margin of error. One of 90 (the default), 95, or 99.
#' @param api_key The API key provided to you from the Census formated in quotes. A key can be acquired at
#' \href{http://api.census.gov/data/key_signup.html}{US Census Bureau API Key}.
#' @param geometry (logical) if FALSE (the default), return a regular tibble of ACS data. if TRUE, uses the tigris package
#'  to return
#' an sf tibble with simple feature geometry in the 'geometry' column.
#' @param state An optional vector of states for which you are requesting data. State names, postal codes, and FIPS codes are
#'  accepted. Defaults to "PR"
#' @param name The name of the resultant file. Default to "data_pr"
#'
#' @return
#' If geometry is `TRUE`, the output will be a spatial dataframe, otherwise it will be a dataframe.
#'
#' Aditionally, if format is tidy, the output will be a dataset with the following conditions:
#'
#' * Columns: Each of the variables used as input
#' * Rows: The observation for each variable and each geographical division specified in geography
#' * Geometry columns: If geometry imput is TRUE, a column with the coordinates for the geometry will be included
#'
#' Please note that, if a path is provided, the resultant dataset will be stored in the desired path, instead of being
#' stored in the global environment. Otherwise, the dataset will be available locally.
#'
#' If format is long, the output will be a dataset where each row will be the value for the variable and geography
#' combination. All values will be stored in the column "value", and there will be a column "name" with the corresponding
#' variable name.
#'
#' @seealso [tidycensus::get_acs()] which this function wraps
#'
#' @examples
#' variables <- c(
#'   "DP05_0001E", "S1701_C03_001E", "B18101_001E", "B18101_004E", "B18101_007E", "B18101_010E",
#'   "B18101_013E", "B18101_016E", "B18101_019E", "B18101_023E", "B18101_026E", "B18101_029E",
#'   "B18101_032E", "B18101_035E", "B18101_038E"
#' )
#'
#' info <- us_census_data(
#'   acs_variables = variables
#' )
#' @export

us_census_data <- function(search, survey, year, acs_variables, geography, api_key, geometry, path, file_type, CRS, format,
                     moe_level, name, state) {
  #   ____________________________________________________________________________
  #   Import of required packages                                             ####

  require(tidycensus)
  require(tidyverse)
  require(sf)
  require(snakecase)

  census_api_key(key = "0a5aa2b38e08c2ecd82a4e3c9a1a27f7942b3cd8")


  #   ____________________________________________________________________________
  #   Error handling from the inputs                                          ####

  if (missing(acs_variables)){
    stop(strwrap("At least one variable is required to retrieve the information from the ACS"))
  }

  #   ____________________________________________________________________________
  #   Default inputs assign                                                   ####

  ## ACS type of survey

  if (missing(survey)) {
    survey <- "acs5"
  }

  ## Last year of the ACS survey

  if (missing(year)) {
    year <- 2021
  }

  ## Level of geography to retrieve

  if (missing(geography)) {
    geography <- "tract"
  }

  ## Logical. If the output must be a spatial object

  if (missing(geometry)) {
    geometry <- T
  }

  ## File type to export

  if (missing(file_type)) {
    file_type <- "gpkg"
  }

  ## Format of the output dataframe. One of "tidy" (long) or "wide"

  if (missing(format)) {
    format <- "tidy"
  }

  if (missing(moe_level)) {
    moe_level <- 90
  }

  if (missing(search)) {
    search <- NULL
  }

  if (missing(path)) {
    warning("There is no path specified, the dataset will be available locally")
  }

  if (missing(name)) {
    name <- "data_pr"
  }

  if (!missing(path)) {
    if (!dir.exists(path)) {
      stop("The provided path does not exist")
    }
  }

  if (missing(state)) {
    state <- "PR"
  }

  if (!missing(path)) {
    if(!str_ends(path, "/")){
      path <- paste0(path, "/")
    }
  }

  #   ____________________________________________________________________________
  #   Load of the labels dataframes                                           ####

  labels_bc <- load_variables(
    year, # Last year of the ACS survey of interest
    survey # ACS to retrieve
  )

  labels_s <- load_variables(
    year, # Last year of the ACS survey of interest
    paste0(survey, "/subject") # # ACS to retrieve, filtering for subject tables
  )

  labels_p <- load_variables(
    year, # Last year of the ACS survey of interest
    paste0(survey, "/profile") # # ACS to retrieve, filtering for profile tables
  )

  labels_cp <- load_variables(
    year, # Last year of the ACS survey of interest
    paste0(survey, "/cprofile") # # ACS to retrieve, filtering for cprofile tables
  )

  #   ____________________________________________________________________________
  #   Survey data gathering                                            ####

  data <- get_acs(
    county = search, # Condado de búsqueda
    state = state, # Estado de búsqueda (PR = Puerto Rico)
    geography = geography, # Mínima geografía de búsqeuda
    survey = survey, # Encuesta de búsuqeda (American Comunity Survey 5 years)
    variables = acs_variables, # Variables de búsqueda
    year = year, # Último año de la encuesta
    geometry = geometry, # Inclusión de la variable espacial
    output = format,
    moe_level = moe_level
  )

  #   ____________________________________________________________________________
  #   Database depuration                                                     ####

  if (format == "tidy") {
    data_dep <- data %>%
      separate(NAME, into = c("tract", "municipio", "estado"), sep = ", ") %>%
      mutate(
        tract = str_remove(tract, "Census Tract "),
        municipio = str_remove(municipio, " Municipio")
      ) %>%
      left_join(labels_bc, by = c("variable" = "name")) %>%
      left_join(labels_s, by = c("variable" = "name")) %>%
      left_join(labels_p, by = c("variable" = "name")) %>%
      left_join(labels_cp, by = c("variable" = "name")) %>%
      select(-geography) %>%
      unite(label, starts_with("label"), na.rm = T) %>%
      unite(concept, starts_with("concept"), na.rm = T) %>%
      mutate(
        label = str_remove(label, "Estimate!!Total:!!"),
        label = str_replace(label, "!!", " ")
      ) %>%
      mutate(across(
        c(label, concept),
        ~ snakecase::to_snake_case(.)
      )) %>%
      mutate(column = paste0(concept, "_", label)) %>%
      select(-c(variable, moe, label, concept)) %>%
      pivot_wider(names_from = "column", values_from = "estimate") %>%
      relocate(geometry, .before = 1) %>%
      filter(!st_is_empty(.))
  } else {
    data_dep <- data
  }

  #   ____________________________________________________________________________
  #   File saving                                                             ####

  if (!missing(path)) {
    file_path <- paste0(path, name, ".", file_type)

    if (file.exists(file_path)) {
      st_write(data_dep, file_path, delete_layer = T)
      message(str_wrap(paste0(
        "There was already a file with the same name in the path provided, the previous file will be replaced.",
        "The dataset was succesfully exported to the specified path: ",
        path,
        name,
        ".", file_type
      )))
    } else {
      st_write(data_dep, file_path)
      message(str_wrap(paste0(
        "The dataset was succesfully exported to the specified path: ",
        path,
        name,
        ".", file_type
      )))
    }
  } else {
    return(data_dep)
  }
}
