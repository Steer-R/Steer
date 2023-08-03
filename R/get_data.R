#' geocode in Colombia
#'
#' This function allows you to retrieve information from the US Census Bureau
#' @param search The dataframe containing the information.
#' @param survey The CRS for geocoding the addresses.
#' @param year The name of the column with the id for keeping track of your data.
#' @param acs_variables The name of the column with the addresses for geocoding.
#' @param geography The name of the column with the city of the address.
#' @param path description
#' @param file_type description
#' @param CRS description
#' @param format description
#' @param moe_level description
#' @param name description
#' @keywords geocode Colombia address.
#' @return The coordinates of the address given, and the match score.
#' @examples
#' variables <- c("DP05_0001E", "S1701_C03_001E", "B18101_001E", "B18101_004E", "B18101_007E", "B18101_010E",
#'                "B18101_013E", "B18101_016E", "B18101_019E", "B18101_023E", "B18101_026E", "B18101_029E",
#'                "B18101_032E", "B18101_035E", "B18101_038E")
#'
#'info <- get_data(
#'   acs_variables = variables
#'   )
#' @export

get_data <- function(search, survey, year, acs_variables, geography, api_key, geometry, path, file_type, CRS, format,
                     moe_level, name){

  #   ____________________________________________________________________________
  #   Import of required packages                                             ####

  require(tidycensus)
  require(tidyverse)
  require(sf)
  require(mapview)
  require(snakecase)

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

  if (missing(path)){
    warning("There is no path specified, the dataset will be available locally")
  }

  if (missing(name)){
    name <- "data_pr"
  }

  if (!missing(path)){
    if (!dir.exists(path)){
      stop("The provided path does not exist")
    }
  }

  #   ____________________________________________________________________________
  #   Load of the labels dataframes                                           ####

  labels_bc <- load_variables(
    year,  # Last year of the ACS survey of interest
    survey # ACS to retrieve
  )

  labels_s <- load_variables(
    year,                      # Last year of the ACS survey of interest
    paste0(survey,"/subject")  # # ACS to retrieve, filtering for subject tables
  )

  labels_p <- load_variables(
    year,                      # Last year of the ACS survey of interest
    paste0(survey,"/profile")  # # ACS to retrieve, filtering for profile tables
  )

  labels_cp <- load_variables(
    year,                       # Last year of the ACS survey of interest
    paste0(survey,"/cprofile")  # # ACS to retrieve, filtering for cprofile tables
  )

  #   ____________________________________________________________________________
  #   Survey data gathering                                            ####

  data <- get_acs(
    county = search,       # Condado de búsqueda
    state = "PR",          # Estado de búsqueda (PR = Puerto Rico)
    geography = geography, # Mínima geografía de búsqeuda
    survey = survey,       # Encuesta de búsuqeda (American Comunity Survey 5 years)
    variables = variables, # Variables de búsqueda
    year = year,           # Último año de la encuesta
    geometry = geometry,   # Inclusión de la variable espacial
    output = format,
    moe_level = moe_level
  )

  #   ____________________________________________________________________________
  #   Database depuration                                                     ####

  if(format == "tidy"){
    data_dep <- data %>%
      separate(NAME, into = c("tract", "municipio", "estado"), sep = ", ") %>%
      mutate(tract = str_remove(tract, "Census Tract "),
             municipio = str_remove(municipio, " Municipio")) %>%
      left_join(labels_bc, by = c("variable" = "name")) %>%
      left_join(labels_s, by = c("variable" = "name")) %>%
      left_join(labels_p, by = c("variable" = "name")) %>%
      left_join(labels_cp, by = c("variable" = "name")) %>%
      select(-geography) %>%
      unite(label, starts_with("label"), na.rm = T) %>%
      unite(concept, starts_with("concept"), na.rm = T) %>%
      mutate(label = str_remove(label, "Estimate!!Total:!!"),
             label = str_replace(label, "!!", " ")) %>%
      mutate(across(c(label, concept),
                    ~ snakecase::to_snake_case(.))) %>%
      mutate(column = paste0(concept, "_", label)) %>%
      select(-c(variable, moe, label, concept)) %>%
      pivot_wider(names_from = "column", values_from = "estimate") %>%
      relocate(geometry, .before = 1)
  } else {
    data_dep <- data
  }

  #   ____________________________________________________________________________
  #   File saving                                                             ####

  if(!missing(path)){

    file_path <- paste0(path, name, ".", file_type)

    if(file.exists(file_path)){
      st_write(data, file_path, delete_layer = T)
      message(str_wrap(paste0(
        "There was already a file with the same name in the path provided, the previous file will be replaced.",
        "The dataset was succesfully exported to the specified path: ",
        path,
        name,
        ".", file_type
      )))
    } else {
      st_write(data, file_path)
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
