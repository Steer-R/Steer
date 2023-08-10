#' Get information from the American Community Survey associated with a spatial element
#'
#' `get_data()` download a dataset of information from the American Community Survey, using an API connection to
#' the US Census Bureau. This dataset may include a geometry column, allowing to create a spatial feature object
#' with the downloaded variables
#'
#' @param search (optional) The name of a county to search specific information. If ommited, the information will be for all
#'   Puerto Rico.
#' @param survey The ACS survey to be searched. The ACS contains one-year, three-year, and five-year surveys expressed as
#'   "acs1", "acs3", and "acs5".
#' @param year The year, or endyear, of the ACS sample. As now, 5-year ACS data is available from 2009 through 2021; 1-year
#'   ACS data is available from 2005 through 2021, except for 2020.
#' @param acs_variables The code of the variables to be searched in the acs.
#' @param geography The geography scale to retrieve the information. refer to
#'   \href{https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus}{Tidycensus geographies} to see the available gegraphies.
#' @param path description
#' @param file_type description
#' @param CRS description
#' @param format description
#' @param moe_level description
#' @param api_key key
#' @param geometry geometry
#' @param state state
#' @param name description
#'
#' @keywords geocode Colombia address.
#' @return The coordinates of the address given, and the match score.
#' @seealso [tidycensus::get_acs()] which this function wraps
#' @examples
#' variables <- c(
#'   "DP05_0001E", "S1701_C03_001E", "B18101_001E", "B18101_004E", "B18101_007E", "B18101_010E",
#'   "B18101_013E", "B18101_016E", "B18101_019E", "B18101_023E", "B18101_026E", "B18101_029E",
#'   "B18101_032E", "B18101_035E", "B18101_038E"
#' )
#'
#' info <- get_data(
#'   acs_variables = variables
#' )
#' @export

get_data <- function(search, survey, year, acs_variables, geography, api_key, geometry, path, file_type, CRS, format,
                     moe_level, name, state) {
  #   ____________________________________________________________________________
  #   Import of required packages                                             ####

  require(tidycensus)
  require(dplyr)
  require(sf)
  require(snakecase)

  census_api_key(key = "0a5aa2b38e08c2ecd82a4e3c9a1a27f7942b3cd8")

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
    variables = variables, # Variables de búsqueda
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
      relocate(geometry, .before = 1)
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
        "There was already a file with the same name in the path provided, the previous file was be replaced.",
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
