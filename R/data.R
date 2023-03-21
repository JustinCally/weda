#' Name conversions between scientific and common names
#'
#' A dataset containing the scientific names and common names of taxa in the VBA
#'
#' \itemize{
#'   \item scientific_name. The scientific name in the VBA
#'   \item common_name. The common name in the VBA
#' }
#'
#' @docType data
#' @keywords datasets
#' @name vba_name_conversions
#' @usage data(vba_name_conversions)
#' @format A data frame with 38,587 rows and 2 variables
NULL


#' Data Dictionary
#'
#' A data dictionary of the database
#'
#' \itemize{
#'   \item schema. Schema name
#'   \item table_name. Name of the table
#'   \item table_description. Description of the purpose of the table
#'   \item table_type. Level of table processing (raw, curated or processed)
#'   \item column_name. Name of the column/field
#'   \item column_class. Class of the column as per R conventions
#'   \item column_description. Description of the column purpose
#' }
#'
#' @docType data
#' @keywords datasets
#' @name data_dictionary
#' @usage data(data_dictionary)
#' @format A data frame with 131 rows and 7 variables
NULL
