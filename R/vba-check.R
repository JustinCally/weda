#' This function standardises species names in a dataframe
#'
#' @param recordTable A dataframe containing species names
#' @param format The format of the species names in the dataframe
#' @param speciesCol The column name of the species names in the dataframe
#' @param return_data Whether to return data or just verbose of name conversions
#'
#' @return A dataframe with standardised species names
#'
#' @export
#' @examples
#' \dontrun{
#' standardise_species_names(recordTable = recordTable,
#'                           format = "scientific",
#'                           speciesCol = "Species")
#'                           }
standardise_species_names <- function(recordTable,
                                      format = c("scientific", "common"),
                                      speciesCol = "Species",
                                      return_data = TRUE) {

  # Create a variable name for the format
  if(!methods::hasArg(format) & !(format %in%  c("scientific", "common"))) {
  # Create a variable name for the other format
    stop("Please provide the format to the names: scientific or common")
  }
  # Get the unique species names in the dataframe

  var_name <- paste0(format, "_name")
  # Get the conversions from the vba_name_conversions dataframe
  other_name <- paste0(setdiff(c("scientific", "common"), format), "_name")

  un <- unique(recordTable[[speciesCol]])

  conversions <- vba_name_conversions %>%
    dplyr::filter(!!rlang::sym(var_name) %in% un)

  # Get the species names that are not in the conversions dataframe
  which_not_in <- un[which(!(un %in% conversions[[var_name]]))]
  # If there are species names that are not in the conversions dataframe
  if(length(which_not_in > 0)) {
    warning("No match found for ",
            paste(which_not_in, collapse = ", "),
            ". Please provide names within the VBA taxa list")
  }



  conversions_grouped <- conversions %>%
    dplyr::group_by(!!rlang::sym(var_name)) %>%
    dplyr::summarise(dplyr::across(!!rlang::sym(other_name), .fns = ~ paste(.x, collapse = "/")),
                     n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(name = dplyr::case_when(n == 1 ~ "v",
                                   n > 1 ~ "!"))

  if(max(conversions_grouped$n) > 1) {
    warning("Taxa matching cannot be conducted at a 1:1 ratio, grouping ",
            other_name,
            " to all possible options")
  }


  conv_text <- paste(conversions_grouped[[var_name]], " -> ",
                     conversions_grouped[[other_name]]) %>%
    `names<-`(conversions_grouped$name)

  which_not_in_cli <- which_not_in %>%
    `names<-`(rep("x", length(which_not_in)))

  cli::cli({
    cli::cli_bullets(c(conv_text,
                       which_not_in_cli))
  })

  names(recordTable)[which(names(recordTable) == speciesCol)] <- var_name

  # Join data
  final_data <- recordTable %>%
    dplyr::left_join(conversions_grouped %>%
                       dplyr::select(scientific_name, common_name),
                     by = c(var_name)) %>%
    dplyr::ungroup()

  if(return_data) {
  return(final_data)
  }

}

