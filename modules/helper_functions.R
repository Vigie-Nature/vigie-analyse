#' filter dataset from analysis report and update the select input
#'
#' @param analysis_history reactive values containing all information about the objects and objects themself
#' @param input_to_update vector of the input to update
#'
#' @export
#'
#' @examples
#' filter_and_update_datasets()
filter_and_update_datasets <- function(analysis_history, input_to_update, session, ns){
  datasets_names <- names(analysis_history)
  datasets_names_keep <- rep(TRUE, length(datasets_names))
  if(length(datasets_names) > 1) {

    cat("  update dataset list\n")
    for (i in seq_along(datasets_names)){
      datasets_names_keep[i] <- ifelse(analysis_history[[datasets_names[i]]][["type"]] != "dataset", FALSE, TRUE)
    }
    datasets_names <- datasets_names[datasets_names_keep]
    for (i in seq_along(input_to_update)){
      updateSelectInput(session = session,
                        inputId = ns(input_to_update[i]),
                        choices = datasets_names)
    }
  }
}
