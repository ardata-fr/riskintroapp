
#' @importFrom workspace new_workspace store_dataset pack_workspace
#' @importFrom purrr iwalk
save_workspace = function(file, datasets, settings) {

  ws <- workspace::new_workspace()

  # save settings -----
  ws <- workspace::store_yaml(ws, list = settings, filename = "settings", subdir = "assets")

  # save datasets -----
  datasets <- nullify(datasets)
  purrr::iwalk(
    datasets,
    function(x, i){
      ws <- workspace::store_dataset(ws, x, i)
    }
  )
  file_location <- pack_workspace(x = ws, file = file)
  file_location
}

#' @importFrom workspace unpack_workspace list_object_in_workspace read_dataset_in_workspace
load_workspace <- function(file) {
  ws <- workspace::unpack_workspace(file)
  all_objs <- list_object_in_workspace(ws)

  dataset_list <- list()

  # Import datasets
  datasets <- all_objs[all_objs$type %in% c("dataset", "geospatial"),]
  for (i in seq_len(nrow(datasets))) {
    curr_dataset <- datasets[i, ]
    name <- curr_dataset$name
    dataset_list[[name]] <- read_dataset_in_workspace(ws, name)
  }

  # Import settings
  settings <- list()

  return(list(datasets = dataset_list, settings = settings))
}
