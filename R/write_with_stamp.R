#' Title
#' This function add the date and time stamp to the file name that will be saved as rds.
#' @param object 
#' @param name 
#' @param dir 
#' @param ext 
#' @param ... 
#'
#' @returns
#' @export


write_with_stamp <- function(object, name, dir, ext = c("rds", "csv")) {
  
  # match extension
  ext <- base::match.arg(ext)
  
  # timestamp e.g., 2025-12-10_14-30
  stamp <- base::format(base::Sys.time(), "%Y-%m-%d_%H-%M")
  
  # full file path
  file_path <- base::file.path(dir, base::paste0(name, "_", stamp, ".", ext))
  
  # write based on extension
  switch(
    ext,
    rds = base::saveRDS(object, file = file_path),
    csv = utils::write.csv(object, file = file_path, row.names = FALSE)
  )
  
  base::message("Saved: ", file_path)
  
  return(file_path)
}
