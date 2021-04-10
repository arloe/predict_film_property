#' A function that load data
#' 
#' @importFrom data.table
#' 
#' @param data_dir the directory
#' @param filename the number of csv file
#' @param doff the column name of doff
#' @param outlier the outlier doff

data_loader <- function( data_dir, filename, doff, species, outlier_doff, outlier_species ){
  
  # read dataset
  cat( "Loading Data ...." )
  file_path = file.path(data_dir, filename)
  df = data.table::fread(input = file_path)
  
  # preprocessing (remove outlier doff)
  # remove outlier doff
  df = df[ !get(doff) %in% outlier_doff ]
  # remove outlier species
  df = df[ !get(species) %in% outlier_species ]
  
  cat( "The dimension of data:", dim(x = df) )
  
  return( df )  
}
