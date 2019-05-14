#'ss_add_phino
#'
#'A function that adds Phi_NO and NO_qL to a dataset
#'@param df A tibble with NPQt, qL data.
#'@param fm darkadapted fm data
#'@param fo darkadapted fo data
#'@name ss_add_phinpq
#'@export



ss_add_phinpq <- function(df,fm,fo){

  npqtdat <- df[,which(grepl(colnames(df),pattern = "NPQt"))[1]]
  qldat <- df[,which(grepl(colnames(df),pattern = "qL"))[1]]
  #phinpq <- 1/(qL * (4.8824)/NPQt+1)
  #npqql <- 1/((4.8824)/NPQt+1)
  phinpq <- 1/(qldat * (fm/fo)/npqtdat+1)
  npqql <- 1/((fm/fo)/npqtdat+1)
  df <- tibble::add_column(df,"PhiNPQ" = unlist(unname(phinpq)))
  df <- tibble::add_column(df,"NPQ_qL" = unlist(unname(npqql)))
  return(df)
}