#'ss_add_phino
#'
#'A function that adds Phi_NO and NO_qL to a dataset
#'@param df A tibble with NPQt, qL data.
#'@param fm darkadapted fm data
#'@param fo darkadapted fo data
#'@name ss_add_phinpq
#'@export



ss_add_phinpq <- function(df,fm,fo){

  npqtdat <- fopdat[,which(grepl(colnames(fopdat),pattern = "NPQt"))[1]]
  qldat <- fopdat[,which(grepl(colnames(fopdat),pattern = "qL"))[1]]
  #phinpq <- 1/(qL * (4.8824)/NPQt+1)
  #npqql <- 1/((4.8824)/NPQt+1)
  phinpq <- 1/(qldat * (fm/fo)/npqtdat+1)
  npqql <- 1/((fm/fo)/npqtdat+1)
  df <- add_column(df,"PhiNPQ" = phinpq)
  df <- add_column(df,"NPQ_qL" = npqql)
  return(df)
}