#'ss_add_phino
#'
#'A function that adds Phi_NO and NO_qL to a dataset
#'@param df A tibble with NPQt, qL data.
#'@param fm darkadapted fm data
#'@param fo darkadapted fo data
#'@name ss_add_phino
#'@export

ss_add_phino <- function(df,fm,fo){
  npqtdat <- df[,which(grepl(colnames(df),pattern = "NPQt"))[1]]
  qldat <- df[,which(grepl(colnames(df),pattern = "qL"))[1]]
  #phino <- 1/(NPQt + 1 + qL * (3.8825))
  #noql <- 1/(NPQt + 1 + 3.8825)
  phino <- 1/(npqtdat + 1 + qldat * (fm/fo - 1))
  noql <- 1/(npqtdat + 1 + (fm/fo - 1))
  df <- tibble::add_column(df,"PhiNO" = unlist(unname(phino)))
  df <- tibble::add_column(df,"NO_qL" = unlist(unname(noql)))
  return(df)
}

