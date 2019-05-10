#'Add NPQ and NPQt to a data frame.
#'
#'Given a data frame, returns the same data frame with NPQ and NPQt added.
#'@param df A data frame with Fm' and Fo' data.
#'@param fm The darkadapted fm
#'@name ss_addNPQ
#'@export

ss_addNPQ <- function(df,fm){
  #npq is (Fm-Fm')/Fm'
  if(!"NPQ" %in% colnames(df) & !"NPQt" %in% colnames(df)){
    Fmdat <- fopdat[,which(grepl(colnames(fopdat),pattern = "Fm"))[1]]
    Fsdat <- fopdat[,which(grepl(colnames(fopdat),pattern = "Fs"))[1]]
    Fodat <- fopdat[,which(grepl(colnames(fopdat),pattern = "Fo"))[1]]
    npq <- (fm - Fmdat)/Fmdat
    npqt <- (4.88/((Fmdat/Fodat)-1))-1
    df <- tibble::add_column(df,"NPQ" = npq)
    df <- tibble::add_column(df,"NPQt" = npqt)
  } else{
    cat("NPQ or NPQt already in df\n")
  }
  
  return(df)
}