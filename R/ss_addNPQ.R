#'Add NPQ and NPQt to a data frame.
#'
#'Given a data frame, returns the same data frame with NPQ and NPQt added.
#'@param df A data frame with Fm' and Fo' data.
#'@param fm The darkadapted fm
#'@name ss_addNPQ
#'@export

ss_addNPQ <- function(df,fm=NA){
  #npq is (Fm-Fm')/Fm'
  if(!"NPQ" %in% colnames(df) & !"NPQt" %in% colnames(df)){
    test <- TRUE
    Fodat <- dplyr::select(df,dplyr::contains(Fo))
    Fsdat <- dplyr::select(df,dplyr::contains(Fs))
    Fmdat <- dplyr::select(df,dplyr::contains(Fm))
    if(length(Fsdat) == 0 | length(Fodat) == 0){
      test <- FALSE
      cat("Cannot find Fs or Fm in df\n")
    }
    if(test){
      if(!is.na(fm)){
        npq <- (fm - Fmdat)/Fmdat
        df <- tibble::add_column(df,"NPQ" = unlist(unname(npq)))
      }
      if(!length(Fodat)==0){
        npqt <- (4.88/((Fmdat/Fodat)-1))-1
        df <- tibble::add_column(df,"NPQt" = unlist(unname(npqt)))
      }
    }
  } else{
    cat("NPQ or NPQt already in df\n")
  }
  
  return(df)
}
