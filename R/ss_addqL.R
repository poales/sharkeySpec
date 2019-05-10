#'Add qL to a dataframe with Fm', Fs, and Fo' data
#'
#'Given a data frame, returns the same data frame with qL added.
#'@param df A data frame with Fm',Fs, and Fo' data.
#'@name ss_addqL
#'@export

ss_addqL <- function(df){
  if(!"qL" %in% colnames(df)){
    Fmdat <- fopdat[,which(grepl(colnames(fopdat),pattern = "Fm"))[1]]
    Fsdat <- fopdat[,which(grepl(colnames(fopdat),pattern = "Fs"))[1]]
    Fodat <- fopdat[,which(grepl(colnames(fopdat),pattern = "Fo"))[1]]
    ql <- ((Fmdat - Fsdat)/(Fmdat - Fodat))*(Fodat / Fsdat)
    df <- tibble::add_column(df,"qL" = ql)
  } else{
    cat("qL already in df\n")
  }
  return(df)
}


