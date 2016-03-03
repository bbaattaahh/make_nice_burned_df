#' Some thing
#'
#' Take id
#' @param Nincs
#' @return Lovely message
#' @export
make_burned_df <- function(input, output_path){
    
    if (class(input) == "data.frame"){
    rawdata <- input
    }
    else if (file.exists(input)){
    rawdata <- read.csv(input, header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = NA)
    }
    else {
    print("Unvalid input!")
    return (-1)
    }

    #define final output
    final_output <- c()
    
        
    #row names
    row_names <- "row_names <- as.factor( c("
    
    for (i in 1:length(rownames(rawdata))){
        row_names <- paste(row_names, " '", rownames(rawdata)[i], "',", sep = "")
    }
    
    substr(row_names, nchar(row_names), nchar(row_names)) <- ")"
    row_names <- paste(row_names, ")", sep = "")

    final_output <- append(final_output, row_names)  
        
    # coloumnl names        
    col_names <- names(rawdata)
    
    for (act_names in col_names){
    
    act_class <- class(rawdata[, act_names])
    
    act_col <- paste( act_names, " <- ", "as.", act_class,"( c(", sep = "")
    for (i in 1:nrow(rawdata[act_names])){
      if (is.na(rawdata[i, act_names])){
        act_col <- paste(act_col, " ", rawdata[i, act_names], ",", sep="")
      }
      else if (class(rawdata[i, act_names]) == "character"){
        act_col <- paste(act_col, " '", rawdata[i, act_names], "',",sep="")
      }
      else{
        act_col <- paste(act_col, " ", rawdata[i, act_names], ",", sep="")
      }
    }
    
    substr(act_col, nchar(act_col), nchar(act_col)) <- ")"
    act_col <- paste(act_col, ")", sep ="")
    
    final_output <- append(final_output, act_col)
    }
    
    
    data_frame <- "df <- data.frame("
    
    for (act_names in col_names){
    data_frame <- paste(data_frame, " ", act_names, ",", sep = "")
    } 
    
    substr(data_frame, nchar(data_frame), nchar(data_frame)) <- " "
    data_frame <- paste0(data_frame, ", stringsAsFactors = FALSE, row.names = row_names )")
    
    
    final_output <- append(final_output, data_frame)  
    
    
    
    #write out to flat file
    fileConn<-file(output_path)
    writeLines(final_output, fileConn)
    close(fileConn)

}


input_file <- "c:\\Personal Files\\PartsHarmonization\\working_directory\\part_harmonization\\Input Files\\epims_extract_merged_smpl_to_burn_into_code.csv"
output_file <- "c:\\Personal Files\\PartsHarmonization\\working_directory\\part_harmonization\\Input Files\\burn_into_code_output.txt"


make_burned_df(input_file, output_file)

