# ABOUT THE FUNCTIOn

# From a data.frame type vatiable or a csv file this function generate a string which is printed to a txt file.
# You can copy-paste this string to yous source code. The structure of this string is well-indented, so you can easiely 
# decode what is contained in the data.fame 
# 

# Input:    input: path of a csv file, or a dataframe
#           output_path: path of the txt where the well indented dataframe will be printed.
#           name_df: name of the created dataframr
#
# Output:   a text file which contains the importable dataframe source code 

make_nice_burned_df <- function(input, output_path, df_name){
    
    if (class(input) == "data.frame"){
        input_df <- input
    }
    else if (file.exists(input)){
        input_df <- read.csv(input, header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = NA)
    }
    else {
        print("Unvalid input!")
        return (-1)
    }
    
    
    empty_df <- define_empty_df(input_df, df_name)    
    
    process_df <- give_apostrophe_to_char_type_vars(input_df)
    process_df <- convert_all_col_to_character(process_df)
    process_df <- add_apastrophe_to_col_names(process_df)
    process_df <- add_apostrophe_to_row_names(process_df)
    
    process_df <- make_same_len_chars_to_each_cols(process_df)
    process_df <- make_correct_len_colnames(process_df)
    process_df <- make_same_len_rownames(process_df)
    
    col_name_str_row <- get_colnaems_str_row(process_df, df_name)
    all_rows <- get_all_rows(process_df, df_name)
    
    
    
    
    final_output <- c()
    final_output <- append(final_output, empty_df)  
    final_output <- append(final_output, "\n")
    final_output <- append(final_output, col_name_str_row)
    final_output <- append(final_output, all_rows)  
    
    
    #write out to flat file
    fileConn<-file(output_path)
    writeLines(final_output, fileConn)
    close(fileConn)
    
}



define_empty_df <- function(df, df_name){
    empty_df <- paste0(df_name, " <- data.frame(")
    
    for (i in 1: length(colnames(df))){
        empty_df <- paste0(empty_df, colnames(df)[i],  " <- ", class(df[, i]), "(),\n")
    }
    
    empty_df <- paste0(empty_df, "stringsAsFactors = FALSE)")
}


get_max_lenght_in_cols <- function(df){
    
    lengths <- rep(0, ncol(df))
    
    for (i in 1 : ncol(df)){
        str_col <- as.character(df[ , i])
        max_len_in_data <- max(nchar(str_col))
        
        act_colname <- names(df)[i]
        len_of_colname <- nchar(act_colname)
        
        lengths[i] <-  max(max_len_in_data, len_of_colname)
    }
    
    return(lengths)
}


get_max_lenght_in_rows <- function(df){
    
    row_names <- row.names(df)
    row_names <- as.character(row_names)
    
    lens <- sapply(row_names, nchar)
    
    max_len <- max(lens)
    
    return(max_len)
}




prepare_one_item <- function(item){
    if (class(item) == "character" && !is.na(item)){
        str_item <- paste0("'", item, "'")
        return(str_item)
    }
    
    
    str_item <- as.character(item)
    return(str_item)
}


get_one_row <- function(df, act_row_num, prepared_row_names, df_name){
    # sample : df[1, ] <-  list('ACTUATOR_PART:NA:NA:CARBON STL',  1,  2) 
    act_row <- paste0(df_name, "[", prepared_row_names[act_row_num], ", ] <- list(")
    
    for (i in 1:ncol(df)){
        act_item <- df[act_row_num, i]
        act_row <- paste0(act_row, " ", act_item, ",")
    }
    
    #delete last ","
    act_row <-  substr(act_row, 1, nchar(act_row) - 1)
    
    act_row <- paste0(act_row, ")")
    
    return(act_row)
}


fill_one_item_with_space <- function(item, expectes_length){
    len <- nchar(item)
    len_diff <- expectes_length - len
    nec_spaces <- paste0(rep(" ", len_diff), collapse = "")
    item_with_spaces <- paste0(nec_spaces, item)
    
    return(item_with_spaces)
}


convert_all_col_to_character <- function(df){
    for (i in 1:ncol(df)){
        df[, i] <- as.character(df[, i])
    }
    
    return(df)
}



give_apostrophe_to_char_type_vars <- function(df){
    for(i in 1:ncol(df)){
        if (class(df[ , i]) == "character"){
            df[ , i] <- sapply(df[ , i], prepare_one_item)
        }
    }
    
    return(df)
}


make_same_len_chars_to_each_cols <- function(df){
    lens <- get_max_lenght_in_cols(df)
    
    for (i in 1:ncol(df)){
        for (j in 1:nrow(df)){
            df[j, i] <- fill_one_item_with_space(df[j, i], lens[i])
        }
    }
    
    return(df)
}


make_same_len_rownames <- function(df){
    max_len <- get_max_lenght_in_rows(df)
    row_names <- row.names(df)
    
    for (i in 1:nrow(df)){
        row_names[i] <- fill_one_item_with_space(row_names[i], max_len)
    }
    
    row.names(df) <- row_names
    
    return(df)
}


make_correct_len_colnames <- function(df){
    max_lens_in_cols <- get_max_lenght_in_cols(df)
    
    col_names <- names(df)
    
    for (i in 1:ncol(df)){
        col_names[i] <- fill_one_item_with_space(col_names[i], max_lens_in_cols[i])
    }
    
    names(df) <- col_names
    
    return(df)
}



get_all_rows <- function(df, df_name){
    
    prepared_row_names <- row.names(df)
    all_rows <- ""
    
    
    for (i in 1:nrow(df)){
        act_row_str <- get_one_row(df, i, prepared_row_names, df_name)
        all_rows <- paste0(all_rows, act_row_str, "\n")
    }
    
    return(all_rows)
}

get_colnaems_str_row <- function(df, df_name){
    
    max_row_name_len <- get_max_lenght_in_rows(df)
    
    
    spaces_to_sinc_the_diff_because_of_rownames <- fill_one_item_with_space("", max_row_name_len-3)
    colname_row <- paste0("names(", df_name, ")", spaces_to_sinc_the_diff_because_of_rownames,  " <-    c( ")
    
    
    
    for (i in 1:ncol(df)){
        colname_row <- paste0(colname_row, names(df)[i], ", ")
    }
    
    colname_row <- substr(colname_row, 1, nchar(colname_row) - nchar(", "))
    colname_row <- paste0(colname_row, ")")
    
    return(colname_row)
}


prepare_row_names <- function(df){
    raw_row_names <- row.names(df)
    
    proc_row_names <- as.character(raw_row_names)
    proc_row_names <- prepare_one_item(proc_row_names)
    #Henrik
    max_len <- max(nchar(proc_row_names))
    
    proc_row_names <- mapply(fill_one_item_with_space, proc_row_names, max_len)
    proc_row_names <- as.character(proc_row_names)
    
    row.names(df) <- proc_row_names
    
    return(df)
}


add_apastrophe_to_col_names <- function(df){
    col_names <- names(df)
    col_names <- as.character(col_names)
    col_names_with_aphostrophes <- sapply(col_names, prepare_one_item)
    names(df) <- col_names_with_aphostrophes
    
    return(df)
}


add_apostrophe_to_row_names <- function(df){
    row_names <- row.names(df)
    row_names <- as.character(row_names)
    col_names_with_aphostrophes <- sapply(row_names, prepare_one_item)
    row.names(df) <- col_names_with_aphostrophes
    
    return(df)
}



get_max_row_name_length <- function(df){
    row_names <- row.names(df);
    row_names <- as.character(row_names)
    lens <- nchar(row_names)
    max_len <- max(lens)
    
    return(max_len)
}


# #Example input
# 
# row_names <- as.factor( c( '1', '2', '3'))
# precluster_keys <- as.character( c( 'ACTUATOR_PART:NA:NA:CARBON STL', 'ACTUATOR_PART:NA:NA:LA 60K NACE', 'ACTUATOR_PART:NA:NA:NA'))
# precluster_start <- as.integer( c( 1, 3, 5))
# precluster_stop <- as.integer( c( 2, 4, 9))
# precluster_boundaries <- data.frame( precluster_keys, precluster_start, precluster_stop , stringsAsFactors = FALSE, row.names = row_names )
# 
# 
# df <- data.frame(precluster_keys = character(),
#                  precluster_start = integer(), 
#                  precluster_stop = integer(),
#                  stringsAsFactors = FALSE)
# 
# df[1, ] <-  list('ACTUATOR_PART:NA:NA:CARBON STL',  1,   2) 
# df[2, ] <-  list('ACTUATOR_PART:NA:NA:LA 60K NACE', 3,   4) 
# df[3, ] <-  list('ACTUATOR_PART:NA:NA:NA',          5,   6) 
# 
# 
# output <- "c:\\Personal Files\\PartsHarmonization\\working_directory\\part_harmonization\\Output Files\\res.txt"
# make_nice_burned_df(df, output, df_name = "Henrik")

