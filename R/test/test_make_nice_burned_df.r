test_that("test_get_max_lenght_in_cols_9_10", {
    
    col1 <- c("'Janika1'",  "'Joli'")
    col2 <- c("'bacsika1'", "'neni'")
    
    df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    max_length_in_cols <- get_max_lenght_in_cols(df);

    expected_res <- c(9, 10)
    
    expect_that( max_length_in_cols == expected_res, equals(c(T, T)) )
    
})


test_that("test_get_max_lenght_in_cols_when_colname_longer_than_its_values", {
    
    col1 <- c("1",  "2")
    col2 <- c("1",  "2")
    
    df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    names(df) <- c("'col1'", "'col2'")
    max_length_in_cols <- get_max_lenght_in_cols(df);
    
    expected_res <- c(6, 6)
    
    expect_that( identical(max_length_in_cols, expected_res), equals(TRUE) )
    
})


test_that("test_get_max_lenght_in_case_logical_and_numeric_values", {
    
    col1 <- c(TRUE,   F)
    col2 <- c(   T,   F)
    col3 <- c(   1,  12)
    col4 <- c(1234, 123)
    
    
    df <- data.frame(col1, col2, col3, col4, stringsAsFactors = FALSE)
    max_length_in_cols <- get_max_lenght_in_cols(df);
    
    expected_res <- c(5, 5, 4, 4)
    
    expect_that( identical(max_length_in_cols, expected_res), equals(TRUE) )
    
})



test_that("test_prepare_one_item_string_between_apostrophe", {
    
    string_with_apostrophe <- prepare_one_item("Henrik")
    expected_res <- "'Henrik'"
    
    expect_that( identical(string_with_apostrophe, expected_res), equals(TRUE) )
    
})


test_that("test_convert_all_col_to_character_working", {
    
    col1 <- c(TRUE,  F)
    col2 <- c(1234,  123)
    
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    all_char_df <- convert_all_col_to_character(example_df)
    
    
    col1 <- c("TRUE",  "FALSE")
    col2 <- c("1234",  "123")

    expected_char_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    
    expect_that( identical(all_char_df, all_char_df), equals(TRUE) )
})


test_that("test_get_one_row_working", {
     
    col1 <- c("'Henrik'",  " 'Tamas'")
    col2 <- c(" TRUE", "FALSE")
    col3 <- c(" 1", "12")
    
    
    df <- data.frame(col1, col2, col3, stringsAsFactors = FALSE)
    defined_row_str <- get_one_row(df, 1, c("'1'", "'2'", "'3'"), df_name = "df")
    
    
    
    expected_row <- "df['1', ] <- list( 'Henrik',  TRUE,  1)"
    
    expect_that( defined_row_str, equals(expected_row) )
    
})


test_that("test_fill_one_item_with_space_working", {
     
    str_with_whitespace <- fill_one_item_with_space("10",  4)
    expected_white_spaces <- "  10"
    
    expect_that( str_with_whitespace, equals(expected_white_spaces) )
    
})



test_that("test_give_apostrophe_to_char_type_vars_JUST_chars", {
    
    col1 <- c(1234,  123)
    col2 <- c("Tamas", "Henrik")
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    chars_with_apostrophe <- give_apostrophe_to_char_type_vars(example_df)
    
    col1 <- c(1234,  123)
    col2 <- c("'Tamas'", "'Henrik'")
    expected_chars_with_apostrophe_df <- data.frame(col1, col2, stringsAsFactors = FALSE)

    expect_that( identical(chars_with_apostrophe, expected_chars_with_apostrophe_df), equals(TRUE) )
    
})


test_that("test_make_same_len_chars_to_each_cols_working", {
    
    col1 <- c("1234",  "123")
    col2 <- c("'Tamas'", "'Henrik'")
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    same_len_chars <- make_same_len_chars_to_each_cols(example_df)
    
    col1 <- c("1234",  " 123")
    col2 <- c(" 'Tamas'", "'Henrik'")
    expected_same_len_chars <- data.frame(col1, col2, stringsAsFactors = FALSE)

    
    expect_that( identical(same_len_chars, expected_same_len_chars), equals(TRUE) )
})



test_that("test_get_all_rows_working", {
    
    col1 <- c("1234",  " 123")
    col2 <- c(" 'Tamas'", "'Henrik'")
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    row.names(example_df) <- c("'1'", "'2'")
    all_row_str <- get_all_rows(example_df, df_name = "df")
    
    expected_all_row_str <- "df['1', ] <- list( 1234,  'Tamas')\ndf['2', ] <- list(  123, 'Henrik')\n"
    
    expect_that( all_row_str, equals(expected_all_row_str) )
})


test_that("test_prepare_row_names_char_nums_with_apostrophe", {
    
    col1 <- c(1234,  123)
    col2 <- c(" 'Tamas'", "'Henrik'")
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    prepared_df <- prepare_row_names(example_df)
    
    
    expected_row_names_of_df <- c("'1'", "'2'")
    
    expect_that( row.names(prepared_df), equals(expected_row_names_of_df) )
})


test_that("test_prepare_row_names_same_length_with_spaces", {
    # There is the difference in the rownames
    col1 <- c(1234,  123)
    col2 <- c(" 'Tamas'", "'Henrik'")
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    row.names(example_df) <- c("abc", "abcd")
    prepared_row_same_len <- prepare_row_names(example_df)
    
    col1 <- c(1234,  123)
    col2 <- c(" 'Tamas'", "'Henrik'")
    expected_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    row.names(expected_df) <- c(" 'abc'", "'abcd'")
    
    expect_that( identical(prepared_row_same_len, expected_df), equals(T) )
})




test_that("test_get_max_row_name_length_working", {
    
    col1 <- c(1234,  123)
    col2 <- c(" 'Tamas'", "'Henrik'")
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    row.names(example_df) <- c("'abc'", "'abcd'")
    
    max_row_name_len <- get_max_row_name_length(example_df)
    
    expected_max_row_name_len <- 6
    
    expect_that( max_row_name_len, equals(expected_max_row_name_len) )
})




test_that("test_add_apastrophe_to_col_names", {
    
    col1 <- c(1234,  123)
    col2 <- c(" 'Tamas'", "'Henrik'")
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    
    
    col_name_with_apostrophes <- add_apastrophe_to_col_names(example_df)

    
    col1 <- c(1234,  123)
    col2 <- c(" 'Tamas'", "'Henrik'")
    expected_with_apostrophed_colnames <- data.frame(col1, col2, stringsAsFactors = FALSE)
    names(expected_with_apostrophed_colnames) <- c("'col1'", "'col2'")
    

    expect_that( identical(col_name_with_apostrophes, expected_with_apostrophed_colnames), equals(T) )
})




test_that("test_add_apostrophe_to_row_names_working", {
    
    col1 <- c(1234,  123)
    col2 <- c(" 'Tamas'", "'Henrik'")
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    row.names(example_df) <- c("a", "bb")
    
    row_name_with_apostrophes <- add_apostrophe_to_row_names(example_df)
    
    
    col1 <- c(1234,  123)
    col2 <- c(" 'Tamas'", "'Henrik'")
    expected_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    row.names(expected_df) <- c("'a'", "'bb'")    
    
    expect_that( identical(row_name_with_apostrophes, expected_df), equals(T) )
})




test_that("test_get_max_lenght_in_rows_working", {
    
    col1 <- c(1234,  123)
    col2 <- c(" 'Tamas'", "'Henrik'")
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    row.names(example_df) <- c("'a'", "'bb'")
    
    max_row_name_length <- get_max_lenght_in_rows(example_df)
    
    expect_that( max_row_name_length, equals(4) )
})



test_that("test_make_same_len_rownames_working", {
    
    col1 <- c(1234,  123)
    col2 <- c(" 'Tamas'", "'Henrik'")
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    row.names(example_df) <- c("'a'", "'bb'")
    
    same_len_row_names_filled_with_spaces <- make_same_len_rownames(example_df)
    
    col1 <- c(1234,  123)
    col2 <- c(" 'Tamas'", "'Henrik'")
    expected_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    row.names(expected_df) <- c(" 'a'", "'bb'")
    
    
    expect_that( identical(expected_df, same_len_row_names_filled_with_spaces), equals(T) )
})

test_that("test_get_colnames_str_row_working", {
    
    col1 <- c(1, 2)
    col2 <- c(3, 4)
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    names(example_df) <- c("'col1'", "'col2'")
    row.names(example_df) <- c("'1'", "'2'")
    row_to_print <- get_colnaems_str_row(example_df, df_name = "df")
    
    expected_colnames_str_row <- "names(df) <-    c( 'col1', 'col2')" 
    
    expect_that( row_to_print, equals(expected_colnames_str_row) )
})


test_that("test_make_correct_len_colnames_working", {
    
    col1 <- c("1234",  "123")
    col2 <- c(" 'Tamas'", "'Henrik'")
    example_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    
    correct_len_col_names <- make_correct_len_colnames(example_df)
    
    col1 <- c("1234",  "123")
    col2 <- c(" 'Tamas'", "'Henrik'")
    expected_df <- data.frame(col1, col2, stringsAsFactors = FALSE)
    names(expected_df) <- c("col1", "    col2")


    expect_that( identical(expected_df, correct_len_col_names), equals(T) )
})





# make_nice_burned_df

df <- data.frame(col1 <- numeric(),
                 col2 <- numeric(),
                 stringsAsFactors = FALSE)

names(df) <-    c( 'col1', 'col2')
df['1', ] <- list(      1,      3)
df['2', ] <- list(      2,      4)


