
#' @title insert_table_sql
#' @description Insert a table into the database
#' @param conn An established database connection
#' @param db_table_name Name of a table in the database
#' @param table A table in the database
#' @param check_db_table whether to check database table. Default = TRUE.
#' 
#' @keywords internal
#' 
#' @export
insert_table_sql <- function(
    conn, 
    db_table_name, 
    table,
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check if table is a data frame object and not empty
  if(!methods::is(table, "data.frame") || base::length(table) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("\n'table' must be a data frame object and cannot be empty.\n")
  }
  
  # If table is not empty, import table into database
  if(base::nrow(table) > 0){
    
    # Get overlapping column names
    tbl_col_names <- base::colnames(table)[base::which(base::colnames(table) %in% db_col_names)]
    
    # Join column variables
    coln_var <- paste0("(", base::paste0(tbl_col_names, collapse = ", "), ")")    
    
    # Get values of each row
    coln_val <- base::seq_len(base::nrow(table)) |> 
      purrr::map_chr(
        function(r){
          #r=1;
          values <- base::paste0("'", table[r, tbl_col_names], "'", collapse = ", ")
          if(r < base::nrow(table)){
            values <- base::paste0("(", values, "),\n")
          }else{
            values <- base::paste0("(", values, ");\n")
          }
        }
      ) |> base::paste0(collapse = "") |> purrr::map_chr(function(x){ base::gsub("'NULL'", "NULL", x) })
    
    # Create a SQL query to insert table into database
    statement <- base::sprintf(
      "
      INSERT INTO %s %s
      VALUES %s
      ", db_table_name, coln_var, coln_val
    ) |> 
      purrr::map_chr(function(x){ base::gsub("'NULL'", "NULL", x) })
    
    # Export the sql statement to a sql file
    #base::writeLines(statement, base::file.path("~/SigRepo/inst/data/sql_schemas", base::paste0(db_table_name, ".sql")))
    
    # Insert table into database
    base::tryCatch({
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
    }, error = function(e){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop(e, "\n")
    })
    
  }
}

#' @title delete_table_sql
#' @description delete an entry from database table
#' @param conn An established database connection
#' @param db_table_name Name of a table in the database
#' @param delete_coln_var A column variable in the table for removing rows
#' @param delete_coln_val A list of values associated with delete_coln_var to be removed.
#' @param check_db_table whether to check database table. Default = TRUE.
#' @keywords internal
#' @export
delete_table_sql <- function(
    conn, 
    db_table_name, 
    delete_coln_var, 
    delete_coln_val,
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check delete_coln_var
  if(!base::length(delete_coln_var) == 1 || base::any(delete_coln_var %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("\n'delete_coln_var' must have length of 1 and cannot be empty.\n")
  }
  
  # Check column fields
  if(base::any(!delete_coln_var %in% db_col_names)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("\n'%s' table does not have the following column names: %s.\n", db_table_name, base::paste0(delete_coln_var[base::which(!delete_coln_var %in% db_col_names)], collapse = ", ")))
  }
  
  # Check delete_coln_val
  if(base::length(delete_coln_val) == 0 || base::any(delete_coln_val %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("\n'delete_coln_val' cannot be empty.\n")
  }
  
  # Create a where clause to remove entry 
  delete_where_clause <- base::paste0(delete_coln_var, " IN (", base::paste0("'", delete_coln_val, "'", collapse = ", "), ")")
  
  # Create sql statement
  statement <- base::sprintf(
    "
    DELETE FROM %s \n
    WHERE %s;
    ", db_table_name, delete_where_clause
  )
  
  # Set foreign key checks to false when dropping tables
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))
  
  # Delete entry from database
  base::tryCatch({
    base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
  }, error = function(e){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(e, "\n")
  })
  
}

#' @title lookup_table_sql
#' @description Look up a list of variables based on a particular variable 
#' and its associated values in the database
#' @param conn An established database connection
#' @param db_table_name A table in the database
#' @param return_var a list of column variables to be returned from the given table. 
#' Default '*' (means everything).
#' @param exclude_return_var a list of column names to be excluded from the returned table.
#' @param filter_coln_var a list of column variables in the given table. Default NULL.
#' @param filter_coln_val a list of values associated with 'filter_coln_var' variables. 
#' Most importantly, 'filter_coln_val' must have names or labels that matched the values of 'filter_coln_var'.
#' Default NULL.
#' @param filter_var_by if length(filter_coln_var) > 1, then 'filter_var_by' must be
#' provided as a vector of logical operators (e.g., OR/AND) with n = length(filter_coln_var) - 1. 
#' Default NULL.
#' @param check_db_table Check whether table exists in the database. Default = TRUE.
#' @keywords internal
#' @export
lookup_table_sql <- function(
    conn, 
    db_table_name, 
    return_var = "*", 
    exclude_return_var = NULL,
    filter_coln_var = NULL, 
    filter_coln_val = NULL, 
    filter_var_by = NULL, 
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check return_var
  if(base::length(return_var) == 0 || base::any(return_var %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("\n'return_var' cannot be empty.\n")
  }
  
  # Check column fields
  if(base::any(!filter_coln_var %in% db_col_names)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("\n'%s' table does not have the following column names: %s.\n", db_table_name, base::paste0(filter_coln_var[base::which(!filter_coln_var %in% db_col_names)], collapse = ", ")))
  }
  
  # Check filter_coln_var and filter_coln_val
  if(base::length(filter_coln_var) == 0 && base::length(filter_coln_val) == 0){
    
    where_clause <- ""
    
  }else{  
    
    if(base::length(filter_coln_var) != base::length(filter_coln_val) || !base::all(base::names(filter_coln_val) %in% filter_coln_var)){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop(
        "\nThe length of 'filter_coln_var' must equal to the length of 'filter_coln_val'.\n",
        "\nFurthermore, 'filter_coln_val' must a list with names or labels that matched the values of 'filter_coln_var'.\n"
      )
    }
    
    if((base::length(filter_coln_var) > 1) && (base::length(filter_var_by) != (base::length(filter_coln_var)-1)) && (!base::any(toupper(filter_var_by) %in% c("OR", "AND")))){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop("\n'filter_var_by' must contain a vector of logical operators (e.g, AND/OR) with n = length(filter_coln_var) - 1.\n")
    }
    
    # Create a where clause to look up values
    sql_clause <- base::seq_along(filter_coln_var) |> 
      purrr::map_chr(
        function(s){
          #s=1;
          clause <- base::sprintf("trim(lower(%s)) IN (%s)", filter_coln_var[s], base::paste0(paste0("'", base::trimws(base::tolower(filter_coln_val[[filter_coln_var[s]]])), "'"), collapse = ", "))
          if(s < base::length(filter_coln_var)){
            clause <- base::paste0(clause, " ", filter_var_by[s], " ")
          }
          return(clause)
        }
      ) |> base::paste0(collapse="")

    where_clause <- base::paste0("WHERE ", sql_clause)
    
  }
  
  # Create a list of return variables
  return_var_list <- base::paste0(return_var, collapse = ", ")
  
  # Create SQL statement to return table
  statement <- base::sprintf(
    "
    SELECT %s
    FROM %s %s
    ", return_var_list, db_table_name, where_clause
  )
  
  # Get table
  table <- base::tryCatch({
    base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
  }, error = function(e){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(e, "\n")
  }) 
  
  # Whether to exclude selected column names from the returned table
  if(base::length(exclude_return_var) > 0){
    table <- table %>% dplyr::select(dplyr::all_of(base::colnames(table)[base::which(!base::colnames(table) %in% exclude_return_var)]))
  }
  
  # Return table
  return(table)
  
}

