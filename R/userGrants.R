#' @title user_grant_statements
#' @description Build the GRANT statements that give a SigRepo account the
#' MySQL privileges its role needs. The schema is whatever database the
#' connection is on, never a literal name: a hard-coded `sigrepo` here made
#' `addUser()` fail with "Table 'sigrepo.users' doesn't exist" on any database
#' with another name, because MySQL requires the table to exist for a
#' table-level grant (#245, montilab/SigRepo_Server#130).
#' @param user_name MySQL account name (one string)
#' @param user_role One of "admin", "editor", "viewer". Anything else yields no statements.
#' @param dbname Name of the database the grants apply to, normally
#' `DBI::dbGetInfo(conn)$dbname` of the connection issuing them.
#' @return A character vector of SQL statements, possibly empty.
#'
#' @keywords internal
#'
#' @export
user_grant_statements <- function(user_name, user_role, dbname){

  if(base::length(user_role) != 1 || base::is.na(user_role)) return(base::character(0))

  db <- base::sprintf("`%s`", base::gsub("`", "``", dbname, fixed = TRUE))
  who <- base::sprintf("'%s'@'%%'", user_name)

  if(user_role == "admin"){
    base::sprintf("GRANT ALL PRIVILEGES ON %s.* TO %s WITH GRANT OPTION;", db, who)
  }else if(user_role == "editor"){
    table_grants <- c(
      "users" = "SELECT",
      "keywords" = "SELECT, INSERT",
      "phenotypes" = "SELECT, INSERT",
      "platforms" = "SELECT, INSERT",
      "signatures" = "SELECT, INSERT, UPDATE, DELETE",
      "signature_access" = "SELECT, INSERT, UPDATE, DELETE",
      "signature_feature_set" = "SELECT, INSERT, UPDATE, DELETE",
      "signature_collection_access" = "SELECT, INSERT, UPDATE, DELETE",
      "collection" = "SELECT, INSERT, UPDATE, DELETE",
      "collection_access" = "SELECT, INSERT, UPDATE, DELETE"
    )
    c(
      base::sprintf("GRANT SELECT ON %s.* TO %s;", db, who),
      base::sprintf("GRANT %s ON %s.`%s` TO %s;", table_grants, db, base::names(table_grants), who)
    )
  }else if(user_role == "viewer"){
    base::sprintf("GRANT SELECT ON %s.* TO %s;", db, who)
  }else{
    base::character(0)
  }
}
