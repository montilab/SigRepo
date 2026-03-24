# script to add the metabolomics sql tables into the database


library(DBI)
library(RMySQL)
library(dplyr)

# Connect to database
conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "sigrepo",
  host = "sigrepo.org",
  port = 3306,
  user = "root",
  password = "sigrepo"
)

# ---- Create Tables ---- #

create_refmet <- "
CREATE TABLE IF NOT EXISTS refmet_features (
  feature_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  feature_name VARCHAR(255) NOT NULL,
  chemical_name VARCHAR(255) NOT NULL,
  is_current BOOL DEFAULT 1,
  feature_hashkey VARCHAR(32) NOT NULL,
  version INT NOT NULL,
  PRIMARY KEY (feature_id),
  UNIQUE (feature_name),
  CHECK (is_current IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
"

create_hmdb <- "
CREATE TABLE IF NOT EXISTS hmdb_features (
  feature_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  feature_name VARCHAR(255) NOT NULL,
  chemical_name VARCHAR(255) NOT NULL,
  is_current BOOL DEFAULT 1,
  feature_hashkey VARCHAR(32) NOT NULL,
  version INT NOT NULL,
  PRIMARY KEY (feature_id),
  UNIQUE (feature_name),
  CHECK (is_current IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
"

create_smiles <- "
CREATE TABLE IF NOT EXISTS smiles_features (
  feature_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  feature_name VARCHAR(255) NOT NULL,
  chemical_name VARCHAR(255),
  is_current BOOL DEFAULT 1,
  feature_hashkey VARCHAR(32) NOT NULL,
  version INT NOT NULL,
  PRIMARY KEY (feature_id),
  UNIQUE (feature_name),
  CHECK (is_current IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
"

create_inchikey <- "
CREATE TABLE IF NOT EXISTS inchikey_features (
  feature_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  feature_name VARCHAR(255) NOT NULL,
  chemical_name VARCHAR(255),
  is_current BOOL DEFAULT 1,
  feature_hashkey VARCHAR(32) NOT NULL,
  version INT NOT NULL,
  PRIMARY KEY (feature_id),
  UNIQUE (feature_name),
  CHECK (is_current IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
"

# Execute queries
DBI::dbExecute(conn, create_refmet)
DBI::dbExecute(conn, create_hmdb)
DBI::dbExecute(conn, create_smiles)
DBI::dbExecute(conn, create_inchikey)


# optional commands for deleting schemas
DBI::dbExecute(conn, "DROP TABLE IF EXISTS refmet_features;")
DBI::dbExecute(conn, "DROP TABLE IF EXISTS hmdb_features;")
DBI::dbExecute(conn, "DROP TABLE IF EXISTS smiles_features;")
DBI::dbExecute(conn, "DROP TABLE IF EXISTS inchikey_features;")
# Optional: verify tables
DBI::dbListTables(conn)

# Disconnect when done
DBI::dbDisconnect(conn)



# adding the data to the reference tables


# reading refmet

refmet_dict <- read.csv(file = "/restricted/projectnb/montilab-p/personal/camv/misc_scripts/refmet_metab.numbers")

refmet_data <- metabolites_sig |> 
  select(CHEMICAL_NAME, RefMet) |> 
  mutate(is_current = 1,
         version = 0322026) |> 
  rename("CHEMICAL_NAME" = "chemical_name",
         "RefMet" = "feature_name",)




