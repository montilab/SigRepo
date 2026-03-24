# script to add the metabolomics sql tables into the database


library(DBI)
library(RMySQL)

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

# Optional: verify tables
DBI::dbListTables(conn)

# Disconnect when done
DBI::dbDisconnect(conn)
