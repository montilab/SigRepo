# SigRepo: An R package for storing and processing omic signatures

  

![build](https://github.com/montilab/SigRepo/workflows/rcmdcheck/badge.svg)![pkgdown](https://github.com/montilab/SigRepo/workflows/pkgdown/badge.svg)![Docker
pulls](https://img.shields.io/docker/pulls/montilab/sigrepo)![Docker
image
size](https://img.shields.io/docker/image-size/montilab/sigrepo)![GitHub
last commit](https://img.shields.io/github/last-commit/montilab/SigRepo)

The `SigRepo` package provides a comprehensive set of functions for easy
storage and management of biological signatures and their components.
SigRepo (the `client`) works alongside `SigRepo_Server`, its `server`
counterpart. While SigRepo enables you to store, search, and retrieve
signatures and signature collections, these operations rely on a running
SigRepo_Server instance.

Interested in setting up your own SigRepo_Server? Check out the
installation instructions
[here](https://montilab.github.io/SigRepo_Server/articles/install_sigrepo.html).

To upload and download signatures — and to fully utilize the
functionalities offered by the SigRepo package — signatures and
signature collections must be represented as specific R6 objects. You
can create these objects using our proprietary package,
[OmicSignature](https://github.com/montilab/OmicSignature).

Click on each link below for more information:

- [Overview of the object
  structure](https://montilab.github.io/OmicSignature/articles/ObjectStructure.html)
- [Create an OmicSignature
  (OmS)](https://montilab.github.io/OmicSignature/articles/CreateOmS.html)
- [Create an OmicSignatureCollection
  (OmSC)](https://montilab.github.io/OmicSignature/articles/CreateOmSC.html)

Below, we walk you through few essential steps to install the `SigRepo`
package, and to store, retrieve, and interact with a list of signatures
stored in an [already deployed SigRepo server](https://sigrepo.org).

# Installation

- Using `devtools` package

``` R
# Load devtools package
library(devtools)

# Install SigRepo
devtools::install_github(repo = 'montilab/SigRepo')

# Install OmicSignature
devtools::install_github(repo = 'montilab/OmicSignature')

# Load tidyverse package
library(tidyverse)

# Load SigRepo package
library(SigRepo)

# Load OmicSignature package
library(OmicSignature)
```

## Before you begin

Please navigate to our [sigrepo.org](https://sigrepo.org) portal to
create your account. On the login page, click `"Register here!"` and
fill out the registration form to create an account. You will receive an
email when your account has been activated. Due to SQL constraints,
having multiple users on the same testing account, like running the
tutorial in the readme, will fail to connect. Each user using their own
account is ideal.

## Visibility

The SigRepo Project is a project that holds various sensitive
signatures. Because of this, visibility of certain signatures is taken
into account. What this means is that in order to retrieve certain
signatures with the visibility of private(visibility = 0), standard
accounts will not be able to access that signature and you will need
permission from the signature author to access it. Conversely, a
signature with the visibility of public (visibility = 1, this is also
the DEFAULT) will be available to everyone with an account. Please note,
viewing a signature with searchSignature() is different than using
getSignature(), searchSignature() only shows the metadata of the
signature and this is shown to everyone. getSignature takes into account
the visibilty of the signatures.

# Connect to SigRepo Database

We adopt a MySQL database structure for efficiently storing, searching,
and retrieving the biological signatures and its constituents. To access
the signatures stored in our database, [VISIT OUR
WEBSITE](https://sigrepo.org/) to create an account or [CONTACT
US](mailto:sigrepo@bu.edu) to be added.

There are three types of user accounts:  
- `admin` has **READ** and **WRITE** access to all signatures in the
database.  
- `editor` has **READ** and **WRITE** access to ONLY their own uploaded
signatures in the database.  
- `viewer` has **ONLY READ** access to see a list of signatures that are
publicly available in the database but **DO NOT HAVE WRITE** access to
the database.  

Once you have a valid account, to connect to our SigRepo database, one
can use the
[`SigRepo::newConnHandler()`](https://montilab.github.io/SigRepo/reference/newConnHandler.md)
function to create a handler which contains user credentials to
establish connection to our database.

``` R
# Create a connection handler
conn_handler <- SigRepo::newConnHandler(
  dbname = "sigrepo", 
  host = "sigrepo.org", 
  port = 3306, 
  user = <your_username>, 
  password = <your_password>
)
```

# Guides

- [Uploading Signature
  Tutorial](https://montilab.github.io/SigRepo/articles/signature-tutorials.html)
- [Uploading Signature Collection
  Tutorial](https://montilab.github.io/SigRepo/articles/collection-tutorials.html)
