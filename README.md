<br>

# <img src="man/figures/logo.png" align="left" width="190" /> SigRepo: An R package for storing and processing omic signatures

![build](https://github.com/montilab/SigRepo/workflows/rcmdcheck/badge.svg)
![pkgdown](https://github.com/montilab/SigRepo/workflows/pkgdown/badge.svg)
![Docker pulls](https://img.shields.io/docker/pulls/montilab/sigrepo)
![Docker image
size](https://img.shields.io/docker/image-size/montilab/sigrepo)
![GitHub last
commit](https://img.shields.io/github/last-commit/montilab/SigRepo)

The `SigRepo` package provides a comprehensive set of functions for easy
storage and management of biological signatures and their components.
SigRepo (the `client`) works alongside `SigRepo_Server`, its `server`
counterpart. While SigRepo enables you to store, search, and retrieve
signatures and signature collections, these operations rely on a running
SigRepo\_Server instance.

Interested in setting up your own SigRepo\_Server? Check out the
installation instructions
<a target="_blank" href="https://montilab.github.io/SigRepo_Server/articles/install_sigrepo.html" >here</a>.

To upload and download signatures — and to fully utilize the
functionalities offered by the SigRepo package — signatures and
signature collections must be represented as specific R6 objects. You
can create these objects using our proprietary package,
<a target="_blank" href="https://github.com/montilab/OmicSignature">OmicSignature</a>.

Click on each link below for more information:

-   <a
    href="https://montilab.github.io/OmicSignature/articles/ObjectStructure.html"
    target="_blank">Overview of the object structure</a>
-   <a
    href="https://montilab.github.io/OmicSignature/articles/CreateOmS.html"
    target="_blank">Create an OmicSignature (OmS)</a>
-   <a
    href="https://montilab.github.io/OmicSignature/articles/CreateOmSC.html"
    target="_blank">Create an OmicSignatureCollection (OmSC)</a>

Below, we walk you through few essential steps to install the `SigRepo`
package, and to store, retrieve, and interact with a list of signatures
stored in an <a target="_blank" href="https://sigrepo.org">already
deployed SigRepo server</a>.

# Installation

-   Using `devtools` package

<!-- -->

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

## Before you begin

Please navigate to our
<a href="https://sigrepo.org" target="_blank">sigrepo.org</a> portal to
create your account. On the login page, click `"Register here!"` and
fill out the registration form to create an account. You will receive an
email when your account has been activated. Due to SQL constraints,
having multiple users on the same testing account, like running the
tutorial in the readme, will fail to connect. Each user using their own
account is ideal.

# Connect to SigRepo Database

We adopt a MySQL database structure for efficiently storing, searching,
and retrieving the biological signatures and its constituents. To access
the signatures stored in our database,
<a target="_blank" href="https://sigrepo.org/">VISIT OUR WEBSITE</a> to
create an account or <a href="mailto:sigrepo@bu.edu">CONTACT US</a> to
be added.

There are three types of user accounts:<br> - `admin` has <b>READ</b>
and <b>WRITE</b> access to all signatures in the database.<br> -
`editor` has <b>READ</b> and <b>WRITE</b> access to ONLY their own
uploaded signatures in the database.<br> - `viewer` has <b>ONLY READ</b>
access to see a list of signatures that are publicly available in the
database but <b>DO NOT HAVE WRITE</b> access to the database.<br>

Once you have a valid account, to connect to our SigRepo database, one
can use the `SigRepo::newConnHandler()` function to create a handler
which contains user credentials to establish connection to our database.

    # Create a connection handler
    conn_handler <- SigRepo::newConnHandler(
      dbname = "sigrepo", 
      host = "sigrepo.org", 
      port = 3306, 
      user = <your_username>, 
      password = <your_password>
    )

# Guides

-   \[Uploading Signature Tutorial\]
-   [Uploading Signature Collection
    Tutorial](https://montilab.github.io/SigRepo/articles/collection-tutorials.html)
