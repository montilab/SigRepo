<br>

# <img src="man/figures/logo.png" align="left" width="190" /> SigRepo: An R package for storing and processing omic signatures

![build](https://github.com/montilab/SigRepo/workflows/rcmdcheck/badge.svg)
![pkgdown](https://github.com/montilab/SigRepo/workflows/gh-pages-pkgdown/badge.svg)
![Docker pulls](https://img.shields.io/docker/pulls/montilab/sigrepo)
![Docker image
size](https://img.shields.io/docker/image-size/montilab/sigrepo)
![GitHub last
commit](https://img.shields.io/github/last-commit/montilab/SigRepo)

The SigRepo package provides a comprehensive set of functions for storing,
managing, and interacting with biological signatures and their components. 
SigRepo (the client) works in tandem with SigRepo_Server, its server-side counterpart. 
While SigRepo allows you to store, search, and retrieve signatures and signature collections, 
all such operations require access to a running SigRepo_Server instance.

Interested in setting up your own SigRepo\_Server? Check out the
installation instructions
<a target="_blank" href="https://montilab.github.io/SigRepo_Server/articles/install_sigrepo.html" >here</a>.

To upload and download signatures—and to fully leverage the functionality of the SigRepo package—signatures
and signature collections must be represented as specific R6 objects. 
These objects are created using our companion package,
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

Please visit
<a href="https://sigrepo.org" target="_blank">sigrepo.org</a> to create an account.
On the login page, click “Register here!” and complete the registration form. 
You will receive an email once your account has been activated.

Due to SQL constraints, multiple users sharing the same testing account (for example, when following the README tutorial) 
may encounter connection failures. We strongly recommend that each user create and use their own account.

## Visibility

SigRepo hosts a collection of potentially sensitive biological signatures, so access control is enforced through visibility settings.

- Private signatures (visibility = 0) are accessible only to the signature author and users who have been granted explicit permission.

- Public signatures (visibility = 1, the default) are accessible to all registered users.

Note that searchSignature() and getSignature() behave differently:

- searchSignature() returns metadata only and is visible to all users, regardless of signature visibility.

- getSignature() retrieves the full signature and enforces visibility restrictions.

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

-   [Uploading Signature
    Tutorial](https://montilab.github.io/SigRepo/articles/signature-tutorials.html)
-   [Uploading Signature Collection
    Tutorial](https://montilab.github.io/SigRepo/articles/collection-tutorials.html)
