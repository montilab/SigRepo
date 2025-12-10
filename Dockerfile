# Build according to a specified version of R
ARG R_VERSION
ARG R_VERSION=${R_VERSION:-4.5.0}

############# Build Stage ##################

# Get shiny+tidyverse+devtools packages from rocker image
FROM rocker/tidyverse:${R_VERSION} as base

# Set working directory to install OmicSignature
WORKDIR / 

# Create package directory 
ENV OMICSIG_DIR=/OmicSignature

# Clone OmicSignature repo
RUN git clone https://github.com/montilab/OmicSignature.git

# Install dependencies for OmicSignature 
RUN R -e "BiocManager::install('limma')"

# Install OmicSignature 
RUN R -e "devtools::install_github(repo = 'montilab/OmicSignature', dependencies = TRUE)"

# Create package directory 
ENV SIGREPO_DIR=/SigRepo 

# Make package as working directory
WORKDIR ${SIGREPO_DIR}

# Copy package code to Docker image
COPY . ${SIGREPO_DIR}

# Install all package dependencies
RUN Rscript "${SIGREPO_DIR}/install_r_packages.R"

# Expose app at port 8787
EXPOSE 8787



