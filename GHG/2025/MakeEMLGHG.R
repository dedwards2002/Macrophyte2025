### MakeEMLGHG ###

### Updated script to publish LO-L GHG data to EDI
### Following: MakeEMLInflow.R and MakeEMLChemistry.R
### 24Jun2020, A. Hounshell
### Updated for 01Sep2020, A. Hounshell
### Publishing GHG data from RPM: 2015-2019

### Updated for 2021 GHG data publication, A. Hounshell, 10 Jan 2022
### Updated for May 2022 GHG data publication, A. Hounshell, 30 June 2022

###############################################################################

## Clear workspace
rm(list = ls())

# Find and set-up working directory
#wd <- getwd()
#setwd(wd)
#library(tidyverse)

# Add site descriptions

#Install the required googlesheets4 package
#install.packages('googlesheets4')
#Load the library 

# (install and) Load EMLassemblyline #####
# install.packages('devtools')

# devtools::install_github("EDIorg/EMLassemblyline")

#note that EMLassemblyline has an absurd number of dependencies and you
#may exceed your API rate limit; if this happens, you will have to wait an
#hour and try again or get a personal authentification token (?? I think)
#for github which allows you to submit more than 60 API requests in an hour

# (install and) Load EMLassemblyline #####
# install.packages('devtools')

pacman::p_load(devtools, EMLassemblyline, here, xml2, XML, tidyverse)
#library(devtools)

# devtools::install_github("EDIorg/EDIutils")
# devtools::install_github("EDIorg/taxonomyCleanr")
# devtools::install_github("EDIorg/EMLassemblyline")
# remotes::install_github("EDIorg/EMLassemblyline")
# remotes::install_github("ropensci/taxize", dependencies = TRUE)
#install.packages("taxize")
#note that EMLassemblyline has an absurd number of dependencies and you
#may exceed your API rate limit; if this happens, you will have to wait an
#hour and try again or get a personal authentification token (?? I think)
#for github which allows you to submit more than 60 API requests in an hour

#library(EMLassemblyline)

#Step 1: Create a directory for your dataset
#in this case, our directory is Reservoirs/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_GHG

folder = paste0(here(),"/Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_GHG/2025/")

#Step 2: Move your dataset to the directory

#Step 3: Identify an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset
#right now the only supported option is "table"; happily, this is what 
#we have!

#Step 5: Import the core metadata templates

#for our application, we will need to generate all types of metadata
#files except for taxonomic coverage, as we have both continuous and
#categorical variables and want to report our geographic location

# View documentation for these functions
#?template_core_metadata
#?template_table_attributes
#?template_categorical_variables #don't run this till later
#?template_geographic_coverage

# Import templates for our dataset licensed under CCBY, with 1 table.
#template_core_metadata(path = folder,
#                       license = "CCBY",
#                       file.type = ".txt",
#                       write.file = TRUE)
#
template_table_attributes(path = folder,
                          data.path = folder,
                          data.table = c("ghg_maintenancelog_2015_2025.csv", "ghg_2015_2025.csv", "site_descriptions.csv"),
                          write.file = TRUE)
#
#
##we want empty to be true for this because we don't include lat/long
##as columns within our dataset but would like to provide them
#template_geographic_coverage(path = folder,
#                             data.path = folder,
#                             data.table = c("GHG_2015_2023.csv"),
#                             empty = TRUE,
#                             write.file = TRUE)
#
#Step 6: Script your workflow
#that's what this is, silly!

#Step 7: Abstract
#copy-paste the abstract from your Microsoft Word document into abstract.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 8: Methods
#copy-paste the methods from your Microsoft Word document into methods.txt
#if you want to check your methods for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics
  
#Step 9: Additional information
# Copy and paste from previous years here

#Step 10: Keywords
#DO NOT EDIT KEYWORDS FILE USING A TEXT EDITOR!! USE EXCEL!!
#not sure if this is still true...let's find out! :-)
#see the LabKeywords.txt file for keywords that are mandatory for all Carey Lab data products

#Step 11: Personnel
#copy-paste this information in from your metadata document
#Cayelan needs to be listed several times; she has to be listed separately for her roles as
#PI, creator, and contact, and also separately for each separate funding source (!!)

#Step 12: Attributes
#grab attribute names and definitions from your metadata word document
#for units....
# View and search the standard units dictionary
#view_unit_dictionary()
#put flag codes and site codes in the definitions cell
#force reservoir to categorical

#Step 14: Categorical variables
#THIS WILL ONLY WORK once you have filled out the attributes_chemistry.txt and
#identified which variables are categorical
template_categorical_variables(path = folder,
                               data.path = folder,
                              #data.table = c("ghg_maintenancelog_2015_2024.csv", "ghg_2015_2024.csv", "site_descriptions.csv"),
                               write.file = TRUE)

#open the created value IN A SPREADSHEET EDITOR and add a definition for each category - NONE

#Step 15: Geographic coverage
#copy-paste the bounding_boxes.txt file (or geographic_coverage.txt file) that is Carey Lab specific into your working directory

#Step 16: Custom units
# Copy and pased custom units .txt file from prior year


## Step 17: Obtain a package.id FROM STAGING ENVIRONMENT. ####
# Go to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords. 

# Select Tools --> Data Package Identifier Reservations and click 
# "Reserve Next Available Identifier"
# A new value will appear in the "Current data package identifier reservations" 
# table (e.g., edi.123)
# Make note of this value, as it will be your package.id below

## Step 18: Make EML metadata file using the EMLassemblyline::make_eml() command ####
# For modules that contain only zip folders, modify and run the following 
# ** double-check that all files are closed before running this command! **

## Make EML for staging environment
## NOTE: Will need to check geographic coordinates!!!
eml_file <- make_eml(
  path = folder,
  data.path = folder,
  eml.path = folder,
  dataset.title = "Time series of dissolved methane and carbon dioxide concentrations for Falling Creek Reservoir, Beaverdam Reservoir and Carvins Cove Reservoir in southwestern Virginia, USA during 2015-2025",
  temporal.coverage = c("2015-03-31", "2025-12-02"),
  maintenance.description = 'ongoing',
  data.table = c('ghg_2015_2025.csv',
                 'site_descriptions.csv',
                 "ghg_maintenancelog_2015_2025.csv" ),
  data.table.description = c("GHG dataset",
                             "Descriptions of sites in this dataset with lat/long coordinates",
                             'Maintenance log used in L1 generation'),
 # data.table.name = c("ghg_2015_2025",
  #                    "site_descriptions",
   #                   "ghg_maintenancelog_2015_2025"),
  other.entity= c('ghg_qaqc_2015_2025.R',
                  'ghg_functions_for_L1.R',
                  'ghg_inspection_2015_2025.Rmd'),
  #other.entity.name = c("ghg_qaqc_2015_2025", 
   #                     'ghg_functions_for_L1',
    #                    "ghg_inspection_2015_2025"),
  other.entity.description = c("R script for GHG QA/QC to generate L1 in 2025", 
                               'Functions used in the L1 generation script for QA/QC in 2025',
                               'Script to generate plots and combine L1 and EDI products'),
  user.id = 'ccarey',
  user.domain = 'EDI',
  #package.id = 'edi.997.24', #package id for staging

  package.id = 'edi.551.10', #package id for production
 write.file = T, ### write the file to the folder
 return.obj = T) ## return the object so we can get the package.id



# get the package.id from above
package.id = eml_file$packageId

# read in the xml file that you made from the make_eml function
doc <- read_xml(paste0(folder,package.id,".xml"))

# Find the parent node where <licensed> should be added
parent <- xml_find_first(doc, ".//dataset")   # change to your actual parent

# Create <licensed> node with the name of the licence, the url, the identifier
licensed <- xml_add_child(parent, "licensed")

xml_add_child(licensed, "licenseName",
              "Creative Commons Attribution Non Commercial 4.0 International")
xml_add_child(licensed, "url",
              "https://spdx.org/licenses/CC-BY-NC-4.0")
xml_add_child(licensed, "identifier",
              "CC-BY-NC-4.0")

# Find the parent
parent <- xml_find_first(doc, "//dataset")

# Find the nodes
childC <- xml_find_first(parent, "licensed")

# Remove childC from its current position
xml_remove(childC)

# Insert childC at position 10 (after Intellectual_rights)
xml_add_child(parent, childC, .where = 15)

# Save the file with the changes
write_xml(doc, paste0(folder,package.id,".xml"))



## Step 8: Check your data product! ####
# Return to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using one of the Carey Lab usernames and passwords. 

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File", 
# choose your metadata (.xml) file (e.g., edi.270.1.xml), check "I want to 
# manually upload the data by selecting files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder), 
# then click Upload. Files will upload and your EML metadata will be checked 
# for errors. If there are no errors, your data product is now published! 
# If there were errors, click the link to see what they were, then fix errors 
# in the xml file. 
# Note that each revision results in the xml file increasing one value 
# (e.g., edi.270.1, edi.270.2, etc). Re-upload your fixed files to complete the 
# evaluation check again, until you receive a message with no errors.

## Step 17: Obtain a package.id. ####

## Step 18: Upload revision to EDI
# Go to EDI website: https://portal.edirepository.org/nis/home.jsp and login with Carey Lab ID
# Click: Tools then Evaluate/Upload Data Packages
# Under EML Metadata File, select 'Choose File'
# Select the .xml file of the last revision (i.e., edi.202.4)
# Under Data Upload Options, select 'I want to manually upload the data by selecting...'
# Click 'Upload'
# Select text files and R file associated with the upload
# Then click 'Upload': if everything works, there will be no errors and the dataset will be uploaded!
# Check to make sure everything looks okay on EDI Website

