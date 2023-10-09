#reference.R

# Default display text in the main panel
displayText <- "This is placeholder text until you select values from the form on the left. "

# sponsor names for picklist
sponsors <- list("Alfred P. Sloan Foundation",
                 "Digital Curation Centre",
                 "Gordon and Betty Moore Foundation",
                 "Institute of Museum and Library Services",
                 "National Aeronautics and Space Administration",
                 "National Endowment for the Humanities",
                 "National Institute of Standards and Technology ",
                 "National Institutes of Health",
                 "National Oceanic and Atmospheric Administration",
                 "National Science Foundation",
                 "United States Department of Agriculture",
                 "United States Department of Defense",
                 "United States Department of Education",
                 "United States Department of Energy",
                 "United States Geological Survey")

# sponsor names used to refer to data management plans
planNames <- list("Information Products",
                  "Data Management and Sharing Plan",
                  "Data Sharing and Management Plan",
                  "Digital Product Form",
                  "Open Science and Data Management Plan",
                  "Data Management Plan",
                  "Data Management Plan",
                  "Data Management and Sharing Plan",
                  "Data Management Plan",
                  "Data Management Plan",
                  "Data Management Plan",
                  "Data Management Plan",
                  "Data Management Plan",
                  "Data Management Plan",
                  "Data Management Plan")

# data format picklist values
dataFormats <- list("Spreadsheet - Excel",
                    "Spreadsheet - Google Sheets",
                    "Spreadsheet - Numbers",
                    "Spreadsheet - LibreOffice Calc",
                    "Spreadsheet - Other",
                    "Statistical - SPSS",
                    "Statistical - SAS",
                    "Statistical - MatLab",
                    "Statistical - Other",
                    "Media - Audio - MP3",
                    "Media - Audio - WAVE",
                    "Media - Audio - AAC",
                    "Media - Audio - Other",
                    "Media - Video - MPEG-2",
                    "Media - Video - MPEG-4",
                    "Media - Video - AVI",
                    "Media - Video - Quicktime",
                    "Media - Video - WMV",
                    "Media - Video - Other",
                    "Text/Document - HTML",
                    "Text/Document - PDF",
                    "Text/Document - XML",
                    "Text/Document - ASCII Text",
                    "Text/Document - Other",
                    "Structured Data - JSON",
                    "Structured Data - XML",
                    "Structured Data - CSV",
                    "Structured Data - SQL database",
                    "Structured Data - Other",
                    "Geospatial - Shapefile",
                    "Geospatial - GeoTIFF",
                    "Geospatial - GML",
                    "Geospatial - ESRI GeoDatabase",
                    "Geospatial - SpatiaLite database",
                    "Geospatial - HDF",
                    "Geospatial - NetCDF",
                    "Geospatial - Other",
                    "Other")

# programming language picklist
languages <- list("Python",
                  "R",
                  "MatLab",
                  "Fortran",
                  "C",
                  "C++",
                  "Other",
                  "None")

# repositories
repositories <- list("Dryad",
                      "Zenodo",
                      "ICPSR",
                      "UNM Digital Repository",
                      "UNM's LabDrive Dark Archive",
                      "Other")

# documentation standards
documentationStandards <- list("ISO 19115",
                      "FGDC",
                      "EML",
                      "Darwin Core",
                      "Readme file(s)",
                      "Other")

# licenses
licenses <- list("CC0",
                 "CC-BY 4.0",
                 "CC-BY-SA 4.0",
                 "CC-BY-ND 4.0",
                 "CC-BY-SA-ND 4.0",
                 "CC-BY-SA-NC 4.0",
                 "CC-BY-SA-ND-NC 4.0")

dataVolumes <- list("1-100 MB",
                    "101 MB - 1 GB",
                    "1-50 GB",
                    "51-100 GB",
                    "101-500 GB",
                    "501 GB - 1 TB",
                    ">1TB")

# ChatGPT Models
models <- list("gpt-3.5-turbo",
               "gpt-3.5-turbo-16k",
               "gpt-4",
               "gpt-4-32k")
