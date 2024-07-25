library(magrittr)
library(metaclipR)  ## >= v1.4.0 ## remotes::install_github("METACLIP/metaclipR")
library(igraph)
library(jsonlite)
library(jsonld)

## LOAD MASTER TABLES
master <- read.csv("inst/C3S_CMIP6_single-levels_extended.csv")
model.comp.master <- read.csv("inst/master_model_components.csv")
variables.master <- read.csv("inst/master_variables.csv")


#' @title SSP named individual matching
#' @description
#' Maps the SSP labels in master table with the corresponding vocabulary named individuals
#' @param exp Character string. Experiment label as found in master table
#' @return Character string. ipcc_terms named individual instance (ipcc: prefix)
#' @note
#' Compatible with vocabulary ipcc_terms >= 0.7
#' @author juaco
#' @keywords internal

set.exp.nodename <- function(exp) {
    exp <- match.arg(exp, choices = c("historical",
                                      "ssp1_1_9",
                                      "ssp1_2_6",
                                      "ssp2_4_5",
                                      "ssp5_8_5",
                                      "ssp3_7_0",
                                      "ssp4_3_4",
                                      "ssp4_6_0",
                                      "ssp5_3_4os"))
    switch(exp,
           "historical" = "ipcc:Historical",
           "ssp1_1_9" = "ipcc:SSP119",
           "ssp1_2_6" = "ipcc:SSP126",
           "ssp2_4_5" = "ipcc:SSP245",
           "ssp5_8_5" = "ipcc:SSP585",
           "ssp3_7_0" = "ipcc:SSP370",
           "ssp4_3_4" = "ipcc:SSP434",
           "ssp4_6_0" = "ipcc:SSP460",
           "ssp5_3_4os" = "ipcc:SSP534-os"
    )
}
