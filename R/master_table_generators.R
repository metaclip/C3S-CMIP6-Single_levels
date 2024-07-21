## Current version of METACLIP-CMIP6: v0.0.0
source("https://raw.githubusercontent.com/metaclip/CMIP6/v0.0.0/R/model_component_helpers.R")

## /////////////////////////////////////////////////////////////////////////////
## DATASETS MASTER TABLE -------------------------------------------------------
## /////////////////////////////////////////////////////////////////////////////

## reference table (creator: Javi)
tabla <- read.csv("gitignore/C3S_CMIP6_single-levels.csv")

## Unique model output identifiers - put in table refVarTables
# vec <- paste(tabla$temporal_resolution, tabla$variable, sep = "-") %>% unique()
## unique frequency+variable codes
# data.frame("fullvarname" = vec) %>% write.csv("ignore/refVarTables.csv")

## New empty fields to complete
df <- cbind.data.frame(tabla,
                       "model_ID" = NA,
                       "CMIP6_table" = NA,
                       "CMIP6_table_shortname" = NA,
                       "grid" = 'gn')

## Map model names to actual CMIP6 voc model names:
df$model_ID <- gsub("_", "-", df$model) %>% toupper()

vars <- tabla$variable %>% unique()
ref <- read.csv("gitignore/refVarTables.csv")

for (i in 1:length(vars)) {
    # Daily model outputs
    ind <- which(df$variable == vars[i] & df$temporal_resolution == "daily")
    if (length(ind) > 0) {
        indrow <- grep(paste0("^daily-", vars[i], "$"), ref$fullvarname)
        if (length(indrow) > 0) {
            df$CMIP6_table[ind] <- ref[indrow, "CMIP6_table"]
            df$CMIP6_table_shortname[ind] <- ref[indrow, "shortname"]
        }
    }
    # Monthly model outputs
    ind <- which(df$variable == vars[i] & df$temporal_resolution == "monthly")
    if (length(ind) > 0) {
        indrow <- grep(paste0("^monthly-", vars[i], "$"), ref$fullvarname)
        if (length(indrow) > 0) {
            df$CMIP6_table[ind] <- ref[indrow, "CMIP6_table"]
            df$CMIP6_table_shortname[ind] <- ref[indrow, "shortname"]
        }
    }
    # Fixed model outputs
    ind <- which(df$variable == vars[i] & df$temporal_resolution == "fixed")
    if (length(ind) > 0) {
        indrow <- grep(paste0("^fixed-", vars[i], "$"), ref$fullvarname)
        if (length(indrow) > 0) {
            df$CMIP6_table[ind] <- ref[indrow, "CMIP6_table"]
            df$CMIP6_table_shortname[ind] <- ref[indrow, "shortname"]
        }
    }
}

# str(df)
# ## ////////////////////////////////////////////////////////////////////////////
#  write.csv(df, file = "inst/C3S_CMIP6_single-levels_extended.csv",
#            row.names = FALSE)
# ## ////////////////////////////////////////////////////////////////////////////



## /////////////////////////////////////////////////////////////////////////////
## MODEL COMPONENTS AND INSTITUTIONS -------------------------------------------
## /////////////////////////////////////////////////////////////////////////////

## CMIP6 Activities
url <- "https://raw.githubusercontent.com/WCRP-CMIP/CMIP6_CVs/main/CMIP6_source_id.json"
## List of all models
model.list <- fromJSON(url) %>% extract2("source_id")
## Filter Models participating in "ScenarioMIP" AND/OR "CMIP" activities
ml <- sapply(model.list, "[[", "activity_participation")
ind <- sapply(ml, "grepl",
              pattern = "ScenarioMIP|CMIP") %>% sapply(., "any") %>% which(isTRUE(.))
## Final list
## NOTE: this includes models participating in ScenarioMIP and CMIP activities
CMIP.models <- model.list[ind]
## Remove sample
CMIP.models <- CMIP.models[-grep("PCMDI-test-1-0", names(CMIP.models))]

model.components <- c("aerosol", "atmos", "atmosChem",
                      "land", "landIce", "ocean",
                      "ocnBgchem", "seaIce")

## Create empty data.frame
names.df <- c("gcm", "institution", model.components)

df <- matrix(data = NA,
             ncol = length(names.df),
             nrow = length(CMIP.models)) %>% as.data.frame()
names(df) <- names.df

## Fill data.frame
for (i in 1:length(CMIP.models)) {

    gcm.name <- CMIP.models[[i]]$source_id
    df[i, "gcm"] <- gcm.name

    gcm.institution <- CMIP.models[[i]]$institution_id
    ## Special individual delimiter "-/-" when several institutions involved
    df[i, "institution"] <- paste(gcm.institution, collapse = "-/-")

    for (j in 1:length(model.components)) {

        model <- CMIP.models[[i]][["model_component"]][[model.components[j]]]
        desc <- model$description
        name <- setIndividual(model.comp = model.components[j], descr = desc)

        df[i, model.components[j]] <- if (name == "none") {
            "none"
        } else {
            paste(gcm.name, model.components[j], name, sep = "_")
        }

    }

}

# # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# write.csv(df, file = "inst/master_model_components.csv", row.names = FALSE)
# # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



## /////////////////////////////////////////////////////////////////////////////
## VARIABLES -------------------------------------------------------------------
## /////////////////////////////////////////////////////////////////////////////

## Reference table of C3S CMIP6 variables and corresponding CMIP6 tables:
ref <- read.csv("gitignore/refVarTables.csv")
## Variable Individual names in vocabulary:
varID <- paste0("CMIP6_", ref$CMIP6_table, "-", ref$shortname)
vartable <- cbind.data.frame(ref, "varID" = varID)

## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## write.csv(vartable, file = "inst/master_variables.csv", row.names = FALSE)
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



## /////////////////////////////////////////////////////////////////////////////
## DATASET DOI'S ---------------------------------------------------------------
## /////////////////////////////////////////////////////////////////////////////

## Recycles metadata from dataset master table of IPCC AR6 Atlas
## The final table has missing DOIs from datasets not included in AR6 Atlas Chapter
## Need to be filled in by hand :-(

ref <- read.csv("https://raw.githubusercontent.com/metaclip/metaclipR-atlas/master/inst/dataset_table.csv")
ds.table <- subset(ref,
                   subset = Project == "CMIP6",
                   select = c("GCM", "Experiment", "doi"))
exp <- gsub("Historical", "historical", ds.table$Experiment)
exp <- gsub("SSP126", "ssp1_2_6", exp)
exp <- gsub("SSP245", "ssp2_4_5", exp)
exp <- gsub("SSP370", "ssp3_7_0", exp)
exp <- gsub("SSP585", "ssp5_8_5", exp)
ds.table$Experiment <- exp
ds.table$GCM <- toupper(ds.table$GCM)

## Open the C3S reference dataset table to find matching/missing datasets
c3s.table <- read.csv("inst/C3S_CMIP6_single-levels_extended.csv")
ids.c3s <- paste(c3s.table$experiment, c3s.table$model_ID, sep = "/") %>% unique()
ids.atlas <- paste(ds.table$Experiment, ds.table$GCM, sep = "/") %>% unique()
ind <- match(ids.c3s, ids.atlas)


experiment <- sapply(ids.c3s, "strsplit", split = "/") %>% sapply(., "extract2", 1)
modelID <- sapply(ids.c3s, "strsplit", split = "/") %>% sapply(., "extract2", 2)

doi.table <- cbind.data.frame("model_ID" = modelID,
                              "dataset_ref" = ids.c3s,
                              "experiment" = experiment,
                              "doi" = ds.table[ind,"doi"],
                              "atlas" = ifelse(is.na(ds.table[ind,"doi"]), 0, 1))

## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# write.csv(doi.table, file = "gitignore/master_datasets.csv", row.names = FALSE)
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


