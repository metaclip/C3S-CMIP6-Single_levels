library(magrittr)
library(metaclipR)  ## remotes::install_github("METACLIP/metaclipR")
library(igraph)

## LOAD MASTER TABLES
master <- read.csv("inst/C3S_CMIP6_single-levels_extended.csv")
model.comp.master <- read.csv("inst/master_model_components.csv")
variables.master <- read.csv("inst/master_variables.csv")

variables <- paste(master$temporal_resolution,
                   master$variable, sep = "-") %>% unique()

for (i in 1:length(variables)) {

    var <- variables[i]
    aux <- strsplit(var, split = "-")[[1]]
    var.subset <- subset(master,
                         subset = (temporal_resolution == aux[1] & variable == aux[2]))

    exps <- var.subset$experiment %>% unique()

    for (j in 1:length(exps)) {

        exp <- exps[j]
        exp.subset <- subset(var.subset, subset = experiment == exp)
        # gcms <- exp.subset$model_ID %>% unique()

        ds.subset.list <- lapply(1:nrow(exp.subset), function(k) {

            info.gcm <- exp.subset[k,]
            gcm <- info.gcm$model_ID

            ## Initialize graph
            graph <- make_empty_graph(directed = TRUE)

            ## /////////////////////////////////////////////////////////////////
            ## DATASET ---------------------------------------------------------
            ## /////////////////////////////////////////////////////////////////

            ## Dataset named individual
            dlabel <- paste(gcm, exp, sep = ".")
            dname <- paste("c6d:CMIP6", dlabel, sep = ".")

            graph <- my_add_vertices(graph,
                                     name = dname,
                                     label = dlabel,
                                     className = "ds:MultiDecadalSimulation")

            ## Project
            graph <- my_add_vertices(graph,
                                     name = "ipcc:CMIP6",
                                     label = "CMIP6",
                                     className = "ds:Project")
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, dname),
                                 getNodeIndexbyName(graph, "ipcc:CMIP6")),
                               label = "ds:hadProject")

            ## Data Provider
            graph <- my_add_vertices(graph,
                                     name = "ds:ESGF",
                                     label = "ESGF",
                                     className = "ds:DataProvider")
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, dname),
                                 getNodeIndexbyName(graph, "ds:ESGF")),
                               label = "ds:hadDataProvider")

            ## Experiment
            exp.nodename <- set.exp.nodename(exp)
            graph <- my_add_vertices(graph,
                                     name = exp.nodename,
                                     label = exp,
                                     className = "ds:Experiment")
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, dname),
                                 getNodeIndexbyName(graph, exp.nodename)),
                               label = "ds:hadExperiment")

            ## Modelling center
            ind <- grep(paste0("^", gcm, "$"),
                        model.comp.master$gcm,
                        ignore.case = TRUE)
            model.info <- model.comp.master[ind,]
            insts <- model.info$institution %>% strsplit(., split = "-/-") %>% extract2(1)

            for (l in 1:length(insts)) {
                inst <- insts[l]
                inst.nodename <- paste0("c6i:", inst)
                graph <- my_add_vertices(graph,
                                         name = inst.nodename,
                                         label = inst,
                                         className = "ds:ModellingCenter")
                graph <- add_edges(graph,
                                   c(getNodeIndexbyName(graph, dname),
                                     getNodeIndexbyName(graph, inst.nodename)),
                                   label = "ds:hadModellingCenter")
            }

            ## GCM
            label <- model.info$gcm
            gcm.nodename <- paste0("c6m:", label)
            graph <- my_add_vertices(graph,
                                     name = gcm.nodename,
                                     label = label,
                                     className = "ds:GCM")
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, dname),
                                 getNodeIndexbyName(graph, gcm.nodename)),
                               label = "ds:hadSimulationModel")

            ## ATMOS
            label <- gsub(".*_", "", model.info$atmos)
            comp.nodename <- paste0("c6m:", label)
            graph <- my_add_vertices(graph,
                                     name = comp.nodename,
                                     label = label,
                                     className = "ds:AtmosModel")
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, gcm.nodename),
                                 getNodeIndexbyName(graph, comp.nodename)),
                               label = "ds:hasAtmosModelComponent")

            ## LAND
            label <- gsub(".*_", "", model.info$land)
            comp.nodename <- paste0("c6m:", label)
            graph <- my_add_vertices(graph,
                                     name = comp.nodename,
                                     label = label,
                                     className = "ds:LandSurfaceModel")
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, gcm.nodename),
                                 getNodeIndexbyName(graph, comp.nodename)),
                               label = "ds:hasLandSurfaceModelComponent")

            ## AEROSOL
            label <- gsub(".*_", "", model.info$aerosol)
            if (label != "none") {
                comp.nodename <- paste0("c6m:", label)
                graph <- my_add_vertices(graph,
                                         name = comp.nodename,
                                         label = label,
                                         className = "ds:AerosolModel")
                graph <- add_edges(graph,
                                   c(getNodeIndexbyName(graph, gcm.nodename),
                                     getNodeIndexbyName(graph, comp.nodename)),
                                   label = "ds:hasAerosolModelComponent")
            }

            ## ATMOS-CHEM
            label <- gsub(".*_", "", model.info$atmosChem)
            if (label != "none") {
                comp.nodename <- paste0("c6m:", label)
                graph <- my_add_vertices(graph,
                                         name = comp.nodename,
                                         label = label,
                                         className = "ds:AtmosChemModel")
                graph <- add_edges(graph,
                                   c(getNodeIndexbyName(graph, gcm.nodename),
                                     getNodeIndexbyName(graph, comp.nodename)),
                                   label = "ds:hasAtmosChemModelComponent")
            }

            ## LAND-ICE
            label <- gsub(".*_", "", model.info$landIce)
            if (label != "none") {
                comp.nodename <- paste0("c6m:", label)
                graph <- my_add_vertices(graph,
                                         name = comp.nodename,
                                         label = label,
                                         className = "ds:LandIceModel")
                graph <- add_edges(graph,
                                   c(getNodeIndexbyName(graph, gcm.nodename),
                                     getNodeIndexbyName(graph, comp.nodename)),
                                   label = "ds:hasLandIceModelComponent")
            }

            ## OCEAN
            label <- gsub(".*_", "", model.info$ocean)
            if (label != "none") {
                comp.nodename <- paste0("c6m:", label)
                graph <- my_add_vertices(graph,
                                         name = comp.nodename,
                                         label = label,
                                         className = "ds:OceanModel")
                graph <- add_edges(graph,
                                   c(getNodeIndexbyName(graph, gcm.nodename),
                                     getNodeIndexbyName(graph, comp.nodename)),
                                   label = "ds:hasOceanModelComponent")
            }

            ## OCNBGCHEM
            label <- gsub(".*_", "", model.info$ocnBgchem)
            if (label != "none") {
                comp.nodename <- paste0("c6m:", label)
                graph <- my_add_vertices(graph,
                                         name = comp.nodename,
                                         label = label,
                                         className = "ds:OceanBgchemModel")
                graph <- add_edges(graph,
                                   c(getNodeIndexbyName(graph, gcm.nodename),
                                     getNodeIndexbyName(graph, comp.nodename)),
                                   label = "ds:hasOceanBgchemModelComponent")
            }

            ## SEAICE
            label <- gsub(".*_", "", model.info$seaIce)
            if (label != "none") {
                comp.nodename <- paste0("c6m:", label)
                graph <- my_add_vertices(graph,
                                         name = comp.nodename,
                                         label = label,
                                         className = "ds:SeaIceModel")
                graph <- add_edges(graph,
                                   c(getNodeIndexbyName(graph, gcm.nodename),
                                     getNodeIndexbyName(graph, comp.nodename)),
                                   label = "ds:hasSeaIceModelComponent")
            }

            ## /////////////////////////////////////////////////////////////////
            ## DATASET SUBSET --------------------------------------------------
            ## /////////////////////////////////////////////////////////////////

            dsubname <- paste0("DatasetSubset.", dlabel)
            descr <- paste("This step entails extracting a logical subset of the",
                           dlabel, "Dataset")
            attr.list <- list("dc:description" = descr)
            graph <- my_add_vertices(graph,
                                     name = dsubname,
                                     label = "DatasetSubset",
                                     className = "ds:DatasetSubset",
                                     attr = attr.list)
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, dname),
                                 getNodeIndexbyName(graph, dsubname)),
                               label = paste0("ds:hadDatasetSubset"))

            ## REALIZATION
            label <- info.gcm$realization_number
            memname <- paste(label, dlabel, sep = ".")
            graph <- my_add_vertices(graph,
                                     name = memname,
                                     label = label,
                                     className = "ds:Realization")
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, dsubname),
                                 getNodeIndexbyName(graph, memname)),
                               label = paste0("ds:hasRealization"))

            ## TEMPORAL EXTENT
            if (info.gcm$temporal_resolution != "fixed") {

                datestring <- strsplit(info.gcm$date, split = "/")[[1]]
                textentname <- paste("TemporalPeriod", randomName(), sep = ".")
                start <- datestring[1]
                end <- datestring[2]
                label <- paste(substr(start, 1, 4), substr(end, 1, 4), sep = "-")
                attr.list <- list("prov:startedAtTime" = start,
                                  "prov:endedAtTime" = end)
                graph <- my_add_vertices(graph,
                                         name = textentname,
                                         label = label,
                                         className = "ds:TemporalPeriod",
                                         attr = attr.list)
                graph <- add_edges(graph,
                                   c(getNodeIndexbyName(graph, dsubname),
                                     getNodeIndexbyName(graph, textentname)),
                                   label = paste0("ds:hasValidTemporalPeriod"))
            }

            ## VARIABLE
            label <- info.gcm$CMIP6_table_shortname
            tres <- info.gcm$temporal_resolution
            fullvarname <- paste(tres,
                                 info.gcm$variable, sep = "-")
            ind <- grep(fullvarname, variables.master$fullvarname)
            varnodename <- paste0("c6v:", variables.master[ind, "varID"])
            graph <- my_add_vertices(graph,
                                     name = varnodename,
                                     label = label,
                                     className = "ds:Variable",
                                     attr = list("ds:hasTimeFrequency" = tres))
            graph <- add_edges(graph,
                               c(getNodeIndexbyName(graph, dsubname),
                                 getNodeIndexbyName(graph, varnodename)),
                               label = paste0("ds:hasVariable"))
            return(list("parentnodename" = dsubname, "graph" = graph))
        })

        ## /////////////////////////////////////////////////////////////////////
        ## ENSEMBLE BUILDING ---------------------------------------------------
        ## /////////////////////////////////////////////////////////////////////

        ## Ensemble dataset
        ens <- metaclipR.Ensemble(graph.list = ds.subset.list,
                                  disable.command = TRUE)

        ## /////////////////////////////////////////////////////////////////////
        ## EXPORT JSON-LD ------------------------------------------------------
        ## /////////////////////////////////////////////////////////////////////

        ## metaclipR::graph2json()
        ## TO-DO: adapt graph2jason to use user-defined json templates

    }
}

