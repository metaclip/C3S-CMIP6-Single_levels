# CMIP6-C3S-Single_levels

This repository implements and stores a full provenance description of the [C3S CMIP6 Projections Dataset](https://doi.org/10.24381/cds.c866074c) for the single-level variables.

The provenance description strictly adheres to CMIP6 controlled vocabularies and incorporates a semantic layer, enhancing the interpretability of provenance information through the [CMIP6 vocabularies of METACLIP](https://github.com/metaclip/CMIP6/).

## Repo directory structure:
* `*CMIP6-C3S-METACLIP-Provenance*`: This directory contains the JSON-LD representations of dataset provenance. It is organized into subdirectories by variables, with each file providing a full description of the corresponding variable and scenario.
* `R`: R scripts involved in output JSON-LD generation
* `inst`: internal lookup tables and other auxiliary elements

## More information:
* Overview of METACLIP: [Paper in Environmental Modelling and Software](https://doi.org/10.1016/j.envsoft.2019.07.005)


