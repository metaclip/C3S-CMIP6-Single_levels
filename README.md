# CMIP6-C3S-Single_levels

This repository implements and stores a complete provenance description of the [C3S CMIP6 Projections Dataset](https://doi.org/10.24381/cds.c866074c) for the single-level variables.

The provenance description strictly adheres to CMIP6 controlled vocabularies and incorporates a semantic layer, enhancing the interpretability of provenance information through the [CMIP6 vocabularies of METACLIP](https://github.com/metaclip/CMIP6/).


## Repo directory structure:
* `*json_ld/*`: Contains the JSON‑LD provenance representations. The directory is organised into subfolders by variable, where each file provides a full provenance description for the corresponding variable–scenario combination.
At the top level, the directory includes dataset‑level (experiment‑level) provenance, without further subdivision by variable.
* `R/`: R scripts used to generate the JSON‑LD output.
* `inst/`: internal lookup tables and other auxiliary resources.


## Note:
To inspect the JSON-LD files in more detail, the [JSON-LD playground](https://json-ld.org/playground/) can be used, by pasting the Raw URL of any file within the `json_ld` directory


## More information:
* METACLIP overview: [Paper in Environmental Modelling and Software](https://doi.org/10.1016/j.envsoft.2019.07.005)


