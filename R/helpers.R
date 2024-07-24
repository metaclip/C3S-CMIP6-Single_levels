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