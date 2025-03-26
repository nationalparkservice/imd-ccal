getParamCrosswalk <- function(description) {
  crosswalk <- c("dissolved inorganic carbon" = "DIC",
                 "dissolved organic carbon" = "DOC",
                 "total dissolved nitrogen" = "TDN",
                 "total dissolved phosphorus" = "TDP",
                 "total dissolved solids" = "TDS",
                 "total nitrogen" = "TN",
                 "total organic carbon" = "TOC",
                 "total phospohorus" = "TP",
                 "total solids" = "TS",
                 "unfiltered total nitrogen" = "UTN",
                 "unfiltered total phosphorus" = "UTP",
                 "inorganic nitrogen" = "NO3-N+NO2-N",
                 "nitrate+nitrite nitrogen" = "NO3-N+NO2-N",
                 "nitrate nitrogen" = "NO3",
                 "orthophosphorus" = "PO4-P")

  if (missing(description)) {
    return(crosswalk)
  } else {
    return(crosswalk[description])
  }

}
