defineModule(sim, list(
  name = "gmcsSummary",
  description = "Summarize and plot GMCS mortality and growth predictions produced by Biomass_core",
  keywords = c("LandR", "LandR.CS", "GMCS"),
  authors = c(
    person("Jonathan", "Van Elslander", email = "jonathan.vanelslander@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person(c("Ian", "MS"), "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(gmcsSummary = "0.2.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "gmcsSummary.Rmd"),
  reqdPkgs = list(
    "SpaDES.core (>= 3.0.4)",
    "data.table",
    "ggplot2",
    "LandR.CS (>=2.0.0)"
  ),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("summaryStatistic", "character", "mean", NA, NA,
                    desc = paste("Which summary statistic (calculated in Biomass_core) to plot.",
                                 "One of 'mean' or 'median'.")
    ),
    defineParameter("plotYears", "numeric", NA, NA, NA,
                    desc = "Years to include in the summary plots. NA uses all available years."
    )
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "gmcsSummaryByYear", objectClass = "data.table",
                 desc = paste(
                   "Summaries by year of gmcs predictions produced by Biomass_core.",
                   "Includes mean_* and median_* columns."
                 )
    ),
    expectsInput(objectName = "gmcsSummaryByYearSpecies", objectClass = "data.table",
                 desc = paste(
                   "Species summaries (by year) of gmcs predictions produced by Biomass_core.",
                   "Includes mean_* and median_* columns."
                 )
    )
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "GMCS summary figures", objectClass = "png",
                  desc = paste(
                    "Time series and species-faceted summary plots of GMCS",
                    "mortality and growth predictions."
                  )
    )
  )
))


## event types
##   - type `init` is required for initialization

doEvent.gmcsSummary <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- scheduleEvent(sim, end(sim), module = "gmcsSummary", eventType = "Plotting", eventPriority = .last())
    },

    Plotting = {
      outDir <- figurePath(sim)
      if (!dir.exists(outDir)) {
        dir.create(outDir, recursive = TRUE)
      }

      if (!"Biomass_core" %in% names(sim$modules)) {
        stop("gmcsSummary requires Biomass_core to be run before it.")
      }
      if (is.null(sim$gmcsSummaryByYear) ||
          is.null(sim$gmcsSummaryByYearSpecies)) {
        stop(
          "Required GMCS summary objects not found in simList.\n",
          "Ensure Biomass_core generated:\n",
          "  - sim$gmcsSummaryByYear\n",
          "  - sim$gmcsSummaryByYearSpecies"
        )
      }

      stat <- P(sim)$summaryStatistic
      mort_col   <- paste0(stat, "_mortPred")
      growth_col <- paste0(stat, "_growthPred")

      #Year By Year Summaries
      yearDT <- data.table::as.data.table(sim$gmcsSummaryByYear)

      if (!is.na(P(sim)$plotYears)[1]) {
        yearDT <- yearDT[year %in% P(sim)$plotYears]
      }

      plotData <- data.table::melt(
        yearDT,
        id.vars = "year",
        measure.vars = c(mort_col, growth_col),
        variable.name = "Metric",
        value.name = "Value"
      )

      plotData[, Metric := fifelse(Metric == mort_col, "Mortality", "Growth")]

      p_time <- ggplot(plotData, aes(x = year, y = Value, colour = Metric, group = Metric)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        facet_wrap(~Metric, scales = "free_y") +
        labs(title = paste(tools::toTitleCase(stat), "GMCS Mortality and Growth Predictions"),
          x = "Year",
          y = paste(stat, "predicted value")
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )

      ggsave(filename = file.path(outDir, paste0("ModelsOverTime_", stat, ".png")),
             plot = p_time, width = 10, height = 6, dpi = 150
      )

      #Species by Year summaries
      spDT <- data.table::as.data.table(sim$gmcsSummaryByYearSpecies)

      if (!is.na(P(sim)$plotYears)[1]) {
        spDT <- spDT[year %in% P(sim)$plotYears]
      }

      ## Mortality by species
      p_mort <- ggplot(spDT, aes(x = year, y = get(mort_col), colour = speciesCode, group = speciesCode)) +
        geom_line() +
        geom_point() +
        facet_wrap(~speciesCode) +
        labs(title = paste(tools::toTitleCase(stat), "GMCS Mortality Predictions by Species"),
          x = "Year",
          y = paste(stat, "mortality prediction")
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggsave(filename = file.path(outDir, paste0("species_", stat, "_Mortality.png")),
             plot = p_mort, width = 12, height = 8, dpi = 150
      )

      ## Growth by species
      p_growth <- ggplot(spDT, aes(x = year, y = get(growth_col), colour = speciesCode, group = speciesCode)) +
        geom_line() +
        geom_point() +
        facet_wrap(~speciesCode) +
        labs(title = paste(tools::toTitleCase(stat), "GMCS Growth Predictions by Species"),
          x = "Year",
          y = paste(stat, "growth prediction")
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggsave(filename = file.path(outDir, paste0("species_", stat, "_Growth.png")),
        plot = p_growth, width = 12, height = 8, dpi = 150
      )
    },

    warning(paste(
      "Undefined event type:", current(sim)[1, "eventType"], "in module gmcsSummary"
    ))
  )

  return(invisible(sim))
}

