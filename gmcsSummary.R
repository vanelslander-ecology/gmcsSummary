defineModule(sim, list(
  name = "gmcsSummary",
  description = "Summarize and plot GMCS mortality and growth predictions produced by Biomass_core",
  keywords = c("LandR", "LandR.CS", "GMCS"),
  authors = c(
    person(c("Jonathan"), "Van Elslander", email = "jonathan.vanelslander@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person(c("Ian", "MS"), "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(gmcsSummary = "0.1.0.9000"),
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
                    desc = "Summary statistic to apply to GMCS predictions. One of 'mean' or 'median'."),
    defineParameter("plotYears", "numeric", NA, NA, NA,
                    desc = "Years to include in the summary plots. NA uses all available years."),
    defineParameter("outputDir", "character", "outputs/LandRCS", NA, NA,
                    desc = "Directory where GMCS summary plots will be written.")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "gmcsPredictions", objectClass = "list",
                 desc = paste("List of data.tables containing yearly GMCS debug outputs",
                              "from Biomass_core (requires LandR.CS.debug = TRUE)."))
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "GMCS summary figures",objectClass = "png",
                  desc = "Time series and species-faceted summary plots of GMCS mortality and growth predictions."))
))

## event types
#   - type `init` is required for initialization

doEvent.gmcsSummary <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    end = {

      ## --- dependency checks --------------------------------------------------
      if (!"Biomass_core" %in% names(sim$modules)) {
        stop("gmcsSummary requires Biomass_core to be run before it.")
      }

      if (is.null(sim$gmcsPredictions)) {
        stop(
          "sim$gmcsPredictions not found.\n",
          "Ensure Biomass_core was run with LandR.CS.debug = TRUE\n",
          "and that GMCS outputs were retained in memory."
        )
      }

      stat <- match.arg(P(sim)$summaryStatistic, c("mean", "median"))
      statFun <- switch(
        stat,
        mean   = function(x) mean(x, na.rm = TRUE),
        median = function(x) median(x, na.rm = TRUE)
      )

      outDir <- P(sim)$outputDir
      if (!dir.exists(outDir)) {
        dir.create(outDir, recursive = TRUE)
      }

      gmcsData <- data.table::rbindlist(sim$gmcsPredictions, fill = TRUE)

      if (!is.na(P(sim)$plotYears)[1]) {
        gmcsData <- gmcsData[year %in% P(sim)$plotYears]
      }

      yearlySummary <- gmcsData[, .(
        mortPred   = statFun(mortPred),
        growthPred = statFun(growthPred)
      ), by = year]

      plotData <- data.table::melt(
        yearlySummary,
        id.vars = "year",
        variable.name = "Metric",
        value.name = "Value"
      )

      p_time <- ggplot(plotData,
                       aes(x = year, y = Value, colour = Metric, group = Metric)
      ) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        facet_wrap(~Metric, scales = "free_y") +
        labs(
          title = paste(
            tools::toTitleCase(stat),
            "GMCS Mortality and Growth Predictions"
          ),
          x = "Year",
          y = paste(stat, "predicted value")
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )

      ggsave(
        filename = file.path(outDir, paste0("ModelsOverTime_", stat, ".png")),
        plot = p_time,
        width = 10, height = 6, dpi = 150
      )

      ## --- species × year summaries -------------------------------------------
      speciesSummary <- gmcsData[
        !is.na(mortPred),
        .(
          mortPred   = statFun(mortPred),
          growthPred = statFun(growthPred)
        ),
        by = .(year, speciesCode)
      ]

      p_mort <- ggplot(speciesSummary,
                       aes(x = year, y = mortPred, colour = speciesCode, group = speciesCode)
      ) +
        geom_line() +
        geom_point() +
        facet_wrap(~speciesCode) +
        labs(
          title = paste(tools::toTitleCase(stat),
                        "GMCS Mortality Predictions by Species"),
          x = "Year",
          y = paste(stat, "mortality prediction")
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggsave(
        filename = file.path(outDir,
                             paste0("species_", stat, "_Mortality.png")),
        plot = p_mort,
        width = 12, height = 8, dpi = 150
      )

      p_growth <- ggplot(speciesSummary,
                         aes(x = year, y = growthPred, colour = speciesCode, group = speciesCode)
      ) +
        geom_line() +
        geom_point() +
        facet_wrap(~speciesCode) +
        labs(
          title = paste(tools::toTitleCase(stat),
                        "GMCS Growth Predictions by Species"),
          x = "Year",
          y = paste(stat, "growth prediction")
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggsave(
        filename = file.path(outDir,
                             paste0("species_", stat, "_Growth.png")),
        plot = p_growth,
        width = 12, height = 8, dpi = 150
      )
    },

    warning(
      paste("Undefined event type:",
            current(sim)[1, "eventType"],
            "in module gmcsSummary")
    )
  )

  return(invisible(sim))
}