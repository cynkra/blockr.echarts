#' Register ECharts blocks
#'
#' Registers all blocks from the blockr.echarts package.
#'
#' @return NULL (invisibly)
#' @export
register_echarts_blocks <- function() {
  # nocov start
  register_blocks(
    "new_echart_block",
    name = "EChart",
    description = "Create interactive visualizations with Apache ECharts",
    category = "plot",
    icon = "bar-chart-line",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_echart_heatmap_block",
    name = "EChart Heatmap",
    description = "Create heatmap visualizations with Apache ECharts",
    category = "plot",
    icon = "grid-3x3",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_echart_radar_block",
    name = "EChart Radar",
    description = "Create radar chart visualizations with Apache ECharts",
    category = "plot",
    icon = "bullseye",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_echart_sankey_block",
    name = "EChart Sankey",
    description = "Create Sankey flow diagrams with Apache ECharts",
    category = "plot",
    icon = "diagram-3",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_echart_gauge_block",
    name = "EChart Gauge",
    description = "Create gauge/meter visualizations with Apache ECharts",
    category = "plot",
    icon = "speedometer2",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_echart_candlestick_block",
    name = "EChart Candlestick",
    description = "Create candlestick/OHLC charts with Apache ECharts",
    category = "plot",
    icon = "graph-up-arrow",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_echart_calendar_block",
    name = "EChart Calendar",
    description = "Create calendar heatmaps with Apache ECharts",
    category = "plot",
    icon = "calendar3",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_echart_treemap_block",
    name = "EChart Treemap",
    description = "Create treemap visualizations with Apache ECharts",
    category = "plot",
    icon = "grid-fill",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_echart_hierarchy_block",
    name = "EChart Hierarchy",
    description = "Hierarchical filter (sunburst, treemap, tree)",
    category = "plot",
    icon = "diagram-3",
    package = utils::packageName(),
    overwrite = TRUE
  )
} # nocov end
