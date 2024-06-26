#'Plot variables from a .nc file 
#'
#'Plots variables directly from a .nc file output from a GLM simulation. Replaces function plot_var.
#'@export plot_var plot_var_nc
#'@aliases plot_var
#'@param nc_file a string with the path to the netcdf output from GLM
#'@param var_name a character vector of valid variable names (see \code{\link{sim_vars}})
#'@param fig_path Default is NULL (only plots to screen). Enter string path to save as output file. File type can be anything supported by \code{\link[ggplot2:ggsave]{ggplot2:ggsave}}. See examples. 
#'@param reference String; 'surface' or 'bottom'. Only used for heatmap plots. We recommend using 'bottom' if surface levels are fluctuating. This will present a more realistic representation of surface conditions. 
#'@param legend.title Vector string; Default (`NULL`) will use variable and units from netcdf file
#'@param interval Positive number indicating the depth interval in meters to interpolate output data. Must be less than max depth of lake. Default = 0.5 m. 
#'@param text.size Integer; Default is 12. Higher values will increase text size in plot.
#'@param show.legend Logical; TRUE to show legend (default), FALSE to hide legend
#'@param legend.position String; Legend position. Default is 'right'. Options: 'left','right','top','bottom'
#'@param plot.title Vector string; Default is no title. 
#'@param color.palette See \code{\link[ggplot2:scale_color_distiller]{ggplot2:scale_color_distiller}} . If a string, will use that named palette. Default is 'RdYlBu'. If a number, will index into the list of palettes of appropriate. 
#' Palettes available include: Diverging:
#' BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn. Spectral. Qualitative: Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3. Sequential:
#' Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd.
#'@param zlim Color palette limits for z-variable. Default is maximum range of variable. Set as c(value,value). 
#'@param color.direction Sets the order of colors in the scale. If 1, colors are as output by brewer.pal. If -1, the order of colors is reversed (default).
#'@param ... additional arguments passed to \code{\link[ggplot2:ggsave]{ggplot2:ggsave}} 
#'@keywords methods
#'@seealso \code{\link{get_temp}}, \code{\link{sim_var_longname}}, 
#'\code{\link{sim_vars}}, \code{\link{plot_temp}},  \code{\link{get_var}}
#'@author
#'Jordan S. Read, Luke A. Winslow, Hilary A. Dugan
#'
#'@examples
#'nc_file <- system.file("extdata", "output/output.nc", package = "glmtools")
#'plot_var_nc(nc_file, 'temp')
#'
#'#Plotting two variables
#'plot_var_nc(nc_file, var_name = c('temp','wind'), show.legend = FALSE, 
#'text.size = 14, plot.title = c('My Lake: Temp','My Lake: Wind'))
#'
#'#Change color palette
#'plot_var_nc(nc_file, var_name = 'temp', color.palette = 'PuBuGn', 
#'color.direction = 1, show.legend = FALSE)
#'
#'\dontrun{
#'#'#How to save a plot
#'plot_var_nc(nc_file,var_name = c('temp', 'u_mean'),
#'fig_path = './figtest.png', width = 6, height = 2, units = 'in')
#'
#'# need to specify a valid .nc file here: 
#'plot_var_nc(file = fabm_sim_nc.nc,
#'var_name = 'aed_oxygen_oxy', 
#'fig_path = 'aed_out.png')
#'}
#'@importFrom patchwork wrap_plots
#'@export
plot_var_nc <- function(nc_file = 'output.nc', var_name = 'temp', fig_path = NULL, reference = 'surface', 
                        legend.title = NULL, interval = 0.5, text.size = 12, show.legend = TRUE, 
                        legend.position = 'right', plot.title = NULL, 
                        color.palette = 'RdYlBu', color.direction = -1, zlim = NULL,...) {
  
  heatmaps <- glmtools:::.is_heatmap(nc_file, var_name)
  num_divs <- length(var_name)
  
  is_heatmap = any(heatmaps)
  
  # iterate through plots
  h = list() #for ggplots
  for (j in 1:num_divs){
    if (heatmaps[j]){
      h[[j]] = plot_nc_heatmap(file = nc_file, var_name = var_name[j], reference = reference,
                                legend.title = legend.title[j], interval=interval, text.size = text.size, 
                                show.legend = show.legend, legend.position = legend.position, 
                                plot.title = plot.title[j], 
                                color.palette = color.palette, color.direction = color.direction, zlim = zlim)
    } else {
      h[[j]] = .plot_nc_timeseries(file = nc_file, var_name = var_name[j], 
                                   plot.title = plot.title[j], text.size = text.size)
      
    }
  }
  
  # Saving plot 
  if (!is.null(fig_path)){
    ggsave(plot = wrap_plots(h,ncol = 1), filename = fig_path,...)
  } 
  
  return(wrap_plots(h,ncol = 1))
}


#'@describeIn plot_var_nc Deprecated. Use plot_var_nc
plot_var = plot_var_nc
