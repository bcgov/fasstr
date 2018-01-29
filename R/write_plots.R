# Copyright 2017 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#' @title Write a plot in a directory
#'
#' @description Write plots, single or multiple, into a folder or pdf document
#'
#' @param plot Name of plot or name of list of plots to write to a directory.
#' @param file Character string of name of file (single or combined_pdf) or name of folder (mulitple plots) in a directory 
#'    to write the plot(s). Listing the image format (ex. ".png" or ".pdf") will overwrite the \code{format} argument for
#'    single plots.
#' @param format Character string of image type to write. One of "png", "eps", "ps", "tex", "pdf", "jpeg", "tiff", "bmp", or "svg".
#'    Image type will be overwritten if using \code{combined_pdf argument}.
#' @param height Numeric plot height in \code{units}. If not supplied, uses the size of current graphics device.
#' @param width Numeric plot width in \code{units}. If not supplied, uses the size of current graphics device.
#' @param units Character string plot height and width units, one of "in", "cm", or "mm". Default \code{"in"}.
#' @param combined_pdf Logical value indicating whether to combine list of plots into one pdf document. Default \code{FALSE}.
#'
#' @examples
#' \dontrun{
#' 
#' write_plots()
#' 
#' }
#' @export



write_plots <- function(plot = NULL,
                        file = "",
                        format = NULL,
                        height = NA,
                        width = NA, 
                        units = "in",
                        combined_pdf = FALSE){  
  
  
  ## ARGUMENT CHECKS
  ## ---------------
  
  if(is.null(plot)) stop("No plots provided using plot argument.")
  
  if(file == "") stop("file/folder name must be provided using file argument.")
  
  if(!is.logical(combined_pdf))  stop("combined_pdf argument must be logical (TRUE/FALSE).")
  
  # Grab format if format not provided
  if(!combined_pdf) {
    if(is.null(format)) {
      if(!sub('.*\\.', '', file) %in% c("png", "eps", "ps", "tex", "pdf", "jpeg", "tiff", "bmp", "svg"))
        stop("No image file type provided. Please provide using format argument.")
      
      if(sub('.*\\.', '', file) %in% c("png", "eps", "ps", "tex", "pdf", "jpeg", "tiff", "bmp", "svg")) {
        format <- sub('.*\\.', '', file)
        file <- sub(paste0(".", format), '', file)
      }
    }
    
    if(!is.null(format)) {
      if(sub('.*\\.', '', file) %in% c("png", "eps", "ps", "tex", "pdf", "jpeg", "tiff", "bmp", "svg"))
        warning("Provided format will be overwritten by the format provided in the file argument.")
      file <- sub(paste0(".", format), '', file)
    }
  }
  
  
  
  if(combined_pdf) {
    if(sub('.*\\.', '', file) %in% c("png", "eps", "ps", "tex", "jpeg", "tiff", "bmp", "svg"))
      warning("format provided in file will be overridden when using combined_pdf function.")
    if(!is.null(format)) {
      if(format %in% c("png", "eps", "ps", "tex", "jpeg", "tiff", "bmp", "svg"))
        warning("format provided will be overridden when using combined_pdf function.")
    }
    #format <- sub('.*\\.', '', file)
    file <- sub(paste0(".", sub('.*\\.', '', file)), '', file)
  }
  
  if((!is.na(height) & !is.numeric(height)) | (!is.na(width) & !is.numeric(width)))
    stop("height and width arguments must be numeric.")
  if(length(height) !=1 | length(width) !=1) stop("Only one height and width values can be provided.")
  
  if(length(units) != 1)  stop("only one unit type can be provided.")
  if(!units %in% c("in", "cm", "mm"))  stop("Only units of 'in', 'cm', or 'mm' can be provided.")
  
  
  
  ## WRITE THE PLOTS
  ## ---------------
  
  # Plotting for just one image
  if(length(plot) == 1) {
    ggplot2::ggsave(plot = plot,
                    filename = file,
                    height = height,
                    width = width,
                    units = units)
  }
  
  # Plotting for mulitple images
  if(length(plot) > 1) {
    
    if(!combined_pdf) {
      if(is.null(format)) {
        warning("No file format for multiple plots was selected, will use jpeg.")
        format <- "jpeg"
      }
      
      dir.create(paste(file), showWarnings = FALSE)
      
      for (item in names(plot)) {
        suppressWarnings(ggplot2::ggsave(plot = plotsss <- plot[[item]],
                                         filename = paste0(file, "/", item, ".", format),
                                         height = height,
                                         width = width,
                                         units = units)
        )
      }
    }
    
    if(combined_pdf) {
      
      if(is.na(width)) {
        width <- dev.size(units = units)[1]
      }
      
      if(is.na(height)) {
        height <- dev.size(units = units)[2]
      }
      
      pdf(file = paste0(file, ".pdf"), width = width, height = height)
      for (item in names(plot)) {
        suppressWarnings(plot(plot[[item]]))
      }
      invisible(dev.off())
      
    }
  }
  
  
  
}

