#Set of helper functions
#############################################################
#Custom label format for leaflet maps that have a color gradient for each point that is based on a timestamp
myLabelFormat = function(...,dates=FALSE){ 
  if(dates){ 
    function(type = "numeric", cuts){ 
      as.Date(cuts, origin="1970-01-01")
    } 
  }else{
    labelFormat(...)
  }
}

#############################################################
#Custom icons creation function with shapes from base R
# this is modified from 
# https://github.com/rstudio/leaflet/blob/master/inst/examples/icons.R#L24
pchIcons = function(pch = 1, width = 20, height = 20, bg = "transparent", col = "black", ...) {
  n = length(pch)
  files = character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f = tempfile(fileext = '.png')
    png(f, width = width, height = height, bg = bg)
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = pch[i], col = col[i], cex = min(width, height) / 8, ...)
    dev.off()
    files[i] = f
  }
  files
}


#############################################################
library(sp)
library(maptools)

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

#############################################################
#Finds the start and end points of a continuous sequence in a vector. npts: number of consecutive points where speed is <0.1
Find_index <- function(x,npts=5) {
  with(rle(x), {
  ok <- values < 0.1 & lengths > npts
  ends <- cumsum(lengths)
  starts <- ends - lengths + 1
  data.frame(starts, ends)[ok, ]
})}


#############################################################
#Function for pdf or html output of tables
knit_table <- function(df){
  if (is_html_output()) {
    df %>%
      kable("html", escape = F) %>%
      kable_styling()
  } else {
    df <- data.frame(lapply(df, function(x) {gsub("<br>", "\n", x)}), stringsAsFactors = F)
    
    df %>%  
      mutate_all(linebreak) %>%
      kable("latex", booktabs = T, escape = F)  
  }
}

#############################################################
#To create a mapping function for the maps of the introduction. To do.

#############################################################

## Custom colour palette

ColorPalette <- c("#449BCF", "#82C893", "#D696C8", "#FF5F68", "#FF9252", "#FFC95B", "#056A89")


## Adding custom legend in leaflet maps
addLegendCustom <- function(map, colors, sizes, labels, opacity = 1, title='fill in your title', position="topright"){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", 
                           sizes, "px; inline-block; vertical-align: bottom; ")
  labelAdditions <- paste0("<div style='display: inline-block; vertical-align: middle; top: ",
                           sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>",
                           labels, "</div>")
  #position <- 'topright'
  
  return(addLegend(map, position=position, colors = colorAdditions, labels = labelAdditions,
                   opacity = opacity, title = title))
}


# Function to create a circle size legend in Leaflet

# This takes as input a variable to determine the size range,
# a number of sizes to show, and a label and writes the HTML
# to create the legend.
#
# It requires some custom CSS that must be included somewhere:
#
# .legendCircle {
#   border-radius:50%;
#   border: 2px solid black;
#   display: inline-block;
#   position: relative;
# }
#
# There is no guarantee that the sizes will be comparable, so
# use with caution.  This assumes you are mapping circle radius
# to the square root of the value.  If some other function is 
# being used, if should be changed in the function.

circleSizeLegend <- function(x,n,label) {
  # x <- tmp$Density
  # label <- 'density'  
  # n <- 5
  # Get the range
  
  max.x = max(x)
  min.x = min(x)
  
  # Set up the initial header for the code
  # The width is set to be reasonable and make the spacing work.
  # It really should be done more intelligently.
  
  myhtml <- paste0("<div id='customlegend' style='width:90px'><strong>",label,"</strong><br>")
  
  # Loop over the number of divisions  
  
  myvalue = round(max.x, 0)
  
  size = round(sqrt(myvalue) * 2, 1)
  margin = 16 # This is just a good starting value visually
  
  for(i in 1:n){
    
    myhtml <- paste0(myhtml,"<div class = 'legendCircle' style='width: ",size,"px;height: ",
                     size,"px; margin-left:",margin,"px'></div><span style='position:absolute;right:8px'>",
                     myvalue,"</span><br>")
    
    if (i == n){break}
    
    # Compute value for next iteration
    
    myvalue = round(max.x - (i * ((max.x - min.x) / (n - 1))),0)
    oldsize = size
    size = round(sqrt(myvalue) * 2, 1)
    margin = margin + (oldsize - size) / 2
    
  }
  
  # Close up the div
  
  myhtml <- paste0(myhtml,"</div>")
  
  return(myhtml)
}


########################
#JSON2diary_df
JSON2diary_df <- function(json_path){
activities = jsonlite::fromJSON(txt = json_path, simplifyDataFrame = T, flatten = T)
activities<-activities %>% dplyr::select("activityTypeName","activityTypeCode","superstationNumber",
                                         "localstationNumber","activityNumber","startTime","endTime",
                                         "comment", "startPosition.coordinates","endPosition.coordinates",
                                         "fields") 

fields <- map(activities$fields, ~ (.x %>% select(-"extendedValue")))
names(fields) <- 1:length(fields)

fields <- as.data.frame(do.call(rbind, fields)) %>% 
  filter(name %in% c("bottom_depth_start","bottom_depth_end","log_start","log_end",
                     "course","gear_code_spd","region","remark_start","remark_end","longitude_start",
                     "latitude_start","longitude_end","latitude_end","Kode (SPD)","Redskapsnummer","Serienummer")) %>% 
  dplyr::select(name,type,value)

fields <- fields %>% mutate(ID = str_extract(rownames(fields), "[0-9]+")) %>% pivot_wider(ID)

activities<-cbind(activities, fields)

activities <- activities %>% dplyr::select(-endPosition.coordinates,-startPosition.coordinates,-fields) %>% 
  filter(!activityTypeCode==0) %>% slice(rep(1:n(), each = 2)) %>% 
  mutate(event=ifelse(row_number()%%2,  "S","E"), survey=survey,
         gear_code_spd=ifelse(is.na(gear_code_spd), `Kode (SPD)`, gear_code_spd)) %>% 
  dplyr::select(-`Kode (SPD)`)
for(i in 1:nrow(activities)){
  if(activities$event[i]=="S"){
    activities[i, names(activities) %in% c("longitude_end", "latitude_end", "endTime", "log_end", "remark_end")] = NA
  } else {
    activities[i, names(activities) %in% c("longitude_start", "latitude_start", "startTime", "log_start", "remark_start")] = NA
  }
}

return(activities)
}


##############################
## generate_YML.fn
#' @title Generate YML file according to output requirements
#' @description Generate _bookdown.yml file according to output requirements from parameters section
#' @param chapters String from interactively defined parameters specified at the beginning of the YAML file. 
#' @return print a new _bookdown.yml according to the chapter selection

generate_YML.fn <- function(chapters){
  
  if(chapters=="Full Report") my_files = list.files(pattern='.Rmd')
  if(chapters=="Sailing Order only") my_files = c("index.Rmd",list.files(pattern='06-'))
  if(chapters=="Survey report only") my_files = c("index.Rmd","01-introduction.Rmd","02-methods.Rmd","03-results.Rmd","04-concl_remarks.Rmd","05-references.Rmd")
  
  my_yml <- paste0(
  "book_filename: 'Nansen_survey_report'
delete_merged_file: true
language:
  ui:
    chapter_name: \"\""
)

  # create the _bookdown.yml
  cat(my_yml,
    "\nrmd_files: [\n  ", paste0(my_files, collapse = ", \n  "), "\n]",
    file = "_bookdown.yml", sep = "")

}






