##############################
# File version 2020-02-27 ####
# Author and contact: mikko.vihtakari@hi.no

#' @title Generate data for species overview plots
#' @description Generates data required by species overview plots in Biotic Explorer
#' @param data stnall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$stnall}.
#' @return Returns a list of tibbles containing data required by station-based various plots.
#' @import dplyr data.table

speciesOverviewData <- function(data) {
  #data <- out$stnall

  # Number of stations
  
  nStn <- data %>% dplyr::group_by(commonname) %>% dplyr::summarise(n = length(unique(paste(startyear, serialnumber))))
  nStn <- nStn[order(-nStn$n),]
  nStn$commonname <- factor(nStn$commonname, nStn$commonname)
  
  # Catch weight
  
  catchW <- data[!is.na(data$catchweight),]
  
  catchS <- catchW %>% dplyr::group_by(commonname) %>% 
    dplyr::summarise(mean = mean(catchweight, na.rm = TRUE), se = se(catchweight), max = max(catchweight, na.rm = TRUE), min = min(catchweight, na.rm = TRUE), sum = sum(catchweight, na.rm = TRUE))
  catchS <- catchS[order(-catchS$sum),]
  catchS$commonname <- factor(catchS$commonname, catchS$commonname)
  catchS$se[is.na(catchS$se)] <- 0
  catchW$commonname <- factor(catchW$commonname, catchS$commonname)
  
  # Mean weight
  
  meanW <- data %>% filter(!is.na(catchweight) & catchweight > 0 & !is.na(catchcount) & catchcount > 0) %>% 
    group_by(commonname, cruise, startyear, serialnumber) %>% summarise(weight = sum(catchweight), n = sum(catchcount), indw = weight/n)
  meanW <- droplevels(meanW)
  meanW <- meanW %>% group_by(commonname) %>% summarise(mean = mean(indw), min = min(indw), max = max(indw), sd = sd(indw), se = se(indw)) %>% arrange(-mean)
  meanW$commonname <- factor(meanW$commonname, meanW$commonname)
  
  # Numbers in catch
  
  catchN <- data[!is.na(data$catchcount) & data$catchcount > 0,]
  
  meanN <- catchN %>% group_by(commonname) %>% summarise(mean = mean(catchcount), se = se(catchcount), max = max(catchcount), min = min(catchcount), Nstn = length(unique(paste(cruise, startyear, serialnumber))))
  meanN <- meanN[order(-meanN$mean),]
  meanN$se[is.na(meanN$se)] <- 0
  meanN$commonname <- factor(meanN$commonname, meanN$commonname)
  catchN$commonname <- factor(catchN$commonname, meanN$commonname)
  
  # Catch by gear
  
  catchGBase <- data[!is.na(data$catchweight),]
  catchG <- catchGBase %>% group_by(gear, commonname) %>% 
    summarise(sum = sum(catchweight))
  
  catchG$commonname <- factor(catchG$commonname, catchS$commonname)
  
  # Bottom depth and fishing depth by station
  
  stnD <- data %>% group_by(cruise, startyear, serialnumber) %>% 
    summarise(bdepth = unique(bottomdepthstart), fdepth = unique(fishingdepthmin))

  stnD <- data.table::melt(data.table::as.data.table(stnD), id.vars = 1:3)
  stnD$variable <- dplyr::recode_factor(stnD$variable, "bdepth" = "Bottom depth (start)", "fdepth" = "Minimum fishing depth")
  
  # catch composition data
  
  compDat <- data %>% filter(!is.na(catchweight)) %>% 
    group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, fishingdepthmin, commonname) %>%
    summarise(catchweight = sum(catchweight)) 
  
  sumCompDat <- compDat %>% group_by(commonname) %>% summarise(sum = sum(catchweight)) %>% arrange(-sum)
  
  compDat$commonname <- factor(compDat$commonname, sumCompDat$commonname)
  
  if (length(sumCompDat$commonname) > 6) {
    levels(compDat$commonname)[!levels(compDat$commonname) %in% sumCompDat$commonname[1:6]] <- "Andre arter"
  }
  
  levels(compDat$commonname) <- gsub("(^[[:alpha:]])", "\\U\\1", levels(compDat$commonname), perl = TRUE)    
  
  compDat <- compDat %>% 
    group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, fishingdepthmin, commonname, .drop = FALSE) %>% 
    summarise(catchweight = sum(catchweight)) %>% 
    arrange(cruise, startyear, serialnumber, commonname)
  
  compDatW <- tidyr::spread(compDat, commonname, catchweight)
  
  compDatW$total <- rowSums(compDatW[,levels(compDat$commonname)]) 
  
  # Return
  
  list(nStn = nStn, catchW = catchW, catchS = catchS, meanW = meanW, catchN = catchN, meanN = meanN, catchG = catchG, stnD = stnD, compDat = compDat, compDatW = compDatW)
}

#' @title Plot Number of stations containing a species
#' @description Plots a species composition in an \link[=processBioticFile]{bioticProcData} object. 
#' @param data data object from \link{speciesOverviewData}. Requires the nStn data frame.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

speciesCompositionPlot <- function(data, base_size = 14) {

  x <- data$nStn
  
  ggplot(x, aes(y = n, x = commonname)) + 
    geom_col() +
    ylab("Number of stations containing the species") +
    xlab("Species database name") +
    coord_cartesian(expand = FALSE, ylim = range(pretty(x$n))) + 
    theme_bw(base_size = base_size) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot summed catch weight
#' @description Plots summed catch weights in an \link[=processBioticFile]{bioticProcData} object. 
#' @param data data object from \link{speciesOverviewData}. Requires the catchS data frame.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchweightSumPlot  <- function(data, base_size = 12) {

  x <- data$catchS
  
  ggplot(x, aes(x = commonname, y = sum)) +
    geom_col() +
    scale_y_log10("Summed catch weight [log10(kg)]") +
    xlab("Species database name") +
    coord_cartesian() +
    theme_bw(base_size = base_size) +
    annotate("text", x = Inf, y = Inf, label = paste("Total catch\n all species\n", round(sum(x$sum), 0), "kg"), vjust = 1, hjust = 1, size = 5) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

## 

#' @title Plot mean catch weight
#' @description Plots mean catch weights in an \link[=processBioticFile]{bioticProcData} object. Error bars are standard error of the mean.
#' @param data data object from \link{speciesOverviewData}. Requires the catchS data frame.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchweightMeanPlot  <- function(data, base_size = 14) {

  x <- data$catchS
  
  ggplot(x, aes(x = commonname, y = mean, ymax = mean + se, ymin = mean - se)) +
    geom_linerange() +
    geom_point() +
    ylab("Mean catch weight (kg; +/- SE)") +
    xlab("Species database name") +
    coord_cartesian() +
    theme_bw(base_size = base_size) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot range of catch weights
#' @description Plots the range of catch weights in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the catchS and catchW data frames.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchweightRangePlot  <- function(data, base_size = 14) {

  x <- data$catchS
  y <- data$catchW
  
  ggplot() +
    geom_linerange(data = x, aes(x = commonname, ymax = max, ymin = min), color = "red") +
    geom_point(data = y, aes(x = commonname, y = catchweight), size = 1, shape = 21) +
    scale_y_log10("Catch weight range [log10(kg)]") +
    xlab("Species database name") +
    coord_cartesian() +
    theme_bw(base_size = base_size) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot mean weight of fish in catch
#' @description Plots mean weight of fish in catch in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the meanW data frame.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchIndMeanWeightPlot <- function(data, base_size = 14) {

  x <- data$meanW
  
  ggplot(x, aes(x = commonname, y = mean, ymin = min, ymax = max)) +
    geom_pointrange() +
    scale_y_log10("Mean specimen weight (kg +/- range)", labels = scales::number_format(accuracy = 0.001)) +
    xlab("Species database name") +
    theme_bw(base_size = base_size) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot mean number in catches
#' @description Plots mean number of fish in catches in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the meanN data frame.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchcountMeanPlot <- function(data, base_size = 14) {

  x <- data$meanN
  
  ggplot(x, aes(x = commonname, y = mean, ymax = mean + se, ymin = mean - se)) +
    geom_linerange() +
    geom_point() +
    ylab("Mean number in catch (+/- SE)") +
    xlab("Species database name") +
    theme_bw(base_size = base_size) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot range of number in catches
#' @description Plots range of number of fish in catches in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the meanN and catchN data frames.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchcountRangePlot <- function(data, base_size = 14) {

  x <- data$meanN
  y <- data$catchN
  
  ggplot() +
    geom_linerange(data = x,
                   aes(x = commonname, ymax = max, ymin = min), color = "red") +
    geom_point(data = y,
               aes(x = commonname, y = catchcount), size = 1, shape = 21) +
    scale_y_log10("Range for number in catch (log10)") +
    xlab("Species database name") +
    coord_cartesian() +
    theme_bw(base_size = base_size) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot catch by species and gear code
#' @description Plots total catch by fish and gear type in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the catchG data frame.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

gearCatchPlot <- function(data, base_size = 14) {

  x <- data$catchG
  ggplot(x, aes(x = commonname, y = as.factor(gear),
                          size = sum, color = sum)) +
    geom_point() +
    scale_color_distiller(name = "Total catch [log10(kg)]",
                          palette = "Spectral", trans = "log10",
                          breaks = c(1 %o% 10^(-4:4))
    ) +
    scale_size(name = "Total catch [log10(kg)]", trans = "log10",
               breaks = c(1 %o% 10^(-4:4), range = c(1,8))
    ) +
    ylab("Gear code") +
    xlab("Species database name") +
    theme_bw(base_size = base_size) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot bottom depth and minimum fishing depth distribution for stations
#' @description Plots bottom depth and minimum fishing depth distribution for stations in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the stnD data frame.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

stationDepthPlot <- function(data, base_size = 14) {

  x <- data$stnD
  ggplot(x, aes(x = value)) +
    geom_histogram(binwidth = 100, color = "black", fill = "grey") +
    facet_wrap(~variable) +
    scale_y_continuous("Count", expand = c(0, 0)) +
    scale_x_continuous("Depth (m)", expand = c(0,0.05)) +
    expand_limits(x = 0) +
    theme_classic(base_size = base_size) +
    theme(strip.background = element_blank())
  
}

#' @title Plot minimum fishing depth by catch for six most dominant species
#' @description Plots minimum fishing depth by catch for six most dominant species in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the compDat data frame.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchSpeciesWeightPlot <- function(data, base_size = 14) {

  x <- data$compDat
  
  ggplot(x[x$commonname != "Andre arter",], aes(x = fishingdepthmin, y = catchweight, group = commonname)) +
    geom_smooth(se = FALSE) +
    geom_point() +
    ylab("Catch weight (kg)") + 
    xlab("Minimum fishing depth (m)") +
    facet_wrap(~commonname, scales = "free_y") + 
    theme_bw(base_size = base_size)
}

#' @title Plot catch composition on a map
#' @description Plots catch composition in an \link[=processBioticFile]{bioticProcData} object on a \link[leaflet]{leaflet} map.
#' @param data data object from \link{speciesOverviewData}. Requires the compDat data frame.
#' @return Returns a \link[leaflet]{leaflet} object
#' @import leaflet

catchCompMap <- function(data) {
  x <- data$compDatW
  y <- data$compDat
  
  leaflet::leaflet() %>% 
    addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
             attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
    addMinicharts(
      x$longitudestart, x$latitudestart,
      type = "pie", chartdata = x[,levels(y$commonname)],
      colorPalette = ColorPalette,
      width = 40 * log(x$total) / log(max(x$total)), 
      transitionTime = 0
    )
  
}


#' #' @title Plot total catch of a specific species on a map
#' #' @description Plots total catch of a species in an \link[=processBioticFile]{bioticProcData} object on a \link[leaflet]{leaflet} map.
#' #' @param data stnall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$stnall}.
#' #' @param species NMD commonname for a species. The names are in Norwegian. Use "All" to plot a sum of total catch including all species.
#' #' @return Returns a \link[leaflet]{leaflet} object
#' #' @import leaflet
#' 
#' catchMap <- function(data, species, region) {
#'   #species = "All"
#'   # species <- prior_dem_taxa_list[[1]]
#'   #region <- levels(stn_demersal$region)[1]
#'   #data <- stn_demersal
#'   ## Definitions
#'   
#'   if (species == "All") {
#'     sps <- unique(data$commonname)
#'   } else {
#'     sps <- species
#'   }
#'   
#'   if (region == "All") {
#'     rgn <- unique(data$region)
#'   } else {
#'     rgn <- region
#'   }
#'   
#'   ## Prepare data
#'   tmp <- as.data.frame(data) %>% 
#'     filter(commonname %in% sps & !is.na(longitudestart) & !is.na(latitudestart) & region %in% rgn) %>% 
#'     # group_by(startyear, serialnumber, region, longitudestart, 
#'     #          latitudestart, gear, bottomdepthstart, stationstartdate) %>% 
#'     # summarize(catchsum = round(sum(catchweight, na.rm = TRUE), 3),
#'     #           Kgperhour = round(sum(Kgperhour, na.rm = TRUE), 6),
#'     #           TonnesperNM2 = round(sum(TonnesperNM2, na.rm = TRUE), 6)) %>% 
#'     data.frame()
#'   #Need to find a way to plot the legend properly 
#'   #mutate(interval=ggplot2::cut_width(tmp$Kgperhour/max(tmp$Kgperhour), 0.2, boundary = 0))
#'   
#'   tmp2 <- as.data.frame(data) %>% dplyr::filter(!is.na(longitudestart) & !is.na(latitudestart) & region %in% rgn) 
#'   ## addition for Nansen ###########
#'   standardization.time = ifelse(soaktime>1, 60/soaktime, 1/soaktime)
#'   #tmp2 <- tmp2 %>% mutate(Kgperhour=catchweight*standardization.time, TonnesperNM2=(catchweight)/(1000*(18.52/1852)*distance)) 
#'   ##################################
#'   tmp2 <- tmp2[!paste(tmp2$startyear, tmp2$serialnumber, sep = "_") %in% paste(tmp$startyear, tmp$serialnumber, sep = "_"), !names(tmp2) %in% c("catchsampleid", "commonname", "catchcategory", "catchpartnumber", "catchweight", "catchcount", "lengthsampleweight", "lengthsamplecount")]
#'   
#'   if (nrow(tmp2) > 0) tmp2$catchsum <- 0
#'   ## addition for Nansen ###########
#'   if (nrow(tmp2) > 0) {tmp2$Kgperhour <- 0; tmp2$TonnesperNM2 <- 0}
#'   #################################
#'   #hist(tmp$TonnesperNM2, breaks=100)
#'   ## Plot
#'   # graduated radius
#'   # sty <- styleGrad(prop="TonnesperNM2", breaks=c(10^-5, 10^-4, 10^-2, 10^-1, 0.5, 1), style.par="rad",
#'   #                  style.val=c(2,5,9,14,20), leg="tonnes*NM<sup>-2<sup>")
#'   #range(tmp$TonnesperNM2)
#'   breaks=c(10^-5, 10^-4, 10^-2, 10^-1, 0.5, 1, 5, 10)
#'   sizes = c(2, 2, 3, 4, 7, 10, 15, 20, 25)
#'   tmp <- tmp %>% mutate(TonnesperNM2Class = case_when(
#'     between(tmp$TonnesperNM2, breaks[1], breaks[2]) ~ as.character(sizes[2]),
#'     between(tmp$TonnesperNM2, breaks[2], breaks[3]) ~ as.character(sizes[3]),
#'     between(tmp$TonnesperNM2, breaks[3], breaks[4]) ~ as.character(sizes[4]),
#'     between(tmp$TonnesperNM2, breaks[4], breaks[5]) ~ as.character(sizes[5]),
#'     between(tmp$TonnesperNM2, breaks[5], breaks[6]) ~ as.character(sizes[6]),
#'     between(tmp$TonnesperNM2, breaks[6], breaks[7]) ~ as.character(sizes[7]),
#'     between(tmp$TonnesperNM2, breaks[7], breaks[8]) ~ as.character(sizes[8]),
#'     tmp$TonnesperNM2 > breaks[8] ~ as.character(sizes[9])))
#'   
#'   
#'   #getRadius(tmp$TonnesperNM2, breaks = breaks, sizes = sizes)
#'   p <- leaflet::leaflet(tmp, options = leafletOptions(zoomControl = FALSE)) %>% 
#'     #setView(-14, 8, zoom = 6) %>% 
#'     addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
#'              attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>%
#'     # addCircles(lat = ~ latitudestart, lng = ~ longitudestart, 
#'     #            #weight = 4, radius = 5e4*(tmp$catchsum/max(tmp$catchsum)),
#'     #            ## replacement for Nansen
#'     #            #weight = 4, radius = 5e4*(tmp$Kgperhour/max(tmp$Kgperhour)),
#'     #            weight = 1, radius = tmp$TonnesperNM2, #radius is in m units by default that is why I multiply by 1e2 to convert to cm that is used in the legend function.
#'     #            label = paste0(tmp$serialnumber, "; ", tmp$catchsum, " kg"), 
#'     #            popup = paste("Serial number:", tmp$serialnumber, "<br>",
#'     #                          "Date:", tmp$stationstartdate, "<br>",
#'     #                          "Gear code:", tmp$gear, "<br>",
#'     #                          "Bottom depth:", round(tmp$bottomdepthstart, 0), "m",
#'     #                          #"<br>", species, "catch:", tmp$catchsum, 
#'   #                          #"kg"), 
#'   #                          ## replacement for Nansen
#'   #                          "<br>", species, "density:", tmp$TonnesperNM2, 
#'   #                          #"kg per hour"), 
#'   #                          "tonnes*NM<sup>-2<sup>"),
#'   #            color = "red", fill = NA
#'   # ) %>%
#'   addCircleMarkers(lat = ~ latitudestart, lng = ~ longitudestart, 
#'                    #weight = 4, radius = 5e4*(tmp$catchsum/max(tmp$catchsum)),
#'                    ## replacement for Nansen
#'                    #weight = 4, radius = 5e4*(tmp$Kgperhour/max(tmp$Kgperhour)),
#'                    #weight = 2, radius = as.numeric(tmp$TonnesperNM2), #radius is in px
#'                    weight = 2, radius = as.numeric(1e1*(tmp$TonnesperNM2/max(tmp$TonnesperNM2))), #radius is in px
#'                    label = paste0(tmp$serialnumber, "; ", tmp$catchsum, " kg"), 
#'                    popup = paste("Serial number:", tmp$serialnumber, "<br>",
#'                                  "Date:", tmp$stationstartdate, "<br>",
#'                                  "Gear code:", tmp$gear, "<br>",
#'                                  "Bottom depth:", round(tmp$bottomdepthstart, 0), "m",
#'                                  #"<br>", species, "catch:", tmp$catchsum, 
#'                                  #"kg"), 
#'                                  ## replacement for Nansen
#'                                  "<br>", species, "density:", tmp$TonnesperNM2, 
#'                                  #"kg per hour"), 
#'                                  "tonnes*NM<sup>-2<sup>"),
#'                    color = "red", fill = NA) 
#'   # %>%
#'     #circleSizeLegend(x = tmp$TonnesperNM2, n = 5, label = "TonnesperNM2")
#'     # addLegendCustom(title = paste0(species, "<br>", "TonnesperNM2 (Tonnes*NM<sup>-2</sup> )"),
#'     #                 colors = c("black", rep("red",8)),
#'     #                 labels = c("Not caught",
#'     #                            "< 10<sup>-4<sup>",
#'     #                            "10<sup>-4</sup> - 10<sup>-2</sup>",
#'     #                            "10<sup>-2</sup> - 10<sup>-1</sup>",
#'     #                            "10<sup>-1<sup> - 0.5",
#'     #                            "0.5 - 1",
#'     #                            "1 - 5",
#'     #                            "5 - 10",
#'     #                            ">10"),
#'     #                 #sizes = c(0.1, 0.5, 1, 1.5, 1.75, 2.0, 3.0))
#'     #                 sizes = sizes*2 #For some reason this *2 seems to be needed for the legend circles to have the right size. Perhaps it has to do with the 50% in the CustomLegend function. Need to investigate
#'     # )
#'   
#'   
#'   
#'   
#'   if (nrow(tmp2) > 0) {
#'     p %>% 
#'       addCircles(lat = tmp2$latitudestart, lng = tmp2$longitudestart, 
#'                  weight = 1, radius = 1, 
#'                  label = paste0(tmp2$serialnumber, "; ", tmp2$catchsum, " kg"), 
#'                  popup = paste("Serial number:", tmp2$serialnumber, "<br>",
#'                                "Date:", tmp2$stationstartdate, "<br>",
#'                                "Gear code:", tmp2$gear, "<br>",
#'                                "Bottom depth:", round(tmp2$bottomdepthstart, 0), "m",
#'                                #"<br>", species, "catch:", tmp2$catchsum, 
#'                                #"kg"),
#'                                ## replacement for Nansen
#'                                "<br>", species, "density:", tmp$TonnesperNM2,
#'                                #"kg per hour"), 
#'                                "tonnes*NM<sup>-2<sup>"), 
#'                  color = "black"
#'       ) 
#'   } else {
#'     p 
#'   }
#'   
#' }


#' @title Plot catch rates of a taxon on a map
#' @description Plot catch rates of a species or a group (taxonomic or other) on a map \link[=processBioticFile]{bioticProcData} object on a \link[leaflet]{leaflet} map.
#' @param data stnall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$stnall}.
#' @param species NMD scientificname for a taxon. Use "All" to plot a sum of total catch including all taxa.
#' @return Returns a \link[leaflet]{leaflet} object
#' @import leaflet

catchMapGroups <- function(data, taxon, taxon.level, zoom=7, legend.position=c("bottomleft"), jitter.map.v=0, jitter.map.h=0) {
  #'   #taxon = "All"
  #'   # taxon <- "Selar crumenophthalmus"
  #'   #region <- levels(stnall$region)
  #'   #data <- stn_demersal
  #taxon.level='species'
  ## Definitions

  if (taxon == "All") {
    taxa <- unique(data$scientificname)
  } else {
    taxa <- taxon
  }
  
  ## Prepare data
  
  tmp2 <- data %>% lazy_dt() %>% filter(!is.na(longitudestart) & !is.na(latitudestart)) %>% collect()
    ## addition for Nansen ###########
  # standardization.time = ifelse(soaktime>1, 60/soaktime, 1/soaktime)
  # mutate(CPUE=catchweight*standardization.time,
  #        Density=(catchweight)/(1000*(18.52/1852)*distance)) %>% collect() 
  #################################
  if(taxon.level=='species'){
    tmp <- tmp2 %>% lazy_dt() %>% 
      filter(scientificname %in% taxa) %>% 
      # group_by(startyear, serialnumber, longitudestart, latitudestart, gear, bottomdepthstart, stationstartdate) %>% 
      # summarize(catchsum = round(sum(catchweight, na.rm = TRUE), 2),
      #           CPUE = round(sum(CPUE, na.rm = TRUE), 6),
      #           Density = round(sum(Density, na.rm = TRUE), 6)) %>% 
      collect()
  } else {
    tmp <- tmp2 %>% lazy_dt() %>% 
      filter(get(taxon.level) %in% taxa) %>% #get returns the column name to be used in the filtering when the object taxon.level is a character
      # group_by(startyear, serialnumber, longitudestart, latitudestart, gear, bottomdepthstart, stationstartdate) %>% 
      # summarize(catchsum = round(sum(catchweight, na.rm = TRUE), 2),
      #           CPUE = round(sum(CPUE, na.rm = TRUE), 6),
      #           Density = round(sum(Density, na.rm = TRUE), 6)) %>% 
      collect() 
  }
  #Need to find a way to plot the legend properly 
  #mutate(interval=ggplot2::cut_width(tmp$CPUE/max(tmp$CPUE), 0.2, boundary = 0))
  
  #tmp2 is created to have the absence stations
  tmp2 <- as.data.frame(data) %>% dplyr::filter(!is.na(longitudestart) & !is.na(latitudestart)) 
  ## addition for Nansen ###########
  #tmp2 <- tmp2 %>% mutate(CPUE=catchweight*60/soaktime, Density=(catchweight)/(1000*(18.52/1852)*distance)) 
  ##################################
  tmp2 <- tmp2[!paste(tmp2$startyear, tmp2$serialnumber, sep = "_") %in% paste(tmp$startyear, tmp$serialnumber, sep = "_"), 
               !names(tmp2) %in% c("catchsampleid", "commonname", "catchcategory", "catchpartnumber", "catchweight", 
                                   "catchcount", "lengthsampleweight", "lengthsamplecount")]
  
  if (nrow(tmp2) > 0) tmp2$catchsum <- 0
  ## addition for Nansen ###########
  if (nrow(tmp2) > 0) {tmp2$CPUE <- 0; tmp2$Density <- 0}
  #################################
  #hist(tmp$Density, breaks=100)
  ## Plot
  # graduated radius
  # sty <- styleGrad(prop="Density", breaks=c(10^-5, 10^-4, 10^-2, 10^-1, 0.5, 1), style.par="rad",
  #                  style.val=c(2,5,9,14,20), leg="tonnes*NM<sup>-2<sup>")
  #range(tmp$Density)
  #breaks=c(10^-5, 10^-4, 10^-2, 10^-1, 0.5, 1, 5, 10)
  breaks=c(10^-4, 10^-2, 10^-1, 0.5, 1, 5)
  #sizes = c(2, 2, 3, 4, 7, 10, 15, 20, 25)
  sizes = c(2, 3, 4, 7, 10, 15, 20)
  tmp <- as.data.frame(tmp) %>% mutate(DensityClass = case_when(
    between(tmp$TonnesperNM2, breaks[1], breaks[2]) ~ as.character(sizes[2]),
    between(tmp$TonnesperNM2, breaks[2], breaks[3]) ~ as.character(sizes[3]),
    between(tmp$TonnesperNM2, breaks[3], breaks[4]) ~ as.character(sizes[4]),
    between(tmp$TonnesperNM2, breaks[4], breaks[5]) ~ as.character(sizes[5]),
    between(tmp$TonnesperNM2, breaks[5], breaks[6]) ~ as.character(sizes[6]),
    #between(tmp$TonnesperNM2, breaks[6], breaks[7]) ~ as.character(sizes[7]),
    #between(tmp$TonnesperNM2, breaks[7], breaks[8]) ~ as.character(sizes[8]),
    #tmp$TonnesperNM2 > breaks[8] ~ as.character(sizes[9])))
    tmp$TonnesperNM2 > breaks[6] ~ as.character(sizes[7])))
  tmp$DensityClassnumeric <- as.numeric(tmp$DensityClass)
  
  
  ## Plot
  p <- leaflet::leaflet(tmp, options = leafletOptions(zoomControl = FALSE)) %>% 
    #setView(-3.75, 5, zoom = zoom) %>% 
    setView(mean(data$longitudestart)+jitter.map.h, mean(data$latitudestart)+jitter.map.v, zoom = zoom) %>%									
    addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
             #attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>%
             attribution = NULL) %>%
    # addCircles(lat = ~ latitudestart, lng = ~ longitudestart, 
    # weight = 4, radius = 5e4*(tmp$catchsum/max(tmp$catchsum)), 
    # label = paste0(tmp$serialnumber, "; ", tmp$catchsum, " kg"), 
    # popup = paste("Serial number:", tmp$serialnumber, "<br>",
    # "Date:", tmp$stationstartdate, "<br>",
    # "Gear code:", tmp$gear, "<br>",
    # "Bottom depth:", round(tmp$bottomdepthstart, 0), "m",
    # "<br>", species, "catch:", tmp$catchsum, 
    # "kg"), 
    # color = "red", fill = NA
    # ) 
  addCircleMarkers(lat = ~ latitudestart, lng = ~ longitudestart, 
                   #weight = 4, radius = 5e4*(tmp$catchsum/max(tmp$catchsum)),
                   ## replacement for Nansen
                   #weight = 4, radius = 5e4*(tmp$CPUE/max(tmp$CPUE)),
                   weight = 2, radius = as.numeric(tmp$DensityClass), #radius is in px
                   label = paste0(tmp$serialnumber, "; ", tmp$catchsum, " kg"), 
                   popup = paste("Serial number:", tmp$serialnumber, "<br>",
                                 "Date:", tmp$stationstartdate, "<br>",
                                 "Gear code:", tmp$gear, "<br>",
                                 "Bottom depth:", round(tmp$bottomdepthstart, 0), "m",
                                 #"<br>", species, "catch:", tmp$catchsum, 
                                 #"kg"), 
                                 ## replacement for Nansen
                                 "<br>", taxa, "density:", tmp$Density, 
                                 #"kg per hour"), 
                                 "tonnes*NM<sup>-2<sup>"),
                   color = "red", fill = NA
  ) %>%
    #circleSizeLegend(x = tmp$Density, n = 5, label = "Density")    
    addLegendCustom(title = paste0(taxa, "<br>", "Density (Tonnes*NM<sup>-2</sup> )"),
                    position = legend.position,
                    colors = c("black", rep("red",6)),
                    labels = c("Not caught", 
                               #"< 10<sup>-4<sup>",
                               "10<sup>-4</sup> - 10<sup>-2</sup>",
                               "10<sup>-2</sup> - 10<sup>-1</sup>",
                               "10<sup>-1<sup> - 0.5",
                               "0.5 - 1",
                               "1 - 5",
                               #"5 - 10",
                               ">5"),
                    #sizes = c(0.1, 0.5, 1, 1.5, 1.75, 2.0, 3.0))
                    sizes = sizes*2 #For some reason this *2 seems to be needed for the legend circles to have the right size. Perhaps it has to do with the 50% in the CustomLegend function. Need to investigate
    )  
  if (nrow(tmp2) > 0) {
    p %>% 
      addCircles(lat = tmp2$latitudestart, lng = tmp2$longitudestart, 
                 weight = 4, radius = 1, 
                 label = paste0(tmp2$serialnumber, "; ", tmp2$catchsum, " kg"), 
                 popup = paste("Serial number:", tmp2$serialnumber, "<br>",
                               "Date:", tmp2$stationstartdate, "<br>",
                               "Gear code:", tmp2$gear, "<br>",
                               "Bottom depth:", round(tmp2$bottomdepthstart, 0), "m",
                               "<br>", taxa, "catch:", tmp2$catchsum, 
                               "kg"), 
                 color = "black"
      )
  } else {
    p
  }
  
}



NASCMap <- function(data, species) {
  #data <- tmp
  p <- leaflet::leaflet(tmp, options = leafletOptions(zoomControl = FALSE)) %>% 
    #setView(-3.5, 5, zoom = 7) %>% 
    setView(-11.5, 6.5, zoom = 8) %>% 
    #addGraticule(interval = 1, style = list(color = "#FF0000", weight = 1)) %>% 
    addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean_Basemap/MapServer/tile/{z}/{y}/{x}",
             attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>%
    addCircleMarkers(lat = ~ LATITUDE, lng = ~ LONGITUDE, 
                   weight = 1, radius = as.numeric(tmp$NASC_Class), #radius is in px
                   color = "red", fill = NA,
                   popup = paste(tmp$Pel_CH_00)) %>%
    addLegendCustom(title = paste0(species, " NASC"),
                    colors = "red",
                    labels = c("<10", 
                               "10 - 10<sup>2</sup>",
                               "10<sup>2</sup> - 10<sup>3</sup>",
                               "10<sup>3</sup> - 10<sup>4</sup>",
                               ">10<sup>4</sup>"),

                    sizes = sizes*2 #For some reason this *2 seems to be needed for the legend circles to have the right size. Perhaps it has to do with the 50% in the CustomLegend function. Need to investigate
    )
    p 
  }
  
calc.mapRatio.fn <- function(limits) {
  
  # calculate span lat and long:
  span.x <- abs(limits[2]-limits[1])
  span.y <- abs(limits[4]-limits[3])
  # get ratio:
  ratio <- span.x/span.y
  return(ratio)
  
}

# mybasemap <- MyMap
# data <- input_acoustic_subset
# ACCAT <- ACCAT_names_of_interest[4]
# ratio
# label_offset <- label_offset_df
NASCggOceanMap <- function(data, ACCAT="PEL2", mybasemap, 
                           #ratio, 
                           #eez=eez.world.v11,
                           show.landmarks=FALSE, landmark_file=NULL,
                           show.countries=FALSE, countries_file=NULL,
                           country.names=c("Ghana", "Ivory Coast", "Liberia", "Sierra Leone", "Guinea-Bissau", "Guinea"),
                           label_offset=0, country.name.size=3) {
  
  .tmp <- data[data$ACCAT %in% ACCAT & data$Pel_CH_00>0,]
  
  #NASC.breaks <- ifelse(max(.tmp$Pel_CH_00)<=1e3, c(0,1e1,1e2,2.5e2,5e2), c(0,1e1,1e2,1e3,5e3,1e4))
  if(max(.tmp$Pel_CH_00)<=5e3) {
    NASC.limits = c(0.1,5e3)
    NASC.breaks = c(0.1,1,10,100,500,1000)
    NASC.range = c(0.1,15)
  } else {
    NASC.limits = c(0.1,1e5)
    NASC.breaks = c(0,10,1000,10000,50000)
    NASC.range = c(0.5,15)
  }
  #range <- c(10,1e4), c(1,1e2))
  
  # country names
  if(show.countries){
    if(is.null(countries_file)) {
      # countries_file <- list.files('.\\input\\countries\\',pattern='shp',full.names=TRUE)
      # countries_data <- rgdal::readOGR(countries_file)
      # countries_data <- as.data.frame(countries_data)
      
      #load the saved object sh_shapefiles.rda to read the land shapefile and extract the country names
      load('./_output/sh_shapefiles.rda')
      sh_land <- shapefiles$land
      #Convert to sf and get country names. It creats a dataframe from which one selects the countries of interest
      sh_land_sf_names <- st_as_sf(sh_land) %>% dplyr::select(sovereignt)
      sh_land_sf_names <- cbind(sh_land_sf_names, st_coordinates(st_centroid(sh_land_sf_names)))
      #country.names=c("Ghana", "Ivory Coast", "Liberia", "Sierra Leone", "Guinea-Bissau", "Guinea")
      #country.names<-country.names
      
      countries_data<-sh_land_sf_names %>% filter(sovereignt %in% country.names) %>% 
        mutate(sovereignt=case_when(
          sovereignt=='Ivory Coast'~ "CÃ´te d'Ivoire",
          TRUE~sovereignt
        ))
      
    } else {
      countries_data <- read.csv(countries_file)
    }
    
    if(length(label_offset)==1) {
      # countries_data$coords.x1 <- countries_data$coords.x1+label_offset
      # countries_data$coords.x2 <- countries_data$coords.x2+label_offset
      countries_data$X <- countries_data$X+label_offset
      countries_data$Y <- countries_data$Y+label_offset
    }
    
    if(length(label_offset)==2) {
      # countries_data$coords.x1 <- countries_data$coords.x1+label_offset[1]
      # countries_data$coords.x2 <- countries_data$coords.x2+label_offset[2]
      countries_data$X <- countries_data$X+label_offset[1]
      countries_data$Y <- countries_data$Y+label_offset[2]
    }
    
    # if(is.data.frame(label_offset) && nrow(label_offset)==length(unique(countries_data$ROMNAM))){
    #   countries_data$coords.x1 <- countries_data$coords.x1+label_offset[,1]
    #   countries_data$coords.x2 <- countries_data$coords.x2+label_offset[,2]
    # }
    
    if(is.data.frame(label_offset) && nrow(label_offset)==length(unique(countries_data$sovereignt))){
      countries_data$X <- countries_data$X+label_offset[,"X"]
      countries_data$Y <- countries_data$Y+label_offset[,"Y"]
    }
    
    # lon <- limits[1:2]
    # lat <- limits[3:4]
    # Poly_Coord_df = data.frame(lon, lat)
    # # create boundying polygon
    # poly <- SpatialPolygons(list(Polygons(list(Polygon(cbind(
    #   Poly_Coord_df$lon[c(1,2,2,1,1)],
    #   Poly_Coord_df$lat[c(1,1,2,2,1)]))), ID = 1)),proj4string=CRS(projection))
    # # clipped landmark to area of interest
    # sh_countries <- countries_data[!is.na(over(countries_data,poly)),]
    # countries_df <- as.data.frame(sh_countries)
  }
  
  
  # landmarks
  if(show.landmarks){
    if(is.null(landmark_file)) {
      landmark_file <- list.files('.\\input\\PointsOfInterest\\', pattern='shp', full.names=TRUE)
      landmark_data <- rgdal::readOGR(landmark_file)
      landmark_data <- as.data.frame(landmark_data)
    } else {
      landmark_data <- read.csv(landmark_file)
    }
    
    
    names(landmark_data)[grep(pattern='lon',names(landmark_data))] <- 'lon'
    names(landmark_data)[grep(pattern='lat',names(landmark_data))] <- 'lat'
  }
  
  ### Plot maps
  #tiff("plots/Hmack NASC.tiff", width=2500, height=4000,res=500,units="px",compression="lzw")
  pp <- mybasemap +
    geom_point(data = .tmp, aes(x = LONGITUDE, y = LATITUDE,size = Pel_CH_00),
               stroke = .5, alpha = .25, colour="red") +    
    #scale_radius(limits = NASC.limits, range = c(0, 5), name = paste0("NASC"), breaks = NASC.breaks)+
    #scale_size(limits = NASC.limits, range = NASC.range, name = paste0("NASC"), breaks = NASC.breaks)+
    scale_size_binned(limits = NASC.limits, range = NASC.range, name = paste0("NASC"), breaks = NASC.breaks)+
    #scale_size_area(limits = NASC.limits, max_size = 15,          #range = NASC.range,                     name = paste0("NASC"), breaks = NASC.breaks)+
    #coord_sf(xlim = limits[1:2], ylim = limits[3:4]) + 
    #geom_sf(data=eez, colour = alpha("black", 1/2), size = 1) + 
    
    # formatting
    labs(title = ACCAT, y="", x="")+
    theme(plot.title = element_text(face = "italic")) #+ theme(legend.position=c(0.6,0.25), legend.text=element_text(size=10))
  
  pp
  if(show.countries) {
    #pp <- pp + geom_text(data=countries_data,aes(x=coords.x1,y=coords.x2,label=ROMNAM),size=4) 
    pp <- pp + geom_label(data=countries_data,aes(x=X,y=Y,label=sovereignt),
                          size=country.name.size, fontface="bold", check_overlap = T) 
  }
  
  if(show.landmarks) {
    if(any(names(landmark_data)=='size')) {
      pp <- pp + #geom_point(data=landmark_data,aes(x=lon,y=lat)) +
        geom_text(data=landmark_data,aes(x=lon,y=lat,label=label),size=landmark_data$size) 
    } else {pp <- pp + #geom_point(data=landmark_data,aes(x=lon,y=lat)) + 
      geom_text(data=landmark_data,aes(x=lon,y=lat,label=label))} 
  }
  
  
  # if(ratio<=1) {
  #   pp <- pp + theme(legend.position='bottom', legend.text=element_text(size=10)) +
  #     guides(fill=guide_legend(ncol=1,title.position="top")) + 
  #     guides(size=guide_legend(ncol=1,title.position="top"))}
  # if(ratio>1) pp <- pp + theme(legend.position='left', legend.text=element_text(size=10))
  # 
  #pp <- pp + theme(legend.position='left', legend.text=element_text(size=10))
  return(pp)
  #dev.off()
}


#' @title Create static basemap with bathymetry for area of interest
#' @description Uses the ggOceanMaps package to get the bathimetry data from the Eastern Atlantic GEBCO file 
#' available in folder '_GEBCO_depth'
#' @param bathy_path: path to GEBCO bathymetry file. Default set to './_GEBCO_depth' folder.
#' @param bathy_file: file name. Default to NULL, so in that case it will just get .nc file in the folder
#' @param lims: limits of the area of interest. Default set to the tracklog xmin-xmax, ymin-ymax from the 
#' tracklog sp object, but a vector, e.g. c(lon_min,lon_max,lat_min,lat_max) could be provided instead.
#' @param outPath: Optional. Path where bathymetry data will be saved as rda file. 
#' Default set to NULL, so the data will not be saved on a file.  
#' @param projection: projection to be used. Default set to WGS84.
#' @return Returns a basemap for the area of interest. 
#' @import ggOceanMaps, marmap,

baseMapStatic <- function(lims, bathy_path='./_GEBCO_depth', bathy_file=NULL, depth_countours=c(20, 50, 100, 200, 300, 500, 1000), 
                          outPath=NULL, projection="+proj=longlat +datum=WGS84") {
  
  # list files in GEBCO_depth folder and grab nc file 
  if(is.null(bathy_file)) {
    bathy_file_Ls <- list.files(bathy_path)
    bathy_file <- bathy_file_Ls[grep(pattern='.nc',bathy_file_Ls)]
  }
  # specify path to depth file
  gebcoPath <- paste(bathy_path,bathy_file,sep='/')
  # read in depth file
  b <- readGEBCO.bathy(gebcoPath)
  
  # if lims provided is a sf object, then we just extract the bbox attribute and add some extra space around it
  if(class(lims)[1]=='sf') {
    lims <- st_bbox(lims)
    lims[1:2] <- round(lims[1:2]-1.5,2)
    lims[3:4] <- round(lims[3:4]+1.5,2)
    lims <- as.vector(lims[c('xmin','xmax','ymin','ymax')])
  }
  
  #simplify the bathymetry raster to prepare it for the vector_bathymetry function
  rb <- raster_bathymetry(bathy = gebcoPath, depths = depth_countours, proj.out = projection, boundary = lims)
  # N.B. no need to worry about this warning message
  #"vobjtovarid4: error #F: I could not find the requsted var (or dimvar) in the file!"
  
  # Now we vectorize the bathymetry raster
  bs_bathy <- vector_bathymetry(rb)
  
  # Get Natural Earth world map
  world <- rnaturalearth::ne_countries(scale = 10)
  # is the geometry valid?
  rgeos::gIsValid(world) 
  
  # clip the world map to the area of interest
  bs_land <- clip_shapefile(world, lims) #be patient
  # set the right projection
  bs_land <- sp::spTransform(bs_land, CRSobj = sp::CRS(projection))
  # check again if geometry is ok
  rgeos::gIsValid(bs_land) # Has to return TRUE, if not use rgeos::gBuffer

  # if outPath is provided, then data will be saved.
  if(!is.null(outPath)) {
    save(bs_bathy, bs_land, bs_glacier, file = paste(outPath, "bs_shapes.rda", sep = "/"), compress = "xz")
  }
  
  # create map
  bm <- basemap(shapefiles = list(land = bs_land, glacier = NULL, bathy = bs_bathy), bathymetry = TRUE, glaciers = TRUE)
  BM <- bm + scale_x_continuous(breaks = seq(floor(lims[1]),ceiling(lims[2]),2)) +
    scale_y_continuous(breaks = seq(floor(lims[3]),ceiling(lims[4]),2))
  
  return(BM)
} 



#' @title Get elements of a vector when splitted in n parts
#' @description Provided a vector x, it extract the elements obtained by evenly split the vector
#' in as many parts as provided in 'by', including the first and the last element of x
#' @param x: vector
#' @param by: number of parts in which you want to split the vector
#' @return Elements corresponding to n parts
split_func <- function(x, by) {
  r <- diff(range(x))
  out <- seq(0, r - by - 1, by = by)
  c(round(min(x) + c(0, out - 0.51 + (max(x) - max(out)) / 2), 0), max(x))
}


  
# map <- leaflet(data=qks,
#                title="Fiji Earthquakes", style=sty)
# 
# 
# 
# 
# library(tmap)
# library(rgdal)
# coordinates(tmp) <- ~ longitudestart+latitudestart
# tmap_mode("view")
# 
# p <-   tm_shape(shp = tmp) + 
#   tm_symbols("Density", col = 'red', 
#              border.alpha = .5, 
#              #style="fixed", breaks=c(-Inf, 1e-5, 1e-2, 1e-1, 1, 5, 10, Inf),
#              palette="-RdYlBu", contrast=1, 
#              title.size=expression(paste("Density (T ",NM^-2 ,")"))) +
#   tm_basemap(server = "Esri.OceanBasemap") + 
#   tm_grid(projection="+init=epsg:4326", labels.size = 1, col = "grey85", labels.inside.frame = T) +
#   tm_compass(position = c('RIGHT', 'TOP')) + 
#   tm_add_legend(type = "symbol", border.col = "grey40",
#                  col = "red",  
#                  size = c(1, 2, 3, 4, 5, 6),
#   #               labels = c("0 to 10 mln","0 to 10 mln","0 to 10 mln","0 to 10 mln", "10 to 20 mln", "20 to 30 mln", "30 to 40 mln"),
#   
#   ) + 
#   tm_legend(outside=FALSE)
# 
# 
# p + tm_view(view.legend.position = c('left','top'))
#   
#   
# 
# 
# 
# map1 <- tm_shape(metro) +
#   tm_bubbles("pop2010", col = "growth", 
#              border.col = "black", border.alpha = .5, 
#              style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
#              palette="-RdYlBu", contrast=1, 
#              title.size="Metro population", 
#              title.col="Growth rate (%)", id="name", 
#              popup.vars=c("pop2010", "pop2020", "growth")) + 
#   tm_legend(outside=TRUE)
# 
# current.mode <- tmap_mode("plot")
# 
# # plot map
# map1
# 
# # view map with default view options
# tmap_mode("view")
# map1
# 
# # view map with changed view options
# map1 + tm_view(alpha = 1, basemaps = "Stamen.Watercolor")
# 
# # restore current mode
# tmap_mode(current.mode)
# 

## individualFigureData ####

#' @title Generate data for individual plots
#' @description Generates data required by individual plots in Biotic Explorer
#' @param indall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$indall}.
#' @param indSpecies character defining the species in \code{indall$scientificname). Typically input$indSpecies.
#' @param lengthUnit character defining the unit of length measurements for output. Options: "mm", "cm" or "m". The NMD standard is "m". 
#' @param weightUnit character defining the unit of weight measurements for output. Options: "g", "kg". The NMD standard is "kg".
#' @param useEggaSystem logical indicating whether "delnummer" (catchpartnumber) defines the sex of individuals. This has systematically been used for Greenland halibut collected during "EggaNord" and "EggaSÃ¸r" surveys.  
#' @return Returns a list of tibbles containing data required by various individual-based plots.
#' @import dplyr data.table

# indall = rv$indall; indSpecies = "snabeluer"; lengthUnit = "m"; weightUnit = "kg"; useEggaSystem = FALSE
# indall = rv$indall; indSpecies = input$indSpecies; lengthUnit = input$lengthUnit; weightUnit = input$weightUnit; useEggaSystem = FALSE
individualFigureData <- function(indall, indSpecies = indSpecies, lengthUnit = "m", weightUnit = "kg", useEggaSystem = FALSE) {
  #indSpecies <- "Dactylopterus volitans"
  ## Base data
  
  tmpBase <- indall %>% filter(scientificname == indSpecies)
  tmpBase <- setDT(tmpBase)
  if (indSpecies == "blÃ¥kveite" & useEggaSystem) {
    
    tmpTab <- data.table::dcast(tmpBase, cruise + startyear + serialnumber + longitudestart + latitudestart ~ catchpartnumber, fun.aggregate = length, value.var = "length")
    
    if(all(c(1, 2) %in% names(tmpTab))) {
      tmpTab$EggaSystem <- tmpTab$`1` > 0 & tmpTab$`2` > 0
      tmpBase <- dplyr::left_join(tmpBase, tmpTab[, !names(tmpTab) %in% 1:10, with = FALSE], by = c("startyear", "serialnumber", "cruise", "longitudestart", "latitudestart"))  
      tmpBase$sex <- ifelse(!is.na(tmpBase$sex), tmpBase$sex, ifelse(is.na(tmpBase$sex) & tmpBase$EggaSystem & tmpBase$catchpartnumber == 1, 1, ifelse(is.na(tmpBase$sex) & tmpBase$EggaSystem & tmpBase$catchpartnumber == 2, 2, NA)))
      tmpBase <- as.data.table(tmpBase[, names(tmpBase) != "EggaSystem"])
    }
  }
  
  tmpBase$sex[is.na(tmpBase$sex)] <- "Unidentified"
  tmpBase$sex <- factor(tmpBase$sex)
  tmpBase$sex <- dplyr::recode_factor(tmpBase$sex, "1" = "Female", "2" = "Male", "3" = "Unidentified", "4" = "Unidentified")
  
  ## Length-weight data
  if(nrow(na.omit(tmpBase[, .(length, individualweight)])) >= 1) {
    lwDat <- tmpBase[!is.na(length) & !is.na(individualweight),]
    lwDat$weightMod <- log(lwDat$individualweight*1000)
    lwDat$lengthMod <- log(lwDat$length*100)
    
    if(nrow(lwDat) >= 30){
      lwMod <- lm(weightMod ~ lengthMod, 
                  data = lwDat[!is.infinite(lengthMod) & !is.infinite(weightMod)]
      )
      lwModA <- unname(exp(coef(lwMod)[1]))
      lwModB <- unname(coef(lwMod)[2])
    } else {
      lwModA <- NULL
      lwModB <- NULL
    }
    if(lengthUnit == "cm") lwDat$length <- lwDat$length*100
    if(lengthUnit == "mm") lwDat$length <- lwDat$length*1000
    if(weightUnit == "g") lwDat$individualweight <- lwDat$individualweight*1000
    lwDat$weightModTrans <- log(lwDat$individualweight)
    lwDat$lengthModTrans <- log(lwDat$length)
    if(nrow(lwDat) >= 30){
      lwModTrans <- lm(weightModTrans ~ lengthModTrans, 
                       data = lwDat[!is.infinite(lengthModTrans) & !is.infinite(weightModTrans)]
      )
      lwModTransA <- unname(exp(coef(lwModTrans)[1]))
      
    } else {
      
      lwModTransA <- NULL
    }
  } else {
    
    lwDat <- NULL
    lwModA <- NULL
    lwModB <- NULL
    lwModTransA <- NULL
  }
  
  ## Transform tmpBase units (untransformed needed above)
  
  if(lengthUnit == "cm") tmpBase$length <- tmpBase$length*100
  if(lengthUnit == "mm") tmpBase$length <- tmpBase$length*1000
  if(weightUnit == "g") tmpBase$individualweight <- tmpBase$individualweight*1000
  
  ## Length-age data
  
  if (all(c("length", "age") %in% names(tmpBase))) {
    if (nrow(na.omit(tmpBase[, c("length", "age"), with = FALSE])) > 10) {
      
      laDat <- tmpBase[!is.na(tmpBase$age) & !is.na(tmpBase$length), ]
      
    } else {laDat <- NULL}} else {laDat <- NULL}
  
  ## L50 maturity data
  
  if (nrow(na.omit(tmpBase[, .(length, sex, maturationstage)])) > 20) { 
    
    l50Dat <- tmpBase[!is.na(tmpBase$length) & !is.na(tmpBase$sex) & !is.na(tmpBase$maturationstage) & (tmpBase$sex == "Female" | tmpBase$sex == "Male"), ]
    
    tmp <- table(l50Dat$sex)
    
    if(tmp[names(tmp) == "Female"] < 10 | tmp[names(tmp) == "Male"] < 10) {
      
      l50Dat <- NULL
      
    } else {
      
      l50Dat$maturity <- ifelse(l50Dat$maturationstage < 2, 0, ifelse(l50Dat$maturationstage >= 2, 1, NA))
      
    } 
  } else {
    
    l50Dat <- NULL
    
  }
  
  ## Sex ratio data
  
  if(nrow(na.omit(tmpBase[, .(sex)])) > 5) {
    srDat <- tmpBase %>% lazy_dt() %>% 
      dplyr::filter(!is.na(sex)) %>% 
      dplyr::group_by(cruise, startyear, serialnumber, longitudestart, latitudestart) %>% 
      dplyr::summarise(Female = sum(sex == "Female"), Male = sum(sex == "Male")) %>% 
      dplyr::mutate(Total = Female + Male) %>% 
      dplyr::filter(Total > 0) %>% 
      dplyr::collect()
  } else {
    srDat <- NULL
  }
  
  ## Geographic size distribution data
  
  if(nrow(na.omit(tmpBase[, .(length)])) > 20) {
    sdDat <- tmpBase %>% lazy_dt() %>% 
      dplyr::filter(!is.na(length)) %>% 
      dplyr::select(cruise, startyear, serialnumber, longitudestart, latitudestart, length) %>% 
      dplyr::mutate(interval = ggplot2::cut_interval(length, n = 5)) %>% 
      dplyr::group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, interval, .drop = FALSE) %>% 
      dplyr::summarise(count = n()) %>% 
      dplyr::collect()
  } else {
    sdDat <- NULL
  }
  
  ## Length distribution data
  
  if(nrow(na.omit(tmpBase[, .(length, sex)])) > 10) {
    print(nrow(na.omit(tmpBase[, .(length, sex)])))
    ldDat <- tmpBase %>% as_tibble() %>% 
      filter(!is.na(length)) %>% 
      replace_na(list(sex = "Unidentified")) %>% 
      select(sex, length, maturationstage, specialstage) 
  } else {
    ldDat <- NULL
  }
  
  ## Return
  
  invisible(list(units = list(length = lengthUnit, weight = weightUnit), tmpBase = tmpBase, lwDat = lwDat, lwMod = list(a = lwModA, b = lwModB, aTrans = lwModTransA), laDat = laDat, l50Dat = l50Dat, srDat = srDat, sdDat = sdDat, ldDat = ldDat))
  
}

## lwPlot ####

lwPlot <- function(data, lwPlotLogSwitch = FALSE, LWrel=TRUE) {
  if(is.null(data$lwDat)){
    Plot <- NULL} else {
    p <- suppressWarnings({
      ggplot() +
        geom_point(data = data$lwDat, aes(x = length, y = individualweight, text = paste0(  "cruise: ", cruise, "\nserialnumber: ", serialnumber, "\ncatchpartnumber: ", catchpartnumber, "\nspecimenid: ", unique(data$specimenid)))) + 
        theme_classic(base_size = 12) + 
        labs(title = data$lwDat$scientificname, 
             subtitle = paste0("Coefficients (calculated using cm and g): a = ", round(data$lwMod$a, 3), "; b = ", round(data$lwMod$b, 3), 
                               "\n R-square = ", data$lwModFit, 
                               "\n Number of included specimens = ", nrow(data$lwDat), 
                               "\n Total number of measured = ", nrow(data$tmpBase), 
                               "\n Excluded (length or weight missing): Length = ", 
                               sum(is.na(data$tmpBase$length)), "; weight = ", 
                               sum(is.na(data$tmpBase$individualweight)))) + 
        theme(plot.subtitle=element_text(size=10))
    })
  
  if (lwPlotLogSwitch) {
    p <- suppressMessages({
      p + 
        scale_x_log10(paste0("Length [log10(", data$units$length, ")]")) +
        scale_y_log10(paste0("Weight [log10(", data$units$weight, ")]")) 
    })
    
    if(LWrel && nrow(data$lwDat)>=30) {
      # if(nrow(data$lwDat)>=30){
      p <- #suppressMessages({
        p + geom_smooth(data = data$lwDat, aes(x = length, y = individualweight), method = "lm", formula = y ~ x, se = TRUE) 
      #})
      # } else {
      # p < renderText('Less than 30 specimen - LW relationship will not be fitted')
      # }
    }
    
  } else {
    p <- suppressWarnings({
      p + 
        scale_x_continuous(paste0("Length (", data$units$length, ")")) +
        scale_y_continuous(paste0("Weight (", data$units$weight, ")")) 
    })
    
    if(LWrel && nrow(data$lwDat)>=30){
      # if(nrow(data$lwDat)>=30){
      p <- #suppressWarnings({
        p + stat_function(data = data.frame(x = range(data$lwDat$length)), aes(x),
                          fun = function(a, b, x) {a*x^b},
                          args = list(a = data$lwMod$aTrans, b = data$lwMod$b),
                          color = "blue", size = 1)
      # } else {
      #   p < renderText('Less than 30 specimen - LW relationship will not be fitted')
      # }
      #})
    }
  }
  
  suppressMessages(p)
  } 
}

## laPlot ####

# data = indOverviewDat = individualFigureData(indall = rv$indall, indSpecies = input$selSpeciesDb); laPlotSexSwitch = FALSE; growthModelSwitch = "vout"; forceZeroGroupLength = NA; forceZeroGroupStrength = 10
laPlot <- function(data, laPlotSexSwitch, growthModelSwitch, forceZeroGroupLength = NA, forceZeroGroupStrength = 10) {
  
  modName <- c("von Bertalanffy" = "vout", "Gompertz" = "gout", "Logistic" = "lout")
  modName <- names(modName[modName == growthModelSwitch])
  
  if (laPlotSexSwitch) {
    
    laDat <- data$laDat %>% lazy_dt() %>% filter(!is.na(sex) & (sex == "Female" | sex == "Male")) %>% select(cruise, serialnumber, catchpartnumber, specimenid, sex, age, length) %>% collect()
    
    laDatF <- laDat %>% filter(sex == "Female") %>% select(age, length)
    laDatM <- laDat %>% filter(sex == "Male") %>% select(age, length)
    
    if(nrow(laDatM) < 10 | nrow(laDatF) < 10) {
      
      Plot <- ggplot() +
        geom_blank() +
        annotate("text", x = 1, y = 1, label = "Not enough age data for\nsex separated growth models", size = 6) +
        ylab(paste0("Total length (", data$units$length, ")")) +
        xlab("Age (years)") +
        coord_cartesian(expand = FALSE, clip = "off") +
        theme_classic(base_size = 14)
      
      Text <- paste0(
        "Not enough age data:",
        "\n Number of included specimens = ", nrow(laDatF), " and ", nrow(laDatM)
      )
      
    } else {
      
      if(!is.na(forceZeroGroupLength)) {
        laDatF <- rbind(laDatF, tibble(age = rep(0, ceiling(nrow(laDatF)*(forceZeroGroupStrength/100))), length = rep(forceZeroGroupLength, ceiling(nrow(laDatF)*(forceZeroGroupStrength/100)))))
        laDatM <- rbind(laDatM, tibble(age = rep(0, ceiling(nrow(laDatM)*(forceZeroGroupStrength/100))), length = rep(forceZeroGroupLength, ceiling(nrow(laDatM)*(forceZeroGroupStrength/100)))))
      } 
      
      laModF <- fishmethods::growth(age = laDatF$age, size = laDatF$length, Sinf = max(laDatF$length), K = 0.1, t0 = 0, graph = FALSE)
      laModM <- fishmethods::growth(age = laDatM$age, size = laDatM$length, Sinf = max(laDatM$length), K = 0.1, t0 = 0, graph = FALSE)
      
      laModFpred <- data.frame(age = 0:max(laDat$age), length = predict(eval(parse(text = paste0("laModF$", growthModelSwitch))), newdata = data.frame(age = 0:max(laDat$age))))
      laModMpred <- data.frame(age = 0:max(laDat$age), length = predict(eval(parse(text = paste0("laModM$", growthModelSwitch))), newdata = data.frame(age = 0:max(laDat$age))))
      
      laModFpars <- coef(eval(parse(text = paste0("laModF$", growthModelSwitch))))
      laModMpars <- coef(eval(parse(text = paste0("laModM$", growthModelSwitch))))
      
      ## Plot 
      
      Plot <- suppressWarnings({
        ggplot() +
          geom_point(data = laDat, aes(x = age, y = length, color = as.factor(sex), text = paste0("cruise: ", cruise, "\nserialnumber: ", serialnumber, "\ncatchpartnumber: ", catchpartnumber, "\nspecimenid: ", specimenid))) +
          expand_limits(x = c(0, round_any(max(laDat$age), 10, ceiling)), y = c(0, max(pretty(c(0, max(laDat$length)))))) +
          scale_color_manual("Sex", values = c(ColorPalette[4], ColorPalette[1])) + 
          geom_hline(yintercept = laModFpars[1], linetype = 2, color = ColorPalette[4], alpha = 0.5) +
          geom_hline(yintercept = laModMpars[1], linetype = 2, color = ColorPalette[1], alpha = 0.5) +
          geom_path(data = laModFpred, aes(x = age, y = length), color = ColorPalette[4]) + 
          geom_path(data = laModMpred, aes(x = age, y = length), color = ColorPalette[1]) + 
          ylab(paste0("Total length (", data$units$length, ")")) +
          xlab("Age (years)") +
          coord_cartesian(expand = FALSE, clip = "off") +
          theme_classic(base_size = 14)
      })
      
      ## Text
      
      Text <- paste0(
        modName, " growth function coefficients\n for females and males, respectively: \n Linf (asymptotic average length) = ", round(laModFpars[1], 3), " and ", round(laModMpars[1], 3), " ", data$units$length, 
        "\n K (growth rate coefficient) = ", round(laModFpars[2], 3), " and ", round(laModMpars[2], 3), 
        "\n t0 = ", round(laModFpars[3], 3), " and ", round(laModMpars[3], 3), " ", data$units$length, 
        "\n tmax (life span; t0 + 3/K) = ", round(laModFpars[3] + 3 / laModFpars[2], 1), " and ", round(laModMpars[3] + 3 / laModMpars[2], 1), " years",
        "\n Number of included specimens = ", nrow(laDatF), " and ", nrow(laDatM),
        "\n Total number of measured = ", nrow(data$tmpBase), 
        "\n Excluded (length, age or sex missing): \n Length = ", sum(is.na(data$tmpBase$length)), "; age = ", sum(is.na(data$tmpBase$age)), "; sex = ", sum(is.na(data$tmpBase$sex))
      )
    }
  } else {
    
    laDat <- data$laDat %>% lazy_dt() %>% select(cruise, serialnumber, catchpartnumber, specimenid, sex, age, length) %>% collect()
    
    if(!is.na(forceZeroGroupLength)) {
      laDat <- bind_rows(laDat, tibble(age = rep(0, ceiling(nrow(laDat)*(forceZeroGroupStrength/100))), length = rep(forceZeroGroupLength, ceiling(nrow(laDat)*(forceZeroGroupStrength/100)))))
    } 
    
    if(length(laDat$age) < 30) {
      
      #if(eval(parse(text = paste0("laMod$", growthModelSwitch))) == "Fit failed") {
      
      Plot <- ggplot() +
        geom_blank() +
        annotate("text", x = 1, y = 1, label = "Not enough age data to\ncalculate a growth model", size = 6) +
        ylab(paste0("Total length (", data$units$length, ")")) +
        xlab("Age (years)") +
        coord_cartesian(expand = FALSE, clip = "off") +
        theme_classic(base_size = 14)
      
      Text <- paste0(
        "Not enough age data:",
        "\n Number of included specimens = ", nrow(laDat)
      )
      
    } else {
      
      laMod <- fishmethods::growth(age = laDat$age, size = laDat$length, Sinf = max(laDat$length), K = 0.1, t0 = 0, graph = FALSE)
      
      laModpred <- data.frame(age = 0:max(laDat$age), length = predict(eval(parse(text = paste0("laMod$", growthModelSwitch))), newdata = data.frame(age = 0:max(laDat$age))))
      
      laModpars <- coef(eval(parse(text = paste0("laMod$", growthModelSwitch))))
      
      ## Plot
      
      Plot <- suppressWarnings({
        ggplot() +
          geom_point(data = laDat, aes(x = age, y = length, text = paste0("cruise: ", cruise, "\nserialnumber: ", serialnumber, "\ncatchpartnumber: ", catchpartnumber, "\nspecimenid: ", specimenid))) +
          expand_limits(x = c(0, round_any(max(laDat$age), 10, ceiling)), y = c(0, max(pretty(c(0, max(laDat$length)))))) +
          geom_hline(yintercept = laModpars[1], linetype = 2, color = "blue", alpha = 0.5) +
          geom_path(data = laModpred, aes(x = age, y = length), color = "blue") + 
          ylab(paste0("Total length (", data$units$length, ")")) +
          xlab("Age (years)") +
          coord_cartesian(expand = FALSE, clip = "off") +
          theme_classic(base_size = 14)
      })
      
      ## Text
      
      Text <- paste0(
        modName, " growth function coefficients: \n Linf (asymptotic average length) = ", round(laModpars[1], 3), " ", data$units$length, 
        "\n K (growth rate coefficient) = ", round(laModpars[2], 3), 
        "\n t0 (length at age 0) = ", round(laModpars[3], 3), " ", data$units$length, 
        "\n tmax (life span; t0 + 3/K) = ", round(laModpars[3] + 3 / laModpars[2], 1), " years", 
        "\n Number of included specimens = ", nrow(data$laDat), 
        "\n Total number of measured = ", nrow(data$tmpBase), 
        "\n Excluded (length or age missing): \n Length = ", sum(is.na(data$tmpBase$length)), "; age = ", sum(is.na(data$tmpBase$age))
      )
    }
  }
  
  ## Return
  
  return(list(laPlot = Plot, laText = Text))
}

## l50Plot ####

l50Plot <- function(data) {
  
  modF <- glm(maturity ~ length, data = data$l50Dat[data$l50Dat$sex == "Female",], family = binomial(link = "logit"))
  modM <- glm(maturity ~ length, data = data$l50Dat[data$l50Dat$sex == "Male",], family = binomial(link = "logit"))
  
  # shiny::validate(need(modF$converged && modM$converged,
  #                      "% is not achieved by any species or taxonomic group"))
  out <- tryCatch(
    expr={
      # if(!modF$converged | !modM$converged){
      #   Plot <- ggplot()
      #   Text <- paste0("glm model did not converge")
      # 
      # } else {
      # 
      Fdat <- unlogit(0.5, modF)
      Fdat$sex <- "Female"
      Mdat <- unlogit(0.5, modM)
      Mdat$sex <- "Male"
      modDat <- rbind(Fdat, Mdat)
      
      ### Plot
      
      Plot <- suppressMessages({
        
        ggplot(data$l50Dat, aes(x = length, y = maturity, shape = sex)) + 
          geom_point() + 
          geom_segment(data = modDat, 
                       aes(x = mean, xend = mean, y = 0, yend = 0.5, color = sex),
                       linetype = 2) +
          geom_segment(data = modDat, 
                       aes(x = -Inf, xend = mean, y = 0.5, yend = 0.5, color = sex),
                       linetype = 2) +
          geom_text(data = modDat, 
                    aes(x = mean, y = -0.03, label = paste(round(mean, 2), data$units$length),
                        color = sex), size = 3) +
          stat_smooth(aes(color = sex), method = "glm", formula = y ~ x,
                      method.args = list(family = "binomial")) +
          xlab(paste0("Total length (", data$units$length, ")")) +
          ylab("Maturity") + 
          scale_color_manual("Sex", values = c(ColorPalette[4], ColorPalette[1])) +
          scale_shape("Sex", solid = FALSE) + 
          theme_bw(base_size = 14) + 
          guides(color=guide_legend(override.aes=list(fill=NA))) + 
          theme(legend.position = c(0.9, 0.25), 
                legend.background = element_blank(), legend.key = element_blank())
      })
      
      ### Text
      
      Text <- paste0(
        "50% maturity at length (L50) based on logit regressions and assuming maturitystage >= 2 as mature:",
        "\n\n Females: ", round(modDat[modDat$sex == "Female", "mean"], 3), " ", data$units$length, ". 95% confidence intervals: ", round(modDat[modDat$sex == "Female", "ci.min"], 3), " - ", round(modDat[modDat$sex == "Female", "ci.max"], 3),
        "\n  Number of specimens: ", nrow(data$l50Dat[data$l50Dat$sex == "Female",]),
        "\n\n Males: ", round(modDat[modDat$sex == "Male", "mean"], 3), " ", data$units$length, ". 95% confidence intervals: ", round(modDat[modDat$sex == "Male", "ci.min"], 3), " - ", round(modDat[modDat$sex == "Male", "ci.max"], 3),
        "\n  Number of specimens: ", nrow(data$l50Dat[data$l50Dat$sex == "Male",])
      )
    },
    error=function(e) {
      message(paste("Models did not converge"))
      print(e)
    }
    # Plot <- ggplot()
    # Text <- "Models did not converge"
    # # Choose a return value in case of error
    # return(list(Plot = Plot, Text = Text))
    #},
    # finally={
    #   message('All done, quitting.')
    #   # return(list(Plot = Plot, Text = Text))
    # }
    ### Return
  )
  return(out)
  #return(list(Plot = Plot, Text = Text))
  
}


###### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ALTERNATIVES MYBASEMAP FUNCTIONS
###### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Basemap for survey report 
## Does not include EEZ
#' @title Mybasemap 
#' @description create map for Sailing order report, which can include EEZ
#' @return area map

Mybasemap <- function (limits = NULL, data = NULL, shapefiles = NULL, bathymetry = TRUE, 
                       glaciers = FALSE, resolution = "high", rotate = FALSE, 
                       legends = TRUE, legend.position = "right", lon.interval = NULL, 
                       lat.interval = NULL, bathy.style = "poly_blues", bathy.border.col = NA, 
                       bathy.size = 0.1, land.col = "grey60", land.border.col = "black", 
                       land.size = 0.1, gla.col = "grey95", gla.border.col = "black", 
                       gla.size = 0.1, grid.col = "grey70", grid.size = 0.1, 
                       base_size = 11, projection.grid = FALSE, verbose = FALSE, 
                       bathy_path='./_GEBCO_depth', bathy_file=NULL, 
                       depth_countours=c(200, 500, 1000), jitter.x.limits=0.1, jitter.y.limits=0.1,
                       outPath='./_output', projection="+proj=longlat +datum=WGS84") 
  
{
  if (!requireNamespace("ggOceanMapsData", quietly = TRUE)) {
    stop("The ggOceanMapsData package needs to be installed for ggOceanMaps to function.\nInstall the data package by running\ninstall.packages(\"ggOceanMapsData\", repos = c(\"https://mikkovihtakari.github.io/drat\", \"https://cloud.r-project.org\")\nOR\ndevtools::install_github(\"MikkoVihtakari/ggOceanMapsData\")")
  }
  if (is.null(data) & is.null(limits) & is.null(shapefiles)) 
    stop("One or several of the arguments limits, data and shapefiles is required.")
  if (class(legends) != "logical" | !length(legends) %in% 
      1:2) 
    stop("'legends' argument has to be a logical vector of length 1 or 2. Read the explantion for the argument in ?basemap")
  
  # list files in GEBCO_depth folder and grab nc file 
  if(is.null(bathy_file)) {
    bathy_file_Ls <- list.files(bathy_path)
    bathy_file <- bathy_file_Ls[grep(pattern='.nc',bathy_file_Ls)]
    
    # specify path to depth file
    gebcoPath <- paste(bathy_path,bathy_file,sep='/')
    
    # read in depth file
    b <- readGEBCO.bathy(gebcoPath)
    #simplify the bathymetry raster to prepare it for the vector_bathymetry function
    bathy_file <- raster_bathymetry(bathy = gebcoPath, depths = depth_countours, 
                                    proj.out = projection, boundary = limits)
    # N.B. no need to worry about this warning message
    #"vobjtovarid4: error #F: I could not find the requsted var (or dimvar) in the file!"
    
  } else {
    #simplify the bathymetry raster to prepare it for the vector_bathymetry function
    bathy_file <- raster_bathymetry(bathy = bathy_file, depths = depth_countours, 
                                    proj.out = projection, boundary = limits)
    # N.B. no need to worry about this warning message
    #"vobjtovarid4: error #F: I could not find the requsted var (or dimvar) in the file!"
  }
  sf::sf_use_s2(FALSE)
  # Now we vectorize the bathymetry raster
  sh_bathy <- vector_bathymetry(bathy_file)  
  
  # if lims provided is a sf object, then we just extract the bbox attribute and add some extra space around it
  if(class(limits)[1]=='sf') {
    limits <- st_bbox(limits)
    limits[1:2] <- round(limits[1:2]-jitter.x.limits,2)
    limits[3:4] <- round(limits[3:4]+jitter.y.limits,2)
    limits <- as.vector(limits[c('xmin','xmax','ymin','ymax')])
  }
  
  #simplify the bathymetry raster to prepare it for the vector_bathymetry function
  rb <- raster_bathymetry(bathy = gebcoPath, depths = depth_countours, 
                          proj.out = projection, boundary = limits)
  # N.B. no need to worry about this warning message
  #"vobjtovarid4: error #F: I could not find the requsted var (or dimvar) in the file!"
  
  # Now we vectorize the bathymetry raster
  sh_bathy <- vector_bathymetry(rb)
  
  # Get Natural Earth world map
  world <- rnaturalearth::ne_countries(scale = 10)
  # is the geometry valid?
  #rgeos::gIsValid(world) 
  
  # clip the world map to the area of interest
  sh_land <- clip_shapefile(world, limits) #be patient
  st_bbox(sh_land)
  # set the right projection
  sh_land <- sp::spTransform(sh_land, CRSobj = sp::CRS(projection))
  # check again if geometry is ok
  #rgeos::gIsValid(bs_land) # Has to return TRUE, if not use rgeos::gBuffer
  
  sh_glacier <- NULL
  shapefiles <- list(bathy=sh_bathy, land=sh_land, glacier=sh_glacier)
  # if outPath is provided, then data will be saved.
  
  if(!is.null(outPath)) {
    dir.create(outPath, showWarnings = FALSE)
    save(shapefiles, file = paste(outPath, "sh_shapefiles.rda", sep = "/"), compress = "xz")
  }
  
  
  X <- basemap_data(limits = limits, data = data, shapefiles = shapefiles, 
                    
                    bathymetry = bathymetry, glaciers = glaciers, #resolution = resolution, 
                    lon.interval = lon.interval, lat.interval = lat.interval, 
                    rotate = rotate, verbose = verbose)
  if (bathymetry & !is.null(X$shapefiles$bathy)) {
    bathy_cmd <- switch(bathy.style,poly_blues = "bathy_pb",
                        poly_greys = "bathy_pg",      contour_blues = "bathy_cb",
                        contour_grey = "bathy_cg",  stop(paste("bathy.style not found"))
    )
    bathy.legend <- ifelse(length(legends) == 1, legends,legends[1])
    if (bathy_cmd == "bathy_cg" & is.na(bathy.border.col)) bathy.border.col <- "grey"
    layers <- paste(map_cmd("base"), map_cmd(bathy_cmd),sep = " + ")
  } else {
    layers <- map_cmd("base")
  }
  if (length(X$shapefiles$land) > 0) {
    layers <- paste(layers, map_cmd("land"), sep = " + ")
  }
  if (glaciers & !is.null(X$shapefiles$glacier)) {
    if (length(X$shapefiles$glacier) > 0) {
      layers <- paste(layers, map_cmd("glacier"),sep = " + ")
    }
  }
  if (X$polar.map) {
    if (projection.grid) {
      
      layers <- paste(layers, map_cmd("defs_polar_proj"),sep = " + ")
    } else {
      layers <- paste(layers, map_cmd("defs_polar"),sep = " + ")
    }
  } else {
    if (projection.grid) {
      layers <- paste(layers, map_cmd("defs_rect_proj"),sep = " + ")
    } else {
      layers <- paste(layers, map_cmd("defs_rect"),sep = " + ")
    }
  }
  out <- eval(parse(text = layers))
  attributes(out)$class <- c(attributes(out)$class, "ggOceanMaps")
  attributes(out)$bathymetry <- bathymetry
  attributes(out)$glaciers <- glaciers
  attributes(out)$limits <- X$map.limits
  attributes(out)$polarmap <- X$polar.map
  attributes(out)$crs <- X$shapefiles$crs
  attributes(out)$proj <- X$proj
  out
}


## Basemap for Sailing order
## Possibility to include EEZ
#' @title Mybasemap_SO 
#' @description create map for Sailing order report, which can include EEZ
#' @return area map

Mybasemap_SO <- function (limits = NULL, data = NULL, shapefiles = NULL, bathymetry = TRUE, 
                       glaciers = FALSE, eez=FALSE, eez_obj=NULL, resolution = "high", rotate = FALSE, 
                       legends = TRUE, legend.position = "right", 
                       lon.interval = NULL, lat.interval = NULL, 
                       bathy.style = "poly_blues", bathy.border.col = NA, bathy.size = 0.1,
                       eez.col = "grey91", eez.alpha=1, eez.border.col=NA, eez.border.linetype=1, eez.size=0.1,
                       land.col = "grey60", land.border.col = "black", land.size = 0.1, 
                       gla.col = "grey95", gla.border.col = "black", gla.size = 0.1, 
                       grid.col = "grey70", grid.size = 0.1, base_size = 11, 
                       projection.grid = FALSE, verbose = FALSE, 
                       bathy_path='./_GEBCO_depth', bathy_file=NULL, depth_countours=c(200, 500, 1000), 
                       jitter.x.limits=0.1, jitter.y.limits=0.1,
                       outPath='./_output', projection="+proj=longlat +datum=WGS84") 
  
{
  if (!requireNamespace("ggOceanMapsData", quietly = TRUE)) {
    stop("The ggOceanMapsData package needs to be installed for ggOceanMaps to function.\nInstall the data package by running\ninstall.packages(\"ggOceanMapsData\", repos = c(\"https://mikkovihtakari.github.io/drat\", \"https://cloud.r-project.org\")\nOR\ndevtools::install_github(\"MikkoVihtakari/ggOceanMapsData\")")
  }
  if (is.null(data) & is.null(limits) & is.null(shapefiles)) 
    stop("One or several of the arguments limits, data and shapefiles is required.")
  if (class(legends) != "logical" | !length(legends) %in% 
      1:2) 
    stop("'legends' argument has to be a logical vector of length 1 or 2. Read the explantion for the argument in ?basemap")
  
  # list files in GEBCO_depth folder and grab nc file 
  if(is.null(bathy_file)) {
    bathy_file_Ls <- list.files(bathy_path)
    bathy_file <- bathy_file_Ls[grep(pattern='.nc',bathy_file_Ls)]
    
    # specify path to depth file
    gebcoPath <- paste(bathy_path,bathy_file,sep='/')
    
    # read in depth file
    b <- readGEBCO.bathy(gebcoPath)
    #simplify the bathymetry raster to prepare it for the vector_bathymetry function
    bathy_file <- raster_bathymetry(bathy = gebcoPath, depths = depth_countours, 
                                    proj.out = projection, boundary = limits)
    # N.B. no need to worry about this warning message
    #"vobjtovarid4: error #F: I could not find the requsted var (or dimvar) in the file!"
    
  } else {
    #simplify the bathymetry raster to prepare it for the vector_bathymetry function
    bathy_file <- raster_bathymetry(bathy = bathy_file, depths = depth_countours, 
                                    proj.out = projection, boundary = limits)
    # N.B. no need to worry about this warning message
    #"vobjtovarid4: error #F: I could not find the requsted var (or dimvar) in the file!"
  }
  sf::sf_use_s2(FALSE)
  # Now we vectorize the bathymetry raster
  sh_bathy <- vector_bathymetry(bathy_file)  
  
  # if lims provided is a sf object, then we just extract the bbox attribute and add some extra space around it
  if(class(limits)[1]=='sf') {
    limits <- st_bbox(limits)
    limits[1:2] <- round(limits[1:2]-jitter.x.limits,2)
    limits[3:4] <- round(limits[3:4]+jitter.y.limits,2)
    limits <- as.vector(limits[c('xmin','xmax','ymin','ymax')])
  }
  
  #simplify the bathymetry raster to prepare it for the vector_bathymetry function
  rb <- raster_bathymetry(bathy = gebcoPath, depths = depth_countours, 
                          proj.out = projection, boundary = limits)
  # N.B. no need to worry about this warning message
  #"vobjtovarid4: error #F: I could not find the requsted var (or dimvar) in the file!"
  
  # Now we vectorize the bathymetry raster
  sh_bathy <- vector_bathymetry(rb)
  
  # Get Natural Earth world map
  world <- rnaturalearth::ne_countries(scale = 10)
  # is the geometry valid?
  #rgeos::gIsValid(world) 
  
  # clip the world map to the area of interest
  sh_land <- clip_shapefile(world, limits) #be patient
  st_bbox(sh_land)
  # set the right projection
  sh_land <- sp::spTransform(sh_land, CRSobj = sp::CRS(projection))
  if(eez) {
    sh_eez <- as_Spatial(eez_obj)
  } else {
    sh_eez <- NULL
  }
  # check again if geometry is ok
  #rgeos::gIsValid(bs_land) # Has to return TRUE, if not use rgeos::gBuffer
  
  sh_glacier <- NULL
  shapefiles <- list(bathy=sh_bathy, land=sh_land, eez=sh_eez, glacier=sh_glacier)
  # if outPath is provided, then data will be saved.
  
  if(!is.null(outPath)) {
    dir.create(outPath, showWarnings = FALSE)
    save(shapefiles, file = paste(outPath, "sh_shapefiles.rda", sep = "/"), compress = "xz")
  }
  
  
  X <- basemap_data(limits = limits, data = data, shapefiles = shapefiles, 
                    # eez_obj=eez_obj,
                    bathymetry = bathymetry, glaciers = glaciers, #resolution = resolution, 
                    lon.interval = lon.interval, lat.interval = lat.interval, 
                    rotate = rotate, verbose = verbose)
  
  if(eez & !is.null(X$shapefiles$eez)) {
    layers <- paste(map_cmd("base"), map_cmd_eez("eez"),sep = " + ")
  } else {
    layers <- map_cmd("base")
  }
  
  if (bathymetry & !is.null(X$shapefiles$bathy)) {
    bathy_cmd <- switch(bathy.style,poly_blues = "bathy_pb",
                        poly_greys = "bathy_pg",      contour_blues = "bathy_cb",
                        contour_grey = "bathy_cg",  stop(paste("bathy.style not found"))
    )
    bathy.legend <- ifelse(length(legends) == 1, legends,legends[1])
    if (bathy_cmd == "bathy_cg" & is.na(bathy.border.col)) bathy.border.col <- "grey"
    layers <- paste(layers, map_cmd(bathy_cmd),sep = " + ")
  } else {
    layers <- layers
  }  
  

  if (length(X$shapefiles$land) > 0) {
    layers <- paste(layers, map_cmd("land"), sep = " + ")
  }
  if (glaciers & !is.null(X$shapefiles$glacier)) {
    if (length(X$shapefiles$glacier) > 0) {
      layers <- paste(layers, map_cmd("glacier"),sep = " + ")
    }
  }
  if (X$polar.map) {
    if (projection.grid) {
      
      layers <- paste(layers, map_cmd("defs_polar_proj"),sep = " + ")
    } else {
      layers <- paste(layers, map_cmd("defs_polar"),sep = " + ")
    }
  } else {
    if (projection.grid) {
      layers <- paste(layers, map_cmd("defs_rect_proj"),sep = " + ")
    } else {
      layers <- paste(layers, map_cmd("defs_rect"),sep = " + ")
    }
  }
  out <- eval(parse(text = layers))
  attributes(out)$class <- c(attributes(out)$class, "ggOceanMaps")
  #attributes(out)$eez_obj <- eez_obj
  attributes(out)$bathymetry <- bathymetry
  attributes(out)$glaciers <- glaciers
  attributes(out)$limits <- X$map.limits
  attributes(out)$polarmap <- X$polar.map
  attributes(out)$crs <- X$shapefiles$crs
  attributes(out)$proj <- X$proj
  out
}


## Derived from map_cmd function
## Designed to include aesthetic specifications for eez. 
#' @title map_cmd_eez 
#' @description specify eez aesthetics
#' @return string

map_cmd_eez <- function (command, alternative = FALSE) {
  out <- switch(command, 
                eez = "\n      ggspatial::layer_spatial(data = X$shapefiles$eez, fill = eez.col, alpha=eez.alpha, color = eez.border.col, linetype=eez.border.linetype, size = eez.size)\n    ", 
                stop(paste("map command", command, "not found.")))
  trimws(gsub("\n", " ", out))
}



## NOT NEEDED SINCE INCLUDING EEZ DRAWING DIRECTLY INTO THE BASEMAP FUNCTION TAKES CARE OF IT
## Function to crop eez to the station diary limits 
## Allows the inclusion of buffer, in case the user want a bounding_box larger than the xlims-ylims in station_diary
#' @title eez_crop 
#' @description crop eez to bounding box provided
#' @return sf feature, geometry_type=POLYGON

eez_crop <- function(dataDiary,buffer_size=25000, eezToCrop){
  ## Create buffered polygon around area of interest using Bounding box coordinates contained in the stations_diary object. 
  poly <- dataDiary %>%   
    st_bbox() %>% # create box
    st_as_sfc() %>% # convert to sfc object
    st_transform(crs="+proj=utm +zone=28 +datum=WGS84 +units=m +no_defs") %>% # transform coordinates
    st_buffer(buffer_size)  %>% # add 25 km buffer around bounding box (~0.25 degree as in bathymetry cropping)
    st_transform(crs = "+proj=longlat +datum=WGS84") # re-trasform to match eez geometry
  
  ## Crop eez object to polygon dimension
  eez_subset_crop = st_intersection(eezToCrop, poly)
  
  return(eez_subset_crop)
}
  
###### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




#' @title spLengthPlot 
#' @description create LFD plots by species
#' @param indall dataframe
#' @param nLimit minimum number of observation needed for species to be included
#' @param unit length unit
#' @param base_size font in plot base size
#' @return LFD plots by species

spLengthPlot <- function(indall, nLimit = 30, unit = "cm", base_size = 14) {
  
  sps <- indall %>% lazy_dt() %>% filter(!is.na(length)) %>% group_by(scientificname) %>% count() %>% filter(n > nLimit) %>% pull(scientificname) 
  
  if(length(sps) > 0) {
    
    indLen <- indall %>% lazy_dt() %>%
      filter(!is.na(length), scientificname %in% sps) %>% collect() %>% as.data.table()
    
    indLen <- janitor::remove_empty(indLen, which = "cols")
    
    meanIndLen <- indLen %>% lazy_dt() %>%  
      #group_by(scientificname) %>%
      group_by(scientificname, cruise) %>%
      summarise(medLength = median(length)) %>% 
      arrange(-medLength) %>% collect()
    
    #indLen[, scientificname := factor(scientificname, levels = meanIndLen$scientificname)]
    indLen[, scientificname := factor(scientificname, levels = unique(meanIndLen$scientificname))]
    # vlines <- seq(1:(length(levels(indLen$scientificname))-1))+0.5
    
    ggplot(data = indLen) + geom_histogram(aes(x = length*100), binwidth=1, position = 'dodge') +
      #geom_point(data = meanIndLen, aes(x = medLength, y = scientificname), shape = 95, size = 5) + 
      ylab('Frequency [n]') +
      xlab(paste0("Length (", unit, ")")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      #coord_cartesian(expand = FALSE) +
      theme_bw(base_size = base_size) +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(.~scientificname,scales='free_x') +
      scale_x_continuous(breaks = equal_breaks(n = 5)) 
    
  } else {
    
    ggplot() +
      annotate("text", x = 1, y = 1, label = paste0("None of the species has\n >", nLimit, " length measured individuals")) +
      ylab(paste0("Length (", unit, ")")) +
      xlab("Taxon name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  }
  
}



#' @title nice integer breaks for LFD plots
#' @description create pretty sequence for x axis breaks (length) 
#' @param n number of tick marks (average values, the number of breaks will depend from the number of size classes available).
#' @param unit if in m will adapt the rounding accordingly
#' @return sequence of numbers for breaks
equal_breaks <- function(n = 3, unit='cm',...){
  function(x){
    if(unit=='cm') {customDigit <- 0}
    if(unit=='m') {customDigit <- 2}
    # create sequence
    mySeq <- round(min(x),digits = customDigit):max(round(max(x),digits = customDigit))
    seq(min(mySeq),max(mySeq),by=round(length(mySeq)/n)) #, length=n)
  }
}


