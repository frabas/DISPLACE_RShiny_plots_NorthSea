## Map average layer files
## Author: Alexandros Kokkalis (alko@aqua.dtu.dk)
##         Francois Bastardie (fba@aqua.dtu.dk)

library(maptools)

## Functions ----
distance <- function (lon, lat, lonRef, latRef)  # vmstools::distance()
{
  pd <- pi/180
  a1 <- sin(((latRef - lat) * pd)/2)
  a2 <- cos(lat * pd)
  a3 <- cos(latRef * pd)
  a4 <- sin(((lonRef - lon) * pd)/2)
  a <- a1 * a1 + a2 * a3 * a4 * a4
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  return(6371 * c)
}

legend.gradient2 <-
  function(pnts, cols = heat.colors(100), limits = c(0, 1), title = "Legend", legend = "", ...) {
    pnts = try(as.matrix(pnts), silent = TRUE)
    if (!is.matrix(pnts))
      stop("you must have a 4x2 matrix")
    if (dim(pnts)[1] != 4 || dim(pnts)[2] != 2)
      stop("Matrix must have dimensions of 4 rows and 2 columms")
    if (length(cols) < 2)
      stop("You must have 2 or more colors")
    yvals = seq(min(pnts[, 2]), max(pnts[, 2]), length = length(cols) +
                  1)
    for (i in 1:length(cols)) {
      polygon(x = pnts[, 1], y = c(yvals[i], yvals[i], yvals[i +
                                                               1], yvals[i + 1]), col = cols[i], border = F)
    }
    text(max(pnts[, 1]), min(pnts[, 2]), labels = limits[1],
         pos = 4, ...)
    text(max(pnts[, 1]), max(pnts[, 2]), labels = limits[2],
         pos = 4, ...)
    start_pos <- (min(pnts[, 2])+((max(pnts[, 2])-min(pnts[, 2]))/length(legend))/2)
    for (i in seq_along(legend)) {
      text(max(pnts[, 1]), start_pos + ((i - 1) * ((max(pnts[, 2]) - min(pnts[, 2])) / length(legend)) ),
           labels = legend[i], pos = 4, ...)
    }
    text(min(pnts[, 1]) - 0.05, max(pnts[, 2]), labels = title, adj = c(0, -1), ...)
  }

Satellite.Palette.baseline <- colorRampPalette(c("cyan","aquamarine","orange","red"))

makeCumulativeMap <- function(scedir,
                              outdir,
                              scenarios = dir(scedir, "^sce[^_]*$"),
                              a_type="cumcatches",  # cumftime, cumsweptarea, nbchoked, cumulcatches_per_pop_popXX
                              in_relative=TRUE,
                              the_baseline= "scebaselineplgnb") {
  ## Add any of those needed as arguments to the function
  a_type2 = ""
  func = "ratio"    # or func="rate";
  field_pos = 4
  a_pop = ""
  selected_scenarios_for_plot = scenarios
  selected_scenarios_for_table = scenarios
  namesce = scenarios ## Add names here
  #selected_areas_for_table = c("7.g", "7.h")
  if(a_type=="cumcatches"){
  the_breaks_baseline = c(0.5, 1, round(exp(seq(0.5, 14, by = 4.5))))
  the_breaks = c(rev(-round(exp(seq(0, 7, by = 1)))),  0, round(exp(seq(0, 7, by = 1))))
  legend_text1 = "kg/km^2"
  }
  if(a_type=="cumulcatches_per_pop"){
  the_breaks_baseline = c(0.5, 1, round(exp(seq(0.5, 14, by = 4.5))))
  the_breaks = c(rev(-round(exp(seq(0, 7, by = 1)))),  0, round(exp(seq(0, 7, by = 1))))
  legend_text1 = "kg/km^2"
  a_pop = "_pop4"
  }
  if(a_type=="cumftime"){
  the_breaks_baseline=   c(0, round(exp(seq(-1.5, 3.5, by=0.3)),1), 10000) 
  the_breaks=  c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2))))
  legend_text1 = "h/km^2"    # if cumftime
  }
  gis_shape = list()
  a_width = 3400
  a_height = 3500
  xlims = c(-5,11)
  ylims = c(50, 62)
  #xcell = 12; ycell = 17
  xcell = 10; ycell = 10
  
  ## Create outdir if it does not exist
  if (!dir.exists(outdir)) dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  ## Rearrange scenarios to put baseline first
  selected_scenarios_for_table <- unique(c(the_baseline, selected_scenarios_for_table))
  selected_scenarios_for_plot <- unique(c(the_baseline, selected_scenarios_for_plot))
  namesce <- unique(c(the_baseline, namesce))

  ## Output table
  #table_obj <- matrix(0, nrow=length(selected_scenarios_for_table), ncol=length(selected_areas_for_table)+1)
  #rownames(table_obj) <- c(selected_scenarios_for_table)
  #colnames(table_obj) <- c(selected_areas_for_table, "Other")

  ## Output filenames
  nametype <- if(a_type2!="") paste0(a_type, a_pop,"over",a_type2) else paste0(a_type, a_pop)
  fn_tiff <- file.path(outdir, paste0("map_averaged_", nametype, "_selected_in_relative", in_relative, ".tiff"))
  fn_table <- file.path(outdir, paste0("table_", nametype,".txt"))

  ## Plot layout and plotting parameters
  m <- if (length(selected_scenarios_for_plot) %% 2 == 0) {
    matrix(seq(selected_scenarios_for_plot), ncol = 2)
  } else {
    matrix(c(rep(1, 4), seq(2, length(selected_scenarios_for_plot))), ncol = 2, byrow = TRUE)
  }

  layout(m)
  par(mar=c(2,2,3,1))
  par(oma=c(4,4,1,1))

  for (i in seq(selected_scenarios_for_table)) {
    sce <- selected_scenarios_for_table[i]
    f <- file.path(outdir, paste0(sce, "_average_",a_type,"_layer",a_pop,"ICESareas.Rds"))
    if (file.exists(f)) {
      this <- readRDS(f)
    } else {
      library(vmstools)
      data(ICESareas)

      ## Read in and preprocess the file (set small values to 0, add ICES areas, set to grid)
      this <- read.table(file=file.path(scedir, sce,
                                        paste("average_",a_type,"_layer",a_pop,".txt", sep='')), header=FALSE, skip = 1)
      colnames(this) <- c("node", "lat", "long", nametype)

      ## Set small values to 0
      this[, 4][this[, 4] < 0.1] <- 0

      #spPoint <- SpatialPoints(data.frame(x = this$long, y = this$lat),
      #                         proj4string = CRS(proj4string(ICESareas)))
      #this$area <- over(spPoint, ICESareas) #Area_27
      #levels(this$area)[! levels(this$area) %in% selected_areas_for_table] <- "Other"  ## BROKEN!
      # replace by:
      this$area <- NA
      
      #table_obj[sce, ] <-  tapply(this [, nametype], this$area, sum, na.rm=TRUE)[colnames(table_obj)]
      this$round_long <- round(this$long*xcell)
      this$round_lat  <- round(this$lat*ycell)

      # find out the grid res
      lo <- sort(this$round_long, decreasing=TRUE)
      la <- sort(this$round_lat, decreasing=TRUE)
      most_freq_in_long <- as.numeric(names(sort(table(diff(lo/xcell)), decreasing=TRUE)[2]))
      most_freq_in_lat  <- as.numeric(names(sort(table(diff(la/ycell)), decreasing=TRUE)[2]))
      xcellkm <- distance(this$round_long[1]/xcell, mean(this$round_lat)/ycell, (this$round_long[1]/xcell) + most_freq_in_long, mean(this$round_lat)/ycell)
      ycellkm <- distance(mean(this$round_long)/xcell, this$round_lat[2]/ycell , mean(this$round_long)/xcell, (this$round_lat[2]/ycell) + most_freq_in_lat)

      if (func!="rate") this[,nametype]  <- round(this[,nametype])  /(xcellkm * ycellkm) # (5.576564*6.540486)  # if 15 and 20 then divide by cell area 8.925*5.561km  check??

      this$cell_id <-  paste(this$round_long, this$round_lat, sep="_")
      saveRDS(this, file = f)
    }

    if (sce == the_baseline) {
      the_baseline_layer <- this
      the_baseline_layer <- aggregate(the_baseline_layer[,nametype],
                                      list(the_baseline_layer$round_long, the_baseline_layer$round_lat, the_baseline_layer$cell_id),
                                      sum, na.rm=TRUE)
      colnames(the_baseline_layer) <- c("round_long", "round_lat", "cell_id", nametype)


      maxbr <- tail(the_breaks_baseline, 1)
      the_baseline_layer[, nametype] <- replace (the_baseline_layer[, nametype],
                                                the_baseline_layer[, nametype] > maxbr,
                                                maxbr)

      the_points <- tapply(the_baseline_layer[,nametype],
                           list(the_baseline_layer$round_lat, the_baseline_layer$round_long), sum, na.rm = TRUE)

      #the_points <- replace (the_points, the_points>the_breaks_baseline[length(the_breaks_baseline)], the_breaks_baseline[length(the_breaks_baseline)])
      image(
        x=as.numeric(as.character(colnames(the_points)))/xcell,     # 8.925km  at 53 degree N
        y=as.numeric(as.character(rownames(the_points)))/ycell,     # 5.561km at 53 degree N
        z= t(the_points),  # convert in tons
        breaks=c(the_breaks_baseline),
        col =  Satellite.Palette.baseline(length(the_breaks_baseline[-1]))  ,
        useRaster=FALSE,
        xlab="",
        ylab="",
        asp=1,
        axes=FALSE , xlim=xlims, ylim=ylims
      )
      library(maps)
      if (!is.null(gis_shape))
        if(length(gis_shape[[sce]])>0)
          for (i in 1:length(gis_shape[[the_baseline]]))
            plot(gis_shape[[the_baseline]][[i]], add=TRUE, col=grey(0.8), border=TRUE)

      graphics::box()
      mtext(side=3, sub("sce", "Scenario: ", namesce[i]), cex = 1.2, line = 0.5)
      axis(1, cex.axis = 1.2)
      axis(2, las = 2, cex.axis = 1.2)

  
      x = c(xlims[1] + 0.2, xlims[1] + 0.4, xlims[1] + 0.4, xlims[1] + 0.2)
      y = c(ylims[1] + 0.5, ylims[1] + 3, ylims[1] + 3, ylims[1] + 0.5)
      the_breaks_leg <- NULL
      a_title <- substitute( expression(paste(legend_text1, km^2)), list(legend_text1 = legend_text1))
      if (func == "rate") a_title <- legend_text1  # overwrite
      for (i in 1:length(the_breaks_baseline)) {
        if (the_breaks_baseline[i] > 1) {
          the_breaks_leg[i] <- round(the_breaks_baseline[i])
        } else{
          the_breaks_leg[i] <- the_breaks_baseline[i]
        }
      }
      legend.gradient2(cbind(x = x , y = y ), cols=Satellite.Palette.baseline(length(the_breaks_baseline[-1])),
                       limits = "", title = eval(a_title),
                       legend = the_breaks_leg,
                       cex = 1, col = "black")

      maps::map("mapdata::worldHires", add = TRUE, fill = TRUE, col = "darkgrey")
    } else { ## Not the baseline

      this <- aggregate(this[,nametype], list(this$round_long, this$round_lat, this$cell_id), sum, na.rm=TRUE)
      colnames(this) <- c("round_long", "round_lat", "cell_id", nametype)

      # Merge!
      this           <- merge(the_baseline_layer, this, by.x="cell_id", by.y="cell_id")

      # filter for close to 0 values
      this[,paste0(nametype,".x")] <- replace(this[,paste0(nametype,".x")], this[,paste0(nametype,".x")]<1e-1, 0)
      this[,paste0(nametype,".y")] <- replace(this[,paste0(nametype,".y")], this[,paste0(nametype,".y")]<1e-1, 0)

      # percent
      this[,nametype]  <- (100 * as.numeric(as.character(this[,paste0(nametype,".y")])) / as.numeric(as.character(this[,paste0(nametype,".x")])) )  -100


      # CAUTION!!!!: correct for area with low absolute value to avoid visual effect
      xcol <- paste0(nametype,".x")
      this[,nametype] [ this[, xcol] <quantile(this[, xcol] [ this[, xcol] !=0], prob=0.05)]  <- 0


      if(in_relative){
        the_points <- tapply( this[,nametype],
                              list(this$round_lat.y, this$round_long.y), sum)
        Satellite.Palette <-colorRampPalette(c("cyan","aquamarine","white","yellow","red"))
      } else{
        the_points <- tapply(this[,paste0(nametype,".y")],
                             list(this$round_lat.y, this$round_long.y), sum)
        the_breaks <-  the_breaks_baseline
        Satellite.Palette <- Satellite.Palette.baseline
      }

      maxbr <- the_breaks[length(the_breaks)]
      the_points[the_points > maxbr] <- maxbr

      if (sce %in% selected_scenarios_for_plot){
        image(
          x=as.numeric(as.character(colnames(the_points)))/xcell,   #15
          y=as.numeric(as.character(rownames(the_points)))/ycell,   # 20
          z= t(the_points),  # convert in tons
          breaks = the_breaks,
          col = Satellite.Palette(length(the_breaks[-1])),
          useRaster=FALSE,
          xlab="",
          ylab="",
          axes=FALSE,
          asp=1,
          xlim=xlims, ylim=ylims
        )
        if (!is.null(gis_shape))
          if (length(gis_shape[[sce]])>0)
            for (i in 1:length(gis_shape[[the_baseline]]))
              plot(gis_shape[[the_baseline]][[i]], add=TRUE, col=grey(0.8), border=FALSE)
        maps::map("mapdata::worldHires", add = TRUE, fill = TRUE, col = "darkgrey")

        graphics::box()
        mtext(side=3, sub("sce", "Scenario: ", namesce[i]), cex=1.2, line=0.5)
        axis(1, cex.axis=1.2)
        axis(2, las=2, cex.axis=1.2)

        x = c(xlims[1]+0.2, xlims[1]+0.4, xlims[1]+0.4, xlims[1]+0.2)
        y = c(ylims[1]+0.5, ylims[1]+3, ylims[1]+3, ylims[1]+0.5)
        if (in_relative) a_title_leg <- substitute( expression(paste("% difference per cell")))
        if (!in_relative) a_title_leg <- substitute( expression(paste(legend_text1, km^2)), list(legend_text1 = legend_text1))
        the_breaks_leg <- NULL
        #for(i in 1: length(the_breaks[-1])){ if(the_breaks[i]>1) {the_breaks_leg[i] <- round(the_breaks[i])} else{the_breaks_leg[i]<- the_breaks[i]}}
        the_breaks_leg <- the_breaks
        legend.gradient2 (cbind(x = x , y = y ), cols=Satellite.Palette(length(the_breaks[-1])),
                          limits="", title=eval(a_title_leg),
                          legend= the_breaks_leg,
                          cex=1.0, col="black")


        # add closure polygons:
        if (!is.null(gis_shape)) if(length(gis_shape[[sce]])>0) for (i in 1:length(gis_shape[[sce]])) plot(gis_shape[[sce]][[i]], add=TRUE,  border=grey(0.2), col=NA)
      }
    }
  }
}

