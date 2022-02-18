
#' Produce polygon plots of time series for aggregated loglike indicators
#'
#' This function produces all accumulated time series over month to compare scenario outcomes
#' All the plots are being stored in a polygons_plots folder that can further be found in the output folder.
#' (compare up to 5 scenarios simultaneously)
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' \dontrun{
#' general <- setGeneralOverallVariable(main_path_outputs =file.path("C:","DISPLACE_outputs"),
#'                                       case_study="DanishFleet",
#'                                       igraph=41,
#'                                       a.year="2015",
#'                                       a.country="DEN",
#'                                       nbpops=39,
#'                                       nbszgroup=14,
#'                                       namefolderinput="DanishFleet",
#'                                       the_scenarios= c("svana_baseline",
#'                                                       "svana_sub1mx20",
#'                                                       "svana_sub4mx20",
#'                                                       "svana_sub4mx5ns20bt",
#'                                                       "svana_sub4mx20ns5bt",
#'                                                       "svana_sub4mx5ns5bt" ),
#'                                       nbsimus=20
#'                                       )
#'
#'
#' loadLoglikeFiles(general=general, use_port_info=FALSE)
#'
#'
#'
#' polygonPlotsFromAggLoglikeFiles (general=general,
#'                                            the_baseline="svana_baseline",
#'                                            a_width=3500,
#'                                            a_height=1000,
#'                                            selected_scenarios=general$namefolderoutput[1:3]
#'                                            )
#'
#'   }
do_polygon_plot <- function(
  a_variable="gradva",
  nby=5,
  a_set_of_scenarios=general$namefolderoutput[1:3],
  the_scenario_names= general$namefolderoutput[1:3],
  name_set_of_sces= "setA",
  selected=selected,
  export=FALSE,
  documsum = FALSE,
  a_xlab="# months",
  a_ylab="Accumulated Gross Added Value (millions Euro)",
  add_legend=FALSE,
  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5),
                  rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)) ,
  a_width=a_width,
  a_height=a_height
) {
  ## Made docsum into a function: it is `cumsum` if cumulative sum is needed documsum is the cumsum function otherwise it's just the identity function
  documsum <- if (documsum) cumsum else identity ## if(a_variable %in% c("GVAPerFTE")) identity else cumsum

  sce <- a_set_of_scenarios
  obj <- lapply(sce, function(x) get(paste("lst_loglike_agg_weight", selected, x, sep=''), env=.GlobalEnv))

  # caution: complete missing records if 0s in given year.month
  complete_all_year_month <- function (x, years_span=2019:(2019+nby-1)){
    allcombi              <- expand.grid(month=sprintf("%02d", 1:12), year=years_span)
    allcombi$year.month   <- paste0(allcombi$year,".",allcombi$month)
    allcombi              <- cbind.data.frame(year.month=allcombi$year.month,  matrix(0, ncol=ncol(x)-1))
    colnames(allcombi)    <- colnames(x)
    allmissingcombi       <- allcombi[!allcombi$year.month %in% x$year.month,]
    dd <- rbind.data.frame(x, allmissingcombi)
    rownames(dd) <- dd$year.month
    dd <- dd[as.character(allcombi$year.month),] # get the right order...
    return(dd)
  }

  obj <- lapply(obj, function(o) lapply(o, complete_all_year_month))
  simu_names <- Reduce(intersect, lapply(obj, names))

  ## a nice quantile plot for profit
  #plot(cumsum(loglike_Scenario1_save[loglike_Scenario1_save$simu=="simu2",]$gradva), type="l", col=2)
  mat <- lapply(seq_along(obj), function(i) {
    res <- matrix(NA, nrow=length(simu_names),  ncol=nby*12)
    rownames(res) <- simu_names
    return(res)
  })

  for (sim in simu_names){
    for (i in seq_along(mat)) {
      mat[[i]][sim, ] <- documsum(as.numeric(obj[[i]][[sim]][,a_variable]) )[1:dim(mat[[i]])[2]]
    }
  }

  sim_ref <- names(which.max (apply(mat[[1]], 1, function(x) sum(x, na.rm=TRUE))) )

  er <- try(   {
    qs <- lapply(mat, function(m) apply(m[,1:(nby*12)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE))
    ylim <- range(unlist(qs)) / 1e6
    xs <-  ym2date(obj[[1]][[1]]$year.month)
    plot(xs, obj[[1]][[1]][, a_variable], ylim = ylim, type = "n", xlab = "Year", ylab = "", axes = FALSE)
    makeone <- function(i) {
      ## xs <- (1:ncol(mat[[i]]))[1:(nby*12)]

      polygon(x=c(xs, rev(xs)), y=c(qs[[i]]["5%", ], rev(qs[[i]]["95%", ])) / 1e6,
              col = color_legend[i], border = NA)
    }
    lapply(seq_along(mat), makeone)

    axis(2, las=2)
    axis(1)
    if(add_legend) legend("topleft", fill=color_legend, border =color_legend, legend=the_scenario_names, cex=1.3, bty="n")
    graphics::box()

    abline(h=0, lty=2, col=grey(0.7))
    mtext(side = 2, text = a_ylab, line = 3.6, cex=1.2)
  }, silent=TRUE)

  if(class(er)=="try-error"){
    print(paste("no data."))
  }

  return()
}



polygonPlotsFromAggLoglikeFiles <- function(general=general,
                                            the_baseline="svana_baseline",
                                            a_width=3500,
                                            a_height=1000,
                                            selected_scenarios=general$namefolderoutput[1:3],
                                            the_scenario_names=general$namefolderoutput[1:3],
                                            nby=5
)
{
  sce1 <- selected_scenarios[1] # init


  var_names <- colnames(get(paste("lst_loglike_agg_weight_","selected_set1_",sce1, sep=''), env=.GlobalEnv)[[1]])
  # dir.create(file.path(general$main.path, general$namefolderinput,"polygon_plots"))

  for (a_var in var_names[-1]){

    ##    graphics.off()
    ##    tiff(file=file.path(general$main.path, general$namefolderinput, "polygon_plots", paste("accumulated_per_scenario_polygon_", a_var, ".tiff", sep="")),
    ##                                  width = a_width, height = a_height,   compression="lzw",
    ##                                   units = "px", pointsize = 12,  res=300)
    par(mfrow=c(1,3))
   par(mar=c(2,4.4,2,2))
    par(oma=c(4,4,1,1))


    cat (paste0("plot for ", a_var,"\n"))

    do_polygon_plot(
      a_variable=a_var,
      nby=nby,
      a_set_of_scenarios= selected_scenarios,
      the_scenario_names= selected_scenarios,
      name_set_of_sces= "setA",
      selected="_selected_set1_",
      export=FALSE,
      a_xlab="# months",
      a_ylab = switch(a_var,
                      gradva = "Acc. GVA (mio Euro)",
                      rev_explicit_from_av_prices = "Income from landings (mio Euro)",
                      "Accumulated Gross Added Value (millions Euro)"),
      add_legend=TRUE,
      #color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
      color_legend= c(rgb(33/255,102/255,172/255,0.6), rgb (77/255,146/255,33/255,0.6), rgb(254/255,224/255,139/255,0.6), rgb(178/255,24/255,43/255,0.6), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),                                      
         a_width=a_width,
      a_height=a_height)
    ## cat (paste0("plot for ", a_var," for set1...ok\n"))

    # do_polygon_plot(
    #                  a_variable=a_var,
    #                  nby=nby,
    #                  a_set_of_scenarios= selected_scenarios,
    #                  the_scenario_names= selected_scenarios,
    #                  name_set_of_sces= "setA",
    #                  selected="_selected_set2_",
    #                  export=FALSE,
    #                  a_xlab="# months",
    #                 if(a_var=="gradva") {a_ylab="Acc. GVA (mio Euro)"} else{ if(a_var=="rev_explicit_from_av_prices"){a_ylab="Income from landings (mio Euro)"} else{a_ylab=a_var}},
    #                       add_legend=TRUE,
    #                  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
    #                  a_width=a_width,
    #                  a_height=a_height
    #                  )
    #   cat (paste0("plot for ", a_var," for set2...ok\n"))
    #
    # do_polygon_plot(
    #                  a_variable=a_var,
    #                  nby=nby,
    #                  a_set_of_scenarios= selected_scenarios,
    #                  the_scenario_names= selected_scenarios,
    #                  name_set_of_sces= "setA",
    #                  selected="_selected_set3_",
    #                  export=FALSE,
    #                  a_xlab="# months",
    #                  if(a_var=="gradva") {a_ylab="Acc. GVA (mio Euro)"} else{ if(a_var=="rev_explicit_from_av_prices"){a_ylab="Income from landings (mio Euro)"} else{a_ylab=a_var}},
    #                  add_legend=TRUE,
    #                  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
    #                  a_width=a_width,
    #                  a_height=a_height
    #                  )
    #    cat (paste0("plot for ", a_var," for set3...ok\n"))
    #
    #
    #  mtext("# months", 1, line=2, cex=1.5, outer=TRUE)
    #  mtext(side=2,"Indicators",line=2, cex=1.5, outer=TRUE)
    #
    #
    #
    #  dev.off()

  } # end a_var


  return()
}




##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!CALLS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##



# if(TRUE){
#  # GENERAL SETTINGS
#   general <- list()
#
#   general$case_study <- "CelticSea"
#
#  # if(.Platform$OS.type == "unix") {}
#  #  general$main.path         <- file.path("~","ibm_vessels","DISPLACE_outputs")
#  #  general$main.path.igraph  <- file.path("~","ibm_vessels","DISPLACE_input_raw", "igraph")
#  #  general$main.path.param   <- file.path("~","ibm_vessels", paste("DISPLACE_input_",general$case_study, sep=""))
#  #  general$main.path.ibm     <- file.path("~","ibm_vessels", paste("DISPLACE_input_", general$case_study, sep=''))
#  #  # do not forget to install the R packages on the qrsh interactive node linux platform, i.e. R > install.packages("data.table"), etc.
#  #  # (and possibly kill the current jobs on HPC with the command qselect -u $USER | xargs qdel)
#  #  # submit the shell to HPC with the command qsub ./IBM_processoutput_plots_for_loglike.sh
#  #
#  # if(.Platform$OS.type == "windows") {
#  #  general$main.path         <- file.path("C:","DISPLACE_outputs")
#  #  general$main.path.igraph  <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_",general$case_study, sep=""), "graphsspe")
#  #  general$main.path.param   <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_gis_",general$case_study, sep=""))
#  #  general$main.path.ibm     <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_", general$case_study, sep=''))
#  # }
#
#  if(general$case_study=="CelticSea"){
#     general$igraph            <- 3
#     general$a.year            <- "2015"
#     general$a.country         <- c("IRL", "BEL", "FRA", "GBR", "NLD")
#     general$nbpops            <- 28
#     general$nbszgroup         <- 14
#     general$namefolderinput   <- "CelticSea"
#     general$use_sqlite        <- FALSE
#
#     general$namefolderoutput   <- c(
#                                  "scesizespectra",
#                                  "scebaseline",
#                                  "sceavchok",
#                                  "sceavchokpszpectra",
#                                  "scesizespectrastopifchok",
#                                  "sceavchokpszpctrastopifchok",
#                                  "sceavhtariffspszpctratariffs",
#                                  "scetrgthtariffspszpctratariffs"
#                                  #"sceavchokpszpctrafmsyrange",
#                                  #"sceavchokpszpctrafmsyrangstopifchok",
#
#                                  )
#      general$namesimu           <- list(
#                                 "scesizespectra"=   paste("simu", c(1:10), sep=''),
#                                  "scebaseline"=   paste("simu", c(1:10), sep=''),
#                                  "sceavchok"=   paste("simu", c(1:10), sep=''),
#                                  "sceavchokpszpectra"=   paste("simu", c(1:10), sep=''),
#                                  "scesizespectrastopifchok"=   paste("simu", c(1:10), sep=''),
#                                  "sceavchokpszpctrastopifchok"=   paste("simu", c(1:10), sep=''),
#                                  "sceavhtariffspszpctratariffs"=   paste("simu", c(1:10), sep=''),
#                                  "scetrgthtariffspszpctratariffs"=   paste("simu", c(1:10), sep='')
#                                  #"sceavchokpszpctrafmsyrange"=   paste("simu", c(1:10), sep=''),
#                                  #"sceavchokpszpctrafmsyrangstopifchok"=   paste("simu", c(1:10), sep='')
#                                  )
#      the_scenarios1 <-  c("Size spectra Baseline",
#                           " - Predation",
#                           " - Predation + Avoiding choke spp.",
#                           "+ Avoidance",
#                           "+ Stop if choked",
#                           "+ Avoidance + Stop if choked",
#                           "+ Avoid High Tariffs",
#                           "+ Focus on High Tariffs"
#                           )
#
#
#
#
#    }
#
# } # end FALSE
#
# source(file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis_CelticSea","DISPLACE_R_outputs_ForCelticSea","loadAggLoglikeFiles.R"))
# loadLoglikeFiles(general=general, use_port_info=FALSE)
#
# polygonPlotsFromAggLoglikeFiles (general=general,
#                                             the_baseline="scebaseline",
#                                             a_width=3500,
#                                             a_height=1000,
#                                             selected_scenarios=c(
#                                            "scesizespectra",
#                                            #"scesizespectrastopifchok",
#                                            "sceavchokpszpctrastopifchok",
#                                            "sceavhtariffspszpctratariffs",
#                                            "scetrgthtariffspszpctratariffs"
#                                                            ),
#                                             the_scenario_names=c(
#                                            "Baseline Size Spectra",
#                                            #"+ Stop if choked",
#                                            "+ Avoidance + Stop if choked",
#                                            "+ Avoid High Tariffs",
#                                            "+ Focus on High Tariffs"
#                                               ),
#                                               nby=5
#
#                                             )
#
