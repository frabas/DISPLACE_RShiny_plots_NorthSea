
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)

shinyOptions(shiny.autoreload = TRUE)
shinyOptions(shiny.launch.browser = TRUE)

## Source R scripts
source("R-scripts/mapAverageLayerFiles.R", local = TRUE)
source("R-scripts/polygonPlotsFromAggLoglikeFiles.R", local = TRUE)
source("R-scripts/polygonPlotsFromPopDynFiles.R", local = TRUE)
source("R-scripts/helperFunctions.R", local = TRUE)
source("R-scripts/barplotLanDisPerScenario.R", local = TRUE)
source("R-scripts/responseCurvesSBBandF.R", local = TRUE)
source("R-scripts/AnnualIndicTraj.R", local = TRUE)
source("R-scripts/AggLoglikeTraj.R", local = TRUE)

sbox <- shinydashboard::box

## Find available RData files and pick out scenarios
loglikefns <- dir("data", "loglike.*RData", full.names = TRUE)
loglikescenarios <- gsub("^.*agg_|[.]RData", "", loglikefns)
popdynfns <- dir("data", "popdyn.*RData", full.names = TRUE)
popdynscenarios <- gsub("^.*popdyn_|[.]RData", "", popdynfns)
annualindicfns <- dir("data", "lst_annualindic.*RData", full.names = TRUE)
annualindicscenarios <- gsub("^.*lst_annualindic_|[.]RData", "", popdynfns)

## Load all loglike and popdyn files
for (f in c(loglikefns, popdynfns, annualindicfns)) load(f, envir = .GlobalEnv)

## and read some tables
fleetindicfns <- dir("data", "outcomes_all_simus_relative_to_baseline_sce_*", full.names = TRUE)
fleetindicnames <- gsub("^.*outcomes_all_simus_|[.]txt", "", fleetindicfns)
for (fi in 1: length(fleetindicfns)) assign(fleetindicnames[fi], read.table(fleetindicfns[fi], header=TRUE, sep=";"))


## Read population names
## setwd(file.path("D:","FBA","DISPLACE_RShiny_plots_NorthSea","data"))
## popnames <- read.table("NorthSea/pop_names_NorthSea.txt", header = TRUE); save("popnames", file = "popnames.Rdata")
load(file = "data/popnames.Rdata")

convertMenuItem <- function(tabName, mi) {
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

## User interface ----
ui <- dashboardPage(
  dashboardHeader(title = "DISPLACE output viewer"),
  dashboardSidebar(
    sidebarMenu(
      sidebarMenu(id = "menu",
                  convertMenuItem("intro",
                                  menuItem("Model info", tabName = "intro", icon = icon("info"), startExpanded = TRUE)),
                  convertMenuItem("map",
                                  menuItem("Maps", tabName = "map", icon = icon("map"),
                                           selectInput("sel.mapquantity", "Select quantity", choices = selquantity(), multiple = FALSE, selectize = FALSE),
                                           selectInput("sel.is_relative", "Relative to baseline sce",  choices = c(TRUE, FALSE), selected = FALSE,
                                                       multiple = FALSE, selectize = FALSE))),
                  convertMenuItem("ts",
                                  menuItem("Time series", tabName = "ts", icon = icon("chart-line"),
                                           selectInput("sel.sce", "Select scenarios", choices = selsce(), selected = selsce(), multiple = TRUE, selectize = FALSE),
                                           selectInput("sel.var", "Select a variable", choices = selvar(), selected = "gradva", multiple = FALSE),
                                           checkboxInput("quantCumSum", label = "Cumulative sum", value = TRUE))),
                  convertMenuItem("tab_landis_perpop",
                                  menuItem("Populations", tabName = "tab_landis_perpop", icon = icon("chart-bar"),
                                           selectInput("sel.sce2", "Select scenarios", choices = selsce(), selected = selsce(), multiple = TRUE, selectize = FALSE),
                                           selectInput("sel.pop", "Select populations", choices = selpop(), selected = c("pop.1", "pop.2", "pop.3", "pop.4", "pop.11",
                                                                "pop.12","pop.13", "pop.14", "pop.22", "pop.23", "pop.27", "pop.31"), multiple = TRUE, selectize = FALSE),
                                           selectInput("sel.indic", "Select indicators", choices = selindic(), selected = c("F/Finit"), multiple = TRUE, selectize = FALSE),
                                           selectInput("sel.sum.szgroups", "Sum over size groups", choices = c(TRUE, FALSE), selected = TRUE,
                                                       multiple = FALSE, selectize = FALSE))),
                  convertMenuItem("kobe_plot",
                                  menuItem("Kobe plots (pop)", tabName = "kobe_plot", icon = icon("check", lib = "glyphicon"),
                                          selectInput("sel.sce3", "Select scenarios", choices = selsce(), selected = selsce(), multiple = TRUE, selectize = FALSE),
                                           selectInput("sel.pop2", "Select populations", choices = selpop(), selected = c("pop.1", "pop.2", "pop.3", "pop.4", "pop.11",
                                                                "pop.12","pop.13", "pop.14", "pop.22", "pop.23", "pop.27", "pop.31"), multiple = TRUE, selectize = FALSE),
                                                               selectInput("relative_to_init", "Relative to init",  choices = c(TRUE, FALSE), selected = TRUE,
                                                       multiple = FALSE, selectize = FALSE))),                                     
                  convertMenuItem("kobe_plot_loglike",
                                  menuItem("Kobe plots (catch)", tabName = "kobe_plot_loglike", icon = icon("check", lib = "glyphicon"),
                                          selectInput("sel.sce4", "Select scenarios", choices = selsce(), selected = selsce(), multiple = TRUE, selectize = FALSE),
                                          selectInput("agg_level", "Aggregation level",  choices = c("All", "Country", "Metier", "Vessel"), selected = "All",
                                                       multiple = FALSE, selectize = FALSE),
                                          selectInput("sel_countries", "Country",  choices = c('BEL','DEU','DNK','FIN','FRA','GBR','IRL','NLD','SWE'), selected = "All",
                                                       multiple = FALSE, selectize = FALSE),
                                          selectInput("sel_metids", "Metiers IDs",  choices = c( 1,11,14, 15, 21, 25), selected = "All",
                                                       multiple = TRUE, selectize = FALSE),
                                          selectInput("sel_vids", "Vessels IDs",  choices = c("BEL013161987"), selected = "All",
                                                       multiple = TRUE, selectize = FALSE)))
                                         
              
      
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("intro",
              h2("Climate change and the Common Fisheries Policy: North Sea case study with the DISPLACE spatial fisheries model"),
              sbox(width = 12, title = "Background", status = "primary", solidHeader = FALSE, collapsible = FALSE,
                   div("We present the outcomes of the DISPLACE agent-based modelling platform for simulating bio-economic fisheries dynamics and clarifying options for sustainable and viable fisheries in the North Sea.
                       In the North Sea. This study focuses on the most important species and stocks in terms of fisheries resources for the Danish fleet in the North Sea i.e. cod, haddock, plaice, sole, herring, sprat and other species, which constitute most of the total commercial catch of the Danish fleet. The ICES North Sea Fisheries Assessment (ICES WGNSSK 2020) and widely distributed stocks WGWIDE Working Groups focus on assessing 45 stocks that we have integrated into the North Sea DISPLACE model. Besides stock number-at-age issued by the last available ICES analytical assessment estimates (2019), we included fish life-history and ontogenetic growth for modelling the different populations with a body size-based model. 
                       The main environmental drivers affected by climate change and affecting the productivity of 
                       target and non-target species include increasing temperature. We defined two environmental scenarios for the North Sea region (current conditions, and worst-case scenario). These scenarios correspond to particular climatic paths, respectively no further change in the climate, and one pessimistic IPCC scenario (RCP8.5).
                       ")),
              sbox(width = 6, title = "Study area", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                   div(img(src = "studyAreaMap.png", width = "35%"))),
              sbox(width = 6, title = "Species", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                   div(tableOutput("speciesTable"))),
              sbox(width = 6, title = "Gear categories", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                   div(tableOutput("gearTable"))),
              sbox(width = 6, title = "Conditioning fisheries", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                   div("Fishing vessels of the North Sea international fisheries are considered. "), collapsed = FALSE)),

      tabItem("map",
              plotOutput("cumulativeMap", height = "500px")
      ),
      tabItem("ts",
              fluidRow(
                sbox(width = 12, plotOutput("linePlot"), title = "", status = "primary", solidHeader = FALSE)
              )),
      tabItem("tab_landis_perpop",
              fluidRow(
              sbox(width = 6, plotOutput("catchTimeSeriesPlot"), title = "Catch development over time", status = "primary", solidHeader = TRUE),
              sbox(plotOutput("barplot_landis_perpop"), title = "Landings per population", solidHeader = TRUE, status = "primary"),
              sbox(width = 6, plotOutput("populationSizePlot", height = "auto"), title = "Population size", status = "primary", solidHeader = TRUE),
              sbox(width = 6, plotOutput("annualIndicPlot", height = "auto"), title = "Annual Indicator", status = "primary", solidHeader = TRUE),
              sbox(width = 6, plotOutput("fleetIndicatorsPlot", height = "auto"), title = "Fleet Indicators", status = "primary", solidHeader = TRUE))),
      tabItem("tab_plotlymap",
              plotlyOutput("cumulativeMaps", height = "800px")),
      tabItem("kobe_plot",
              fluidRow(
                sbox(width = 12, plotOutput("kobe_plot"), title = "", status = "primary", solidHeader = FALSE)
              )),
      tabItem("kobe_plot_loglike",
              fluidRow(
                sbox(width = 6, plotOutput("kobe_plot_loglike"), title = "", status = "primary", solidHeader = FALSE)
              ))
    )
  )
)

## Server side logic ----
server <- function(input, output) {
  output$speciesTable <- renderTable(read.csv("data/species.csv"))
  output$gearTable <- renderTable({
    tbl <- read.csv("data/gears.csv")
    names(tbl) <- c("Gear", "Code")
    tbl
  })

  output$cumulativeMap <- renderPlot({
     scedir <- "data/NorthSea/"
    # scedir <- ""
    scenarios <- dir(scedir, "^sce[^_]*")
     m <- regexpr("sce[^_]*", scenarios)
     scenarios <- unique(regmatches(scenarios, m))
    first <- function(x) x[1]
    #scenarios <- unique(sapply(strsplit(dir("output", ".*Rds"), split = "_"), first))
    outdir <- "output"
  
    makeCumulativeMap(scedir = scedir, outdir = outdir, scenarios = scenarios,
                      a_type = input$sel.mapquantity, in_relative = as.logical(input$sel.is_relative))

  })

    output$linePlot <- renderPlot({
    req(input$sel.var, input$sel.sce)
    par(mar = c(4, 5, 1, 1))
    do_polygon_plot(
      a_variable = input$sel.var,
      nby = 10,
      documsum = input$quantCumSum,
      a_set_of_scenarios = input$sel.sce,
      the_scenario_names =names(selsce()),
      name_set_of_sces = "setA",
      selected = "_selected_set1_",
      export = FALSE,
      a_ylab = switch(input$sel.var,
                      gradva = "Accumulated Gross Value Added (million euros)",
                      rev_from_av_prices = "Income from landings (million euros)",
                      effort = "Effort",
                      nbtrip = "Number of trips",
                      totland = "Total landings",
                      ""),
      add_legend = TRUE,
      color_legend = c(rgb(94/255,79/255,162/255,0.5), rgb(158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4),
                       rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
      a_width = 3500,
      a_height = 1000

    )
  })

  output$catchTimeSeriesPlot <- renderPlot({
    ## ColorBrewer: paired
    ##cols <- c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a")
    ## ColorBrewer: set3
    ##cols <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd")
    cols <-  c('#7FC97F','#BEAED4','#FDC086','#FFFF99','#386CB0','#F0027F','#BF5B17','#666666','#1B9E77','#D95F02','#7570B3',
               '#E7298A','#66A61E','#E6AB02','#A6761D','#666666','#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99','#E31A1C',
               '#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A','#FFFF99','#B15928','#FBB4AE','#B3CDE3','#CCEBC5','#DECBE4','#FED9A6',
               '#FFFFCC','#E5D8BD','#FDDAEC','#F2F2F2','#B3E2CD','#FDCDAC','#CBD5E8','#F4CAE4','#E6F5C9','#FFF2AE','#F1E2CC',
               '#CCCCCC','#E41A1C','#377EB8','#4DAF4A','#984EA3','#FF7F00','#FFFF33','#A65628','#F781BF','#999999','#66C2A5',
               '#FC8D62','#8DA0CB','#E78AC3','#A6D854','#FFD92F','#E5C494','#B3B3B3','#8DD3C7','#FFFFB3','#BEBADA','#FB8072',
               '#80B1D3','#FDB462','#B3DE69','#FCCDE5','#D9D9D9','#BC80BD','#CCEBC5','#FFED6F')
    par(mar = c(6,4,3.5,0.9), xpd = TRUE)
    #onesim <- lst_loglike_agg_weight_all_scebaseline[[1]]
    add <- FALSE
    for (s in input$sel.sce2) {
      onesim <- get(paste0("lst_loglike_agg_weight_all_", s))[[1]]
      onesim <- onesim[onesim$year.month != "NA.NA", ]
      ym <- ym2date(onesim$year.month)
      nms <- names(onesim)
      selected <- onesim[, nms %in% input$sel.pop, drop = FALSE] / 1000
      maxima <- apply(selected, 2, max)
      limits <- c(0, 10, 100, 1000, Inf)
      labels <- paste("Max catches: ", paste(limits[-5], limits[-1], sep = "-"), "tonnes")
      labels[length(labels)] <- "Max catches: > 1000 tonnes"
      lvls <- droplevels(cut(maxima, limits, include.lowest = TRUE,
                             labels = labels))
      switch(length(levels(lvls)),
             "1" = par(mfrow = c(1,1)),
             "2" = par(mfrow = c(2,1)),
             "3" = par(mfrow = c(2,2)),
             "4" = par(mfrow = c(2,2)))
      for (l in levels(lvls)) {
        pops <- selected[, lvls == l, drop = FALSE]
        nms <- as.vector(sapply(names(pops), function(x) popnames$spp[paste0("pop.", popnames$idx) == x]))
        cls <- cols[as.integer(sub("pop.", "", names(pops))) + 1]
        matplot(ym, pops,
                type = "l", ylab = "Catch (tonnes)", xlab = "", add = add, lty = 1, lwd = 3,
                col = cls)
        mtext(l, line = 0.5, cex = 1.3)
        legend("bottomleft", bty = "n", legend = nms, col = cls, inset = c(0, -0.8),
               lty = 1, seg.len = 1, lwd = 3, box.col = "#00000022", ncol = 3, cex = 0.8)
      }
    }
  })


  output$fleetIndicatorsPlot <- renderPlot({

    selected <- "_selected_set1_"
    selected <- "_selected_set2_"
    selected <- "_selected_set3_"

    ## par(mar = c(6,4,3.5,0.9), xpd = TRUE)

    outcomes <- get(paste0("relative_to_baseline_sce_", selected))
    ## CAUTION: (not the same levels when reading or when using directly the obj in the env)
    # levels(outcomes$scenario) <-  c("sceavchok","sceavchokpszpctrastopifchok",
    #                                "sceavchokpszpectra",
    #                                "sceavhtariffspszpctratariffs",   "scebaseline",
    #                                "scesizespectrastopifchok", "scetrgthtariffspszpctratariffs")


    # add baseline at 0,0,0, etc.
    baseline <- outcomes[outcomes$scenario == "scefmsylowplgnb",]  # init
    baseline$ratio_percent <- 0
    baseline$scenario <- "scebaselineplgnb"
    outcomes <- rbind.data.frame(baseline, outcomes)
    outcomes$scenario <- factor(outcomes$scenario)

    selected_variables <- c("feffort", "seffort", "nbtrip", "av_trip_duration", "fishing_based_cpue_explicit",
                                       "totland_explicit",
                                        "sweptarea", "npv", "av_vpuf_month", "hoover")
    outcomes           <- outcomes[outcomes$variable %in% selected_variables,]

    outcomes$variable <- factor(outcomes$variable)
    outcomes$variable <- factor(outcomes$variable, levels=selected_variables, labels= c( "F. effort", "S. effort", "Nb. of trips", "Trip duration",  "CPUE at fishing",
                                                                                       "Tot landings",
                                                                                        "Swept Area",
                                                                                        "NPV", "VPUF", "Income inequality"))
    
    selected_scenarios <- input$sel.sce2
    nms <- names(selsce())[selsce() == input$sel.sce2]

    outcomes <- outcomes[outcomes$scenario %in% selected_scenarios,]
    #outcomes$scenario <- factor(outcomes$scenario)
    outcomes$scenario <- factor(outcomes$scenario, levels=selected_scenarios, labels=  nms)

    outcomes[outcomes$ratio_percent< -25, "ratio_percent"] <- -25
    outcomes[outcomes$ratio_percent>25, "ratio_percent"] <- 25
    p <- ggplot(outcomes[outcomes$ratio_percent>=-25 & outcomes$ratio_percent<=25,], aes(factor(variable), ratio_percent))  + geom_boxplot(outlier.shape=NA)  +
             labs(x = "Indicators", y = "% ratio over the baseline") # + ylim(-20, 20)

    p + facet_wrap( ~ scenario, ncol=2, scales="free_y")    + theme_bw() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.x =element_text(size =10),  panel.grid.major = element_line(colour = grey(0.4),linetype =3 ),
             strip.background = element_blank(),
             panel.border = element_rect(colour = "black"))  +
       geom_abline(intercept=0, slope=0, color="grey", lty=2)

}, height = function() {((length(input$sel.sce2) + 1) %/% 2 ) * 300 })




  output$populationSizePlot <- renderPlot({
    req(input$sel.pop, input$sel.sce2, input$sel.sum.szgroups)
    plot_popdyn(sces = input$sel.sce2,
                scenarios_names= names(selsce())[selsce() %in% input$sel.sce2],
                explicit_pops = input$sel.pop,
                sum_all = input$sel.sum.szgroups)
  }, height = function() {((length(input$sel.pop) + 1) %/% 2 ) * 300 })

  output$annualIndicPlot <- renderPlot({
    req(input$sel.pop, input$sel.sce2, input$sel.indic)
    plot_annualindic(sces = input$sel.sce2,
                 scenarios_names = names(selsce())[selsce() %in% input$sel.sce2],
                explicit_pops = input$sel.pop,
                indic = input$sel.indic)
  }, height = function() {length(input$sel.indic) * 150 + 150 })


  output$barplot_landis_perpop <- renderPlot({
    #warningPlot("Not implemented yet")
    barplotTotLandingsPerSce(selected_scenarios = input$sel.sce2, scenarios_names = names(selsce())[selsce()%in%input$sel.sce2],
                             selected_pops = sub("pop.", "", input$sel.pop))
  })

  output$cumulativeMaps <- renderPlotly({

  })
  
  
  output$kobe_plot <- renderPlot({
    req(input$sel.pop2, input$sel.sce3)
    kobe_plot(sces = input$sel.sce3,
                 scenarios_names = names(selsce())[selsce() %in% input$sel.sce3],
                explicit_pops = input$sel.pop2,
                relative_to_init = as.logical(input$relative_to_init))
  }, height = function() {length(input$sel.pop) * 100 + 100 })



  output$kobe_plot_loglike <- renderPlot({
    req(input$sel.sce4)
    kobe_plot_loglike(sces = input$sel.sce4,
                 scenarios_names = names(selsce())[selsce() %in% input$sel.sce3],
                 agg_level = as.character(input$agg_level),
                 nby=10,
                 remove_firsty=TRUE,
                 some_countries=as.character(input$sel_countries),
                 some_vids=as.character(input$sel_vids),
                 some_metiers=as.character(input$sel_metids)
                 )
  })


 
  
  
  
}


shinyApp(ui = ui, server = server, options = list("shiny.autoload.r" = FALSE))




