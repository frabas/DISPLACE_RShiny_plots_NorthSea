kobe_plot_loglike <- function(sces=sces,
                        scenarios_names=names(selsce()),
                        agg_level="Metier",
                        nby=5,
                        a_comment="",
                        remove_firsty=TRUE,
                        some_countries=c("den"),
                        some_vids="DNK000012527",
                        some_metiers=10
                        )
{

   sce1 <- "scebaselineplgnb" # init

   do_arrow_plot <- function(
                  nby=5,
                  a_set_of_scenarios=sces,
                  scenarios_names= scenarios_names,
                  selected=selected,
                  export=TRUE,
                  add_legend=FALSE,
                  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5),
                              rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)) ,
                  remove_firsty=FALSE,                  
                  some_countries=c("den","deu","est"),
                  some_vids="DNK000012527",
                  some_metiers=10
                  ) {


   obj <- list()
   for(sce in a_set_of_scenarios){ 
      obj[[sce]]        <- get(paste("lst_loglike_agg_weight", selected, sce, sep=''), env=.GlobalEnv)
   }


  if(some_countries!=""){
  # nothing to do, cause the entire obj is a country specific
  }

  if(some_vids!=""){
     for(sce in a_set_of_scenarios){ 
      obj[[sce]] <- lapply(obj[[sce]], function(x, some_vids) x[x$VE_REF%in%some_vids,], some_vids=some_vids)
       }
  }     

   if(some_metiers!=""){
     for(sce in a_set_of_scenarios){ 
      obj[[sce]] <- lapply( obj[[sce]], function(x, some_metiers) x[x$dominant_metier%in%some_metiers,], some_metiers=some_metiers)
     }
  }


  if(some_vids=="" && some_metiers==""){
    # caution: complete missing records if 0s in given year.month
    complete_all_year_month <- function (x, years_span=2019:2030){
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
     for(sce in a_set_of_scenarios)
      {
       obj[[sce]] <- lapply( obj[[sce]], complete_all_year_month)
      }
  }


   simu_names <-  names(obj[[a_set_of_scenarios[1] ]])
   for(sce in a_set_of_scenarios[-1])
      {
       simu_names <- simu_names[simu_names %in% names(obj[[sce]])]
      }

   res <- NULL
    for (sim in simu_names){
       for(sce in a_set_of_scenarios){ 
         # sce
         obj[[sce]][[sim]]$year <- sapply(strsplit(obj[[sce]][[sim]]$year.month, "\\."), function(x)x[1])
         annual_gradva <- tapply(obj[[sce]][[sim]][,c("gradva")], list(obj[[sce]][[sim]]$year), mean)
         annual_gradva <- tapply(obj[[sce]][[sim]][,c("gradva")], list(obj[[sce]][[sim]]$year), mean)
         annual_vapuf <- tapply(obj[[sce]][[sim]][,c("vapuf")], list(obj[[sce]][[sim]]$year), mean)
         res <- rbind.data.frame(res, cbind.data.frame (year=names(annual_gradva), gradva=annual_gradva, vapuf=annual_vapuf, sim=sim, sce=sce))
        }
    }
        
    outcome <- merge(res, res[res$year=="2020",], by.x=c( "sim", "sce"), by.y=c( "sim", "sce"))

    #outcome$sce <- factor(outcome$sce, levels=sces, labels=  scenarios_names)

    outcome$"gvagvainit" <- outcome$gradva.x/outcome$gradva.y
    outcome$"vapufvapufinit" <- outcome$vapuf.x/outcome$vapuf.y

     # aggregrate
   agg <- aggregate(data.frame(outcome$gvagvainit, outcome$vapufvapufinit), list(outcome$sce, outcome$year.x), mean)
   colnames(agg) <- c("Sce", "Year", "GVAoGVAinit", "VPUFoVPUFinit")


   ## get ggplots
   library(ggplot2)

    p <-  ggplot(data=agg[agg$Year%in% c(2020:2029),], aes(x=GVAoGVAinit, y=VPUFoVPUFinit, group=Sce)) +
         geom_path(aes(col=Sce), size = 1.5, lineend = "round", linetype = 1, arrow = arrow(type = "closed")) + ggtitle("")+ ylab("VPUF/VPUFi") + xlab("GVA/GVAi") +  theme_minimal() +
         geom_point(x=1, y=1, shape=3, size=4) +  annotate("rect", xmin=1, xmax=Inf, ymin=1, ymax=Inf, alpha = .2, fill="green") #+
          # geom_point(aes(col=Sce), size = 3)

    return(p)
    }



   cat(paste("plot for select_set_1...\n"))
   
   
   

   if(agg_level=="All"){
      plot_list1 <- list(NULL)
      plot_list1[[1]]<- do_arrow_plot(
                  nby=nby,
                  a_set_of_scenarios=sces,
                  scenarios_names= scenarios_names,
                  selected="_selected_set1_",
                  export=FALSE,
                  add_legend=TRUE,
                 # color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2))
                 color_legend= c(rgb(33/255,102/255,172/255,0.6), rgb (77/255,146/255,33/255,0.6), rgb(254/255,224/255,139/255,0.6), rgb(178/255,24/255,43/255,0.6), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
                  remove_firsty=remove_firsty,
                   some_countries="",
                   some_vids="",
                   some_metiers=""
                  )


    library(ggpubr)
    print(ggarrange(plotlist =plot_list1, ncol=1, common.legend = FALSE, legend="bottom"))
    }
    
    

  ## PER COUNTRY
  if(agg_level=="Country"){
    plot_list1 <- list(NULL)
    for (ctry in some_countries){
     cat(paste("plot for ctry...", ctry,"\n"))

     plot_list1[[ctry]]<-  do_arrow_plot(
                  nby=nby,
                  a_set_of_scenarios=sces,
                  scenarios_names= scenarios_names,
                  selected=paste0("_",ctry,"_"),
                  export=FALSE,
                  add_legend=TRUE,
                 # color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2))
                 color_legend= c(rgb(33/255,102/255,172/255,0.6), rgb (77/255,146/255,33/255,0.6), rgb(254/255,224/255,139/255,0.6), rgb(178/255,24/255,43/255,0.6), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
                  remove_firsty=remove_firsty,
                  some_countries=ctry,
                  some_vids="",
                  some_metiers=""
                  )

    } # end ctry
    library(ggpubr)
    print(ggarrange(plotlist =plot_list1,  common.legend = TRUE, legend="bottom"))
  }
  
  

  ## PER VID
  if(agg_level=="Vessel"){
   plot_list1 <- list(NULL)
   for (a_vid in some_vids){
    cat(paste("plot for vid...", a_vid,"\n"))

   plot_list1[[a_vid]] <-  do_arrow_plot(
                  nby=nby,
                  a_set_of_scenarios=sces,
                  scenarios_names= scenarios_names,
                  selected=paste0("_vid_"),
                  export=FALSE,
                  add_legend=TRUE,
                 # color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2))
                 color_legend= c(rgb(33/255,102/255,172/255,0.6), rgb (77/255,146/255,33/255,0.6), rgb(254/255,224/255,139/255,0.6), rgb(178/255,24/255,43/255,0.6), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
                  remove_firsty=FALSE,
                  some_countries="",
                  some_vids=a_vid,
                  some_metiers=""
                  )
       } # end a_vid
    library(ggpubr)
    print(ggarrange(plotlist =plot_list1,  common.legend = TRUE, legend="bottom"))
   }
   
   

 ## PER METIER
  if(agg_level=="Metier"){
    plot_list1 <- list(NULL)
    a_count <- 0
   for (a_met in some_metiers){
    cat(paste("plot for met...", a_met,"\n"))
    a_count <- a_count+1
    plot_list1[[a_count]]  <- do_arrow_plot(
                  nby=nby,
                  a_set_of_scenarios=sces,
                  scenarios_names= scenarios_names,
                  selected=paste0("_met_"),
                  export=FALSE,
                  add_legend=TRUE,
                 # color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2))
                 color_legend= c(rgb(33/255,102/255,172/255,0.6), rgb (77/255,146/255,33/255,0.6), rgb(254/255,224/255,139/255,0.6), rgb(178/255,24/255,43/255,0.6), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
                  remove_firsty=TRUE,
                  some_countries="",
                  some_vids="",
                  some_metiers=a_met
                  )
        } # end a_met
    library(ggpubr)
    print(ggarrange(plotlist =plot_list1,  common.legend = TRUE, legend="bottom"))
  }


return()
}



