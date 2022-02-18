


kobe_plot <- function(sces=sces,
                        scenarios_names=names(selsce()),
                        explicit_pops= explicit_pops,
                        relative_to_init=TRUE){

 
  popnames <- read.csv("data/species.csv")

  # look at annual indics such as the TACs...
  res <- NULL
    for(sce in sces) {
       print(paste("sce ", sce))
       lst <- get(paste("lst_annualindic_", sce, sep = ''), env = .GlobalEnv)
       for(simu in 1: length(lst)) {
          print(paste("sim ", simu))
          annual_indics <- lst[[simu]]
          colnames(annual_indics)    <-  c("tstep", "stk", "multi", "multi2", "Fbar", "totland_kg", "totdisc_kg", "SSB_kg", "tac", paste0("N",0:10), paste0("F",0:10), paste0("W",0:10), paste0("M",0:10))
          
          rownames(popnames) <- popnames$Code
          annual_indics$stknames <- popnames[as.character(annual_indics$stk), "Stock"]

          annual_indics <- cbind.data.frame(annual_indics [, 1:9], stknames=annual_indics[, "stknames"])   # FOR NOW...
          res <- rbind.data.frame (res, cbind.data.frame(annual_indics, sce=sce, simu=paste("simu", simu, sep="_")))
          }
      }

 
   outcome_firsty <- res[res$tstep==8761,]
   outcome <- merge(res, outcome_firsty, by.x=c('stk', 'sce', 'simu'), by.y=c('stk', 'sce', 'simu'))
   outcome$"FFinit" <- outcome$Fbar.x/outcome$Fbar.y
   outcome$"SSBSSBinit" <- outcome$SSB_kg.x/outcome$SSB_kg.y



   #outcome$sce <- factor(outcome$sce)
   #outcome$sce <- factor(outcome$sce, levels=scenarios_names, labels=  scenarios_names)


   ## merge with reference points
   referencepoints <- read.csv("data/referencepoints.csv")
   rownames(referencepoints) <- referencepoints$stock
   outcome <- merge(outcome, referencepoints[,c("stock","FMSY", "B_trigger")], by.x="stknames.x", by.y="stock" )
  
   outcome[outcome$stknames.x=="PLE.2432","FMSY"] <-   as.numeric(as.character(outcome[outcome$stknames.x=="PLE.2432" &  outcome$tstep.x==8761,"Fbar.x"])) [ 1]  # because no FMSY for this stock
   outcome[outcome$stknames.x=="PLE.2432","B_trigger"] <-    as.numeric(as.character(outcome[outcome$stknames.x=="PLE.2432" &  outcome$tstep.x==8761,"SSB_kg.x"]))[ 1]/1e3 # because no B_trigger for this stock
   outcome[outcome$stknames.x=="COD.kat","B_trigger"] <-    as.numeric(as.character(outcome[outcome$stknames.x=="COD.kat" &  outcome$tstep.x==8761,"SSB_kg.x"]))[ 1]/1e3 # because no B_trigger for this stock
  
   outcome$FFMSY <- outcome$Fbar.x/outcome$FMSY
   outcome$SSBBtrigger <- (outcome$SSB_kg.x/1e3)/outcome$B_trigger

   # aggregrate
   agg <- aggregate(data.frame(outcome$FFinit, outcome$SSBSSBinit, outcome$FFMSY, outcome$SSBBtrigger), list(outcome$sce, outcome$tstep.x, outcome$stknames.x), mean)
   colnames(agg) <- c("Sce", "Tstep", "Stock", "FFinit", "SSBSSBinit", "FFMSY", "SSBBtrigger")


  
   ## get ggplots
   if(relative_to_init){
     a_count <- 0
     plot_list1 <- list(NULL)
     for (a_stock in explicit_pops){
        a_count <- a_count+1
        a_stkname <- as.character(popnames[popnames$Code==gsub("pop.","",a_stock), 2])

        agg_this_stock <- agg[agg$Stock==a_stkname,]


        library(ggplot2)

      plot_list1[[a_count]] <-
         ggplot(data=agg_this_stock, aes(x=FFinit, y=SSBSSBinit, group=Sce)) +
         geom_path(aes(col=Sce), size = 1.5, lineend = "round", linetype = 1, arrow = arrow(type = "closed")) + ggtitle(a_stkname)+ ylab("SSB/SSBi") + xlab("F/Fi") +  theme_minimal() + #+
           annotate("rect", xmin=-Inf, xmax=1, ymin=1, ymax=Inf, alpha = .2, fill="green") # geom_point(aes(col=Sce), size = 3)
    }
    library(ggpubr)
    gg <- ggarrange(plotlist =plot_list1,  common.legend = TRUE, legend="bottom")
    print(gg)
  }


 ## get ggplots
   if(!relative_to_init){
      a_count <- 0
      plot_list2 <- list(NULL)
      for (a_stock in explicit_pops){
         a_count <- a_count+1
         a_stkname <- as.character(popnames[popnames$Code==gsub("pop.","",a_stock), 2])


         agg_this_stock <- agg[agg$Stock==a_stkname,]


         library(ggplot2)

      plot_list2[[a_count]] <-
         ggplot(data=agg_this_stock, aes(x=FFMSY, y=SSBBtrigger, group=Sce)) +
         geom_path(aes(col=Sce), size = 1.5, lineend = "round", linetype = 1, arrow = arrow(type = "closed")) + ggtitle(a_stkname)+ ylab("SSB/SSBtrigger") + xlab("F/Fmsy")+  theme_minimal() +
          annotate("rect", xmin=-Inf, xmax=1, ymin=1, ymax=Inf, alpha = .2, fill="green") #+
          # geom_point(aes(col=Sce), size = 3)
    }
    library(ggpubr)
    gg <- ggarrange(plotlist =plot_list2,  common.legend = TRUE, legend="bottom")
    print(gg)
   }

  return()
  }
  
  
 