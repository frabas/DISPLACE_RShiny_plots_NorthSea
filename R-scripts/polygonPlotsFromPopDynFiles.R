plot_popdyn <- function(sces=sces,
                        scenarios_names=names(selsce()),
                        explicit_pops= explicit_pops,
                        sum_all=FALSE){

   pops <- sapply( strsplit(as.character(explicit_pops), split = "\\."), function(x) x[2])
   namesimu <- lst_popdyn <- vector("list", length(sces))

   for (i in seq(sces)) {
      res <- get(paste("lst_popdyn_", sces[i], sep = ''), env = .GlobalEnv)
      ## filter lst_popdyn_ to trash away the failed (i.e. non-complete) simus:
      ## detection according to the number of rows...
      dd                <- table(unlist(lapply(res, nrow)))
      expected_nb_rows  <- as.numeric(names(dd[dd == max(dd)]))[1] # most common number of rows
      idx               <- unlist(lapply(res, function(x) nrow(x) == expected_nb_rows))
      namesimu[[i]]     <- c(names(unlist(lapply(res, function(x) nrow(x) == expected_nb_rows)))[idx])
      lst_popdyn[[i]]   <- res[namesimu[[i]]]
   }

   # sum over sizegroup?
   if (sum_all) {
      lst_popdyn <- lapply(lst_popdyn, function(lp) {
         lapply(lp, function(x) {
            x[,3] <- apply(x[, -c(1:2)], 1, sum, na.rm = TRUE) ; colnames(x)[3] <- "totN" ; x[,1:3]
         })
      })
   }

   # 1. nb of induviduals per (explicit) pop in each size bin
   # from a bunch of simus
   npop <- length(pops)
   if (npop > 1) {
      layout(matrix(seq(npop + npop %% 2), ncol = 2))
      par(cex = 1 + ((npop - 1) %/% 2) / 40)
   }
   for (pop in pops) {  # for each (explicit) pop
      cat (paste("plot Population size for pop ", pop, "\n"))
  
      simu1 <- lst_popdyn[[1]][[1]] 
      if(any(pop %in% simu1[simu1$tstep!=0,"pop"]))
      {
      xs <- as.POSIXct("2019-1-1") + unique(simu1$tstep) * 3600
      this_pop <- simu1[simu1$pop == pop, -c(1:2)]
      this_pop <- replace(this_pop, is.na(this_pop), 0)
      if (any(this_pop < 0)) cat(paste("negative numbers! check this pop ", pop, "\n"))
   
      a.unit <- 1e3  # if divide N by 1e3 then converting in millions because already in thosuand
      a.xlab <- "month"
      a.ylab <- "million individuals"

      if(sum_all)  y <- this_pop else y <- this_pop[[1]]
       plot(xs, y, type = 'n', axes = TRUE, las = 1,
           ylim = c(min(this_pop, na.rm = TRUE) / a.unit, (max(this_pop, na.rm = TRUE) / a.unit) * 1.1),
           ylab = "", xlab = "")
      title(popnames$spp[popnames$idx == pop])
      title(ylab = a.ylab, line = 3)

      # axis(1, labels = 1:nrow(simu1[simu1$pop==pop,]),
      #      at = 1:nrow(simu1[simu1$pop==pop,]))
      graphics::box()

      a.count <- 0
      for (seg in colnames( simu1 )[-c(1:2)] ){  # for each col
         cat(paste(seg, "\n"))
         a.count <- a.count + 1
         mat.sim <- mapply(function(lp, ns) {
            res <- matrix(unlist(lapply(lp[ns], function(x) {
               res <- try(x[x$pop == pop,seg], silent = TRUE)
               if (class(res) == "try-error") res <- rep(NA, ncol(lp[[1]]))
               res
            })), nrow = nrow(lp[[1]][lp[[1]]$pop == pop,]), byrow = FALSE)
            colnames(res) <- c(paste(seg,"_", ns, sep = ''))
            replace(res, is.na(res), 0)
            res
         }, lst_popdyn, namesimu, SIMPLIFY = FALSE)

         cols <- list(rgb(colorRamp(c("#d95f02", "white"))(seq(0, 1, length = 14)), max = 255, alpha = 100),
                      rgb(colorRamp(c("#7570b3", "white"))(seq(0, 1, length = 14)), max = 255, alpha = 100),
                      rgb(colorRamp(c("#1b9e77", "white"))(seq(0, 1, length = 14)), max = 255, alpha = 100),
                      rgb(colorRamp(c("#E5D8BD", "white"))(seq(0, 1, length = 14)), max = 255, alpha = 100),
                      rgb(colorRamp(c("#A6D854", "white"))(seq(0, 1, length = 14)), max = 255, alpha = 100),
                      rgb(colorRamp(c("#CCEBC5", "white"))(seq(0, 1, length = 14)), max = 255, alpha = 100),
                      rgb(colorRamp(c("#32a852", "white"))(seq(0, 1, length = 14)), max = 255, alpha = 100),
                      rgb(colorRamp(c("#264142", "white"))(seq(0, 1, length = 14)), max = 255, alpha = 100),
                      rgb(colorRamp(c("#9ca37e", "white"))(seq(0, 1, length = 14)), max = 255, alpha = 100),
                      rgb(colorRamp(c("#a867bf", "white"))(seq(0, 1, length = 14)), max = 255, alpha = 100),
                      rgb(colorRamp(c("#2052e8", "white"))(seq(0, 1, length = 14)), max = 255, alpha = 100),
                      rgb(colorRamp(c("#2052e8", "white"))(seq(0, 1, length = 14)), max = 255, alpha = 100))


         # polygon 5-95% for simus
         for (i in seq(mat.sim)) {
            polygon(c(xs, rev(xs)),
                    c(apply(mat.sim[[i]], 1, quantile, 0.05) / a.unit,
                      rev(apply(mat.sim[[i]], 1, quantile, 0.95) /  a.unit)) ,
                    col = cols[[i]][a.count],
                    border = cols[[i]][a.count])
         }
         abline(h = 0, lty = 3)



      }   # end for seg

    } # end if
   } # end for pop

   legend("topleft", box.lty = 0, legend = scenarios_names, fill = sapply(cols, function(x) x[1]))
   ## fill = c(rgb( ramp1(seq(0, 1, length = 14)), max = 255, alpha=100)[1], rgb( ramp2(seq(0, 1, length = 14)), max = 255)[1]))
   graphics::box()

   return()
}


