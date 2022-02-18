ym2date <- function(x) {
  sapply(strsplit(x, "[.]"),
         function(y) {
           as.numeric(y[1]) + as.numeric(y[2]) / 12 - 1/12
         })
}

selpop <- function() {
  explicit_pop_ids <- unique(lst_popdyn_scebaselineplgnb[[ 1 ]][lst_popdyn_scebaselineplgnb[[ 1 ]]$tstep!=0,"pop"])
  names(explicit_pop_ids) <- popnames$spp [match(explicit_pop_ids, popnames$idx)]
  explicit_pop_ids[ ] <- paste0("pop.",explicit_pop_ids[ ])
 explicit_pop_ids
}

selindic <- function() {
c("F/Finit", "SSB/SSBinit", "TLand/TLandinit", "TDisc/TDiscinit", "Tac/Tacinit")
}

selsce <- function() {
  loglikefns <- dir("data", "loglike.*RData", full.names = TRUE)
  sces <- gsub("^.*lst_loglike_weight_agg_|[.]RData", "", loglikefns)
  
  
  names(sces) <- sces
  nms <- c(    
                                 scebaselineplgnb=   "FMSY + LO", 
                                 scesevereandchokeplgnb=   "FMSY + LO + shoke", 
                                 scefmsylowplgnb=  "FMSYlow + LO", 
                                 scefmsylowsevereandchokeplgnb=   "FMSYlow + LO + shoke", 
                                 scesmallerselplgnb=   "FMSY + LO + low50", 
                                 scejcodboxplgnb=   "FMSY + LO + cod box" 
                                 ) 
    
   
   
  scenarios <- names(nms[match(names(sces),names(nms))])
  names(scenarios) <- nms[match(names(sces),names(nms))] 
  scenarios
}

selvar <- function() {
  #nms <- names(lst_loglike_agg_weight_all_scebaseline[[1]])
  #nms[-1]
  c(GVA = "gradva", VPUF="vapuf", Revenue = "rev_from_av_prices", "Total landings" = "totland", "Effort" = "effort", "Number of trips" = "nbtrip")
}

selsumoverszgrp <- function(){
  dd <- c(FALSE, TRUE)
  dd
}

selquantity <- function() {
  tablefns <- dir("output", pattern = "average")
  matches <- regexpr(pattern = "[^_]*cum[^u._]+", tablefns)
  res <- unique(regmatches(tablefns, matches))
  cumul <- function(x) {sub("cum", "Cumulative ", x)}
  over <- function(x) paste(cumul(x[1]), cumul(x[2]), sep = " over ")
  res <- setNames(res, ifelse(grepl("over", res), sapply(strsplit(res, "over"), over), cumul(res) ))
  res[!grepl("over", res)]
}


warningPlot <- function(text = "No data", cex = 1.3, ...) {
  plot(1, type="n", xlab="", ylab="", axes = FALSE)
  text(1, 1, text, cex = cex, ...)
}