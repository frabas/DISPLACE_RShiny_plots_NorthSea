

setwd(file.path("D:","FBA","DISPLACE_RShiny_plots_NorthSea","data"))
lstfiles <- list.dirs(path="NorthSea", full.names = FALSE)
lstfiles <- lstfiles[lstfiles!=""]
lstfiles <- lstfiles[!grepl("jpeg", lstfiles)]

a_list <- NULL
for (i in 1: length(lstfiles))
{
   for(sim in 1:10)
   {
      dd <- read.table (file.path(getwd(),"NorthSea", lstfiles[i], paste0("popdyn_annual_indic_simu",sim,".dat")))
      a_list[[sim]] <- dd
   }
   assign( paste0("lst_annualindic_", lstfiles[i]), a_list)
   save(list= paste0("lst_annualindic_", lstfiles[i]), file=file.path(getwd(), paste0("lst_annualindic_", lstfiles[i],".RData")))


  file.copy(file.path(getwd(),"NorthSea", lstfiles[i], paste0("lst_loglike_weight_agg_",lstfiles[i],".RData")), file.path(getwd(), paste0("lst_loglike_weight_agg_",lstfiles[i],".RData")))
  file.copy(file.path(getwd(),"NorthSea", lstfiles[i], paste0("lst_popdyn_",lstfiles[i],".RData")), file.path(getwd(),  paste0("lst_popdyn_",lstfiles[i],".RData")))
}


