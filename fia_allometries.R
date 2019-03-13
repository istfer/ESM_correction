library(data.table)

file.in.normal = ".../ESM.rds" 

q <- as.data.table(readRDS(file.in.normal))
setnames(q, tolower(names(q)))
q[, plt_cn := as.character(plt_cn)]

# q is what the results of my FIA query look like


# all the (unique) species in FIA plots
fia_spcd <- unique(q$spcd)

agb_samp_list <- list()

outdir <- ".../allometry_files"


# fit allometries, loop over species
no_data_spp <- rep(1, length(fia_spcd))

for(i in seq_along(fia_spcd)){
  
  fspcd <- fia_spcd[i]
  pfts <- list(data.frame(spcd = fspcd))
  names(pfts) <- paste0("spp_", fspcd)
  allom.stats <- AllomAve(pfts, outdir=outdir, ngibbs=500)
  allom.fit <- try(load.allom(paste0(outdir,"/Allom.spp_", fspcd, ".6.Rdata")))
  if(class(allom.fit)=="try-error"){
    no_data_spp[i] <- 0
    next
  }
  spp_dbh <- unique((q[q$spcd == fspcd, ]$dia_end) * 2.54)
  allom.out <- allom.predict(allom.fit, dbh = spp_dbh, pft = paste0("spp_", fspcd), component = 6, use = "Bg", interval = "predict", single.tree = TRUE)
  colnames(allom.out) <- spp_dbh
  
  agb_samp_list[[as.character(fspcd)]] <- allom.out # Biomass (kg)

}

save(agb_samp_list, file = "agb_samples.Rdata")
# agb_samp_list is a list of length length(fia_spcd)
# i.e. each sublist is a species, N_samples x DBH_class (actually not a class but unique DBH values that are in the FIA database for that species)

# create ab lookup table
# it will have 250 + 3 columns, dbh (in cm), dens (in trees ha-1), spcd, 205 iterations for ab (in  Mg trees-1)

ab <- q[ ,c("dia_end","tpasum","spcd")] 
ab <- unique(ab)
ab <-ab[complete.cases(ab),]

# conversions
# inches to cm
# trees acre-1 to trees ha-1
ab_tmp <- data.frame(dbh = ab$dia_end * 2.54, dens = ab$tpasum / 0.404686, spcd = ab$spcd)


collect_list <- list()
c <- 1
for(i in seq_along(fia_spcd)){
  
  if(no_data_spp[i] == 0) next
  
  spp <- as.character(fia_spcd[i])
  samp_sub <- agb_samp_list[[spp]]
  
  spp_sub <- ab_tmp[ab_tmp$spcd == spp, ]
  
  samp_sub <- sapply(seq_len(nrow(spp_sub)), function(x){
    samp_ind <- sample(1:dim(samp_sub)[1], 250)
    ab_samps <- samp_sub[samp_ind, which(colnames(samp_sub) == spp_sub$dbh[x])]  
    res <- c(spp_sub$dbh[x], spp_sub$dens[x], spp_sub$spcd[x], ab_samps* 1e-03) # kg to Mg
    return(res)
    })
  
  collect_list[[c]] <- t(samp_sub)
  c <- c + 1
  
}

# the resulting lookup table
ab_lut <- do.call("rbind", collect_list)


