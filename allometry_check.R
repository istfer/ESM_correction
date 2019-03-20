## This code provides a validation that the way I do the allometry calculation agrees with the PalEON model estimates.
library(PEcAn.allometry)

file.in.model  = "/fs/data3/istfer/fia_esm/scripts/fading_record/HF_DBH_AB_tree_iter.RDS"
hf_rec = readRDS(file.in.model)
hf_raw <- hf_rec[hf_rec$model == "Model RW + Census" , ]
years <- 2012:1960
site_id <- 1

allom.fit <- load.allom('allometry_files/')


ablut_validation <- function(allom.fit, hf_raw, site_id = 1, years){
  
  ab_list <- pal_list <- list()
  site_sub <- hf_raw[hf_raw$site_id == site_id, ]
  
  for(y in seq_along(years)){
    
    year_sub <- site_sub[site_sub$year_id == years[y],]
    
    ab_list[[y]] <- list()
    pal_list[[y]] <- list()
    
    for(i in 1:250){
      
      sub_plot <- year_sub[year_sub$iter == i,]
      
      
      sub_plot$taxon <- as.character(sub_plot$taxon)
      sub_plot$taxon[sub_plot$taxon == "BEAL"] <- "BEAL2"
      sub_plot$taxon[sub_plot$taxon == "PRSE"] <- "PRSE2"
      sub_plot$taxon[sub_plot$taxon == "ACSA"] <- "ACSA3"     
      
      sub_spps <- unique(sub_plot$taxon)
      
      
      dbh_list <- list()
      for(si in seq_along(sub_spps)){
        dbh_list[[si]]  <- matrix((sub_plot$dbh[sub_plot$taxon== sub_spps[si]]), ncol=1)  
      }
      names(dbh_list) <- sub_spps
      
      
      pred <- allom.predict(allom.fit[sub_spps],
                            dbh = dbh_list,
                            pft = sub_spps,
                            component = 6,
                            use = "Bg",
                            interval = "prediction", 
                            single.tree = FALSE)
      
      # process preds here (otherwise reulsting objects will get very big)
      # draw 1 sample from each tree
      ngibbs   <-  dim(pred[[1]])[1]
      
      
      draws <- sapply(pred, function(x) x[sample(seq(1,ngibbs), 250),,])
      if(is(draws, "list")) draws <- do.call("cbind", draws)
      draw_Mg <- draws * 1e-03 # kg to Mg
      draw_plt <- draw_Mg / (20^2*pi) # Mg/m2
      draw_con <- draw_plt / 1e-04  # Mg/acre to  # Mg/ha
      draw_fnl <- apply(draw_con,1,sum)
      
      ab_list[[y]][[i]] <- draw_fnl
      pal_list[[y]][[i]] <- sum(sub_plot$ab)
    } # end iteration-loop
    
    print(years[y])
  }# end year-loop

  # CI for plotting
  abCI <- sapply(ab_list, function(x){
    tmp <- do.call("rbind",x) 
    res <-quantile(tmp, c(0.025, 0.5, 0.975), na.rm=TRUE)
    return(res)
  } )
  palCI <- sapply(pal_list, function(x){
    tmp <- do.call("rbind",x) 
    res <-quantile(tmp, c(0.025, 0.5, 0.975), na.rm=TRUE)
    return(res)
  } )
  
  # plot
  ab_poly <- 1:dim(abCI)[2]
  plot(abCI[1,], ylim = range(abCI), xaxt = "n",lwd = 3, xlab = "Time", ylab = "AB", 
       main = paste("HF - site", site_id), type = "n", cex.lab=1.5)
  polygon(c(ab_poly, rev(ab_poly)),c((abCI[3,]), rev(abCI[1,])),col=adjustcolor("lightgray",alpha.f=0.5),border="darkgray", lwd =2)
  lines(abCI[2,], col = "darkgray", lwd=2)

  polygon(c(ab_poly, rev(ab_poly)),c((palCI[3,]), rev(palCI[1,])),col=adjustcolor("lightblue",alpha.f=0.5),border="lightblue3", lwd =2)
  lines(palCI[2,], col = "lightblue3", lwd=2)
  legend("topright", col = c("darkgray", "lightblue3"), legend = c("Istem", "Andria"),lty=1, lwd=3)
  
  
  
} # function end

ablut_validation(allom.fit, hf_raw, site_id = 1, years)
ablut_validation(allom.fit, hf_raw, site_id = 2, years)
ablut_validation(allom.fit, hf_raw, site_id = 3, years)