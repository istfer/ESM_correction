library(PEcAn.allometry)
library(data.table)
library(plyr)
library(methods)

bety <- dplyr::src_postgres(dbname = 'bety',
                            user = 'bety',
                            password = 'bety',
                            host = 'localhost')

file.in.normal = ".../ESM.rds"
file.in.fading = ".../ESM_F.rds"

# --- read in FIA DB query
# read in ESM 
q = as.data.table(readRDS(file.in.normal))
# read in ESM fading record scenario 
w = as.data.table(readRDS(file.in.fading))

sumNA  = function(x) sum(x,na.rm=T)
meanNA = function(x) mean(x,na.rm=T)
q[, PREVTPAsum     := sumNA(start1tpa), by=PLT_CN                 ]  # "startsumTPA"
q[, TPAsum         := sumNA(end1tpa), by=PLT_CN                   ]  # "endsumTPA"
# Compute plot mean diameters
q[, PREVDIAmean    := meanNA(DIA_BEGIN), by=PLT_CN                ]  # "DIAbeginmean"
q[, DIAmean        := meanNA(DIA_END),   by=PLT_CN                ]  # "DIAendmean"

w[, PREVTPAsum     := sumNA(start1tpa), by=PLT_CN                 ]  # "startsumTPA"
w[, TPAsum         := sumNA(end1tpa), by=PLT_CN                   ]  # "endsumTPA"
# Compute plot mean diameters
w[, PREVDIAmean    := meanNA(DIA_BEGIN), by=PLT_CN                ]  # "DIAbeginmean"
w[, DIAmean        := meanNA(DIA_END),   by=PLT_CN                ]  # "DIAendmean"

setnames(q, tolower(names(q)))
q[, plt_cn := as.character(plt_cn)]


setnames(w, tolower(names(w)))
w[, plt_cn := as.character(plt_cn)]


# --- Convert units
q[, diamean.m     := diamean     * 2.54      ] # inches to cm
q[, prevdiamean.m := prevdiamean * 2.54      ]
q[, tpasum.m      := tpasum      / 0.404686  ] # trees acre-1 to trees ha-1 0.404686
q[, prevtpasum.m  := prevtpasum  / 0.404686  ]

w[, diamean.m     := diamean     * 2.54      ]
w[, prevdiamean.m := prevdiamean * 2.54      ]
w[, tpasum.m      := tpasum      / 0.404686  ]
w[, prevtpasum.m  := prevtpasum  / 0.404686  ]



# a version of PEcAn function, additionally returns symbols
match_species_id <- function(input_codes, format_name = 'custom', bety = NULL, translation_table = NULL, ...) {
  
  # Relate format names to BETY columns
  formats_dict <- c('usda' = 'Symbol',
                    'fia' = 'spcd',
                    'latin_name' = 'scientificname',
                    'custom' = 'custom')
  
  
  column <- formats_dict[format_name]
  
  if(!is.null(bety)){
    # query BETY for species, id, genus, and latin name
    translation <- dplyr::tbl(bety, 'species') %>%
      dplyr::select_('bety_species_id' = 'id', 'genus', 'species','Symbol',
                     'input_code' = column) %>%
      dplyr::collect()
    translation<- translation %>% dplyr::mutate(input_code = toupper(input_code)) #match_species_id is case-sensitive, to match species names in obs to translation, 'input_codes' needs to be upper-case since 'latin_names' in obs are upper-case
    colnames(translation) <- c('bety_species_id', 'genus', 'species','Symbol',"input_codes") #semi_join requires that the column name within the tables being matched have the same name
    translation$input_codes <- as.numeric(as.character(translation$input_codes))
    translation <- dplyr::semi_join(translation, as.data.frame(input_codes), by = "input_codes" )  #Keep rows in translation table that have the same 'latin_name' within obs
  }else{
    # can call traits::betydb_query one at a time?
    # reduce the number of calls
    translation <- data.frame(bety_species_id  = rep(NA, length(unique(input_codes))),
                              genus            = rep(NA, length(unique(input_codes))),
                              species          = rep(NA, length(unique(input_codes))),
                              symbol           = rep(NA, length(unique(input_codes))),
                              input_code = unique(input_codes),
                              stringsAsFactors = FALSE)
    for(i in 1:nrow(translation)){
      foo <- eval(parse(text =paste0("traits::betydb_query(",
                                     column, "='", translation$input_code[i], "', table = 'species', user = 'bety', pwd = 'bety')")))
      if(!is.null(foo)){
        translation$bety_species_id[i] <- foo$id
        translation$genus[i]           <- foo$genus
        translation$species[i]         <- foo$species
        translation$symbol[i]          <- foo$acceptedsymbol
      }
    }
    
  }
  
  
  
  input_table <- data.frame(input_code = input_codes, stringsAsFactors = FALSE)
  # preserving the order is important for downstream
  colnames(translation)<- c('bety_species_id', 'genus', 'species', "symbol", "input_code") #changed 'latin_name' back to 'input_codes' to enable 'left_join' since columns being matched must have same name, also changed 'id' back to 'bety_species_id' so species id can be checked in bety database  
  merge_table <- dplyr::left_join(input_table, translation)
  if(sum(is.na(merge_table$bety_species_id)) > 0){
    bad <- unique(merge_table$input_code[is.na(merge_table$bety_species_id)])
    PEcAn.logger::logger.warn(paste0("Species for the following code(s) not found : ", paste(bad, collapse = ", ")))
  }
  return(merge_table)
} # match_species_id




##################### fit allometries. needs to be done once! ##################
#remove plots with NA pfts
q <- q[!(q$plt_cn %in% unique(q$plt_cn[is.na(q$pft)])),]
fia_pfts <- unique(q$pft) 
fia_pfts <- as.character(fia_pfts)
pfts <- list()
for(fi in seq_along(fia_pfts)){
  these_spp <- unique(q$spcd[q$pft==fia_pfts[fi]])
  pfts[[fia_pfts[fi]]] <- data.frame(spcd = these_spp[])
}
names(pfts) <- fia_pfts


#outdir <- ".../allometry_files"
#allom.stats <- AllomAve(pfts, ngibbs=500, components=6, outdir=outdir)

allom.fit <- load.allom('allometry_files/')


############################ build ab_lut (loop over FIA plots) ############################ 

fia_plots <- unique(q$plt_cn) # 64628 plots

nsamples <- 250 # keep 250 samples per each plot
ab_lut  <- as.data.frame(matrix(NA, nrow = length(fia_plots), ncol= nsamples+3))
colnames(ab_lut) <- c("plt_cn","dbh", "dens", paste0("iter_",1:nsamples))

for(p in seq_along(fia_plots)){
  # develop/debug
  # p <- 5
  sub_plot <- q[q$plt_cn == fia_plots[p], ] 
  if(all(is.na(sub_plot$dia_end))) next

  sub_pfts <- unique(sub_plot$pft)
  sub_pfts <- as.character(sub_pfts)
  if(any(is.na(sub_pfts))) next
  
  dbh_list <- list()
  for(si in seq_along(sub_pfts)){
    dbh_list[[si]]  <- matrix((sub_plot$dia_end[sub_plot$pft == sub_pfts[si]])  * 2.54, ncol=1 ) # inch to cm
  }
  names(dbh_list) <- sub_pfts
  
  
  pred <- allom.predict(allom.fit[sub_pfts],
                        dbh = dbh_list,
                        pft = sub_pfts,
                        component = 6,
                        use = "Bg",
                        interval = "prediction", 
                        single.tree = FALSE)

  ngibbs   <-  dim(pred[[1]])[1]
  
  if(length(sub_pfts) == 1){
    draws <- sapply(seq_along(sub_plot$diamean.m), function(x) pred[[1]][sample(seq(1,ngibbs), nsamples),x,])
    draw_Mg <- draws * 1e-03 # kg to Mg
    draw_plt <- draw_Mg / (4*7.3152^2*pi) # Mg/m2
    draw_con <- draw_plt / 1e-04  # Mg/m2 to Mg/ha
    draw_fnl <- apply(draw_con,1,sum, na.rm = TRUE)
    
  }else{
    draws <- sapply(pred, function(x) x[sample(seq(1,ngibbs), nsamples),,])
    if(is(draws, "list")) draws <- do.call("cbind", draws)
    #if(is(draws, "list")) draws <- unlist(draws)
    draw_Mg <- draws * 1e-03 # kg to Mg
    draw_plt <- draw_Mg / (4*7.3152^2*pi) # Mg/m2
    draw_con <- draw_plt / 1e-04  # Mg/m2 to Mg/ha
    #draw_plt <- draw_Mg / 0.166166884 # Mg/acre (1 FIA plot = 0.166166884 acre)
    #draw_con <- draw_plt / 0.404686  # Mg/acre to  # Mg/ha
    draw_fnl <- apply(draw_con,1,sum, na.rm = TRUE)
  }
  
  
  ab_lut[p, 1]     <- sub_plot$plt_cn[1]    # plot no
  ab_lut[p, 2]     <- sub_plot$diamean.m[1] # mean dbh (cm)
  ab_lut[p, 3]     <- sub_plot$tpasum.m[1]
  ab_lut[p, 4:ncol(ab_lut)] <- draw_fnl
  
  if(p %% 100 == 0) print(p)
}

#some plots had no dbh data
ab_nf_pft_present <- ab_lut[complete.cases(ab_lut),]
save(ab_nf_pft_present, file="ab_nf_pft_present.Rdata")


