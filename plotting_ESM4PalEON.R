rm(list=ls())
library(data.table)


# these two datasets were prepared by fia_extract.R first run that 
file.in.normal = "/fs/data3/istfer/fia_esm/scripts/fading_record/ESM.rds"
file.in.fading = "/fs/data3/istfer/fia_esm/scripts/fading_record/ESM_F.rds"

x.lim = c(10,50) 
y.lim = c(0,2000) 

dia.bin   = 2.5
dia.lim   = c(0,50)

tpa.bin   = 100
tpa.lim   = c(0,1500)

bin.min.n = 100

# read in ESM 
q = as.data.table(readRDS(file.in.normal))
setnames(q, tolower(names(q)))
q[, plt_cn := as.character(plt_cn)]

# read in ESM fading record scenario 
w = as.data.table(readRDS(file.in.fading))
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

# --- Binning

q[, prevdiabin   := ceiling(prevdiamean.m/dia.bin)*dia.bin       ]
w[, prevdiabin   := ceiling(prevdiamean.m/dia.bin)*dia.bin       ]

q[, prevstockbin := ceiling(prevtpasum.m/tpa.bin)*tpa.bin  ]
w[, prevstockbin := ceiling(prevtpasum.m/tpa.bin)*tpa.bin  ]

q[, prevdiastockbin := paste(prevdiabin,prevstockbin,sep='_')]
w[, prevdiastockbin := paste(prevdiabin,prevstockbin,sep='_')]

w$prevdiastockbin <- q$prevdiastockbin

# --- Bin means
binmean = function(x) {
  if(length(x)<bin.min.n) {
    return(NULL)
  } else {
    return(mean(x, na.rm=T))
  }
}
meanB = q[, .(binmean(prevdiamean.m),binmean(diamean.m),
              binmean(prevtpasum.m),binmean(tpasum.m)), by=prevdiastockbin]

meanD = w[, .(binmean(prevdiamean.m),binmean(diamean.m),
              binmean(prevtpasum.m),binmean(tpasum.m)), by=prevdiastockbin]

setnames(meanB, c("bin","prevdiameanB","diameanB","prevtpasumB","tpasumB"))
setnames(meanD, c("bin","prevdiameanD","diameanD","prevtpasumD","tpasumD"))
meanB
meanD


meanB = meanB[tpasumB < 2000 ]
meanD = meanD[tpasumD < 2000 ]


# --- PLOT
plot(c((q$prevdiamean.m),(q$diamean.m)),c((q$prevtpasum.m),(q$tpasum.m)), type ="n", col="grey94",
     main="ESM normal vs fading record",ylim=y.lim, xlim=x.lim, mgp=c(2,1,0), xlab=NA, ylab=NA, axes=F)
axis(side=1, tck=-0.01, labels=NA, lwd=0.75)
axis(side=2, tck=-0.01, labels=NA, lwd=0.75)
axis(side=1, lwd=0, line= -0.7)
axis(side=2, lwd=0, line= -0.7)
mtext(side=1, "mean tree diameter (cm)", line=2)
mtext(side=2, expression(paste("stem density (trees ha"^"-1",")")), line=2)

box()


arrows(meanB$diameanB,meanB$tpasumB,meanB$prevdiameanB,meanB$prevtpasumB, col=1,
       length=0.065, angle=22,lwd=1) 

arrows(meanD$diameanD,meanD$tpasumD,meanD$prevdiameanD,meanD$prevtpasumD, col=2,
       length=0.065, angle=22,lwd=1) 

legend("topright",legend="fading record",bty="n", lty=1, col=2)


plot(c((q$prevdiamean.m),(q$diamean.m)),c((q$prevtpasum.m),(q$tpasum.m)), type ="n", col="grey94",
     main="ESM correction vectors",ylim=y.lim, xlim=x.lim, mgp=c(2,1,0), xlab=NA, ylab=NA, axes=F)
axis(side=1, tck=-0.01, labels=NA, lwd=0.75)
axis(side=2, tck=-0.01, labels=NA, lwd=0.75)
axis(side=1, lwd=0, line= -0.7)
axis(side=2, lwd=0, line= -0.7)
mtext(side=1, "mean tree diameter (cm)", line=2)
mtext(side=2, expression(paste("stem density (trees ha"^"-1",")")), line=2)

box()

arrows(meanD$prevdiameanD,meanD$prevtpasumD,meanB$prevdiameanB,meanB$prevtpasumB, col="blue",
       length=0.065, angle=22,lwd=1) 
legend("topright",legend="correction vector",bty="n", lty=1, col="blue")



