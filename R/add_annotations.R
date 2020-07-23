   ### add text with Q-value, dfs, p-value, and I^2 statistic
   text(-5.1, min(rows1$trows)-2, pos=3, cex=0.75, bquote(paste("RE Model for All Studies (Q = ",
                                                                .(formatC(PM_mv$QE, digits=2, format="f")), ", df = ", .(PM_mv$k - PM_mv$p),
                                                                ", p = ", .(formatC(PM_mv$QEp, digits=3, format="f")), ") ")))


### add column headings to the plot
text(c(-2.7, -2.35,-2.1,-1.8,-1.4)+1, 156,cex = 1, 
     c("Location", "Year", "Row\nSpacing","Host\nCultivar","DMI dose"),
     font = 2)

### Add subtitles for each SpraySchedule breakdown
text(-1, rows1$stitle_row ,cex = 1.3, 
     paste0(c("control","Early", "Late", "Recommended", "RecommededPlus", "LatePlus"),
            " Spray Schedule - (tau^2 = " ,round(PM_mv$tau2, digits = 3), ") "),
     font = 2)

addpoly(x = polys$x,
        ci.lb = polys$ci.lb,
        ci.ub = polys$ci.ub,
        col ="black",
        rows = polys$rows,
        cex = 1.3,
        efac = 0.2,
        font = "Arial Rounded MT Bold")
abline(h = rows1$stitle_row +1)

### Early lines
lines(c(-2.9,-1.25)+1,c(153,153), col = "grey70")
text(x = -2.7, y = 152.5, 
     labels = "Missen Flats" ,cex=1)
lines(c(-2.9,-1.25)+1,c(147,147), col = "grey70")
text(x = -2.7, y = 146.5, 
     labels = "Marys Mount" ,cex=1)
lines(c(-2.9,-1.25)+1,c(146,146), col = "grey70")
text(x = -2.7, y = 145.5, 
     labels = "Hermitage" ,cex=1)
lines(c(-2.9,-1.25)+1,c(140,140), col = "grey70")

### Late lines
lines(c(-2.9,-1.25),c(135,135), col = "grey70")
text(x = -2.7, y = 134.5, 
     labels = "Missen Flats" ,cex=1)
lines(c(-2.9,-1.25),c(129,129), col = "grey70")
text(x = -2.7, y = 128.5, 
     labels = "Kingaroy" ,cex=1)
lines(c(-2.9,-1.25),c(128,128), col = "grey70")
text(x = -2.7, y = 127.5, 
     labels = "Hermitage" ,cex=1)
lines(c(-2.9,-1.25),c(118,118), col = "grey70")

### Recommended lines
lines(c(-2.9,-1.25),c(113,113), col = "grey70")
text(x = -2.7, y = 112.5, 
     labels = "Premer" ,cex=1)
lines(c(-2.9,-1.25),c(112,112), col = "grey70")
text(x = -2.7, y = 111.5, 
     labels = "Missen Flats" ,cex=1)
lines(c(-2.9,-1.25),c(106,106), col = "grey70")
text(x = -2.7, y = 105.5, 
     labels = "Millmerran" ,cex=1)
lines(c(-2.9,-1.25),c(105,105), col = "grey70")
text(x = -2.7, y = 104.5, 
     labels = "Kingaroy" ,cex=1)
lines(c(-2.9,-1.25),c( 99, 99), col = "grey70")
text(x = -2.7, y = 98.5, 
     labels = "Hermitage" ,cex=1)
lines(c(-2.9,-1.25),c( 87, 87), col = "grey70")
text(x = -2.7, y = 86.5, 
     labels = "Goolhi" ,cex=1)
lines(c(-2.9,-1.25),c( 86, 86), col = "grey70")

### Recommended_Plus Lines
lines(c(-2.9,-1.25),c( 81, 81), col = "grey70")
text(x = -2.7, y = 80.5, 
     labels = "Premer" ,cex=1)
lines(c(-2.9,-1.25),c( 79, 79), col = "grey70")
text(x = -2.7, y = 78.5, 
     labels = "Missen Flats" ,cex=1)
lines(c(-2.9,-1.25),c( 67, 67), col = "grey70")
text(x = -2.7, y = 66.5, 
     labels = "Millmerran" ,cex=1)
lines(c(-2.9,-1.25),c( 65, 65), col = "grey70")
text(x = -2.7, y = 64.5, 
     labels = "Kingaroy" ,cex=1)
lines(c(-2.9,-1.25),c( 59, 59), col = "grey70")
text(x = -2.7, y = 58.5, 
     labels = "Hermitage" ,cex=1)
lines(c(-2.9,-1.25),c( 43, 43), col = "grey70")
text(x = -2.7, y = 42.5, 
     labels = "Goolhi" ,cex=1)
lines(c(-2.9,-1.25),c( 41, 41), col = "grey70")

### Late_Plus Lines
lines(c(-2.9,-1.25),c( 36, 36), col = "grey70")
text(x = -2.7, y = 35.5, 
     labels = "Wellcamp" ,cex=1)
lines(c(-2.9,-1.25),c( 24, 24), col = "grey70")
text(x = -2.7, y = 23.5, 
     labels = "Kingaroy" ,cex=1)
lines(c(-2.9,-1.25),c( 21, 21), col = "grey70")
text(x = -2.7, y = 20.5, 
     labels = "Hermitage" ,cex=1)
lines(c(-2.9,-1.25),c( 17, 17), col = "grey70")