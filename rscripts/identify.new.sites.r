new.sites <- function(x,shrub){
  if(shrub=="Ephedra"){
site.names <- c("Panoche","Cuyama","Barstow","MojavePreserve","SheepholeValley","Tecopa","TejonRanch")
site.lat <- c(36.70000998,34.85521998,35.094051,34.698199,34.205676,35.85151504,34.875994)
site.lon <- c(-120.801116,-119.48851,-116.8349,-115.684169,-115.719676,-116.186706,-118.60246)
  
## turn sites into spatial points
sites <- data.frame(site.names,site.lon,site.lat)
sites <- SpatialPoints(sites[,2:3])
proj4string(sites) <- crs.world
  
## extract aridity of spatial points
aridity.eph.samp <- extract(global.AR, sites)
  
match.vals <- function(x) {which(abs(hist1$mids-x)==min(abs(hist1$mids-x)))} ## find the aridity values that are already used in the study
covered <- unique(sapply(aridity.eph.samp, match.vals))
test <- hist1$mids[covered]## aridity values that are currently being used
colours.eph <- match(hist1$breaks,test+100)
colours.eph <- ifelse(is.na(colours.eph),0,8)
  
#hist1 <- hist(subset(aridity.ephedra, aridity.ephedra<5000), breaks=20, col=colours.eph, main="", xlab="Aridity for Ephedra locations")
#legend(3700,100, c("unsampled","sampled"), pch=22, pt.bg=c(0,8))
  
## add other points
sites.2 <- SpatialPoints(x)
proj4string(sites.2) <- crs.world

## extract aridity of spatial points
aridity.sites.2 <- extract(global.AR, sites.2)

match.vals <- function(x) {which(abs(hist1$mids-x)==min(abs(hist1$mids-x)))} ## find the aridity values that are already used in the study
covered <- unique(sapply(aridity.sites.2, match.vals))
test <- hist1$mids[covered]## aridity values that are currently being used
colours2 <- match(hist1$breaks,test+100)
colours2 <- ifelse(is.na(colours2),0,5)
colours3 <- colours.eph + colours2

par(mar=c(4.5,4.5,.5,.5))
hist(subset(aridity.ephedra, aridity.ephedra<5000), breaks=20, col=colours3, main="", xlab="Aridity for Ephedra locations")
legend(3700,140, c("unsampled","sampled","potential"), pch=22, pt.bg=c(0,8,5))
  } else{
    site.names <- c("kelbaker1","kelbaker2","kelbaker3","kelbaker4","kelbaker5")
    site.lat <- c(35.35,35.35,35.28,35.26,35.235)
    site.lon <- c(-116.0333333,-115.9166667,-115.85,-115.7833333,-115.7166667)
    
    ## turn sites into spatial points
    sites <- data.frame(site.names,site.lon,site.lat)
    sites <- SpatialPoints(sites[,2:3])
    proj4string(sites) <- crs.world
    
    ## extract aridity of spatial points
    aridity.larrea.samp <- extract(global.AR, sites)
    
    match.vals <- function(x) {which(abs(hist2$mids-x)==min(abs(hist2$mids-x)))} ## find the aridity values that are already used in the study
    covered <- unique(sapply(aridity.larrea.samp, match.vals))
    test <- hist2$mids[covered]## aridity values that are currently being used
    colours.lar <- match(hist2$breaks,test+100)
    colours.lar <- ifelse(is.na(colours.lar),0,8)
    
    #hist3 <- hist(subset(aridity.larrea, aridity.larrea<5000), breaks=20, col=colours, main="", xlab="Aridity for Ephedra locations")
    #legend(3700,100, c("unsampled","sampled"), pch=22, pt.bg=c(0,8))
    
    ## add other points
    sites.2 <- SpatialPoints(x)
    proj4string(sites.2) <- crs.world
    
    ## extract aridity of spatial points
    aridity.sites.2 <- extract(global.AR, sites.2)
    
    match.vals <- function(x) {which(abs(hist2$mids-x)==min(abs(hist2$mids-x)))} ## find the aridity values that are already used in the study
    covered <- unique(sapply(aridity.sites.2, match.vals))
    test <- hist2$mids[covered]## aridity values that are currently being used
    colours2 <- match(hist2$breaks,test+100)
    colours2 <- ifelse(is.na(colours2),0,5)
    colours3 <- colours.lar + colours2
    
    par(mar=c(4.5,4.5,.5,.5))
    hist(subset(aridity.larrea, aridity.larrea<5000), breaks=20, col=colours3, main="", xlab="Aridity for Ephedra locations")
    legend(3700,140, c("unsampled","sampled","potential"), pch=22, pt.bg=c(0,8,5))
    
}
}
