#plot to examine
plot(rq)
plot(land, add=TRUE)



region1 <- rbind(c(0,0), c(50,0), c(50,50), c(20,20), c(0,0))
region2 <- rbind(c(50,0), c(80,0), c(100,50), c(60,40), c(80,20), c(50,0))
polys <- SpatialPolygons(list(Polygons(list(Polygon(region1)), "region1"),
                              Polygons(list(Polygon(region2)), "region2")))

r <- raster(ncol=1000, nrow=1000)
r[] <- runif(ncell(r),0,1)
extent(r) <- matrix(c(0, 0, 1000, 1000), nrow=2)

r_crop <- crop(r, extent(polys), snap="out", progress='text')
r_mask <- mask(r_crop, polys) 

plot(r_mask)
plot(polys, add=TRUE)

land<- readShapePoly('land.shp')
landmasked<- mask(r,land)




randoPts<- randomPoints(rq, 880, Dornpts)