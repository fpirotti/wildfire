if(!require("lasR")) install.packages('lasR', repos = 'https://r-lidar.r-universe.dev')
library(lasR)
library()

plotShade <- function(dtm){
  hillshade <- shade(terrain(dtm, "slope", unit="radians"),
                     terrain(dtm, "aspect", unit="radians"),
                     angle = 45, direction = 315)
  dtm_cols <- terrain.colors(100)
  dtm_cols_alpha <- adjustcolor(dtm_cols, alpha.f = 0.5)  # alpha.f from 0 (transparent) to 1 (opaque)

  plot(hillshade, col = gray.colors(100), main = "DTM with Hillshade", legend=F)
  plot(dtm, col = dtm_cols_alpha, add = TRUE )
}
set_parallel_strategy(sequential())
# step 1 - create a decent CHM -----------
f <- "~/Downloads/lignano.laz"
ofile = paste0("dataset_merged.laz")

### check ground points ---------
hasGround <- function(f){
  pipe <- reader(filter=keep_class(33))  + write_las(ofile = "tmp.laz")
  ans = exec(pipe, on = f)
  suppressWarnings(file.remove("tmp.laz"))
}
### create ground points ---------
if(!hasGround){

}
### FINISH check  ground class has points

del = triangulate(filter = keep_ground())
dtm = rasterize(1, del)
pipeline = del + dtm
ans = exec(pipeline, on = f)
plotShade(ans)
