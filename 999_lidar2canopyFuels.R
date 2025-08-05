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

d <- file.path(dirname(f), "outdir")
if(dir.exists(d)){
  answer <- readline(prompt = "Do you want to overwrite contents of ? (Y/N): ")
  if (tolower(answer) == "Y") {
    message("Continuing...")
  }
} else {
  suppressWarnings(dir.create(d))
}

outdir <- tools::file_path_sans_ext(f)
ofile = paste0("dataset_merged.laz")

### check ground points ---------
fg <- f
hasGround <- function(f){
  pipe <- reader(filter=keep_class(33))  + write_las(ofile = "tmp.laz")
  ans = exec(pipe, on = f)
  suppressWarnings(file.remove("tmp.laz"))
}
### create ground points ---------
if(!hasGround){
  pipeline = classify_with_csf(TRUE, 1 ,1, time_step = 1) + write_las()
  fg = exec(pipeline, on = f, progress = TRUE)
}
### FINISH check  ground class has points

del = triangulate(filter = keep_ground())
dtm = rasterize(1, del, ofile = f)
pipeline = del + dtm
ans = exec(pipeline, on = fg)
plotShade(ans)
