rescale_gdal <- function(x, src_min = 1000, src_max = 3300,
                         dst_min = 0, dst_max = 255) {
  # if src_min/max not provided, use range of x

  if (src_max == src_min) {
    return(matrix(dst_min, nrow = nrow(as.matrix(x)), ncol = ncol(as.matrix(x))))
  }

  # apply linear rescaling
  x_scaled <- (x - src_min) / (src_max - src_min) * (dst_max - dst_min) + dst_min
  print(x_scaled)

  dfScale = (dst_max - dst_min) /
    (src_max - src_min);
  dfOffset = -1 * src_min * dfScale + dst_min;

  message("scale ", dfScale, " offset ", dfOffset)
  x_scaled <- (x + dfOffset)*dfScale
  sprintf("%.2f", x_scaled)

  # clamp values outside range
  x_scaled[x < src_min] <- dst_min
  x_scaled[x > src_max] <- dst_max

  return(x_scaled)
}
quantities <- c(
  0, 1110, 1120, 1130, 1140, 1150,
  1210, 1220, 1310, 1320, 1410, 1420,
  1430, 1440, 2100, 2200, 2310, 2320,
  3100, 3200, 65535
)
rescale_gdal(quantities)

