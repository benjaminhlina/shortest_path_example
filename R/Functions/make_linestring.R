make_line <- function(lon, lat, llon, llat) {
  st_linestring(matrix(c(lon, llon, lat, llat), 2, 2))
}