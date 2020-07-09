#' @title Converts from long-lat coords to corresponding 3D
#'   coords, (x, y, z), on a sphere with radius 1
#' @description This function takes in a vector of longitude and a vector of
#' latitude and returns coordinates on the S2 sphere (globe living in
#' 3D) in (x, y, z) coords
#'
#' @param lon numeric vector of longitude coords
#' @param lat numeric vector of latitude coords
#'
#' @return 3 column numeric matrix where each row is a (x,y,z) of the
#'   transformed (long, lat) coords
#' @export
lonlat3D <- function(lon, lat) {
  cbind(
    cos((lon / 180) * pi) * cos((lat / 180) * pi),
    sin((lon / 180) * pi) * cos((lat / 180) * pi),
    sin((lat / 180) * pi)
  )
}
