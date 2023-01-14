coords2continent = function(points, regionName="continent")
{  
  countriesSP <- getMap(resolution='low')
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  if (regionName == 'country') return(fct_explicit_na(indices$ADMIN, "Ocean"))
  # continent
  else return(fct_explicit_na(indices$REGION, "Ocean"))   # returns the continent (7 continent model + Ocean)
}