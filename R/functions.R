
 
#' Function 1
#' rm_oceanpts: removes points that occur in the ocean.
#' x = Dataframe of presence data.
#' g = Name of apheanogaster species to manipulate.
#' Ex rm_oceanpts(Data, "rudis") 

rm_oceanpts <- function(df, species_name=""){
  species <- filter(df, Data$ant.species==species_name) %>%
    unique() %>% select(-ant.species)
  coordinates(species) <- ~longitude+latitude
  crs(species) <- crs(wrld_simpl)
  ovr <- over(species, wrld_simpl)
  ocean_points <- which(is.na(ovr$NAME))
  species <- as.data.frame(species)
  species <- filter(species[-(ocean_points),])
  return(species)
}