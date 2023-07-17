library(terra)

Forest <- terra::vect("O:/Nat_Ecoinformatics/C_Write/_User/PilPedersen_au207185/Projects/Vilhelmsborg/p25 forest/p25_offentligareal.shp") |>
  terra::project("EPSG:25832")

Circle2000 <- terra::vect("Circle_2000_Vilhelm.shp") |>
  terra::project("EPSG:25832")

Forest <- Forest |> terra::crop(Circle2000)

Ownership <- terra::vect("O:/Nat_BDR-data/Arealanalyse/RAW/Archive/ejerskab_20220609.gpkg")
Ownership$Ownership <- ifelse(Ownership$ejerforhold_dni %in% c("fond", "andet"), "Privat",
                              ifelse(Ownership$ejerforhold_dni %in% c("naturstyrelsen", "forsvaret","landbrugsstyrelsen", "kystdirektoratet"), "Statslig", "Kommunal"))

Ownership <- Ownership["Ownership"]

Forest <- Ownership |>
  terra::project("EPSG:25832") |>
  terra::crop(Circle2000) |>
  terra::aggregate("Ownership") |>
  terra::intersect(Forest)


Forest$Area_Buffer_8 <- Forest |>
  terra::buffer(-8) |>
  terra::expanse(unit = "ha") |>
  as.numeric()

Forest <- Forest[,c(1,10,11)]

Forest <- Forest[Forest$Ownership == "Kommunal",]

Forest$Area <- expanse(Forest, unit = "ha")

Forest$n_samples <- ifelse(Forest$Area < 1, 1, floor(Forest$Area))

terra::writeVector(Forest, "ForestVilhelm_2000.shp", overwrite = TRUE)


Forest_A <- Forest[Forest$Area_Buffer_8 > 0 & Forest$n_samples > 1]
Forest_B <- Forest[Forest$Area_Buffer_8 > 0 & Forest$n_samples == 1]
Forest_C <- Forest[Forest$Area_Buffer_8 == 0]


set.seed(2023)

library(dplyr)
library(spThin)

ForestPointsA <- list()

for(i in 1:nrow(Forest_A)){
  Temp <- terra::buffer(Forest_A[i,], -8)
  set.seed(i)
  TempPoints <- terra::spatSample(x = Temp, size = Temp$n_samples*10) |> terra::project("+proj=longlat +datum=WGS84") |>
    as.data.frame(geom = "XY") |>
    dplyr::rename("Latitude" = y, "Longitude" = x)
  ForestPointsA[[i]] <- spThin::thin(TempPoints, lat.col = "Latitude", long.col = "Longitude", spec.col = "p25_id", thin.par = 0.1, write.files = F, write.log.file = F, reps = 1, locs.thinned.list.return = T)[[1]] |> left_join(TempPoints) |>
    slice_sample(n = TempPoints$n_samples[1]) |>
    terra::vect(geom=c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84") |>
    terra::project("EPSG:25832")
}

ForestPointsA <- ForestPointsA  |>
  purrr::reduce(rbind)

set.seed(2023)

ForestPointsB <- terra::spatSample(x = terra::buffer(Forest_B, -8), size = 1, strata = "p25_id")

Forest_points <- rbind(ForestPointsA, ForestPointsB)

terra::writeVector(Forest_points, "Forest_points.shp", overwrite = TRUE)

Sampling_points <- terra::vect("SamplingPoints.shp")
