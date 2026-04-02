#!/usr/bin/env Rscript

# Prepare predictors by cropping/masking both rasters to a Brazil polygon shapefile.
# Outputs are written so both rasters share the same CRS/res/extent/origin, allowing stacking.

suppressPackageStartupMessages({
  library(terra)
})

args <- commandArgs(trailingOnly = TRUE)
project_root <- if (length(args) >= 1 && nzchar(args[1])) args[1] else getwd()
setwd(project_root)

source("Etapas Modelagem/01_config_dirs.R")

bio_path <- file.path(dir_variaveis, "bio_brasil_30s.tif")
cov_path <- file.path(dir_variaveis, "cobertura_arborea_ambdata.tif")

br_shp_path <- file.path(dir_input, "BR_Pais_2024", "BR_Pais_2024.shp")

out_dir <- dir_variaveis
out_bio <- file.path(out_dir, "bio_brasil_30s_recortado_BR_Pais_2024.tif")
out_cov <- file.path(out_dir, "cobertura_arborea_ambdata_recortada_BR_Pais_2024.tif")

cat("📌 Project root: ", normalizePath(getwd(), winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("📦 Bioclim: ", bio_path, "\n", sep = "")
cat("🌳 Cobertura: ", cov_path, "\n", sep = "")
cat("🇧🇷 Shapefile BR: ", br_shp_path, "\n", sep = "")
cat("💾 Output bioclim: ", out_bio, "\n", sep = "")
cat("💾 Output cobertura: ", out_cov, "\n\n", sep = "")

if (!file.exists(bio_path)) stop("Bioclim not found: ", bio_path)
if (!file.exists(cov_path)) stop("Cobertura not found: ", cov_path)
if (!file.exists(br_shp_path)) stop("Brazil shp not found: ", br_shp_path)

bio <- rast(bio_path)
cov <- rast(cov_path)
br <- vect(br_shp_path)

# Use bioclim first layer as canonical template grid
bio_tpl <- bio[[1]]

cat("🔎 bio ext/res/crs: ", paste(as.vector(ext(bio_tpl)), collapse = ", "), " | ", paste(res(bio_tpl), collapse = ", "), " | ", as.character(crs(bio_tpl)), "\n", sep = "")
cat("🔎 cov ext/res/crs: ", paste(as.vector(ext(cov)), collapse = ", "), " | ", paste(res(cov), collapse = ", "), " | ", as.character(crs(cov)), "\n", sep = "")
cat("🔎 br crs: ", as.character(crs(br)), "\n\n", sep = "")

# Reproject BR polygon to bioclim CRS if needed
if (!same.crs(br, bio_tpl)) {
  cat("🔁 Reprojecting Brazil polygon to bioclim CRS...\n")
  br <- project(br, crs(bio_tpl))
}

# Align cobertura to bioclim grid first (so after mask they match perfectly)
if (!same.crs(cov, bio_tpl)) {
  cat("🔁 Reprojecting cobertura to bioclim CRS...\n")
  cov <- project(cov, crs(bio_tpl))
}
cat("🧩 Resampling cobertura to bioclim grid...\n")
cov_aligned <- resample(cov, bio_tpl, method = "bilinear")

# Crop+mask both by Brazil polygon
cat("✂️ Cropping+masking bioclim by Brazil polygon...\n")
bio_crop <- crop(bio, br)
bio_mask <- mask(bio_crop, br)

cat("✂️ Cropping+masking cobertura by Brazil polygon...\n")
cov_crop <- crop(cov_aligned, br)
cov_mask <- mask(cov_crop, br)

# Final sanity checks: geometry match for first layer
stopifnot(same.crs(bio_mask[[1]], cov_mask))
stopifnot(all(res(bio_mask[[1]]) == res(cov_mask)))
stopifnot(all(ext(bio_mask[[1]]) == ext(cov_mask)))
stopifnot(all(origin(bio_mask[[1]]) == origin(cov_mask)))

cat("✅ Geometry match after crop/mask (CRS/res/ext/origin)\n")

cat("💾 Writing outputs (LZW)...\n")
writeRaster(bio_mask, out_bio, overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW")))
writeRaster(cov_mask, out_cov, overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW")))

cat("✅ Done.\n")
