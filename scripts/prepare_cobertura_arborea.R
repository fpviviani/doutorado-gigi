#!/usr/bin/env Rscript

# Prepare predictors so biobrasil (larger extent) is cropped/aligned to cobertura (smaller Brazil-only extent).
# This avoids expanding cobertura to South America and keeps both layers aligned for stacking.

suppressPackageStartupMessages({
  library(terra)
})

# Try to set working directory to project root
args <- commandArgs(trailingOnly = TRUE)
project_root <- if (length(args) >= 1 && nzchar(args[1])) args[1] else getwd()
setwd(project_root)

# Load directory config (defines dir_variaveis)
source("Etapas Modelagem/01_config_dirs.R")

# Keep paths consistent with the main pipeline (Etapa 05)
bio_path <- file.path(dir_variaveis, "bio_brasil_30s.tif")
# cobertura is expected in dir_variaveis root
cov_path <- file.path(dir_variaveis, "cobertura_arborea_ambdata.tif")
out_path <- file.path(dir_variaveis, "cobertura_arborea_ambdata_alinhada_bio_brasil_30s.tif")

# Also write a cropped/aligned bioclim (same geometry as cobertura)
out_bio_path <- file.path(dir_variaveis, "bio_brasil_30s_alinhado_cobertura_arborea.tif")

cat("📌 Project root:", normalizePath(getwd(), winslash = "/", mustWork = FALSE), "\n")
cat("📦 Bioclim:", bio_path, "\n")
cat("🌳 Cobertura:", cov_path, "\n")
cat("💾 Output cobertura alinhada:", out_path, "\n")
cat("💾 Output bioclim recortado/alinhado:", out_bio_path, "\n\n")

if (!file.exists(bio_path)) stop("Bioclim not found: ", bio_path)
if (!file.exists(cov_path)) stop("Cobertura not found: ", cov_path)

bio <- rast(bio_path)
# Use first layer for geometry checks
bio_tpl <- bio[[1]]

cov <- rast(cov_path)

cat("🔎 Bioclim ext (orig): ", paste(as.vector(ext(bio_tpl)), collapse = ", "), "\n", sep = "")
cat("🔎 Bioclim res (orig): ", paste(res(bio_tpl), collapse = ", "), "\n", sep = "")
cat("🔎 Bioclim crs (orig): ", as.character(crs(bio_tpl)), "\n", sep = "")

cat("🔎 Cobertura ext (orig): ", paste(as.vector(ext(cov)), collapse = ", "), "\n", sep = "")
cat("🔎 Cobertura res (orig): ", paste(res(cov), collapse = ", "), "\n", sep = "")
cat("🔎 Cobertura crs (orig): ", as.character(crs(cov)), "\n\n", sep = "")

# 1) Ensure CRS matches (use cobertura as reference)
if (!same.crs(cov, bio_tpl)) {
  cat("🔁 Reprojecting bioclim to cobertura CRS...\n")
  bio <- project(bio, crs(cov))
  bio_tpl <- bio[[1]]
}

# 2) Crop bioclim to cobertura extent (smaller Brazil-only extent)
cat("✂️ Cropping bioclim to cobertura extent...\n")
bio_crop <- crop(bio, cov)

# 3) Resample bioclim to cobertura grid (forces res/extent/origin alignment)
# (bioclim is continuous, so bilinear is OK)
cat("🧩 Resampling bioclim to cobertura grid...\n")
bio_aligned <- resample(bio_crop, cov, method = "bilinear")

# 4) Ensure cobertura is also aligned to itself grid (no-op) but keeps naming consistent
cov_aligned <- cov
names(cov_aligned) <- "cobertura_arborea"

# Sanity checks (geometry must match between bio_aligned[[1]] and cov_aligned)
stopifnot(same.crs(bio_aligned, cov_aligned))
stopifnot(all(res(bio_aligned) == res(cov_aligned)))
stopifnot(all(ext(bio_aligned) == ext(cov_aligned)))
stopifnot(all(origin(bio_aligned) == origin(cov_aligned)))

cat("✅ Aligned: bioclim cropped/resampled to cobertura geometry\n")

# Write outputs (compressed)
cat("💾 Writing outputs...\n")
writeRaster(cov_aligned, out_path, overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW")))
writeRaster(bio_aligned, out_bio_path, overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW")))

cat("✅ Done. Outputs written to:\n- ", out_path, "\n- ", out_bio_path, "\n", sep = "")
