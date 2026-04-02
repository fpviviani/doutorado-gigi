#!/usr/bin/env Rscript

# Align cobertura_arborea.tif to the bioclim grid (CRS/res/extent/origin)
# and save a new file so the main pipeline doesn't need to redo this every run.

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
out_path <- file.path(dir_variaveis, "cobertura_arborea_ambdata_alinhada_bioclim.tif")

cat("📌 Project root:", normalizePath(getwd(), winslash = "/", mustWork = FALSE), "\n")
cat("📦 Bioclim:", bio_path, "\n")
cat("🌳 Cobertura:", cov_path, "\n")
cat("💾 Output:", out_path, "\n\n")

if (!file.exists(bio_path)) stop("Bioclim not found: ", bio_path)
if (!file.exists(cov_path)) stop("Cobertura not found: ", cov_path)

bio <- rast(bio_path)
# Use the first layer as template for geometry
bio_tpl <- bio[[1]]

cat("🔎 Bioclim ext: ", paste(as.vector(ext(bio_tpl)), collapse = ", "), "\n", sep = "")
cat("🔎 Bioclim res: ", paste(res(bio_tpl), collapse = ", "), "\n", sep = "")
cat("🔎 Bioclim crs: ", as.character(crs(bio_tpl)), "\n", sep = "")

cov <- rast(cov_path)
cat("🔎 Cobertura ext (orig): ", paste(as.vector(ext(cov)), collapse = ", "), "\n", sep = "")
cat("🔎 Cobertura res (orig): ", paste(res(cov), collapse = ", "), "\n", sep = "")
cat("🔎 Cobertura crs (orig): ", as.character(crs(cov)), "\n\n", sep = "")

# 1) Ensure CRS matches
if (!same.crs(bio_tpl, cov)) {
  cat("🔁 Reprojecting cobertura to bioclim CRS...\n")
  cov <- project(cov, crs(bio_tpl))
}

# 2) Resample to bioclim geometry (forces res/extent/origin alignment)
# cobertura is continuous (%), so bilinear interpolation is appropriate
cat("🧩 Resampling cobertura to bioclim grid...\n")
cov_aligned <- resample(cov, bio_tpl, method = "bilinear")

# 3) Crop to bioclim extent (usually redundant, but explicit)
cov_aligned <- crop(cov_aligned, bio_tpl)

names(cov_aligned) <- "cobertura_arborea"

# Sanity checks
stopifnot(same.crs(bio_tpl, cov_aligned))
stopifnot(all(res(bio_tpl) == res(cov_aligned)))
stopifnot(all(ext(bio_tpl) == ext(cov_aligned)))
stopifnot(all(origin(bio_tpl) == origin(cov_aligned)))

cat("✅ Aligned: CRS/res/ext/origin match bioclim\n")

# Write output (compressed)
cat("💾 Writing output...\n")
writeRaster(cov_aligned, out_path, overwrite = TRUE, wopt = list(gdal = c("COMPRESS=LZW")))

cat("✅ Done. Output written to:\n", out_path, "\n", sep = "")
