# -----------------------------------------------------------------------------
# 4. LIMPEZA INICIAL
# -----------------------------------------------------------------------------

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("INICIANDO MODELAGEM\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("🧹 LIMPEZA INICIAL...\n")

if (dir.exists(dir_temp)) {
  arquivos_terra <- list.files(dir_temp, full.names = TRUE)
  if (length(arquivos_terra) > 0) {
    unlink(arquivos_terra, force = TRUE)
  }
}

gc()
cat("✅ LIMPEZA CONCLUÍDA!\n\n")
