# =============================================================================
# BOOTSTRAP (Windows / renv / versão do R)
# =============================================================================

# Exigir versão do R (conforme renv.lock)
versao_r_necessaria <- "4.3.3"
versao_r_atual <- paste(R.version$major, R.version$minor, sep = ".")
if (versao_r_atual != versao_r_necessaria) {
  stop(paste0(
    "Versão do R incompatível. Necessário: ", versao_r_necessaria,
    " | Atual: ", versao_r_atual,
    "
Instale/abra o R ", versao_r_necessaria, " e tente novamente."
  ))
}

# Garantir repositório CRAN (Windows)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Instalar renv no user library, se necessário
if (!requireNamespace("renv", quietly = TRUE)) {
  dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
  install.packages("renv", lib = Sys.getenv("R_LIBS_USER"))
}

# Restaurar dependências conforme renv.lock (se existir)
if (file.exists("renv.lock")) {
  renv::restore(prompt = FALSE)
}

# =============================================================================
# MODELAGEM - VERSÃO SIMPLIFICADA E FUNCIONAL (ORQUESTRADOR)
# =============================================================================

source("Etapas Modelagem/00_setup.R")
source("Etapas Modelagem/01_config_dirs.R")
source("Etapas Modelagem/02_params.R")
source("Etapas Modelagem/03_background_adaptativo.R")
source("Etapas Modelagem/04_cleanup.R")
source("Etapas Modelagem/05_load_data.R")
source("Etapas Modelagem/06_processar_especie.R")
source("Etapas Modelagem/07_run_modelagem.R")
