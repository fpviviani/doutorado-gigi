# -----------------------------------------------------------------------------
# 5. CARREGAR DADOS
# -----------------------------------------------------------------------------

cat("📦 Carregando variáveis climáticas...\n")
bioclimaticas <- rast(file.path(dir_variaveis, "wc2.1_country/BRA_wc2.1_30s_bio.tif"))

# Listar espécies
arquivos_ocorrencias <- list.files(dir_ocorrencias, pattern = "\\.csv$", full.names = TRUE)
nomes_arquivos <- basename(arquivos_ocorrencias)

extrair_nome_especie <- function(nome_arquivo) {
  nome <- gsub("_rarefeito\\.csv$|_bruto\\.csv$|\\.csv$", "", nome_arquivo)
  return(nome)
}

especies_df <- data.frame(
  arquivo = arquivos_ocorrencias,
  nome_arquivo = nomes_arquivos,
  especie = sapply(nomes_arquivos, extrair_nome_especie),
  stringsAsFactors = FALSE
)

especies_df <- especies_df[order(especies_df$especie), ]

# Identificar espécies já processadas
arquivos_processados <- list.files(dir_modelagem, pattern = "_ensemble\\.tif$", full.names = FALSE)
especies_processadas <- gsub("_ensemble\\.tif$", "", arquivos_processados)

cat("\n📊 Total de espécies:", nrow(especies_df), "\n")
cat("📊 Já processadas:", length(especies_processadas), "\n")

# Seleção de espécies (loop vs single)
modo_execucao <- tolower(trimws(as.character(modo_execucao)))
if (is.na(modo_execucao) || modo_execucao == "") modo_execucao <- "loop"

if (modo_execucao == "single") {
  if (is.null(especie_unica) || trimws(as.character(especie_unica)) == "") {
    stop("modo_execucao='single' requer que 'especie_unica' seja definido em 02_params.R")
  }

  especie_unica <- trimws(as.character(especie_unica))
  especies_pendentes <- especies_df[especies_df$especie == especie_unica, ]

  if (nrow(especies_pendentes) == 0) {
    stop(paste0(
      "Espécie única não encontrada na pasta de ocorrências: '", especie_unica, "'. ",
      "Confira o nome do arquivo (sem .csv / sem sufixos)."
    ))
  }

  # No modo single, ainda respeitamos o filtro de já processadas
  especies_pendentes <- especies_pendentes[!especies_pendentes$especie %in% especies_processadas, ]

  cat("🎯 Modo de execução: SINGLE\n")
  cat("🎯 Espécie única:", especie_unica, "\n")
} else {
  # Encontrar posição da espécie de partida
  posicao_partida <- 1
  if (!is.null(especie_partida) && especie_partida != "") {
    posicao_partida <- which(especies_df$especie == especie_partida)
    if (length(posicao_partida) == 0) posicao_partida <- 1
  }

  # Espécies pendentes
  especies_pendentes <- especies_df[posicao_partida:nrow(especies_df), ]
  especies_pendentes <- especies_pendentes[!especies_pendentes$especie %in% especies_processadas, ]

  cat("🎯 Modo de execução: LOOP\n")
  cat("🎯 Espécie de partida:", especie_partida, "\n")
}

cat("📊 Espécies pendentes:", nrow(especies_pendentes), "\n")
