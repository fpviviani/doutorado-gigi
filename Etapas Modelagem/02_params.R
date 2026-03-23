# -----------------------------------------------------------------------------
# 2. PARÂMETROS DE MODELAGEM
# -----------------------------------------------------------------------------

lote_tamanho <- 3
pausa_minutos <- 15
max_tentativas <- 3
n_cores <- 6

# MODO SEGURO (reduz paralelismo para evitar estouro de memória/travamentos)
# Ative com a variável de ambiente: MODELAGEM_SAFE_MODE=1
safe_mode <- tolower(Sys.getenv("MODELAGEM_SAFE_MODE", "0")) %in% c("1", "true", "yes")
if (safe_mode) {
  n_cores <- max(1, min(2, n_cores))
}
limiar_vif <- 10
n_replicacoes <- 10
test_percent <- 30

# BACKGROUND: Proporção 1:1 (Rausell-Moreno 2025)
background_min <- 200
background_max <- 10000

# MÉTODOS
metodos_modelagem <- c('maxent', 'rf', 'gam')

# ESPÉCIE DE PARTIDA
especie_partida <- "Aburria_cujubi"
