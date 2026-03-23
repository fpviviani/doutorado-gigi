# renv (reprodutibilidade)

Este projeto foi preparado para usar **renv** (lockfile de dependências), mas a máquina onde o OpenClaw está rodando **não tem `Rscript` instalado**, então eu não consigo gerar o `renv.lock` automaticamente daqui.

Para habilitar o renv no seu ambiente R, rode no terminal (com R instalado):

```bash
Rscript -e 'if (!requireNamespace("renv", quietly=TRUE)) install.packages("renv", repos="https://cloud.r-project.org"); renv::init(bare=TRUE); renv::snapshot(prompt=FALSE)'
```

Isso vai criar:
- `renv/`
- `renv.lock`

Depois disso, para instalar as dependências conforme o lockfile em uma máquina nova:

```bash
Rscript -e 'renv::restore(prompt=FALSE)'
```
