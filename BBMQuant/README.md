# BBMQuant (versão mockada)

Pacote mockado para desenvolvimento local da aplicação SAT. Em produção, o pacote real abstrai autenticação e outras funcionalidades das aplicações Shiny.

## ShinyAppBBM

A função `ShinyAppBBM` nesta versão funciona exatamente como:

```r
shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = 8888))
```

Todos os parâmetros de autenticação (tenant, app_id, app_secret, resource, redirect, grantedUsers, authEnabled) são ignorados.

## Instalação local

```r
install.packages("/caminho/para/SAT-INPE-APP/BBMQuant", repos = NULL, type = "source")
```
