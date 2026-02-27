#' ShinyAppBBM - Wrapper para aplicações Shiny com suporte a autenticação
#'
#' Versão mockada para desenvolvimento local. Funciona exatamente como shinyApp(),
#' ignorando parâmetros de autenticação (tenant, app_id, app_secret, etc).
#'
#' @param ui Objeto de interface do usuário Shiny
#' @param server Função server do Shiny
#' @param tenant ID do tenant (ignorado na versão mockada)
#' @param app_id ID da aplicação (ignorado na versão mockada)
#' @param app_secret Secret da aplicação (ignorado na versão mockada)
#' @param resource Recursos OAuth (ignorado na versão mockada)
#' @param redirect URL de redirect (ignorado na versão mockada)
#' @param grantedUsers Lista de usuários autorizados (ignorado na versão mockada)
#' @param authEnabled Se autenticação está habilitada (ignorado na versão mockada)
#' @param port Porta para rodar a aplicação (usado se fornecido)
#' @param ... Argumentos adicionais passados para shinyApp
#'
#' @return Objeto Shiny app (retorno de shinyApp)
#' @export
ShinyAppBBM <- function(ui,
                        server,
                        tenant = NULL,
                        app_id = NULL,
                        app_secret = NULL,
                        resource = NULL,
                        redirect = NULL,
                        grantedUsers = NULL,
                        authEnabled = TRUE,
                        port = 8888,
                        log_path = NULL,
                        ...) {
  # Versão mockada: ignora autenticação e roda como shinyApp padrão
  options <- list(
    host = "0.0.0.0",
    port = port
  )
  shiny::shinyApp(ui = ui, server = server, options = options, ...)
}
