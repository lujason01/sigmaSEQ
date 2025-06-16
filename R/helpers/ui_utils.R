#' Get theme-consistent box
#' @param ... Additional parameters to pass to box
#' @return A box with consistent theming
themed_box <- function(...) {
  theme_config <- get_config("theme")
  box(
    status = theme_config$primary,
    solidHeader = TRUE,
    ...
  )
}

#' Get theme-consistent button
#' @param ... Additional parameters to pass to actionButton
#' @return A button with consistent theming
themed_button <- function(...) {
  theme_config <- get_config("theme")
  actionButton(
    class = paste0("btn-", theme_config$primary),
    ...
  )
} 