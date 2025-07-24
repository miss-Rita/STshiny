withSpinner <- function(
    ui_element,
    type = getOption("spinner.type", default = 1),
    color = getOption("spinner.color", default = "#0275D8"),
    size = getOption("spinner.size", default = 1),
    color.background = getOption("spinner.color.background"),
    custom.css = getOption("spinner.custom.css", default = FALSE),
    proxy.height = getOption("spinner.proxy.height"),
    id = NULL,
    image = getOption("spinner.image"),
    image.width = getOption("spinner.image.width"),
    image.height = getOption("spinner.image.height"),
    hide.ui = getOption("spinner.hide.ui", default = TRUE),
    caption = getOption("spinner.caption")
) {
  
  if (!inherits(ui_element, "shiny.tag") && !inherits(ui_element, "shiny.tag.list")) {
    stop("`ui_element` must be a Shiny tag", call. = FALSE)
  }
  
  spinner <- buildSpinner(
    spinner_type = "output",
    ui_element = ui_element,
    type = type,
    color = color,
    size = size,
    color.background = color.background,
    custom.css = custom.css,
    proxy.height = proxy.height,
    id = id,
    image = image,
    image.width = image.width,
    image.height = image.height,
    hide.ui = hide.ui,
    caption = caption
  )
  
  htmltools::attachDependencies(spinner, getDependencies())
}
