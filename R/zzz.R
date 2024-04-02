.onLoad <- function(...) {
  ggplot2::register_theme_elements(
    phylepic.grid.bar = ggplot2::element_rect(fill = "#eefaff"),
    phylepic.grid.line = ggplot2::element_line(linetype = 3),
    phylepic.grid.every = 2L,
    phylepic.grid.every.bar = NULL,
    phylepic.grid.every.line = 1L,

    element_tree = list(
      phylepic.grid.bar = ggplot2::el_def("element_rect", "rect"),
      phylepic.grid.line = ggplot2::el_def("element_line", "line"),
      phylepic.grid.every = ggplot2::el_def(c("numeric", "integer")),
      phylepic.grid.every.bar = ggplot2::el_def(c("numeric", "integer"), "phylepic.grid.every"),
      phylepic.grid.every.line = ggplot2::el_def(c("numeric", "integer"), "phylepic.grid.every")
    )
  )
}
