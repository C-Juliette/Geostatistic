#' Plot a matrix
#'
#' @param M a matrix
#' @param r a radius for the window of the moving average
#' @param paletteinf extreme color of color scale
#' @param palettesup extreme color of color scale
#' @param titre a title for the matrix
#' @param nom_axeX name for X axis
#' @param nom_axeY name for Y axis
#' @param echelle title of scale
#'
#' @return a plot
#' @export
#'
#' @examples
#'
#' plot_matrix(matrix(rbinom(10*10, 1, 0.5), nrow = 10))
plot_matrix <- function(M, r= "", paletteinf = "", palettesup = "", titre = "", nom_axeX = "", nom_axeY = "", echelle = "Echelle"){
  titre <- stringr::str_c(titre, "  Var = " ,as.character(round(variance(M), 4)),  sep = "")
  M <- reshape2::melt(M) # trois colonnes : les i, les j, les valeurs
  p <- ggplot2::ggplot(data = M, ggplot2::aes(x = Var1 -0.5 , y = Var2-0.5) )
  if(r != ""){p <- ggplot2::ggplot(M, ggplot2::aes(x = Var1 -0.5+r , y = Var2 -0.5+r, z= value, fill=value))}
  p <- p +
    ggplot2::geom_tile(ggplot2::aes(fill = value))
  if(paletteinf != "" & palettesup != ""){p <- p+ggplot2::scale_fill_viridis_c(option = "B", direction = -1, limits = c(paletteinf,palettesup))}
  else{p <- p+ggplot2::scale_fill_viridis_c(option = "B", direction = -1)}
  p <- p +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(title = titre,
         x = nom_axeX,
         y = nom_axeY) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(title = echelle))+
    ggplot2::scale_y_continuous(breaks= scales::pretty_breaks())+
    ggplot2::scale_x_continuous(breaks= scales::pretty_breaks())
  return(p)
}
