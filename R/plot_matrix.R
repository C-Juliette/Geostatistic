library(ggplot2)
str_c <- stringr::str_c
#ggplot <- ggplot2::ggplot
#aes <- ggplot2::aes
#geom_tile <- ggplot2::geom_tile
#scale_fill_viridis_c <- ggplot2::scale_fill_viridis_c
#labs <- ggplot2::labs
#guides <- ggplot2::guides

#' Plot a matrix
#'
#' @param M a matrix/vector/number/dataframe of numbers
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
  if (is.data.frame(M)){M <- as.matrix(M)}
  if (!is.numeric(M)){stop("M must contain numbers")}

  if(titre == ""){titre <- str_c("Var = " ,as.character(round(variance(M), 4)),  sep = "")}
  else{titre <- str_c(titre, "  -  Var = " ,as.character(round(variance(M), 4)),  sep = "")}
  M <- M |> long_shaped_matrix() # trois colonnes : les i, les j, les valeurs
  p <- ggplot(data = M, aes(x = Var1 -0.5, y = Var2 - 0.5) )
  if(r != ""){p <- ggplot(M, aes(x = Var1 -0.5+r , y = Var2 -0.5+r, z= value, fill=value))}
  p <- p +
    geom_tile(aes(fill = value))
  if(paletteinf != "" & palettesup != ""){p <- p+scale_fill_viridis_c(option = "B", direction = -1, limits = c(paletteinf, palettesup))}
  else{p <- p+scale_fill_viridis_c(option = "B", direction = -1)}
  p <- p +
    labs(title = titre,
         x = nom_axeX,
         y = nom_axeY) +
    guides(fill = guide_colorbar(title = echelle))+
    scale_y_continuous(breaks= scales::pretty_breaks())+
    scale_x_continuous(breaks= scales::pretty_breaks())
  return(p)
  }

