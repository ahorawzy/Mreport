#' @export
gg_boxplot <- function(calcwmean, xangle = 0, xlabname = FALSE, ylabname = FALSE) {
    x_factor <- factor(as.integer(rownames(calcwmean)), labels = calcwmean[[1]])
    y <- ggplot2::ggplot(data = calcwmean, ggplot2::aes(x = x_factor, y = Wmean)) + ggplot2::geom_bar(fill = "steelblue", 
        stat = "identity") + ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, angle = xangle))
    if (is.character(xlabname)) {
        y <- y + ggplot2::xlab(xlabname)
    }
    if (is.character(ylabname)) {
        y <- y + ggplot2::ylab(ylabname)
    }
    return(y)
}
