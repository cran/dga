#' @export
plotPosteriorN <- function(weights, N, main = NULL) {
  graphics::plot(N, colSums(weights), type = "l", col = "black", lwd = 3, ylab = "Posterior Probability of N", xlab = "N", ylim = c(0, 1.25 * max(colSums(weights))))
  graphics::title(main)
  wts <- rowSums(weights)
  for (i in 1:nrow(weights)) {
    graphics::lines(N, weights[i, ], lwd = wts[i] * 3, lty = "dashed")
  }
  graphics::legend("topright", legend = c("Averaged Post. Prob.", "Post. Prob. By Model"), lty = c(1, 2), cex = .75)
}

#' @export
plotGraph <- function(graph,
                      labels=TRUE,
                      layout="circle",
                      normalize=FALSE,
                      edge.color="black",
                      edge.width=1.5,
                      label.font=2,
                      label.prop=0.9,
                      label.norm="OO",
                      ...) {
  p = length(unique(unlist(graph$C)))

  qgraph::qgraph(adjMat(graph, p),
         labels=labels,
         layout=layout,
         normalize=normalize,
         edge.color=edge.color,
         edge.width=edge.width,
         label.font=label.font,
         label.prop=label.prop,
         label.norm=label.norm,
         ...)
}


