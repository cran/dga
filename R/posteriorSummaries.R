#' @export
posteriorSummaryTable <- function(weights, N, levels=c(0.025, 0.975), nrows = 10, ndigits=3) {
  if (is.numeric(weights) & is.null(dim(weights))) {
    weights = matrix(weights, ncol=length(weights))
  }

  assert(is.matrix(weights),
         is.numeric(N),
         ncol(weights) == length(N))

  models = order(-rowSums(weights))[1:min(nrows, nrow(weights))]
  weights = weights[models, , drop=FALSE]

  cols = c("Model", "Prob.", "Bayes est.", "Mode", levels)

  df = data.frame(models,
                  round(rowSums(weights), ndigits),
                  round(bayesEstimator(weights, N)),
                  round(posteriorMode(weights, N)),
                  round(t(posteriorQuantiles(weights, N, levels))))
  colnames(df) = cols

  return(df)
}

#' @export
htmlSummary <- function(filepath, weights, N, graphs=NULL, levels=c(0.025, 0.975), nrows = 10,  type="html", size=60, subplots=TRUE, bg=grDevices::rgb(1,1,1,0)) {

  postSummary = posteriorSummaryTable(weights, N, levels, nrows)

  cols = colnames(postSummary)
  # Prepare space for posterior plots
  if (subplots) {
    postSummary$Posterior = postSummary$Model
    postSummary = postSummary[, c(cols[1], "Posterior", cols[2:length(cols)])]
  }

  table = xtable::xtable(postSummary, auto=TRUE)

  if (!is.null(graphs)) {
    for (i in 1:nrow(postSummary)) {
      model_index = postSummary$Model[i]
      file = paste0(filepath, ".fig", i, ".svg")
      grDevices::svg(file, width=3, height=3, bg=bg)
      plotGraph(graphs[[model_index]], bg=bg)
      grDevices::dev.off()

      table[i,"Model"] = paste0("<img src='./", basename(file), "' width=", size, ">")
    }
  }
  if (subplots) {
    for (i in 1:nrow(postSummary)) {
      model_index = postSummary$Model[i]
      file = paste0(filepath, ".posterior", i, ".svg")
      grDevices::svg(file, width=2, height=3/2, bg=bg)
      graphics::par(mar=c(2,0,2,0))
      graphics::plot(N, weights[model_index,]/sum(weights[model_index,]),
           lwd=2, type="l", xlab="", ylab="", axes=F)
      graphics::axis(1)
      grDevices::dev.off()

      table[i,"Posterior"] = paste0("<img src='./", basename(file), "' width=", size, ">")
    }
  }

  xtable::print.xtable(table, file=filepath, type=type, sanitize.text.function=function(x) x, include.rownames=FALSE, html.table.attributes="")
}

