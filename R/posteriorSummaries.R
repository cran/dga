#' @export
posteriorSummaryTable <- function(weights, N, levels=c(0.025, 0.975), nrows = 10) {
  if (is.numeric(weights) & is.null(dim(weights))) {
    weights = matrix(weights, ncol=length(weights))
  }

  assert(is.matrix(weights),
         is.numeric(N),
         ncol(weights) == length(N))

  models = order(-rowSums(weights))[1:min(nrows, nrow(weights))]
  weights = weights[models, , drop=FALSE]

  cols = c("Model", "Posterior prob.", "Bayes est.", "Mode", levels)

  df = data.frame(models,
                  rowSums(weights),
                  bayesEstimator(weights, N),
                  posteriorMode(weights, N),
                  t(posteriorQuantiles(weights, N, levels)))
  colnames(df) = cols

  return(df)
}

#' @export
htmlSummary <- function(filepath, weights, N, graphs=NULL, levels=c(0.025, 0.975), nrows = 10,  type="html", size=60, ...) {

  postSummary = posteriorSummaryTable(weights, N, levels, nrows)
  table = xtable::xtable(postSummary)

  if (!is.null(graphs)) {
    for (i in 1:nrow(postSummary)) {
      model_index = postSummary$Model[i]
      file = paste0(filepath, ".fig", i, ".svg")
      svg(file, width=3, height=3)
      plotGraph(graphs[[model_index]])
      dev.off()

      table[i,1] = paste0("<img src='./", basename(file), "' width=", size, ">")
    }
  }

  print(table, file=filepath, type=type, sanitize.text.function=function(x)x)
}

