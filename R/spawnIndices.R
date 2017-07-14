#' @export
spawnIndices <- function(x, min = 3) {
  seq1 <- data.frame(1, (seq.int(min, (x-min))))
  seq2 <- data.frame((seq.int(min, (x-min)) + 1), x)
  seqs <- unname(as.matrix(cbind(seq1, seq2)))
  seqs
}
