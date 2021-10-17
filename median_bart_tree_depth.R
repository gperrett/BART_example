source("simulate_univariate.R")


# Turns a flatted tree into a list of lists
rebuild_tree_recurse <- function(tree.flat, x, ind) {
  # Check if is terminal
  if (tree.flat$var[1L] == -1)
    return(list(value = tree.flat$value[1L], ind = ind))
  
  goes_left <- x[ind, tree.flat$var[1L]] <= tree.flat$value[1L]
  
  left <- rebuild_tree(tree.flat[-1L,], x, ind[goes_left])
  if (is.null(left$n_nodes)) {
    n_nodes.left <- 1L
  } else {
    n_nodes.left <- left$n_nodes
    left$n_nodes <- NULL
  }
  
  right <- rebuild_tree(tree.flat[seq.int(2L + n_nodes.left, nrow(tree.flat)),], x, ind[!goes_left])
  if (is.null(right$n_nodes)) {
    n_nodes.right <- 1L
  } else {
    n_nodes.right <- right$n_nodes
    right$n_nodes <- NULL
  }
  
  list(var = tree.flat$var[1L], value = tree.flat$value[1L], ind = ind,
       left = left, right = right, n_nodes = 1L + n_nodes.left + n_nodes.right)
}

rebuild_tree <- function(tree.flat, x, ind) {
  result <- rebuild_tree_recurse(tree.flat, x, ind)
  result$n_nodes <- NULL
  result
}

get_depth <- function(tree) {
  if (is.null(tree[["left"]])) return(1)
  1 + max(get_depth(tree$left), get_depth(tree$right))
}


# For counting the average tree depth per sample with one tree only
bart_fit <- dbarts::bart2(y ~ z + age, dat,
                          n.trees = 1, keepTrees = TRUE,
                          seed = 0, verbose = FALSE)


trees <- bart_fit$fit$getTrees()
rebuilt_trees <- by(
  trees[c("tree", "var", "value")],
  trees[c("chain", "sample")],
  function(trees) {
    by(trees[c("var", "value")], trees[["tree"]],
       rebuild_tree, x = bart_fit$fit$data@x, ind = seq_along(bart_fit$fit$data@y))
  })

cat("median base-bart tree depth: ", median(sapply(rebuilt_trees, get_depth)), "\n", sep = "")

bartc_fit <- bartCause::bartc(y, z, age, verbose = FALSE,
                              data = dat, estimand = "att",
                              seed = 0,
                              args.rsp = list(n.trees = 1, keepTrees = TRUE))
trees <- bartc_fit$fit.rsp$fit$getTrees()
rebuilt_trees <- by(
  trees[c("tree", "var", "value")],
  trees[c("chain", "sample")],
  function(trees) {
    by(trees[c("var", "value")], trees[["tree"]],
       rebuild_tree, x = bartc_fit$fit.rsp$fit$data@x, ind = seq_along(bartc_fit$fit.rsp$fit$data@y))
  })

cat("median bartc tree depth: ", median(sapply(rebuilt_trees, get_depth)), "\n", sep = "")

