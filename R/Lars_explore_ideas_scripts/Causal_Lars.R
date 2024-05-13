m <- 10
n_samples <- 50
mu <- rep(1, m)
cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
x_test <- matrix(MASS::mvrnorm(1, mu, cov_mat), nrow = 1)
cnms <- paste0("x", seq(m))
colnames(x_test) <- cnms
index_given <- c(4, 7)
causal_ordering <- list(c(1:3), c(4:6), c(7:10))
confounding <- c(TRUE, FALSE, TRUE)
r <- sample_causal(
  index_given, n_samples, mu, cov_mat, m, x_test,
  causal_ordering, confounding)


x_train = data.table(matrix(MASS::mvrnorm(100, mu, cov_mat), ncol = m))
S <- shapr::feature_matrix_cpp(unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE), m = m)
S <- shapr::feature_matrix_cpp(get_legit_causal_coalitions(causal_ordering = causal_ordering), m = m)

get_S_list_causal(S, causal_ordering, confounding, as_string = TRUE)[[38]]



features = X$features

features


features[sapply(features, respects_order, causal_ordering = list(c(1,2,3), 4:m))]


index = features[[34]]
index

causal_ordering







dt2 = dt[check_coalitions_respect_order(features, causal_ordering)]


causal_ordering2 = causal_ordering


causal_ordering = explanation$internal$parameters$feature_names

causal_ordering = list(c("Solar.R", "Wind"), c("Temp", "Month"))

causal_ordering_vec_sort = sort(unlist(causal_ordering))



group_list <- list(
  A = c("Temp", "Month"),
  B = c("Wind", "Solar.R")
)

group_list <- list(
  c("Temp", "Month"),
  c("Wind", "Solar.R")
)

# Use the empirical approach
explanation_group <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  prediction_zero = p0,
  group = group_list
)


group_names = names(explanation_group$internal$objects$group_num)




causal_ordering_vec_sort = sort(unlist(causal_ordering))

if (explanation_group$internal$parameters$is_groupwise) {
  # Group-wise Shapley values

  # Want to check if `causal_ordering` is the group names or feature

  n_groups = internal$parameters$n_groups
  group_names = names(explanation_group$internal$parameters$group)

  if (is.character(causal_ordering_vec_sort)){
    # Check that all feature names are included
    if (length(causal_ordering_vec_sort) != n_groups || any(causal_ordering_vec_sort != sort(feature_names))) {
      stop(paste0("When the `causal_ordering` list contains strings, then it most contain all group names (`",
                  paste0(group_names, collapse ="`, `"), "`) once."))
    }
  } else if (is.integer(causal_ordering_vec_sort)){
    # Check that the we have m elements and that they are 1 through m (i.e., no duplicates).
    if (length(causal_ordering_vec_sort) != n_groups || any(sort(causal_ordering_vec_sort) != seq(n_groups))) {
      stop(paste0("When the `causal_ordering` list contains integers, then it most contain all integers from 1 to ",
                  n_groups,", the number of groups, once."))
    }
  } else {
    stop(paste0("The `causal_ordering` list must contain either only integers representing the group",
                "indices or the group names as strings. See the documentation for more details."))
  }


} else {
  # Feature-wise Shapley values
  n_features = internal$parameters$n_features
  feature_names = internal$parameters$feature_names

  if (is.character(causal_ordering_vec_sort)){
    # Check that all feature names are included
    if (length(causal_ordering_vec_sort) != n_features || any(causal_ordering_vec_sort != sort(feature_names))) {
      stop(paste0("When the `causal_ordering` list contains strings, then it most contain all feature names (`",
                  paste0(feature_names, collapse ="`, `"), "`) once."))
    }
  } else if (is.integer(causal_ordering_vec_sort)){
    # Check that the we have m elements and that they are 1 through m (i.e., no duplicates).
    if (length(causal_ordering_vec_sort) != n_features || any(sort(causal_ordering_vec_sort) != seq(n_features))) {
      stop(paste0("When the `causal_ordering` list contains integers, then it most contain all integers from 1 to ",
                  n_features,", the number of features, once."))
    }
  } else {
    stop(paste0("The `causal_ordering` list must contain either only integers representing the feature",
                "indices or the feature names as strings. See the documentation for more details."))
  }
}




#' Sample conditional Gaussian variables following a causal chain graph with do-calculus.
#'
#' @inheritParams sample_copula
#'
#' @param causal_ordering List of vectors specifying (partial) causal ordering. Each element in
#' the list is a component in the order, which can contain one or more variable indices in a vector.
#' For example, in list(1, c(2, 3)), 2 > 1 and 3 > 1, but 2 and 3 are not comparable.
#' @param confounding Logical vector specifying which variables are affected by confounding.
#' Confounding must be specified globally with a single TRUE / FALSE value for all components,
#' or separately for each causal component in the causal ordering.
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @author Tom Heskes, Ioan Gabriel Bucur
#'
#' @examples
#' m <- 10
#' n_samples <- 50
#' mu <- rep(1, m)
#' cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
#' x_test <- matrix(MASS::mvrnorm(1, mu, cov_mat), nrow = 1)
#' cnms <- paste0("x", seq(m))
#' colnames(x_test) <- cnms
#' index_given <- c(4, 7)
#' causal_ordering <- list(c(1:3), c(4:6), c(7:10))
#' confounding <- c(TRUE, FALSE, TRUE)
#' r <- shapr:::sample_causal(
#'   index_given, n_samples, mu, cov_mat, m, x_test,
#'   causal_ordering, confounding
#' )
sample_causal <- function(index_given, n_samples, mu, cov_mat, m, x_test,
                          causal_ordering, confounding) {

  # Check input
  stopifnot(is.matrix(x_test))
  stopifnot(is.list(causal_ordering))
  stopifnot(is.logical(confounding))

  if (length(confounding) > 1 && length(confounding) != length(causal_ordering)) {
    stop("Confounding must be specified globally (one value for all components), or separately for each component in the causal ordering.")
  }

  # In case of global confounding value, replicate it across components.
  if (length(confounding) == 1) {
    confounding <- rep(confounding, length(causal_ordering))
  }

  if (!base::setequal(unlist(causal_ordering), seq(m))) {
    stop(paste("Incomplete or incorrect partial causal_ordering specified for", m, "variables"))
  }

  # Handles the unconditional and full conditional separately when predicting
  if (length(index_given) %in% c(0, m)) {
    return(data.table::as.data.table(x_test))
  }


  #index_given = c(1, 6, 9)
  #dependent_ind <- setdiff(1:ncol(S), index_given)
  dependent_ind <- setdiff(seq(m), index_given)
  xall <- data.table(matrix(ncol = m, nrow = n_samples))
  xall[, (index_given) := lapply(x_test[index_given], rep, n_samples)] # Add values from x_test to specified columns in xall

  what_kind = rep(NA, m)
  what_kind[index_given] = "given"

  for(i in seq(length(causal_ordering))) {
   # print(what_kind)

    # check overlap between dependent_ind and component
    to_be_sampled <- intersect(causal_ordering[[i]], dependent_ind)

    if (length(to_be_sampled) > 0) {
      # condition upon all variables in ancestor components
      to_be_conditioned <- unlist(causal_ordering[0:(i-1)])

      # back to conditioning if confounding is FALSE or no conditioning if confounding is TRUE
      if (!confounding[i]) {
        # add intervened variables in the same component
        to_be_conditioned <- union(intersect(causal_ordering[[i]], index_given), to_be_conditioned)
      }

      if (length(to_be_conditioned) == 0) {
        # draw new samples from marginal distribution
        newsamples <- mvnfast::rmvn(n_samples, mu=mu[to_be_sampled], sigma=as.matrix(cov_mat[to_be_sampled,to_be_sampled]))
        newsamples <- create_marginal_data(x_train = x_train, Sbar_features = to_be_sampled, n_samples = 50)


        what_kind[to_be_sampled] = "marginal"


      } else {

        # compute conditional Gaussian
        C <- cov_mat[to_be_sampled,to_be_conditioned, drop=FALSE]
        D <- cov_mat[to_be_conditioned, to_be_conditioned]
        CDinv <- C %*% solve(D)
        cVar <- cov_mat[to_be_sampled,to_be_sampled] - CDinv %*% t(C)
        if (!isSymmetric(cVar)) {
          cVar <- Matrix::symmpart(cVar)
        }

        # draw new samples from conditional distribution
        mu_sample <- matrix(rep(mu[to_be_sampled],each=n_samples),nrow=n_samples)
        mu_cond <- matrix(rep(mu[to_be_conditioned],each=n_samples),nrow=n_samples)
        cMU <- mu_sample + t(CDinv %*% t(xall[,to_be_conditioned] - mu_cond))
        newsamples <- mvnfast::rmvn(n_samples, mu=matrix(0,1,length(to_be_sampled)), sigma=as.matrix(cVar))
        newsamples <- newsamples + cMU

        what_kind[to_be_sampled] = "conditional"

      }


      xall[, (to_be_sampled) := newsamples] # Data table to data table
      # xall[, (to_be_sampled) := split(newsamples, seq_len(ncol(newsamples)))] matrix to data table

      # xall[,to_be_sampled] <- newsamples Matrix version
      #print(c(to_be_sampled, c(0,0,0), to_be_conditioned))
      print(as.data.table(xall))
    }
  }
  what_kind

  #return(what_kind)

  colnames(xall) <- colnames(x_test)
  return(as.data.table(xall))
}

explanation$internal$objects$S
explanation$internal$objects$S









m = 10
S <- feature_matrix_cpp(unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE), m = m)
causal_ordering <- list(c(1:3), c(4:6), c(7:10))
confounding <- c(FALSE, FALSE, FALSE)
confounding <- c(FALSE, FALSE, FALSE)
apply(what_type(S, causal_ordering, confounding), 2, unique)
View(what_type(S, causal_ordering, confounding))


m = 5
S <- feature_matrix_cpp(unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE), m = m)
causal_ordering <- list(1:2, 3:5)
confounding <- c(FALSE, FALSE)
what_type(S, causal_ordering, confounding)


# Hvorfor er ikke andre component også marginal her da for S = 5?
m = 5
causal_ordering <- list(1:2, 3:4, 5)
S <- feature_matrix_cpp(get_legit_causal_coalitions(causal_ordering = causal_ordering), m = m)
confounding <- c(TRUE, TRUE, FALSE)
a1 = what_type(S, causal_ordering, confounding)
a1
SS = get_S_list_causal(S, causal_ordering, confounding, as_string = TRUE)
S[3,]
SS[[3]]

# Effekten av å sette de confounding er at vi får marginal for component 1 når enten feature 1 eller 2 er kjent.
causal_ordering <- list(1:2, 3:4, 5)
confounding <- c(TRUE, FALSE, FALSE)
a2 = what_type(S, causal_ordering, confounding)
a2

a1

names(a1[a1 == ""])
names(a2[a2 == ""])



sbar = c(1,2,3)

tmp = sapply(names(a1), function(pattern) grepl(pattern, paste0(sbar, collapse = ","), fixed = TRUE))
plaussible = rev(names(tmp)[tmp])



# Marginal features
lapply(strsplit(names(a2[a2 == ""]), ","), as.integer)

lapply(strsplit(names(a2[a2 != ""]), ","), as.integer)

a2[a2 == ""]
a2[a2 != ""]
get_legit_causal_coalitions(causal_ordering = causal_ordering)






# Hvorfor skjer ikke dette for component 2?
causal_ordering <- list(1:2, 3:4, 5)
confounding <- c(TRUE, TRUE, FALSE)
what_type(S, causal_ordering, confounding)

causal_ordering <- list(1:2, 3:4, 5)
confounding <- c(FALSE, FALSE, TRUE)
what_type(S, causal_ordering, confounding)

m = 3
S <- feature_matrix_cpp(unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE), m = m)
causal_ordering <- list(1, 2:3)
confounding <- c(TRUE, TRUE)
what_type(S, causal_ordering, confounding)


#
causal_ordering <- list(1, 2, 3)
confounding <- c(TRUE, TRUE, TRUE)
what_type(S, causal_ordering, confounding)

causal_ordering <- list(1, 2, 3)
confounding <- c(FALSE, FALSE, FALSE)
what_type(S, causal_ordering, confounding)

# Cofounder but no causal graph
causal_ordering <- list(1:3)
confounding <- c(TRUE)
what_type(S, causal_ordering, confounding)

# Regular. No Causal graph and no cofounder
causal_ordering <- list(1:3)
confounding <- c(FALSE)
what_type(S, causal_ordering, confounding)


m = 7
S <- feature_matrix_cpp(unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE), m = m)
causal_ordering <- list(1:2, 3:5, 6:7)
confounding <- c(TRUE, FALSE, TRUE)
what_type(S, causal_ordering, confounding)







m <- 10
n_samples <- 50
mu <- rep(1, m)
cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
x_test <- matrix(MASS::mvrnorm(1, mu, cov_mat), nrow = 1)
cnms <- paste0("x", seq(m))
colnames(x_test) <- cnms
causal_ordering <- list(c(1:3), c(4:6), c(7:10))
confounding <- c(FALSE, FALSE, TRUE)
index_given <- c(7)
sample_causal(index_given, n_samples, mu, cov_mat, m, x_test, causal_ordering, confounding)








m = 5
causal_ordering <- list(1:2, 3:4, 5)
S <- feature_matrix_cpp(get_legit_causal_coalitions(causal_ordering = causal_ordering), m = m)
S
confounding <- c(TRUE, TRUE, FALSE)
get_S_list_causal(S, causal_ordering, confounding, as_string = TRUE)

sort(unique(unlist(get_S_list_causal(S, causal_ordering, confounding, as_string = TRUE))))

SS1 = get_S_list_causal(S, causal_ordering, confounding = c(FALSE, FALSE, FALSE), as_string = TRUE)
SS2 = get_S_list_causal(S, causal_ordering, confounding = c(TRUE, FALSE, FALSE), as_string = TRUE)
SS3 = get_S_list_causal(S, causal_ordering, confounding = c(TRUE, TRUE, FALSE), as_string = TRUE)
SS4 = get_S_list_causal(S, causal_ordering, confounding = c(TRUE, TRUE, TRUE), as_string = TRUE)

all.equal(SS1, SS2)
SS1[[2]]
SS2[[2]]
SS1[[3]]
SS2[[3]]

all.equal(SS1, SS3)
SS1[[5]]
SS3[[5]]
SS1[[6]]
SS3[[6]]

all.equal(SS2, SS3)
SS2[[5]]
SS3[[5]]
SS2[[6]]
SS3[[6]]





m = 7
causal_ordering <- list(1:4, 5:6, 7)
S <- feature_matrix_cpp(get_legit_causal_coalitions(causal_ordering = causal_ordering), m = m)
S
confounding <- c(TRUE, TRUE, FALSE)
get_S_list_causal(S, causal_ordering, confounding, as_string = TRUE)









# Sort functions --------------------------------------------------------------------------------------------------
# Copied from `gtools` package
mixedsort <- function (x, decreasing = FALSE, na.last = TRUE, blank.last = FALSE, numeric.type = c("decimal", "roman"),
                       roman.case = c("upper", "lower", "both"), scientific = TRUE) {
  ord <- mixedorder(x, decreasing = decreasing, na.last = na.last, blank.last = blank.last, numeric.type = numeric.type,
                    roman.case = roman.case, scientific = scientific)
  x[ord]
}

mixedorder <- function (x, decreasing = FALSE, na.last = TRUE, blank.last = FALSE, numeric.type = c("decimal", "roman"),
                        roman.case = c("upper", "lower", "both"), scientific = TRUE) {
  numeric.type <- match.arg(numeric.type)
  roman.case <- match.arg(roman.case)
  if (length(x) < 1) return(NULL)
  else if (length(x) == 1) return(1)
  if (!is.character(x)) return(order(x, decreasing = decreasing, na.last = na.last))
  delim <- "\\$\\@\\$"
  if (numeric.type == "decimal") {
    if (scientific) {
      regex <- "((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)(?:(?:[eE])(?:(?:[-+]?)(?:[0123456789]+))|)))"
    }
    else {
      regex <- "((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)))"
    }
    numeric <- function(x) as.numeric(x)
  }
  else if (numeric.type == "roman") {
    regex <- switch(roman.case, both = "([IVXCLDMivxcldm]+)",
                    upper = "([IVXCLDM]+)", lower = "([ivxcldm]+)")
    numeric <- function(x) roman2int(x)
  }
  else {
    stop("Unknown value for numeric.type: ", numeric.type)
  }
  x <- as.character(x)
  which.nas <- which(is.na(x))
  which.blanks <- which(x == "")
  delimited <- gsub(regex, paste(delim, "\\1", delim, sep = ""), x, perl = TRUE)
  step1 <- strsplit(delimited, delim)
  step1 <- lapply(step1, function(x) x[x > ""])
  suppressWarnings(step1.numeric <- lapply(step1, numeric))
  suppressWarnings(step1.character <- lapply(step1, function(x) ifelse(is.na(numeric(x)), toupper(x), NA)))
  maxelem <- max(sapply(step1, length))
  step1.numeric.t <- lapply(1:maxelem, function(i) sapply(step1.numeric, function(x) x[i]))
  step1.character.t <- lapply(1:maxelem, function(i) sapply(step1.character, function(x) x[i]))
  rank.numeric <- sapply(step1.numeric.t, rank)
  rank.character <- sapply(step1.character.t, function(x) as.numeric(factor(x)))
  rank.numeric[!is.na(rank.character)] <- 0
  rank.character <- t(t(rank.character) + apply(matrix(rank.numeric), 2, max, na.rm = TRUE))
  rank.overall <- ifelse(is.na(rank.character), rank.numeric, rank.character)
  order.frame <- as.data.frame(rank.overall)
  if (length(which.nas) > 0) {
    if (is.na(na.last)) order.frame[which.nas, ] <- NA
    else if (na.last) order.frame[which.nas, ] <- Inf
    else order.frame[which.nas, ] <- -Inf
  }
  if (length(which.blanks) > 0) {
    if (is.na(blank.last)) order.frame[which.blanks, ] <- NA
    else if (blank.last) order.frame[which.blanks, ] <- 1e+99
    else order.frame[which.blanks, ] <- -1e+99
  }
  order.frame <- as.list(order.frame)
  order.frame$decreasing <- decreasing
  order.frame$na.last <- NA
  retval <- do.call("order", order.frame)
  return(retval)
}


# Functions -------------------------------------------------------------------------------------------------------
#' Auxiliary function that verifies that the number of combinations is possible
#'
#' @param n_combinations
#' @param causal_ordering
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
check_n_combinations_causal = function(n_combinations, causal_ordering) {
  # Check that we have a legit number of combinations.
  n_combinations_max = get_n_comb_max_causal_ordering(causal_ordering)
  if (n_combinations < 2 || n_combinations > n_combinations_max) {
    stop(paste0("`n_combinations` (", n_combinations ,") must be a strictly postive integer larger than or equal to ",
                "two and less than the number of coalitions respecting the causal ordering (", n_combinations_max, ")."))
  }
}


#' Auxiliary function that verifies that the coalitions respect the causal order
#'
#' @param coalitions List of integer vectors containing the coalitions indicating which
#' features to conditioning on that we are to check against the causal ordering.
#'
#' @param causal_ordering List of vectors containing the partial causal ordering.
#' The elements in the list represents the components in the causal ordering and can either
#' be a single feature index or several, that is, a vector. For example, we can have
#' `list(c(1,2), c(3, 4))`, which means that `1,2 -> 3` and `1,2 -> 4`, i.e., one and
#' two are the ancestors of three and four, but three and four are not related.
#'
#' @return Logical array indicating whether the coalitions respect the causal order or not.
#'
#' @keywords internal
#'
#' @examples
#' coalitions = list(c(1,2,3,5), c(1,2,4), c(1,4))
#' causal_ordering = list(1:3, 4:7, 8:10)
#' check_coalitions_respect_order(coalitions, causal_ordering) # c(TRUE, FALSE, FALSE)
#' check_coalitions_respect_order(c(1,2,3,5), causal_ordering) # TRUE
#' check_coalitions_respect_order(list(c(1,2,3,5)), causal_ordering) # TRUE
#' check_coalitions_respect_order(list(c(1,2,5)), causal_ordering) # FALSE
#' check_coalitions_respect_order(list(c(1:7,10)), causal_ordering) # TRUE
#' check_coalitions_respect_order(list(c(1:3,5:6,10)), causal_ordering) # FALSE
#'
#' @author Lars Henry Berge Olsen
check_coalitions_respect_order <- function(coalitions, causal_ordering) {
  if (!is.list(coalitions)) coalitions = list(coalitions) # Ensure that we are given a list and not a vector
  n_causal_ordering = length(causal_ordering) # Get the number of causal orderings

  # Create a vector to store all ancestors for each causal position/component
  ancestors <- list(integer(0)) # The root component has no ancestors
  if (n_causal_ordering > 1) ancestors = c(ancestors, Reduce(c, causal_ordering[-n_causal_ordering], acc = TRUE))

  # Array to store which coalitions respects the `causal_ordering`. Change to FALSE if coalition does not.
  coalition_respects_order = rep(TRUE, length(coalitions))

  # Iterate over the coalitions
  for (coalition_idx in seq_along(coalitions)) {
    coalition = coalitions[[coalition_idx]]

    # Iterate over the features in the coalition
    for (feature in coalition) {

      # Extract which component the feature is part of (a number between 1 and `length(causal_ordering)`)
      feature_component <- Position(function(ith_component) feature %in% ith_component, causal_ordering)

      # # The feature should always be in the causal_ordering, thus, this is not necessary
      # if (is.na(feature_position)) stop("`feature_position` should never be `NA`.")

      # Get the ancestors of the feature from the pre-computed ancestors list
      current_ancestors <- ancestors[[feature_component]]

      # Check that all ancestors of the feature are present in the coalition, if not, then set to FALSE
      if (!all(current_ancestors %in% coalition)) coalition_respects_order[coalition_idx] = FALSE
    }
  }

  # Return whether the coalitions respect the causal order or not
  return(coalition_respects_order)
}

#' Get the number of coalitions that respects the causal ordering
#'
#' @inheritParams check_coalitions_respect_order
#'
#' @details The function obtains the number of coalitions by computing the number
#' of coalitions in each partial causal component and then summing these. We compute
#' the number of coalitions in \eqn{i}th a partial causal component by \eqn{2^n - 1},
#' where \eqn{n} is the number of features in the the \eqn{i}th partial causal component
#' and we subtract one as we do not want to include the situation where no features in
#' the \eqn{i}th partial causal component are present. In the end, we add 1 for the
#' empty coalition.
#'
#' @examples
#' get_n_comb_max_causal_ordering(list(1:10)) # 2^10 = 1024 (no causal order)
#' get_n_comb_max_causal_ordering(list(1:3, 4:7, 8:10)) # 30
#' get_n_comb_max_causal_ordering(list(1:3, 4:5, 6:7, 8, 9:10)) # 18
#' get_n_comb_max_causal_ordering(list(1:3, c(4,8), c(5,7), 6, 9:10)) # 18
#' get_n_comb_max_causal_ordering(list(1,2,3,4,5,6,7,8,9,10)) # 11
#'
#' @return Integer. The (maximum) number of combinations that respects the causal ordering.
#' @keywords internal
#' @author Lars Henry Berge Olsen
get_n_comb_max_causal_ordering = function(causal_ordering) {
  return(sum(2^sapply(causal_ordering, length)) - length(causal_ordering) + 1)
}




#' Get all coalitions satisfying the causal ordering
#'
#' @inheritParams check_coalitions_respect_order
#' @param sort_features_in_coalitions Boolean. If `TRUE`, then the feature indices in the
#' coalitions are sorted in increasing order. Note that this gives the same order as creating
#' all coalitions and then removing the coalitions that does not satisfy the causal ordering
#' using [check_coalitions_respect_order()]. If `FALSE`, then the function maintains the
#' order of features within each group given in `causal_ordering`.
#'
#' @return List of vectors containing all coalitions that respects the causal ordering.
#' @keywords internal
#'
#' @examples
#' get_legit_causal_coalitions(list(1:3, 4:7, 8:10))
#' get_legit_causal_coalitions(list(1:3, c(4,8), c(5,7), 6, 9:10))
#' get_legit_causal_coalitions(list(3:1, c(8,4), c(7,5), 6, 9:10)) # Same as previous
#'
#' m = 11
#' causal_ordering = list(3:1, c(8,4), c(7,5), 6, 9:10, 11) # All m features must be in this list
#' dt <- data.table::data.table(features = unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE))
#' all.equal(get_legit_causal_coalitions(causal_ordering, sort_features_in_coalitions = TRUE),
#'  dt[check_coalitions_respect_order(features, causal_ordering)]$features)
#'
#' @author Lars Henry Berge Olsen
get_legit_causal_coalitions = function(causal_ordering, sort_features_in_coalitions = TRUE) {
  # Create a list to store the possible coalitions/combinations and start with the empty coalition
  combs = unlist(lapply(0, utils::combn, x = 0, simplify = FALSE), recursive = FALSE)

  # Iterate over the remaining partial causal orderings
  for (i in seq(1, length(causal_ordering))) {

    # Get the number of features in the ith partial causal component
    ith_order_length = length(causal_ordering[[i]])

    # Create a all list of vectors containing all possible feature coalitions except the empty one (with temp indices)
    ith_order_combs =
      unlist(lapply(seq(ith_order_length), utils::combn, x = ith_order_length, simplify = FALSE), recursive = FALSE)

    # Get the ancestors of the ith partial causal component
    ancestors = combs[[length(combs)]]

    # Update the indices by adding the number of ancestors and concatenate the ancestors
    combs = c(combs, sapply(ith_order_combs, function(x) c(ancestors, x + length(ancestors)), simplify = FALSE))
  }

  # Sort the causal components such that the singletons are in the right order
  if (sort_features_in_coalitions) causal_ordering = sapply(causal_ordering, sort)

  # Convert the temporary indices to the correct feature indices
  combs = sapply(combs, function(x) unlist(causal_ordering)[x])

  # Sort the coalitions
  if (sort_features_in_coalitions) combs = sapply(combs, sort)

  return(combs)
}


#' Get the steps for generating MC samples for coalitions following a causal ordering
#'
#' @inheritParams check_coalitions_respect_order
#' @param S ADD inheritParams from somewhere else. NOTE that we assume that this S has been checked. I.e., it only
#' contains coalitions that respects the causal order.
#' @param confounding Boolean or boolean vector specifying which features are affected by confounding. If a single
#' boolean is given, then each component is given this value. Otherwise, `confounding` must be a vector of length
#' `causal_ordering` specifying if each component in the causal order is subject to confounding or not.
#' @param as_string Boolean. If the returned object is to be a list of lists of integers or a list of vectors of strings.
#'
#' @return Depends on the value of the parameter `as_string`. If a string, then `results[j]` is a vector specifying
#' the process of generating the samples for coalition `j`. The length of `results[j]` is the number of steps, and
#' `results[j][i]` is a string of the form `features_to_sample|features_to_condition_on`. If the
#' `features_to_condition_on` part is blank, then we are to sample from the marginal distribution.
#' For `as_string == FALSE`, then we rather return a vector where `results[[j]][[i]]` contains the elements
#' `Sbar` and `S` representing the features to sample and condition on, respectively.
#' @export
#'
#' @examples
#' m = 5
#' causal_ordering <- list(1:2, 3:4, 5)
#' S <- shapr::feature_matrix_cpp(get_legit_causal_coalitions(causal_ordering = causal_ordering), m = m)
#' confounding <- c(TRUE, TRUE, FALSE)
#' get_S_list_causal(S, causal_ordering, confounding, as_string = TRUE)
#'
#' SS1 = get_S_list_causal(S, causal_ordering, confounding = c(FALSE, FALSE, FALSE), as_string = TRUE)
#' SS2 = get_S_list_causal(S, causal_ordering, confounding = c(TRUE, FALSE, FALSE), as_string = TRUE)
#' SS3 = get_S_list_causal(S, causal_ordering, confounding = c(TRUE, TRUE, FALSE), as_string = TRUE)
#' SS4 = get_S_list_causal(S, causal_ordering, confounding = c(TRUE, TRUE, TRUE), as_string = TRUE)
#'
#' all.equal(SS1, SS2)
#' SS1[[2]]
#' SS2[[2]]
#' SS1[[3]]
#' SS2[[3]]
#'
#' all.equal(SS1, SS3)
#' SS1[[5]]
#' SS3[[5]]
#' SS1[[6]]
#' SS3[[6]]
#'
#' all.equal(SS2, SS3)
#' SS2[[5]]
#' SS3[[5]]
#' SS2[[6]]
#' SS3[[6]]
#'
#' @author Lars Henry Berge Olsen.
get_S_list_causal = function(S, causal_ordering, confounding, as_string = FALSE) {
  # Check input
  stopifnot(is.matrix(S))
  stopifnot(is.list(causal_ordering))
  stopifnot(is.logical(confounding))

  if (length(confounding) > 1 && length(confounding) != length(causal_ordering)) {
    stop("Confounding must be specified globally (one value for all components), or separately for each component in the causal ordering.")
  }

  # In case of global confounding value, replicate it across components.
  if (length(confounding) == 1) confounding <- rep(confounding, length(causal_ordering))

  if (!base::setequal(unlist(causal_ordering), seq(m))) {
    stop(paste0("Incomplete or incorrect partial causal_ordering specified for ", m, " variables"))
  }

  # List to store the sampling process
  results = lapply(seq(nrow(S)), function(x) list())
  results_str = lapply(seq(nrow(S)), function(x) NULL)

  # Iterate over the coalitions
  for (j in seq(2, nrow(S)-1)) {

    # Get the given and dependent features for this coalition
    index_given = seq(ncol(S))[as.logical(S[j,])]
    index_dependent = seq(ncol(S))[as.logical(1 - S[j,])]

    # Iterate over the causal orderings
    for(i in seq(length(causal_ordering))) {

      # check overlap between index_dependent and ith causal component
      to_sample <- intersect(causal_ordering[[i]], index_dependent)

      if (length(to_sample) > 0) {
        to_condition <- unlist(causal_ordering[0:(i-1)]) # Condition on all features in ancestor components

        # If confounding is FALSE, add intervened features in the same component to the `to_condition` set.
        # If confounding is TRUE, then no extra conditioning.
        if (!confounding[i]) to_condition <- union(intersect(causal_ordering[[i]], index_given), to_condition)

        # Save Sbar and S (sorting is for the visual)
        to_sample = sort(to_sample)
        to_condition = sort(to_condition)
        results[[j]][[length(results[[j]]) + 1]] = list(Sbar = to_sample, S = to_condition)

        if (as_string) {
          results_str[[j]] = c(results_str[[j]],
                               paste0(paste0(to_sample, collapse = ","), "|", paste0(to_condition, collapse = ",")))
        }
      }
    }
  }

  return(if(as_string) results_str else results) # Return the results
}


#' Create all possible combinations
#'
#' @param m
#' @param causal_ordering
#' @param weight_zero_m
#'
#' @examples
#' m <- 5
#' causal_ordering <- list(1:2, 3:4, 5)
#' X = feature_not_exact_causal(m = m, causal_ordering = causal_ordering, n_combinations = 5)
#' X
#'
#' @keywords internal
#' @author Martin Jullum and Lars Henry Berge Olsen
feature_exact_causal <- function(m, causal_ordering, weight_zero_m = 10^6) {
  # TODO: talk with Martin. We do not need the split here as `causal_ordering` is going to be
  # list(1:m) when no ordering is provided and then `get_legit_causal_coalitions` will produce

  if (length(causal_ordering[[1]]) == m) {
    # Regular
    combinations <- unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)
  } else {
    # New version using the causal ordering
    combinations = get_legit_causal_coalitions(causal_ordering)
  }

  dt <- data.table::data.table(id_combination = seq(length(combinations)))
  dt[, features := combinations]
  dt[, n_features := length(features[[1]]), id_combination]
  dt[, N := .N, n_features]
  dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]

  return(dt)
}


#' Sample a subset of possible combinations
#'
#' @param m
#' @param causal_ordering
#' @param n_combinations
#' @param weight_zero_m
#'
#' @examples
#' m <- 5
#' causal_ordering <- list(1:2, 3:4, 5)
#' n_combinations = 5
#' X = feature_not_exact_causal(m = m, causal_ordering = causal_ordering, n_combinations = n_combinations)
#' X
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
feature_not_exact_causal <- function(m, causal_ordering, n_combinations = 200, weight_zero_m = 10^6) {
  # Check that n_combinations is a valid number of combinations
  check_n_combinations_causal(n_combinations = n_combinations, causal_ordering = causal_ordering)

  # Get all legit combinations
  all_combs = feature_exact_causal(m = m, causal_ordering = causal_ordering, weight_zero_m = weight_zero_m)

  # Sample the `n_combinations` relevant combinations using the `shapley_weights` entries as probabilities
  rel_combs = sample(seq(2, nrow(all_combs) - 1), size = n_combinations - 2, prob = all_combs[-c(1,.N), shapley_weight])

  # Extract the empty, sampled/relevant (sorted), and grand combination, and update the id_combination counter.
  return(all_combs[c(1, sort(rel_combs), .N),][,id_combination := seq(.N)])
}

#' Function that samples data from the empirical marginal training distribution
#'
#' @description
#' Sample observations from the empirical distribution P(X) using the training dataset.
#'
#' @param x_train Data table
#' @param Sbar_features Vector of integers containing the features indices to generate marginal observations for.
#' That is, if `Sbar_features` is `c(1,4)`, then we sample `n_samples` observations from \eqn{P(X_1, X_4)} using the
#' empirical training observations (with replacements). That is, we sample the first and fourth feature values from
#' the same training observation, so we do not break the dependence between them.
#' @param n_samples
#' @param ... Not used.
#'
#' @return Data table of dimension \eqn{`n_samples` \times \text{length}(`Sbar_features`)} with the
#' sampled observations.
#'
#' @examples
#' data("airquality")
#' data <- data.table::as.data.table(airquality)
#' data <- data[complete.cases(data), ]
#'
#' x_var <- c("Solar.R", "Wind", "Temp", "Month")
#' y_var <- "Ozone"
#'
#' ind_x_explain <- 1:6
#' x_train <- data[-ind_x_explain, ..x_var]
#' x_train
#' create_marginal_data(x_train = x_train, Sbar_features = c(1, 4), n_samples = 10)
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
create_marginal_data <- function(x_train, Sbar_features = NULL, n_samples = 10e3, ...) {
  # TODO: Kanskje man skal ha med x_explain her eller den data tablen som man skal fylle opp med data

  # Get the number of training observations
  n_train = nrow(x_train)

  # TODO: decide which method to use
  # If n_samples > n_train, then we do include each training observations n_samples %/% n_train times and
  # then sample the remaining n_samples %% n_train samples. Only the latter is done when n_samples < n_train.
  sampled_indices = c(rep(seq(n_train), each = n_samples %/% n_train), sample(n_train, n_samples %% n_train))
  # Or sample everything and not guarantee that we use all training observations
  # sampled_indices = sample(n_train, n_samples, replace = TRUE)

  # Sample the marginal data and return them
  return(x_train[sampled_indices, ..Sbar_features])
}


