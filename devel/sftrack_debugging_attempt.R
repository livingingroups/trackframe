x <- list(id = c("a", "a", "", "", "a_b"), id_2 = c("b", "", "b", "", "a/c"))
active_group <- c("id", "id_2")
group <-
        do.call(function(...) {
          mapply(list, ..., SIMPLIFY = FALSE)
        }, x)
names(group) <- NULL
    lvlz <-
      vapply(group, function(y) {
        paste0(y[active_group], collapse = "_")
      }, NA_character_)
lvlz

id_list <- make_c_grouping(list(id = c("a", "a", "", "", "a_b"),
                     id_2 = c("b", "", "b", "", "a/c")), active_group = c("id", "id_2"))

