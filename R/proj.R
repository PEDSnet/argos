make_tree <- function(base_dir = config('base_dir'),
                      dirs = list('code', 'local', 'reporting',
                                  'results', 'site')) {

  # Don't create more than one level for root directory
  fs::dir_create(base_dir, recurse = FALSE)

  paths <- purrr::map(dirs, ~do.call(fs::path, as.list(c(base_dir, .))))
  created <- purrr::map(paths, ~fs::dir_create(., recurse = TRUE))
  purrr::walk(created, .f = function(x) {
    if (length(fs::dir_ls(x, recurse = TRUE, type = 'file')) == 0)
      fs::file_create(path(x, '.placeholder'))
  })
}
