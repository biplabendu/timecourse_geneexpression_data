check_sys_dependencies <- function(os = "ubuntu-22.04") {
  if(!require(pak)){
    install.packages("pak")
  }
  pak::pkg_sysreqs(
    sapply(renv:::renv_lockfile_read("renv.lock")$Packages, `[[`, 1), 
    sysreqs_platform = os
  )
}
