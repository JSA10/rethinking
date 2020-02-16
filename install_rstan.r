#https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)

pkgbuild::has_build_tools(debug = TRUE)

# configure c++ toolchain
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
    if( grepl("^darwin", R.version$os)) "CXX14FLAGS += -arch x86_64 -ftemplate-depth-256" else 
        if (.Platform$OS.type == "windows") "CXX11FLAGS=-O3 -march=corei7 -mtune=corei7" else
            "CXX14FLAGS += -fPIC",
    file = M, sep = "\n", append = TRUE)

library(rstan)
