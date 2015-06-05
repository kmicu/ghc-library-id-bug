# create-workspace.r

library(data.table)
library(foreach)
library(testthat)

inputfiles <- Sys.glob("data/*/*/*/*.csv")
builds <- foreach(file = inputfiles, .combine=rbind, .multicombine=TRUE, .inorder=FALSE) %do% {
    seg <- strsplit(file, "/", fixed=TRUE)[[1]]
    expect_equal(seg[1], "data")
    system <- seg[2]
    compiler <- seg[3]
    package.hash <- sub("^([a-z0-9]+)-(haskell-)?(.*)$", "\\1", seg[4])
    package.name <- sub("^([a-z0-9]+)-(haskell-)?(.*)$", "\\3", seg[4])
    machine <- sub("^(.+)-id\\.csv$", "\\1", seg[5])
    t <- as.data.table(read.csv(file, stringsAsFactors=FALSE))
    t$system <- system
    t$package.hash <- package.hash
    t$package.name <- package.name
    t$libraryid <- sapply(t$libraryid, sub, pattern=paste0("^",package.name,"-"), replacement="")
    expect_true(all(grepl("^[0-9a-z]+$", t$libraryid)))
    t$machine <- machine
    t
}
builds <- within(builds, {
    out <- sub("^/nix/store/([a-z0-9]+)-ghc-(.*)$", "\\1", storepath)
    ghc <- sub("^/nix/store/([a-z0-9]+)-ghc-(.*)$", "\\2", storepath)
    storepath <- NULL
})
