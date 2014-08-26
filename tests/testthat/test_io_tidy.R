######################################################################
#
# test_io_tidy.R
#
# copyright (c) 2002, Karl W Broman
# last modified Feb, 2002
# first written Feb, 2002
#
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License,
#     version 3, as published by the Free Software Foundation.
# 
#     This program is distributed in the hope that it will be useful,
#     but without any warranty; without even the implied warranty of
#     merchantability or fitness for a particular purpose.  See the GNU
#     General Public License, version 3, for more details.
# 
#     A copy of the GNU General Public License, version 3, is available
#     at http://www.r-project.org/Licenses/GPL-3
#
# This file contains code for testing the cross IO in R/qtl.
#
# Needed input files from tidy/:
#
#    gen.csv, map.csv, phe.csv    [complete data]
######################################################################
setwd("..")

context("Complete data")

test_that("read.cross.tidy output matches read.cross.csv", {
  expect_identical(read.cross.tidy("tidy"),
                   read.cross.csv("", "listeria.csv"))
})

csv <- read.cross("csv", "", "listeria.csv")
tdy <- read.cross("tidy", "tidy")

test_that("cross created from tidy data", {
  expect_match(class(tdy), "f2|cross")
  expect_identical(tdy, csv)
})


context("Missing data")

test_that("Individuals with missing phenotypes are handled", {
  expect_warning(mp1 <- read.cross("tidy", "tidy", phefile = "phe-missing1.csv"),
                 "1 individuals with genotypes but no phenotypes")
  expect_equal(nind(mp1), nind(csv))
  
  expect_warning(mp2 <- read.cross("tidy", "tidy", phefile = "phe-missing2.csv"),
                 "2 individuals with genotypes but no phenotypes")
  expect_equal(nind(mp2), nind(csv))
})

test_that("Individuals with missing genotypes are handled", {
  expect_warning(mg1 <- read.cross("tidy", "tidy", genfile = "gen-missing1.csv"),
                 "1 individuals with phenotypes but no genotypes")
  expect_equal(nind(mg1), nind(csv))
  
  expect_warning(mg2 <- read.cross("tidy", "tidy", genfile = "gen-missing2.csv"),
                 "2 individuals with phenotypes but no genotypes")
  expect_equal(nind(mg2), nind(csv))
})

context("Exporting data")

write.cross(tdy, "tidy", filestem = "tidy/temp")

test_that("Files exported in tidy format", {
  expect_true(file.exists("tidy/temp_gen.csv"))
  expect_true(file.exists("tidy/temp_phe.csv"))
  expect_true(file.exists("tidy/temp_map.csv"))
})

test_that("Cross can be created from exported files", {
  tdy2 <- read.cross("tidy", "tidy", genfile = "temp_gen.csv", 
                     mapfile = "temp_map.csv", phefile = "temp_phe.csv",
                     genotypes = c("AA", "AB", "BB", "not BB", "not AA"))
  expect_equivalent(tdy2, tdy)
})

# cleanup
unlink(dir("tidy", "temp", full.names = TRUE))

