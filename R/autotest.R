#!/usr/bin/env Rscript

library(testthat)
library(methods)
library(stringr)

# get the folder of this R file
args <- commandArgs(trailingOnly = F)  
autotest_Path <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))
setwd(autotest_Path)

code_path <- c('./src')
test_path <- c('./test')

auto_test(code_path, test_path)