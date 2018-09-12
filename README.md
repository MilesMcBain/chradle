
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

chradle
=======

A simple test harness for Chromium/Chrome, drive-able from R using a websocket connection. It started out as an R6 class but all the asyc/promise craziness got to be too hard in that context, so now it's just a library of functions.

Installation
------------

``` r
devtools::install_github("milesmcbain/chradle")
```

Example
-------

``` r
library(chradle)
library(magrittr)

test_session <-
  chr_init(url = "http://localhost:8080",
           debug_port = 9222,
           bin = "chromium-browser",
           block_on_message_pattern = "THREE.WebGLRenderer") 

session_state <-
  chr_call(test_session, get_attributes(id = "#block",
                                        attribute = "material"))
session_state

chr_kill(test_session)
chr_clean()
```
