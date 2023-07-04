Greater Rhea project
================
AL Luza, AV Rodrigues, L Mamalis, V Zulian
2023-06-30

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

Spatial distribution of the Greater Rhea, *Rhea americana* (Linnaeus,
1758), in Rio Grande do Sul, southern Brazil: citizen-science data,
probabilistic mapping, and comparison with expert knowledge

You can find the app we used to get expert opinion in the following
link:

<https://andreluza.shinyapps.io/dashboardapp/?_ga=2.49360814.9824114.1608313240-1160325263.1601748869>

<img src="RheaPicture.JPG" width="100%" height="100%" style="display: block; margin: auto;" />
Picture taken by André Luza in Jaguarão, Rio Grande do Sul, 2013.

#### This paper was produced using the following software and associated packages:

    ## R version 4.3.0 (2023-04-21 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
    ## [3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.utf8    
    ## 
    ## time zone: America/Sao_Paulo
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] reshape_0.8.9            tidybayes_3.0.4          lubridate_1.9.2         
    ##  [4] forcats_1.0.0            stringr_1.5.0            purrr_1.0.1             
    ##  [7] readr_2.1.4              tibble_3.2.1             tidyverse_2.0.0         
    ## [10] tidyr_1.3.0              dplyr_1.1.2              corrplot_0.92           
    ## [13] viridis_0.6.3            viridisLite_0.4.2        rasterVis_0.51.5        
    ## [16] gridExtra_2.3            ggplot2_3.4.2            spocc_1.2.2             
    ## [19] rvertnet_0.8.2           CoordinateCleaner_2.0-20 rgbif_3.7.7             
    ## [22] maptools_1.1-6           maps_3.4.1               spdep_1.2-8             
    ## [25] sf_1.0-12                spData_2.2.2             rgeos_0.6-2             
    ## [28] raster_3.6-20            rgdal_1.6-6              sp_1.6-0                
    ## [31] emmeans_1.8.7            brms_2.19.0              Rcpp_1.0.10             
    ## [34] jagsUI_1.5.2             R2WinBUGS_2.1-21         boot_1.3-28.1           
    ## [37] coda_0.19-4              vegan_2.6-4              lattice_0.21-8          
    ## [40] permute_0.9-7            here_1.0.1              
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] svUnit_1.0.6         shinythemes_1.2.0    splines_4.3.0       
    ##   [4] later_1.3.1          rnaturalearth_0.3.2  xts_0.13.1          
    ##   [7] lifecycle_1.0.3      rprojroot_2.0.3      StanHeaders_2.26.27 
    ##  [10] processx_3.8.1       MASS_7.3-58.4        crosstalk_1.2.0     
    ##  [13] ggdist_3.3.0         backports_1.4.1      magrittr_2.0.3      
    ##  [16] rmarkdown_2.21       yaml_2.3.7           httpuv_1.6.10       
    ##  [19] pkgbuild_1.4.0       DBI_1.1.3            RColorBrewer_1.1-3  
    ##  [22] abind_1.4-5          tensorA_0.36.2       inline_0.3.19       
    ##  [25] terra_1.7-29         units_0.8-2          bridgesampling_1.1-2
    ##  [28] codetools_0.2-19     DT_0.28              xml2_1.3.4          
    ##  [31] tidyselect_1.2.0     bayesplot_1.10.0     farver_2.1.1        
    ##  [34] rjags_4-14           matrixStats_1.0.0    stats4_4.3.0        
    ##  [37] base64enc_0.1-3      jsonlite_1.8.4       e1071_1.7-13        
    ##  [40] ellipsis_0.3.2       tools_4.3.0          glue_1.6.2          
    ##  [43] xfun_0.39            mgcv_1.8-42          distributional_0.3.2
    ##  [46] loo_2.6.0            withr_2.5.0          fastmap_1.1.1       
    ##  [49] latticeExtra_0.6-30  fansi_1.0.4          shinyjs_2.1.0       
    ##  [52] callr_3.7.3          digest_0.6.31        timechange_0.2.0    
    ##  [55] R6_2.5.1             mime_0.12            estimability_1.4.1  
    ##  [58] colorspace_2.1-0     wk_0.7.3             gtools_3.9.4        
    ##  [61] jpeg_0.1-10          markdown_1.6         threejs_0.3.3       
    ##  [64] utf8_1.2.3           generics_0.1.3       hexbin_1.28.3       
    ##  [67] data.table_1.14.8    class_7.3-21         prettyunits_1.1.1   
    ##  [70] httr_1.4.6           htmlwidgets_1.6.2    whisker_0.4.1       
    ##  [73] pkgconfig_2.0.3      dygraphs_1.1.1.6     gtable_0.3.3        
    ##  [76] htmltools_0.5.5      scales_1.2.1         png_0.1-8           
    ##  [79] posterior_1.4.1      knitr_1.42           rstudioapi_0.14     
    ##  [82] tzdb_0.4.0           geosphere_1.5-18     reshape2_1.4.4      
    ##  [85] checkmate_2.2.0      nlme_3.1-162         proxy_0.4-27        
    ##  [88] zoo_1.8-12           KernSmooth_2.23-20   miniUI_0.1.1.1      
    ##  [91] foreign_0.8-84       rebird_1.3.0         s2_1.1.3            
    ##  [94] pillar_1.9.0         grid_4.3.0           vctrs_0.6.2         
    ##  [97] shinystan_2.6.0      promises_1.2.0.1     arrayhelpers_1.1-0  
    ## [100] xtable_1.8-4         cluster_2.1.4        evaluate_0.21       
    ## [103] oai_0.4.0            mvtnorm_1.1-3        cli_3.6.1           
    ## [106] compiler_4.3.0       rlang_1.1.1          crayon_1.5.2        
    ## [109] ridigbio_0.3.6       rstantools_2.3.1     interp_1.1-4        
    ## [112] classInt_0.4-9       ps_1.7.5             plyr_1.8.8          
    ## [115] stringi_1.7.12       rstan_2.21.8         deldir_1.0-6        
    ## [118] assertthat_0.2.1     munsell_0.5.0        lazyeval_0.2.2      
    ## [121] colourpicker_1.2.0   Brobdingnag_1.2-9    Matrix_1.5-4        
    ## [124] hms_1.1.3            shiny_1.7.4          highr_0.10          
    ## [127] igraph_1.4.2         RcppParallel_5.1.7
