# dsnlayer="D:\\nabil\\cartino_last\\Cartino_1D"
# setwd(dsnlayer)
############################### CARTINO 1D #####################################
################################################################################

## PACKAGES & DIRECTORIES ######################################################

Start_time <- Sys.time()

# require(caRtino)
require("magrittr")
require("dplyr")
require("raster")
require("sf")
require("tidyselect")
require("tidyverse")
library("sp")
library("ggplot2")
library("lwgeom")
library("tibble")
library("shp2graph")
library("rootSolve")
library("RcppRoll")
#library(velox)

## PROJ ########################################################################

## parameters and global variables
rep_param = "01-RUN/03-test/01-IN"
source(file.path(rep_param, "CARTINO_PARAM.R"))

## LPo : Pour ne pas passer par le package caRtino
source(file.path(rep_caRtino, "00-StreamNetwork.R"), encoding="utf-8")
source(file.path(rep_caRtino, "01-PreHydrau.R"), encoding="utf-8")
source(file.path(rep_caRtino, "02-SmoothAngle.R"), encoding="utf-8")
source(file.path(rep_caRtino, "03-PrepHydrauFlutor.R"), encoding="utf-8")
source(file.path(rep_caRtino, "03-PrepHydrauHEC.R"), encoding="utf-8")
source(file.path(rep_caRtino, "03-ProcessXS.R"), encoding="utf-8")
source(file.path(rep_caRtino, "04-RunHydrau.R"), encoding="utf-8")
source(file.path(rep_caRtino, "05-CheckResults.R"), encoding="utf-8")
source(file.path(rep_caRtino, "06-UpdateXS.R"), encoding="utf-8")
source(file.path(rep_caRtino, "07-PostHydrau.R"), encoding="utf-8")
source(file.path(rep_caRtino, "08-ExportGIS.R"), encoding="utf-8")
source(file.path(rep_caRtino, "09-ExportGraphs.R"), encoding="utf-8")
source(file.path(rep_caRtino, "10-EnergyDensification.R"), encoding="utf-8")
source(file.path(rep_caRtino, "99-RunReach.R"), encoding="utf-8")

mode_CARTINO1D = "automatique"

################################################################################
## calculation for each area
##                          each reach
##                                    each discharge
##                                                each roughness
##                                                            each model
################################################################################

fun_check_exist =                                                                           
  function(dir) if(!file.exists(dir)) print(paste(dir, "not found"))
fun_check_exist(dir_exe)
fun_check_exist(dir_tmp_hecras)
fun_check_exist(dir_qml)
fun_check_exist(rep_data_in)

## input directory - list of areas
i_list_area = which(!grepl(".", list.files(rep_data_in), fixed=T))                     
list_area = list.files(rep_data_in)[i_list_area]                                       
nb_area = length(list_area)               
# LPo : list_area = Liste des dossiers portant un nom de cours d'eau dans "01-RUN/01-CEREMA/01-IN"
# LPo : nb_area = Nombre de dossiers portant un nom de cours d'eau dans "01-RUN/01-CEREMA/01-IN

for (i_area in 1:nb_area)
  # LPo : Les dossiers dans "01-RUN/01-CEREMA/01-IN" sont étudiés tour à tour
{
  ## read files in the input directory -----------------------------------------            
  name_riv = list_area[i_area] 
  # name_dem = paste0(name_riv, ".vrt") 
  name_dem = paste0(name_riv, ".tif") 
  rep_vect = file.path(rep_data_in, name_riv)                                               
  fun_check_exist(file.path(rep_vect, "MNT", name_dem))                                     
  
  cat(paste0(Sys.time()," -- Reading MNT \n"))
  
  ras_dem = raster::raster(file.path(rep_vect, "MNT", name_dem))                            
  
  # Possibilité de gestion de différents modules de traitement du raster
  if (opt_read_raster == "velox")
  {
    ras_dem_vx = velox::velox(ras_dem)
    # LPo : Lecture du MNT par le package "velox" (pour gagner en vitesse)
    res_dem = ras_dem_vx$res[1]                                                               
    # LPo : res_dem = Vecteur contenant la résolution du MNT
  }
  else if(opt_read_raster == "GRASS")
  {
  }
  else
  {
    res_dem = res(ras_dem)[1]
    ras_dem_vx = ras_dem
    # On récupère la résolution du RasterLayer (créé via le package "raster")
  }
  
  ## read and process the stream network ---------------------------------------
  cat(paste0(Sys.time()," -- Processing stream geometry: ", name_riv, "\n"))                                   
  
  ## read the network
  # name_stream_bv = ifelse(length(name_stream) > 1, name_stream[i_area], name_stream)        
  # Automatization to find the name of network
  tmp_shp = list.files(rep_vect, pattern=".shp") 
  tmp_shp=tmp_shp[startsWith(tmp_shp, "ponts")==F][1]
  name_stream_bv=substr(tmp_shp,1,nchar(tmp_shp)-4)
  print(name_stream_bv)
  
  fun_check_exist(file.path(rep_vect, paste0(name_stream_bv, ".shp")))
  # LPo : Vérifie si le shapefile du réseau hydro existe bien dans rep_vect 
  
  stream_sf = sf::read_sf(rep_vect, name_stream_bv)                                         
  # LPo : stream_sf = Lecture du shapefile du réseau hydro (grâce au package sf) sous forme de tibble avec géométrie
  
  if ("NSecteurs" %in% names(stream_sf)) stream_sf = subset(stream_sf, NSecteurs==i_area)   
  
  ## identify several reaches and prolongate them downstream
  dist_down = dist_bief_aval                                                                
  
  stream_sf_ = create_streamnet(stream_sf, dist_down)                                       
  
  stream_df = stream_sf_$new_Lines                                                          
  # LPo : stream_df = Objet sf LINESTRING = Le réseau hydro (extraction du premier des 3 objets de stream_sf_)
  
  ## create the output directories                                                          
  dir.create(rep_data_out, showWarnings=F)
  rep_area = file.path(rep_data_out, name_riv)
  dir.create(rep_area, showWarnings=F)
  
  ## export shp
  if (do_export) exp_streamnet(stream_sf_, rep_area)
  
  ## loop for each reach -------------------------------------------------------    
  # LPo : boucle pour chaque bief prolongé
  
  if (do_export_all_sections)
    # LPo : Si l'option pour exporter le shp de tous les profils en travers 
    # LPo : (de tous les BV du cours d'eau) est activée (=TRUE)
  {
    List_export_all = list()
    # LPo : Création d'une liste vide
    
    ## ini variables at the area level                                       
    # results_area = NULL; xs_area = NULL
    
    for (i_reach in 1:nrow(stream_df))                                       
      # LPo : Pour chaque ligne de stream_df (= pour chaque bief prolongé)
    {
      ## select the reach from the network
      df_reach = stream_sf_$df_stream %>% dplyr::filter(L1==i_reach)         
      
      shp_reach = stream_sf_$new_Lines[i_reach, ]                            
      
      num_reach = formatC(stream_df$NBief[i_reach], width=3, flag="0")       
      
      num_bv = formatC(stream_df$NBasVers[i_reach], width=2, flag="0")     
      
      ## create the output directories                                       
      rep_resu = file.path(rep_area, paste0("Bv", num_bv))                   
      # LPo : rep_resu = "01-RUN/01-CEREMA/02-OUT/garonette3/Bv01" 
      # LPo : puis au tour suivant idem ".../BV02" (s'il y en a plusieurs) etc
      
      dir.create(rep_resu, showWarnings=F)
      # dir.create(file.path(rep_resu, rep_res), showWarnings=F)               
      # LPo : rep_res = "run_flutor"
      
      # dir.create(file.path(rep_resu, rep_inon), showWarnings=F)              
      # LPo : rep_inon = "run_post"
      
      dir.create(file.path(rep_resu, rep_hydrau), showWarnings=F)            
      # LPo : rep_hydrau = "run_hydrau"
      
      ## create functions that returns the positions along the reach
      interpol_bief_X = approxfun(df_reach$dist_cum, df_reach$X, method="linear")       
      # LPo : fonction d'interpolation des points du cours d'eau en X
      interpol_bief_Y = approxfun(df_reach$dist_cum, df_reach$Y, method="linear")       
      # LPo : fonction d'interpolation  des points du cours d'eau en Y
      
      ## creation a function that can used to exclude some cross sections                         
      # LPo : exclut certains profils en travers, lorsqu'il y a des ponts par exemple
      if(opt_rm_bridge) 
      {
        nfile_bridge = file.path(rep_vect, name_bridge)
        interpol_bief_bridge = prep_exclude_sections(nfile_bridge, dist_extra, shp_reach)
      }
      
      ## ini variables at the reach level                                                         
      results_reach = NULL; xs_reach = NULL
      
      xs_table_iter = NULL                                                                        
      # LPo : Création de xs_table_iter (data.frame)
      
      
      ## loop for each discharge ---------------------------------------------    
      tmp_asc = list.files(rep_vect, pattern=".tif") 
      tmp_asc=substr(tmp_asc,1,nchar(tmp_asc)-4)
      raster_debit=as.matrix(tmp_asc,1,lenght(tmp_asc))
      i_stop_reach=F
      for (i_debit in 1:length(raster_debit))
      {
        name_disch = raster_debit[i_debit]
        
        file_disch = paste(name_disch, ".tif", sep="")                                           
        
        ## crop raster for discharge values with the extent of the stream network
        ras_shyrex = raster(file.path(rep_vect, file_disch))
        # ras_shyrex = lecture du raster de débit grâce au package "raster"
        ras_shyrex_ = crop(ras_shyrex, extent(stream_sf) + 2*1000)                               
        # LPo : crop = retourne un jeu de données géographiques (avec extension par exemple)
        # LPo : extention de 2*1000      
        
        ## create a buffer to exclude discharge values around the endpoints
        # width_buffer = 20  # LPo : Paramètre déplacé dans le script CARTINO_PARAM.R 
        ras_shyrex_mask = 
          mask(ras_shyrex_, 
               as_Spatial(st_buffer(stream_sf_$junctions, dist=width_buffer)), inverse=T)
        
        ## prepare the interpolation function of discharge values along the stream
        stream_disch = get_disch_stream(ras_shyrex_mask, df_reach, shp_reach)                    
        
        interpol_bief_Q =                                                                        
          approxfun(stream_disch$DIST, stream_disch$Q, method=met_int_disch, rule=2)             
        
        for (i_Str in 1:nrow(Strickler))
          ## LPo : Ajout d'une boucle pour pouvoir faire tourner CARTINO avec plusieurs frottements 
        {    
          Strick=Strickler[i_Str,]
          for(i_mod in 1:nrow(opt_modele))
          {    opt_mod=opt_modele[i_mod]
          ## run calculation for a reach and a discharge
          if (i_stop_reach==F)
          {
            tmp = cartino_bief(xs_table_iter, i_reach)     
            if (is.null(tmp)==T)
            {
              i_stop_reach=T
              Bug_name=paste("BUG_Bv", num_bv, "_Bi", num_reach, "_", name_disch,"_",opt_mod,"_K",
                             Strick[1],  "_",
                             Strick[2], sep="")
              file.create(file.path(rep_area,Bug_name))
            }else{
              # ## store the results
              # results_reach[[i_debit]] = tmp$Tab_Bief_Result                                              
              # 
              # xs_reach[[i_debit]] =
              #   tmp$trans_bief_lines %>%
              #   dplyr::mutate(NBasVers=num_bv, NBief=num_reach, debit=raster_debit[i_debit]) #FRED PR=per_retour
              
              ## update the cross sections information for the next iteration
              xs_table_iter = tmp$xs_table_iter                                                        
            }
          }else{
            Bug_name=paste("BUG_Bv", num_bv, "_Bi", num_reach, "_", name_disch,"_",opt_mod,"_K",
                           Strick[1],  "_",
                           Strick[2], sep="")
            file.create(file.path(rep_area,Bug_name))
          }
          cat(paste0(Sys.time()," -- End of the discharge : ", i_debit, "\n"))                                            
          cat(paste0(rep("----", 20), collapse=""), "\n\n")
          }
          ## end of the loop for each discharge ----------------------------------                   
        }
      }
        ## clean the cross sections information for the next reach                                 
        rm(xs_table_iter)
      
    } ## end of the loop for each reach ------------------------------------------
    
    ## post-processing -----------------------------------------------------------
    if(post_processing)
    {
      ## Create output directory
      dir.create(file.path(rep_area,rep_inon))
      ## loop for each discharge/roughness/model and export Raster and Vector
      for (i_debit in 1:nrow(raster_debit))
      {
        debit = raster_debit[i_debit]
        for (i_Str in 1:nrow(Strickler))
        {    
          Strick=Strickler[i_Str,]
          for(i_mod in 1:nrow(opt_modele))
          {    
            opt_mod=opt_modele[i_mod]
            ## Create output directory
            rep_out_disch=file.path(rep_area,rep_inon,debit)
            dir.create(rep_out_disch)
            carto_dicarto(debit, formatC(unique(stream_df$NBasVers), width = 2, flag="0"))
          }
        }
      }
    }
  }
}
End_time <- Sys.time()

Taken_time <- hms::as_hms(End_time-Start_time)
print("taken time")
print(Taken_time)
