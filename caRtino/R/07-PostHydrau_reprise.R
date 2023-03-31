#' Create spatial lines between two lines and interpolate a value                           
#' @noRd
line_interp_reprise = function(line1, line2, Z1, Z2)                               
  # LPo : création de nouvelles XS afin de créer le raster de résultats (fonction appelée dans la fonction carto_bief du même script)
{
  line1_ = as.matrix(line1)
  line2_ = as.matrix(line2)
  #FRED : 
  #diff=line1_-line2_
  #diffside1 = (diff[1,1]^2+diff[1,2]^2)^0.5
  #diffside2 = (diff[2,1]^2+diff[2,2]^2)^0.5
  #distR = max(diffside1, diffside2)
  
  diff  = (line1_ - line2_)**2
  distR = max((diff[, 1] + diff[, 2])^0.5)
  nlines = max(round(2*distR / (as.numeric(res_dem))) + 1, 2)  
  #LPo : problèmes d'interpolation même en augmentant le nombre de sections (post-traitement) : pixels blancs
  list_line = NULL
  Z = seq(1, (nlines-1))
  for (i in 1:(nlines-1))
  {
    list_line[[i]] = 
      st_linestring(line1_ * (nlines-i)/(nlines-1) + line2_ * (i-1)/(nlines-1))              
    Z[i] = Z1 * (nlines-i)/(nlines-1) + Z2 * (i-1)/(nlines-1)
  }
  st_sfc(list_line) %>% st_sf() %>% mutate(Z_m=Z)
}



#' Create a surface water raster by interpolating between cross sections                    
#' Création d'un raster de résultats (hauteurs d'eau) en interpolant entre deux XS
#' @noRd
carto_bief_reprise = function(Bief_results_reprise_sf, final_sections_reach, name_reach_reprise, rep_bv_reprise)                                
# LPo : carto_bief fonction qui permet d'écrire le raster à partir de Tab_Bief_Results, xs_station, nom_calc         
{
  

  ## create spatial object
  final_sections_reach %<>% dplyr::mutate(Z_m=Bief_results_reprise_sf$Z_m)                                            
  # LPo : copie dans xs_station de Z_M de Tab_bief_results
  
  geom_profil = final_sections_reach %>% st_geometry()                                                
  # LPo : géométries des XS stockées dans geom_profil
  
  ## interpolate between cross sections
  tmp = lapply(1:(length(geom_profil)-1), 
               function(i) line_interp_reprise(geom_profil[i][[1]],                                 
                                               # LPo : appel de la fonction line_interp
                                               geom_profil[i+1][[1]],                               
                                               # LPo : interpolation entre i et i+1
                                               final_sections_reach$Z_m[i], final_sections_reach$Z_m[i+1]))
  
  tmp = do.call(rbind, tmp) %>% st_set_crs(crs_dem)                                         
  
  st_crs(tmp) = crs_dem
  st_crs(final_sections_reach)=crs_dem
  
  final_sections_reach %<>% dplyr::mutate(Z_m=Bief_results_reprise_sf$Z_m)
  
    # 20200916 lines_int = rbind(tmp, final_sections_reach[nrow(final_sections_reach), "Z_m"])  
  lines_int=tmp
  
  # LPo : création de la colonne Z_M dans xs_station
  lines_int$Z_m = round(lines_int$Z_m, 3)                                                     
  # LPo : arrondit à trois chiffres après la virgule
  
  ## write spatial lines
  st_write(lines_int,
           dsn=file.path(rep_bv_reprise, name_reach_reprise, paste0(name_reach_reprise, "_HW.gpkg")),                 
           # LPo : création du shapefile ..._HW.gpkg (profils en travers interpolés avec la côte NGF du miroir)
           layer="Z_m", delete_layer=T, quiet=T)
  
  
  ## create an empty file and fill it with height of water values
  fichasc = file.path(rep_bv_reprise, name_reach_reprise, paste0(name_reach_reprise, "_HW_RAS"))                      
  # LPo : création du raster ..._HW_RAS.gpkg (raster créé à partir du shapefile ..._HW.gpkg)
  tmp_rst = raster::raster(extent(lines_int)*1.05, res=res_dem, vals=-999)
  tmp_rst@crs = CRS(paste0("+init=epsg:", as.character(crs_dem)))
  writeRaster(tmp_rst, 
              paste0(fichasc, ".gpkg"), format="GPKG", overwrite=T)

  gdal_utils("rasterize", 
             file.path(rep_bv_reprise, name_reach_reprise, paste0(name_reach_reprise, "_HW.gpkg")), 
             paste0(fichasc, ".gpkg"), 
             options=c("-at", "-a", "Z_m"))
  
  ## create a raster of water level, height of water minus elevation data
  nomfichmnse = file.path(rep_bv_reprise, name_reach_reprise, paste0(name_reach_reprise, "_WL_RAS.gpkg"))             
  # LPo : création du raster en hauteur d'eau ..._WL_RAS.gpkg (côte NGF du miroir - altitude) 
  
  
  #FRED : copie du qml associé aux rasters water level dans le même dossier
  file.copy(from = file.path(dir_qml_HEAU,'HEAU.qml'),
            to = file.path(rep_bv_reprise,name_reach_reprise,paste0(name_reach_reprise, "_WL_RAS.qml")))              
  
  nomfichasc = paste0(fichasc,".gpkg")
  ras_mns = raster(nomfichasc)
  tmp_origin = raster::origin(ras_mnt_reprise)
  raster::origin(ras_mns) = tmp_origin
  ras_dem_ = crop(ras_mnt_reprise, ras_mns@extent)
  ras_dem_@crs = ras_mns@crs
  ras_mnse = ras_mns - ras_dem_
  ras_mnse[ras_mnse<0] = NA
  writeRaster(ras_mnse, nomfichmnse, format="GPKG", overwrite=T)                            
}


#' Create a surface water raster by aggregation of rasters for each stream
#' @noRd
carto_dicarto_reprise = function(BV_results_reprise, final_sections_BV, raster_debit_reprise, coeff, rep_post_reprise, rep_bv_reprise, NBasVers_formate)                          
# LPo : carto_dicarto = fonction qui compile tous les BV pour ne faire qu'un seul raster de résultats
{

  # LPo : Affichage dans la console
  if(opt_mod_reprise=="hecras") 
  {## write the cross sections
    cat(paste0("-- Post-Processing: ", name_riv_reprise,  
               ", BV",NBasVers_formate, 
               ",  Raster : ", raster_debit_reprise, 
               ", Coeff de Manning n: ", coeff, "\n \n \n"))
  }
  else
  {
    if (coeff[[1]] != coeff[[2]]) 
    {cat(paste0(Sys.Date(),
                "-- Post-Processing: ", name_riv_reprise,  
                ", BV",NBasVers_formate, 
                ",  Raster : ", raster_debit_reprise,
                ", Coefficient de Strickler K lit mineur : ", Strickler[[1]], 
                ", Coefficient de Strickler K lit majeur ", Strickler[[2]], "\n \n \n")) 
    }else
    {
      cat(paste0(Sys.Date(),
                 "-- Post-Processing: ", name_riv_reprise,  
                 ", BV",NBasVers_formate, 
                 ",  Raster : ", raster_debit_reprise,
                 ", Coefficient de Strickler K lit mineur et lit majeur : ", Strickler[[1]], "\n \n \n"))
    }
    
  }
  
  
  # Fusion de final_sections_fusion (sections renumérotées dans le bon ordre) et des résultats obtenus BV_results_reprise
  # Suppression de la colonne "Q_m3_s" (car elle y est deux fois dont une avec des mauvaises valeurs?)
  # et des colonnes "NSection", NBief" et "NBasVers" (en double car tables fusionnées)
  BV_results_reprise_CS = st_sf(cbind(data.frame(final_sections_BV), BV_results_reprise))
  BV_results_reprise_CS = BV_results_reprise_CS[,-c(16,24:26)]
  
  #ProfilBV = trans_riv_lines_PR
  #trans_riv_lines_PR =data.frame(cbind(trans_riv_lines_PR, XS_results))
  
  if (opt_mod_reprise=="flutor")
  {## write the cross sections
    # LPo : ajout d'un if pour rajouter le strickler dans le nom du gpkg du raster unique de résultats 
    if ( Strickler[[1]] !=  Strickler[[2]])
    {
      nom_layer_reprise = paste("Bv", NBasVers_formate, "_Bi", "XXXX", "_", rastr_debit_reprise, "_K_m",  Strickler[[1]], "_K_M",
                                Strickler[[2]], sep="")                        
      # LPo : création des dossiers Bv01_Bi001_Q0005, Bv01_Bi001_q0010... (exemple de la Garonette)
    } else
    {
      nom_layer_reprise = paste("Bv", NBasVers_formate, "_Bi", "XXXX", "_", rastr_debit_reprise, "_K",  Strickler[[1]], sep="")  
    }
  }
  else
  {
    nom_layer_reprise = paste("Bv", NBasVers_formate, "_Bi", "XXXX", "_", rastr_debit_reprise, "_n", as.character(Manning), sep="")
  }
  # LPo : création du ...XXXX_....gpkg (résultats de tous les profils en travers)
  chem_layer = file.path(rep_post_reprise, paste0(nom_layer_reprise, ".gpkg"))
  
  # LPo : 
  st_write(BV_results_reprise_CS, dsn=chem_layer, layer=nom_layer_reprise, delete_layer=F, quiet=T)
  # LPO:
  # st_write(ProfilBV, dsn=chem_layer, layer=nom_layer_reprise, delete_layer=T, quiet=T)            
  
  # LPo : quiet=true = on supprime les infos de nom, de pilote, de taille et de référence spatiale 
  # LPo : en écrivant dans la table du .gpkg
  
  ## aggregate raster data
  tmp_mnse = list.files(rep_bv_reprise, pattern="WL_RAS.gpkg", recursive=T)                     
  # LPo : on copie dans ce raster unique les résultats (Water Level)
  tmp_mnse = tmp_mnse[endsWith(tmp_mnse, "gpkg")]
  tmp_mnse = tmp_mnse[grep(tmp_mnse, pattern=rastr_debit_reprise)]
  
  if(length(tmp_mnse)>1)
  {
    do_vrt = F
    if(do_vrt) {
      tmp_wd = getwd(); setwd(rep_bv_reprise)
      gdal_utils("buildvrt", tmp_mnse, paste0(nom_layer_reprise, ".vrt"))
      setwd(tmp_wd)
    } else {
      x = lapply(file.path(rep_bv_reprise, tmp_mnse), raster)
      names(x)[1:2] = c('x', 'y')
      x$fun = max
      x$na.rm = TRUE
      y = do.call(mosaic, x)
      writeRaster(y, file.path(rep_bv_reprise, paste0(nom_layer_reprise, "_WL_RAS.gpkg")), 
                  format="GPKG", overwrite=TRUE)
      
      
      #FRED : copie du qml associé dans le même dossier
      file.copy(from = file.path(dir_qml_HEAU_reprise,'HEAU.qml'),
                to = file.path(rep_bv_reprise,paste0(nom_layer_reprise, "_WL_RAS.qml")))                
    }
  } else file.copy(from=file.path(rep_bv_reprise, tmp_mnse), 
                   to=file.path(rep_bv_reprise, paste0(nom_layer_reprise, "_WL_RAS.gpkg")), 
                   overwrite=TRUE)
  
  #FRED : copie du qml associé dans le même dossier
  file.copy(from = file.path(dir_qml_HEAU_reprise,'HEAU.qml'),
            to = file.path(rep_bv_reprise,paste0(nom_layer_reprise, "_WL_RAS.qml")))                   
  
}


