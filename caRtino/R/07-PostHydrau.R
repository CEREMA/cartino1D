#' Create spatial lines between two lines and interpolate a value                           
#' @noRd
line_interp = function(line1, line2, Z1, Z2)                               
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
  st_sfc(list_line) %>% st_sf() %>% mutate(ZM=Z)
}



#' Create a surface water raster by interpolating between cross sections                    
#' création d'un raster de résultats (hauteurs d'eau) en interpolant entre deux XS
#' @noRd
carto_bief = function(Tab_Bief_Result, xs_station, nom_rep, nom_calc)                                
  # LPo : carto_bief fonction qui permet d'écrire le raster à partir de Tab_Bief_Results, xs_station, nom_calc         
{
  ## create spatial object
  xs_station %<>% mutate(ZM=Tab_Bief_Result$Z_M)                                            
  # LPo : copie du champ Z_M de Tab_Bief_Results dans xs_station
  geom_profil = xs_station %>% st_geometry()                                                
  # LPo : géométrie des XS
  
  ## interpolate between cross sections
  tmp = lapply(1:(length(geom_profil)-1), 
               function(i) line_interp(geom_profil[i][[1]],                                 
                                       # LPo : appel de la fonction line_interp
                                       geom_profil[i+1][[1]],                               
                                       # LPo : interpolation entre i et i+1
                                       xs_station$ZM[i], xs_station$ZM[i+1]))
  tmp = do.call(rbind, tmp) %>% st_set_crs(crs_dem)                                         
  # LPo : SCR=2154
  lines_int = rbind(tmp, xs_station[nrow(xs_station), "ZM"])                                
  # LPo : création de la colonne Z_M dans xs_station
  lines_int$ZM = round(lines_int$ZM, 3)                                                     
  # LPo : arrondit à trois chiffres après la virgule
  
  ## write spatial lines
  st_write(lines_int,
           dsn=file.path(rep_resu, nom_rep, paste0(nom_calc, "_HW.gpkg")),                 
           # LPo : création du shapefile ..._HW.gpkg (profils en travers interpolés avec la côte NGF du miroir)
           layer="ZM", delete_layer=T, quiet=T)
  
  ## create an empty file and fill it with height of water values
  fichasc = file.path(rep_resu, nom_rep, paste0(nom_calc, "_HW_RAS"))                      
  # LPo : création du raster ..._HW_RAS.gpkg (raster créé à partir du shapefile ..._HW.gpkg)
  tmp_rst = raster::raster(extent(lines_int)*1.05, res=res_dem, vals=-999)
  tmp_rst@crs = CRS(paste0("+init=epsg:", as.character(crs_dem)))
  writeRaster(tmp_rst, 
              paste0(fichasc, ".gpkg"), format="GPKG", overwrite=T)
  gdal_utils("rasterize", 
             file.path(rep_resu, nom_rep, paste0(nom_calc, "_HW.gpkg")), 
             paste0(fichasc, ".gpkg"), 
             options=c("-at","-a", "ZM"))
  
  ## create a raster of water level, height of water minus elevation data
  nomfichmnse = file.path(rep_resu, nom_rep, paste0(nom_calc, "_WL_RAS.gpkg"))             
  # LPo : création du raster en hauteur d'eau ..._WL_RAS.gpkg (côte NGF du miroir - altitude) 
  
  
  #FRED : copie du qml associé aux rasters water level dans le même dossier
  file.copy(from = file.path(dir_qml_HEAU,'HEAU.qml'),
            to = file.path(rep_resu,nom_rep,paste0(nom_calc, "_WL_RAS.qml")))              
  
  nomfichasc = paste0(fichasc,".gpkg")
  ras_mns = raster(nomfichasc)
  tmp_origin = raster::origin(ras_dem)
  raster::origin(ras_mns) = tmp_origin
  ras_dem_ = crop(ras_dem, ras_mns@extent)
  ras_dem_@crs = ras_mns@crs
  ras_mnse = ras_mns - ras_dem_
  ras_mnse[ras_mnse<0] = NA
  writeRaster(ras_mnse, nomfichmnse, format="GPKG", overwrite=T)                            
}


#' Create a surface water raster by aggregation of rasters for each stream, each model and each roughness
#' @noRd
carto_dicarto = function(debit,NBasVBers)   
{
  cat(paste0(Sys.time()," -- Post-Processing: ", name_riv,  ", ", debit,"_", opt_mod,"_K", Strick,"_",
             Strick), "\n \n \n")
  
  for (num_bv in NBasVBers)
  {
    rep_resu = file.path(rep_area, paste0("Bv", num_bv))                                    
    keyword=paste(debit,"_",
                    opt_mod,"_K",
                    Strick[1],"_",
                    Strick[2], sep="")
    
    nomlayer = paste("Bv", num_bv, "_Bi", "XXXX", "_", 
                     keyword, sep="")  
    
    # LPo : création du ...XXXX_....gpkg (du raster unique de résultats pour tous les biefs)
    # chem_layer = file.path(rep_resu, rep_inon, paste0(nomlayer, ".gpkg"))
    # st_write(ProfilBV, dsn=chem_layer, layer=nomlayer, delete_layer=T, quiet=T)            
    # LPo : quiet=true = on supprime les infos de nom, de pilote, de taille et de référence spatiale en écrivant 
    # LPo : dans la table du .gpkg
    
    
    ## aggregate raster data
    tmp_mnse = list.files(rep_resu, pattern="WL_RAS.gpkg", recursive=T)                     
    tmp_mnse = tmp_mnse[grep(tmp_mnse, pattern=keyword)]
    
    if(length(tmp_mnse)>1)
    {
      do_vrt = F
      if(do_vrt) {
        tmp_wd = getwd(); setwd(rep_resu)
        gdal_utils("buildvrt", tmp_mnse, paste0(nomlayer, ".vrt"))
        setwd(tmp_wd)
      } else {
        x = lapply(file.path(rep_resu, tmp_mnse), raster)
        names(x)[1:2] = c('x', 'y')
        x$fun = max
        x$na.rm = TRUE
        y = do.call(mosaic, x)
        writeRaster(y, file.path(rep_out_disch,paste0(nomlayer, "_WL_RAS.gpkg")), 
                    format="GPKG", overwrite=TRUE)
      }
    } else file.copy(from=file.path(rep_resu, tmp_mnse), 
                     to=file.path(rep_out_disch,paste0(nomlayer, "_WL_RAS.gpkg")), 
                     overwrite=TRUE)
    
    #FRED : copie du qml associé dans le même dossier
    file.copy(from = file.path(dir_qml_HEAU,'HEAU.qml'),
              to = file.path(rep_out_disch,paste0(nomlayer, "_WL_RAS.qml")))                  
    
    keywordCS=paste(debit,"_",
                   opt_mod,"_K",
                   Strick[1],"_",
                   Strick[2], 
                   "_final_results.gpkg",sep="")
    
    ## Vector aggregation
    tmp_cs = list.files(rep_resu, pattern="_final_results.gpkg", recursive=T)
    tmp_cs = tmp_cs[grep(tmp_cs, pattern=keyword)]
    if (length(tmp_cs)>0)
    {
      x=lapply(file.path(rep_resu,tmp_cs),st_read)
      
      st_write(obj= do.call(rbind,x),
               file.path(rep_out_disch,paste0(nomlayer, "_final_results.gpkg")),
               nomlayer,driver = "GPKG",delete_dsn=F,delete_layer=T,quiet=T)
    }
  }
}

