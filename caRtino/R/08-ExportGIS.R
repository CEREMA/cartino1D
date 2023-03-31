#' Export streamnet
#' @noRd
exp_streamnet = function(reseau_liste, rep_sect)                   
  # LPo : Fonction permettant de créer le fichier StreamNet.gpkg
{
  
  st_write(reseau_liste$new_Lines,                                  
           dsn=file.path(rep_sect, "StreamNet.gpkg"),                   
           layer="StreamNet", delete_layer=T, quiet=T)
  # LPo : Création dans la couche StreamNet du geopackage StreamNet.gpkg, du réseau hydrographique 
  # LPo : (new_Lines = biefs prolongés)
  # LPo : delete_layer = TRUE  : S'il existe déjà une couche du même nom, la supprimer
  
  st_write(reseau_liste$junctions,                                 
           dsn=file.path(rep_sect, "StreamNet.gpkg"),                    
           layer="Confluence", delete_layer=T, quiet=T)  
  # LPo : Création dans la couche Confluence du geopackage StreamNet.gpkg, des jonctions 
  # LPo : (colonne junctions de reseau_liste)
  # LPo : delete_layer = TRUE : S'il existe déjà une couche du même nom, la supprimer
  
  file.copy(from = file.path(dir_qml,"reseau.qml"),         
            to = file.path(rep_sect,"StreamNet.qml"))
  # LPo : Copie du qml (symbologie SIG) associé à StreamNet.gpkg
  
  return()
}


#' Export cross sections                                  
#' @noRd
# LPo : Fonction permettant de créer les fichiers ..._CS.gpkg (ph1)

exp_sections = function(xs_station, hydrau, nm_layer, rep_out,opt_mod) 
{
  ifelse ((opt_mod == "flutor"), 
          xs_station %<>% st_bind_cols(hydrau$res[, -c(1,10:11,14)], "w_iter" = hydrau$posi[1,4]),
          xs_station %<>% st_bind_cols(hydrau$res[, -c(11:12,15)], "w_iter" = hydrau$posi[,4]))
  # LPo : Avant pour flutor on avait : xs_station %<>% bind_cols(hydrau$res[, -c(1:2)], hydrau$posi[, 2:4])
  # LPo : = Copie dans xs_station de toutes les colonnes de hydrau$res, sauf la colonne 1 et la colonne 2 
  # LPo : et copie des colonnes 2 à 4 de hydrau$posi
  
  st_write(xs_station,
           dsn=file.path(rep_out, paste0(nm_layer, "_CS.gpkg")),
           layer="ph1", delete_layer=T, quiet=T)
  #layer=paste0(nm_layer, "_ph1"), delete_layer=T, quiet=T)
  # LPo : Création du geopackage "nm_layer"_CS.gpkg et de sa couche "nm_layer"_ph1
  
  return()
}

#' Export cross sections during iterations                                    
#' @noRd
# LPo : Fonction permettant de créer les fichiers ..._CS.gpkg (ph2)

exp_sections_iter = function(trans_bief_lines, hydrau, i_tour, i_densi , nm_layer, rep_out,opt_mod)
{
  #LPo : ajout d'une condition do_densification
  nm_layer_tmp=ifelse(do_densification,
                      paste0("ph2_d", formatC(i_densi-1, width = 2, flag = "0"),"_t",formatC(i_tour, width = 3, flag = "0")),
                      paste0("ph2_t",formatC(i_tour, width = 3, flag = "0"))) 
  
  # nm_layer_tmp=ifelse(do_densification,
  #                     paste0(nm_layer, "_ph2_d", formatC(i_densi, width = 2, flag = "0"),"_t",formatC(i_tour, width = 3, flag = "0")),
  #                     paste0(nm_layer, "_ph2_t",formatC(i_tour, width = 3, flag = "0")))
  
  trans_bief_lines_2 = trans_bief_lines
  
  ifelse ((opt_mod == "flutor"), 
          trans_bief_lines_2 %<>% bind_cols(hydrau$res[, -c(1,10:11,14)]), 
          trans_bief_lines_2 %<>% bind_cols(hydrau$res[, -c(11:12,15)]))
  
  
  st_write(trans_bief_lines_2,
           dsn=file.path(rep_out, paste0(nm_layer, "_CS.gpkg")),
           layer=nm_layer_tmp, delete_layer=T, quiet=T)
  return()
}


#' Export the last cross sections iteration in a geopackage format

exp_sections_gpkg = function (xs_station, hydrau, nm_layer, name_run, rep_out=file.path(rep_resu, name_run),opt_mod)
{
  if (opt_mod == "flutor")
  {
        export_gpkg = st_bind_cols(xs_station[, -c(2:3)], hydrau$res[, -c(1,10:11)], hydrau$profils["LM"])
      }
  
  if (opt_mod == "hecras")
  {
    export_gpkg = st_bind_cols(xs_station[, -c(2:3)], hydrau$res[, -c(11:12,15)], hydrau$profils["LM"])
  }
  
    st_write(obj= export_gpkg, file.path(rep_out, paste0(name_run,
                                                         "_final_results.gpkg")), 
             driver = "GPKG", 
             delete_dsn=T,  
             delete_layer=T, 
             quiet=T)

  return()
}
