#' @title Fonction principale pour un cours d'eau et un débit donné
#' @description ENG : Main function for a stream and a discharge
#' 
#' FR : Cette fonction est issue du script 99-RunReach.R.
#' 
#' Elle appelle, dans l'ordre (étape 1/2) :
#' 
#' - La fonction transect_bief_pre ;
#' 
#' - La fonction get_river_angle ;
#' 
#' - La fonction create_sf_riv_sections ;
#' 
#' - La fonction get_elevation ;
#' 
#' - La fonction get_discharge ;
#' 
#' - La fonction run_hydrau_enlarge ;
#' 
#' - La fonction exp_sections (si option "do_export" = TRUE) ;
#' 
#' - La fonction postpro_hydrau ;
#' 
#' - La fonction estimate_posi_width ;
#' 
#' - La fonction exclude_sections (si option "opt_rm_bridge" = TRUE) ;
#' 
#' 
#' 
#' Elle appelle ensuite, dans l'ordre (étape 2/2) :
#' 
#' - La fonction get_river_angle ;
#' 
#' - La fonction create_sf_riv_sections (si option = TRUE) ;
#' 
#' - La fonction smooth_angle (si option "elargissement" = TRUE) ;
#' 
#' - La fonction get_elevation (si option "elargissement" = TRUE) ;
#' 
#' - La fonction get_discharge (si option "elargissement" = TRUE) ;
#' 
#' - La fonction run_hydrau_estimate (si option "elargissement" = TRUE) ;
#' 
#' - La fonction exp_sections_iter (si option "elargissement" = TRUE) ;
#' 
#' - La fonction energy_densification (si option "do_densification" = TRUE) ;
#' 
#' - La fonction exp_sections_gpkg (si option "do_sections_gpkg" = TRUE) ;
#' 
#' - La fonction postpro_hydrau ;
#' 
#' - La fonction run_hydrau_estimate (si option "opt_hydrau_compl" = TRUE) ;
#' 
#' - La fonction postpro_hydrau (si option "opt_hydrau_compl" = TRUE) ;
#' 
#' - La fonction estimate_posi_width ;
#' 
#' - La fonction carto_bief ;
#' 
#'
#' @param xs_table_iter la fonction s'applique sur un tableau de données contenant les informations des profils en travers 
#' / dataframe with cross sections information
#' @return la fonction retourne les profils en travers du cours d'eau en tant que lignes spatiales, ainsi que leurs 
#' caractéristiques / river cross sections as spatial lines with features

cartino_bief = function(xs_table_iter, i_reach)                                                             
  # LPo : cartino_bief = fonction principale (qui fait appel à toutes les autres) pour un bief
{
  ## create run directory                                                                          
  # LPo : Création des répertoires de sortie
  
  if(opt_mod=="flutor") {rep_res = rep_flutor}
  if(opt_mod=="hecras") {rep_res = rep_hecras}
  if(opt_mod=="mascaret") {rep_res = rep_mascaret}
  # # LPo : rep_res = rep_flutor ou rep_hecras ou rep_mascaret en fonction de l'option hydraulique choisie
  
  
  i_Str=1
  # name_run = paste("Bv", num_bv, "_Bi", num_reach, "_", name_disch,"_",opt_mod,"_K",
  #                  Strick[1],  "_",
  #                  Strick[2], sep="")
  
  # Directory for 1 dischrage and all roughness or models
  name_run = paste("Bv", num_bv, "_Bi", num_reach, "_", name_disch, sep="")
  
  dir.create(file.path(rep_resu), showWarnings=F)
  dir.create(file.path(rep_resu, name_run), showWarnings=F)                                        
  # LPo : Répertoires par bief, par débit et par coefficient sont placés dans 
  # LPo : rep_resu = "01-RUN/01-CEREMA/02-OUT/garonette3/Bv01" idem...BV02 etc 
  # LPo : (défini dans le script CARTINO.R)
  
  name_run_opt_str=paste0(name_run,"_",opt_mod,"_K",Strick[1],  "_",Strick[2], sep="")
  ## define run names
  name_res = file.path(rep_resu, rep_res, name_run_opt_str)
  # LPo : name_res = "01-RUN/01-CEREMA/02-OUT/garonette3/Bv01/run_flutor/Bv01_Bi001_q0005"
  
  name_res_hyd = file.path(rep_resu, rep_hydrau, name_run_opt_str)
  # LPo : name_res_hyd = "01-RUN/01-CEREMA/02-OUT/garonette3/Bv01/run_hydrau/Bv01_Bi001_q0005_flutor"
  
  ## ---------------------------------------------------------------------------
  ## Step 1 --------------------------------------------------------------------      
  ## ---------------------------------------------------------------------------
  
  if (is.null(xs_table_iter))
    # LPo : Vérifie que xs_table_iter a bien été effacé à la fin du tour précédent
    # LPo : xs_table_iter doit être NULL pour commencer l'étape 1
  {
    
    ## define cross sections with the same widths # LPo : (and spacing), with a margin
    xs_width_ini = max(xs_width_ini, 2*delta)                                                      
    # LPo : xs_width_ini = largeur maximale entre xs_width_ini (défini dans CARTINO_PARAM.R) et 2*delta 
    # LPo : (delta est également défini dans CARTINO_PARAM.R)
    
    marge = marge_ini - xs_width_ini                                                               
    # LPo : Définition de la marge (LG = LD = marge_ini) (PG = marge) (PD=PG + 2*xs_width_ini)
    
    
    cat(paste0("\n",Sys.time()," -- Sections with the same widths and spacing : ",
               xs_width_ini, " - ", xs_space_ini), "\n")                                           
    # LPo : Affiche dans la console les largeurs des profils en travers 
    # LPo : et le pas d'espace entre eux (choisis par l'utilisateur)
    
    xs_station = transect_bief_pre(shp_reach, min(xs_space_ini,0.4*st_length(shp_reach)))                                  
    # LPo : Applique la fonction transect_bief_pre (création des profils en travers) du script 01-PreHydrau.R
    # LPo : sur shp_reach (le bief du réseau hydrographique étudié sous forme de tibble objet sf)
    # LPo : Avec un pas d'espace défini dans CARTINO_PARAM.R (xs_space_ini)
    # LPo : Les résultats sont stockés dans xs_station qui contient donc : les coordonnées XY du point central du 
    # LPo : profil en travers, la distance cumulée et les écarts entre chaque profil
    
    
    xs_station %<>% 
      mutate(NBief=num_reach, NSection=1:nrow(xs_station),                                                     
             LG = marge_ini, LD = marge_ini,
             PG = marge, PD = PG + 2*xs_width_ini)
    # LPo : Définition de PG et PD
    # LPo : On ajoute au dataframe xs_station : 
    # LPo : Nbief, NSection, LG, LD, PG et PD
    # LPo : Il contenait déjà : DIST, ECART, X, Y
    
    # LPo : LIGNE D'ORIGINE
    # xs_station %<>% 
    #   mutate(NBief=num_reach, NSection=1:n(),                                                     
    #          LG = marge_ini, LD = marge_ini,
    #          PG = marge, PD = PG + 2*xs_width_ini)
    
    ## create cross sections spatial lines perpendicular to the stream geometry
    xs_station$ANGLE = get_river_angle(df_reach, xs_station)                                       
    # LPo : applique la fonction get_river_angle (du script 01-PreHydrau.R) pour obtenir l'angle (radians) des profils 
    # LPo : résultats stockés dans le dataframe xs_station
    
    
    
    xs_station = create_sf_riv_sections(xs_station)                                                
    # LPo : applique la fonction create_sf_riv_sections (du script 01-PreHydrau.R) pour obtenir la géométrie
    # LPo : sous forme de linestring (package sf) -> xs_station devient un objet sf
    
    xs_station %<>% st_set_crs(crs_dem)                                                            
    # LPo : associe le SCR (2154) sur le dataframe xs_station
    
    ## extract elevation and discharge values from rasters
    cat(paste0(Sys.time()," -- Extract elevation and discharge data"), "\n")                                   
    # LPo : affiche dans la console que Cartino extrait les altitudes et les débits à chaque XS
    
    xs_sta_z = get_elevation(ras_dem_vx, xs_station)                                               
    # LPo : applique la fonction get_elevation (du script 01-PreHydrau.R) sur ras_dem_vs et xs_station : 
    # LPo : extraction des altitudes à chaque point de chaque profil en travers (100 points par profil)
    
    xs_sta_q = get_discharge(xs_station)  
    # LPo : applique la fonction get_discharge (du script 01-PreHydrau.R) sur xs_station : extraction du débit 
    # LPo : à chaque profil en travers -> rajout d'une colonne Q au dataframe xs_station
    
    ## run with iterations to enlarge the cross sections
    hydrau = run_hydrau_enlarge(df_reach, rep_res, name_run_opt_str,                                       
                                xs_sta_z, xs_sta_q, opt_mod)
    
    if (is.null(hydrau)==T | max(hydrau$res$Z_M)==3.4e38)
    {
      i_stop_reach=T
      return(NULL)
    }
    
    # LPo : Appel de la fonction run_hydrau_enlarge (du script 04-RunHydrau.R)
    # LPo : qui elle-même applique la fonction run_hydrau_hecras et/ou run_hydrau_flutor
    # LPo : puis également la fonction estimate_bank_posi du script 03-ProcessXS.R
    
    # LPo : Dans hydrau sont stockés les résultats des calculs hydrauliques
    # LPo : hydrau est composé de trois data.frames (c'est une liste de data.frames) :
    # LPo : - hydrau$res = les résultats hydrauliques : par profil en travers : 
    # LPo : charge, hauteur d'eau, débit, côte NGF de l'eau...
    # LPo : - hydrau$posi = les résultats qu'on avait déjà : NSection, PG (= ?), PD (= ?), w_iter (= ?) 
    # LPo : - hydrau$mnt = les résultats qu'on avait déjà : NSection, IPOSI, POSI, POSI_C, RIVE, Z, X, Y
    # LPo : altitude à tous les points des profils en travers (bathymétrie) :
    # LPo : POSI = position des points des XS de 2 en 2 de la rive G à la rive D 
    # LPo : POSI_C = position des points des XS avec point central = 0 
    # LPo : (négatifs = rive G, positifs = rive D)
    # LPo : IPOSI = points numérotés dans l'ordre de la rive G jusqu'à la rive D
    
    ## export cross sections
    if (do_export) 
    { exp_sections(xs_station, hydrau, name_run_opt_str, file.path(rep_resu, name_run),opt_mod)                    
    }
    # LPo : Appel de la fonction exp_sections (du script 08-ExportGIS.R) 
    # LPo : et place le geopackage créé dans rep_resu/name_run 
    # LPo : ("01-RUN/01-CEREMA/02-OUT/aude/Bv01/Bv01_Bi001_Q2018_K15")
    
    ## estimate top width and format results
    Tab_Bief_Result = postpro_hydrau(hydrau, name_res_hyd, name_run_opt_str)                               
    # LPo : Appel de la fonction postpro_hydrau (du script 05-CheckResults.R)  
    # LPo : qui applique elle-même la fonction estimate_water_width 
    # LPo : (du script 05-CheckResults.R) sur hydrau$res, hydrau$mnt : 
    # LPo : estime la largeur de la zone inondée ? 
    # LPo : Z_REF = altitude moyenne du profil en travers ?  
    # LPo : Tab_Bief_Result est un dataframe unique qui réunit les résultats des 3 df de hydrau
    
    
    ## define new position of the cross sections
    cat(paste0(Sys.time()," -- New positioning of cross sections\n\n"), "\n")                                      
    # LPo : affiche dans la console "Repositionnement des profils en travers"
    xs_table_iter = estimate_posi_width(Tab_Bief_Result)                                           
    # LPo : applique la fonction estimate_posi_width (du script 06-UpdateXS.R) : 
    # LPo : copie dans xs_table_iter : NSection, LM, LD, LG, DIST, X, Y des NOUVEAUX PROFILS EN TRAVERS 
    # LPo : (qui seront élargis et densifiés en phase 2)
    
  }## end of step 1 -----------------------------------------------------------
  
  ## remove some cross sections based on their location                                            
  # LPo : Cas des ponts 
  if (opt_rm_bridge) xs_table_iter = exclude_sections(xs_table_iter)                               
  # LPo : Si opt_rm_bridge = T alors on applique la fonction exclude_sections (du script 06-UpdateXS.R)
  
  
  ## ---------------------------------------------------------------------------
  ## Step 2 --------------------------------------------------------------------                   
  ## ---------------------------------------------------------------------------
  # LPo : permet de créer les profils en travers phase 2 (avec le DPNC)
  cat(paste0(Sys.time(),
             "-- Hydraulic option : ", opt_mod,"\n",
             "                    -- Strickler coeff minor river bed : ", Strick[1],
             "\n                    -- Strickler coeff flood plain : ", Strick[2], "\n"))
  ## --------------------------------------------------------------------------------
  # name_run = paste("Bv", num_bv, "_Bi", num_reach, "_", name_disch,"_",opt_mod,"_K",
  #                  Strick[1],  "_",
  #                  Strick[2], sep="")
  # dir.create(file.path(rep_resu, name_run), showWarnings=F) 
  # 
  # ## define run names
  # name_res = file.path(rep_resu, rep_res, name_run)
  # # LPo : name_res = "01-RUN/01-CEREMA/02-OUT/garonette3/Bv01/run_flutor/Bv01_Bi001_q0005"
  # 
  # name_res_hyd = file.path(rep_resu, rep_hydrau, paste0(name_run, "_", opt_mod))
  # # LPo : name_res_hyd = "01-RUN/01-CEREMA/02-OUT/garonette3/Bv01/run_hydrau/Bv01_Bi001_q0005_flutor"
  
  cat(paste0(rep("----", 20), collapse=""), "\n")                                                  
  cat(paste0(Sys.time(),
             " ", name_riv," : ", name_run_opt_str,
             " - Discharge ", i_debit,                                                                 
             "/", length(raster_debit),
             " - Step 2/2"), "\n\n")
  # LPo : affiche dans la console le nom du BV, du bief et du débit en train de tourner 
  # LPo : et affiche "Etape 2/2"
  
  
  i_densi = 0
  # LPo : rajout d'un compteur pour la densification des profils en travers par la charge
  densification_i=T
  while (densification_i)                                                                            
  {
    densification_i=do_densification
    i_densi = i_densi+1
    
    if (i_densi == 1)# & i_mod == 1 && i_Str==1)
    {
      
      ## estimate the angle of each cross section and initialise some values
      xs_table_iter$ANGLE = get_river_angle(df_reach, xs_table_iter)                                 
      # LPo : lorsque i_densi =1 , applique la fonction get_river_angle (du script 01-PreHydrau.R) 
      # LPo : pour calculer les angles des NOUVEAUX profils en travers
      
      Bief_Dist = 
        xs_table_iter %>% 
        dplyr::select(NSection, X, Y, DIST) %>% 
        mutate(ECART = lead(DIST) - (DIST))
      #Bief_Dist$ECART[nrow(Bief_Dist)] = 0
      
    } else
    {
      # LPo : lorsque i_densi=2 et + , on récupère l'angle calculé dans le script 10-EnergyDensification.R
      xs_table_iter$ANGLE = tab_angle2$ANGLE
      
    }
    
    xs_table_iter$w_iter = 0                                                                       
    # LPo : initialisation de w_iter
    
    ## estimate the spacing between cross section                                                  
    # LPo : Estimation des écarts entre les profils en travers
    Bief_Dist = 
      xs_table_iter %>% 
      dplyr::select(NSection, X, Y, DIST) %>% 
      dplyr::mutate(ECART = lead(DIST) - (DIST))
    Bief_Dist$ECART[nrow(Bief_Dist)] = 0
    
    # LPo : Morceau de code remplacé 
    # LPo : Voir la fin du code de energy_densification du script 10-EnergyDensification.R
    # LPO : Et par l'appel de la fonction calculating_distances du script 01-PreHydrau.R (voir + bas)
    
    
    ## loop to enlarge and rotate cross sections by smoothing angles -----------                   
    i_tour = 0                                                                  
    while (elargissement && i_tour < num_iter_enlarge)
    {
      i_tour = i_tour + 1
      
      cat(paste0(Sys.time()," -- Enlarge sections n°: ", i_tour), "\n")
      
      ## create cross sections geometry                                                            
      if (i_tour==1)
      {xs_table_iter$SMOOTH=T}
      
      # if (i_densi ==1)
      # {st_geometry(xs_station)=NULL}
      
      xs_station = 
        xs_table_iter %>% 
        dplyr::select(ANGLE, SMOOTH , w_iter, LG, LD) %>% 
        dplyr::bind_cols(Bief_Dist) %>%
        dplyr::mutate(NBief=num_reach, PG=0, PD=LG+LD) %>%
        create_sf_riv_sections()                                                                   
      # LPo : Appel de la fonction create_sf_riv_sections du script 01-PreHydrau.R  
      # LPo : afin d'obtenirles profils en travers sous forme de LINESTRING 
      
      
      ##### GRAVE
      # sf_ecarts = calculating_distances(xs_station, stream_df, i_reach, num_bv, num_reach)
      # xs_station[,"ECART"] =  sf_ecarts$ECART
      # xs_station[,"DIST"] = sf_ecarts$DIST
      # # LPo : Appel de la fonction calculating_distances pour recalculer ECART et DIST de manière + précise 
      
      ## estimate intersections and apply smoothing by iteration if needed                         
      # LPo : suppression des intersections par lissage si nécessaire
      cat(paste0(Sys.time()," -- Estimate intersections"), "\n")                                               
      # LPo : Affiche dans la console "Estimation des intersections (entre profils)"
      
      xs_station = smooth_angle(xs_station)                                                        
      # LPo : Appel de la fonction smooth_angle du script 02-SmoothAngle.R
      
      xs_station %<>% st_set_crs(crs_dem)
      # LPo : SCR du projet (2154) s'applique sur xs_station
      
      xs_table_iter$SMOOTH = xs_station$SMOOTH
      # LPo : Création de la colonne SMOOTH dans xs_table_iter, 
      # LPo : dont les valeurs sont égales à celles de xs_station$SMOOTH
      
      xs_table_iter$ANGLE = xs_station$ANGLE
      # LPo : Idem avec les angles
      
      
      # ## export cross sections
      # if (do_export) 
      #   exp_sections_iter(xs_station, hydrau, i_tour, i_densi , name_run, file.path(rep_resu, name_run))             
      # # LPo : applique la fonction exp_sections_iter (du script 08-ExportGIS.R) qui permet de créer les profils en travers 
      # # LPo : phase 2 et en précisant le nombre de fois qu'ils ont été élargis (fichier _CS.gpkg ph2/i_tour)
      # 
      
      ## extract elevation and discharge values from rasters - if needed
      cat(paste0(Sys.time()," -- Extract elevation and discharge data"), "\n")
      
      if (i_tour == 1) {
        # pour le tour 1
        
        xs_sta_z = get_elevation(ras_dem_vx, xs_station)                                           
        # LPo : Appel de la fonction get_elevation du script 01-PreHydrau.R. 
        # LPo : Résultats stockés dans xs_sta_z
        
        xs_sta_q = get_discharge(xs_station)                                                       
        # LPo : Appel de la fonction get_discharge du script 01-PreHydrau.R. 
        # LPo : Résultats stockés dans xs_sta_q
        
      } else {
        # Pour les tours =>2
        
        i_iter = xs_station$NSection[xs_station$w_iter>0]
        # LPo : i_iter est l'indice des profils où w_iter est strictement supérieur à 0
        
        xs_sta_z_ = dplyr::filter(xs_sta_z, ! NSection %in% i_iter)
        # LPo : = évite le premier segment de cours d'eau ?
        
        xs_sta_z_iter = get_elevation(ras_dem_vx, xs_station[i_iter, ])                            
        # LPo : Appel de la fonction get_elevation du script 01-PreHydrau.R
        # LPo : sur les profils où w_iter est strictement supérieur à 0
        
        xs_sta_z = rbind(xs_sta_z_, xs_sta_z_iter) %>% dplyr::arrange(NSection)   
        # LPo : Compile les résultats dans un dataframe unique (xs_sta_z)
        # LPo : Résultats regroupés par profil en travers
        
        xs_sta_q = get_discharge(xs_station)
        # LPo : Appel de la fonction get_discharge du script 01-PreHydrau.R
      }
      
      
      ## run and check if there is a need to enlarge the cross sections
      hydrau = run_hydrau_estimate(df_reach, rep_res, name_run_opt_str,                                    
                                   xs_table_iter, xs_sta_z, xs_sta_q)#, opt_mod)#, i_densi)
      # LPo : Appel de la fonction run_hydrau_estimate du script 04-RunHydrau.R 
      # LPo : sur xs_table_iter, xs_sta_z, xs_sta_q 
      # LPo : en fonction du modèle hydraulique choisi
      if (is.null(hydrau)==T | max(hydrau$res$Z_M)==3.4e38)
      {
        i_stop_reach=T
        return(NULL)
      }else{
        
        cat(paste0(Sys.time()," -- ", hydrau$sections_elargies,"/", dim(hydrau$posi)[1]," cross-sections enlarged \n\n" ))
        # cat(paste0(Sys.time()," -- ", length(which(Bief_MNT_NL$RIVE == "G")),"G ",
        #            length(which(Bief_MNT_NL$RIVE == "D")),"D ", "/",dim(Bief_Position_Iter)[1]," cross-sections enlarged \n\n"))
        
        ## export cross sections
        if (do_export) 
          exp_sections_iter(xs_station, hydrau, i_tour, i_densi , name_run_opt_str, file.path(rep_resu, name_run),opt_mod)             
        # LPo : Appel de la fonction exp_sections_iter du script 08-ExportGIS.R 
        # LPo : qui permet de créer les profils en travers phase 2 
        # LPo : et en précisant le nombre de fois qu'ils ont été élargis (fichier _CS.gpkg ph2/i_tour)
        
        
        
        if (identical(xs_table_iter, hydrau$profils)) break
        else xs_table_iter = hydrau$profils
        # LPo : vérifie les résultats par comparaison
        
      }
    }
    ## end of the loop ---------------------------------------------------
    
    
    if (do_densification)
      # LPo : Vérification de la différence de charge et affichage
    { 
      cat(paste0("\n",Sys.time()," -- Check if densification according to the energy is needed :\n", 
                 "                    Parameters :\n",
                 "                        - Energy delta : ", dCharUtil[,1], "\n",
                 "                        - Minimum distance between two cross sections: ", dCharUtil[,2], "\n",
                 "                        - Maximum number of cross sections: ", dCharUtil[3], "\n\n"))
      
      densi = energy_densification(hydrau, xs_station, xs_table_iter, stream_df, i_reach, num_bv, num_reach, i_densi)
      # LPo : Appel de la fonction energy_densification du script 10-EnergyDensification.R
      
      # LPo : S'il n'y a plus de nouveaux profils intermédiaires
      if (dim(densi$tab_energy_densi)[1]==dim(hydrau$profils)[1]) 
      {
        cat(paste0(Sys.time()," -------- Densification complete for this reach \n"), "\n")
        #xs_table_iter = densi$tab_energy_densi
        densification_i=F
        
      }
      # LPo : S'il y a des nouveaux profils intermédiaires
      else
      {
        
        cat(paste0("\n",Sys.time()," -------- Densification n° ", i_densi), "\n")
        cat(paste0("\n",Sys.time()," -------- Densification done for : ",
                   densi$compteur,"/",dim(densi$tab_energy_densi)[1], " gaps \n\n"))
        
        xs_table_iter = densi$tab_energy_densi
        # LPo : Récupération du data.frame qui contient les angles calculés 
        # LPo : des nouveaux profils issus de 10-EnergyDensification.R 
        # LPo : pour i_densi = 2 et +
        
        #FRED:
        st_geometry(xs_table_iter) = NULL
        
        tab_angle2 = densi$tab_angle
        # LPo : Récupération du data.frame qui contient la géométrie des nveaux profils
      }
      
    }#else break
    
  } 
  
  ## end of the loop ---------------------------------------------------------
  # fin de l'étape 2
  if (do_export_gpkg==T)
    # LPo : Appel de la fonction qui exporte le dernier tour de la phase 2 (derniers resultats)
  {
    results_exp_gpkg = exp_sections_gpkg(xs_station, hydrau, nm_layer,
                                         name_run_opt_str, rep_out = file.path(rep_resu, name_run),opt_mod)
    cat(paste0(Sys.time()," -- Shape export \n"))
  }
  
  
  ## estimate top width and format results
  Tab_Bief_Result = postpro_hydrau(hydrau, name_res_hyd, name_run_opt_str)                                 
  # LPo : applique la fonction postpro_hydrau (du scrip 05-CheckResults.R) 
  
  Tab_Bief_Result %<>% mutate(NBasVers=num_bv, debit=raster_debit[i_debit])
  # LPo : avant on avait : Tab_Bief_Result %<>% mutate(NBasVers=num_bv, PR=vec_rp[i_rp])
  # LPo : ajoute les colonnes NBasVers et débit à Tab_Bief_Result
  
  
  # ## define new position of the cross sections
  # cat(paste0(Sys.time()," -- New positioning of cross sections"), "\n")
  # xs_table_iter = estimate_posi_width(Tab_Bief_Result)                                             
  # # LPo : applique la fonction estimate_posi_width du script 06-UpdateXS.R sur Tab_Bief_Result 
  # # LPo : (résultats stockés dans xs_table_iter) : estime la largeur de la zone inondée
  
  
  ## create a surface of surface water and write some raster files
  cat(paste0(Sys.time()," -- Stream post-processing"), "\n")
  carto_bief(Tab_Bief_Result, xs_station, name_run, name_run_opt_str)                                                
  # LPo : applique la fonction carto_bief (du script 07-PostHydrau.R) qui elle-même applique 
  # LPo : la fonction line_interp (du script 07-PostHydrau.R également)
  # LPo : permet de créer un shapefile avec les profils interpolés et la côte NGF au point central du miroir (=HW), 
  # LPo : un raster de HW (=HW_RAS) et un raster de résultats des zones inondées (=WL_RAS)
  
  
  return(list(Tab_Bief_Result = Tab_Bief_Result,                                                   
              trans_bief_lines = xs_station,
              xs_table_iter = xs_table_iter))
  # LPo : retourne les résultats de la fonction principale de 99-RunReach.R (cartino_bief)
}
