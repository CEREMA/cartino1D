#' @title Lancer les simulations hydrauliques et élargit les profils en travers si besoin
#' @description ENG : Run steady simulations and enlarge sections if needed                                  
#' 
#' FR : Cette fonction est issue du script 04-RunHydrau.R.
#' 
#' Ne s'applique que dans l'étape 1/2.
#' 
#' La fonction lance les simulations hydrauliques :
#' 
#' - Si l'option choisie est HEC_RAS, on appellera run_hydrau_hecras du script 03-PrepHydrauHEC.R ;
#' 
#' - Si l'option choisie est Flutor, on appellera run_hydrau_flutor du script 03-PrepHydrauFlutor.R ;
#' 
#' Ensuite, la fonction run_hydrau_enlarge appelle la fonction estimate_bank_posi du script 03-ProcessXS.R 
#' pour recalculer les positions des points hauts (digues) en fonction de la zone inondée 
#' (pour éviter d'avoir des profils en travers qui se terminent dans l'eau).
#' En boucle elle élargit les profils en travers jusqu'à déterminer un point haut, de chaque côté 
#' de chacun des profils en travers.
#' 
#' @param df_reach la fonction s'applique sur un tableau de données qui contient les coordonnées des points de construction 
#' du cours d'eau / dataframe with XY centers of the stream
#' @param rep_res la fonction s'applique sur le dossier de résultats / directory of the results
#' @param name_run la fonction s'applique sur la variable nom qui comprend le débit + le bief + le BV étudié / name of the run file
#' @param xs_sta_z la fonction s'applique sur un tableau de données qui contient les altitudes / dataframe with elevation data
#' @param xs_sta_q la fonction s'applique sur un tableau de données qui contient les débits / dataframe with discharge data
#' @param opt_mod permet le choix du modèle hydraulique / name of the hydraulic model
#' @return la fonction retourne un tableau de données avec les altitudes limitées ??? pour chaque profil en travers 
#' et d'autres infos / dataframe with limited elevation data for each section and more info
#' 
run_hydrau_enlarge = function(df_reach, rep_res, name_run,                                 
                              xs_sta_z, xs_sta_q, opt_mod)
{
  ## get information of the stream
  XY = sf::st_coordinates(st_sf(data.frame(xs_sta_z)))
  # LPo : XY = nouveau dataframe. Les coordonnées des points le long des XS sont extraits de xs_sta_z 
  # LPo : grâce à st_coordinates et stockés dans XY
  # LPo : LIGNE D'ORIGINE :
  #XY = xs_sta_z %>% sf::st_coordinates() %>% data.frame()  
  Bief_MNT = cbind(xs_sta_z, "X"= XY[,"X"], "Y"=XY[,"Y"])
  # LPo : LIGNE D'ORIGINE :
  #Bief_MNT = xs_sta_z %>% bind_cols(XY)
  Bief_MNT = st_sf(data.frame(Bief_MNT))
  # LIGNE D'ORIGINE : 
  #colnames(Bief_MNT) = c("NSection", "POSI", "IPOSI", "POSI_C", "RIVE", "Z", "X", "Y", "geometry")
  sf::st_geometry(Bief_MNT) = NULL
  # LPo : Bief_MNT : objet sf qui contient les colonnes de xs_sta_z (=les 100 points le long des XS)
  # LPo : + les colonnes X et Y avec les coordonnées extraits
  # LPo : Suppression de la colonne "geometry" :
  # LPo : Il n'est plus un objet sf mais simplement un tibble
  
  Bief_STA = xs_sta_q                                                                      
  # LPo : On copie xs_sta_q dans un nouveau objet sf : Bief_STA
  # LPo : Contient les caractéristiques des XS sous forme de LINESTRING :
  # LPo : DIST, ECART, X point central, Y point central, NBief, NSection,
  # LPo : LG, LD, PG, PD, ANGLE, geometry, et Q
  
  ## initialise bank position
  Bief_Position_Iter = Bief_STA %>% dplyr::select(NSection, PG, PD)                        
  # LPo : Bief_Position_Iter = Bief_STA avec uniquement les champs NSection, PG, PD et geometry 
  # LPo : (Bief_Position_Iter = objet sf)                                                                                        
  st_geometry(Bief_Position_Iter) = NULL                                                   
  # LPo : Bief_Position_Iter n'a plus de géométrie (donc cela devient un data.frame)
  Bief_Position_Iter %<>% mutate(w_iter=0)                                                 
  # LPo : Création de la colonne w_iter dans Bief_Position_Iter (initialisée à 0)
  
  cat(paste0("\n",Sys.time()," ----  Run Hydrau : ", name_run), "\n")                                    
  # LPo : cat = lorsque cartino s'exécute, permet d'écrire le nom du BV, du bief, et du débit qui sont calculés (variable name_run)
  
  for (i_iter in 1:10)                                                                     
    # LPo : Pour les 10 itérations (= les 10 élargissements tour à tour)
  {
    
    ## identify and keep only the points within the left and right banks                   
    # LPo : identifier les XS
    Bief_MNT_Iter =                                                                        
      Bief_MNT %>% 
      left_join(Bief_Position_Iter, by="NSection") %>%                                     
      # LPo : Jointure à gauche, copie les colonnes de Bief_Position_Iter et de Bief_MNT dans Bief_MNT_Iter 
      # LPo : avec la variable de jointure "Nsection" 
      dplyr::filter(POSI >= PG & POSI <= PD) %>%                                                  
      # LPo : POSI = positions des points le long de la XS, de la rive G à la rive D
      # LPo : Filtre pour ne conserver que les 20 points centraux de chaque XS
      dplyr::select(NSection, IPOSI, POSI, POSI_C, RIVE, Z, X, Y)                          
    # LPo : Sélection pour ne conserver que 8 champs
    
    
    
    if(opt_mod=="hecras")
    {
      Bief_Results_Iter =                                                                  
        run_hydrau_hecras(df_reach, rep_res, name_run, Bief_MNT_Iter, Bief_STA)            
      # LPo : Si l'option choisie est HEC_RAS, on appelle run_hydrau_hecras (script 03-PrepHydrauHEC.R)
      # LPo : Bief_Results_Iter = tableau de données (data.frame) qui contient les résultats hydrauliques
    }
    if(opt_mod=="flutor")
    {
      Bief_Results_Iter =                                                                  
        run_hydrau_flutor(rep_res, name_run, Bief_MNT_Iter, Bief_STA, i_iter)                      
      # LPo : Si l'option choisie est flutor, on appelle la fonction run_hydrau_flutor (script 03-PrepHydrauFlutor.R)
      # LPo : Bief_Results_Iter = tableau de données (data.frame) qui contient les résultats hydrauliques
    }
    if (opt_mod=="mascaret")
    {
      Bief_Results_Iter =
        run_hydrau_mascaret(rep_res, name_run, Bief_MNT_Iter, Bief_STA)
      # LPo : Si l'option choisie est mascaret, on appelle la fonction run_hydrau_mascaret 
      # LPo : Bief_Results_Iter = tableau de données (data.frame) qui contient les résultats hydrauliques
    }
    
    if (is.null(Bief_Results_Iter)==T)
    {
      return(NULL)
    }else{
      
      ## create a variable of surface water level or head
      if (opt_h=="SL_m") {                                                                     
        Bief_Results_Iter %<>% mutate(Z_M = SL_m)                                           
      } else Bief_Results_Iter %<>% mutate(Z_M = CHAR_m)                                    
      # LPo : La colonne Z_M peut contenir soit la côte NGF de l'eau soit la charge
      
      ## identity the new bank positions
      Bief_Position_Iter = 
        estimate_bank_posi(Bief_Results_Iter, Bief_MNT, Bief_Position_Iter, i_iter)         
      # LPo : Appel de la fonction estimate_bank_posi du script 03-ProcessXS.R
      
      
      ## break the loop if there is no need to enlarge a cross section
      if ( sum(Bief_Position_Iter$w_iter == i_iter) == 0) break
    }
  }
  # 
  cat(paste0(Sys.time()," ---- Number of iteration : ", i_iter), "\n")                                
  # LPo : Retourne le numéro de l'itération 
  
  ## keep the distance values
  Bief_Results_Iter$DIST = Bief_STA$DIST                                                  
  # LPo : On conserve les valeurs de distance cumulée
  
  
  return(list(res=Bief_Results_Iter, posi=Bief_Position_Iter, mnt=Bief_MNT_Iter))         
  # LPo : stocke dans une liste de data.frames les résultats hydrauliques $res, les positions des digues $posi, et les altitudes $mnt
}


#' @title Lancer les simulations hydrauliques et estimer les nouvelles largeurs des profils en travers
#' @description ENG : Run steady simulations and estimate the new widths of sections                         
#' 
#' FR : Cette fonction est issue du script 04-RunHydrau.R.
#' 
#' Ne s'applique que dans l'étape 2/2.
#' 
#' La fonction identifie les XS ayant besoins d'être élargies.
#' 
#' Ensuite elle lance les simulations hydrauliques :
#' 
#' - Si l'option choisie est HEC_RAS, on appellera run_hydrau_hecras du script 03-PrepHydrauHEC.R ;
#' 
#' - Si l'option choisie est Flutor, on appellera run_hydrau_flutor du script 03-PrepHydrauFlutor.R ;
#' 
#' Elle élargit les XS et stocke en mémoire les nouvelles largeurs. 
#' 
#' @param df_reach la fonction s'applique sur un tableau de données qui contient les coordonnées des 
#' points de construction du cours d'eau / dataframe with XY centers of the stream
#' @param rep_res la fonction s'applique sur le dossier de résultats / directory of the results
#' @param name_run la fonction s'applique sur la variable nom qui comprend le débit + le bief + le BV étudié / name of the run file
#' @param Profils_Iter la fonction s'applique sur un tableau de données qui contient les informations sur les profils en travers 
#' / dataframe with section information
#' @param xs_sta_z la fonction s'applique sur un tableau de données qui contient les altitudes / dataframe with elevation data
#' @param xs_sta_q la fonction s'applique sur un tableau de données qui contient les débits / dataframe with discharge data
#' @param opt_mod permet le choix du modèle hydraulique / name of the hydraulic model
#' @return retourne une liste de résultats formatés / list of formatted outputs

run_hydrau_estimate = function(df_reach, rep_res, name_run,                               
                               Profils_Iter, xs_sta_z, xs_sta_q)#, opt_mod)#, i_densi)                 
{
  ## get information of the stream
  XY = sf::st_coordinates(st_sf(data.frame(xs_sta_z)))
  # LPo : XY = nouveau dataframe. Les coordonnées des points le long des XS sont extraits de xs_sta_z 
  # LPo : grâce à st_coordinates et copiés dans XY
  Bief_MNT = cbind(xs_sta_z, "X"= XY[,"X"], "Y"=XY[,"Y"]) 
  #Bief_MNT = xs_sta_z %>% bind_cols(XY)
  Bief_MNT = st_sf(data.frame(Bief_MNT))
  #colnames(Bief_MNT) = c("NSection", "POSI", "IPOSI", "POSI_C", "RIVE", "Z", "X", "Y", "geometry")
  sf::st_geometry(Bief_MNT) = NULL                                                         
  # LPo : Bief_MNT n'a plus de géométrie
  # LPo : Bief_MNT contient les points centraux des profils en travers (coordonnées XY)
  
  Bief_STA = xs_sta_q                                                                     
  # LPo : les caractéristiques des XS dont les données de débits à chaque XS sont stockées dans le dataframe Bief_STA
  
  ## initialise bank position                                                             
  Bief_Position_Iter = Bief_STA %>% dplyr::select(NSection, PG, PD)                       
  st_geometry(Bief_Position_Iter) = NULL                                                  
  # LPo : Bief_Position_Iter n'a plus de géométrie
  Bief_Position_Iter %<>% mutate(w_iter=0)                                                
  # LPo : w_iter = colonne de Bief_Position_Iter initialisée à 0
  
  cat(paste0(" \n",Sys.time()," ---- Run Hydrau : ", name_run, "\n\n") )                                   
  # LPo : cat = retourne le nom du BV, du bief et du débit (=name_run)
  
  ## identify and keep only the points within the left and right banks
  Bief_MNT_Iter =                                                                         
    # LPo : Bief_MNT_Iter = altitudes
    Bief_MNT %>% 
    left_join(Bief_Position_Iter, by="NSection") %>%                                      
    # LPo : jointure à gauche, copie dans Bief_MNT les colonnes de Bief_Position_Iter avec la variable de jointure "Nsection" 
    filter(POSI >= PG & POSI <= PD) %>%                                                   
    dplyr::select(NSection, IPOSI, POSI, POSI_C, RIVE, Z, X, Y)                           
  # LPo : Sélection
  
  
  if(opt_mod=="hecras")                                                                   
    # LPo : si l'option choisie est HECRAS
  {
    Bief_Results_Iter = 
      run_hydrau_hecras(df_reach, rep_res, name_run, Bief_MNT_Iter, Bief_STA)           
    # LPo : on appellera run_hydrau_hecras (script 03-PrepHydrauHEC.R)
  }
  if(opt_mod=="flutor")                                                                   
    # LPo : si l'option choisie est flutor
  {
    Bief_Results_Iter = 
      run_hydrau_flutor(rep_res, name_run, Bief_MNT_Iter, Bief_STA)                     
    # LPo : on appellera run_hydrau_flutor (script 03-PrepHydrauFlutor.R)
  }
  if (opt_mod=="mascaret")
  {
    Bief_Results_Iter =
      run_hydrau_mascaret(rep_res, name_run, Bief_MNT_Iter, Bief_STA)
  }
  
  if (is.null(Bief_Results_Iter)==T)
  {
    return(NULL)
  }else{
    
    ## create a variable of surface water level or head
    if (opt_h=="SL_m") {
      Bief_Results_Iter %<>% mutate(Z_M = SL_m) 
    } else Bief_Results_Iter %<>% mutate(Z_M = CHAR_m)                                      
    # LPo : CHAR_m = charge
    
    
    
    ## identity if left and right enpoints are under water
    Bief_Results_Iter_ = Bief_Results_Iter %>% dplyr::select(NSection, Z_M)
    section_underwater =                                                                    
      # LPo : la variable section_underwater stocke : 
      Bief_MNT %>%                                                                          
      # LPo : les altitudes du mnt groupées par segments de cours d'eau
      group_by(NSection) %>%  
      arrange(IPOSI) %>%                                                                    
      # LPo : arrange = ordonne les IPOSI (= numéros de XS)
      slice(c(1, n())) %>%                                                                  
      # LPo : slice = choisis les colonnes par leur position ordinale dans le tableau
      mutate(SIDE = ifelse(RIVE=="G", "ZLB", "ZRB")) %>%                                    
      # LPo : si rive gauche alors écrire "ZLB", si rive droite écrire "ZRB" dans SIDE
      left_join(Bief_Results_Iter_, by="NSection") %>%                                      
      # LPo : jointure à gauche de Bief_Results_Iter_ par la variable de jointure "Nsection" 
      dplyr::select(NSection, SIDE, Z, Z_M) %>%
      tidyr::spread(SIDE, Z) %>%                                                            
      # LPo : spread = ordonne par (SIDE, Z)
      mutate(DXG = Z_M > ZLB, DXD = Z_M > ZRB)                                              
    # LPo : Z_M = (?), ZLB = (?), ZRB = (?), DXG = (?), DXD = (?)
    
    
    
    do_enlarge_one = T                                                                      
    # LPo : Si on écrit TRUE, le programme élargit seulement le côté le plus bas des XS
    if (do_enlarge_one) ## enlarge only the lowest side
    {
      section_underwater %<>%
        mutate(DXG = ifelse(ZLB >= ZRB, FALSE, DXG),
               DXD = ifelse(ZLB >= ZRB, DXD, FALSE))
    }
    
    n_section = sum(section_underwater$DXD) + sum(section_underwater$DXG)                   
    # LPo : la variable n_section stocke les sections sous l'eau à gauche et à droite
    
    ## estimate the new widths of sections if it is needed
    if (n_section > 0)  
      # LPo : S'il existe des sections sous l'eau 
    {
      #FRED
      Profils_Iter %<>% 
        left_join(section_underwater, by="NSection") %>%                                    
        # LPo : faire une jointure à gauche dans la variable Profils_Iter des section_underwater avec la variable 
        # LPo : de jointure "NSection" 
        dplyr::mutate(XLG = pmin(pmax(perc_elarg * LG, min_elarg), max_elarg),
                      XLD = pmin(pmax(perc_elarg * LD, min_elarg), max_elarg),
                      LG  = LG + DXG * XLG, 
                      LD  = LD + DXD * XLD,
                      LG  = pmin(pmax(delta, delta * ceiling(LG / delta)), max_larg),              
                      # LPo : ceiling = retourne les éléments correspondant à LG/delta dans un vecteur numérique
                      LD  = pmin(pmax(delta, delta * ceiling(LD / delta)), max_larg),              
                      # LPo : ceiling = retourne les éléments correspondant à LD/delta dans un vecteur numérique
                      w_iter = ifelse((DXG|DXD), 1, 0)) %>%
        dplyr::select(NSection, LM, LD, LG, DIST, X, Y, ANGLE, SMOOTH, w_iter)
    }
    
    ## keep the distance values
    Bief_Results_Iter$DIST = Bief_STA$DIST                                                  
    # LPo : valeurs de distance cumulée
    
    return(list(res=Bief_Results_Iter, posi=Bief_Position_Iter,                             
                mnt=Bief_MNT_Iter, profils=Profils_Iter,
                sections_elargies = n_section))
    # LPo : stocke dans une liste les résultats de hauteurs d'eau "res", les positions des digues "posi", 
    # LPo : l'altitude "mnt" et les profils en travers "profils"
  }
}


