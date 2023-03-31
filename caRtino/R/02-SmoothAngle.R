#' Intersection between a cross section and its neighbours [-n_lines:+n_lines]
#' @noRd 
# LPo : intersect_trans_riv = fonction qui permet de détecter les croisements entre la XS et ses deux XS voisines.
# LPo : intersect_trans_riv s'applique sur des variables "temporaires" (insérées uniquement pour créer la fonction).

intersect_trans_riv = function(sp_lines, i)                                                   
{ 
  # LPo : i = les profils en travers ?  
  
  j = max(0, i-n_lines_cross) : min(nrow(sp_lines), i+n_lines_cross)                          
  
  j = j[j!=i]                                                                                 
  # LPo : ne récupère que les profils en travers dans j qui sont différentes de i 
  # LPo : j stocke les profils en travers voisins ?
  
  sp_lines_j = sp_lines[j, ]                                                                  
  # LPo : sp_lines_j = variable créée (dataframe) qui stocke les lignes j
  
  st_intersection(sp_lines[i, ], sp_lines_j)                                                  
  # LPo : st_intersection = créé la géométrie des croisements entre les lignes i de sp_lines et sp_lines_j 
  # LPo : donc entre un profil en travers et ses deux voisins
  
}


#' Intersection between all cross sections and their neighbours                               
#' @noRd 
# LPo : intersect_trans = fonction qui permet de détecter les croisements restants (entre un profil en travers 
# LPo : et tous les autres profils en travers)
# LPo : intersect_trans s'applique sur des variables "temporaires" (insérées uniquement pour créer la fonction).
# LPo : intersect_trans appelle intersect_trans_riv et l'applique sur la totalité de sp_lines

intersect_trans = function(sp_lines)
{   
  tmp_pt = lapply(1:nrow(sp_lines),                                                           
                  function(i) intersect_trans_riv(sp_lines, i))                               
  # LPo : applique la fonction intersect_trans_riv sur toutes les XS (=sur sp_lines)
  
  i_null = sapply(tmp_pt, nrow)                                                               
  # LPo : sapply = applique une fonction sur tous les éléments d'un vecteur/d'une matrice
  
  inter_pt = do.call(rbind, tmp_pt[i_null>0])                                                 
  # LPo : inter_pt = "NULL"
  # LPo : do.call = exécute une fonction
  }


#' Create new sections and check intersections
#' @noRd 
# LPo : Cette fonction s'applique sur des variables "temporaires" (insérées uniquement pour créer la fonction)                                                                                   
# LPo : Elle est appelée dans la fonction smooth_angle du script 02-SmoothAngle.R
# LPo : fun_intersect permet d'élargir les profils en travers et de les créer sous forme de LINESTRING avec la 
# LPo : fonction create_sfLines du script 01.PreHydrau.R
# LPo : ensuite fun_intersect permet de détecter les intersections des profils en travers avec la fonction intersect_trans 
# LPo : du script 02-SmoothAngle.R

fun_intersect = function(trans_riv_xy_)                                                       
{     
    l_trans_riv_xy_ =                                                                           
    lapply(1:nrow(trans_riv_xy_),                                                             
           function(i) matrix(c(trans_riv_xy_$XL[i], trans_riv_xy_$XR[i],                     
                                # LPo : élargissement côté après côté (gauche puis droite) du profil en travers en X
                                trans_riv_xy_$YL[i], trans_riv_xy_$YR[i]), nrow=2))           
                                # LPo : élargissement côté après côté (gauche puis droite) du profil en travers en Y 
  # LPo : stock des résultats dans la variable l_trans_riv_xy_
  
  trans_bief = create_sfLines(l_trans_riv_xy_)                                                
  # LPo : create_sf_Lines = permet de transformer les profils en travers (contenus dans l_trans_riv_xy_ = nouveaux 
  # LPo : profils en travers ?? ) en lignes spatiales (LINESTRING)
  
  st_geometry(trans_riv_xy_) = trans_bief                                                     
  # LPo : trans_riv_xy_ possède désormais la même géométrie que trans_bief (colonne geometry)
  
  trans_riv_xy_ %<>% dplyr::select(NSection)
  
  tmp = intersect_trans(trans_riv_xy_)                                                        
  # LPo : applique la fonction intersect_trans du script 02-SmoothAngle.R sur trans_riv_xy_ (XS groupées par segments 
  # LPo : de cours d'eau (=Nsection)). 
  # LPo : Cela détecte toutes les intersections entre XS. Résultats stockés dans le fichier "temporaire" tmp
  
  if(is.null(tmp))                                                                            
  # LPo : vérifie s'il y a des valeurs nulles
    
    
  { return(NULL) } else { names(tmp)[1:2] = c("ID_F", "ID_T"); return(tmp)}                   
  # LPo : s'il y a des valeurs nulles "TRUE" sera affiché (ainsi que les valeurs nulles)
  # LPo : s'il n'y en a pas on renomme les colonnes 1 et 2 de tmp respectivement ID_F et ID_T (ID_False et ID_True?) 
  
}


#' Apply rolling mean as a smoothing function for the angle                              
#' @noRd  
# LPo : fun_smooth_angle = fonction de moyenne pondérée = lisse les angles des profils en travers de trans_riv_xy_

fun_smooth_angle = function(trans_riv_xy_)                                                    
{
  trans_riv_xy_ %>%
    mutate(COS = cos(as.numeric(ANGLE)), SIN = sin(as.numeric(ANGLE))) %>%
    mutate(COS = roll_mean(COS, weights=c(1,3,1), fill=NA),   
           SIN = roll_mean(SIN, weights=c(1,3,1), fill=NA)) %>%                               
    mutate(ANGLE_ = atan2(SIN, COS)) %>%                                                      
    mutate(ANGLE_ = ifelse(is.na(ANGLE_), ANGLE, ANGLE_)) %>%
    dplyr::select(-COS, -SIN)                                                                 
  # LPo : pour la XS 1 : ((cos(theta0)*1) + (cos(theta1)*3) + (cos(theta2)*1))/5 ; etc pour chacune des XS
  # LPo : pour la XS 1 : ((sin(theta0)*1) + (sin(theta1)*3) + (sin(theta2)*1))/5 ; etc pour chacune des XS
  # LPo : atan2 = arc tangeante
  # LPo : si valeur manquante, alors récupére les valeurs contenues dans ANGLE (colonne de trans_riv_xy_ ?)
}


#' @title Lissage des angles des profils en travers s'ils se croisent 
#' @description FR: Cette fonction appartient au script 02-SmoothAngle.R.
#' 
#' Elle permet de lisser les angles des profils en travers pour éviter les intersections.
#' 
#' Elle fait appel aux fonctions :
#' 
#' - intersect_trans du script 02-SmoothAngle.R, qui elle-même permet d'appliquer intersect_trans_riv 
#' (fonction qui détecte les intersections entre les profils en travers voisins mais pour un unique profil 
#' en travers), tandis que intersect_trans s'applique sur tous les profils (grâce à "lapply") ;
#' 
#' - fun_smooth_angle du script 02-SmoothAngle.R, qui est une fonction appliquant une moyenne pondérée 
#' pour lisser les angles des profils en travers qui se croisent ;
#' 
#' - fun_intersect du script 02-SmoothAngle.R, qui permet la création des extrémités des nouveaux profils en travers
#'  et qui appelle create_sfLines du script 01-PreHydrau.R, qui permet de transformer les nouveaux profils en travers 
#'  en lignes spatiales (LINESTRING). Ensuite elle permet aussi la vérification des croisements (elle appelle 
#'  intersect_trans à nouveau, après lissage des angles par fun_smooth_angle) ;          
#'
#' ??? create_sfLines
#' 
#' Le champ SMOOTH de xs_station est un booléen qui prend la valeur "vraie" si le profil en travers a été lissé et 
#' la valeur "faux" s'il n'a pas été lissé.
#'     
#' ENG : Smooth angle of the cross section if they intersect
#'
#' @param xs_station la fonction s'applique sur le tableau de données des profils en travers avec leurs caractéristiques 
#' / spatial data of cross sections
#' 
#' @return la fonction retourne les profils en travers avec angles lissés / xs_station with smoothed angles

smooth_angle = function(xs_station)                                                           
{
  ## keep only the necessary cross sections                                                   
  # LPo : on conserve uniquement les XS qui se croisent ? 
  i_smooth = which(xs_station$SMOOTH > 0 | xs_station$w_iter > 0)                             
  # LPo : xs_station$SMOOTH colonne vide de la table attributaire des résultats (doit être supprimée à un moment), 
  # LPo : xs_station$w_iter colonne remplie soit par 0 soit par 1 dans la table attributaire du shapefile ..._CS
  # LPo : which = retourne la position ; lorsque SMOOTH est strictement positif et que w_iter est strictement positif
  
  tmp = lapply(i_smooth,                                                                      
               function(i)                                                                    
                 max(1, i-n_lines_cross) : min(nrow(xs_station), i+n_lines_cross))  
  # LPo : lapply = applique une fonction à tous les éléments d'une liste 
  # LPo : applique la fonction sur xs_station et stocke les résultats dans tmp
  # LPo : n_lines_cross = 10 ici (défini par l'utilisateur dans CARTINO_PARAM.R)
  
  if (is.null(dim(tmp))) tmp = do.call('c', tmp)                                              
  # LPo : dim = donne les dimensions d'une variable
  # LPo : si tmp est vide, application de la fonction 'c' sur tmp ?
  
  i_smooth = as.numeric(unique(tmp))                                                          
  # LPo : unique = ? 
  
  
  ## initialise
  xs_station$SMOOTH = T                                                                       
  # LPo : initialisation : champ SMOOTH = si T = le profil doit être ? lissé ? et inversement avec F
  
  xs_station_smooth = xs_station[i_smooth, ]                                                  
  # LPo : ensuite on copie i_smooth de xs_station (les profils qui doivent être? lissés) 
  # LPo : dans la variable xs_station_smooth
  
  
  ## check the intersections
  xs_station_ = xs_station_smooth %>% dplyr::select(NSection)                                 
  # LPo : stocke dans xs_station_, xs_station_smooth par profil en travers
  
  trans_pt_intersects = intersect_trans(xs_station_)                                          
  # LPo : stocke dans trans_pt_intersects l'application de la fonction intersect_trans sur xs_station_ (détection ?? 
  # LPo : des croisements entre tous les profils en travers et ses 10 voisins) 
  
  if(!is.null(trans_pt_intersects))                                                           
  # LPo : s'il n'y a pas de valeurs nulles
  {
    names(trans_pt_intersects)[1:2] = c("ID_F", "ID_T")                                       
    # LPo : renomme les colonnes 1 et 2 ID_FALSE et ID_TRUE (?)
    
    ## loop to smooth the angles
    iter = 0;                                                                                 
    # LPo : initialisation
    
    trans_bief_xy_loop =                                                                      
      # LPo : stocke dans la variable trans_bief_xy_loop
      
      xs_station_smooth %>%
      # LPo : les angles à lisser 
      
      st_set_geometry(NULL) %>% 
      
      mutate(ANGLE_ = ANGLE,
             XL = X + LG * cos(as.numeric(ANGLE)),                                                        
             YL = Y + LG * sin(as.numeric(ANGLE)),
             XR = X - LD * cos(as.numeric(ANGLE)),
             YR = Y - LD * sin(as.numeric(ANGLE)))
      # LPo : même calcul que dans le script 01-PreHydrau.R : calcul des extrémités des profils
    
    trans_pt_loop = trans_pt_intersects                                                       
    # LPo : trans_pt_intersects est transféré dans trans_pt_loop
    
    NSection_max = max(trans_bief_xy_loop$NSection)                                           
    # LPo : Dans Nsection_max on stocke le profil en travers le plus en aval de trans_bief_xy_loop    
    
    while (iter < num_iter_lissage)                                                           
      # LPo : tant que le nombre d'itérations est inférieur à num_iter_lissage
    {
      iter = iter + 1                                                                         
      # LPo : on passe à l'itération suivante
      
      j_seq_ = NULL                                                                           
      # LPo : initialisation de j_seq
      
      for (j in 1:nrow(trans_pt_loop))                                                        
      # LPo : boucle sur chacune des lignes de trans_pt_loop
      {
        j_seq = seq(max(0, trans_pt_loop$ID_F[j]-n_lines_smooth),                              
                    min(trans_pt_loop$ID_T[j]+n_lines_smooth, NSection_max))
        j_seq_ = c(j_seq_, j_seq)
        # LPo : j_seq stocke donc trans_pt_loop$ID_F - 1 (car n_lines_smooth = 1 dans CARTINO_PARAM.R)
        # LPo : ainsi que trans_pt_loop$ID_T + 1 (idem)
      }
      j_seq_ = unique(j_seq_)
      j_seq_ = which(trans_bief_xy_loop$NSection %in% j_seq_)                                 
      # LPo : retourne la position des j_seq qui matchent avec trans_bief_xy_loop$NSection 
      # LPo : (les profils en travers de trans_bief_xy_loop ?)
      
      
      ## smooth the angles and create new endpoints for the cross sections
      trans_bief_xy_loop = fun_smooth_angle(trans_bief_xy_loop)                               
      # LPo : applique la fonction de moyenne pondérée fun_smooth_angle (lissage des angles) sur trans_bief_xy_loop
      
      trans_bief_xy_loop[j_seq_, ] %<>%                                                       
        
        mutate(ANGLE = ANGLE_,
               XL = X + LG * cos(as.numeric(ANGLE)),
               YL = Y + LG * sin(as.numeric(ANGLE)),
               XR = X - LD * cos(as.numeric(ANGLE)),
               YR = Y - LD * sin(as.numeric(ANGLE)))
      # LPo : même calcul que dans le script 01-PreHydrau.R : 
      # LPo : calcul des extrémités des nouveaux profils en travers (qui se croisaient et qui ont été lissés 
      # LPo : avec la fonction de moyenne pondérée fun_smooth_angle)
      
      
      ## check again the intersection
      trans_pt_loop = fun_intersect(trans_bief_xy_loop[j_seq_, ])                             
      # LPo : applique fun_intersect sur les profils qui se croisent :
      # LPo : création des extrémités des nouveaux profils, création des LINESTRING, 
      # LPo : et vérification des derniers croisements
      
      if(is.null(trans_pt_loop)) break                                                        
      # LPo : si trans_pt_loop (nouveaux profils en travers lissés) est NULL , arrêt de la boucle 
    }
    
    cat(paste0(Sys.time()," -- Number of iteration for smoothing: ", iter), "\n")                         
    # LPo : le programme retourne le numéro du tour de lissage des angles
    
    
    ## get the new geometries   
    l_trans_riv_xy_ =                                                                              
      lapply(1:nrow(trans_bief_xy_loop), 
             function(i) matrix(c(trans_bief_xy_loop$XL[i], trans_bief_xy_loop$XR[i],         
                                  trans_bief_xy_loop$YL[i], trans_bief_xy_loop$YR[i]),
                                nrow=2))
    # LPo : création des extrémités sur trans_bief_xy_loop
    # LPo : résultats stockés dans l_trans_riv_xy_
    
    st_geometry(xs_station_smooth) = create_sfLines(l_trans_riv_xy_)                          
    # LPo : la géométrie de xs_station_smooth devient la même que celle de l_trans_riv_xy_ sur laquelle on
    # LPo : applique la fonction create_sfLines du script 01-Pre-Hydrau.R (donc LINESTRING)
    
    
    ## get the new angles
    xs_station_smooth$ANGLE = trans_bief_xy_loop$ANGLE                                        
    # LPo : copie les nouveaux angles lissés dans xs_station_smooth, dans la colonne ANGLE
    
    
    ## check if there are unresolved intersections
    if(!is.null(trans_pt_loop))                                                               
    # LPo : vérifie s'il existe encore des intersections
    {
      xs_station_smooth %<>%
        mutate(SMOOTH = ifelse(NSection %in% trans_pt_loop$ID_F, TRUE, FALSE))                
      # LPo : vérifie si les valeurs sont les mêmes entre Nsection et trans_pt_loop$ID_F 
      # LPo : (et le résultat "true" ou "false" est copié dans xs_station_smooth)
    }
    # ## graphique
    # if (do_graph) graph_profil_liss(trans_bief_xy)                                          
  }
  
  xs_station[i_smooth, ] = xs_station_smooth                                                  
  # LPo : finalisation
  

  return(xs_station) 
  # LPo : les résultats sont stockés dans xs_station
}

