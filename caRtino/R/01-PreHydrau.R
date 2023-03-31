#' Create a sf object lines from a list of points                                    
#' @noRd                                                                             
# LPo : Transforme des points en LINESTRING (d'abord sous forme de liste puis en un "objet sfc")

create_sfLines = function(trans_bief_xy)
  # LPo : trans_bief_xy contient les points des extrémités des profils (un bief après l'autre (boucle)) 
  # LPo : Suppression des données stockées précédemment à chaque tour de la boucle
  # LPo : trans_bief_xy est appelé dans 01-PreHydrau.R par la fonction create_sf_riv_sections du script 01-PreHydrau.R
  # LPo : trans_bief_xy est appelé dans 02-SmoothAngle.R à plusieurs reprises : dans la fonction fun_intersect  
  # LPo : et dans la fonction smooth_angle 
  # LPo : et trans_bief_xy est appelé dans le script 09-ExportGraphs.R pour le graph du profil en long
{    

  list_lines = lapply(trans_bief_xy, function(x) st_linestring(x))                   
  # LPo : lapply = Applique la fonction st_linestring (conversion en LINESTRING) pour les X-Y , XR-YR, XL-YL de xs_station
  # LPo : trans_bief_xy n'est qu'une variable intermédiaire mais au final cela va s'appliquer sur xs_station
  
  trans_bief_sf = st_sfc(list_lines)                                                 
  # LPo : trans_bief_sf est un objet sf ayant pour géométrie les LINESTRING issues de list_lines
  
  return(trans_bief_sf)                                                               
}




#' Create river cross sections                                                       
#' @noRd
# LPo : Fonction de création des points centraux des profils en travers (utilisée au début de CARTINO1D)
# LPo : Plus bas dans le script 01-PreHydrau.R, dans la fonction  get_disch_stream,
# LPo : on appelle transect_bief_pre, sur les variables du projet : sur shp_reach, xs_space_ini, 
# LPo : et le résultat est stocké dans xs_station (= stream_disch)
#
# LPO : Dans la troisième boucle de CARTINO.R est appelé get_disch_stream : 
# LPo : agit sur ras_shyrex_mask, df_reach, shp_reach, et le résultat esr stocké dans stream_disch

# LPo : transect_bief_pre est également appelé dans la fonction cartino_bief du script 99-RunReach.R : 
# LPo : agit sur shp_reach, xs_space_ini, et le résultat est stocké dans xs_station

# LPo : LIGNE D'ORIGINE : transect_bief_pre = function(shp_reach, pas=75)
# LPo : Paramètre "pas" déplacé dans CARTINO_PARAM.R 
transect_bief_pre = function(shp_reach, pas)
{
  # LPo : shp_reach = Contient un seul bief à la fois (objet sf)
  # LPo : shp_reach contient donc (pour un bief à la fois) les colonnes : ID, NBasVers, geom et Nbief
  
  density = 1/pas                                                                    
  # LPo : Au départ les profils en travers sont créés à pas constant
  
  l = as.numeric(st_length(shp_reach))                                               
  # LPo : l = Longueur du bief prolongé 
  
  n = round(density * l)                                                             
  # LPo : n = densité * longueur 
  # LPo : n = nombre de profils en travers sur la longueur totale du réseau
  # LPo : round = permet d'arrondir n (par défaut à un entier)
  
  
  l_posi = data.frame(DIST=round((1:n - 0.5)/n * l, 2))                             
  # LPo : Création des points où positionner les profils en travers sur le réseau
  # LPo : l_posi est un data.frame
  # LPo : dans la colonne DIST (distance cumulée)
  # LPo : Division (du numéro de chaque profil - 0,5) par n (nombre total de profils sur le bief), 
  # LPo : ensuite on multiplie chaque résultat par l
  # LPo : "round(... , 2)" = Permet d'arrondir à 2 chiffres après la virgule
  
  l_posi$ECART = round(1/n * l, 2)                                                   
  # LPo : Calcul de l'écart entre chacun des profil en travers ("ECART")
  # LPo : "round(... , 2)" = Permet d'arrondir à 2 chiffres après la virgule
  
  xs_sta_pt = st_line_sample(shp_reach, type="regular", density=density)             
  # LPo : st_line_sample = Positionnement des points de manière réguliere sur le réseau hydrographique  
  # LPo : (= sur shp_reach) avec une densité = 1/pas
  
  xs_sta_pt %<>% st_sf() %>% st_cast("POINT", warn=F)                                
  # LPo : st_cast = Transforme en points (auparavant xs_sta_pt était un MULTIPOINT)
  # LPo : st_sf = Transforme en objet sf
  
  # LPo:
  #l_posi_ecarts = calculating_distances(xs_sta_pt, shp_reach, i_reach, num_bv, num_reach)
  
  xs_sta_pt %<>% bind_cols(l_posi)                                                   
  # LPo : Copie-colle l_posi à xs_sta_pt (donc rajout des colonnes distance cumulée et écarts dans xs_sta_pt)
  
  tmp_xy = xs_sta_pt %>% st_coordinates() %>% data.frame()                           
  # LPo : Les coordonnées des points centraux des profils en travers sont maintenant stockés dans tmp_xy (dataframe)
  
  st_geometry(xs_sta_pt) = NULL                                                      
  # LPo : xs_sta_pt n'a plus de géométrie
  
  xs_sta_pt %<>% bind_cols(tmp_xy)                                                   
  # LPo : xs_sta_pt devient un dataframe contenant les coordonnées des points centraux des profils 
  # LPo : (colonnes "X" et "Y") + colonnes "DIST" et "ECART"
  
  return(xs_sta_pt)
  
}



#' @title Calcul de l'angle des profils en travers par rapport au cours d'eau
#' @description FR : Cette fonction est issue du script 01-PreHydrau.R. 
#' 
#' Elle permet de définir l'angle pour un profil en travers (en radians).
#' 
#' get_section_angle s'applique sur des variables "temporaires" (insérées uniquement pour créer la fonction).
#' Dans le script 01-PreHydrau.R (ce script), la fonction suivante (get_river_angle), appelle get_section_angle 
#' et permet son application sur les variables du projet.
#' De plus, get_section_angle ne s'applique que sur un seul objet, tandis que get_river_angle permet d'effectuer 
#' une boucle pour que chacun des profils en travers soit traité.
#' 
#' Les fonctions du script 02-SmoothAngle.R (script suivant) permettent de lisser les angles des profils en travers 
#' qui se croisent, comme ceux situés notamment au niveau des méandres du cours d'eau.
#' 
#' La fonction get_section_angle est la deuxième étape de construction des profils en travers le long du réseau. 
#' (Etape 1 : Création de la variable contenant les points centraux des profils en travers avec la fonction 
#' transect_bief_pre du script 01-PreHydrau.R).
#' 
#' La méthode de définition des angles est la suivante : 
#' 
#' - On considère la variable contenant les coordonnées des points centraux des profils en travers, créé avec la 
#' fonction transect_bief_pre du script 01-PreHydrau.R :
#' 
#' - On récupère les points de construction du cours d'eau, précédant et suivant ce point central ;
#' 
#' - Définition de l'angle grâce à arc tangente ;
#' 
#' - Les angles sont finalement stockés dans le vecteur thetaT.
#' 
#' Cette fonction est donc l'étape suivante à la fonction transect_bief_pre, et l'étape précédente à la fonction get_river_angle.
#'     
#' ENG : Estimate the angle of a cross section relative to the river.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
#' 
#' @param riv_xy la fonction s'applique sur le tableau de données comprenant les coordonnées des points de construction 
#' du cours d'eau / dataframe with points of river XY
#' @param xyi la fonction s'applique sur le tableau de données comprenant les coordonnées des points centraux des profils 
#' en travers / dataframe with points of the center of the cross section XY
#' @return la fonction retourne l'angle perpendiculaire au cours d'eau pour la construction des profils en travers 
#' / angle of the section

get_section_angle = function(riv_xy, xyi)                                            
{ 
  ## where is the point among stream points
  # dist_min = 100000                                                                  
  # LPo : On définit une distance minimale
  
  ipt = which(riv_xy$X == xyi$X & riv_xy$Y == xyi$Y)
  # LPo : riv_xy = Points de construction du cours d'eau (correspond à df_stream)
  # LPo : xyi = Points centraux des XS (correspond à xs_station)
  # LPO : ipt est un vecteur qui contient la position des points de construction du cours d'eau qui sont 
  # LPo : situés aux mêmes emplacements que les points centraux des XS (normalement, au départ = 0)
  # LPo : Chaque point est étudié un par un (grâce à sapply)
  # LPo : Donc ipt ne contiendra jamais plus qu'un seul point
  
  # LPo :
  #plot(riv_xy$X,riv_xy$Y)
  #points(xyi$X,xyi$Y, col ="red")
  
  if(length(ipt) != 1)                                                               
    # LPo : Si ipt est différent de 1 (=vide)
  {
    dist_riv = cbind(riv_xy$X - xyi$X, riv_xy$Y - xyi$Y)
    dist_riv = dist_riv[, 1]**2 + dist_riv[, 2]**2                                   
    # LPo : dist_riv = Distance en X (première colonne) et en Y (deuxième colonne)
    # LPo : entre le point de construction du cours d'eau et le point central du profil
    # LPo : "**" = exposant
    # LPo : Ensuite on met ces distances au carré et on les somme (utilisation de Pythagore)
    
    i_riv = which.min(dist_riv)
    # LPo : On retient l'index de la valeur minimum de cette longueur au carré 
    # LPo : Donc le point du cours d'eau le plus près du point central du profil concerné
    
    if(sqrt(dist_riv[i_riv]) < dist_min) ipt = i_riv                                 
    # LPo : Si la racine carrée de cette distance au carré
    # LPo : est inférieure à la distance minimale fixée au départ par l'utilisateur
    # LPo : alors on stocke dans ipt l'index du point de construction le plus proche 
    # LPo : du point central du profil concerné
  }
  
  if(length(ipt) != 1) return("NOT IN THE RIVER")
  # LPo : Si désormais ipt n'est pas égal à 1 ce n'est pas normal
  # LPo : car get_section_angle doit avoir trouvé le point le plus proche
  
  ## estimate the perpendicular section
  ## use the river points after and before the closest one
  ia = max(ipt - 1, 1)                                                               
  # LPo : ia index qui précède celui stocké dans ipt  
  
  ib = min(ipt + 1, nrow(riv_xy))                                                    
  # LPo : ib index qui suit celui stocké dans ipt
  
  d = riv_xy[c(ia, ib), ]                                                            
  # LPo : d = Stock des coordonnées des deux points de construction du cours d'eau  
  # LPo : précédant et suivant celui contenu dans ipt (donc précédant et suivant le point central du profil concerné)
  
  theta = atan2(d$Y[1] - d$Y[2], d$X[1] - d$X[2])                                    
  # LPo : atan2 = Arc tangente
  
  thetaT = theta - pi/2
  # LPo : Enlève 90° 
  
  return(thetaT)                                                                     
  # LPo : Retourne les angles nécessaires à la construction des profils en travers
  # LPo : Les profils en travers sont construits entre deux points de construction du cours d'eau
}                                                                                    




#' @title Définir l'angle des profils en travers par rapport au cours d'eau : application de la fonction get_section_angle 
#' à tous les profils en travers
#' @description FR : Cette fonction est issue du script 01-PreHydrau.R. 
#'
#' Elle permet de définir l'angle pour chacun des profils en travers (en radians).
#' get_river_angle appelle la fonction get_section_angle du script 01-PreHydrau.R, qui elle, s'applique sur des variables 
#' "temporaires" (insérées uniquement pour créer la fonction), et ainsi permet son application sur les variables du projet.
#' De plus, get_section_angle ne s'applique que sur un seul objet, tandis que get_river_angle permet d'effectuer une boucle 
#' pour que chacun des profils en travers soit traité.
#' 
#' Les fonctions du script 02-SmoothAngle.R (script suivant) permettent de lisser les angles des profils en travers qui se 
#' croisent, comme ceux situés notamment au niveau des méandres du cours d'eau.
#' 
#' Les résultats sont stockés dans le vecteur vec_theta.
#' 
#' ENG : Estimate the angle of the cross sections relative to the river : application of the 
#' get_section_angle function for all cross sections.
#' 
#' @param df_reach la fonction s'applique sur le tableau de données comprenant les coordonnées des points de construction 
#' du cours d'eau / dataframe with points of river XY
#' @param xs_station la fonction s'applique sur le tableau de données comprenant les coordonnées des points centraux des 
#' profils en travers / dataframe with points XY of the center of the cross sections
#' @return la fonction retourne un vecteur contenant les angles nécessaires à la construction des profils en travers 
#' / vector of angles

get_river_angle = function(df_reach, xs_station)                                     
{

  vec_theta =                                                                       
    sapply(1:nrow(xs_station),                                                       
           function(i) get_section_angle(df_reach[, c("X", "Y")],                       
                                         xs_station[i, c("X", "Y")]))
  # LPo : sapply = Applique une fonction sur tous les éléments d'un vecteur/dataframe
  
  return(vec_theta) 
}                                                                                 



#' @title Création des profils en travers en ajoutant une distance de chaque côté du point central
#' @description FR : Cette fonction est issue du script 01-PreHydrau.R. 
#' 
#' A partir :
#' 
#' - du tableau de données contenant les coordonnées des points centraux des profils en travers 
#' (fonction transect_bief_pre du script 01-PreHydrau.R) ;
#'
#' - et du vecteur contenant les angles en radians (fonctions get_section_angle et get_river_angle du script 01-PreHydrau.R) :
#'
#' create_sf_riv_sections permet de calculer le point de l'extrémité de chaque côté du profil en travers. 
#' Elle y parvient en ajoutant/soustrayant les distances (LG x cos(ANGLE)) ou (LD x sin(ANGLE)) de chaque côté du point 
#' central en X et en Y (LG = LD = marge_ini) .
#' 
#' (marge_ini est normalement définie par l'utilisateur dans CARTINO_PARAM.R.
#' Cependant la fonction get_disch_stream du script 01-PreHydrau.R permet à nouveau à l'utilisateur de définir marge_ini.
#' C'est donc la fonction get_disch_stream qui prévaut pour la définition de marge_ini. 
#' On a aussi marge = marge_ini - xs_width_ini dans le script 99.RunReach.R.)
#' 
#' Ensuite create_sf_riv_sections appelle la fonction create_sfLines (du script 01-PreHydrau.R également) qui elle, 
#' permet de créer une ligne spatiale LINESTRING ("objet sfc").
#'
#' create_sf_riv_sections est donc la troisième étape pour la création des profils en travers (on obtient les extrémités 
#' des profils en travers élargis).
#' 
#' create_sfLines est par conséquent la quatrième étape pour la création des profils en travers (on créé les profils en 
#' travers en reliant les extrémités au point central sous forme de lignes spatiales LINESTRING).  
#' 
#' ENG : Create river cross sections spatial object, by adding a distance on each side of the central point.
#' 
#' @param xs_station la fonction s'applique au départ sur le tableau de données contenant les coordonnées des points 
#' centraux des profils en travers / dataframe with points XY of the center of the cross sections 
#' @return la fonction retourne xs_station qui correspond aux profils en travers en tant que LINESTRING ("objet sf"), 
#' avec leurs caractéristiques / river cross sections as spatial lines with features

create_sf_riv_sections = function(xs_station)                                        
{       
  tmp =                                                                                
    xs_station %>%                                                                   
    dplyr::mutate(XL = X + LG * cos(as.numeric(ANGLE)),                                                 
                  # LPo : XL = le X de l'extrémité du segment LG (prolongement à gauche)  
                  
                  YL = Y + LG * sin(as.numeric(ANGLE)),                                                 
                  # LPo : YL = le Y de l'extrémité du segment LG (prolongement à gauche)
                  
                  XR = X - LD * cos(as.numeric(ANGLE)),                                                 
                  # LPo : XR = le X de l'extrémité du segment LD (prolongement à droite)
                  
                  YR = Y - LD * sin(as.numeric(ANGLE)))                                                 
  # LPo : YR = le Y de l'extrémité du segment LD (prolongement à droite)
  # LPo : mutate = Ajoute une ou plusieurs colonnes dans la variable tmp (dataframe) 
  # LPo : (ici 4 colonnes : XL, YL, XR, YR), 
  
  
  tmp_ =                                                                            
    lapply(1:nrow(tmp),
           function(i) matrix(c(tmp$XL[i], tmp$XR[i],                                     
                                tmp$YL[i], tmp$YR[i]),
                              nrow=2))
  # LPo : Création d'une liste de matrices tmp_ qui stocke les coordonnées XY de chaque extrémité des profils 
  
  sf_tmp = create_sfLines(tmp_)                                                      
  # LPo : Appelle la fonction create_sfLines du script 01_PreHydrau.R 
  # LPo : Création des profils sous forme de LINESTRING (objet sf) 
  
  st_geometry(xs_station) = sf_tmp                                                   
  # LPo : Stocke les résultats dans xs_station. 
  # LPo : xs_station est un objet sf dont la géométrie devient celle de sf_tmp c'est-à-dire des LINESTRING
  
  return(xs_station)                                                                 
  # LPo : Fin de la fonction et retour des résultats
}



#' @title Calcul de l'écart entre chaque profil en travers et de la distance cumulée
#' @description FR : Cette fonction est issue du script 01-PreHydrau.R. 
#' Elle est appelée par la fonction create_sf_riv_sections.
#' Elle permet, une fois que les profils sont créés en tant que LINESTRING avec LG et LD,
#' d'obtenir la valeur des ECART et DISTance cumulée.
#' ENG : Calculating distances between two sections and cumsum the results
#' @param xs_station les profils en travers / sections
#' @param stream_sf le réseau / stream network
#' @return la fonction retourne les ecarts entre chaque profil et la distance cumulée à chaque profil / return distances between sections and cumulative distance
calculating_distances = function(xs_sf, str_net_sf, i_reach, num_bv, num_reach)
{
  st_crs(xs_sf)=crs_dem
  st_crs(str_net_sf)=crs_dem
  
  xs_sf = st_cast(xs_sf, "LINESTRING")
  
  stream_ = st_sf(data.frame(str_net_sf[,]))
  stream_to_split = stream_[i_reach,]
  xs_sf = st_sf(data.frame(xs_sf))
  
  
  #split_= st_sf(st_split(stream_to_split, xs_sf$geometry))
  split_= st_sf(st_split(stream_to_split, xs_sf))
  extract_split=st_collection_extract(split_, "LINESTRING")
  
  
  extract_split %>% dplyr::mutate("ECART" = NA,
                                  "NSection" = 1:n(),
                                  "NBief" = num_reach,
                                  "NBasVers" = num_bv)
  extract_split = st_sf(data.frame(extract_split))
  
  
  
  extract_split$ECART = as.numeric(st_length(extract_split$geom[,]))
  
  extract_split = extract_split[-nrow(extract_split), ]
  
  extract_split = extract_split %>% dplyr::mutate("DIST" = cumsum(extract_split$ECART))
  sf_ecarts = extract_split
  
    return(sf_ecarts)
}  







#' @title Extraction des valeurs de débit le long du réseau hydrographique
#' @description FR : Cette fonction est issue du script 01-PreHydrau.R. 
#' 
#' get_disch_stream est appelée dans CARTINO.R
#' Les résultats seront stockés dans la variable stream_disch (= xs_station).
#' 
#' get_disch_stream appelle tout d'abord la fonction transect_bief_pre du script 01-PreHydrau.R, 
#' soit la fonction de création des profils en travers, (en utilisant la variable shp_reach = le réseau hydro).
#' 
#' Entre chaque profil en travers, l'espacement est défini par la valeur que l'utilisateur donne à xs_space_ini 
#' Deux possibilités pour définir l'espacement : dans CARTINO_PARAM.R et dans cette fonction
#' -> Ligne de code mise en commentaire (LPo), l'utilisateur ne peut maintenant que le définir dans CARTINO_PARAM.R.
#' 
#' get_disch_stream permet de créer dans xs_station les colonnes des distances gauche "LG" et droite "LD", 
#' définies par la valeur que l'utilisateur donne à la variable marge_ini (car LG = marge_ini, LD = marge_ini)) ;
#' 
#' 
#' Ensuite, get_disch_stream appelle la fonction get_river_angle afin de stocker dans xs_station les angles entre 
#' df_reach et les profils en travers ;
#' 
#' La fonction get_disch_stream appelle la fonction create_sf_riv_sections, qui permet de rejoindre les deux extrémités 
#' du profil en travers sous forme de LINESTRING (création de la colonne "geometry" de l'objet sf) ;  
#' 
#' get_disch_stream permet alors d'extraire les valeurs de débits max à chaque point central de profil en travers 
#' (avec "$extract"); 
#' 
#' Enfin, get_disch_stream permet d'interpoler les valeurs manquantes de débits avec "approx". 
#' Les résultats sont stockés dans la colonne Q du data.frame xs_station.
#' 
#' La fonction prend fin et les résultats sont stockés dans la variable stream_disch du script CARTINO.R.
#' 
#' 
#' ENG : Extract discharge values along the stream.
#' 
#' @param ras_shyrex_mask la fonction s'applique sur le raster de débits / raster of discharge values
#' @param df_reach la fonction s'applique sur le tableau de données avec les coordonnées des points de construction du cours d'eau 
#' (pour l'angle) / dataframe of river points XY
#' @param shp_reach la fonction s'applique sur le réseau hydrographique (pour créer les profils au départ) / reach network
#' @return la fonction retourne les valeurs de débit à chaque profil en travers / discharge values for each point of the river 

get_disch_stream = function(ras_shyrex_mask, df_reach, shp_reach)                     
{
  ## create cross section along the stream
  #xs_space_ini = 2; marge_ini = 10                                                    
  # LPo : C'est ici que l'utilisateur peut redéfinir xs_space_ini et marge_ini
  xs_station = transect_bief_pre(shp_reach, min(xs_space_ini,0.4*st_length(shp_reach)))
  # LPo : Appelle la fonction transect_bief_pre qui créé les points centraux des profils en travers sur shp_reach
  # LPo : Avec un pas d'espace défini dans CARTINO_PARAM.R ou qui peut être défini à nouveau juste au-dessus
  # LPo : En sortie xs_station contient le X et le Y du point central du profil, l'écart entre les profils 
  # LPo : (même écart entre chaque profil) et la distance cumulée 
  

    # Valeur fixe ajoutée car on ne peut pas calculer le débit max sur une trop grosse largeur
  cat("# Maximum 25m de part et d'autre => Paramètre?\n")
  xs_station %<>% dplyr::mutate(LG = min(marge_ini,25), LD = min(marge_ini,25))
  # LPo : mutate = Permet de rajouter les deux colonnes LG et LD à xs_station
  # LPo : LG et LD (élargissement à gauche et à droite) sont définis par marge_ini dans CARTINO_PARAM.R et/ou au-dessus
  
  
  ## create cross sections spatial lines perpendicular to the stream geometry
  xs_station$ANGLE = get_river_angle(df_reach, xs_station)
  # LPo : Appelle la fonction get_river_angle, qui elle-même appelle get_section_angle pour obtenir les angles absolus
  # LPo : Les angles absolus (entre xs_station et df_reach) sont stockés dans la table xs_station dans la colonne ANGLE
  
  xs_station = create_sf_riv_sections(xs_station)                                     
  # LPo : Appelle la fonction create_sf_riv_sections pour transformer xs_station 
  # LPo : (point centraux des profils en travers) en lignes (LINESTRING objet sf) :
  # LPo : Elargissement de chaque côté du point
  
  xs_station %<>% st_set_crs(crs_dem)                                                 
  # LPo : SCR de xs_station = SCR de crs_dem, lui-même défini dans CARTINO_PARAM.R 
  

  if(opt_read_raster == "velox")
  {
    ras_shyrex_mask_vx = velox(ras_shyrex_mask)
    xs_station$Q = 
      as.numeric(ras_shyrex_mask_vx$extract(xs_station,                                  
                                            fun = function(x) max(x, na.rm=T)))         
    # LPo : xs_station$Q = Création de la colonne débit Q dans xs_station
    # LPo : Applique la fonction max pour récupérer le débit max extrait à chaque profil en travers 
  }
  else if(opt_read_raster == "GRASS")
  {}
  else
  {
    ras_shyrex_mask_vx = ras_shyrex_mask
    xs_station$Q = raster::extract(ras_shyrex_mask_vx, xs_station, fun = max, na.rm =T)
  }
  
  ## deal with missing values by using a min value and interpolating values
  # LPo : Remplace les valeurs manquantes en utilisant Qmin et en interpolant
  Qmin = min(xs_station$Q[xs_station$Q>0]) 
  # LPo : Calcul du débit minimum (entre tous les profils en travers du bief)
  xs_station %<>% mutate(Q = pmax(Q, Qmin))                                           
  tmp = xs_station %>% filter(Q>=0)                                                   
  # LPo : tmp = Fichier "temporaire" qui stocke seulement les profils où le débit est positif ou nul
  
  xs_station %<>% mutate(Q = approx(tmp$DIST, tmp$Q, DIST, rule=2)$y)                 
  # LPo : approx = Fonction d'interpolation de R
  # LPo : rule=2 : Signifie que s'il y a des NA, alors on prend la valeur la plus proche
  
  
  ## check for discharge variation from upstream to downstream  
  for (i in 2:nrow(xs_station))                                                       
    # LPo : Pour toutes les lignes de xs_station (à partir de la deuxième)
  {
    xs_station$Q[i] = max(xs_station$Q[i], xs_station$Q[i-1])                         
    # LPo : Pour le profil en travers i, on stocke dans la colonne Q le max 
    # LPo : entre le débit au profil i, et le débit au profil précédent (i-1)
  }
  
  return(xs_station)
}



#' @title Extraction des valeurs d'élévation pour chaque profils en travers
#' @description FR : Cette fonction est issue du script 01-PreHydrau.R.
#' 
#' get_elevation permet de créer des points le long de chaque profil en travers. Cela permettra de récupérer la bathymétrie 
#' du cours d'eau pour chacun des profils en travers.
#' Pour faire ceci, on utilise st_line_sample (package sf) qui créé un échantillon de points à densité régulière définie par 
#' l'utilisateur (density = 1/delta avec delta défini dans CARTINO_PARAM.R). 
#' Ici 100 points par profil en travers.
#' 
#' Les profils en travers sont également numérotés de l'amont à l'aval : résultats stockés dans le champ IPOSI de xs_sta_pt.
#' 
#' Ensuite, au niveau de chaque point de chaque profil en travers, on extrait l'altitude. Les résultats sont stockés dans le 
#' champ Z de xs_sta_pt.
#' Voir aide "VeloxRaster_extract_points" si nécessaire.
#' 
#' La variable NSection représente le numéro des profils en travers.
#' 
#' ENG : Get elevation values for each cross sections.  
#' 
#' @param ras_mnt_vx la fonction s'applique sur le raster MNT (valeurs d'altitudes à chaque pixel) lu par velox 
#' / raster velox of elevation values
#' @param xs_station la fonction s'applique sur le tableau de données des profils en travers en tant que LINESTRING 
#' avec leurs caractéristiques / river cross sections as spatial lines with features
#' 
#' @return la fonction retourne les valeurs d'altitude le long de chaque profil en travers 
#' / elevation values along each cross sections

get_elevation = function(ras_mnt_vx, xs_station)                                      
{
  ## get coordinates and distance of the sampling points
  density = 1/delta                                                                   
  # LPo : delta est défini dans le script CARTINO_PARAM.R
  
  l = st_length(xs_station)                                                           
  # LPo : l = Largeur de chaque profil en travers
  # LPo : st_length est issu du package sf et permet de calculer la longeur d'une entité 
  # LPo : en se basant sur sa géométrie 
  
  n = round(rep(density, length.out = length(l)) * l)                                 
  # LPo : round = Arrondi
  # LPo : n est un data.frame qui a un nombre de lignes = au nombre de profil du bief
  # LPo : Il n'y a qu'une seule colonne (sans nom) et ici (sur l'aude) toutes les valeurs valent 100 m
  
  
  l_posi = lapply(seq_along(l),                                                       
                  function(i) data.frame(POSI=round((1:n[i] - 0.5)/n[i] * l[i], 2)))  
  # LPo : seq_along = signifie qu'une séquence, qui possède la longueur de l'objet (ici l) va être appliquée
  # LPO : lapply = s'applique sur tous les élements
  
  l_posi %<>% bind_rows()   
  # LPo : l_posi n'est plus une liste de dataframes
  # LPo : l_posi devient un dataframe
  
  l_posi = as.data.frame(as.numeric(l_posi$POSI))
  colnames(l_posi)= "POSI"
  # LPo : On nomme la colonne de l_posi "POSI"
  # LPo : POSI contient uniquement des nombres impairs (classés dans ordre croissant de 1 à 199) puis on répète
  
  
  # xs_sta_pt =
  #   st_line_sample(xs_station, type="regular", density=1/delta) %>%
  #   # LPo : st_line_sample = création de points sur les profils en travers (= sur xs_station) de manière régulière
  #   # LPo : avec une densité = 1/delta
  #   st_sf() %>%
  #   mutate(NSection=xs_station$NSection, NBief=xs_station$NBief, LG=xs_station$LG) %>%
  #   st_cast("POINT", warn=F) %>%
  #   # LPo : st_cast convertit la géométrie, ici en points
  #   group_by(NSection) #%>%
  # # LPo : groupe les points des profils en travers par profil en travers
  # 
  # xs_sta_pt = data.frame(xs_sta_pt, POSI = l_posi$POSI)
  # 
  # liste_sections_pts =  list()
  # 
  # for (i_pt in 1:nrow(xs_sta_pt))
  # {
  #   # indice_section = xs_sta_pt$NSection[i_pt]
  #   section_ = xs_sta_pt[i_pt,]
  #   #indice_section = xs_sta_pt[i_pt]
  #   
  #   
  #   section_ = data.frame(section_, 
  #                          "IPOSI" = i_pt, 
  #                          "POSI_C" = section_$POSI - section_$LG, 
  #                          "RIVE" = ifelse(section_$POSI<section_$LG, "G", "D"))
  #   
  #   
  #   liste_sections_pts[[i_pt]] = section_[,]
  #   
  # }
  # 
  # xs_sta_pt = st_sf(do.call(rbind, liste_sections_pts))
  
  
  # MORCEAU DE CODE QUI NE FONCTIONNE PAS AVEC LA MISE A JOUR DU PACKAGE DPLYR (mais qui fonctionnait parfaitement avant)
  # FINALEMENT QUI FONCTIONNE EN METTANT dplyr:: devant chaque commande dplyr
  
  if(mode_CARTINO1D == "automatique")
  {
    xs_sta_pt =
      st_line_sample(xs_station, type="regular", density=1/delta) %>%
      # LPo : st_line_sample = création de points sur les profils en travers (= sur xs_station) de manière régulière
      # LPo : avec une densité = 1/delta
      st_sf() %>% dplyr::mutate(NSection=xs_station$NSection, LG=xs_station$LG) %>% 
      st_cast("POINT", warn=F) %>%
      # LPo : st_cast convertit la géométrie, ici en points (objet sf)
      dplyr::group_by(NSection) %>%
      # LPo : Groupe les points des profils en travers par profil en travers
      bind_cols(l_posi) %>%
      # LPo : On ajoute la colonne "POSI" (nbres impairs) à xs_sta_pt
      dplyr::mutate(IPOSI=1:n(),
                    # LPo : IPOSI = numérote les points des profils en travers de la rive gauche à la rive droite
                    POSI_C = POSI - LG,
                    # LPo : POSI_C permet de définir le point central comme = 0. Les points qui vont jusqu'à la rive gauche
                    # LPo : sont négatifs tandis que les points qui vont jusqu'en rive droite sont positifs.
                    RIVE = ifelse(POSI<LG, "G", "D")) %>%
      # LPo : RIVE : colonne pour préciser de quelle rive il s'agit :
      # LPo : si POSI est inférieure à LG, alors il s'agit de la rive gauche,
      # LPo : sinon il s'agit de la rive droite
      dplyr::select(-LG)
    # LPo : et on supprime la colonne "LG" du tibble xs_sta_pt
  }
  
  else
  {xs_sta_pt =
    st_line_sample(xs_station, type="regular", density=1/delta) %>%
    # LPo : st_line_sample = création de points sur les profils en travers (= sur xs_station) de manière régulière
    # LPo : avec une densité = 1/delta
    st_sf() %>% dplyr::mutate(NSection=xs_station$NSection, LG=xs_station$LG,
                              NBief = xs_station$NBief, NBasVers = xs_station$NBasVers) %>% 
    ## LPo : NBief et NBasVers rajoutés pour que le module 5 du MODULE DE REPRISE fonctionne 
    st_cast("POINT", warn=F) %>%
    # LPo : st_cast convertit la géométrie, ici en points (objet sf)
    dplyr::group_by(NSection) %>%
    # LPo : Groupe les points des profils en travers par profil en travers
    bind_cols(l_posi) %>%
    # LPo : On ajoute la colonne "POSI" (nbres impairs) à xs_sta_pt
    dplyr::mutate(IPOSI=1:n(),
                  # LPo : IPOSI = numérote les points des profils en travers de la rive gauche à la rive droite
                  POSI_C = POSI - LG,
                  # LPo : POSI_C permet de définir le point central comme = 0. Les points qui vont jusqu'à la rive gauche
                  # LPo : sont négatifs tandis que les points qui vont jusqu'en rive droite sont positifs.
                  RIVE = ifelse(POSI<LG, "G", "D")) %>%
    # LPo : RIVE : colonne pour préciser de quelle rive il s'agit :
    # LPo : si POSI est inférieure à LG, alors il s'agit de la rive gauche,
    # LPo : sinon il s'agit de la rive droite
    dplyr::select(-LG)
  # LPo : et on supprime la colonne "LG" du tibble xs_sta_pt
  }
  
  
  
  

  ## extract elevation values
  
  if(opt_read_raster == "velox")
  {
    xs_sta_pt$Z = ras_mnt_vx$extract_points(xs_sta_pt$geometry)[, 1] 
    # LIGNE D'ORIGINE : 
    #xs_sta_pt$Z = ras_mnt_vx$extract_points(as_Spatial(xs_sta_pt))[, 1]
    # LPo : Voir aide "VeloxRaster_extract_points" si nécessaire 
  }
  
  else if(opt_read_raster == "GRASS")
  {
    
  }
  
  else
  {
    
    #xs_sta_pt= st_sf(xs_sta_pt)
    
    xs_sta_pt$Z = raster::extract(ras_mnt_vx, (as_Spatial(xs_sta_pt)))#[, 1])
    xs_sta_pt = tibble(xs_sta_pt)
    # LPo : Pour que le type d'objet soit le même que dans opt_read_raster == "velox" (un tibble)
    if(mode_CARTINO1D == "automatique")
    {xs_sta_pt = xs_sta_pt[, c( "NSection", "geometry", "POSI", "IPOSI", "POSI_C", "RIVE", "Z")]}
    # LPo : Pour que l'ordre des colonnes soit le même que dans opt_read_raster == "velox"
    else
    {xs_sta_pt = xs_sta_pt[, c( "NSection", "geometry", "POSI", "IPOSI", "POSI_C", "RIVE", "Z", "NBief", "NBasVers")]} 
    ## LPo : NBief et NBasVers rajoutés pour que le module 5 du MODULE DE REPRISE fonctionne  
  }
  # LPo : Création de la colonne Z dans xs_sta_pt : altitudes extraites depuis ras_mnt_vx (= le MNT) 
  
  
  ## check missing values
  xyz_na = xs_sta_pt %>% filter(is.na(Z))                                             
  # LPo : Permet de détecter les valeurs manquantes
  if (nrow(xyz_na)>0)
  {
    cat(paste0("-- Warning !!! Missing values were found"), "\n")                     
    # LPo : Affiche "Warning !!! ..." si il manque les altitudes d'un ou plusieurs points d'un profil en travers
    xs_sta_pt %<>% filter(!is.na(Z))
    # LPo : On conserve uniquement les points avec une valeur d'altitude
  }
  
  return(xs_sta_pt)                                                                   
  # LPo : Résultats: retourne les altitudes à chaque point de chaque XS (100 points le long de chaque profil).
  # LPo : Résultats stockés dans le champ Z de xs_sta_pt.
}



#' @title Extraction des débits pour tous les profils en travers par interpolation
#' @description FR : Cette fonction est issue du script 01-PreHydrau.R.
#' 
#' get_discharge est appelé dans 99-RunReach.R en phase 1 et en phase 2.
#' Cette fonction permet d'extraire les valeurs de débit pour chaque profil en travers, en interpolant.
#' Les résultats sont à chaque fois stockés dans la variable xs_sta_q.
#' 
#' Elle fonctionne de la manière suivante :
#' 
#' - get_discharge fait appel à interpol_bief_Q ;
#' - interpol_bief_Q est défini dans CARTINO.R : il utilise "approxfun" (fonction d'interpolation propre à R) 
#' sur stream_disch$DIST et stream_disch$Q, avec la méthode met_int_disch.
#' - et stream_disch (data.frame) avait été créé grâce à la fonction get_disch_stream du script 01-PreHydrau.R.
#' 
#' 
#' ENG : Get discharge values for each cross sections by interpolation 
#' 
#' @param xs_station la fonction s'applique sur le tableau de données des profils en travers (en tant qu'objet sf 
#' (LINESTRING)) avec leurs caractéristiques / river cross sections as spatial lines with features
#' 
#' @return la fonction retourne les profils en travers avec les valeurs de débit / 
#' river cross sections with discharge data

get_discharge = function(xs_station)                                                  
{
  xs_station$Q = interpol_bief_Q(xs_station$DIST)                                     
  # LPo : La fonction interpol_bief_Q permet l'interpolation des débits entre les XS
  # LPo : interpol_bief_Q est défini dans CARTINO.R (approxfun). 
  

  return(xs_station)                                                                  
  # LPo : retourne les résultats
}

