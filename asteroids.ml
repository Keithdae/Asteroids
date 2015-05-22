open Graphics;;

(* constantes et parametres *)
let pi = 4.0 *. atan 1.0;;
let ast_radius = 8.0;;

(* dimension fenetre graphique *)
let width = 1000;;
let height = 600;;


(* --- definition types pour etat du jeu --- *)

(* A DEFINIR : positions, deplacements, etc. *)
type point = {x:float; y:float};;
type segment = {p1:point; p2:point};;
type gameState = Playing | GameOver;;


(* 	asteroid
	pointListAstt: decrit le polygone representant l'asteroid. Les points sont positionnes relativement au centre centerAst de l'asteroid.
	size: Trois tailles possibles : Grand (6.0), moyen (4.5) et petit (3.0).
	vector: represente le deplacement de l'asteroid.
 *)
type asteroid = {pointListAst:point list; colAst:color; centerAst:point; vector:point; size:float};;

(*	shot
	seg: decrit le trait qui represente le tir
	acceleration: represente le deplacement du tir
*)
type shot = {seg:segment; colShot:color; acceleration:point};;


(* 	ship
	pointList: decrit le triangle representant le vaisseau. Les points sont positionnes relativement au centre center du vaisseau.
	orientation: represente l'angle du vaisseau, sert a determiner l'orientation et le deplacement des tirs.
	collisionCircle: contient le centre du cercle inscrit du vaisseau ainsi que son rayon.
	speed: represente le deplacement du vaisseau
 *)
type ship = {pointList:point list; col:color; center:point; orientation:float; collisionCircle:(point*float); speed:point};;


(*	etat
	game: indique si le jeu est en cours ou si le joueur a perdu 
	level: niveau courant du jeu
	score: score du joueur
	cdMultishot: delai de recuperation du tir multiple
*)
type etat = {ship:ship; shotList:shot list; astList:asteroid list; game:gameState; level:float; score:int; cdMultishot:int; life:int};;


(* Fonctions utiles *)
let add_points p1 p2 = {x= p1.x +. p2.x; y= p1.y +. p2.y};;

let distance_points (p1, p2) = sqrt ( ((p2.x -. p1.x) *. (p2.x -. p1.x)) +. ((p2.y -. p1.y) *. (p2.y -. p1.y)) );;

let rec appartient = function(x,l) -> match l with
[] -> false
  | e::r -> (e = x || appartient(x,r));;

let rec intersection = function(l1, l2) -> match l1 with
[] -> []
  | e::r -> if(appartient(e, l2)) then e::intersection(r,l2) 
    else intersection(r,l2);;

(* Calcul du centre et du rayon du cercle inscrit d'un triangle => utilise pour les collisions du vaisseau *)
let calculer_barycentre lp = match lp with
			p1::p2::p3::[] -> {x= (p1.x +. p2.x +. p3.x) /. 3. ; y= (p1.y +. p2.y +. p3.y) /. 3.}
			| _ -> failwith "La figure decrite par la liste de points n'est pas un triangle !";;

let perimetre_triangle lp = match lp with
			p1::p2::p3::[] -> let a = distance_points (p1,p2) and b = distance_points (p2,p3) and c = distance_points (p1,p3) in
					a +. b +. c
			| _ -> failwith "La figure decrite par la liste de points n'est pas un triangle !";;

let aire_triangle lp = match lp with
			p1::p2::p3::[] -> let a = distance_points (p1,p2) and b = distance_points (p2,p3) and c = distance_points (p1,p3) in
					 let s = (a +. b +. c) /. 2. in
					sqrt(s *. (s -. a) *. (s -. b) *. (s -. c))
			| _ -> failwith "La figure decrite par la liste de points n'est pas un triangle !";;

let get_radius lp = match lp with
			p1::p2::p3::[] -> (2. *. (aire_triangle lp)) /. (perimetre_triangle lp)
			| _ -> failwith "La figure decrite par la liste de points n'est pas un triangle !";;




(* Variables *)

(* Vaisseau au centre de l'ecran, immobile *)
let ship1 = let pList = [{x= 0.; y= 25.}; {x= 20. ;y= -.25.}; {x= -.20.;y= -.25.}] and c = {x= 500.; y = 300.} in {pointList = pList; col=white; center = c; orientation=pi /. 2.; collisionCircle = (calculer_barycentre (List.map (add_points c) pList), get_radius pList); speed ={x=0.;y=0.}};;
(* Etat de depart *)
let state = {ship=ship1; shotList=[]; astList=[]; game = Playing; level = 1.0; score = 0; cdMultishot = 0; life = 3};;



(* --- initialisations etat --- *)

(* Generation positions, deplacements initiaux *)
(* Fonction de generation de la couleur des asteroids *)
let gen_color = function () ->
        	let r = (Random.int 191) + 40 in
		let g = (Random.int 191) + 40 in
		let b = (Random.int 191) + 40 in
		rgb r g b;;

(* Generation de la position initiale de l'asteroid, sur les bords de l'ecran *)		
let gen_center = function () ->
		 let xr = (Random.float 300.) in
		 let yr = (Random.float 200.) in
		 {x = if xr > 149. then xr +. 700. else xr; y = if yr > 99. then yr +. 400. else yr};;

(* Generation du deplacement de l'asteroid, plus le niveau de jeu est eleve plus la vitesse peut etre importante *)
let gen_move level =
		let xm = ((Random.float 8.) +. level) -. (4. +. level /. 2.) in
		let ym = ((Random.float 8.) +. level) -. (4. +. level /. 2.) in
		{x = if xm < 1. && xm > (-.1.) then level else xm; y = if ym < 1. && ym > (-.1.) then (-.level) else ym};;

(* Generation de la forme de l'asteroid *)
let rec gen_pList (i, size, ang) = let xr = (cos ang) *. ((Random.float ((ast_radius /. 4.0)*.size)) +. (((ast_radius *. 3. /. 4.)) *. size)) in
			  	   let yr = (sin ang) *. ((Random.float ((ast_radius /. 4.0)*.size)) +. (((ast_radius *. 3. /. 4.)) *. size)) in
			      	   if i = 0 then [] 
				   else {x= xr; y = yr}::gen_pList(i-1, size, ang -. (pi /. 6.) );;
let gen_pointList size = (gen_pList (12, size, (-.pi) ));;

(* Genere un asteroid en fonction de la taille demandee et du niveau actuel *)
let gen_ast (size,level) = {pointListAst = gen_pointList size; colAst = gen_color (); centerAst = gen_center (); vector = gen_move level;size = size};;
(* Genere la liste d'asteroids en prenant une quantite, une taille et le niveau actuel *)
let rec gen_aList (i, size, level):asteroid list = if i = 0 then [] else gen_ast (size, level)::gen_aList (i-1, size, level);; 
(* Appelle la fonction precedente avec un nombre d'asteroids a la fois aleatoire et dependant du niveau actuel *)
let gen_astList size level = gen_aList (((Random.int 3) + 2 + (int_of_float level)), size, level);;

(* Genere un fragment d'un asteroid casse, en gardant une position proche et la meme couleur *)
let gen_child (ast, size, level) = {pointListAst = gen_pointList size; colAst = ast.colAst; centerAst = {x = ast.centerAst.x +. (Random.float 50.) -.25.; y = ast.centerAst.y +. (Random.float 50.) -.25.}; vector = gen_move level; size = size};;
(* Genere la liste des fragments *)
let rec gen_childlist (i, ast, size, level) = if i = 0 then [] else gen_child (ast, size, level)::gen_childlist(i-1, ast, size, level);;

(* Initialise l'etat en ajoutant un nombre aleatoire d'asteroids de taille maximale *)
let init_etat s = {s with astList = gen_astList 6.0 s.level} ;;



(* --- changements d'etat --- *)


(* Perte du jeu *)
let game_over etat = {etat with astList = []; shotList = []; ship = {etat.ship with col = black}; game = GameOver; life = 0};;

(* Passage au niveau suivant *)
let next_level etat = match etat.astList with
			[] -> {etat with astList = gen_astList 6.0 (etat.level +. 1.); level = etat.level +. 1.}
			| _ -> etat;;


(* Deplacement du vaisseau *)
let update_speed ship = {ship with speed = {x= if ship.speed.x < 0.5 && ship.speed.x > -.0.5 then (cos ship.orientation) *. 3. else ship.speed.x +. (cos ship.orientation); y= if ship.speed.y < 0.5 && ship.speed.y > -.0.5 then (sin ship.orientation) *. 3. else ship.speed.y +. (sin ship.orientation)}}
let acceleration etat = {etat with ship = update_speed etat.ship};; 

(* teleportation du vaisseau *)
let teleportation etat = if etat.score = 0 then etat 
			 else {etat with ship = {etat.ship with center = {x = Random.float (float_of_int width); y = Random.float (float_of_int height)}; speed = {x= 0.; y=0.}}; score = if etat.score > 200 then etat.score - 200 else 0 };;

(* rotation vers la gauche et vers la droite du vaisseau *)
(* rotation d'un point du vaisseau d'un angle ang *)
let rotation_shippoint ang poi = {x = (cos ang) *. (poi.x) -. (sin ang) *. (poi.y); y = (sin ang) *. (poi.x) +. (cos ang) *. (poi.y)};;

let rotation_pointlist (plist, ang) = List.map (rotation_shippoint ang) plist

let rotation_ship (ship, ang) = {ship with pointList = rotation_pointlist (ship.pointList, ang) ; orientation = ship.orientation +. ang};;

let rotation_gauche etat = { etat with ship = rotation_ship (etat.ship, pi /. 12.0) };; (* ok *)
let rotation_droite etat = { etat with ship = rotation_ship (etat.ship, -. pi /. 12.0) };; (* ok *)

(* tir d'un nouveau projectile *)
let create_shot etat = {seg = {p1 = add_points (List.hd etat.ship.pointList) etat.ship.center; p2 = {x  = (List.hd etat.ship.pointList).x +. (cos etat.ship.orientation)*. 10. +. etat.ship.center.x; y = (List.hd etat.ship.pointList).y +. (sin etat.ship.orientation)*. 10. +. etat.ship.center.y}}; colShot = white; acceleration = {x = cos etat.ship.orientation *. 25.; y = sin etat.ship.orientation *. 25.}};;
let tir etat = {etat with shotList = if etat.game = Playing then (create_shot etat)::etat.shotList else []};;

(* Tir de plusieurs projectiles en cone, depend du délai cdMultishot qui permet d'en limiter l'utilisation *)
(* tir d'un projectile en prenant un angle ang de decalage par rapport au vaisseau *)
let create_lateral_shot ship ang = {seg = {p1 = add_points (List.hd ship.pointList) ship.center; p2 = {x  = (List.hd ship.pointList).x +. (cos (ship.orientation +. ang))*. 10. +. ship.center.x; y = (List.hd ship.pointList).y +. (sin (ship.orientation +. ang))*. 10. +. ship.center.y}}; colShot = white; acceleration = {x = cos (ship.orientation +. ang) *. 25.; y = sin (ship.orientation +. ang) *. 25.}};;
(* tir d'un nombre i de projectiles avec un decalage de ang entre chaque tir en partant d'un angle angDepart *)
let rec create_multishot ship i angDepart ang = if i = 0 then [] 
						else create_lateral_shot ship (angDepart +. ang)::create_multishot ship (i-1) (angDepart +. ang) ang;;
let multishot etat = {etat with shotList = if etat.game = Playing && etat.cdMultishot = 0 then (create_multishot etat.ship 7 (pi /. 9.) (-.pi /. 36.))@etat.shotList else etat.shotList; cdMultishot = 100};;



(* calcul de l'etat suivant, apres un pas de temps *)



(* Mise a jour des tirs *)
(* Deplacement d'un tir *)
let update_shot (shot:shot) = {shot with seg = {p1 = {x = shot.seg.p1.x +. shot.acceleration.x; y = shot.seg.p1.y +. shot.acceleration.y}; p2 = {x = shot.seg.p2.x +. shot.acceleration.x; y = shot.seg.p2.y +. shot.acceleration.y}}};; 

(* Verification qu'il ne sort pas de l'ecran *)
let check_shot (shot:shot) = (((shot.seg.p1.x < 1000. && shot.seg.p1.y < 600.) && shot.seg.p1.x > 0.) && shot.seg.p1.y > 0.);;

(* Mise a jour de tous les tirs en ne gardant que ceux dans la fenetre *)
let update_shotlist (shotlist: shot list) = List.filter (check_shot) (List.map update_shot shotlist);;



(* Mise a jour des asteroids *)
(* Deplacement d'un asteroid, gere le cas ou l'asteroid arrive en bord de fenetre a l'aide d'un modulo positif *)
let update_ast (ast:asteroid) = {ast with centerAst = {x = (float_of_int ((((int_of_float (ast.centerAst.x +. ast.vector.x)) mod width) + width) mod width)); y = (float_of_int ((((int_of_float (ast.centerAst.y +. ast.vector.y)) mod height) + height) mod height))} };;
(* Mise a jour des positions de tous les asteroids *)
let update_astlist astlist = List.map (update_ast) astlist;;



(* Mise a jour de l'etat *)

(* Collision tir / asteroid *)
(* Cercle de rayon ast_radius*.size pour les collisions *)
let collision_shot_ast ast shot = (distance_points(shot.seg.p2, ast.centerAst)) < (ast_radius *. ast.size);;

(* Determine un coefficient de vitesse d'un asteroid pour le score *)
let speed_to_score vector = ((abs (int_of_float vector.x)) + (abs (int_of_float vector.y))) * 20 + 10;;

(* Fragmentation de l'asteroid *)
let frag_ast (ast, level) = if ast.size = 3.0 then [] else gen_childlist (((Random.int 2) + 2),ast , ast.size -. 1.5, level);;

(* Test de collsion sur les listes completes de tirs et d'asteroids *)
let rec collision_lshot_last last lshot level  = match last,lshot with
			 (_,[]) -> (last,[], 0)
			|([],_) -> ([],lshot, 0)
			|(e::[],x::m) -> if collision_shot_ast e x then 
					((frag_ast (e, level)),m, speed_to_score e.vector )
					 else let (a,b,s) = collision_lshot_last [e] m level in (a,x::b,s)
			|(e::r,x::m) -> if collision_shot_ast e x then 
					let (a,b,s) = collision_lshot_last ((frag_ast (e, level))@r) m level in 
					(a, b, s+(speed_to_score e.vector))
					else let (a,b,score1) = collision_lshot_last [e] lshot level in 
					     let (c,d,score2) = collision_lshot_last r (intersection (lshot, b)) level in 
						(a@c, d, score1 + score2);;


(* Collisions vaisseau / asteroid : test sur les sommets du triangle qui represente le vaisseau ainsi qu'un test sur le cercle inscrit du triangle pour completer les collisions *)

(* Test les sommets *)
let rec collision_ast_ship1 ast shipPList = match shipPList with
					[] -> false
					| x::r -> (distance_points(x, ast.centerAst) < (ast_radius *. ast.size))
							|| collision_ast_ship1 ast r;;

(* Test sur le cercle inscrit *)
let collision_ast_ship2 ast ship = distance_points (ast.centerAst, (fst ship.collisionCircle)) < (ast.size *. ast_radius) +. (snd ship.collisionCircle);;

(* Test combine, les coordonnees des points du vaisseau dependent de son centre, l'appel a List.map permet d'obtenir les coordonnes reelles *)
let collision_ast_ship ship ast = (collision_ast_ship1 ast (List.map (add_points ship.center) ship.pointList)) || (collision_ast_ship2 ast ship);; 
 
(* Test sur la liste d'asteroids entiere *)
let rec collision_astList_ship astList ship = match astList with
						[] -> false
						| x::r -> (collision_ast_ship ship x) || (collision_astList_ship r ship);;

(* Perte d'une vie *)
let no_collision_ast_ship ship ast = not( (collision_ast_ship1 ast (List.map (add_points ship.center) ship.pointList)) || (collision_ast_ship2 ast ship) );; 
let lose_life etat = if etat.life = 1 then game_over etat
		     else {etat with astList = List.filter (no_collision_ast_ship etat.ship) etat.astList ; life = etat.life - 1};;

(* Etat suivant avec tous les deplacements ainsi que toutes les collisions *)
let etat_suivant etat = let (last,lshot,score) = collision_lshot_last etat.astList etat.shotList etat.level in 
			if collision_astList_ship last etat.ship then lose_life etat 
			else if etat.game = Playing then 
			let netat = {etat with 
				shotList = update_shotlist lshot; 
				astList = update_astlist last; 
				score = etat.score + score; 
				ship = {etat.ship with 
					center = {x= (float_of_int ((((int_of_float (etat.ship.center.x +. etat.ship.speed.x)) mod width) + width) mod width));y= (float_of_int ((((int_of_float (etat.ship.center.y +. etat.ship.speed.y)) mod height) + height) mod height))}; 
					speed = {x= if etat.ship.speed.x > -0.2 && etat.ship.speed.x < 0. then 0. else etat.ship.speed.x *. 0.96; y=if etat.ship.speed.y > -0.2 && etat.ship.speed.y < 0. then 0. else etat.ship.speed.y *. 0.96}; 
					collisionCircle = (calculer_barycentre (List.map (add_points etat.ship.center) etat.ship.pointList), get_radius etat.ship.pointList)};
				cdMultishot = if etat.cdMultishot > 0 then etat.cdMultishot - 1 else 0 } 
			in next_level netat 
			else game_over etat;;


(* --- affichages graphiques --- *)

(* fonctions d'affichage du vaisseau, d'un asteroide, etc. *)
let draw_background c = set_color c;
        	fill_rect 0 0 width height;;

let point_to_doubleInt point = (int_of_float point.x, int_of_float point.y);;
let draw_ship ship = let pointArray = (Array.of_list (List.map (function (x, y) -> (x + (int_of_float ship.center.x), y + (int_of_float ship.center.y))) (List.map point_to_doubleInt ship.pointList))) in
			set_color ship.col;
		    	draw_poly pointArray;
			fill_poly pointArray;;
let draw_ast (ast:asteroid) = let pointArray = (Array.of_list (List.map (function (x, y) -> (x + (int_of_float ast.centerAst.x), y + (int_of_float ast.centerAst.y))) (List.map point_to_doubleInt ast.pointListAst))) in
			set_color ast.colAst;
		    	draw_poly pointArray;
			fill_poly pointArray;;
			

let draw_shot (shot:shot) = set_color shot.colShot;
            draw_segments[| (int_of_float shot.seg.p1.x, int_of_float shot.seg.p1.y, int_of_float shot.seg.p2.x, int_of_float shot.seg.p2.y)|];;

let draw_shots (l:shot list) = List.iter draw_shot l;;

let print_game_over etat = match etat.game with
				GameOver -> moveto 475 300;
					    set_color white; 
				            draw_string "GAME OVER"; 
					    moveto 455 270; 
					    draw_string "PRESS Q TO EXIT"
				| _ -> ();;

let print_level level = set_color white;
			moveto 30 (height - 30);
			draw_string ("LEVEL : " ^ string_of_int (int_of_float level));;

let print_score score = set_color white;
			moveto 30 (height - 45);
			draw_string ("SCORE : " ^ string_of_int score);;

let print_multishot cd life = set_color white;
				moveto 30 (height - 60);
				draw_string (if cd = 0 && life > 0 then "MULTISHOT READY" else "");;

let rec life_to_string life = if life = 0 then ""
			  else "|"^life_to_string (life-1);;
let print_life life = set_color white;
		      moveto 30 (height - 75);
		      draw_string (if life = 0 then "YOU DIED" else "LIFE : " ^ (life_to_string life));;
			
			
let affiche_etat etat = clear_graph ();
			draw_background black;
			print_level etat.level;
			print_score etat.score;
			print_multishot etat.cdMultishot etat.life;
			print_life etat.life;
			draw_ship etat.ship;
			List.iter (draw_ast) etat.astList;
			print_game_over etat;
			draw_shots etat.shotList;;


(* --- boucle d'interaction --- *)

let rec boucle_interaction ref_etat =
  let status = wait_next_event [Key_pressed] in (* on attend une frappe clavier *)
  let etat = !ref_etat in (* on recupere l'etat courant *)
  let nouvel_etat = (* on definit le nouvel etat... *) 
    match status.key with (* ...en fonction de la touche frappee *)
    | '1' | 'j' -> rotation_gauche etat (* rotation vers la gauche *)
    | '2' | 'k' -> acceleration etat (* acceleration vers l'avant *)
    | '3' | 'l' -> rotation_droite etat (* rotation vers la droite *)
    | ' ' -> tir etat (* tir d'un projectile *)
    | 'i' -> teleportation etat (* On se teleporte *)
    | 'u' -> multishot etat (* Tir de plusieurs projectiles en cone *)
    | 'q' -> print_endline "Bye bye!"; exit 0 (* on quitte le jeux *)
    | _ -> etat in (* sinon, rien ne se passe *)
  ref_etat := nouvel_etat; (* on enregistre le nouvel etat *)
  boucle_interaction ref_etat;; (* on se remet en attente de frappe clavier *)

(* --- fonction principale --- *)
    
let main () =
  (* initialisation du generateur aleatoire *)
  Random.self_init ();	
  (* initialisation de la fenetre graphique et de l'affichage *)
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  auto_synchronize false;
  (* initialisation de l'etat du jeu *)
  let ref_etat = ref (init_etat state) in
  (* programmation du refraichissement periodique de l'etat du jeu et de son affichage *)
  let _ = Unix.setitimer Unix.ITIMER_REAL
    { Unix.it_interval = 0.05; (* tous les 1/.20eme de seconde... *)
      Unix.it_value = 0.05 } in
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle (fun _ ->
      affiche_etat !ref_etat; (* ...afficher l'etat courant... *)
      synchronize ();
      ref_etat := etat_suivant !ref_etat)); (* ...puis calculer l'etat suivant *)
  boucle_interaction ref_etat;; (* lancer la boucle d'interaction avec le joueur *)





let _ = main ();; (* demarrer le jeu *)