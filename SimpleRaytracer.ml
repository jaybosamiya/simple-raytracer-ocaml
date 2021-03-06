open Core_kernel

type color = {
    r: float;
    g: float;
    b: float;
  }
type point3 = {
    x: float;
    y: float;
    z: float;
  }

type viewport = int -> int -> color
type ray = {
    origin: point3;
    dirn: point3;
  }

type obj =
  | Sphere of point3 * float * color
type space = obj list

(* ************************************* *)

let black = { r = 0.0 ; g = 0.0 ; b = 0.0 }
let red = { r = 1.0 ; g = 0.0 ; b = 0.0 }
let green = { r = 0.0 ; g = 1.0 ; b = 0.0 }
let blue = { r = 0.0 ; g = 0.0 ; b = 1.0 }
let white = { r = 1.0 ; g = 1.0 ; b = 1.0 }

module RayOps = struct
  let dot { x=x1 ; y=y1 ; z=z1 } { x=x2 ; y=y2 ; z=z2 } =
    (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

  let add { x=x1 ; y=y1 ; z=z1 } { x=x2 ; y=y2 ; z=z2 } =
    { x = x1+.x2 ; y = y1+.y2 ; z = z1+.z2 }

  let sub { x=x1 ; y=y1 ; z=z1 } { x=x2 ; y=y2 ; z=z2 } =
    { x = x1-.x2 ; y = y1-.y2 ; z = z1-.z2 }

  let div { x ; y ; z } v = { x = x/.v ; y = y/.v ; z = z/.v }

  let mul { x ; y ; z } v = { x = x*.v ; y = y*.v ; z = z*.v }

  let norm v = div v (sqrt (dot v v))
end

(* Returns (point of hit * normal), if it exists *)
let sphere_hit (Sphere (center,rad,_)) {origin; dirn}
    : (point3 * point3) option =
  let open RayOps in
  let sqr x = x *. x in
  let s = origin in
  let c = center in
  let d = norm dirn in
  let v = sub s c in
  let r = rad in
  let delta = sqr (dot v d) -. ((dot v v) -. sqr r) in
  if delta < 0. then None else
    Some (
        let hit =
          let t1 = (-. (dot v d)) +. sqrt delta in
          let t2 = (-. (dot v d)) -. sqrt delta in
          let t = min t1 t2 in
          add s (mul d t) in
        let n = norm (sub hit c) in
        (hit, n)
      )

let rec ray_extend_aux r spc light : (color * point3) option =
  match spc with
  | [] -> None
  | Sphere (center,rad,col) :: spc' -> (
    match sphere_hit (Sphere (center,rad,col)) r with
    | Some (hit1,n) ->
       let col1 =
         let open RayOps in
         let d = norm light in
         let cosTheta = Float.max (dot n d) 0. in
         { r = col.r *. cosTheta ;
           g = col.g *. cosTheta ;
           b = col.b *. cosTheta } in
       (match ray_extend_aux r spc' light with
        | None -> Some (col1, hit1)
        | Some (col2, hit2) ->
           let open RayOps in
           let dv1 = sub hit1 r.origin in
           let dv2 = sub hit2 r.origin in
           let d1 = dot dv1 dv1 in
           let d2 = dot dv2 dv2 in
           if d1 < d2 then
             Some (col1, hit1)
           else
             Some (col2, hit2))
    | None ->
       ray_extend_aux r spc' light
  )

let ray_extend r spc light : color =
  match ray_extend_aux r spc light with
  | None -> black
  | Some (col, _) -> col

let create_viewport_ray x y persp =
  let open RayOps in
  let origin = { x = float_of_int x ; y = float_of_int y ; z = 0. } in
  let dirn = norm (sub origin persp) in
  { origin ; dirn }

let ray_trace width height spc light persp : viewport =
  fun x y ->
  if x < 0 || x >= width then assert false else
    if y < 0 || y >= height then assert false else (
      let r = create_viewport_ray x y persp in
      ray_extend r spc light
    )

(* ************************************* *)

type ppm = {
    height: int;
    width: int;
    max: int;
    pixels: int -> int -> color;
  }

let print_ppm p =
  let open Format in
  printf "P3\n";
  printf "%d %d\n" p.width p.height;
  printf "%d\n" p.max;
  let pixel_print x y =
    let m = p.max in
    let conv a = int_of_float (float_of_int m *. a) in
    let { r; g; b } = p.pixels x y in
    printf "%d %d %d\n" (conv r) (conv g) (conv b) in
  let rows =
    List.range ~start:`inclusive ~stop:`exclusive 0 p.height in
  let cols =
    List.range ~start:`inclusive ~stop:`exclusive 0 p.width in
  List.iteri rows ~f:(fun y _ ->
      List.iteri cols ~f:(fun x _ ->
          pixel_print x y
        ))

(* ************************************* *)

let _ =
  let w, h = 640, 360 in
  let light = { x= -. 0.4 ; y= -. 0.4 ; z= -. 0.5 } in
  let persp = { x= float_of_int w /. 2. ; y= float_of_int h /. 2. ; z = -. 200. } in
  let spc = [
      Sphere ({x=320.; y=160.; z=80.}, 100., red);
      Sphere ({x=160.; y=220.; z=100.}, 100., green);
      Sphere ({x=480.; y=220.; z=100.}, 100., blue);
      Sphere ({x=320.; y=2000.; z=700.}, 1800., white);
    ] in
  let v = ray_trace w h spc light persp in
  let p = { height = h; width = w; max = 255; pixels = v } in
  print_ppm p
