open UserData

open Batteries
open Gensqlite_tools

let db = Sqlite3.db_open "data/users.db"

module Raw = struct
  let (_, get_user) = [%gensqlite db "SELECT @s{firstname}, @s{lastname}, @s{boners} \
                                      FROM users WHERE username = %s{username}"]
  let (_, get_hash) = [%gensqlite db "SELECT @s{password} FROM users WHERE username = %s{username}"]
  let (_, new_user) = [%gensqlite db "INSERT INTO users (firstname, lastname, username, password, boners) \
                                      VALUES (%s{firstname}, %s{lastname}, %s{username}, %s{password}, \
                                      %s{boners})"]
  let (_, put_user) = [%gensqlite db "UPDATE users SET boners=%s{boners} WHERE username=%s{username}"]

  let (_, get_locs) = [%gensqlite db "SELECT @s{username}, @s{boners} FROM users"]
end

let get_user username =
  let user = Raw.get_user ~username () in
  let elems = function
    | `List xs -> xs
    | _ -> [] in
  match user with
  | [] -> create_user ~firstname:"error" ~lastname:"error" ~username
  | (firstname, lastname, boners)::_ ->
    {(create_user ~firstname ~lastname ~username) with
     boners = Yojson.Safe.from_string boners
              |> elems |> List.map boner_of_json}

let new_user {firstname; lastname; username; boners} password=
  let boners' = List.map json_of_boner boners in
  let boners = `List boners' |> Yojson.Safe.to_string in
  let password = Bcrypt.string_of_hash (Bcrypt.hash password) in
  Raw.new_user ~boners ~password ~username ~lastname ~firstname ()

let put_user {username; boners} password =
  let boners' = List.map json_of_boner boners in
  let boners = `List boners' |> Yojson.Safe.to_string in
  match Raw.get_hash ~username () with
  | str::_ when Bcrypt.verify password (Bcrypt.hash_of_string str) -> Raw.put_user ~username ~boners ()
  | [] | _::_ -> ()

let get_locs ?(range = 40.0 (* km *)) from =
  let mapper (user, json) =
    let json = Yojson.Safe.from_string json in
    match json with
    | `List items -> List.enum items |> Enum.map (fun j -> user, boner_of_json j)
    | _ -> Enum.empty () in
  let locs = Raw.get_locs () |> List.enum |> Enum.concat_map mapper in
  let now = Unix.time () in
  let distance p1 p2 =
    let rad deg = (deg /. 180.) *. 3.14159 in
    let long1, long2 = rad p1.longitude, rad p2.longitude
    and lat1,  lat2  = rad p1.latitude,  rad p2.latitude in
    let dlong = long2 -. long1
    and dlat  = lat2  -. lat1 in
    (* Haversine formula: *)
    let a = (sin (dlat /. 2.) ** 2.) +. cos p1.latitude *. cos p2.latitude *. (sin (dlong /. 2.) ** 2.) in
    let c = 2. *. atan2 (sqrt a) (sqrt @@ 1. -. a) in
    c *. 6371. in
  Enum.filter (fun (_, pos) -> ((now -. pos.time) /. 3600.) *. distance pos from <= range) locs
