open UserData

open Batteries
open Gensqlite_tools

let db = Sqlite3.db_open "data/users.db"

module Raw = struct
  let (_, get_user)   = [%gensqlite db "SELECT @s{firstname}, @s{lastname} FROM users WHERE username = %s{username}"]
  let (_, get_boners) = [%gensqlite db "SELECT @f{latitude}, @f{longitude}, @f{time} FROM boners WHERE \
                                        username = %s{username}"]
  let (_, get_hash)   = [%gensqlite db "SELECT @s{password} FROM users WHERE username = %s{username}"]
  let (_, new_user)   = [%gensqlite db "INSERT INTO users (firstname, lastname, username, password) VALUES \
                                        (%s{firstname}, %s{lastname}, %s{username}, %s{password})"]
  let (_, put_boner)  = [%gensqlite db "INSERT INTO boners (username, latitude, longitude, time) VALUES \
                                        (%s{username}, %s{latitude}, %s{longitude}, %s{time})"]
  let (_, get_locs)   = [%gensqlite db "SELECT @s{username}, @f{latitude}, @f{longitude}, @f{time} FROM boners"]
end

let get_user username =
  let user = Raw.get_user ~username () in
  match user with
  | [] -> create_user ~firstname:"error" ~lastname:"error" ~username
  | (firstname, lastname)::_ ->
    {(create_user ~firstname ~lastname ~username) with
     boners = Raw.get_boners ~username ()
              |> List.map (fun (latitude, longitude, time) ->
                  {latitude  = float_of_string latitude;
                   longitude = float_of_string longitude;
                   time      = float_of_string time})}

let new_user {firstname; lastname; username} password =
  let password = Bcrypt.string_of_hash (Bcrypt.hash password) in
  Raw.new_user ~password ~username ~lastname ~firstname ()

let put_boner username boner password =
  let latitude  = boner.latitude  |> string_of_float
  and longitude = boner.longitude |> string_of_float
  and time      = boner.time      |> string_of_float in
  match Raw.get_hash ~username () with
  | str::_ when Bcrypt.verify password (Bcrypt.hash_of_string str) ->
    Raw.put_boner ~time ~longitude ~latitude ~username ()
  | [] | _::_ -> ()

let get_locs ?(range = 40.0 (* km *)) from =
  let mapper (username, latitude, longitude, time) =
    (username, {latitude  = float_of_string latitude;
                longitude = float_of_string longitude;
                time      = float_of_string time}) in
  let locs = Raw.get_locs () |> List.enum |> Enum.map mapper in
  let now = Unix.time () in
  let distance p1 p2 =
    let rad deg = (deg /. 180.) *. 3.14159 in
    let lat1,  lat2  = rad p1.latitude,  rad p2.latitude
    and long1, long2 = rad p1.longitude, rad p2.longitude in
    let dlat  = lat2  -. lat1
    and dlong = long2 -. long1 in
    (* Haversine formula: *)
    let a = (sin (dlat /. 2.) ** 2.) +. cos p1.latitude *. cos p2.latitude *. (sin (dlong /. 2.) ** 2.) in
    let c = 2. *. atan2 (sqrt a) (sqrt @@ 1. -. a) in
    c *. 6371. in
  Enum.filter (fun (_, pos) -> ((now -. pos.time) /. 3600.) *. distance pos from <= range) locs
