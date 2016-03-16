open Batteries
open Opium.Std

open UserData

let user_info = get "/user/:username" begin fun req ->
    let username = param req "username" in
    match Db.get_user username with
    | None -> `String "{\"error\": true}" |> respond'
    | Some user ->
      let json = json_of_user user |> Yojson.Safe.to_string in
      `String json |> respond'
  end

let user_page = get "/u/:username" begin fun req ->
    let username = param req "username" in
    match Db.get_user username with
    | None -> redirect' (Uri.of_string "/")
    | Some user ->
      let page = Printf.sprintf [%blob "../static/user.html"]
          user.username
          user.username
          user.firstname
          user.lastname
          (json_of_boners' user.boners |> Yojson.Safe.to_string) in
      `String page |> respond'
  end

let nearby_boners = get "/near/:latitude/:longitude" begin fun req ->
    let boner = create_boner ~latitude:(float_of_string @@ param req "latitude")
                             ~longitude:(float_of_string @@ param req "longitude") in
    let geo (user, boner) =
      let open Yojson.Safe in
      to_string @@
        `Assoc ["type", `String "Feature";
                "geometry", `Assoc [
                  "type", `String "Point";
                  "coordinates", `List [`Float boner.longitude; (* geoJSON reverses these *)
                                        `Float boner.latitude];
                  "time", `Int (int_of_float boner.time)
                ];
                "properties", `Assoc [
                  "name", `String ("<a href=\"/u/" ^ user ^ "\">" ^ user ^ "</a>"); (* Link to user page *)
                  "description", `String (format_time boner.time)
                ]
               ] in
    let boners = Enum.map geo (Db.get_locs boner) in
    let folder i obj buff = match i with
      | 0 -> buff ^ obj
      | _ -> buff ^ "," ^ obj in
    let preamble  = "{\"type\":\"FeatureCollection\",\"features\":["
    and postamble = "]}" in
    `String (Enum.foldi folder preamble boners ^ postamble)|> respond'
  end

let new_user = post "/new/" begin fun req ->
    App.urlencoded_pairs_of_body req >>= fun params ->
    let post_param name = List.assoc name params |> List.hd in
    let firstname = post_param "firstname"
    and lastname  = post_param "lastname"
    and username  = post_param "username"
    and password  = post_param "password" in
    (create_user ~firstname ~lastname ~username |> Db.new_user) password;
    redirect' (Uri.of_string "/")
  end

let log_boner = post "/log/" begin fun req ->
    App.urlencoded_pairs_of_body req >>= fun params ->
    let post_param name = List.assoc name params |> List.hd in
    let username  = post_param "username"
    and password  = post_param "password"
    and latitude  = float_of_string @@ post_param "latitude"
    and longitude = float_of_string @@ post_param "longitude" in
    let boner = create_boner ~latitude ~longitude in
    Db.put_boner username boner password;
    redirect' (Uri.of_string "/")
  end

let index = get "/" begin fun req ->
    let page = Printf.sprintf [%blob "../static/index.html"]
        [%blob "../mapsapi.key"] in
    `String page |> respond'
  end

let app =
  let static = Middleware.static ~local_path:"./static" ~uri_prefix:"/static" in (* ./ because from exe directory *)
  App.empty
  |> index
  |> user_info
  |> user_page
  |> new_user
  |> log_boner
  |> nearby_boners
  |> middleware static

let () = App.run_command app
