open Opium.Std

open UserData

let user_info = get "/user/:username" begin fun req ->
    let username = param req "username" in
    let user = Db.get_user username in
    let json = json_of_user user |> Yojson.Safe.to_string in
    `String json |> respond'
  end

let new_user = post "/new/" begin fun req ->
    App.urlencoded_pairs_of_body req >>= fun params ->
    let post_param name = List.assoc name params |> List.hd in
    let firstname = post_param "firstname"
    and lastname  = post_param "lastname"
    and username  = post_param "username" in
    create_user ~firstname ~lastname ~username |> Db.new_user;
    `String [%blob "../static/user.html"] |> respond'
  end

let log_boner = post "/log/" begin fun req ->
    App.urlencoded_pairs_of_body req >>= fun params ->
    let post_param name = List.assoc name params |> List.hd in
    let username  = post_param "username"
    and longitude = float_of_string @@ post_param "longitude"
    and latitude  = float_of_string @@ post_param "latitude" in
    let user = Db.get_user username in
    let boner = create_boner ~longitude ~latitude in
    add_boner boner user |> Db.put_user;
    `String [%blob "../static/user.html"] |> respond'
  end

let index = get "/" begin fun req ->
    `String [%blob "../static/index.html"] |> respond'
  end

let app =
  App.empty
  |> index
  |> user_info
  |> new_user
  |> log_boner

let () = App.run_command app
