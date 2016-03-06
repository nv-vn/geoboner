open Opium.Std

open UserData

let user_info = get "/user/:username" begin fun req ->
    let username = param req "username" in
    let user = Db.get_user username in
    let json = json_of_user user |> Yojson.Safe.to_string in
    `String json |> respond'
  end

let index = get "/" begin fun req ->
    `String [%blob "../static/index.html"] |> respond'
  end

let app =
  App.empty
  |> index
  |> user_info

let () = app |> App.run_command
