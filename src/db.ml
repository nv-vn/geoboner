open UserData

open Gensqlite_tools

let db = Sqlite3.db_open "data/users.db"

module Raw = struct
  let (_, get_user) = [%gensqlite db "SELECT @s{firstname}, @s{lastname}, @s{boners}\
                                      FROM users WHERE username = %s{username}"]
  let (_, new_user) = [%gensqlite db "INSERT INTO users (firstname, lastname, username, boners) VALUES\
                                      (%s{firstname}, %s{lastname}, %s{username}, %s{boners})"]
  let (_, put_user) = [%gensqlite db "UPDATE users SET firstname=%s{firstname}, lastname=%s{lastname},\
                                      username=%s{username}, boners=%s{boners} WHERE username=%s{username}"]
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

let new_user {firstname; lastname; username; boners} =
  let boners' = List.map json_of_boner boners in
  let boners = `List boners' |> Yojson.Safe.to_string in
  Raw.new_user ~firstname ~lastname ~username ~boners ()

let put_user {firstname; lastname; username; boners} =
  let boners' = List.map json_of_boner boners in
  let boners = `List boners' |> Yojson.Safe.to_string in
  Raw.put_user ~firstname ~lastname ~username ~boners ()
