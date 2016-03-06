open UserData

open Gensqlite_tools

let db = Sqlite3.db_open "data/users.db"

module Raw = struct
  let (_, get_user) = [%gensqlite db "SELECT @s{firstname}, @s{lastname}, @s{username}, @s{boners}\
                                      FROM users WHERE username = %s{username}"]
  let (_, put_user) = [%gensqlite db "INSERT OR REPLACE INTO users (firstname, lastname, username, boners)\
                                      VALUES (%s{firstname}, %s{lastname}, %s{username}, %s{boners})"]
end

let get_user username =
  let user = Raw.get_user ~username () in
  let elems = function
    | `List xs -> xs
    | _ -> [] in
  match user with
  | [] -> create_user ~firstname:"error" ~lastname:"error" ~username
  | (firstname, lastname, _, boners)::_ ->
    {(create_user ~firstname ~lastname ~username) with
     boners = Yojson.Safe.from_string boners
              |> elems |> List.map boner_of_json}

let put_user {firstname; lastname; username; boners} =
  ()
