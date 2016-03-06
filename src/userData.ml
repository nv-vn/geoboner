open Yojson.Safe

type boner = {
  longitude : float;
  latitude  : float;
  time      : float
}

let create_boner ~longitude ~latitude =
  {longitude; latitude; time = Unix.time ()}

let json_of_boner {longitude; latitude; time} =
  `Assoc ["longitude", `Float longitude;
          "latitude",  `Float latitude;
          "time",      `Float time]

let boner_of_json json =
  let rec get_field target = function
    | `Assoc (x::xs) when fst x = target -> snd x
    | `Assoc (x::xs) -> get_field target (`Assoc xs)
    | _ -> `Null in
  let float = function
    | `Float f -> f
    | _ -> 0.0 in
  let longitude = json |> get_field "longitude" |> float
  and latitude  = json |> get_field "latitude"  |> float
  and time      = json |> get_field "time"      |> float in
  {(create_boner ~longitude ~latitude) with time}

(** TODO: Profile picture, bio, linkedin contact *)
type user = {
  firstname : string;
  lastname  : string;
  username  : string;
  boners    : boner list
}

let create_user ~firstname ~lastname ~username =
  {firstname; lastname; username; boners = []}

let add_boner boner user =
  {user with boners = boner::user.boners }

let json_of_user {firstname; lastname; username; boners} =
  `Assoc ["firstname", `String firstname;
          "lastname",  `String lastname;
          "username",  `String username;
          "boners",    `List (List.map json_of_boner boners)]
