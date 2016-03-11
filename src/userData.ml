open Yojson.Safe

type boner = {
  latitude  : float;
  longitude : float;
  time      : float
}

let create_boner ~latitude ~longitude =
  {latitude; longitude; time = Unix.time ()}

let json_of_boner {latitude; longitude; time} =
  `Assoc ["latitude",  `Float latitude;
          "longitude", `Float longitude;
          "time",      `Float time]

let json_of_boners boners =
  `List (List.map json_of_boner boners)

let boner_of_json json =
  let rec get_field target = function
    | `Assoc (x::xs) when fst x = target -> snd x
    | `Assoc (x::xs) -> get_field target (`Assoc xs)
    | _ -> `Null in
  let float = function
    | `Float f -> f
    | _ -> 0.0 in
  let latitude  = json |> get_field "latitude"  |> float
  and longitude = json |> get_field "longitude" |> float
  and time      = json |> get_field "time"      |> float in
  {(create_boner ~latitude ~longitude) with time}

let format_time t =
  let open Unix in
  let time = gmtime t in
  let days = [| "Sunday"; "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"; "Saturday" |]
  and months = [| "January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December" |] in
  Printf.sprintf "At %d:%s%d %s (GMT) on %s, %s %d, %d\n"
    (time.tm_hour mod 12)
    (if time.tm_min < 10 then "0" else "")
    time.tm_min
    (if time.tm_hour > 12 then "PM" else "AM")
    days.(time.tm_wday)
    months.(time.tm_mon)
    time.tm_mday
    (time.tm_year + 1900)

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
