open Yojson
open Yojson.Basic.Util
open Write

type message = {
  sent_by:string;
  text:string;
}

type convo = message list

type account = {
  username:string;
  password:string;
}

let get_sent_by msg = msg.sent_by

let get_text msg = msg.text

let get_username act = act.username

let get_password act = act.password

(** [message_from_convo_json j] is a unit that parses data from [j] to type 
    message. *) 
let message_from_convo_json j ={
  sent_by = j |> member "sent_by" |> to_string;
  text = j |> member "text" |> to_string;
}
let convo_from_json yojsont = 
  yojsont |> member "text history" |> to_list |> 
  List.map message_from_convo_json

let accepted_friend_pairs_from_json yojsont = 
  yojsont |> member "accepted friend pairs" |> to_list |> List.map to_string

let pending_friend_pairs_from_json yojsont = 
  yojsont |> member "pending friend pairs" |> to_list |> List.map to_string

(** [expand slist] is a list of string lists, each of which 
    is formed by concatenating the elements of slist with the '&' character.*) 
let rec expand slist = 
  match slist with
  | [] -> []
  | hd::tl -> String.split_on_char '&' hd :: expand tl

(** [list_to_pair list] is a tuple representing the inputted two-element
    list. *) 
let rec list_to_pair list =
  match list with 
  |[] -> failwith "list must have only two elements"
  | hd :: hd2 :: tl -> (hd, hd2)
  | _ -> failwith "list must have only two elements"

(** [list_to triple list] is a tuple representing the inputted 
    three-element list. *) 
let rec list_to_triple list =
  match list with 
  | [] -> failwith "list must have only three elements"
  | hd :: hd2 :: hd3 :: tl -> (hd,hd2,hd3)
  | _ -> failwith "list must have only three elements"

(** [filter_pair s pairlist] is a list whose elements are formed from 
    [pairlist]. If an entry in an element of pairlist is equal to s,
     the other entry is added.  *) 
let rec filter_pair s pairlist = 
  match pairlist with
  | [] -> []
  | (a,b) :: tl -> if a = s then b::filter_pair s tl else if b = s 
    then a::filter_pair s tl else filter_pair s tl

(** [filter_triple s pairlist] is a list whose elements are formed from
    [triplelist]. If the first or second entry in an element of triple list is
    equal to s, the other entry is added. However, if the third entry is equal
    to s, the tuple is ignored. *) 
let rec filter_triple s triplelist = 
  match triplelist with
  | [] -> []
  | (a,b,c) :: tl -> 
    if c = s then filter_triple s tl else
    if a = s then b::filter_triple s tl else
    if b = s then a::filter_triple s tl else
      filter_triple s tl

let get_accepted_friends username =
  (* let slist = 
     accepted_friend_pairs_from_json (Yojson.Basic.from_file "afp.json") in
     slist |> expand |> List.map list_to_pair |> filter_pair username *)
  "afp.json" |> Yojson.Basic.from_file |> accepted_friend_pairs_from_json 
  |> expand |> List.map list_to_pair |> filter_pair username

let get_pending_friends username =
  (* let slist = 
     pending_friend_pairs_from_json (Yojson.Basic.from_file "pfp.json") in
     slist |> expand |> List.map list_to_triple |> filter_triple username *)
  "pfp.json" |> Yojson.Basic.from_file |> pending_friend_pairs_from_json |>
  expand |> List.map list_to_triple |> filter_triple username

(* [accounts_from_login_json j] is a unit that takes [j] and parses data to
   type account. *)
let account_from_login_json j ={
  username = j |> member "username" |> to_string;
  password = j |> member "password" |> to_string;
}

let accounts_from_json j = 
  j |> member "users" |> to_list |> List.map account_from_login_json

(* [usernames_from_accounts acclist] is a list of usernames from [acclist]. *)
let rec usernames_from_accounts acclist = 
  match acclist with
  | [] -> []
  | {username = usr; password = pwd}:: tl -> usr :: usernames_from_accounts tl

let user_exists usr =
  List.mem usr ("logindetails.json" |> Yojson.Basic.from_file 
                |> accounts_from_json |> usernames_from_accounts)

let rec is_verified_password usr pwd actlist = 
  match actlist with
  | [] -> failwith "Username not found in accounts database."
  | {username = username; password = password}:: tl -> 
    if username = usr then password = pwd else is_verified_password usr pwd tl


