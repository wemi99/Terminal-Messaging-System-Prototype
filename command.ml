type command = 
  | Sign_Up
  | New_Username of string
  | New_Password of string
  | Login_As of string
  | Login_Password of string
  | Chat_With of string
  | Send of string
  | Open_Requests
  | Move_Request of string * string
  | Back
  | Quit
  | Current

exception Empty_Login_Id
exception Empty_Login_Password
exception Empty_Chat_With_Id
exception Empty_Send
exception Empty_New_Username
exception Empty_New_Password
exception Empty_Connect

exception Malformed_Login_Id
exception Malformed_Login_Password
exception Malformed_Chat_With
exception Malformed_Chat_With_Self
exception Malformed_New_Username
exception Malformed_New_Password
exception Malformed_Connect

(** [remove_empty slst] is a string list with all empty strings in slst
    removed *)
let rec remove_empty slst =
  match slst with
  | [] -> []
  | hd::tl -> if hd = "" then remove_empty tl else hd :: remove_empty tl

(** [strip_list str] is a string list whose elements are words formed from str.
    A word is consecutive sequence of non-space characters. If str is
    empty, an Empty exception is raised *)
let strip_list str = 
  if str = "" then [] else
    str |> String.split_on_char ' ' |> remove_empty

(** [print_help] is a unit that prints commands used to help user
    navigate system.*)
let print_help() =
  ANSITerminal.(print_string [white]
                  "\nHere's some helpful tips for running our system:\n");
  ANSITerminal.(print_string [magenta]
                  "To quit or leave:\n/quit\n/back
                  \nTo create a new account:\n/signup\n
                  To see friend requests:\n/connect\n
                  To interact with friend requests:\nadd (friend's name)
                  \naccept (friend's name)
                  \ndeny (friend's name)")

let parse current_menu_id current_user_id str =
  let slist = strip_list str in
  match slist with
  | [] -> (if current_menu_id = "login" then raise Empty_Login_Id else
           if current_menu_id = "plaza" then raise Empty_Chat_With_Id else
           if current_menu_id = "password_verification" then 
             raise Empty_Login_Password else
           if current_menu_id = "sign_up_username" then 
             raise Empty_New_Username else
           if current_menu_id = "sign_up_password" then 
             raise Empty_New_Password else
           if current_menu_id = "connect" then raise Empty_Connect else 
             raise Empty_Send)
  | hd :: tl -> 
    if hd = "/help" then Current else
    if hd = "/quit" then Quit else
    if hd = "/back" then Back else
    if current_menu_id = "login" then 
      (if List.length slist <> 1  then raise Malformed_Login_Id else
       if hd = "/signup" then Sign_Up else
         Login_As hd) else
    if current_menu_id = "password_verification" then 
      (if List.length slist <> 1 then raise Malformed_Login_Password else
         Login_Password hd
      ) else
    if current_menu_id = "sign_up_username" then 
      (if List.length slist <> 1 then raise Malformed_New_Username else
         New_Username hd) else 
    if current_menu_id = "sign_up_password" then 
      (if List.length slist <> 1 then raise Malformed_New_Password else
         New_Password hd) else 
    if current_menu_id = "plaza" then
      (if hd = "/connect" then Open_Requests else
       if List.length slist <> 1 then raise Malformed_Chat_With else
       if hd = current_user_id then raise Malformed_Chat_With_Self else 
         Chat_With hd) else
    if current_menu_id = "connect" then 
      (if List.length slist <> 2 then raise Malformed_Connect else
       if hd = "add" then Move_Request ("add", List.hd tl) else
       if hd = "accept" then Move_Request ("accept", List.hd tl) else
         Move_Request ("deny", List.hd tl))
    else Send (String.concat " " (slist))
