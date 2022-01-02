(**
   Parses the user's input into the correct command. Empty inputs and inputs
   with an incorrect number of words are returned as exceptions. Otherwise,
   a command is outputted depending on the current menu. Certain commands are
   accompanied by strings or a string tuple, which carry information on what the
   user typed in.
*)

(** The type [command] represents a player input which depends on the current
    menu. *)
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

(** Raised when an empty command is parsed when a user is in the Login menu. *)
exception Empty_Login_Id

(** Raised when an empty command is parsed when a user is in the LoginVerify 
    menu. *)
exception Empty_Login_Password

(** Raised when an empty command is parsed when a user is in the Plaza menu. *)
exception Empty_Chat_With_Id

(** Raised when an empty command is parsed when a user is in the Send menu. *)
exception Empty_Send

(** Raised when an empty command is parsed when a user is in the SignUpUsername
    menu. *)
exception Empty_New_Username

(** Raised when an empty command is parsed when a user is in the SignUpPassword
    menu. *)
exception Empty_New_Password

(** Raised when empty command is parsed in the Connect menu. *)
exception Empty_Connect

(** Raised when an input with more than one word is encountered in the 
    Login_As menu. *)
exception Malformed_Login_Id

(** Raised when an input with more than one word is encountered in the 
    Login_Password menu. *)
exception Malformed_Login_Password

(** Raised when an input with more than one word is encountered in the 
    Plaza menu. *)
exception Malformed_Chat_With

(** Raised when a malformed command is encountered in the Plaza menu. 
    Handles the specific case where you chat yourself. *)
exception Malformed_Chat_With_Self

(** Raised when an input with more than one word is encountered 
    in the New_Username menu.*)
exception Malformed_New_Username

(** Raised when an input with more than one word is encountered in the 
    New_Password menu. *)
exception Malformed_New_Password

(** Raised when an input with more than two words is encountered in the 
    Connect menu. *)
exception Malformed_Connect

(** [parse current_menu_id current_user_id str] parses a player's input [str]
    into a [command]
    given [menuid] and [userid] as follows. 
    Examples: 
    - [parse "how have you been?"] within the Chat menu is 
      [Send "how have you been?"].
    - [parse "Jessica"] within the Plaza menu is [Chat_With "Jessica"]. 
    - [parse "/back"] is [Back]. 

    Requires: [str] contains only letters, numbers, and space 
    characters.

    Raises: [Empty_<spec>] if [str] is the empty string or contains only spaces,
    where <spec> depends on the current menu. 

    Raises: [Malformed_<spec>] if the input is 
    malformed, where [spec] depends on the specific menu/case in which the input
    is encountered. An input is malformed if the input is more
    than one word in Login, LoginVerify, Plaza, SignUpUsername, or 
    SignUpPassword, or more than two words in Connect. *)
val parse : string -> string -> string -> command
