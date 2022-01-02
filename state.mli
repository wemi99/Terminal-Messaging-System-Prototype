(** 
    Representation of dynamic instant messaging state.

    This module's type t represents the state of the instant messaging system 
    as it is being used, including the user's username, current menu, 
    conversation history/details, and contacts. 
    Furthermore, functions such as [change_state] and [interact_with_request]
    are responsible for returning a new state wrapped in the result type which
    means that corresponding json files are updated as well. 
*)

(** The abstract type of values representing the program state. *)
type t 

(** The type representing a menu window. *)
type menu

(** [init_state] is the initial state of the instant messaging system, which 
    represents the login menu. *)
val init_state : t

(** The type representing the result of an attempted transition. *)
type result = Valid of t | Invalid

(** The type representing the output of an attempted friend request send,
    accept, or deny. *)
type result_prime = PValid of t 
                  | Invalid_Existless 
                  | Invalid_Add_Already_Added
                  | Invalid_Add_Already_Friended 
                  | Invalid_Add_Pending
                  | Invalid_Add_Self
                  | Invalid_Unrecognizable

(** [get_current_menu st] is the menu of the instant messaging interface in 
    which the user is currently located in [st]. *)
val get_current_menu : t -> menu

(** [get_menu_id menu] is the identifier that correlates to [menu]. *)
val get_menu_id : menu -> string

(** [get_current_user st] is the username of the current user in [st]. *)
val get_current_user : t -> string

(** [get_current_contacts st] is the list of friends that the user can currently
    chat with in [st]. *)
val get_current_contacts : t -> string list

(** [get_current_chat st] is message list representing the conversation history 
    between the user and the person the user has chosen to chat with in [st]. *)
val get_current_chat : t -> Read.message list

(** [change_state input st] is [Valid t] if [input] from the current menu in 
    [st] is not invalid. An input can be invalid for several reasons:
    - the user is in the Login menu and inputs a username that doesn't exist
    - the user is in the LoginVerify menu and inputs wrong password 
    - the user is in the SignUpUsername menu and tries to create an account
        w/ a username that already exists
    - the user is in the Plaza and tries to chat with someone who isn't their 
        friend *)
val change_state : string -> t -> result 

(** [interact_with_request tag input t] is [Valid t] if [input] from the current
    menu is not invalid. Several invalid variations of result_prime can be 
    raised depending on the input and tag. For instance, if the user attempts 
    to send a friend request to a user who is already a friend, 
    Invalid_Add_Already_Friended will be outputted.*)
val interact_with_request : string -> string -> t -> result_prime

(** [go_back st] is [Valid t] where t represents the state after the user has
    returned to the previous menu. *)
val go_back : t -> result