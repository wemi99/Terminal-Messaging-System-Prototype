open Command
open State
open Read
open Write

(** [reset_screen] is a unit that clears the screen and resets the terminal 
    cursor to point back to the top of the screen. *)
let reset_screen () = 
  ANSITerminal.erase Screen;
  ANSITerminal.set_cursor 1 1

(** [print_contacts st con_list] is a unit that prints every element of 
    con_list that is not the current user id. *)
let rec print_contacts st con_list =
  match con_list with
  |[] -> print_string ""
  |hd::tl-> 
    if hd = get_current_user st then print_contacts st tl
    else
      (print_endline hd; 
       print_contacts st tl)

(** [output_sender_line text] is a unit that prints the sender's [text] to the 
    terminal in the below format:
    "Me: [text]", such that this entire line is right justified and highlighted
    in color. *)
let output_sender_line text =
  let (w,h) = ANSITerminal.size() in
  let length = String.length text + 4 in
  ANSITerminal.move_cursor (w-length) 0;
  ANSITerminal.(print_string [on_blue]("Me: " ^ text));
  print_endline("");
  print_endline("")

(** [output_receiver_line username text] is a unit that prints the receiver's 
    [text] to the terminal in the below format:
    "[username]: [text]", such that this entire line is left justified and
    higlighted in color. *)
let output_receiver_line username text =
  ANSITerminal.(print_string [on_magenta] (username ^ ": " ^ text));
  print_endline("");
  print_endline("")

(** [print_convo t] is a unit that outputs each text message in t, separating 
    the user name and text body components by color. *)
let rec print_convo texts st =
  match texts with
  |[]->print_string ""
  |x::xs-> 
    let sender = get_sent_by x in
    let msg = get_text x in 
    if (sender = get_current_user st) then 
      (output_sender_line msg; print_convo xs st)
    else 
      (output_receiver_line sender msg; (print_convo xs st))

(** [print_help] is a unit that prints commands used to help user
    navigate system.*)
let print_help() =
  ANSITerminal.(print_string [white]
                  "\nHere's all you need to know to run our system:\n\n");
  ANSITerminal.(print_string [red]
                  "/quit\nQuit the program.\n");
  ANSITerminal.(print_string [magenta]
                  "\n/back\nGo to the previous menu.\n");
  ANSITerminal.(print_string [yellow]
                  "\n/signup\nCreate a new account.\n");
  ANSITerminal.(print_string [green]
                  "\n/connect\nView friend requests. Type add, accept, or deny \
                   followed by a name to interact with requests.\n");
  ANSITerminal.(print_string [blue]
                  "\n/help\nBring up this menu.\n");
  ANSITerminal.(print_string [white]
                  "\n>> ")

(** [print_login] is a unit that prints the login menu opening text. *)
let print_login () =
  ANSITerminal.(print_string [green]
                  "\nWelcome to the login page. What is your username? Enter \
                   /help to view a description of valid commands.\n>> ")

(** [print_login_password st] is a unit that prints a message asking for the 
    current user's password. *)
let print_login_password st  = 
  ANSITerminal.(print_string [green]
                  ("\nWelcome " ^ get_current_user st ^ "! \
                                                         What is your password?
                                                         \n>> "))

(** [print_new_username st] is a unit that prints a message asking for a new 
    username. *)
let print_new_username st = 
  ANSITerminal.(print_string [blue]
                  "\nPlease enter a new one-word username. \
                   This is how others will see you on the service.\n>> ")

(** [print_new_password st] is a unit that prints a message asking for a new 
    password. *)
let print_new_password st = 
  ANSITerminal.(print_string [blue] "\nPlease create a one-word password.\n>> ") 

(** [print_plaza st] is a unit that prints a message asking who the user 
    would like to chat with, along with the user's friends. *)
let print_plaza st =                      
  ANSITerminal.(print_string [cyan] "\nWho would you like to chat with? \
                                     Type /back to log out.\n");
  st |> get_current_contacts |> print_contacts st;
  ANSITerminal.(print_string [cyan] "\n>> ")

(** [print_connect st] is a unit that prints a message instructing the user
    on how to deal with friend requests. *)
let print_connect st =
  let friend_request_num = 
    st |> get_current_user |> get_pending_friends |> List.length in
  if friend_request_num = 0 then
    ANSITerminal.(print_string [yellow]
                    "\nYou haven't received any new friend requests yet.\n\
                     Type add followed by a name to send a friend request!\n\
                     When you have received a request, you can type accept or \
                     deny followed by that person's name.
                     \n>> ")
  else if friend_request_num = 1 then
    (ANSITerminal.(print_string [yellow] 
                     "\nYou have a pending friend request from:\n"); 
     st |> get_current_user |> get_pending_friends |> print_contacts st;           
     ANSITerminal.(print_string [yellow] "\n>> "))
  else 
    (ANSITerminal.(print_string [yellow] 
                     "\nYou have pending friend requests from:\n"); 
     st |> get_current_user |> get_pending_friends |> print_contacts st;
     ANSITerminal.(print_string [yellow] "\n>> "))

(** [print_successful_add str] is a unit that prints a message after a user 
    has succesfullyy sent a friend request to [str]. *)
let print_successful_add str = 
  ANSITerminal.(print_string [yellow] ("\n"^str^" has been sent a friend \
                                                 request. We'll let you know \
                                                 as soon as they accept!\n>> "))

(** [print_successful_accept str] is a unit that prints a message after a user 
    has succesfully accepted a friend request from [str]. *)
let print_successful_accept str = 
  ANSITerminal.(print_string [green]
                  ("\nYou're now friends with "^str^"! If you want to start a \
                                                     new conversation with \
                                                    "^str^", type /back \
                                                           to view your \
                                                           contacts."));
  ANSITerminal.(print_string [yellow] "\n\n>> ")

(** [print_successful_deny str] is a unit that prints a message after a user 
    has succesfully denied a friend request from [str]. *)
let print_successful_deny str = 
  ANSITerminal.(print_string [green]
                  ("\nYou denied "^str^"'s friend request."));
  ANSITerminal.(print_string [yellow] "\n\n>> ")

(** [print_whole_chat st] is a unit that prints the conversation between the 
    user and the receiver. *)
let print_whole_chat st=
  print_string "\n";
  reset_screen();
  print_convo (st |> get_current_chat |> List.rev) st;
  ANSITerminal.(print_string [white] "\n>> ")

(** [print_new_message st] is a unit that prints the most recent
    text message. *)
let print_new_message st=
  (match (get_current_chat st) with
   |[]->print_string ""
   |h::xs-> 
     ANSITerminal.move_cursor 0 (-2);
     ANSITerminal.erase Below;
     let msg = get_text h in 
     output_sender_line msg; 
     ANSITerminal.(print_string [white] "\n>> "))

(** [print_exception_message msg] is a unit that prints a formatted exception 
    message that says [msg]. *)
let print_exception_message msg = 
  ANSITerminal.(print_string [red] ("\n"^msg)); 
  ANSITerminal.(print_string [red] "\n>> ")

(** [print_invalid_contact] is a unit that prints a formatted message for 
    when a user tries to chat with someone who is not their friend. *)
let print_invalid_contact () = 
  ANSITerminal.(print_string [magenta]
                  "\nYou don't have a contact with \
                   that name. Type /connect to add \
                   new contacts, or chat with a \
                   friend!\n>> ")

(** [print_invalid_username] is a unit that prints a formatted message for when
    a user tries to login with an unregistered username. *)
let print_invalid_username () = 
  ANSITerminal.(print_string [magenta]
                  ("\nThat username doesn't exist. \ Type " 
                   ^ {|"/signup" |} ^ "to create a new account. \n>> "))

(** [print_sign_up] is a unit that prints a formatted message for when a user 
    succesfully creates a new account. *)
let print_sign_up () = 
  ANSITerminal.(print_string [green]
                  "You've successfully created your new account! \
                   Welcome!\n")
(** [print_username_taken] is a unit that prints a formatted message for when a
    username tries to sign up with a username that has already been registered.
*)
let print_username_taken () = 
  ANSITerminal.(print_string [magenta]
                  "\n\nSorry! That username is already \
                   taken! Try typing another!\n\n>> ")

(** [transition st] is a unit that reads line and matches against the resulting
    command given [st]. If the command is empty, malformed, or invalid, a 
    warning message is printed and transition is run again. 
    Otherwise, transition prints an appropriate message and runs 
    again with the updated state. 
*)
let rec transition st = 
  try (
    let menu = st |> get_current_menu in 
    let command = (parse (get_menu_id menu)
                     (get_current_user st) (read_line ())) in
    match command with
    | Current -> print_help(); transition st
    | Sign_Up -> 
      (match change_state "create account" st with 
       | Valid t -> reset_screen(); print_new_username st; transition t
       | Invalid -> failwith "should not get here")
    | New_Username str -> 
      (match change_state str st with 
       | Valid t -> print_new_password st; transition t
       | Invalid -> print_username_taken(); transition st)
    | New_Password str -> 
      (match change_state str st with 
       | Valid t -> 
         reset_screen(); print_sign_up(); print_login (); transition t 
       | Invalid -> failwith "should not get here ")
    | Login_As str -> 
      (match change_state str st with
       | Valid t -> print_login_password t; transition t
       | Invalid -> print_invalid_username(); transition st)
    | Login_Password str ->
      (match change_state str st with
       | Valid t -> 
         reset_screen();
         ANSITerminal.(print_string [green] "\nYou are now logged in!\n");
         print_plaza t; 
         transition t
       | Invalid ->  
         ANSITerminal.(print_string [red] ("\nIncorrect password.\n>> ")); 
         transition st)
    | Chat_With str ->
      (match change_state str st with
       | Valid t -> print_whole_chat t; transition t
       | Invalid -> print_invalid_contact(); transition st)
    | Send str ->
      (match change_state str st with
       | Valid t -> print_new_message t; transition t
       | Invalid -> failwith "should not happen send")
    | Open_Requests ->
      (match change_state "open_requests" st with 
       | Valid t -> print_connect t; transition t
       | Invalid -> failwith "should not happen open requests") 
    | Move_Request (tag,str) -> 
      (match interact_with_request tag str st with
       | PValid t -> 
         if tag = "add" then print_successful_add str else 
         if tag = "accept" then print_successful_accept str else 
         if tag = "deny" then print_successful_deny str;
         transition t
       | Invalid_Unrecognizable -> 
         ANSITerminal.(print_string [magenta]
                         "\nYou must type add, accept, or \
                          deny followed by a name.\n>> ");
         transition st
       | Invalid_Add_Already_Friended -> 
         ANSITerminal.(print_string [magenta]
                         "\nYou are already friends with this user. \n>> ");
         transition st
       | Invalid_Add_Already_Added -> 
         ANSITerminal.(print_string [magenta]
                         "\nYou have already added this user. \n>> ");
         transition st
       | Invalid_Add_Pending-> 
         ANSITerminal.(print_string [magenta]
                         "\nYou have already received a friend request from \
                          this user! Type accept <name> to accept the request.\
                          \n>> ");
         transition st
       | Invalid_Add_Self -> 
         ANSITerminal.(print_string [magenta]
                         "\nYou can't send a friend request to yourself.\
                          \n>> ");
         transition st
       | Invalid_Existless -> 
         ANSITerminal.(print_string [magenta]
                         "\nInvalid command. Try something else.\n>> ");
         transition st)
    | Back ->
      (match go_back st with
       | Valid t -> 
         let next_menu_id = t |> get_current_menu |> get_menu_id in
         reset_screen();
         (if next_menu_id = "login" then print_login () else
          if next_menu_id = "plaza" then print_plaza t else
          if next_menu_id = "sign_up_username" then print_new_username t else
            exit 0);
         transition t
       | Invalid -> failwith "should not happen")
    | Quit -> exit 0 )
  with 
  | Empty_Login_Id -> 
    print_exception_message "Whoops! You didn't enter anything. \
                             Enter a valid username to login!"; 
    transition st
  | Empty_Login_Password -> 
    print_exception_message  "You entered a blank password! Try again";
    transition st 
  | Empty_Chat_With_Id -> 
    print_exception_message "Whoops! You can't chat someone with no name!!";
    transition st 
  | Empty_Send -> 
    print_exception_message "Uh oh..you didn't type anything to send! \
                             Try something more meaningful.";
    transition st
  | Empty_New_Username -> 
    print_exception_message "Your username can't be empty.";
    transition st 
  | Empty_New_Password ->
    print_exception_message "Whoops! You didn't enter anything. \
                             Enter a new one-word password to login!";
    transition st
  | Empty_Connect ->  
    print_exception_message  "You must type add, accept, or deny followed by a \
                              name.";
    transition st
  | Malformed_Login_Id -> 
    print_exception_message "Usernames can't have any spaces. Try again!";
    transition st
  | Malformed_Login_Password -> 
    print_exception_message "Passwords can't have spaces. Try again!.";
    transition st
  | Malformed_Chat_With ->
    print_exception_message "Uh oh that person doesn't exist. Try one of your \
                             existing contacts!";
    transition st
  | Malformed_Chat_With_Self -> 
    print_exception_message "You can't chat with yourself! Silly.";
    transition st
  | Malformed_New_Username ->  
    print_exception_message "Your username can only be one word, no spaces.";
    transition st 
  | Malformed_New_Password ->  
    print_exception_message "Your password can only be one word, no spaces.";
    transition st 
  | Malformed_Connect -> 
    print_exception_message "You must type add, accept, or deny followed by \
                             a name.";
    transition st

(** [main ()] is a unit that prompts for the instant messaging interface to 
    play, then starts it. *)
let main () =
  reset_screen();
  let state = init_state in
  print_login ();
  transition state 

(* Execute the game engine. *)
let () = main ()
