(*****************************  TEST PLAN  **********************************)
(*
   OUnit Tested Modules: Read***, Command
   Manually Tested Modules: Write, State, Main
   **** Note: In Read we manually tested 3 functions that could not be
              tested with OUnit:
              1) get_accepted_friends
              2) get_pending_friends 
              3) user_exists 
        These functions directly manipulate/check either afp.json, pfp.json, 
        or login_details.json, which are "global" json files that keep track of 
        registered accounts, accepted friend requests, and pending friend 
        requests. Because these functions actively retrieve or check json files 
        that are constantly changing during run time, it isn't possible to test 
        them using the OUnit method. 

  We implemented a glass box approach coupled with manual testing to catch any 
  errors in our code. We used OUnit test cases to seek statement and condition 
  coverage in our glass box testing, which we applied to our Read, State, 
  and Command modules. Through our test cases, we explicitly tested all 
  functions in those modules that featured types we could readily add in our 
  make OUnit test functions. We were also sure to implicitly test functions 
  that received or outputted unexposed types when possible. For instance, in 
  testing [get_sent_by] and [get_text] with test cases, we implictly tested 
  [convo_from_json], a function which outputs the unexposed type [convo]. 

  In adopting the glass box testing approach, we examined the specifications of
  each tested function as well as the program logic itself. For example, 
  in testing of the [parse] function, we were sure to run every statement within
  [parse]. To test the functions within the Command module, we created test json
  files which were complete with test users and conversations. We then extracted
  Yojson.Basic.t values from those files to test our Command functions. 

  However, we were unable to use OUnit test cases to test several functions
  which were responsible for reading and editing json files. Since our project 
  relies upon storing data within json files, the contents of those files 
  constantly change to reflect new messages sent between users, friends 
  requests sent and denied, etc. For instance, functions which examine the 
  contents of files such as "afp.json" or "logindetails.json" cannot be expected 
  to output the same result each time they are called. Unlike the functions in 
  Read, we could not create test jsons for those functions because the names of 
  the files being examined were explicitly written in their function bodies. 
  Similarly, we could not test functions which created new json files because
  our test module cannot be expected to generate new, distinctly named files for 
  each run of make test. Thus, our team decided to adopt a manual testing 
  approach to ensure that theprocesses governed by such functions run correctly.

  The following is a list of actions we manually performed to test our code:
    1) Account creation (SignUpUsername/SignUpPassword)
      - Create account succesfully with valid username & password
      - Error message when creating an account w/ username that already exists
      - Error message when creating account w/ empty username/password
      - Error message when creating account with username/password that is more 
        than one word 
    2) Login/LoginVerify
      - Login succesfully with valid username & password
      - Error message when entering a username/password with a space (malformed)
      - Error message when logging in w/ an invalid username or invalid password
      - Error message when logging in w/ an empty password or empty password
    3) Connect 
      - Succesfully adding a friend who has a registered account
      - Error message when adding a friend who is not registered 
      - Error message when adding a friend to who you already added previously
      - Error message when trying to add yourself as a friend 
      - Succesfully accept, receive, deny friend requests from someone 
    4) Plaza/Chat
      - Error message when trying to chat with yourself 
      - Error message when trying to chat with a registered account who isn't
        your friend
      - Error message when trying to chat with an unregistered username 
      - Initiated conversations with friends to ensure that messages were being
        stored and printed properly. 
    5) Quit/Back/Help Commands
      - Successfully used /back from each menu. Takes you back to the previous
        menu. In the Login menu, /back quits the program. 
      - Succesfully used /quit from each menu. /quit should exit the program. 
      - Successfully used /help command from each menu 

Our OUnit test module, combined with comprehensive manual testing, guarantees 
that our instant messaging interface is correct on tested inputs and in tested 
environments. Neverthless, we do recognize that testing does guarantee 
correctness on all inputs and environments, only tested ones. However, as 
detailed above, we extensively test for different possible environments and 
inputs, and thus conclude that the probability of undetected faults in our 
system is low enough to demonstrate the correctness of our system. 
*)

open OUnit2
open Write
open Read
open State
open Command

let identity x = x

(************************** Read Tests  ******************************)

let yojsont1 = Yojson.Basic.from_file "testconvo.json"
let yojsont2 = Yojson.Basic.from_file "testaccounts.json"
let convo1 =  "testconvo.json" |> Yojson.Basic.from_file  |> convo_from_json
let accounts1 = "testaccounts.json" 
                |> Yojson.Basic.from_file  
                |> accounts_from_json

let yojsontafp = Yojson.Basic.from_file "testafp.json"
let yojsontpfp = Yojson.Basic.from_file "testpfp.json"
let yojsontafp_empty = Yojson.Basic.from_file "testafp2.json"
let yojsontpfp_empty = Yojson.Basic.from_file "testpfp2.json"

(** [make_get_sent_by_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_sent_by input]. *)
let make_get_sent_by_test 
    (name : string) 
    (input: message) 
    (expected_output : string ) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_sent_by input ) ~printer: identity) 

(** [make_get_text_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_text input]. *)
let make_get_text_test 
    (name : string) 
    (input: message) 
    (expected_output : string ) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (get_text input) ~printer: identity) 

(** [make_get_username_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_username input]. *)
let make_get_username_test 
    (name : string) 
    (input: account) 
    (expected_output : string ) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (get_username input) ~printer: identity)

(** [make_get_password_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_password input]. *)
let make_get_password_test 
    (name : string) 
    (input: account) 
    (expected_output : string ) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (get_password input) ~printer: identity)

(** [make_is_verified_password_test name user pwd actlist expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [is_verified_password user pwd actlist]. *)
let make_is_verified_password_test 
    (name : string) 
    (user: string) 
    (pwd: string) 
    (actlist: account list)
    (expected_output : bool ) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (is_verified_password user pwd actlist) ~printer: string_of_bool)

(** [make_accepted_friend_pairs_from_json_test name input expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [accepted_friend_pairs_from_json input]. *)
let make_accepted_friend_pairs_from_json_test 
    (name : string) 
    (input: Yojson.Basic.t) 
    (expected_output : string list) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (accepted_friend_pairs_from_json input))

(** [make_pending_friend_pairs_from_json_test name input expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [pending_friend_pairs_from_json input]. *)
let make_pending_friend_pairs_from_json_test 
    (name : string) 
    (input: Yojson.Basic.t) 
    (expected_output : string list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pending_friend_pairs_from_json input))

let read_tests = [
  make_get_sent_by_test "get sent_by 1st sender" (List.nth convo1 0)
    "user2";
  make_get_sent_by_test "get sent_by 2nd sender" (List.nth convo1 1)
    "user2";
  make_get_sent_by_test "get sent_by last sender" (List.nth convo1 4)
    "user1";
  make_get_text_test "get text 1st sender" (List.nth convo1 0) "how about you";
  make_get_text_test "get text 2nd sender" (List.nth convo1 1) "nothing";
  make_get_text_test "get text last sender" (List.nth convo1 4) "hello there";
  make_get_username_test "get username first account" (List.nth accounts1 0) 
    "testuser3";
  make_get_username_test "get username 2nd account" (List.nth accounts1 1) 
    "testuser2";
  make_get_username_test "get username 3rd account" (List.nth accounts1 2) 
    "testuser1";
  make_get_password_test "get password first account" (List.nth accounts1 0) 
    "testpassword3";
  make_get_password_test "get password 2nd account" (List.nth accounts1 1) 
    "testpassword2";
  make_get_password_test "get password 3rd account" (List.nth accounts1 2) 
    "testpassword1";
  make_is_verified_password_test "valid username-password combo 1" "testuser1"
    "testpassword1" accounts1 true;
  make_is_verified_password_test "valid username-password combo 2" "testuser2"
    "testpassword2" accounts1 true;
  make_is_verified_password_test "valid username-password combo 3" "testuser3"
    "testpassword3" accounts1 true;
  make_is_verified_password_test "invalid username-password combo 1" "testuser1"
    "testpassword2" accounts1 false;
  make_is_verified_password_test "invalid username-password combo 2" "testuser2"
    "testpassword3" accounts1 false;
  make_is_verified_password_test "valid username-password combo 3" "testuser3"
    "falalalal" accounts1 false;
  make_accepted_friend_pairs_from_json_test "checking accepted friend pairs" 
    yojsontafp ["testuser1&testuser2";"testuser2&testuser3"];
  make_pending_friend_pairs_from_json_test "checking pending friend pairs" 
    yojsontpfp ["testuser1&testuser2&testuser1";"testuser1&testuser3&testuser3";
                "testuser2&testuser3&testuser2"];
  make_accepted_friend_pairs_from_json_test "checking empty accepted friend 
    pairs" yojsontafp_empty [];
  make_pending_friend_pairs_from_json_test "checking empty pending friend pairs" 
    yojsontpfp_empty [];
]

(***************************  Command Tests  *********************************)

(** [make_parse_test name menu_id username input expected_output] constructs 
    an OUnit test named [name] that asserts the quality of [expected_output]
    with [parse menu_id username input]. *)
let make_parse_test
    (name : string) 
    (menu_id: string ) 
    (username : string)
    (input : string)
    (expected_output : command)  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse menu_id username input) )

(** [make_raise_exn_parse_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [parse input]. *)
let make_raise_exn_parse_test 
    (name : string)
    (menu_id: string ) 
    (username : string)
    (input: string)
    (expected_exception : exn) : test = 
  name >:: (fun _ -> 
      assert_raises expected_exception (fun () -> parse menu_id username input))

let command_parse_tests = [
  make_parse_test "help menu command" "login" "username" "/help" Current;
  make_parse_test "quit command" "login" "username" "/quit" Quit;
  make_parse_test "back command" "login" "username" "/back" Back;
  make_parse_test "signup command" "login" "username" "/signup" Sign_Up;
  make_parse_test "login as command" "login" "username" "testuser" 
    (Login_As "testuser");
  make_parse_test "login password command" "password_verification" "username" 
    "key" (Login_Password "key");
  make_parse_test "new username command" "sign_up_username" "username" "userid" 
    (New_Username "userid");
  make_parse_test "new password command" "sign_up_password" "username" "key" 
    (New_Password "key");
  make_parse_test "connect command" "plaza" "username" "/connect" Open_Requests;
  make_parse_test "chat with command" "plaza" "username" "userid" 
    (Chat_With "userid");
  make_parse_test "add command" "connect" "username" "add user1"
    (Move_Request("add","user1"));
  make_parse_test "accept command" "connect" "username" "accept user1"
    (Move_Request("accept","user1"));
  make_parse_test "deny command" "connect" "username" "deny user1"
    (Move_Request("deny","user1"));
  make_parse_test "connect command" "plaza" "username" "/connect" Open_Requests;
  make_parse_test "chat command" "chat" "username" "how are you doing"
    (Send "how are you doing");
  make_parse_test "chat command" "chat" "username" "hello" (Send "hello");
]

let command_exn_tests = [
  make_raise_exn_parse_test "Empty 1: Parse empty string in Login menu" "login" 
    "don't matter" "" Empty_Login_Id;
  make_raise_exn_parse_test "Empty 2: Parse empty string in LoginVerify menu" 
    "password_verification" "don't matter" "" Empty_Login_Password;
  make_raise_exn_parse_test "Empty 3: Parse empty string in SignUpUsername menu" 
    "sign_up_username" "don't matter" "" Empty_New_Username;
  make_raise_exn_parse_test "Empty 4: Parse empty string in SignUpPassword menu" 
    "sign_up_password" "don't matter" "" Empty_New_Password;
  make_raise_exn_parse_test "Empty 5: Parse empty string in Plaza menu" 
    "plaza" "don't matter" "" Empty_Chat_With_Id;
  make_raise_exn_parse_test "Empty 6: Parse empty string in Chat menu" 
    "chat" "don't matter" "" Empty_Send;
  make_raise_exn_parse_test "Empty 7: Parse empty string in Connect menu" 
    "connect" "don't matter" "" Empty_Connect;
  make_raise_exn_parse_test "Malformed 1: multi word username in Login"
    "login" "don't matter" "three words eee" Malformed_Login_Id;
  make_raise_exn_parse_test "Malformed 2: multi word password in LoginVerify"
    "password_verification" "don't matter" "three words eee"
    Malformed_Login_Password;
  make_raise_exn_parse_test "Malformed 3: multi word username in SignUpUsername"
    "sign_up_username" "don't matter" "la 1 la" Malformed_New_Username;
  make_raise_exn_parse_test "Malformed 4: multi word password in SignUpPassword"
    "sign_up_password" "don't matter" "la 1 la" Malformed_New_Password;
  make_raise_exn_parse_test "Malformed 5: multi word user to chat with in Plaza"
    "plaza" "don't matter" "jonathan sungkyun lee" Malformed_Chat_With;
  make_raise_exn_parse_test "Malformed 6: tries to chat with self in Plaza"
    "plaza" "jessica123" "jessica123" Malformed_Chat_With_Self;
  make_raise_exn_parse_test "Malformed 7: chat to yourself in Connect"
    "connect" "jessica123" "jessica" Malformed_Connect;
  make_raise_exn_parse_test "Malformed 8: chat to yourself in Connect"
    "connect" "jessica123" "if you're reading this hello :)" Malformed_Connect;

]

let suite =
  "test suite for ims"  >::: List.flatten [
    read_tests;
    command_parse_tests;
    command_exn_tests;
  ]

let _ = run_test_tt_main suite
