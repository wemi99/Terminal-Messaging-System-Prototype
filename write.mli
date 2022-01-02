(** 
    This modules handles the modification of json files to maintain changes
    in state. It also handles the creation of conversation json files when 
    necessary. These json files include logindetails.json, afp.json, pfp.json,
    and any conversation jsons.
*)


(**[id_creator name1 name 2] is a string that consists of the alphabetical 
   concatenating of [name1] and [name2]. *)
val id_creator : string -> string -> string

(** [json_creator id] is a string of [id]^".json". *)
val json_creator : string -> string

(** [editingtext_json sent_by text id] is a unit returning function that 
    determines if [id] already exists and then saves [sent_by] and [text] to 
    corresponding json. *)
val editingtext_json : string -> string -> string -> unit

(** [afp_add new_contact] is a file writing function that adds [new_contact] 
    to afp.json.*)
val afp_add : string -> unit

(** [afp_empty new_contact] is a unit that adds [new_contacts] to 
    afp.json and afp.json does not contain existing friend pairs.  *)
val afp_empty : string -> unit

(** [pfp_add new_contact] is a unit that adds [new_contacts] to 
    pfp.json if pfp.json already contains existing friend pairs.  *)
val pfp_add : string -> unit

(** [pfp_empty new_contact] is a unit that adds [new_contacts] to 
    pfp.json and pfp.json does not contain existing friend pairs.  *)
val pfp_empty : string -> unit

(** [pfp_remove to_remove] is a unit that takes in given string
    and removes it from "pfp.json". *)
val pfp_remove : string -> unit

(** [account_json_add username password] is a unit that adds [password] and 
    [username] pair to logindetails.json. *)
val account_json_add : string -> string -> unit
