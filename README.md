# Terminal-Messaging-System-Prototype
(OCaml) Instant messaging service on systems with shared file systems that provides users with the means to have one-to-one conversations through their terminals.

Key features:
- The login page will prompt the user to login with a username and password or to
create an entirely new profile
- The user interface will display all the individuals that the user can send messages
to. This display will be partitioned into 2 groups: one group for active individuals
who are concurrently logged in, and another group for those who are not.
- Via the preserved chat history, a user will be permitted to view previous
messages sent between another user. If no chat history exists, the user can begin a
new conversation.
- A user who accidentally sends a message will be able to use the unsend option,
which will delete the message from the client server. This will prevent the
recipient from viewing the message as well as the user.
- A user can search for keywords or phrases within a conversation with another
user. If the search is found, the window will display the most recent message
containing that keyword or phrase, along with a certain number of messages that
were sent before and after that particular text. The message containing the
keyword or phrase will be highlighted.
- A user can also block another user, preventing the blocked individual from
messaging the user.


Narrative description:
- The instant messaging system that we will create will operate in the following
manner. The system will first prompt the user to login with a username and
password. There will also be an option to create a new account. If the user needs
to create a new account, he or she will be asked to create a new username and
password. If the server notices that a different individual already has that
username, the user will be prompted to create a different username.
- Once the user logs in with his or her login information, the program will present a
list of users who can be texted. This list will be split into two categories. At the
top of the list will be individuals who the user has previously chatted with, ranked
in chronological order. In other words, the individual who the user has
communicated with most recently will be at the top of the list, and so on. The
second category, titled other, will include the individuals who the user has yet to
chat with. Each name on the whole list will also be accompanied by a tag
notifying the user whether that individual is active or inactive. Active users are
those who are concurrently logged into the messaging service, and inactive users
are those who are not. If the user has received any new messages, the sender’s
name will be highlighted on the list.
- The user can decide to chat with any individual on this list, whether they are
active or not. To do so, the user can type “chat with <name>.” This will bring up
a new window in which previous texts sent back and forth with the chosen user
will be displayed. If there are no prior messages, no messages will be displayed.
The user can also block individuals using a “block <name>” command. The
system will prompt the user if they are sure that they want to block <name>, and
if the user proceeds, the system will block future communication with the
individual. The user will also be able to unblock blocked individuals.
- After the user sends a message to an active user, the server will alert the recipient
in real time. If the recipient is inactive, the server will alert the recipient when
they next log in.
- At any point, the user may opt to log out with the “log out” command. The user
will then be registered as inactive on any other user’s contacts window.
