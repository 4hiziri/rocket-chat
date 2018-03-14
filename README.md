# rocket-chat
Emacs Rocket.chat client.

## Installation
### Cask
Please add below dependency to Cask file.
```
(depends-on "rocket-chat" :git "https://github.com/4hiziri/rocket-chat")
```

## Usage
`M-x rocket-chat`

You need enter server url (like https://example.com), user name and password.
Then channels list shows up.

Key bind
+ `C-c C-l`
  - Return to channels list.
+ `C-c C-n`
  - If new post exists, fetch it.
+ `C-c C-u`
  - Show user list with their connection status.


