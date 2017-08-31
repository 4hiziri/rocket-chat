# rocket-chat
Emacs Rocket.chat client.

## Installation
### Cask
Please Add line to Cask file.
```
(depends-on "rocket-chat" :git "https://github.com/4hiziri/rocket-chat")
```

## Usage
`M-x rocket-chat`

You need enter server url, user name and password.
Then you can see some channels. 
You can enter channel by C-m on channel name.

Key bind  
+ `C-c C-l`
  - Return channel list.
+ `C-c C-n`
  - If new post exists, fetch it.
+ `C-c C-u`
  - Show user list with their connection status.


