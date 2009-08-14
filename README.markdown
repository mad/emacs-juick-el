<meta http-equiv="content-type" content="text/html; charset=utf-8" />
# WHAT IT

This plugin add next features:

1. colored id/username/tag;
1. clickable id/username/tag:
    1. If press `RET` on id/username, then will been added in reply buffer;
    1. If press `RET` on tags, then printed last 10 message with tag;
    1. If press `s/u` on id/username, then you will been
       subscribed/unsubscribed on user/message;
    1. If press `p` on username, then will been added in reply buffer text
       type `PM @username`;
1. Geoloc (through PEP or message);
1. Tune (through PEP or message);
1. Mood (through message);
1. Upload images through IBB (IBB implement only one way - UPLOAD, DONT RECIEVE);
1. Show avatars;
1. Check bot commands, e.g. РУДЗ -> HELP, № -> # (usefull for russian users);
1. Subscribing on the tags;
1. Auto subscribing on tag or username;
1. Bookmarks.

[screenshot](http://img14.imageshack.us/img14/2484/juickwithavatar.jpg)

# HOW USE

### Navigation

- To move on message, you can use `M-e`, `M-a` (default emacs keybindings);
- `g` - on username or message id, open browser with;

Once you have replied to the message, ie click on the `RET` id, you can press
`C-u C-SPACE` (default emacs combination to navigate local labels) and
move to the position where you are (usefull when it comes a lot of messages and
as reading to answer them)

### Geoloc

Specify the geolocation data may be one of the ways:

1. Through PEP

What would send a message to the PEP geolocation data, press `C-cjp` (or
call `jabber-pep-location-send`)
further define the area (eg, "St. Petersburg"), after
what will be an attempt to find the coordinates of this place through google
maps, and sent to the server.
1. In a message if jabber server does not support the PEP, a report on
geolocation data can be directly in the message. Once you've typed the message,
press `C-cjg` (or `jabber-pep-location-send`), geolocation data indicate both
same as in the previous case.

### Tune

If you use emms, tune it to send messages, you can use the following code:

    (add-hook 'emms-player-started-hook
          '(lambda ()
             (let* ((emms-current-track (emms-playlist-current-selected-track)))
               (run-with-timer 10 nil 'jabber-pep-tune-send
                               (emms-track-get emms-current-track 'info-artist)
                               (number-to-string (or (emms-track-get emms-current-track 'info-playing-time) 0))
                               "0" ;;; use rating ?
                               (emms-track-get emms-current-track 'info-album)
                               (emms-track-get emms-current-track 'info-title)
                               (emms-track-get emms-current-track 'info-tracknumber)
                               ""))))

If you use any other player it is necessary to obtain data on
artist (via dbus etc) and use the command:

    emacsclient --eval "(jabber-pep-tune-send \"artist\" \"length\" \"rating\"
    \"source\" \"title\" \"track\" \"uri\")"

or (if the server does not support PEP):

    emacsclient --eval "(jabber-event-tune-send juick-bot-jid \"artist\" \"length\" \"rating\"
    \"source\" \"title\" \"track\" \"uri\")"

Note:

If the server does not support the PEP in the case of emms is used
`jabber-event-tune-send` (function includes an additional parameter - jid
user who will receive this event, ie `jid-juick-bot`.

### Mood

Before post message, press the `Mx jabber-mood-message` select
mood, and then `RET`, now this message will be sent to
the appropriate mood, the next message will be sent without a mood.

### Send images

To send pictures using the standard function of jabber-el --
`jabber-ft-send`

### Avatars

For display avatars responsible variable `juick-icon-mode`, if it is
value t, then they are displayed, if nil - not (default t).

Size avatars regulated by variable `juick-icon-hight`, if it
is set to t, to used size 96x96, if nil used 32x32 pixels (for
default nil).

By default avatars stored in `/ tmp/juick-images- <user name> /`, is responsible
for this variable is `juick-tmp-dir`.

### Subscribe on the tags

In order to subscribe to a tag, you need to add it to the list
`juick-tag-subscribed`, for example like this:

    (setq juick-tag-subscribed '("linux" "juick"))

and activate auto update:

    (juick-auto-update t)


To automatically subscribe to the message (S # NNNNN) with certain tags, or
name (S @ ABC) users, use the list
`juick-auto-subscribe-list`, for example:

    (setq juick-auto-subscribe-list '("linux" "juick" "ugnich"))

### Bookmarks

What would make the user or a message to your bookmarks - put the cursor on
message/username and press `b`.

View bookmarks - `C-cjb`;

Delete bookmark - `C-k`, `d` (in the bookmarks list);

By default, bookmarks are stored in `~ / .emacs.d / .juick-bkm`, is
responsible for this variable `juick-bookmark-file`.

### HOW INSTALL

Download the latest version:

    git clone git://github.com/mad/emacs-juick-el.git

Add the following lines to your initialization file:

    (add-to-list 'load-path "path/to/juick-el/")
    (require 'juick)

# NOTE

This plugin tested on jabber-el 0.7.82 and above.

**ATTENTION**

C jabber-el 0.7.1 and below the plugin does not work!

### KNOW ISSUE

- When sending a large image a "hang" emacs, due to the fact that
   IBB protocol is implemented in such a way that send image no
   asynchronously.

Request for bugs/features/etc write here [#104079](http://juick.com/mad/104079)
or mail.
