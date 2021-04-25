# JARchat

End-to-end encrypted chat client written in Ada

## Building

In the root project directory, run:

```bash
gprbuild
```

## Usage

To run, execute

```bash
sudo obj/main
```

Superuser privileges are required in order to bind to ports.

At this point, the software will ask `(server/client) >`.
One end of the connection should start a `server` first, 
  and then the client should connect to the server.
To be clear: Two instances of the same software should be running.

At this point,
  the software will perform a connection handshake and key exchange,
  and either user can type and send messages at the prompt (`>`).

### Using the Address Book

When starting a client, the user is given the option to enter `book`
  or an IP address directly.
If you enter `book`, the Address Book CLI will launch,
  supporting the following commands:

- `add <name> <IP>`
- `edit <name> <New_IP>`
- `delete <name>`
- `contacts` - print out all the contacts
- `name` - just enter a name in contacts to connect to the associated IP address

To cleanly exit without specifying a name, use `exit`. The client will save any updates to `address_book.txt` in the root directory and connect to the default IP address.

### Sending and Receiving Files

Either the client or the server may at any time send the command `/file`.
This will first prompt the sender for the filepath to send (relative to the JARchat main directory).
Using file paths with more than just a filename in the JARchat directory causes issues.

The file will then be sent over the network,
  line by line, 
  encrypted using the same encryption as regular messages.

Received files are stored in the `inbox/` folder.

## Platform Support

Tested on:
- Ubuntu 20.10

## Features / Roadmap

#### Connection Handshake and Key Exchange (Radu)

- [x] Program generates keypair when started
- [x] Connection between two running instances of the program
- [x] Public key exchange/save when connected
- [x] Cleanly exits in the event of a dropped connection

#### Sending and Receiving Messages With Encryption (James)

- [x] Software can encrypt and send messages over the network connection
- [x] Software listens for incoming messages and decrypts them
- [x] Concurrency -- can send and receive messages at the same time

#### Frontend - Terminal-based Conversation View (Adam)

- [x] When starting the app, users can pick `server` or `client`
- [x] Client: Program prompts for, parses, and uses a user-specified IP address 
- [x] Terminal displays a scrolling view of (decrypted) incoming messages as they arrive
- [x] Program offers a prompt where users can type in messages

#### Address Book (Adam)

- [x] Program loads existing "remembered" bindings `(name, IP:port)` from address book file
  - [x] If there is no address book file, program creates one
- [x] Client: Users can opt to input a name from the address book instead of typing an IP address and port 
- [x] Users can create new entries to the address book, which are saved to the file and also loaded into memory (immediately use-able)
- [x] Users can delete existing entries from the address book

#### Additional Features

- [x] Users can transfer files through the program

## Known Bugs and Constraints

* Currently only text-based files can be sent via the file transfer feature. Especially long lines in text files also may overflow the receive buffer. 
* If sending files from anywhere besides the root directory, a destination path must already exist in `/inbox` (i.e., if sending the file `/src/crypto.adb`, `/inbox` must contain a directory named `/src`).

## License

This software is available under the GNU General Public License (GPL) v3.0 (see `LICENSE`), as mandated by the license of our compiler, [GNAT Community Edition](https://www.adacore.com/community).

## Attribution

Code for the actual RSA encryption is borrowed from [this library](https://github.com/andrewkiluk/RSA-Library).
