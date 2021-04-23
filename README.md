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

### Running a server

Launch the program, then at the prompt, enter `server`.

### Running a client 

Launch the program, then at the prompt, enter `client`.

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

- [ ] Program loads existing "remembered" bindings `(name, IP:port)` from address book file
  - [ ] If there is no address book file, program creates one
- [ ] Client: Users can opt to input a name from the address book instead of typing an IP address and port 
- [ ] Users can create new entries to the address book, which are saved to the file and also loaded into memory (immediately use-able)
- [ ] Users can delete existing entries from the address book

#### Additional Features

- [ ] Users can transfer files through the program
- [ ] Program displays user's saved name (if available) when a message comes in, rather than just "server" or "client".

## License

This software is available under the GNU General Public License (GPL) v3.0 (see `LICENSE`), as mandated by the license of our compiler, [GNAT Community Edition](https://www.adacore.com/community).

## Attribution

Code for the actual RSA encryption is borrowed from [this library](https://github.com/andrewkiluk/RSA-Library).
