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

- [ ] Program generates keypair when started
- [x] Connection between two running instances of the program
- [x] Public key exchange/save when connected
  - Currently exchanging dummy data as placeholder for public keys
- [ ] Cleanly exits in the event of a dropped connection

#### Sending and Receiving Messages With Encryption (James)

- [ ] Software can encrypt and send messages over the network connection
- [ ] Software listens for incoming messages and decrypts them
- [ ] Concurrency -- can send and receive messages at the same time

#### Frontend - Terminal-based Conversation View (Adam)

- [x] When starting the app, users can pick `server` or `client`
- [ ] Server: Program prompts for, parses, and uses a user-specified port
- [ ] Client: Program prompts for, parses, and uses a user-specified IP address and port
- [ ] Terminal displays a scrolling view of (decrypted) incoming messages as they arrive
- [ ] Program offers a prompt where users can type in messages

#### Address Book (Adam)

- [ ] Program loads existing "remembered" bindings `(name, IP:port)` from address book file
  - [ ] If there is no address book file, program creates one
- [ ] Client: Users can opt to input a name from the address book instead of typing an IP address and port 
- [ ] Users can create new entries to the address book, which are saved to the file and also loaded into memory (immediately use-able)
- [ ] Users can delete existing entries from the address book

#### Additional Features

- [ ] Users can transfer files through the program
