with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with GNAT.Sockets;
with Ada.Streams; use Ada.Streams;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Connection;
with Connection.Server;
with Connection.Client;

with Crypto;

-- with AddressBook;

-- Server:
--    - Server_Recv_Sock, Server_Recv_Channel
--    - Server_Send_Sock, Server_Send_Channel
--
-- Client:
--    - Client_Recv_Sock, Client_Recv_Channel
--    - Client_Send_Sock, Client_Send_Channel
--
-- Connections:
--    Server_Recv_Sock <--> Client_Send_Sock
--    Client_Recv_Sock <--> Server_Send_Sock
--
-- ============================================
--
-- Server sends messages to client by:             |  task: Server_Send
--    String'Write (Server_Send_Channel, Message)  |
--
-- Server receives messages by:                                      |  task: Server_Recv
--    Connection.Read_Until_Sentinel (Server_Recv_Channel, Output)   |
--
-- ============================================
--
-- Client sends messages to server by:             |  task: Client_Send
--    String'Write (Client_Send_Channel, Message)  |
--
-- Client receives messages by:                                      |  task: Client_Recv
--    Connection.Read_Until_Sentinel (Client_Recv_Channel, Output)   |
--
-- ============================================
--
-- Connection handshake:
--    - Server binds socket Server_Host_Sock on port $serverPort (wait for client)
--    - Client connects Client_Send_Sock to Server_Recv_Sock:$serverPort
--       - Server accepts that connection as socket Server_Recv_Connection
--    - Client binds socket Client_Host_Sock on port $clientPort
--    - Client sends $clientPort to the server by writing to Client_Send_Channel
--    - Server reads $clientPort
--    - Server connects Server_Send_Sock to the client
--
-- Key exchange:
--    - Server sends public key on Server_Send_Channel
--    - Client sends public key on Client_Send_Channel
--    - Each receives and stores
--
-- At this point, (use task entries?) start infinite loop for:
--    - Server_Send
--    - Server_Recv
--    - Client_Send
--    - Client_Recv
--

procedure Main is
    VERBOSE : Boolean := True;

    Command      : String (1 .. 256);
    Command_Last : Natural;

    procedure Server is
        -- Sockets / Connections
        Server_Host_Sock : GNAT.Sockets.Socket_Type;

        Server_Recv_Connection : GNAT.Sockets.Socket_Type;
        Server_Recv_Channel    : GNAT.Sockets.Stream_Access;

        Server_Send_Connection : GNAT.Sockets.Socket_Type;
        Server_Send_Channel    : GNAT.Sockets.Stream_Access;

        -- Keys
        Server_Public_Key  : Crypto.Key;
        Server_Private_Key : Crypto.Key;

        Client_Public_Key : Crypto.Key;

        -- Client info
        Client_Addr      : GNAT.Sockets.Sock_Addr_Type;
        Client_Host_Port : GNAT.Sockets.Port_Type;

        -- Tasks
        task Server_Send is
            entry Handshake_Done;
        end Server_Send;

        task body Server_Send is
            Plaintext_Message : Unbounded_String;
            Encrypted_Message : Unbounded_String;
        begin
            accept Handshake_Done;

            loop
                Put ("> ");
                Ada.Text_IO.Unbounded_IO.Get_Line (Plaintext_Message);
                exit when Plaintext_Message = "exit";

                -- Encrypt the message 
                Encrypted_Message := To_Unbounded_String (Crypto.Encrypt (
                    To_String (Plaintext_Message), Client_Public_Key)
                );

                String'Write
                   (Server_Send_Channel,
                    To_String (Encrypted_Message) & "$");
            end loop;

            String'Write (Server_Send_Channel, "/exit$");
            Connection.Close (Server_Send_Connection);
        end Server_Send;

        task Server_Recv is
            entry Handshake_Done;
        end Server_Recv;

        task body Server_Recv is
            Recv_Message      : String (1 .. 1024 * 8 * 16);
            Recv_Message_Last : Natural;

            Decrypted_Message : Unbounded_String;
        begin
            accept Handshake_Done;

            loop
                Connection.Read_Until_Sentinel
                   (Server_Recv_Channel, Recv_Message, Recv_Message_Last);
                   
                -- Decrypt the message 
                Decrypted_Message := To_Unbounded_String(Crypto.Decrypt (
                    Recv_Message(Recv_Message'First..Recv_Message_Last), 
                    Server_Private_Key
                ));

                exit when Decrypted_Message = "/exit";

                Put (Character'Val (13));
                Put_Line
                   ("[client] " &
                    To_String (Decrypted_Message));
                Put ("> ");
            end loop;

            Connection.Close (Server_Recv_Connection);
        end Server_Recv;

    begin
        -- Generate a keypair
        Crypto.Gen_Keypair (Server_Public_Key, Server_Private_Key);

        if VERBOSE then
            Put_Line ("Opening server on port 123");
        end if;

        GNAT.Sockets.Initialize;
        Connection.Server.Start_Server (Server_Host_Sock, 123);

        if VERBOSE then
            Put_Line ("Accepting connection");
        end if;

        -- Wait for the client to connect (this is a blocking call)
        Connection.Server.Accept_Client
           (Server => Server_Host_Sock, Connection => Server_Recv_Connection,
            Client_Addr => Client_Addr);

        Put_Line ("Client connected from " & GNAT.Sockets.Image (Client_Addr));

        Server_Recv_Channel := GNAT.Sockets.Stream (Server_Recv_Connection);

        -- FIXME
        Client_Host_Port := 124;

        if VERBOSE then
            Put_Line
               ("Client hosting on port " &
                GNAT.Sockets.Port_Type'Image (Client_Host_Port));
        end if;

        -- Connect to the client's host
        if VERBOSE then
            Put_Line
               ("Connecting to client's host: " &
                GNAT.Sockets.Image (Client_Addr.Addr));
        end if;

        Connection.Client.Connect
           (Server_Send_Connection, GNAT.Sockets.Image (Client_Addr.Addr),
            Client_Host_Port);
        Server_Send_Channel := GNAT.Sockets.Stream (Server_Send_Connection);

        if VERBOSE then
            Put_Line ("Connected to client.");
        end if;

        -- Send my public key to the client
        String'Write (Server_Send_Channel, "public " & Server_Public_Key);

        -- Read client's public key
        Connection.Read_Assert_Skip_Pattern (Server_Recv_Channel, "public ");
        Connection.Read_Fixed_Length (Server_Recv_Channel, Client_Public_Key);

        if VERBOSE then
            Put_Line ("Client public key is: " & Client_Public_Key);
        end if;

        if VERBOSE then
            Put_Line ("Waking up Server_Send, Server_Recv");
        end if;
        Server_Send.Handshake_Done;
        Server_Recv.Handshake_Done;

    end Server;

    procedure Client is

        -- Sockets / Connections
        Client_Send_Connection : GNAT.Sockets.Socket_Type;
        Client_Send_Channel    : GNAT.Sockets.Stream_Access;

        Client_Host_Sock : GNAT.Sockets.Socket_Type;

        Client_Recv_Connection : GNAT.Sockets.Socket_Type;
        Client_Recv_Channel    : GNAT.Sockets.Stream_Access;

        -- Keys
        Client_Public_Key  : Crypto.Key;
        Client_Private_Key : Crypto.Key;

        Server_Public_Key : Crypto.Key;

        -- Server info
        Server_Addr : GNAT.Sockets.Sock_Addr_Type;

        -- Tasks
        task Client_Send is
            entry Handshake_Done;
        end Client_Send;

        task body Client_Send is
            Plaintext_Message : Unbounded_String;
            Encrypted_Message : Unbounded_String;
        begin
            accept Handshake_Done;

            loop
                Put ("> ");
                Ada.Text_IO.Unbounded_IO.Get_Line (Plaintext_Message);
                exit when Plaintext_Message = "exit";

                Encrypted_Message := To_Unbounded_String (
                    Crypto.Encrypt (To_String (Plaintext_Message), Server_Public_Key)
                );

                String'Write
                   (Client_Send_Channel,
                    To_String (Encrypted_Message) & "$");
            end loop;

            String'Write (Client_Send_Channel, "/exit$");
            Connection.Close (Client_Send_Connection);
        end Client_Send;

        task Client_Recv is
            entry Handshake_Done;
        end Client_Recv;

        task body Client_Recv is
            Recv_Message      : String (1 .. 1024 * 8 * 16);
            Recv_Message_Last : Natural;

            Decrypted_Message : Unbounded_String;
        begin
            accept Handshake_Done;

            loop
                Connection.Read_Until_Sentinel
                   (Client_Recv_Channel, Recv_Message, Recv_Message_Last);

                -- Decrypt the message 
                Decrypted_Message := To_Unbounded_String (Crypto.Decrypt (
                    Recv_Message(Recv_Message'First..Recv_Message_Last), 
                    Client_Private_Key
                ));

                exit when Decrypted_Message = "/exit";

                Put (Character'Val (13));
                Put_Line
                   ("[client] " &
                    To_String (Decrypted_Message));
                Put ("> ");
            end loop;

            Connection.Close (Client_Recv_Connection);
        end Client_Recv;
    begin
        -- Generate keypair
        Crypto.Gen_Keypair (Client_Public_Key, Client_Private_Key);

        GNAT.Sockets.Initialize;

        -- Open a socket to wait for the server to connect
        Connection.Server.Start_Server (Client_Host_Sock, 124);

        -- Connect to the server
        declare
            Input : String(1 .. 256);
            Input_Length : Natural;

            IP_Address : String(1..256);
            IP_Address_Length : Natural;
        begin 
            Put ("(book / IP Address) > ");
            Get_Line(Input, Input_Length);

            if Input(Input'First..Input_Length) = "book" then 
                --IP_Address := AddressBook.Address_Book_CLI;
                --IP_Address_Length := IP_Address'Last;
                IP_Address := "127.0.0.1";
                IP_Address_Length := 9;
            else 
                IP_Address := Input;
                IP_Address_Length := Input_Length;
            end if;
            
        
            if VERBOSE then
                Put_Line ("Connecting to the server");
            end if;
            Connection.Client.Connect (Client_Send_Connection, IP_Address(IP_Address'First..IP_Address_Length), 123);
        end;

        Client_Send_Channel := GNAT.Sockets.Stream (Client_Send_Connection);

        if VERBOSE then
            Put_Line ("Client waiting for server connection");
        end if;

        -- Wait for the server to connect (this is a blocking call)
        Connection.Server.Accept_Client
           (Server => Client_Host_Sock, Connection => Client_Recv_Connection,
            Client_Addr => Server_Addr);

        Client_Recv_Channel := GNAT.Sockets.Stream (Client_Recv_Connection);

        if VERBOSE then
            Put_Line ("Connection established. Sending public key");
        end if;

        -- Send my public key to the server
        String'Write (Client_Send_Channel, "public " & Client_Public_Key);

        -- Receive the server's public key
        Connection.Read_Assert_Skip_Pattern (Client_Recv_Channel, "public ");
        Connection.Read_Fixed_Length (Client_Recv_Channel, Server_Public_Key);

        if VERBOSE then
            Put_Line ("Server public key is: " & Server_Public_Key);
        end if;

        -- Wake up threads, chat functionality begins
        if VERBOSE then
            Put_Line ("Waking up Client_Send, Client_Recv");
        end if;
        Client_Send.Handshake_Done;
        Client_Recv.Handshake_Done;

    end Client;

begin
    Put ("(client/server) > ");
    Get_Line (Command, Command_Last);

    if (Command (Command'First .. Command_Last) = "server") then
        Server;
    else
        -- TODO: Address book implementation here.
        Client;
    end if;
end Main;
