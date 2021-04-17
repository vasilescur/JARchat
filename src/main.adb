with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets;
with Ada.Streams; use Ada.Streams;

with Connection;
with Connection.Server;
with Connection.Client;

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

   Command : String (1..256); Command_Last : Natural;

   procedure Server is
      -- Sockets / Connections 
      Server_Host_Sock        : GNAT.Sockets.Socket_Type;

      Server_Recv_Connection  : GNAT.Sockets.Socket_Type;
      Server_Recv_Channel     : GNAT.Sockets.Stream_Access;

      Server_Send_Connection  : GNAT.Sockets.Socket_Type;
      Server_Send_Channel     : GNAT.Sockets.Stream_Access;

      -- Keys
      Server_Public_Key  : String(1..16) := "publicKeyServer0";
      Server_Private_Key : String(1..16) := "privateKeyServer";

      Client_Public_Key  : String(1..16);

      -- Client info      
      Client_Addr        : GNAT.Sockets.Sock_Addr_Type;
      Client_Host_Port   : GNAT.Sockets.Port_Type;
   begin
      -- TODO: Prompt user for the port
      if VERBOSE then put_Line ("Opening server on port 123"); end if;

      GNAT.Sockets.Initialize;
      Connection.Server.Start_Server (Server_Host_Sock, 123);

      if VERBOSE then Put_Line ("Accepting connection"); end if;

      -- Wait for the client to connect (this is a blocking call)
      Connection.Server.Accept_Client (
         Server      => Server_Host_Sock,
         Connection  => Server_Recv_Connection,
         Client_Addr => Client_Addr
      );

      Put_Line ("Client connected from " & GNAT.Sockets.Image (Client_Addr));

      Server_Recv_Channel := GNAT.Sockets.Stream (Server_Recv_Connection);

      -- Read client's connection info 
      -- Connection.Read_Assert_Skip_Pattern (Server_Recv_Channel, "client port ");
      -- if VERBOSE then Put_Line ("About to parse client port"); end if;
      -- GNAT.Sockets.Port_Type'Read (Server_Recv_Channel, Client_Host_Port); -- blocking here?
      
      -- FIXME 
      Client_Host_Port := 124;

      if VERBOSE then Put_Line ("Client hosting on port " & GNAT.Sockets.Port_Type'Image (Client_Host_Port)); end if;

      -- Connect to the client's host 
      if VERBOSE then Put_Line ("Connecting to client's host: " & GNAT.Sockets.Image (Client_Addr.Addr)); end if;

      Connection.Client.Connect (Server_Send_Connection, GNAT.Sockets.Image (Client_Addr.Addr), Client_Host_Port);
      Server_Send_Channel := GNAT.Sockets.Stream (Server_Send_Connection);

      if VERBOSE then Put_Line ("Connected to client."); end if;

      -- Send my public key to the client
      String'Write (Server_Send_Channel, "public " & Server_Public_Key);

      -- Read client's public key
      Connection.Read_Assert_Skip_Pattern (Server_Recv_Channel, "public ");
      Connection.Read_Fixed_Length (Server_Recv_Channel, Client_Public_Key);

      if VERBOSE then Put_Line ("Client public key is: " & Client_Public_Key); end if;


      -- -- TODO: Chat functionality goes here
      -- loop
      --    Connection.Read_Until_Sentinel (Server_Channel, Rec_Message, Rec_Message_Last);
      --    exit when Rec_Message(Rec_Message'First..Rec_Message_Last) = "/exit";
      --    Put_Line ("[client] " & Rec_Message(Rec_Message'First..Rec_Message_Last));
      -- end loop;

      if VERBOSE then Put_Line ("Closing connections."); end if;
      Connection.Close (Server_Send_Connection);
      Connection.Close (Server_Recv_Connection);
   end Server;

   procedure Client is
      -- Sockets / Connections
      Client_Send_Connection  : GNAT.Sockets.Socket_Type;
      Client_Send_Channel     : GNAT.Sockets.Stream_Access;

      Client_Host_Sock        : GNAT.Sockets.Socket_Type;

      Client_Recv_Connection  : GNAT.Sockets.Socket_Type;
      Client_Recv_Channel     : GNAT.Sockets.Stream_Access;

      -- Keys 
      Client_Public_Key  : String(1..16) := "publicKeyClient0";
      Client_Private_Key : String(1..16) := "privateKeyClient";

      Server_Public_Key  : String(1..16);

      -- Server info 
      Server_Addr        : GNAT.Sockets.Sock_Addr_Type;
   begin
      GNAT.Sockets.Initialize;

      -- Connect to the server
      -- TODO: Prompt user for server address and port 
      if VERBOSE then Put_Line ("Connecting to the server"); end if;
      Connection.Client.Connect (Client_Send_Connection, "127.0.0.1", 123);
      Client_Send_Channel := GNAT.Sockets.Stream (Client_Send_Connection);

      -- Open a socket and wait for the server to connect
      Connection.Server.Start_Server (Client_Host_Sock, 124);

      -- Send our connection info to the server
      -- FIXME 
      -- String'Write (Client_Send_Channel, "client port 124");
      -- if VERBOSE then Put_Line ("Sent to server: client port 124"); end if;

      if VERBOSE then Put_Line ("Client waiting for server connection"); end if;

      -- Wait for the server to connect (this is a blocking call)
      Connection.Server.Accept_Client (
         Server      => Client_Host_Sock,
         Connection  => Client_Recv_Connection,
         Client_Addr => Server_Addr
      );

      Client_Recv_Channel := GNAT.Sockets.Stream (Client_Recv_Connection);

      if VERBOSE then Put_Line ("Connection established. Sending public key"); end if;

      -- Send my public key to the server
      String'Write (Client_Send_Channel, "public " & Client_Public_Key);

      -- Receive the server's public key
      Connection.Read_Assert_Skip_Pattern (Client_Recv_Channel, "public ");
      Connection.Read_Fixed_Length (Client_Recv_Channel, Server_Public_Key);

      if VERBOSE then Put_Line ("Server public key is: " & Server_Public_Key); end if;

      -- -- Chat functionality goes here

      -- -- SENDING --
      -- loop
      --    Put ("message > ");
      --    Get_Line (Plaintext_Message, Message_Last);
      --    exit when Plaintext_Message(Plaintext_Message'First..Message_Last) = "exit";

      --    if VERBOSE then Put_Line ("Sending some unencrypted message: " & Plaintext_Message(Plaintext_Message'First..Message_Last)); end if;

      --    String'Write (Client_Channel, Plaintext_Message(Plaintext_Message'First..Message_Last) & "$");
      -- end loop;

      -- String'Write (Client_Channel, "/exit$");

      -- for I in Unenc_Send_Buffer'Range loop
      --    Unenc_Send_Buffer (I) := Stream_Element (Character'Pos (Plaintext_Message (Integer (I))));
      -- end loop

      -- TODO: Change address and port to those of recipient
      -- Connection.Client.Send_Message(Client_Sock, "127.0.0.1", 123, Unenc_Send_Buffer);
   
      -- End chat functionality

      if VERBOSE then Put_Line ("Closing connections."); end if;
      Connection.Close (Client_Send_Connection);
      Connection.Close (Client_Recv_Connection);

   end Client;
begin
   Put ("(client/server) > ");
   Get_Line (Command, Command_Last);

   if (Command(Command'First..Command_Last) = "server") then
      Server;
   else
      Client;
   end if;
end Main;
