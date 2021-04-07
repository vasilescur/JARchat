with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets;
with Ada.Streams; use Ada.Streams;

with Connection;
with Connection.Server;
with Connection.Client;

procedure Main is
   VERBOSE : Boolean := True;

   Command : String (1..256); Command_Last : Natural;

   procedure Server is
      Server_Sock        : GNAT.Sockets.Socket_Type;
      Server_Connection  : GNAT.Sockets.Socket_Type;
      Server_Channel     : GNAT.Sockets.Stream_Access;
      Client_Addr        : GNAT.Sockets.Sock_Addr_Type;

      Server_Public_Key  : String(1..16) := "publicKeyServer0";
      Server_Private_Key : String(1..16) := "privateKeyServer";

      Client_Public_Key  : String(1..16);
   begin
      -- TODO: Prompt user for the port
      if VERBOSE then put_Line ("Opening server on port 123"); end if;

      GNAT.Sockets.Initialize;
      Connection.Server.Start_Server (Server_Sock, 123);

      if VERBOSE then Put_Line ("Accepting connection"); end if;

      -- Wait for the client to connect (this is a blocking call)
      Connection.Server.Accept_Client (
         Server      => Server_Sock,
         Connection  => Server_Connection,
         Client_Addr => Client_Addr
      );

      Put_Line ("Client connected from " & GNAT.Sockets.Image (Client_Addr));

      Server_Channel := GNAT.Sockets.Stream (Server_Connection);

      -- Read client's public key
      Connection.Read_Assert_Skip_Pattern (Server_Channel, "public ");
      Connection.Read_Fixed_Length (Server_Channel, Client_Public_Key);

      if VERBOSE then Put_Line ("Client public key is: " & Client_Public_Key); end if;

      -- Send my public key to the client
      String'Write (Server_Channel, "public " & Server_Public_Key);

      -- TODO: Chat functionality goes here

      if VERBOSE then Put_Line ("Closing connection."); end if;
      Connection.Close (Server_Connection);
   end Server;

   procedure Client is
      Client_Sock        : GNAT.Sockets.Socket_Type;
      Client_Channel     : GNAT.Sockets.Stream_Access;

      Client_Public_Key  : String(1..16) := "publicKeyClient0";
      Client_Private_Key : String(1..16) := "privateKeyClient";

      Server_Public_Key  : String(1..16);
   begin
      GNAT.Sockets.Initialize;

      -- Connect to the server
      -- TODO: Prompt user for server address and port 
      if VERBOSE then Put_Line ("Connecting to the server"); end if;
      Connection.Client.Connect (Client_Sock, "127.0.0.1", 123);
      Client_Channel := GNAT.Sockets.Stream (Client_Sock);

      if VERBOSE then Put_Line ("Connection established. Sending public key"); end if;

      -- Send my public key to the server
      String'Write (Client_Channel, "public " & Client_Public_Key);

      -- Receive the server's public key
      Connection.Read_Assert_Skip_Pattern (Client_Channel, "public ");
      Connection.Read_Fixed_Length (Client_Channel, Server_Public_Key);

      if VERBOSE then Put_Line ("Server public key is: " & Server_Public_Key); end if;

      -- TODO: Chat functionality goes here

      if VERBOSE then Put_Line ("Closing connection."); end if;
      Connection.Close (Client_Sock);

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
