with Ada.Streams; use Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;

package Connection.Client is

-- Connect to an open server 
procedure Connect (Sock        : in out Socket_Type;
                   Server_IP   : in String;
                   Server_Port : in Port_Type);

-- procedure Send_Message (Sock : in out Socket_Type;
--                         Rec_IP : in String;
--                         Rec_Port : in Port_Type;
--                         Data : in Ada.Streams.Stream_Element_Array);

end Connection.Client;
