with Ada.Streams; use Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;

package body Connection.Client is

procedure Connect (Sock        : in out Socket_Type;
                   Server_IP   : in String;
                   Server_Port : in Port_Type) is
begin
    Create_Socket (Socket => Sock);

    Connect_Socket (
        Socket => Sock,
        Server => (
            Family => Family_Inet,
            Addr   => Inet_Addr (Server_IP),
            Port   => Server_Port
        )
    );
end Connect;

-- procedure Send_Message (Sock : in out Socket_Type;
--                         Rec_IP : in String;
--                         Rec_Port : in Port_Type;
--                         Data : in Ada.Streams.Stream_Element_Array) is
--     Last : Ada.Streams.Stream_Element_Offset;
--     Address : GNAT.Sockets.Sock_Addr_Type;
-- begin
--     Address.Port := Rec_Port;
--     Address.Addr := GNAT.Sockets.Inet_Addr(Rec_IP);
    
--     GNAT.Sockets.Send_Socket (
--         Socket => Sock,
--         Item => Data,
--         Last => Last,
--         To => Address
--     );

-- end Send_Message;

end Connection.Client;
