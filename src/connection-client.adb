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

end Connection.Client;
