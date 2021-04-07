with GNAT.Sockets; use GNAT.Sockets;

package body Connection.Server is

procedure Start_Server (Sock : in out Socket_Type;
                        Port : in Port_Type) is
begin
    Create_Socket (Sock);
    Set_Socket_Option (
        Socket => Sock,
        Level  => Socket_Level,
        Option => (Reuse_Address, True)
    );

    Bind_Socket (
        Socket  => Sock,
        Address => (
            Family => Family_Inet,
            Addr   => Inet_Addr ("127.0.0.1"),
            Port   => Port
        )
    );

    Listen_Socket (Sock);   -- Mark as available for connections 
end Start_Server;

procedure Accept_Client (Server      : Socket_Type;
                         Connection  : out Socket_Type;
                         Client_Addr : out Sock_Addr_Type) is 

begin
    Accept_Socket (
        Server  => Server,
        Socket  => Connection,
        Address => Client_Addr
    );
end Accept_Client;

end Connection.Server;
