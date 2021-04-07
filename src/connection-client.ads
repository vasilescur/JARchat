with GNAT.Sockets; use GNAT.Sockets;

package Connection.Client is

-- Connect to an open server 
procedure Connect (Sock        : in out Socket_Type;
                   Server_IP   : in String;
                   Server_Port : in Port_Type);

end Connection.Client;
