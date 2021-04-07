with GNAT.Sockets; use GNAT.Sockets;

package Connection.Server is

-- Opens a new socket bound to 127.0.0.0 with specified port.
procedure Start_Server (Sock : in out Socket_Type;
                        Port : in Port_Type);

-- Creates a new connected socket to the first client in the queue,
-- returned via "Connection". Also returns client IP address.
procedure Accept_Client (Server      : Socket_Type;
                         Connection  : out Socket_Type;
                         Client_Addr : out Sock_Addr_Type);

end Connection.Server;
