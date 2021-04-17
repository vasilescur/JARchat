with GNAT.Sockets; use GNAT.Sockets;

package Connection is

CRLF : String := (1 => ASCII.CR, 2 => ASCII.LF);

-- Read length(Pattern) characters, asserting that the read data 
-- is equal to Pattern.
Pattern_Assertion_Failed : exception;
procedure Read_Assert_Skip_Pattern (Channel : Stream_Access;
                                    Pattern : in String);

-- Read a fixed length string from the stream.
procedure Read_Fixed_Length (Channel     : Stream_Access;
                             Destination : out String);

procedure Read_Until_Sentinel (Channel     : Stream_Access;
                               Destination : out String;
                               Msg_Last    : out Natural);

-- Read (and ignore) all available data from the stream.
procedure Read_Flush (Channel : Stream_Access);

procedure Close (Sock : in out Socket_Type);

end Connection;
