with GNAT.Sockets; use GNAT.Sockets;
with Ada.IO_Exceptions;

package body Connection is 

procedure Read_Assert_Skip_Pattern (Channel : Stream_Access;
                                    Pattern : in String) is 
begin 
    for I in 1..Pattern'Length loop
        declare
            C : Character := Character'Input (Channel);
        begin
            if (C /= Pattern (I)) then
                raise Pattern_Assertion_Failed with 
                    "Expected " & Pattern (I) 
                    & ", Found " & C 
                    & ", Pattern: " & Pattern;
            end if;
        end;
    end loop;
end Read_Assert_Skip_Pattern;

procedure Read_Fixed_Length (Channel     : Stream_Access;
                             Destination : out String) is 
begin 
    for I in Destination'First..Destination'Length loop
        declare
            C : Character := Character'Input (Channel);
        begin
            Destination (I) := C;
        end;
    end loop;
end Read_Fixed_Length;

procedure Read_Until_Sentinel (Channel     : Stream_Access;
                               Destination : out String;
                               Msg_Last    : out Natural) is
    Chars : Natural := 0;
begin 
    for I in 1..Destination'Length loop 
        declare 
            C : Character := Character'Input (Channel);
        begin 
            exit when C = '$';

            Destination (I) := C;

            Chars := Chars + 1;
            -- if (C = Character'Val(0)) then
            -- if (C = "$") then
            --     return;
            -- else
            -- end if; 
        end;
    end loop;

    Msg_Last := Chars;
end Read_Until_Sentinel;

procedure Read_Flush (Channel : Stream_Access) is
    Trash : Character;
begin 
    begin 
        loop
            Trash := Character'Input (Channel);
        end loop;
    exception
        when Ada.IO_Exceptions.End_Error =>
            null;
    end;
end Read_Flush;

procedure Close (Sock : in out Socket_Type) is
begin 
    Close_Socket (Sock);
end Close;

end Connection;