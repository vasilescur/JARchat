with Crypto;
with GNAT.Sockets;
with Ada.Streams; use Ada.Streams;

with Connection;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO.Unbounded_IO;

package body FileTransfer is

procedure Send_File (Filename   : in String;
                     Channel    : GNAT.Sockets.Stream_Access;
                     Key        : in Crypto.Key) 
is
    F : File_Type;
begin
    if Exists (Filename) then 
        String'Write (Channel, "/file$");

        Open (F, In_File, Filename);

        -- Put_Line ("Sending file...");

        String'Write (Channel, Filename & "$");

        while not End_Of_File (F) loop 
            declare
                Current_Line : String := Get_Line (F);
                Encrypted_Message : Unbounded_String;
            begin 
                Encrypted_Message := To_Unbounded_String (
                    Crypto.Encrypt (Current_Line, Key)
                );

                -- Put_Line ("  " & Current_Line & " --> encryption --> network");

                String'Write (
                    Channel,
                    To_String (Encrypted_Message) & "$"
                );
            end;
        end loop;

        String'Write (Channel, "/EOF$");
    else 
        Put_Line ("The specified file does not exist.");
        return;
    end if;
end Send_File;

procedure Recv_File (Outpath : in String;
                     Channel : GNAT.Sockets.Stream_Access;
                     Key     : in Crypto.Key) 
is
    Recv_Message : String (1 .. 1024 * 8 * 16);
    Recv_Message_Last : Natural; 

    Recv_Path : String(1..256);
    Recv_Path_Last : Natural;
    
    Full_Path : Unbounded_String := To_Unbounded_String (Outpath);

    F : File_Type;
begin 
    -- Put_Line("Receiving a file...");
    Connection.Read_Until_Sentinel (
        Channel => Channel,
        Destination => Recv_Path,
        Msg_Last => Recv_Path_Last
    );
    
    Append (Full_Path, To_Unbounded_String (Recv_Path));

    Put_Line ("Full output path: " & To_String (Full_Path));

    Put_Line ("Creating new file " & To_String (Full_Path));
    Create (F, Out_File, To_String (Full_Path));

    loop
        Connection.Read_Until_Sentinel (
            Channel => Channel,
            Destination => Recv_Message,
            Msg_Last => Recv_Message_Last
        );

        exit when Recv_Message(Recv_Message'First..Recv_Message_Last) = "/EOF";

        declare
            Decrypted_Message : Unbounded_String;
        begin
            -- Decrypt the message 
            Decrypted_Message := To_Unbounded_String (Crypto.Decrypt (
                Recv_Message(Recv_Message'First..Recv_Message_Last), 
                Key
            ));

            -- Put_Line ("  network --> decrypt --> " & To_String (Decrypted_Message));

            Put_Line (F, To_String (Decrypted_Message));
        end;
    end loop;

    Put_Line ("Saved decrypted file at " & To_String (Full_Path));
    Close (F);
end Recv_File;

end FileTransfer;