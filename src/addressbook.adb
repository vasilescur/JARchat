with Ada.Sequential_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO, Ada.Containers.Indefinite_Vectors, Ada.Strings.Fixed, Ada.Strings.Maps;
use Ada.Text_IO, Ada.Containers, Ada.Strings, Ada.Strings.Fixed, Ada.Strings.Maps;

package body AddressBook is

Recent_Length : Natural;

function Get_Recent_IP_Length return Natural
is begin
    return Recent_Length;
end Get_Recent_IP_Length;

function Address_Book_CLI return String
is
    package IP_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");
    use IP_Hashed_Maps;

    package String_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_type   => Natural, 
        Element_type => String);
    use String_Vectors;

    type Address is record
        Name : String (1..256);
        IP   : String (1..256);
    end record;

    package Address_IO is new Ada.Sequential_IO (Address);
    use Address_IO;

    Address_Map : Map;

    procedure Init_Addr_Map is
        F         : Address_IO.File_Type;
        File_Name : constant String := "address.bin";
        Temp_Address : Address;
    begin
        if Exists (File_Name) then
            Open (F, In_File, File_Name);
            while not End_Of_File (F) loop
               Read (F, Temp_Address);
               Put_Line(Temp_Address);
               Address_Map.Include (Temp_Address.Name, Temp_Address.IP);
            end loop;
            Close (F);
        end if;
    end Init_Addr_Map;

    procedure Save_Addr_Map is
        F         : Address_IO.File_Type;
        File_Name : constant String := "address.bin";
    begin
        Put_Line("Saving!");
        if Exists (File_Name) then
            Open (F, Out_File, File_Name);
            Put_Line ("Opened " & File_Name);
            for C in Address_map.Iterate loop
                Put_Line ("Saving: " & Key (C) & " " & Address_Map (C));
                Write (F, (Key (C), Address_Map (C)));
                Put_Line ("Saved: " & Key (C) & " " & Address_Map (C));
            end loop;
            Close (F);
            Put_Line ("Closed " & File_Name);
        else 
            Create (F, Out_File, File_Name);
            Put_Line ("Created " & File_Name);
            for C in Address_map.Iterate loop
                Put_Line ("Saving: " & Key (C) & " " & Address_Map (C));
                Write (F, (Key (C), Address_Map (C)));
                Put_Line ("Saved: " & Key (C) & " " & Address_Map (C));
            end loop;
            Close (F);
            Put_Line ("Closed " & File_Name);
        end if;
        Put_Line("Save Complete!");
    end Save_Addr_Map;

    procedure Add (Name : String;
                  IP    : String) is
    begin
        Address_Map.Include(Name, IP);
        Put_Line ("Added " & Name & " to address book with IP " & IP);
    end Add;

    procedure Remove (Name : String) is
    begin
        if Address_Map.Contains(Name) then
            Address_Map.Exclude(Name);
            Put_Line ("Removed " & Name & " from address book");
        else
            Put_Line (Name & " could not be found");
        end if;
    end Remove;

    procedure Edit (Name     : String;
                    New_IP   : String) is
    begin
        if Address_Map.Contains(Name) then
            Address_Map.Replace(Name, New_IP);
            Put_Line ("Changed " & Name & " to have IP " & New_IP);
        else
            Put_Line (Name & " could not be found");
        end if;
    end Edit;

    procedure Print is
    begin
        for C in Address_map.Iterate loop
            Put_Line(Key (C) & " " & Address_Map (C));
        end loop;
    end Print;

    function Tokenize (Input : String) return String_Vectors.Vector is
        Start  : Positive;
        Finish : Natural  := 0;
        Output : Vector   := Empty_Vector;
    begin
        Start  := Input'First;
        while Start <= Input'Last loop
            Find_Token (Input, To_Set (' '), Start, Outside, Start, Finish);
            exit when Start > Finish;
            Output.Append (Input (Start .. Finish));
            Start := Finish + 1;
        end loop;
        return Output;
    end Tokenize;

    Input : String(1..256);
    Input_Length : Natural;

    Cursor : String_Vectors.Cursor;
    Command : String_Vectors.Vector;

    IP_Addr : String(1..256);

begin
    -- add [name] [ip]
    -- remove [name]
    -- edit [name] [new-ip]
    -- [name] --> returns the associated IP

    -- loop 
        -- get Input
        -- switch on command
        -- use hardcoded file name 

        -- if input does not match commands -- it is a name --> return its IP from this function
    -- end loop
    Init_Addr_Map;

    loop
        Put ("(Command / Name) > ");
        Get_Line (Input, Input_Length);
        exit when Input(Input'First..Input_Length) = "exit";

        Command := Tokenize (Input(Input'First..Input_Length));

        if Command.First_Element = "add" then
            Cursor := Command.First;
            Add (String_vectors.Element(Next(Cursor)), String_vectors.Element(Next(Next(Cursor))));
        else if Command.First_Element = "edit" then
            Cursor := Command.First;
            Edit (String_vectors.Element(Next(Cursor)), String_vectors.Element(Next(Next(Cursor))));
        else if Command.First_Element = "remove" then
            Cursor := Command.First;
            Remove (String_vectors.Element(Next(Cursor)));
        else if Address_Map.Contains(Command.First_Element) then
            Save_Addr_Map;
            IP_Addr := Address_Map (Command.First_Element);
            Recent_Length := IP_Addr'Last;
            Put_Line (IP_Addr);
            return Address_Map (Command.First_Element);
        else 
            Put_Line("Invalid Command / Name: " & Command.First_Element);
        end if;
        end if;
        end if;
        end if;
    end loop;

    Save_Addr_Map;

    Recent_Length := 9;

    Put_Line ("Connecting to default IP Address...");

    return "127.0.0.1";

end Address_Book_CLI;

end AddressBook;
