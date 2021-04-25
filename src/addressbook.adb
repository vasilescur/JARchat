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
    type Address is record
        Name : String (1..256);
        IP   : String (1..256);
    end record;

    -- Map : String -> String 
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

    Address_Map : Map;  -- Global map used for address book 

    -- Equivalent to String.Split, will split on spaces and return a vector of strings 
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

    -- Add a record to the in-memory address book 
    procedure Add (Name : String;
                   IP    : String) is
    begin
        -- Put_Line ("Adding " & Name & " to address book with IP " & IP);
        Address_Map.Insert (Name, IP);
        Put_Line ("Added " & Name & " to address book with IP " & IP);
    end Add;

    -- Remove a record from the in-memory address book 
    procedure Remove (Name : String) is
    begin
        if Address_Map.Contains(Name) then
            Address_Map.Delete (Name);
            Put_Line ("Removed " & Name & " from address book");
        else
            Put_Line (Name & " could not be found");
        end if;
    end Remove;

    -- Edit a record in the in-memory address book 
    procedure Edit (Name     : String;
                    New_IP   : String) is
    begin
        if Address_Map.Contains(Name) then
            Address_Map.Replace (Name, New_IP);
            Put_Line ("Changed " & Name & " to have IP " & New_IP);
        else
            Put_Line (Name & " could not be found");
        end if;
    end Edit;

    -- Print the contents of the in-memory address book 
    procedure Print is
    begin
        for C in Address_Map.Iterate loop
            Put_Line ("  " & Key (C) & " [" & Address_Map (C) & "]");
        end loop;
    end Print;

    -- Initialize the in-memory address book by reading the file
    procedure Init_Addr_Map is
        F         : File_Type;
        File_Name : constant String := "address_book.txt";
    begin
        if Exists (File_Name) then
            Open (F, In_File, File_Name);

            while not End_Of_File (F) loop
                declare
                    Input : String := Get_Line (F);
                    Input_Parts : String_Vectors.Vector;

                    Cursor : String_Vectors.Cursor;
                begin
                    -- Put_Line ("  " & Input);
                    Ada.Text_IO.Flush;

                    -- Put ("  Tokenizing...");
                    Input_Parts := Tokenize (Input);
                    -- Put_Line (" Done. Saving... ");

                    Cursor := Input_Parts.First;
                    Add (String_vectors.Element(Cursor), String_vectors.Element(Next(Cursor)));
                end;
            end loop;
            Close (F);
        end if;
    end Init_Addr_Map;

    -- Writes the contents of the in-memory address book to the file 
    procedure Save_Addr_Map is
        F         : File_Type;
        File_Name : constant String := "address_book.txt";
    begin
        Put ("Saving ");
        if Exists (File_Name) then
            Put_Line ("to existing file");
            Open (F, Out_File, File_Name);

            for C in Address_map.Iterate loop
                Put_Line (F, Key (C) & " " & Address_Map (C));
            end loop;

            Close (F);
            -- Put_Line ("Closed " & File_Name);
        else 
            Put_Line ("to new file");
            Create (F, Out_File, File_Name);

            for C in Address_map.Iterate loop
                Put_Line (F, Key (C) & " " & Address_Map (C));
            end loop;

            Close (F);
        end if;

        Put_Line("Save Complete!");
    end Save_Addr_Map;

    Input : String(1..256);
    Input_Length : Natural;

    Cursor : String_Vectors.Cursor;
    Command : String_Vectors.Vector;

    procedure Print_Contacts is 
    begin 
        Put_Line ("");
        Put_Line ("Contacts: ");
        Print;
        Put_Line ("");
    end Print_Contacts;
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

    Print_Contacts;

    loop
        Put ("(add/edit/remove/contacts / Name) > ");
        Get_Line (Input, Input_Length);
        exit when Input(Input'First..Input_Length) = "exit";

        Command := Tokenize (Input(Input'First..Input_Length));

        if Command.First_Element = "add" then
            Cursor := Command.First;
            Add (String_vectors.Element(Next(Cursor)), String_vectors.Element(Next(Next(Cursor))));

        elsif Command.First_Element = "edit" then
            Cursor := Command.First;
            Edit (String_vectors.Element(Next(Cursor)), String_vectors.Element(Next(Next(Cursor))));

        elsif Command.First_Element = "remove" then 
            Cursor := Command.First;
            Remove (String_vectors.Element(Next(Cursor)));

        elsif Command.First_Element = "contacts" then
            Print_Contacts;

        else 
            -- Put_Line ("Searching for " & Command.First_Element);

            if Address_Map.Contains (Command.First_Element) then
                Save_Addr_Map;

                Put_Line ("Connecting to " & Address_Map (Command.First_Element));

                return Address_Map (Command.First_Element);
            else 
                Put_Line("Invalid Command / Name: " & Command.First_Element);
            end if;
        end if;
    end loop;

    Save_Addr_Map;

    Recent_Length := 9;

    Put_Line ("Connecting to default IP Address...");

    return "127.0.0.1";

end Address_Book_CLI;

end AddressBook;
