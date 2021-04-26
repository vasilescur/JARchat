with Crypto;
with GNAT.Sockets;
with Ada.Streams; use Ada.Streams;

with Connection;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO.Unbounded_IO;

package FileTransfer is

procedure Send_File (Filename   : in String;
                     Channel    : GNAT.Sockets.Stream_Access;
                     Key        : in Crypto.Key);

procedure Recv_File (Outpath : in String;
                     Channel : GNAT.Sockets.Stream_Access;
                     Key     : in Crypto.Key);

end FileTransfer;