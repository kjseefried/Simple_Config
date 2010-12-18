-------------------------------------------------------------------------------
--                                                                           --
--                              Simple_Config                                --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
--                                                                           --
--  Simple_Config is free software;  you can  redistribute it  and/or modify --
--  it under terms of the  GNU General Public License as published  by the   --
--  Free Software  Foundation;  either version 2,  or (at your option) any   --
--  later version.  Simple_Config is distributed in the hope that it will be --
--  useful, but WITHOUT ANY WARRANTY;  without even the  implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Gene-  --
--  ral Public License for  more details.  You should have  received  a copy --
--  of the GNU General Public License  distributed with Simple_Config.  If   --
--  not, write to  the  Free Software Foundation,  51  Franklin  Street,     --
--  Fifth Floor, Boston, MA 02110 - 1301, USA.                               --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Configuration;

procedure Simple_Config
is

   use Ada.Text_IO;
   use Configuration;

   package FIO renames Ada.Float_Text_IO;
   package IIO renames Ada.Integer_Text_IO;
   package UIO renames Ada.Text_IO.Unbounded_IO;
   --  Some convenience renames.

begin

   --  Lets see what config_one settings we have.
   New_Line;
   Put_Line ("Config_One:");
   Put_Line (Config_One.Get (Key => Foo_String));
   UIO.Put_Line (Config_One.Get (Key => Foo_Unbounded_String));
   FIO.Put (Config_One.Get (Key => Foo_Float));
   New_Line;
   IIO.Put (Config_One.Get (Key => Foo_Integer));
   New_Line;

   if Config_One.Get (Key => Foo_Boolean) then
      Put_Line ("We're true!");
   end if;

   --  And now for the config_two settings
   New_Line;
   Put_Line ("Config_Two:");
   Put_Line (Config_Two.Get (Key => Foo_String));
   UIO.Put_Line (Config_Two.Get (Key => Foo_Unbounded_String));
   IIO.Put (Config_Two.Get (Key => Foo_Integer));
   New_Line;

   if Config_Two.Get (Key => Foo_Boolean) then
      Put_Line ("We're true!");
   end if;

   if not Config_Two.Has_Value (Key => Foo_Float) then
      Put_Line ("Foo_Float is empty.");
   end if;
   --  Test if a value is empty. This can be done to avoid triggering the
   --  Empty_Key exception.

   FIO.Put (Config_Two.Get (Key => Foo_Float));
   --  Foo_Float is empty, so the Empty_Key exception is raised because we're
   --  trying to convert an empty key to a Float.

exception
   when Config_Two.Empty_Key =>
      Put_Line ("Nothing to see here...." & Config_Two.Get (Key => Foo_Float));
      --  It is allowed to convert an empty key to a String/Unbounded_String.
      New_Line;

end Simple_Config;
