-------------------------------------------------------------------------------
--                                                                           --
--                             Simple_Config                                 --
--                                                                           --
--                             configuration                                 --
--                                                                           --
--                                  SPEC                                     --
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

with Ada.Strings.Unbounded;
with Config_File_Parser;

package Configuration is

   function TUS (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;
   --  Convenience rename. TUS is much shorter than To_Unbounded_String.

   type Keys is (Foo_Boolean,
                 Foo_String,
                 Foo_Unbounded_String,
                 Foo_Integer,
                 Foo_Float);
   --  These are the valid configuration keys.

   type Defaults_Array is array (Keys) of
     Ada.Strings.Unbounded.Unbounded_String;
   --  The array type we'll use to hold our default configuration settings.

   -------------------------------------------------
   --  Set and load the first configuration file  --
   -------------------------------------------------
   Defaults_One : Defaults_Array :=
                    (Foo_Boolean          => TUS ("False"),
                     Foo_String           => TUS ("Thomas Løcke"),
                     Foo_Unbounded_String => TUS ("Ada rocks!"),
                     Foo_Integer          => TUS ("42"),
                     Foo_Float            => TUS ("42.0"));
   --  Default values for the Config_One object. These can be overwritten by
   --  the contents of the config_one.ini file.

   package Config_One is new Config_File_Parser
     (Keys => Keys,
      Defaults_Array => Defaults_Array,
      Defaults => Defaults_One,
      Config_File    => "config_one.ini");
   --  Instantiate the Config_One configuration object.

   --------------------------------------------------
   --  Set and load the second configuration file  --
   --------------------------------------------------
   Defaults_Two : Defaults_Array :=
                    (others => Ada.Strings.Unbounded.Null_Unbounded_String);
   --  For the Config_Two object we have no default values, so all values have
   --  to be read from the config_two.ini file.

   package Config_Two is new Config_File_Parser
     (Keys => Keys,
      Defaults_Array => Defaults_Array,
      Defaults => Defaults_Two,
      Config_File    => "config_two.ini");
   --  Instantiate the Config_Two configuration object.

end Configuration;
