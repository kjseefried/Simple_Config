-------------------------------------------------------------------------------
--                                                                           --
--                             Simple_Config                                 --
--                                                                           --
--                           config_file_parser                              --
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

--  This package provides simple access to configuration files.
--  The format is:
--       KEY VALUE
--
--  Comments are prefixed with a # or a --:
--
--  # This is a comment
--  -- This is also a comment
--
--  Blank lines and comments are ignored and so is pre-/postfixed whitespace,
--  so this:
--
--  [lots of whitespace]KEY[lots of whitespace]VALUE[lots of whitespace]
--
--  is treated as:
--
--  KEY VALUE
--
--  Values containing whitespace, eg. full sentences and similar, are returned
--  as is. It is not necessary to quote such values, so this:
--
--    KEY some value with whitespace
--
--  is perfectly valid, and will, when calling Get (KEY), return:
--
--    some value with whitespace
--
--  If VALUE is True or False (case-insensitive), then the KEY can be returned
--  as both a String or as a Boolean.
--  Conversions from VALUE to other types, such as Integer or Float, will raise
--  an exception on failure. It will NOT return some dummy value.
--
--  To clear a default value, simply add the key to the configuration file,
--  with no value set. Conversely, you must omit (or comment) keys for whích
--  you want to use the default value.

with Ada.Strings.Unbounded;

generic

   type Keys is (<>);
   --  The Keys type. Must be a discrete type.

   type Defaults_Array is array (Keys) of
     Ada.Strings.Unbounded.Unbounded_String;
   --  The array type used to hold the key/value pairs.

   Defaults : in out Defaults_Array;
   --  The actual key/value array.

   Config_File : in String;
   --  Path to the configuration file.

package Config_File_Parser is

   Unknown_Key             : exception;
   --  Is raised when an unknown KEY has been found in the config file.
   Cannot_Open_Config_File : exception;
   --  Is raised when the given config file cannot be opened, eg. due to bad
   --  path.
   Conversion_Error        : exception;
   --  Is raised when a value cannot be converted to a specific type.
   Empty_Key               : exception;
   --  Is raised when a key with the element Null_Unbounded_String is called.

   function Get (Key : in Keys) return Boolean;
   function Get (Key : in Keys) return Float;
   function Get (Key : in Keys) return Integer;
   function Get (Key : in Keys) return String;
   function Get (Key : in Keys) return Ada.Strings.Unbounded.Unbounded_String;
   --  Get the VALUE for Key and convert it to target type.
   --  Exceptions:
   --    Conversion_Error

   function Has_Value (Key : in Keys) return Boolean;
   --  Return True if Key is not a Null_Unbounded_String.

   procedure Load_File (Config_File : in String);
   --  Load the config file Config_File. This can be done over and over as many
   --  times as necessary. The values from the latest file overwrites the
   --  previous values.
   --  Exceptions:
   --    Cannot_Open_Ini_File
   --    Unknown_Key

private

   function Check_And_Convert (Key : in Keys) return String;
   --  Check if Key contains Null_Unbounded_String. If so, then raise the
   --  Empty_Key exception.
   --  This function is used when a Key must be converted to an Integer, Float
   --  or Boolean.
   --  Exceptions:
   --    Empty_Key

end Config_File_Parser;
