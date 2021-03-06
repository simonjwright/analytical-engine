--  Copyright (C) Simon Wright <simon@pushface.org>
--
--  This file is part of the Analytical Engine Ada emulator
--  project. This file is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This file is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are
--  granted additional permissions described in the GCC Runtime
--  Library Exception, version 3.1, as published by the Free Software
--  Foundation.
--
--  You should have received a copy of the GNU General Public License
--  and a copy of the GCC Runtime Library Exception along with this
--  program; see the files COPYING3 and COPYING.RUNTIME respectively.
--  If not, see <http://www.gnu.org/licenses/>.

with Ada.Characters.Conversions;
with Ada.Strings.Fixed;

package body Analytical_Engine.Store is

   Zero : constant Big_Integer := Make ("0");

   Overwrite_Nonzero_Allowed : Boolean := False;

   procedure Allow_Overwrite_Nonzero (Allow : Boolean)
   is
   begin
      Overwrite_Nonzero_Allowed := Allow;
   end Allow_Overwrite_Nonzero;

   function "+" (Item : String) return Wide_String
     renames Ada.Characters.Conversions.To_Wide_String;

   procedure Set (This : in out Instance; Col : Column; To : Big_Integer)
   is
   begin
      if not Overwrite_Nonzero_Allowed
        and then This.Columns (Col) /= Zero
      then
         raise Store_Error with "storing on non-zero column";
      end if;
      if This.Panel.Tracing then
         This.Panel.Log_Trace_Message
           (+("Store: "
                & Image (To)
                & " => V"
                & Ada.Strings.Fixed.Trim (Col'Img, Ada.Strings.Both)));
      end if;
      Set (This.Columns (Col), To => To);
   end Set;

   procedure Get (This : in out Instance;
                  Col : Column;
                  Result : out Big_Integer;
                  Preserve : Boolean)
   is
   begin
      if This.Panel.Tracing then
         This.Panel.Log_Trace_Message
           (+("Store: V"
                & Ada.Strings.Fixed.Trim (Col'Img, Ada.Strings.Both)
                & "("
                & Image (This.Columns (Col))
                & ") => Mill" & (if Preserve then "" else ", zeroed")));
      end if;
      Set (Result, To => This.Columns (Col));
      if not Preserve then
         Set (This.Columns (Col), To => Zero);
      end if;
   end Get;

end Analytical_Engine.Store;
