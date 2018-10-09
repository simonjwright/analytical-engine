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
with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;

package body Analytical_Engine.Output.Printer is

   procedure Output (To : Instance; S : Wide_String)
   is
   begin
      if S = (1 => Ada.Characters.Conversions.To_Wide_Character (ASCII.LF))
      then
         New_Line;
      else
         Put (S);
         if To.In_Rows then
            New_Line;
         end if;
      end if;
   end Output;

   procedure Output
     (To : Instance; I : GNATCOLL.GMP.Integers.Big_Integer)
   is
      function "+" (Item : String) return Wide_String
        renames Ada.Characters.Conversions.To_Wide_String;
      Picture : constant Wide_String
        := Ada.Strings.Wide_Unbounded.To_Wide_String (To.Picture);
   begin
      if Picture'Length = 0 then
         To.Output (+GNATCOLL.GMP.Integers.Image (I));
      else
         declare
            use type GNATCOLL.GMP.Integers.Big_Integer;
            Negative : constant Boolean := I < 0;
            Image : constant Wide_String :=
              +GNATCOLL.GMP.Integers.Image (abs I);
            Index : Natural := Image'Length;
            Sign_Output : Boolean := False;
            Result : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
            use type Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         begin
            for C of reverse Picture loop
               case C is
                  when '9' =>
                     --  Digit, unconditionally
                     if Index = 0 then
                        Result := '0' & Result;
                     else
                        Result := Image (Index) & Result;
                        Index := Index - 1;
                     end if;
                  when '#' =>
                     --  Digit, if number not exhausted
                     if Index > 0 then
                        Result := Image (Index) & Result;
                        Index := Index - 1;
                     end if;
                  when ',' =>
                     --  Comma, if digits remain to be output
                     if Index > 0
                       --  or else (for some C of Picture => C = '9')
                     then
                        Result := ',' & Result;
                     end if;
                  when '-' =>
                     --  Sign, if negative
                     if Negative then
                        Result := '-' & Result;
                        Sign_Output := True;
                     end if;
                  when '@' | '±' =>
                     --  Plus or minus sign
                     Result := (if Negative then '-' else '+') & Result;
                     Sign_Output := True;
                  when '+' =>
                     --  Sign if negative, space if positive
                     Result := (if Negative then '-' else ' ') & Result;
                     Sign_Output := True;
                  when others =>
                     Result := C & Result;
               end case;
            end loop;
            --  insert any number "left over"
            Result := Image (1 .. Index) & Result;
            if Negative and not Sign_Output then
               Result := '-' & Result;
            end if;
            To.Output (Ada.Strings.Wide_Unbounded.To_Wide_String (Result));
         end;
      end if;
   end Output;

end Analytical_Engine.Output.Printer;
