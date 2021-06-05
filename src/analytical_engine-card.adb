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

with Ada.Wide_Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Wide_Fixed;
with GNATCOLL.GMP.Integers;

with Analytical_Engine.Card.Attendant_Request;
with Analytical_Engine.Mill;
with Analytical_Engine.Store;

with Analytical_Engine.Card.Action_Card;
with Analytical_Engine.Card.Combinatorial_Card;
with Analytical_Engine.Card.Comment_Card;
with Analytical_Engine.Card.Number_Card;
with Analytical_Engine.Card.Operation_Card;
with Analytical_Engine.Card.Stepping_Card;
with Analytical_Engine.Card.Tracing_Card;
with Analytical_Engine.Card.Variable_Card;

package body Analytical_Engine.Card is

   use GNATCOLL.GMP.Integers;

   --  Note, these values are actually 1 more (less) than the maximum
   --  (minimum) possible value that can be held on a column.
   Max_Value : constant Big_Integer
     := Make ("100000000000000000000000000000000000000000000000000");
   Min_Value : constant Big_Integer := -Max_Value;

   function "+" (Item : Wide_String) return String
     is (Ada.Characters.Conversions.To_String (Item, Substitute => ' '));

   function Read (From : Wide_String) return Card'Class
   is
      Start : Positive := From'First;
      Leading : Wide_Character;
   begin
      if From'Length = 0 or else From (Start) in ' ' | '.' then
         return C : Comment_Card.Card do
            C.Source := To_Unbounded_Wide_String (From);
         end return;
      end if;
      Leading := Ada.Wide_Characters.Handling.To_Upper (From (Start));
      Start := Start + 1;
      case Leading is
         when 'N' =>
            return C : Number_Card.Card do
               C.Source := To_Unbounded_Wide_String (From);
               declare
                  First : Positive;
                  Last : Natural;
               begin
                  Ada.Strings.Wide_Fixed.Find_Token
                    (Source => From,
                     Set    => White_Space,
                     From   => Start,
                     Test   => Ada.Strings.Outside,
                     First  => First,
                     Last   => Last);
                  if Last = 0 then
                     raise Card_Error with "invalid number card";
                  end if;
                  C.Target_Column :=
                    Store.Column'Value (+(From (First .. Last)));
                  Start := Last + 1;
                  Ada.Strings.Wide_Fixed.Find_Token
                    (Source => From,
                     Set    => White_Space_Or_Plus, -- GNATCOLL issue 1
                     From   => Start,
                     Test   => Ada.Strings.Outside,
                     First  => First,
                     Last   => Last);
                  if Last = 0 then
                     raise Card_Error with "invalid number card";
                  end if;
                  C.Value :=
                    new Big_Integer'(Make (+(From (First .. Last))));
                  if C.Value.all > Max_Value
                    or C.Value.all < Min_Value
                  then
                     raise Card_Error with "invalid number card";
                  end if;
               end;
            end return;
         when 'L' | 'Z' | 'S' =>
            return C : Variable_Card.Card do
               C.Source := To_Unbounded_Wide_String (From);
               declare
                  First : Positive;
                  Last : Natural;
                  Primed : Boolean;
               begin
                  Ada.Strings.Wide_Fixed.Find_Token
                    (Source => From,
                     Set    => White_Space,
                     From   => Start,
                     Test   => Ada.Strings.Outside,
                     First  => First,
                     Last   => Last);
                  if Last = 0 then
                     raise Card_Error with "invalid variable card";
                  end if;
                  if From (Last) = ''' then
                     Primed := True;
                     Last := Last - 1;
                  else
                     Primed := False;
                  end if;
                  if Primed then
                     C.Axis := (case Leading is
                                   when 'S'       => Mill.Egress_Primed,
                                   when 'L' | 'Z' => Mill.Ingress_Primed,
                                   when others    => raise Program_Error);
                  else
                     C.Axis := (case Leading is
                                   when 'S'       => Mill.Egress,
                                   when 'L' | 'Z' => Mill.Ingress,
                                   when others    => raise Program_Error);
                  end if;
                  C.Column :=
                    Store.Column'Value (+(From (First .. Last)));
                  C.Preserve := Leading = 'L';   -- irrelevant for 'S'
               end;
            end return;
         when '+' | '-' | '*' | '×' | '/' | '÷' =>
            return C : Operation_Card.Card do
               C.Source := To_Unbounded_Wide_String (From);
               C.Op := (case Leading is
                           when '+'       => Mill.Add,
                           when '-'       => Mill.Subtract,
                           when '*' | '×' => Mill.Multiply,
                           when '/' | '÷' => Mill.Divide,
                           when others    => raise Program_Error);
            end return;
         when '>' | '<' =>
            return C : Stepping_Card.Card do
               C.Source := To_Unbounded_Wide_String (From);
               C.Direction := (case Leading is
                                  when '>'    => Mill.Down,
                                  when '<'    => Mill.Up,
                                  when others => raise Program_Error);
               declare
                  First : Positive;
                  Last : Natural;
               begin
                  Ada.Strings.Wide_Fixed.Find_Token
                    (Source => From,
                     Set    => White_Space,
                     From   => Start,
                     Test   => Ada.Strings.Outside,
                     First  => First,
                     Last   => Last);
                  if Last = 0 then
                     raise Card_Error with "invalid step up/down card";
                  end if;
                  C.Step_Count :=
                    Positive'Value (+((From (First .. Last))));
                  if C.Step_Count > 100 then
                     raise Card_Error with "invalid step up/down card";
                  end if;
               end;
            end return;
         when 'C' =>
            return C : Combinatorial_Card.Card do
               C.Source := To_Unbounded_Wide_String (From);
               begin
                  declare
                     Direction : constant Wide_Character
                       := Ada.Wide_Characters.Handling.To_Upper (From (Start));
                  begin
                     if Direction = 'F' then
                        C.Advance := True;
                     elsif Direction = 'B' then
                        C.Advance := False;
                     else
                        raise Card_Error with "invalid combinatorial card";
                     end if;
                  end;
                  Start := Start + 1;
                  if From (Start) = '?' then
                     C.Conditional := True;
                     Start := Start + 1;
                  elsif From (Start) in '0' .. '9' | '+' then
                     C.Conditional := False;
                  else
                     raise Card_Error with "invalid combinatorial card";
                  end if;
                  if Start > From'Last then
                     raise Constraint_Error;
                  end if;
                  declare
                     First : Positive;
                     Last : Natural;
                  begin
                     Ada.Strings.Wide_Fixed.Find_Token
                       (Source => From,
                        Set    => White_Space,
                        From   => Start,
                        Test   => Ada.Strings.Outside,
                        First  => First,
                        Last   => Last);
                     if Last = 0 then
                        raise Card_Error with "invalid combinatorial card";
                     end if;
                     C.Card_Count :=
                       Positive'Value (+(From (First .. Last)));
                  end;
               exception
                  when Constraint_Error =>
                     raise Card_Error with "invalid combinatorial card";
               end;
            end return;
         when 'B' | 'P' | 'H' =>
            return C : Action_Card.Card
              (Act => (case Leading is
                          when 'B'    => Action_Card.Ring_Bell,
                          when 'P'    => Action_Card.Print_Last_Result,
                          when 'H'    => Action_Card.Halt_Engine,
                          when others => raise Program_Error)) do
               C.Source := To_Unbounded_Wide_String (From);
               case Leading is
                  when 'H' =>
                     C.Msg := To_Unbounded_Wide_String
                       (Ada.Strings.Wide_Fixed.Trim
                          (From (From'First + 1 .. From'Last),
                        Ada.Strings.Both));
                  when others => null;
               end case;
            end return;
         when 'T' =>
            return C : Tracing_Card.Card do
               declare
                  First : Positive;
                  Last : Natural;
               begin
                  Ada.Strings.Wide_Fixed.Find_Token
                    (Source => From,
                     Set    => White_Space,
                     From   => Start,
                     Test   => Ada.Strings.Outside,
                     First  => First,
                     Last   => Last);
                  if Last = 0 then
                     raise Card_Error with "invalid tracing card";
                  end if;
                  C.Tracing :=
                    Natural'Value (+(From (First .. Last))) /= 0;
               end;
            end return;
         when 'A' =>
            return Attendant_Request.Read (From);
         when others =>
            raise Card_Error with "unrecognised card";
      end case;
   end Read;

   function Image (C : Card) return String
   is
      use Ada.Characters.Conversions;
   begin
      return
        "("
          & To_String (To_Wide_String (C.Source_File))
          & ":"
          & Ada.Strings.Fixed.Trim (C.Line_Number'Img, Ada.Strings.Both)
          & ") "
          & To_String (To_Wide_String (C.Source));
   end Image;

end Analytical_Engine.Card;
