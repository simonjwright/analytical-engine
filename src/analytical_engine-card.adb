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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Analytical_Engine.Card.Attendant_Request;
with Analytical_Engine.Framework;

package body Analytical_Engine.Card is

   --  Note, these values are actually 1 more (less) than the maximum
   --  (minimum) possible value that can be held on a column.
   Max_Value : constant Big_Integer
     := Make ("100000000000000000000000000000000000000000000000000");
   Min_Value : constant Big_Integer := -Max_Value;

   function Read (From : String) return Card'Class
   is
      Start : Positive := From'First;
      Leading : Character;
   begin
      if From'Length = 0 or else From (Start) in ' ' | '.' then
         return C : Comment_Card do
            C.Source := Ada.Strings.Unbounded.To_Unbounded_String (From);
         end return;
      end if;
      Leading := Ada.Characters.Handling.To_Upper (From (Start));
      Start := Start + 1;
      case Leading is
         when 'N' =>
            return C : Number_Card do
               C.Source := Ada.Strings.Unbounded.To_Unbounded_String (From);
               declare
                  First : Positive;
                  Last : Natural;
               begin
                  Ada.Strings.Fixed.Find_Token
                    (Source => From,
                     Set => White_Space,
                     From => Start,
                     Test => Ada.Strings.Outside,
                     First => First,
                     Last => Last);
                  if Last = 0 then
                     raise Card_Error with "invalid number card";
                  end if;
                  C.Target_Column := Store.Column'Value (From (First .. Last));
                  Start := Last + 1;
                  Ada.Strings.Fixed.Find_Token
                    (Source => From,
                     Set => White_Space_Or_Plus,  -- GNATCOLL issue 1
                     From => Start,
                     Test => Ada.Strings.Outside,
                     First => First,
                     Last => Last);
                  if Last = 0 then
                     raise Card_Error with "invalid number card";
                  end if;
                  C.Value.Number :=
                    new Big_Integer'(Make (From (First .. Last)));
                  if C.Value.Number.all > Max_Value
                    or C.Value.Number.all < Min_Value
                  then
                     raise Card_Error with "invalid number card";
                  end if;
               end;
            end return;
         when 'L' | 'Z' | 'S' =>
            return C : Variable_Card do
               C.Source := Ada.Strings.Unbounded.To_Unbounded_String (From);
               declare
                  First : Positive;
                  Last : Natural;
                  Primed : Boolean;
               begin
                  Ada.Strings.Fixed.Find_Token
                    (Source => From,
                     Set => White_Space,
                     From => Start,
                     Test => Ada.Strings.Outside,
                     First => First,
                     Last => Last);
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
                  C.Column := Store.Column'Value (From (First .. Last));
                  C.Preserve := Leading = 'L';   -- irrelevant for 'S'
               end;
            end return;
         when '+' | '-' | '*' | '/' =>
            return C : Operation_Card do
               C.Source := Ada.Strings.Unbounded.To_Unbounded_String (From);
               C.Op := (case Leading is
                           when '+'    => Mill.Add,
                           when '-'    => Mill.Subtract,
                           when '*'    => Mill.Multiply,
                           when '/'    => Mill.Divide,
                           when others => raise Program_Error);
            end return;
         when '>' | '<' =>
            return C : Stepping_Card do
               C.Source := Ada.Strings.Unbounded.To_Unbounded_String (From);
               C.Direction := (case Leading is
                                  when '>'    => Mill.Down,
                                  when '<'    => Mill.Up,
                                  when others => raise Program_Error);
               declare
                  First : Positive;
                  Last : Natural;
               begin
                  Ada.Strings.Fixed.Find_Token
                    (Source => From,
                     Set => White_Space,
                     From => Start,
                     Test => Ada.Strings.Outside,
                     First => First,
                     Last => Last);
                  if Last = 0 then
                     raise Card_Error with "invalid step up/down card";
                  end if;
                  C.Step_Count := Positive'Value (From (First .. Last));
                  if C.Step_Count > 100 then
                     raise Card_Error with "invalid step up/down card";
                  end if;
               end;
            end return;
         when 'C' =>
            return C : Combinatorial_Card do
               C.Source := Ada.Strings.Unbounded.To_Unbounded_String (From);
               begin
                  declare
                     Direction : constant Character
                       := Ada.Characters.Handling.To_Upper (From (Start));
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
                     Ada.Strings.Fixed.Find_Token
                       (Source => From,
                        Set    => White_Space,
                        From   => Start,
                        Test   => Ada.Strings.Outside,
                        First  => First,
                        Last   => Last);
                     if Last = 0 then
                        raise Card_Error with "invalid combinatorial card";
                     end if;
                     C.Card_Count := Positive'Value (From (First .. Last));
                  end;
               exception
                  when Constraint_Error =>
                     raise Card_Error with "invalid combinatorial card";
               end;
            end return;
         when 'B' | 'P' | 'H' =>
            return C : Action_Card
              (Act => (case Leading is
                          when 'B'    => Ring_Bell,
                          when 'P'    => Print_Last_Result,
                          when 'H'    => Halt_Engine,
                          when others => raise Program_Error)) do
               C.Source := Ada.Strings.Unbounded.To_Unbounded_String (From);
               case Leading is
                  when 'H' =>
                     C.Msg := To_Unbounded_String
                       (Ada.Strings.Fixed.Trim
                          (From (From'First + 1 .. From'Last),
                        Ada.Strings.Both));
                  when others => null;
               end case;
            end return;
         when 'T' =>
            return C : Tracing_Card do
               declare
                  First : Positive;
                  Last : Natural;
               begin
                  Ada.Strings.Fixed.Find_Token
                    (Source => From,
                     Set => White_Space,
                     From => Start,
                     Test => Ada.Strings.Outside,
                     First => First,
                     Last => Last);
                  if Last = 0 then
                     raise Card_Error with "invalid tracing card";
                  end if;
                  C.Tracing := Natural'Value (From (First .. Last)) /= 0;
               end;
            end return;
         when 'A' =>
            return Attendant_Request.Read (From);
         when others =>
            raise Card_Error with "unrecognised card";
      end case;
   end Read;

   procedure Adjust (Obj : in out Controlled_Big_Integer)
   is
      Tmp : constant Big_Integer_P := new Big_Integer;
   begin
      Set (Tmp.all, Obj.Number.all);
      Obj.Number := Tmp;
   end Adjust;

   procedure Finalize (Obj : in out Controlled_Big_Integer)
   is
      procedure Free
        is new Ada.Unchecked_Deallocation (Big_Integer, Big_Integer_P);
   begin
      Free (Obj.Number);
   end Finalize;

   procedure Execute (C : Number_Card;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      In_The_Framework.Store.Set (Col => C.Target_Column,
                                  To => C.Value.Number.all);
   end Execute;

   procedure Execute (C : Operation_Card;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      In_The_Framework.Mill.Set_Operation (C.Op);
   end Execute;

   procedure Execute (C : Variable_Card;
                      In_The_Framework : in out Framework.Instance)
   is
      Value : Big_Integer;
   begin
      case C.Axis is
         when Mill.Ingress =>
            In_The_Framework.Store.Get (Col => C.Column,
                                        Result => Value,
                                        Preserve => C.Preserve);
            In_The_Framework.Mill.Set_Ingress (Value);
         when Mill.Ingress_Primed =>
            In_The_Framework.Store.Get (Col => C.Column,
                                        Result => Value,
                                        Preserve => C.Preserve);
            In_The_Framework.Mill.Set_Ingress_Primed (Value);
         when Mill.Egress =>
            In_The_Framework.Mill.Get_Egress (Value);
            In_The_Framework.Store.Set (Col => C.Column, To => Value);
         when Mill.Egress_Primed =>
            In_The_Framework.Mill.Get_Egress_Primed (Value);
            In_The_Framework.Store.Set (Col => C.Column, To => Value);
      end case;
   end Execute;

   procedure Execute (C : Stepping_Card;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      In_The_Framework.Mill.Step_Axes (Direction => C.Direction,
                                       Amount => C.Step_Count);
   end Execute;

   procedure Execute (C : Combinatorial_Card;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      if not C.Conditional or else In_The_Framework.Mill.Run_Up_Set then
         In_The_Framework.Card_Reader.Step
           (if C.Advance then C.Card_Count else -C.Card_Count);
      end if;
   end Execute;

   procedure Execute (C : Action_Card;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      case C.Act is
         when Halt_Engine =>
            In_The_Framework.Panel.Log_Attendant_Message
              ("Halt: " & To_String (C.Msg));
            In_The_Framework.Card_Reader.Halt;
         when Ring_Bell =>
            In_The_Framework.Panel.Log_Attendant_Message ("Ting!");
         when Print_Last_Result =>
            declare
               Result : Big_Integer;
            begin
               In_The_Framework.Mill.Get_Egress (Result);
               In_The_Framework.Output.Output (Result);
            end;
      end case;
   end Execute;

   procedure Execute (C : Comment_Card;
                      In_The_Framework : in out Framework.Instance)
     is null;

   procedure Execute (C : Tracing_Card;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      In_The_Framework.Panel.Set_Tracing (To => C.Tracing);
   end Execute;

   function Image (C : Card) return String
   is
   begin
      return
        "("
        & Ada.Strings.Unbounded.To_String (C.Source_File)
        & ":"
        & Ada.Strings.Fixed.Trim (C.Line_Number'Img, Ada.Strings.Both)
        & ") "
        & Ada.Strings.Unbounded.To_String (C.Source);
   end Image;

end Analytical_Engine.Card;
