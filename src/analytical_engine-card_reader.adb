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
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Wide_Unbounded;
with Ada.Wide_Text_IO;
with Analytical_Engine.Framework;

package body Analytical_Engine.Card_Reader is

   function "+" (Item : String) return Wide_String
     renames Ada.Characters.Conversions.To_Wide_String;
   function "+" (Item : Wide_String) return String
   is (Ada.Characters.Conversions.To_String (Item, Substitute => ' '));

   procedure Reset (This : out Instance)
   is
   begin
      This.Index := 1;
      This.Chain.Clear;
   end Reset;

   procedure Add_Cards (This : in out Instance; From_File_Named : String)
   is
      procedure Add (From : Ada.Wide_Text_IO.File_Type; Named : String);
      procedure Add (From : Ada.Wide_Text_IO.File_Type; Named : String)
      is
         use Ada.Strings.Wide_Unbounded;
         Line_Number : Positive := 1;
         Source : constant Unbounded_Wide_String
           := To_Unbounded_Wide_String (+Named);
      begin
         loop
            declare
               Line : constant Wide_String := Ada.Wide_Text_IO.Get_Line (From);
            begin
               declare
                  C : Card.Card'Class := Card.Read (Line);
               begin
                  C.Line_Number := Line_Number;
                  C.Source_File := Source;
                  This.Chain.Append (C);
               end;
            exception
               when E : Card.Card_Error =>
                  raise Card.Card_Error
                    with Ada.Exceptions.Exception_Message (E)
                      & " at "
                      & Named
                      & ":"
                      & Ada.Strings.Fixed.Trim (Line_Number'Img,
                                                Ada.Strings.Both)
                      & " "
                      & (+Line);
               when others =>
                  raise Card.Card_Error
                    with "error reading card at "
                    & Named
                    & ":"
                    & Ada.Strings.Fixed.Trim (Line_Number'Img,
                                              Ada.Strings.Both)
                    & " "
                    & (+Line);
            end;
            Line_Number := Line_Number + 1;
         end loop;
      end Add;
   begin
      if From_File_Named'Length = 0 then
         begin
            Add (Ada.Wide_Text_IO.Standard_Input, "<stdin>");
         exception
            when Ada.Wide_Text_IO.End_Error => null;
         end;
      else
         declare
            F : Ada.Wide_Text_IO.File_Type;
         begin
            Ada.Wide_Text_IO.Open (F,
                                   Name => From_File_Named,
                                   Mode => Ada.Wide_Text_IO.In_File,
                                   Form => "wcem=8");
            Add (F, From_File_Named);
            Ada.Wide_Text_IO.Close (F);
         exception
            when Ada.Wide_Text_IO.End_Error =>
               Ada.Wide_Text_IO.Close (F);
         end;
      end if;
   end Add_Cards;

   procedure Execute (This             : in out Instance;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      This.Index := 1;
      This.Halted := False;
      Execution : loop
         declare
            C : constant Card.Card'Class := This.Chain (This.Index);
         begin
            if In_The_Framework.Panel.Tracing then
               In_The_Framework.Panel.Log_Trace_Message
                 (+("Card"
                      & This.Index'Img
                      & " "
                      & C.Image));
            end if;
            This.Index := This.Index + 1;
            C.Execute (In_The_Framework);
         exception
            when E : others =>
               In_The_Framework.Panel.Log_Trace_Message
                 (+("Error at card"
                      & Integer'Image (This.Index - 1)
                      & " "
                      & C.Image
                      & " "
                      & Ada.Exceptions.Exception_Message (E)));
               exit Execution;
         end;
         exit Execution when This.Halted
           or else This.Index > Integer (This.Chain.Length);
      end loop Execution;
      This.Halted := True;
   end Execute;

   procedure Step (This : in out Instance; By : Integer)
   is
   begin
      if not (This.Index + By in 1 .. Integer (This.Chain.Length)) then
         raise Card_Reader_Error with "step out of range";
      end if;
      This.Index := This.Index + By;
   end Step;

   procedure Halt (This : in out Instance)
   is
   begin
      This.Halted := True;
   end Halt;

end Analytical_Engine.Card_Reader;
