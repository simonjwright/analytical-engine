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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Analytical_Engine.Framework;

package body Analytical_Engine.Card_Reader is

   procedure Reset (This : out Instance)
   is
   begin
      This.Index := 1;
      This.Chain.Clear;
   end Reset;

   procedure Add_Cards (This : in out Instance; From_File_Named : String)
   is
      procedure Add (From : Ada.Text_IO.File_Type; Named : String);
      procedure Add (From : Ada.Text_IO.File_Type; Named : String)
      is
         use Ada.Strings.Unbounded;
         Line_Number : Positive := 1;
         Source : constant Unbounded_String := To_Unbounded_String (Named);
      begin
         loop
            declare
               Line : constant String := Ada.Text_IO.Get_Line (From);
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
                    & Line;
               when others =>
                  raise Card.Card_Error
                    with "error reading card at "
                    & Named
                    & ":"
                    & Ada.Strings.Fixed.Trim (Line_Number'Img,
                                              Ada.Strings.Both)
                    & " "
                    & Line;
            end;
            Line_Number := Line_Number + 1;
         end loop;
      end Add;
   begin
      if From_File_Named'Length = 0 then
         begin
            Add (Ada.Text_IO.Standard_Input, "<stdin>");
         exception
            when Ada.Text_IO.End_Error => null;
         end;
      else
         declare
            F : Ada.Text_IO.File_Type;
         begin
            Ada.Text_IO.Open (F,
                              Name => From_File_Named,
                              Mode => Ada.Text_IO.In_File);
            Add (F, From_File_Named);
            Ada.Text_IO.Close (F);
         exception
            when Ada.Text_IO.End_Error =>
               Ada.Text_IO.Close (F);
         end;
      end if;
   end Add_Cards;

   procedure Execute (This             : in out Instance;
                      In_The_Framework : in out Framework.Instance)
   is
   begin
      This.Index := 1;
      This.Halted := False;
      loop
         declare
            C : constant Card.Card'Class := This.Chain (This.Index);
         begin
            if In_The_Framework.Panel.Tracing then
               In_The_Framework.Panel.Log_Trace_Message
                 ("Card"
                    & This.Index'Img
                    & " ("
                    & Ada.Strings.Unbounded.To_String (C.Source_File)
                    & ":"
                    & Ada.Strings.Fixed.Trim (C.Line_Number'Img,
                                              Ada.Strings.Both)
                    & ") "
                    & Ada.Strings.Unbounded.To_String (C.Source));
            end if;
            This.Index := This.Index + 1;
            C.Execute (In_The_Framework);
         end;
         exit when This.Halted
           or else This.Index > Integer (This.Chain.Length);
      end loop;
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
