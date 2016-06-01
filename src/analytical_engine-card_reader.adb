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
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (F,
                        Name => From_File_Named,
                        Mode => Ada.Text_IO.In_File);
      This.Add_Cards (F);
      Ada.Text_IO.Close (F);
   exception
      when Ada.Text_IO.End_Error =>
         Ada.Text_IO.Close (F);
   end Add_Cards;

   procedure Add_Cards (This : in out Instance; From : Ada.Text_IO.File_Type)
   is
   begin
      loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (From);
         begin
            This.Chain.Append (Card.Read (Line));
            --  exit when Ada.Text_IO.End_Of_File (From);
         end;
      end loop;
   exception
      when Ada.Text_IO.End_Error => null;
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
