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
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Command_Line;

with Analytical_Engine.Annunciator_Panel.Command_Line;
with Analytical_Engine.Card_Reader;
with Analytical_Engine.Framework;
with Analytical_Engine.Output.Printer;

use Analytical_Engine;

procedure Aes is

   Command_Line_Config : GNAT.Command_Line.Command_Line_Configuration;

   Panel : constant Annunciator_Panel.Class_P
     := new Annunciator_Panel.Command_Line.Instance;
   F : Framework.Instance
     := Framework.Create
       (With_Panel => Panel,
        With_Output => new Output.Printer.Instance (Panel));
   R : constant Card_Reader.Instance_P := F.Card_Reader;

begin

   GNAT.Command_Line.Set_Usage (Command_Line_Config,
                                Usage => "card-file",
                                Help  => "Run the chain of cards");
   --  There are no specific switches as yet, and there's a bug in
   --  Getopt.
   --  GNAT.Command_Line.Getopt (Command_Line_Config);

   declare
      Chain_File_Name : constant String := GNAT.Command_Line.Get_Argument;
   begin
      if Chain_File_Name = "" then
         R.Add_Cards (Ada.Text_IO.Standard_Input);
      else
         declare
            Chain_File : Ada.Text_IO.File_Type;
         begin
            Ada.Text_IO.Open (Chain_File,
                              Name => Chain_File_Name,
                              Mode => Ada.Text_IO.In_File);
            R.Add_Cards (Chain_File);
            Ada.Text_IO.Close (Chain_File);
         exception
            when E :
              Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Couldn't open " & Ada.Exceptions.Exception_Message (E));
               return;
         end;
      end if;
      F.Run;
   end;
end Aes;
