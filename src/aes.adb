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
with Ada.Text_IO;
with GNAT.Command_Line;

with Analytical_Engine.Annunciator_Panel.Command_Line;
with Analytical_Engine.Card;
with Analytical_Engine.Card_Reader;
with Analytical_Engine.Framework;
with Analytical_Engine.Output.Printer;
with Analytical_Engine.Store;

use Analytical_Engine;

procedure Aes is

   Command_Line_Config : GNAT.Command_Line.Command_Line_Configuration;
   Tracing : aliased Boolean := False;
   Allow_Overwrite_Nonzero : aliased Boolean := False;

   Panel : constant Annunciator_Panel.Class_P
     := new Annunciator_Panel.Command_Line.Instance;
   F : Framework.Instance
     := Framework.Create
       (With_Panel => Panel,
        With_Output => new Output.Printer.Instance (Panel));

begin
   GNAT.Command_Line.Set_Usage
     (Command_Line_Config,
      Usage => "[card-chain-file]",
      Help  =>
        "Run the chain of cards in card-chain-file (or standard input)");
   GNAT.Command_Line.Define_Switch
     (Command_Line_Config,
      Tracing'Access,
      "-t",
      Long_Switch => "--trace",
      Help => "Trace execution (like card T1)");
   GNAT.Command_Line.Define_Switch
     (Command_Line_Config,
      Allow_Overwrite_Nonzero'Access,
      "-z",
      Long_Switch => "--overwrite-nonzero",
      Help => "Allow storing to a non-zero column");

   GNAT.Command_Line.Getopt (Command_Line_Config);

   Panel.Set_Tracing (Tracing);
   Store.Allow_Overwrite_Nonzero (Allow_Overwrite_Nonzero);

   declare
      Chain_File_Name : constant String := GNAT.Command_Line.Get_Argument;
   begin
      F.Card_Reader.Add_Cards (Chain_File_Name);
   end;

   F.Run;
exception
   when E :
     Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error =>
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Couldn't open " & Ada.Exceptions.Exception_Message (E));
   when E : Analytical_Engine.Card.Card_Error =>
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Card error: " & Ada.Exceptions.Exception_Message (E));
   when GNAT.Command_Line.Exit_From_Command_Line
     | GNAT.Command_Line.Invalid_Switch => null;
end Aes;
