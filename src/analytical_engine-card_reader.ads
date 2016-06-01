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

with Ada.Text_IO;
with Analytical_Engine.Annunciator_Panel;
with Analytical_Engine.Attendant;
with Analytical_Engine.Card;

limited with Analytical_Engine.Framework;

private with Ada.Containers.Indefinite_Vectors;
private with Ada.Finalization;
package Analytical_Engine.Card_Reader is

   type Instance
     (Panel     : not null Analytical_Engine.Annunciator_Panel.Class_P;
      Attendant : not null Analytical_Engine.Attendant.Instance_P)
   is tagged limited private;
   type Instance_P is access all Instance;

   Card_Reader_Error : exception;

   procedure Reset (This : out Instance);

   procedure Add_Cards (This : in out Instance; From_File_Named : String);

   procedure Add_Cards (This : in out Instance; From : Ada.Text_IO.File_Type);

   procedure Execute (This             : in out Instance;
                      In_The_Framework : in out Framework.Instance);

   procedure Step (This : in out Instance; By : Integer);

   procedure Halt (This : in out Instance);

private

   package Chains is new Ada.Containers.Indefinite_Vectors
     (Element_Type => Card.Card'Class,
      "="          => Card.Equals,
      Index_Type   => Positive);

   type Instance
     (Panel     : not null Annunciator_Panel.Class_P;
      Attendant : not null Analytical_Engine.Attendant.Instance_P)
     is new Ada.Finalization.Limited_Controlled with record
        Index  : Positive := 1;
        Halted : Boolean  := True;
        Chain  : Chains.Vector;
     end record;

end Analytical_Engine.Card_Reader;
