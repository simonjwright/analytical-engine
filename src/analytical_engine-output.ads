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

with Analytical_Engine.Annunciator_Panel;
with Ada.Finalization;
with Ada.Strings.Wide_Unbounded;
with GNATCOLL.GMP.Integers;

package Analytical_Engine.Output is

   type Instance (Panel : not null Annunciator_Panel.Class_P)
     is abstract new Ada.Finalization.Limited_Controlled with private;
   type Class_P is access all Instance'Class;

   procedure Output
     (To : Instance; S : Wide_String) is abstract;
   procedure Output
     (To : Instance; I : GNATCOLL.GMP.Integers.Big_Integer) is abstract;

   procedure Set_Picture
     (This : in out Instance;
      To : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String);
   procedure Clear_Picture (This : in out Instance);

   procedure Writing_Style (This : in out Instance; In_Rows : Boolean);

private

   type Instance (Panel : not null Annunciator_Panel.Class_P)
     is abstract new Ada.Finalization.Limited_Controlled with record
        Picture : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
        In_Rows : Boolean := True;
     end record;

end Analytical_Engine.Output;
